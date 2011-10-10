/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

/**
 *
 * Firstly, as an aside, note that we could very easily implement a MongoDB backend
 * for db-light using only the OCaml-based bson.ml and mongo.ml code.
 *
 * Design goals for the new high-level MongoDB code are simple, I think:
 *
 *   1) We want a generalisation of the existing Db interface, for example we don't
 *      want to be restricted to int and string maps.
 *
 *   2) We want a MongoDB interface which allows as much of the work, searches etc.
 *      to be carried out by the MongoDB server.
 *
 *   3) We want to be able to implement or emulate foreign keys efficiently.
 *
 *   4) We want to handle maps as objects without reading the entire database to
 *      construct the map.  I am assuming here that the Map interface will continue
 *      to be the "standard" way of viewing a database in OPA.
 *
 *   5) We want the Db interface to be a subset of this module, or at least emulatable
 *      using the constructs in this module.
 *
 * The following are what I would call "side-conditions" rather than subgoals:
 *
 *   a) We are prepared to tolerate a naive mapping of recursive types into nested BSON
 *      documents.  We can accept that a 10^6 element list will map to a document
 *      nested 10^6 deep which MongoDB may or may not be able to handle.
 *
 *   b) We will make an effort to map naturally onto MongoDB's types, for example, MongoDB
 *      provides many useful functions which operate over arrays so it would seem to make
 *      sense to map lists onto arrays.  Other special cases could be handled for efficiency.
 *
 *   c) Initially, the dbGen syntax can be ignored in the assumption that it can be
 *      implemented later using a dedicated syntax-based compiler phase generating calls
 *      to the MongoDb module.
 *
 * I am proposing to use the existing OPA to BSON module which currently only exists
 * in the dbMongo code but which could be factored out for use here.  An initial design
 * might be:
 *
 * Revised design <Thu Oct  6 10:02:54 CEST 2011>
 *
 * Note:
 *
 *   A map is:
 *     { label -> value; label2 -> value2; ... }
 *
 *   A set can be viewed as a degenerate map:
 *     { label -> ; label2 -> ; ... }
 *
 *   A map can be implemented within a set:
 *     { (label,value) -> ; (label2,value2) ->; ... }
 *
 *   An array can be viewed as a specialisation of a map:
 *     { 0 -> value; 1 -> value2; ... }
 *
 *   A map can be implemented within an array:
 *     { 0 -> (label,value); 1 -> (label2,value2); ... }
 *
 *   etc. etc. (set within array):
 *     { 0 -> (label,()); 1 -> (label2,()); ... }
 *
 *   The main difference between these is the computational complexity of the basic ops.
 *
 *   Now MongoDB implements heterogeneous sets (collections) of (labeled) Bson documents:
 *     { {"label":value}, {"label2":value2} }
 *   (where value, value2 can be documents) which is, in effect, a map from keys to values
 *
 *   The current "standard" OPA type to Bson implementation is simple:
 *     {a:'a; b:'b} -> { a: 'a, b: 'b }
 *     'a -> { value: 'a }
 *   where we use a "natural" mapping of constant types: int -> Int64, string -> String etc.
 *   special cases:
 *     list('a) -> { Array=(<label>,{ 0:'a; 1:'a; ... }) }
 *     Bson.document -> Bson.document (verbatim, including labels)
 *
 *   An update is:
 *     update(db,select,update)
 *   where select defines the the values to be updated and update
 *   specifies the new values.
 *
 *   A query is:
 *     query(num_to_skip,num_to_return,select,field_select)
 *   where select defines the values to return, field_select
 *   says which fields in the value to return (_id is always included).
 *
 *   The current schema for mapping dbGen syntax to MongoDB documents is:
 *
 *     file: prog.opa
 *     definitions:
 *       db /coll/x : t
 *       db /coll/y : u
 *       db /coll/...
 * 
 *     Leads to:
 * 
 *     database name: "prog_exe" (or whatever is defined in the program)
 *     collection name: "coll"
 *     OPA type: {x:t} / {y:u} / ...
 *     MongoDB: the OPA to Bson schema above.
 * 
 *  Notes:
 *
 *    - This is different from the existing dbGen syntax since
 *      multiple db definitions at the root of the path now map to sum
 *      types instead of records.  This allows storage of multiple types
 *      in the same collection.
 **/

/* Helper functions */
dl(l:list(Bson.element)):Bson.document = l
doc(n:string,d:Bson.document):Bson.element = {Document=(n,d)}
dd(n:string,d:Bson.document):Bson.document = [{Document=(n,d)}]
arr(n:string,l:list(Bson.document)):Bson.element = {Array=(n,List.mapi((i, d -> ({Document=("{i}",d)}:Bson.element)),l))}
i32(n:string,i:int):Bson.element = {Int32=(n,i)}
i64(n:string,i:int):Bson.element = {Int64=(n,i)}
dbl(n:string,d:float):Bson.element = {Double=(n,d)}
str(n:string,s:string):Bson.element = {String=(n,s)}
bool(n:string,b:bool):Bson.element = {Boolean=(n,b)}
binary(n:string,b:string):Bson.element = {Binary=(n,b)}
null(n:string):Bson.element = {Null=(n,void)}
/*
 * MDB {{ ... }}:
 *   
 *   A low-level module allowing management of connections to MongoDB
 *   servers.  To be used by higher-level modules so that only one
 *   connection is opened to a given server whereas several interfaces
 *   such as those defined below can be attached to the open connection.
 *   
 */

type mongodb = {
  mongo: Mongo.db;
  bufsize: int;
  addr: string;
  port: int;
  dbname: string;
  collection: string;
  link_count: Mutable.t(int);
  keyname: string;
  valname: string;
  idxname: string;
  fields: option(Bson.document);
  limit: int;
  skip: int;
  insert_flags: int;
  update_flags: int;
  delete_flags: int;
  query_flags: int;
}

type MDB = {{
  // TODO: Documentation
  open : int, string, int -> mongodb
  clone : mongodb -> mongodb
  namespace : mongodb, string, string -> mongodb
  close : mongodb -> void
  last_error : mongodb -> Mongo.result
  err : mongodb, string -> void
  limit : mongodb, int -> mongodb
  skip : mongodb, int -> mongodb
  fields : mongodb, option(Bson.document) -> mongodb
  continueOnError : mongodb -> mongodb
  upsert : mongodb -> mongodb
  multiUpdate : mongodb -> mongodb
  singleRemove : mongodb -> mongodb
  tailableCursor : mongodb -> mongodb
  slaveOk : mongodb -> mongodb
  oplogReplay : mongodb -> mongodb
  noCursorTimeout : mongodb -> mongodb
  awaitData : mongodb -> mongodb
  exhaust : mongodb -> mongodb
  partial : mongodb -> mongodb
  opa2doc : 'a -> Bson.document
}}

MDB : MDB = {{

  open(bufsize:int, addr:string, port:int): mongodb =
    mongo = Mongo.open(bufsize,addr,port)
    db = {~mongo; ~bufsize; ~addr; ~port; link_count=Mutable.make(1);
          keyname="key"; valname="value"; idxname="index";
          dbname="db"; collection="collection";
          fields={none}; limit=0; skip=0;
          insert_flags=0; update_flags=0; delete_flags=0; query_flags=0;
         }
    do System.at_exit( ->
                        if db.link_count.get() > 0
                        then
                          do println("closing mongo (exit) {db.link_count.get()}")
                          Mongo.close(db.mongo) 
                        else void)
    db

  clone(db:mongodb): mongodb =
    do db.link_count.set(db.link_count.get()+1)
    db

  namespace(db:mongodb, dbname:string, collection:string): mongodb =
    do db.link_count.set(db.link_count.get()+1)
    { db with dbname=dbname; collection=collection }

  close(db:mongodb): void =
    lc = db.link_count.get()
    if lc > 0
      then
        do db.link_count.set(lc-1)
        if lc <= 1
        then
          do println("closing mongo (close) {db.link_count.get()}")
          Mongo.close(db.mongo)
        else void
      else void

  last_error(db:mongodb): Mongo.result = Cursor.last_error(db.mongo, db.dbname)

  err(db:mongodb, n:string): void =
    err = Cursor.last_error(db.mongo, db.dbname)
    do println("error({n})={Bson.string_of_result(err)}")
    void

  skip(db:mongodb, skip:int): mongodb = { db with ~skip }
  limit(db:mongodb, limit:int): mongodb = { db with ~limit }
  fields(db:mongodb, fields:option(Bson.document)): mongodb = { db with ~fields }

  continueOnError(db:mongodb): mongodb = { db with insert_flags=Bitwise.land(db.insert_flags,Mongo._ContinueOnError) }
  upsert(db:mongodb): mongodb = { db with update_flags=Bitwise.land(db.update_flags,Mongo._Upsert) }
  multiUpdate(db:mongodb): mongodb = { db with update_flags=Bitwise.land(db.update_flags,Mongo._MultiUpdate) }
  singleRemove(db:mongodb): mongodb = { db with delete_flags=Bitwise.land(db.delete_flags,Mongo._SingleRemove) }
  tailableCursor(db:mongodb): mongodb = { db with query_flags=Bitwise.land(db.query_flags,Mongo._TailableCursor) }
  slaveOk(db:mongodb): mongodb = { db with query_flags=Bitwise.land(db.query_flags,Mongo._SlaveOk) }
  oplogReplay(db:mongodb): mongodb = { db with query_flags=Bitwise.land(db.query_flags,Mongo._OplogReplay) }
  noCursorTimeout(db:mongodb): mongodb = { db with query_flags=Bitwise.land(db.query_flags,Mongo._NoCursorTimeout) }
  awaitData(db:mongodb): mongodb = { db with query_flags=Bitwise.land(db.query_flags,Mongo._AwaitData) }
  exhaust(db:mongodb): mongodb = { db with query_flags=Bitwise.land(db.query_flags,Mongo._Exhaust) }
  partial(db:mongodb): mongodb = { db with query_flags=Bitwise.land(db.query_flags,Mongo._Partial) }

  /**
   * opa2doc: We just need to strip off the dummy layer used by dbMongo
   **/
  opa2doc(v:'a): Bson.document =
    match MongoDb.opa_to_bson("value",v,{none}) with
    | [{Document=("value",doc)}] -> doc
    | doc -> doc

}}

/* Test code for MDB */

mongodb = (Magic.id(MDB.open(50*1024,"www.localhost.local",27017)):mongodb)

/*
 * Select {{ ... }}:
 *   
 *   A program-level method for constructing Bson documents for select
 *   and update in a friendly manner.
 *   
 */

@abstract type select('a) = {
  select:Bson.document;
  default:option('a);
}

type Select = {{
  // TODO: Documentation
  select_to_document : select('a) -> Bson.document
  select_to_default : select('a) -> option('a)

  dot_path : MongoDb.path -> string

/*
  path : MongoDb.path -> select('a)
  path_intrange : MongoDb.path, option(int), option(int) -> select('a) 
  check_fst_arg_in_pair : select('a) -> select('a)
  check_field_in_record : select('a), string -> select('a)
*/

  empty : option('a) -> select('a)

  key : string, select('a) -> select('a)

  int32 : select('a), string, int -> select('a)
  int64 : select('a), string, int -> select('a)
  double : select('a), string, float -> select('a)
  string : select('a), string, string -> select('a)

  gti32 : int, select('a) -> select('a)
  lti32 : int, select('a) -> select('a)
  gtei32 : int, select('a) -> select('a)
  ltei32 : int, select('a) -> select('a)

  gti64 : int, select('a) -> select('a)
  lti64 : int, select('a) -> select('a)
  gtei64 : int, select('a) -> select('a)
  ltei64 : int, select('a) -> select('a)

  gtd : float, select('a) -> select('a)
  ltd : float, select('a) -> select('a)
  gted : float, select('a) -> select('a)
  lted : float, select('a) -> select('a)

  gts : string, select('a) -> select('a)
  lts : string, select('a) -> select('a)
  gtes : string, select('a) -> select('a)
  ltes : string, select('a) -> select('a)

  set_op : select('a), string -> select('a)

  gt : select('a) -> select('a)
  lt : select('a) -> select('a)
  gte : select('a) -> select('a)
  lte : select('a) -> select('a)

  and : select('a), select('a) -> select('a)
  andalso : list(select('a)) -> select('a)

  inc : select('a) -> select('a)
  set : select('a) -> select('a)
  unset : select('a) -> select('a)
  push : select('a) -> select('a)
  pushAll : select('a) -> select('a)
  addToSet : select('a) -> select('a)
  pop : select('a) -> select('a)
  pull : select('a) -> select('a)
  pullAll : select('a) -> select('a)
  rename : select('a) -> select('a)
  bit : select('a) -> select('a)
}}

Select : Select = {{

  select_to_document(select:select): Bson.document = select.select
  select_to_default(select:select('a)): option('a) = select.default

  @private
  string_of_element(e:Bson.element): string =
    match e with
    | {Int32=(_,i)} -> Int.to_string(i)
    | {Int64=(_,i)} -> Int.to_string(i)
    | {String=(_,s)} -> s
    | {Boolean=(_,b)} -> Bool.to_string(b)
    | {Document=(_,d)} -> String.concat(".",List.map(string_of_element,d))
    | {Array=(_,a)} -> String.concat("_",List.map(string_of_element,a))
    | {Null=(_,_)} -> "" // <-- ???
    | _ -> @fail

  @private
  string_of_key(key:MongoDb.key): string =
    match key with
    | {IntKey=i} -> Int.to_string(i)
    | {StringKey=s} -> s
    | {AbstractKey=a} -> String.concat(".",List.map(string_of_element,a))

  dot_path(path:MongoDb.path): string =
    String.concat(".",List.map(string_of_key,path))

  empty(default:option('a)): select('a) = {select=[]; ~default}

  key(name:string, s:select('a)): select('a) = { s with select=[{Document=(name,s.select)}] }

  int32(s:select('a), name:string, i:int): select('a) = { s with select=[{Int32=(name,i)}|s.select] }
  int64(s:select('a), name:string, i:int): select('a) = { s with select=[{Int64=(name,i)}|s.select] }
  double(s:select('a), name:string, d:float): select('a) = { s with select=[{Double=(name,d)}|s.select] }
  string(s:select('a), name:string, str:string): select('a) = { s with select=[{String=(name,str)}|s.select] }

  gti32(i:int, s:select('a)): select('a) = int32(s, "$gt", i)
  lti32(i:int, s:select('a)): select('a) = int32(s, "$lt", i)
  gtei32(i:int, s:select('a)): select('a) = int32(s, "$gte", i)
  ltei32(i:int, s:select('a)): select('a) = int32(s, "$lte", i)

  gti64(i:int, s:select('a)): select('a) = int64(s, "$gt", i)
  lti64(i:int, s:select('a)): select('a) = int64(s, "$lt", i)
  gtei64(i:int, s:select('a)): select('a) = int64(s, "$gte", i)
  ltei64(i:int, s:select('a)): select('a) = int64(s, "$lte", i)

  gtd(d:float, s:select('a)): select('a) = double(s, "$gt", d)
  ltd(d:float, s:select('a)): select('a) = double(s, "$lt", d)
  gted(d:float, s:select('a)): select('a) = double(s, "$gte", d)
  lted(d:float, s:select('a)): select('a) = double(s, "$lte", d)

  gts(str:string, s:select('a)): select('a) = string(s, "$gt", str)
  lts(str:string, s:select('a)): select('a) = string(s, "$lt", str)
  gtes(str:string, s:select('a)): select('a) = string(s, "$gte", str)
  ltes(str:string, s:select('a)): select('a) = string(s, "$lte", str)

  set_op(s:select('a), op:string): select('a) =
    select =
      ((match s.select with
        | [] -> []
        | [e] -> [{Document=(Bson.key(e),[Bson.set_key(e,op)])}]
        | l -> List.map((e -> {Document=(Bson.key(e),[Bson.set_key(e,op)])}),l)):Bson.document)
    { s with ~select }

  gt(s:select('a)): select('a) = set_op(s, "$gt")
  lt(s:select('a)): select('a) = set_op(s, "$lt")
  gte(s:select('a)): select('a) = set_op(s, "$gte")
  lte(s:select('a)): select('a) = set_op(s, "$lte")

  and(s1:select('a), s2:select('a)): select('a) =
    { select=([{Array=("$and",([{Document=("0",s1.select)},
                                {Document=("1",s2.select)}]:Bson.document))}]:Bson.document);
      default=s1.default }

  andalso(ss:list(select('a))): select('a) =
    match ss with
    | [] -> empty({none})
    | [s|t] -> { select=[{Array=("$and",(List.mapi((i, ss -> {Document=("{i}",ss.select)}),[s|t]):Bson.document))}];
                 default=s.default }

  inc(s:select('a)): select('a) = key("$inc",s)
  set(s:select('a)): select('a) = key("$set",s)
  unset(s:select('a)): select('a) = key("$unset",s)
  push(s:select('a)): select('a) = key("$push",s)
  pushAll(s:select('a)): select('a) = key("$pushAll",s)
  addToSet(s:select('a)): select('a) = key("$addToSet",s)
  pop(s:select('a)): select('a) = key("$pop",s)
  pull(s:select('a)): select('a) = key("$pull",s)
  pullAll(s:select('a)): select('a) = key("$pullAll",s)
  rename(s:select('a)): select('a) = key("$rename",s)
  bit(s:select('a)): select('a) = key("$rename",s)

}}

/* Test code for select */
// none

/*
 * Collection {{ ... }}:
 *   
 * Implements the collection type presented to the user and also the target
 * for the new db syntax.  I would have preferred "Set" to "Collection"
 * because it would have been easier to type but that name is already occupied
 * in the namespace.
 *
 **/

type batch = list(Bson.document)

type collection('a) = {
  db: mongodb;
  default: option('a);
}

type collection_cursor('a) = {
  collection: collection('a);
  cursor: cursor;
  query: select('a);
  ty: OpaType.ty;
}

type Collection = {{
  // TODO: Documentation
  create : mongodb, option('value) -> (select('value), collection('value))
  limit : collection('value), int -> collection('value)
  skip : collection('value), int -> collection('value)
  fields : collection('value), option(Bson.document) -> collection('value)
  continueOnError : collection('value) -> collection('value)
  upsert : collection('value) -> collection('value)
  multiUpdate : collection('value) -> collection('value)
  singleRemove : collection('value) -> collection('value)
  tailableCursor : collection('value) -> collection('value)
  slaveOk : collection('value) -> collection('value)
  oplogReplay : collection('value) -> collection('value)
  noCursorTimeout : collection('value) -> collection('value)
  awaitData : collection('value) -> collection('value)
  exhaust : collection('value) -> collection('value)
  partial : collection('value) -> collection('value)
  destroy : collection('value) -> void
  insert : collection('value), 'value -> bool
  insert_batch : collection('value), batch -> bool
  update : collection('value), select('value), select('value) -> bool
  delete : collection('value), select('value) -> bool
  find_one : collection('value), select('value) -> outcome('value,Mongo.failure)
  query : collection('value), select('value) -> outcome(collection_cursor('value),Mongo.failure)
  first : collection_cursor('value) -> outcome(collection_cursor('value),Mongo.failure)
  next : collection_cursor('value) -> (collection_cursor('value),outcome('value,Mongo.failure))
  has_more : collection_cursor('value) -> bool
  kill : collection_cursor('value) -> collection_cursor('value)
}}

Batch = {{
  empty = ([]:batch)
  add(b:batch, v:'a): batch = [MDB.opa2doc(v)|b]
  add2(b:batch, (v1:'a, v2:'b)): batch = [MDB.opa2doc(v1)|[MDB.opa2doc(v2)|b]]
  add3(b:batch, (v1:'a, v2:'b, v3:'c)): batch = [MDB.opa2doc(v1)|[MDB.opa2doc(v2)|[MDB.opa2doc(v3)|b]]]
  list(b:batch, vs:list('a)): batch = List.flatten([List.map(MDB.opa2doc,vs),b])
}}

Collection : Collection = {{

  create(db:mongodb, default:option('value)): (select('value), collection('value)) =
    (Select.empty(default), { db=MDB.clone(db); ~default })

  destroy(c:collection('value)): void = MDB.close(c.db)

  skip(c:collection('value), skip:int): collection('value) = {c with db={ c.db with ~skip }}
  limit(c:collection('value), limit:int): collection('value) = {c with db={ c.db with ~limit }}
  fields(c:collection('value), fields:option(Bson.document)): collection('value) = {c with db={ c.db with ~fields }}

  continueOnError(c:collection('value)): collection('value) =
    {c with db={ c.db with insert_flags=Bitwise.land(c.db.insert_flags,Mongo._ContinueOnError) }}
  upsert(c:collection('value)): collection('value)
    = {c with db={ c.db with update_flags=Bitwise.land(c.db.update_flags,Mongo._Upsert) }}
  multiUpdate(c:collection('value)): collection('value)
    = {c with db={ c.db with update_flags=Bitwise.land(c.db.update_flags,Mongo._MultiUpdate) }}
  singleRemove(c:collection('value)): collection('value)
    = {c with db={ c.db with delete_flags=Bitwise.land(c.db.delete_flags,Mongo._SingleRemove) }}
  tailableCursor(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.land(c.db.query_flags,Mongo._TailableCursor) }}
  slaveOk(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.land(c.db.query_flags,Mongo._SlaveOk) }}
  oplogReplay(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.land(c.db.query_flags,Mongo._OplogReplay) }}
  noCursorTimeout(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.land(c.db.query_flags,Mongo._NoCursorTimeout) }}
  awaitData(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.land(c.db.query_flags,Mongo._AwaitData) }}
  exhaust(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.land(c.db.query_flags,Mongo._Exhaust) }}
  partial(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.land(c.db.query_flags,Mongo._Partial) }}

  insert(c:collection('value), v:'value): bool =
    ns = c.db.dbname^"."^c.db.collection
    b = MDB.opa2doc(v)
    Mongo.insert(c.db.mongo,c.db.insert_flags,ns,b)

  insert_batch(c:collection('value), b:batch): bool =
    ns = c.db.dbname^"."^c.db.collection
    Mongo.insert_batch(c.db.mongo,c.db.insert_flags,ns,b)

  update(c:collection('value), select:select('value), update:select('value)): bool =
    ns = c.db.dbname^"."^c.db.collection
    Mongo.update(c.db.mongo,c.db.update_flags,ns,select.select,update.select)

  delete(c:collection('value), select:select('value)): bool =
    ns = c.db.dbname^"."^c.db.collection
    Mongo.delete(c.db.mongo,c.db.delete_flags,ns,select.select)

  find_one(c:collection('value), select:select('value)): outcome('value,Mongo.failure) =
    ns = c.db.dbname^"."^c.db.collection
    (match Cursor.find_one(c.db.mongo,ns,select.select,c.db.fields) with
     | {success=doc} ->
       ty = @typeof(Magic.id(void):'value)
       //do println("  doc={Bson.string_of_bson(doc)}\n  ty={OpaType.to_pretty(ty)}")
       (match MongoDb.bson_to_opa(doc, ty, c.db.valname) with
        | {some=v} -> {success=(Magic.id(v):'value)}
        | {none} -> {failure={Error="Collection.find_one: not found"}})
     | {~failure} -> {~failure})

  query(c:collection('value), select:select('value)): outcome(collection_cursor('value),Mongo.failure) =
    ns = c.db.dbname^"."^c.db.collection
    match Cursor.find(c.db.mongo,ns,select.select,c.db.fields,c.db.limit,c.db.skip,c.db.query_flags) with
    | {success=cursor} -> {success={collection=c; ~cursor; query=select; ty=@typeof(Magic.id(void):'value) }}
    | {~failure} -> {~failure}

  first(cc:collection_cursor('value)): outcome(collection_cursor('value),Mongo.failure) =
    _ = Cursor.reset(cc.cursor)
    query(cc.collection, cc.query)

  next(cc:collection_cursor('value)): (collection_cursor('value),outcome('value,Mongo.failure)) =
    cursor = Cursor.next(cc.cursor)
    match Cursor.check_cursor_error(cursor) with
    | {success=doc} ->
       //do println("  doc={Bson.string_of_bson(doc)}\n  ty={OpaType.to_pretty(cc.ty)}")
       (match MongoDb.bson_to_opa(doc, cc.ty, cc.collection.db.valname) with
        | {some=v} -> ({cc with ~cursor},{success=(Magic.id(v):'value)})
        | {none} -> ({cc with ~cursor},{failure={Error="Collection.next: not found"}}))
    | {~failure} ->
       cursor = Cursor.reset(cursor)
       ({cc with ~cursor},{~failure})

  has_more(cc:collection_cursor('value)): bool = Cursor.valid(cc.cursor)

  kill(cc:collection_cursor('value)): collection_cursor('value) = { cc with cursor=Cursor.reset(cc.cursor) }

}}

/* Test code for Collection */
S = Select
C = Collection
do println("Collection:")
type t = {i:int}
(empty,(c_:collection(t))) = C.create(mongodb,{some={i=-1}})
c1 = C.continueOnError(c_)
// Caveat: bare values have to be magic'ed:
//b = opa2doc(Magic.id(1):int) do println("b={Bson.string_of_bson(b)}")
_ = C.insert(c1,{i=0}) do MDB.err(c1.db,"insert(c1,\{i=0\})")
btch1 = List.fold_right(Batch.add,List.init((i -> {i=i+100}),9),Batch.empty)
//do println("btch1={List.list_to_string(Bson.string_of_bson,btch1)}")
_ = C.insert_batch(c1,btch1) do MDB.err(c1.db,"insert(c1,btch1)")
i(i:int) = S.int64(empty,"i",i)
do println("s={Bson.string_of_bson((i(1)).select)}")
s = i(0)
u = S.set(i(1))
do println("u={Bson.string_of_bson(u.select)}")
do println("u_ty={OpaType.to_pretty(@typeof(u))}")
_ = C.update(c1,s,u) do MDB.err(c1.db,"update(c1,i(0),i(1))")
_ = C.delete(C.singleRemove(c1),i(104)) do MDB.err(c1.db,"delete(c1,i(104))")
q = S.key("i",S.gti32(102,S.lti32(106,empty)))
do println("q={Bson.string_of_bson(q.select)}")
v = C.find_one(c1,q) do MDB.err(c1.db,"find_one(c1,i>102,i<106)")
do match v with
   | {success=v} -> println("v={v}")
   | {~failure} -> println("err={Bson.string_of_failure(failure)}")
do match C.query(c1,q) with
   | {success=cc1} ->
      _ =
      while(cc1,(cc1 ->
                  match C.next(cc1) with
                  | (cc1,{success=v}) ->
                     do println("v={v}")
                     if C.has_more(cc1)
                     then (cc1,true)
                     else (C.kill(cc1),false)
                  | (_cc1,{~failure}) ->
                     do println("err(query)={Bson.string_of_failure(failure)}")
                     (C.kill(cc1),false)))
      println("finished")
   | {~failure} ->
      println("err(query)={Bson.string_of_failure(failure)}")
_ = C.destroy(c1)

/** Later:
MongoMap = {{
  // Implementation of map using underlying MongoDB.
}}

MongoArray = {{
  // Implementation of Array using underlying MongoDB.
}}

MongoTree = {{
  // Implementation of Tree using underlying MongoDB.
  // Note that there is an interesting page on the MongoDB
  // website on embedding trees in MongoDB:
  // http://www.mongodb.org/display/DOCS/Trees+in+MongoDB
}}
**/

/** DEPRECATED
 * MongoMap {{ ... }}:
 *
 *   - We implement the indexing functionality here.  As a starting point we can
 *     define a pseudo-map using a (typed) MongoDb database plus a query object:
 *
 *       make_map(db:(int,string)mongodb, query:Bson.document): (int,string)map
 *
 *     Given the query string we can arrange for the creation (and deletion) of the correct
 *     indices which MongoDB needs for efficiency (only its ObjectID's have implicit and
 *     non-deletable indices generated for them).  We can then provide most (if not all)
 *     of the functionality of a standard OPA map datatype.
 *
 *     Note that we should be able to manipulate the query strings within these functions
 *     so that we can, for example, implement intersection/union maps by combining two queries.
 *
 *     In time we can do fancy things with map-reduce etc. but I view this as quite a powerful
 *     starting point.
 *
 *   - Foreign keys may need a bit of experimentation to get right.  Having read the MongoDB
 *     docs a bit more thoroughly on this issue, there are two ways it could be done.  Firstly,
 *     using the DBref mechanism where a hard reference is built into the database or, alternatively,
 *     using direct manual linking in the client.  The second alternative is the recommended
 *     method since it allows more flexibility and, additionally, can allow more efficient
 *     implementation under some circumstances (apparently).  In either case, the driver itself
 *     needs to be upgraded to support this feature.  Essentially, this has to remain under
 *     control of the client code but we need to support this feature in a more friendly manner,
 *     here.  The ideal method of operation would be to take two databases and add the foreign key
 *     to give a third database with the linking implemented: add_foreign(db1, db2, db1_key, db2_key),
 *     I'm not even attempting to write the types of these objects here, the idea would be to define
 *     maps on the new linked database.
 **/

/**
 * Db {{ ... }}:
 *
 *   - This will look like the current Db module (as far as we can implement it).  Mostly you
 *     will get: read/write/remove/{int/string}map_search/{int/string}map_fold_range.  Note that
 *     implementing history with MongoDB could prove heavy-weight and potentially disastrous
 *     for performance although I'm sure it could be done.  A primitive form of transaction,
 *     however, might be a good option.  Modification time could be handled low-level in
 *     the driver at the expense of some peformance.
 *
 */
