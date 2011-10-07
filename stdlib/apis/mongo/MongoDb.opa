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
 *   The new design for MongoDB high-level support should include:
 *
 **/

/* Helper functions */
dl(l:list(Bson.element)):Bson.document = l
doc(n:string,d:Bson.document):Bson.element = {Document=(n,d)}
dd(n:string,d:Bson.document):Bson.document = [{Document=(n,d)}]
arr(n:string,l:list(Bson.document)):Bson.element = {Array=(n,List.mapi((i, d -> ({Document=("{i}",d)}:Bson.element)),l))}
i32(n:string,i:int):Bson.element = {Int32=(n,i)}
str(n:string,s:string):Bson.element = {String=(n,s)}
bool(n:string,b:bool):Bson.element = {Boolean=(n,b)}
vd(n:string):Bson.element = {Null=(n,void)}
err(n:string):void =
  err = Cursor.last_error(mongo, "db")
  do println("err({n})={Bson.string_of_result(err)}")
  void

/*
 * MDB {{ ... }}:
 *
 *   - A low-level module containing essentially the core of the implementation but
 *     allowing untyped versions of functions, for example, the most general update
 *     function would be update : mongodb, 'key, 'value -> void.  This is prefectly implementable
 *     using OPA's dynamic types and the OPA to BSON functions which are also type-abstract.
 *
 *   - However, there is plenty of possibility for mischief here.  For instance, we want a
 *     unique mapping from keys to values whereas MongoDB would allow multiple values under
 *     the same key.  The best way of looking at the above abstract function is a low-level
 *     hook into: Mongo.update(db,flags,select,update) where select is defined by 'key and
 *     update is defined by 'value.
 */

type mongodb('key,'value) = {
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
}

MDB = {{

  close(db:mongodb('key,'value)): void =
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
  open(bufsize:int, addr:string, port:int): mongodb('key,'value) =
    mongo = Mongo.open(bufsize,addr,port)
    db = {~mongo; ~bufsize; ~addr; ~port; link_count=Mutable.make(1);
          keyname="key"; valname="value"; idxname="index";
          dbname="db"; collection="collection";
         }
    do System.at_exit( ->
                        if db.link_count.get() > 0
                        then
                          do println("closing mongo (exit) {db.link_count.get()}")
                          Mongo.close(db.mongo) 
                        else void)
    db

  namespace(db:mongodb('key,'value), dbname:string, collection:string): mongodb('key,'value) =
    do db.link_count.set(db.link_count.get()+1)
    { db with dbname=dbname; collection=collection }

  idxn(db:mongodb('key,'value), n:int): string = db.idxname^(if n == 0 then "" else Int.to_string(n))

  make_select(db:mongodb('key,'value), indices:'key): Bson.document =
    //do println("index: ty={@typeof(indices)}")
    match @typeof(indices) with
    | {TyName_args=[ty1,ty2]; TyName_ident="tuple_2"} ->
      indices = (Magic.id(indices):tuple_2('a,'b))
      List.flatten([MongoDb.opa_to_bson(db.idxname^"0",indices.f1,{some=ty1}),
                    MongoDb.opa_to_bson(db.idxname^"1",indices.f2,{some=ty2})])
    | {TyName_args=[ty1,ty2,ty3]; TyName_ident="tuple_3"} ->
      indices = (Magic.id(indices):tuple_3('a,'b,'c))
      List.flatten([MongoDb.opa_to_bson(db.idxname^"0",indices.f1,{some=ty1}),
                    MongoDb.opa_to_bson(db.idxname^"1",indices.f2,{some=ty2}),
                    MongoDb.opa_to_bson(db.idxname^"2",indices.f3,{some=ty3})])
    | {TyName_args=[ty1,ty2,ty3,ty4]; TyName_ident="tuple_4"} ->
      indices = (Magic.id(indices):tuple_4('a,'b,'c,'d))
      List.flatten([MongoDb.opa_to_bson(db.idxname^"0",indices.f1,{some=ty1}),
                    MongoDb.opa_to_bson(db.idxname^"1",indices.f2,{some=ty2}),
                    MongoDb.opa_to_bson(db.idxname^"2",indices.f3,{some=ty3}),
                    MongoDb.opa_to_bson(db.idxname^"3",indices.f4,{some=ty4})])
    | { TyName_args=[{TyName_args=[]; TyName_ident="Bson.element"}]; TyName_ident="list" }
    | { TyName_args=_; TyName_ident="Bson.document" } ->
      (Magic.id(indices):Bson.document)
    | {TyName_args=[ty]; TyName_ident="list"} ->
      indices = (Magic.id(indices):list('a))
      List.flatten(List.mapi((n, index -> MongoDb.opa_to_bson(idxn(db,n),index, {some=ty})),indices))
    | ty ->
      MongoDb.opa_to_bson(db.idxname,Magic.id(indices),{some=ty})

  select_keys(db:mongodb('key,'value), indices:'key): list(string) =
    match @typeof(indices) with
    | {TyName_args=[_,_]; TyName_ident="tuple_2"} ->
      [db.idxname^"0",db.idxname^"1"]
    | {TyName_args=[_,_,_]; TyName_ident="tuple_3"} ->
      [db.idxname^"0",db.idxname^"1",db.idxname^"2"]
    | {TyName_args=[_,_,_,_]; TyName_ident="tuple_4"} ->
      [db.idxname^"0",db.idxname^"1",db.idxname^"2",db.idxname^"3"]
    | { TyName_args=[{TyName_args=[]; TyName_ident="Bson.element"}]; TyName_ident="list" }
    | { TyName_args=_; TyName_ident="Bson.document" } ->
      Bson.keys(Magic.id(indices):Bson.document)
    | {TyName_args=[_]; TyName_ident="list"} ->
      indices = (Magic.id(indices):list('a))
      List.mapi((n, _ -> idxn(db,n)),indices)
    | _ ->
      [db.idxname]

  make_insert(db:mongodb('key,'value), value:'value): Bson.document =
    match (@typeof(value):OpaType.ty) with
    | { TyName_args=[({TyName_args=_; TyName_ident="Bson.element"}:OpaType.ty)]; TyName_ident="list" }
    | { TyName_args=_; TyName_ident="Bson.document" } ->
      (Magic.id(value):Bson.document)
    | ty -> MongoDb.opa_to_bson(db.valname,Magic.id(value),{some=ty})

  make_update(db:mongodb('key,'value), value:'value): Bson.document =
    [{Document=("$set",make_insert(db, value))}]

  insert(db:mongodb('key,'value), key:'key, value:'value): bool =
    ns = db.dbname^"."^db.collection
    select_doc = make_select(db, key)
    insert_doc = make_insert(db, value)
    Mongo.insert(db.mongo,0,ns,List.flatten([select_doc,insert_doc]))

  update(db:mongodb('key,'value), key:'key, value:'value): bool =
    ns = db.dbname^"."^db.collection
    select_doc = make_select(db, key)
    update_doc = make_update(db, value)
    Mongo.update(db.mongo,Mongo._Upsert,ns,select_doc,update_doc)

  find_one(db:mongodb('key,'value), key:'key): outcome('value,Mongo.failure) =
    ns = db.dbname^"."^db.collection
    select_doc = make_select(db, key)
    ty = @typeof(Magic.id(void):'value)
    (match Cursor.find_one(db.mongo,ns,select_doc,{some=List.map((f -> {Int32=(f,0)}),Bson.keys(select_doc))}) with
     | {success=doc} ->
       //do println("  doc={Bson.string_of_bson(doc)}\n  ty={OpaType.to_pretty(ty)}")
       (match MongoDb.bson_to_opa(doc, ty, db.valname) with
        | {some=v} -> {success=(Magic.id(v):'value)}
        | {none} -> @fail("MDB.find_one: not found"))
     | {~failure} -> @fail("MDB.find_one: error from MongoDB: {failure}"))

  delete(db:mongodb('key,'value), key:'key): bool =
    ns = db.dbname^"."^db.collection
    select_doc = make_select(db, key)
    Mongo.delete(db.mongo,Mongo._SingleRemove,ns,select_doc)

  ensure_index(db:mongodb('key,'value), key:'key, flags:int): Mongo.result =
    ns = db.dbname^"."^db.collection
    select = select_keys(db, key)
    index = List.map((k -> {Int32=(k,1)}),select)
    if Indexes.create_index(db.mongo,ns,index,flags)
    then Cursor.last_error(db.mongo, db.dbname)
    else {failure={Error="ensure_index send failure"}}

  last_error(db:mongodb('key,'value)): Mongo.result = Cursor.last_error(db.mongo, db.dbname)
}}

/* Test code for MDB */
/*
mongodb_ = (MDB.open(50*1024,"www.localhost.local",27017):mongodb(int,string))
mongodb = MDB.namespace(mongodb_,"db","collection")

tst(mongodb:mongodb('a,'b), idx:'a, val1:'b, val2:'b) =
  ns = mongodb.dbname^"."^mongodb.collection
  err = MDB.ensure_index(mongodb, idx, (Indexes._Sparse+Indexes._Unique))
  do println("err(ensure_index) = {Bson.string_of_result(err)}")
  do if MDB.insert(mongodb, idx, val1)
     then println("result(insert({ns},{idx},{val1}))=\n  {Bson.string_of_result(MDB.last_error(mongodb))}")
     else println("insert failure")
  do if MDB.update(mongodb, idx, val2)
     then println("result(update({ns},{idx},{val2}))=\n  {Bson.string_of_result(MDB.last_error(mongodb))}")
     else println("update failure")
  v = MDB.find_one(mongodb, idx)
  do println("v={v}")
  do if MDB.delete(mongodb, idx)
     then println("result(delete({ns},{idx}))=\n  {Bson.string_of_result(MDB.last_error(mongodb))}")
     else println("delete failure")
  void

do tst(Magic.id(mongodb):mongodb((int,int),string), (123,456), "abc", "def")
do tst(Magic.id(mongodb):mongodb(Bson.document,string), [{Int32=("key",789)}], "ghi", "jkl")
do tst(Magic.id(mongodb):mongodb(Bson.document,Bson.document),
        [{Int32=("_key",456)}], [{String=("s","mno")},{Int32=("i",234)}], [{String=("s","pqr")},{Int32=("i",567)}])
do MDB.close(mongodb)
do MDB.close(mongodb_)
*/

@abstract type select('a) = Bson.document

type Select = {{
  /* Formalises the OPA view of the semantics of select documents 
   * in order to facilitate expression and manipulation of select
   * objects in OPA.
   */
  select_to_document : select('a) -> Bson.document // <-- might be Id(x)=x
  path : MongoDb.path -> select('a)
  path_intrange : MongoDb.path, option(int), option(int) -> select(int) 
  and : select('a), select('a) -> select('a)
  check_fst_arg_in_pair : select('a) -> select(('a,'b))
  check_field_in_record : select('a), string -> select('b)
}}

Select = {{

  select_to_document(select:select('a)): Bson.document = select

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

  string_of_key(key:MongoDb.key): string =
    match key with
    | {IntKey=i} -> Int.to_string(i)
    | {StringKey=s} -> s
    | {AbstractKey=a} -> String.concat(".",List.map(string_of_element,a))

  dot_path(path:MongoDb.path): string =
    String.concat(".",List.map(string_of_key,path))

  /**
   * opa2doc: We just need to strip off the dummy layer used by dbMongo
   * TODO: degenerate case, v:int
   **/
  opa2doc(v:'a): Bson.document =
    match MongoDb.opa_to_bson("value",v,{none}) with
    | [{Document=("value",doc)}] -> doc
    | _ -> @fail

  bson_dot(doc:Bson.document, dollar:bool): (bool, Bson.document) =

    rec subkey(dt,e:Bson.element) =
      match e with
      | {Document=(_,d)} -> dot(dt,d)
      | {Array=(_,_)} -> @fail
      | _ -> @fail

    and dot(dt,doc) =
      match doc with
      | [{Document=(key,d)}] -> dot(dt^key^".",d)
      | [{Array=(key,[e])}] -> // TODO: verify that all subkeys are the same
        //subkeys = List.map((e -> subkey(e)),d)
        //do println("subkeys={subkeys}")
        //subkey = Option.get(List.nth(0,subkeys))
        subkey(dt^"{key}."^(if dollar then "$." else ""),e)
      | [{Int32=(key,i)}] -> (true,[{Int32=(dt^key,i)}])
      | [{Int64=(key,i)}] -> (true,[{Int64=(dt^key,i)}])
      | [{Double=(key,d)}] -> (true,[{Double=(dt^key,d)}])
      | [{String=(key,s)}] -> (false,[{String=(dt^key,s)}])
      | _ -> @fail("Unsuitable dot document {Bson.string_of_bson(doc)}")

    dot("",doc)

  make_select(v:'a): Bson.document = (bson_dot(opa2doc(v),false)).f2

  make_set(v:'a): Bson.document = [{Document=("$set",(bson_dot(opa2doc(v),true)).f2)}]

  make_inc(v:'a): Bson.document =
    (numeric,doc) = bson_dot(opa2doc(v),true)
    if numeric
    then [{Document=("$inc",doc)}]
    else @fail("Non-numeric $inc")

}}

/* Notes:

 - If we map the whole path to a collection, where do we define the namespace in "db /path/x/y/z = ..."?

*/

/* Test code for select */
mongo = Mongo.open(50*1024,"www.localhost.local",27017)
do System.at_exit( -> do println("closing mongo") Mongo.close(mongo))
ns = "db.collection"

/* db /[0]/abc/[true] : int */
/*
path = ([{IntKey=0}, {StringKey="abc"}, {AbstractKey=[{Boolean=("key",{true})}]}]:MongoDb.path)
vpath = (List.flatten([path,[{StringKey="value"}]]):MongoDb.path) // <-- need to add this according to the type
select_name = Select.dot_path(vpath)
select(n:int) = ([{Int32=(select_name,n)}]:Bson.document)
update(n:int) = ([{Document=("$set",([{Int32=(select_name,n)}]:Bson.document))}]:Bson.document)
query_name = Select.dot_path(path)
query(v:string) = ([{String=(query_name,v)}]:Bson.document)
do println("path={path}")
do println("select = {Bson.string_of_bson(select(999))}")
do println("update = {Bson.string_of_bson(update(111))}")
do println("query() = {Bson.string_of_bson(query("value"))}")
*/

/* Second attempt: path is actually a document */
/*
db /a/b[_]/{c:int;d:string}
db /a/b[_]/{f:bool;d:void}
db /h:list(int)
*/
type t =
  {a:
    {b:
      list({c:int;  d:string});
     /*e:
      {f:bool; g:void  }*/
    };
   /*h:
    {i: list(int)}*/
  }

b1 = Select.opa2doc({a={b=[{c=123; d="abc"},{c=456; d="def"}]}})
do println("b1={Bson.string_of_bson(b1)}")
_ = Mongo.insert(mongo,0,ns,b1) do err("insert")

select = Select.make_select({a={b=[{d="abc"}]}})
do println("select=\"{Bson.string_of_bson(select)}\"")
//update = Select.make_set({a={b=[{c=100}]}})
//do println("update=\"{Bson.string_of_bson(update)}\"")
update = Select.make_inc({a={b=[{c=1}]}})
do println("update=\"{Bson.string_of_bson(update)}\"")
_ = Mongo.update(mongo,0,ns,select,update) do err("update")

query = dl([str("a.b.d","abc")])
fields = {some=dl([i32("_id",0)])}
//fields = {some=dl([i32("a.b.$.c",1)])} // <-- gives whole array
/*
fields = {some=dl([doc("a.b",dl([i32("$slice",1)])), // <-- gives element 0 of the list of arrays which have d=abc
                   i32("a.b.d",0),
                   i32("_id",0)
                  ])}
*/
do match Cursor.find_one(mongo,ns,query,fields) with
   | {success=doc} -> println("doc={Bson.string_of_bson(doc)}")
   | err -> println("err={Bson.string_of_result(err)}")
/**/

/*
path = ([{IntKey=0}, {StringKey="abc"}, {AbstractKey=[{Boolean=("key",{true})}]}]:MongoDb.path)
vpath = (List.flatten([path,[{StringKey="value"}]]):MongoDb.path) // <-- need to add this according to the type
select_name = Select.dot_path(vpath)
select(n:int) = ([{Int32=(select_name,n)}]:Bson.document)
update(n:int) = ([{Document=("$set",([{Int32=(select_name,n)}]:Bson.document))}]:Bson.document)
query_name = Select.dot_path(path)
query(v:string) = ([{String=(query_name,v)}]:Bson.document)
do println("path={path}")
do println("select = {Bson.string_of_bson(select(999))}")
do println("update = {Bson.string_of_bson(update(111))}")
do println("query() = {Bson.string_of_bson(query("value"))}")

bson =
  ([{Document=("0",
     ([{Document=("abc",
       ([{Document=("true",([{Int32=("value",999)}]:Bson.document))},
         {Document=("false",([{Int32=("value",888)}]:Bson.document))}
       ]:Bson.document))},
      {Document=("def",
       [{Document=("true",([{Int32=("value",777)}]:Bson.document))},
        {Document=("false",([{Int32=("value",666)}]:Bson.document))}
       ])}
     ]:Bson.document))},
    {Document=("1",
     [{Document=("ghi",
       [{Document=("true",([{Int32=("value",555)}]:Bson.document))},
        {Document=("false",([{Int32=("value",444)}]:Bson.document))}
       ])},
      {Document=("jkl",
       [{Document=("true",([{Int32=("value",333)}]:Bson.document))},
        {Document=("false",([{Int32=("value",222)}]:Bson.document))}
       ])}
     ])}
  ]:Bson.document)

_ = Mongo.insert(mongo,Mongo._Upsert,ns,(bson:Bson.document))
//update = ([{Document=("$set",([{Int32=("0.abc.true.value",111)}]:Bson.document))}]:Bson.document)
_ = Mongo.update(mongo,Mongo._Upsert,ns,select(999),update(111))
err = Cursor.last_error(mongo, "db")
do println("err={Bson.string_of_result(err)}")
do match Cursor.find_one(mongo,ns,query("value"),{none}) with
   | {success=doc} -> println("err={Bson.string_of_bson(doc)}")
   | err -> println("err={Bson.string_of_result(err)}")
*/

type collection('a) = {
  db: mongodb(Bson.document,'a);
  fields: option(Bson.document); // Bury these in here for convenience
  limit: int;
  skip: int;
  flags: int;
}

type collection_cursor('a) = {
  collection: collection('a);
  cursor: cursor
}

type Collection = {{ 
  /* Implements the collection type presented to the user and also the target
   * for the new db syntax.  I would have preferred "Set" to "Collection"
   * because it would have been easier to type but that name is already occupied
   * in the namespace.
   */
  create : mongodb('key,'value) -> collection('value)
  make : mongodb('key,'value), int, int, int, option(Bson.document) -> collection('value)
  set_limit : collection('value), int -> collection('value)
  set_skip : collection('value), int -> collection('value)
  set_flags : collection('value), int -> collection('value)
  set_fields : collection('value), option(Bson.document) -> collection('value)
  destroy : collection('value) -> void
  update : collection('value), select('value), 'value -> bool
  delete : collection('value), select('value) -> bool
  find_one : collection('value), select('value) -> outcome('value,Mongo.failure)
  query : collection('value), select('value) -> outcome(collection_cursor('value),Mongo.failure)
  first : collection_cursor('value) -> outcome('value,Mongo.failure) // <-- will re-issue query
  next : collection_cursor('value) -> outcome('value,Mongo.failure)
  has_more : collection_cursor('value) -> bool
  kill : collection_cursor('value) -> void
}}

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

/*
 * Mdb_make {{ ... }}:
 *
 *   - The main module intended to be used by users.  Here we force type-safety on the
 *     low-level features:  make_db(MDB:{update:mongodb,int,string->void ...}):mongodb(int,string)
 *     (using not very accurate OPA syntax).  This is essentially a null operation just restricting the
 *     types but I think it should be formalised here since we could do some logic on the
 *     given types, if necessary, for more complex operations.
 *
 **/

type Mdb('key,'value) = {{
  // TODO: documentation in here
  close: mongodb('key,'value) -> void
  open: int, string, int -> mongodb('key,'value)
  namespace: mongodb('key,'value), string, string -> mongodb('key,'value)
  insert: mongodb('key,'value), 'key, 'value -> bool
  update: mongodb('key,'value), 'key, 'value -> bool
  find_one: mongodb('key,'value), 'key -> outcome('value,Mongo.failure)
  delete: mongodb('key,'value), 'key -> bool
  ensure_index: mongodb('key,'value), 'key, int -> Mongo.result
  last_error: mongodb('key,'value) -> Mongo.result
}}

/* OK, OK, I know this is ridiculous but it won't let me say:
 
     ISM = (MDB : Mdb(int, string))

   for obvious reasons.  A better choice is to type-specialise at the
   expression level:

     mongodb = (MDB.open(50*1024,"www.localhost.local",27017):mongodb(int,string))

   but, as above, you need to Magic.id it in outer contexts if you want to avoid
   value restriction.  Rank-2 polymorphism, what a waste of time.
*/
Mdb_make(_default_key:'key, _default_value:'value) : Mdb = {{
  close(db:mongodb('key,'value)): void = MDB.close(db)
  open(bufsize:int, addr:string, port:int): mongodb('key,'value) = MDB.open(bufsize, addr, port)
  namespace(db:mongodb('key,'value), dbname:string, collection:string): mongodb('key,'value) =
    MDB.namespace(db, dbname, collection)
  insert(db:mongodb('key,'value), key:'key, value:'value): bool = MDB.insert(db, key, value)
  update(db:mongodb('key,'value), key:'key, value:'value): bool = MDB.update(db, key, value)
  find_one(db:mongodb('key,'value), key:'key): outcome('value,Mongo.failure) = MDB.find_one(db, key)
  delete(db:mongodb('key,'value), key:'key): bool = MDB.delete(db, key)
  ensure_index(db:mongodb('key,'value), key:'key, flags:int): Mongo.result = MDB.ensure_index(db, key, flags)
  last_error(db:mongodb('key,'value)): Mongo.result = MDB.last_error(db)
}}

/* Test code for Mdb_make */
/*
ISM = (Mdb_make(0,"") : Mdb(int,string))

mongodb2_ = ISM.open(50*1024,"www.localhost.local",27017)
mongodb2 = ISM.namespace(mongodb2_,"db","collection")
err = ISM.ensure_index(mongodb2, 0, (Indexes._Sparse+Indexes._Unique))
do println("err(ensure_index) = {Bson.string_of_result(err)}")
ns = "db.collection"
idx = 123
val2 = "abc"
do if ISM.update(mongodb2, idx, val2)
   then println("result(update({ns},{idx},{val2}))=\n  {Bson.string_of_result(ISM.last_error(mongodb2))}")
   else println("update failure")
v = ISM.find_one(mongodb2, idx)
do println("v={v}")
do ISM.close(mongodb2)
do ISM.close(mongodb2_)
*/

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
