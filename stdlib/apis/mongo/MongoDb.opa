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
 */

/*
 * MDB {{ ... }}:
 *
 *   - A low-level module containing essentially the core of the implementation but
 *     allowing untyped versions of functions, for example, the most general write
 *     function would be write : mongodb, 'key, 'value -> void.  This is prefectly implementable
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

  select(db:mongodb('key,'value), indices:'key): Bson.document =
    //do println("index: ty={@typeof(indices)}")
    match @typeof(indices) with
    | {TyName_args = [ty1,ty2]; TyName_ident = "tuple_2"} ->
      indices = (Magic.id(indices):tuple_2('a,'b))
      List.flatten([MongoDb.opa_to_bson(db.idxname^"0",indices.f1,{some=ty1}),
                    MongoDb.opa_to_bson(db.idxname^"1",indices.f2,{some=ty2})])
    | {TyName_args = [ty1,ty2,ty3]; TyName_ident = "tuple_3"} ->
      indices = (Magic.id(indices):tuple_3('a,'b,'c))
      List.flatten([MongoDb.opa_to_bson(db.idxname^"0",indices.f1,{some=ty1}),
                    MongoDb.opa_to_bson(db.idxname^"1",indices.f2,{some=ty2}),
                    MongoDb.opa_to_bson(db.idxname^"2",indices.f3,{some=ty3})])
    | {TyName_args = [ty1,ty2,ty3,ty4]; TyName_ident = "tuple_4"} ->
      indices = (Magic.id(indices):tuple_4('a,'b,'c,'d))
      List.flatten([MongoDb.opa_to_bson(db.idxname^"0",indices.f1,{some=ty1}),
                    MongoDb.opa_to_bson(db.idxname^"1",indices.f2,{some=ty2}),
                    MongoDb.opa_to_bson(db.idxname^"2",indices.f3,{some=ty3}),
                    MongoDb.opa_to_bson(db.idxname^"3",indices.f4,{some=ty4})])
    | { TyName_args=[{TyName_args=[]; TyName_ident="Bson.element"}]; TyName_ident="list" }
    | { TyName_args=_; TyName_ident="Bson.document" } ->
      (Magic.id(indices):Bson.document)
    | {TyName_args = [ty]; TyName_ident = "list"} ->
      indices = (Magic.id(indices):list('a))
      List.flatten(List.mapi((n, index ->
                               MongoDb.opa_to_bson((db.idxname^(if n == 0 then "" else Int.to_string(n))),
                                                   index, {some=ty})),indices))
    | ty ->
      MongoDb.opa_to_bson(db.idxname,Magic.id(indices),{some=ty})

  update(db:mongodb('key,'value), value:'value): Bson.document =
    [{Document=("$set",
      (match (@typeof(value):OpaType.ty) with
       | { TyName_args=[({TyName_args=_; TyName_ident="Bson.element"}:OpaType.ty)]; TyName_ident="list" }
       | { TyName_args=_; TyName_ident="Bson.document" } ->
         (Magic.id(value):Bson.document)
       | ty -> MongoDb.opa_to_bson(db.valname,Magic.id(value),{some=ty})))}]

  write(db:mongodb('key,'value), key:'key, value:'value): bool =
    ns = db.dbname^"."^db.collection
    select = select(db, key)
    update = update(db, value)
    Mongo.update(db.mongo,Mongo._Upsert,ns,select,update)

  /*fields(db:mongodb('key,'value), ty:OpaType.ty): Bson.document =
    match ty with
    | { TyName_args=[({TyName_args=_; TyName_ident="Bson.element"}:OpaType.ty)]; TyName_ident="list" }
    | { TyName_args=_; TyName_ident="Bson.document" } -> []
    | _ -> [{Int32=(db.valname,1)}]*/

  read(db:mongodb('key,'value), key:'key): outcome('value,Mongo.failure) =
    ns = db.dbname^"."^db.collection
    select = select(db, key)
    ty = @typeof(Magic.id(void):'value)
    (match Cursor.find_one(db.mongo,ns,select,{some=List.map((f -> {Int32=(f,0)}),Bson.keys(select))}) with
     | {success=doc} ->
       do println("  doc={Bson.string_of_bson(doc)}\n  ty={OpaType.to_pretty(ty)}")
       (match MongoDb.bson_to_opa(doc, ty, db.valname) with
        | {some=v} -> {success=(Magic.id(v):'value)}
        | {none} -> @fail("MDB.read: not found"))
     | {~failure} -> @fail("MDB.read: error from MongoDB: {failure}"))

  ensure_index(db:mongodb('key,'value), key:'key, flags:int): Mongo.result =
    ns = db.dbname^"."^db.collection
    select = select(db, key)
    index = List.map((e -> {Int32=(Bson.key(e),1)}),select)
    if Indexes.create_index(db.mongo,ns,index,flags)
    then Cursor.last_error(db.mongo, db.dbname)
    else {failure={Error="ensure_index send failure"}}

  last_error(db:mongodb('key,'value)): Mongo.result = Cursor.last_error(db.mongo, db.dbname)

}}

/* Test code for MDB */
/*
mongodb_ = (MDB.open(50*1024,"www.localhost.local",27017):mongodb(int,string))
mongodb = MDB.namespace(mongodb_,"db","collection")

tst(mongodb:mongodb('a,'b), idx:'a, val:'b) =
  ns = mongodb.dbname^"."^mongodb.collection
  err = MDB.ensure_index(mongodb, idx, (Indexes._Sparse+Indexes._Unique))
  do println("err(ensure_index) = {Bson.string_of_result(err)}")
  do if MDB.write(mongodb, idx, val)
     then println("result(write({ns},{idx},{val}))={Bson.string_of_result(MDB.last_error(mongodb))}")
     else println("write failure")
  v = MDB.read(mongodb, idx)
  do println("v={v}")
  void

do tst(Magic.id(mongodb):mongodb((int,int),string), (123,456), "abc")
do tst(Magic.id(mongodb):mongodb(Bson.document,string), [{Int32=("key",123)}], "abc")
do tst(Magic.id(mongodb):mongodb(Bson.document,Bson.document), [{Int32=("_key",456)}], [{String=("s","abc")},{Int32=("i",123)}])
do MDB.close(mongodb)
do MDB.close(mongodb_)
*/

/*
 * MongoDb {{ ... }}:
 *
 *   - The main module intended to be used by users.  Here we force type-safety on the
 *     low-level features:  make_db(MDB:{write:mongodb,int,string->void ...}):mongodb(int,string)
 *     (using not very accurate OPA syntax).  This is essentially a null operation just restricting the
 *     types but I think it should be formalised here since we could do some logic on the
 *     given types, if necessary, for more complex operations.
 *
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
 *
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
