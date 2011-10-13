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

/*
 * MDB {{ ... }}:
 *   
 *   A low-level module allowing management of connections to MongoDB
 *   servers.  To be used by higher-level modules so that only one
 *   connection is opened to a given server whereas several interfaces
 *   such as those defined below can be attached to the open connection.
 *   
 *   TODO: Possibly arrange a map of address:port values to connections?
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

  continueOnError(db:mongodb): mongodb = { db with insert_flags=Bitwise.land(db.insert_flags,Mongo.ContinueOnErrorBit) }
  upsert(db:mongodb): mongodb = { db with update_flags=Bitwise.land(db.update_flags,Mongo.UpsertBit) }
  multiUpdate(db:mongodb): mongodb = { db with update_flags=Bitwise.land(db.update_flags,Mongo.MultiUpdateBit) }
  singleRemove(db:mongodb): mongodb = { db with delete_flags=Bitwise.land(db.delete_flags,Mongo.SingleRemoveBit) }
  tailableCursor(db:mongodb): mongodb = { db with query_flags=Bitwise.land(db.query_flags,Mongo.TailableCursorBit) }
  slaveOk(db:mongodb): mongodb = { db with query_flags=Bitwise.land(db.query_flags,Mongo.SlaveOkBit) }
  oplogReplay(db:mongodb): mongodb = { db with query_flags=Bitwise.land(db.query_flags,Mongo.OplogReplayBit) }
  noCursorTimeout(db:mongodb): mongodb = { db with query_flags=Bitwise.land(db.query_flags,Mongo.NoCursorTimeoutBit) }
  awaitData(db:mongodb): mongodb = { db with query_flags=Bitwise.land(db.query_flags,Mongo.AwaitDataBit) }
  exhaust(db:mongodb): mongodb = { db with query_flags=Bitwise.land(db.query_flags,Mongo.ExhaustBit) }
  partial(db:mongodb): mongodb = { db with query_flags=Bitwise.land(db.query_flags,Mongo.PartialBit) }

  /**
   * opa2doc: We just need to strip off the dummy layer used by dbMongo
   **/
  opa2doc(v:'a): Bson.document =
    match MongoDb.opa_to_bson("value",v,{none}) with
    | [{name="value"; value={Document=doc}}] -> doc
    | doc -> doc

}}

/*
 * Select {{ ... }}:
 *   
 *   A program-level method for constructing Bson documents for select
 *   and update in a friendly manner.
 *   
 */

@abstract type select = {
  select:Bson.document;
  ty:OpaType.ty;
}

type Select = {{
  // TODO: Documentation
  select_to_document : select -> Bson.document

  pretty_of_select : select -> string

  dot_path : MongoDb.path -> string

/*
  path : MongoDb.path -> select
  path_intrange : MongoDb.path, option(int), option(int) -> select 
  check_fst_arg_in_pair : select -> select
  check_field_in_record : select, string -> select
*/

  empty : -> select

  key : string, select -> select
  keyop : string, select -> select
  path : list(string), select -> select
  dot : MongoDb.path, select -> select

  double : select, string, float -> select
  doubleop : select, string, float -> select
  string : select, string, string -> select
  stringop : select, string, string -> select
  doc : select, string, Bson.document -> select
  docop : select, string, Bson.document -> select
  array : select, string, list('b) -> select
  arrayop : select, string, list('b) -> select
  binary : select, string, string -> select
  id : select, string, string -> select
  newid : select, string -> select
  bool : select, string, bool -> select
  boolop : select, string, bool -> select
  date : select, string, Date.date -> select
  null : select, string -> select
  regexp : select, string, string, string -> select
  code : select, string, string -> select
  symbol : select, string, string -> select
  codescope : select, string, string, Bson.document -> select
  int32 : select, string, int -> select
  int32op : select, string, int -> select
  ts : select, string, int, int -> select
  int64 : select, string, int -> select
  int64op : select, string, int -> select

  gti32 : int, select -> select
  lti32 : int, select -> select
  gtei32 : int, select -> select
  ltei32 : int, select -> select
  nei32 : int, select -> select

  gti64 : int, select -> select
  lti64 : int, select -> select
  gtei64 : int, select -> select
  ltei64 : int, select -> select
  nei64 : int, select -> select

  gtd : float, select -> select
  ltd : float, select -> select
  gted : float, select -> select
  lted : float, select -> select
  ned : float, select -> select

  gts : string, select -> select
  lts : string, select -> select
  gtes : string, select -> select
  ltes : string, select -> select
  nes : string, select -> select

  set_op : select, string -> select

  gt : select -> select
  lt : select -> select
  gte : select -> select
  lte : select -> select
  ne : select -> select

  and : select, select -> select
  andalso : list(select) -> select
  or : select, select -> select
  orelse : list(select) -> select
  nor : select, select -> select
  noreither : list(select) -> select

  all : select, list('b) -> select
  in : select, list('b) -> select
  nin : select, list('b) -> select

  exists : select, string, bool -> select

  mod : select, 'b, 'b -> select

  size : select, int -> select
  typ : select, int -> select

  regex : select, string, string -> select

  inc : select -> select
  set : select -> select
  unset : select -> select
  push : select -> select
  pushAll : select -> select
  addToSet : select -> select
  pop : select -> select
  pull : select -> select
  pullAll : select -> select
  rename : select -> select
  bit : select -> select

  elemMatch : select -> select

  not : select -> select

  where : select, string -> select

  returnKey : select, bool -> select
  maxScan : select, int -> select
  query : select, Bson.document -> select
  orderby : select, Bson.document -> select
  explain : select, bool -> select
  snapshot : select, bool -> select
  min : select, Bson.document -> select
  max : select, Bson.document -> select
  showDiskLoc : select, bool -> select
  hint : select, Bson.document -> select
  comment : select, string -> select
}}

Select : Select = {{

  select_to_document(select:select): Bson.document = select.select

  pretty_of_select(select:select): string = "\{select={Bson.pretty_of_bson(select.select)}; ty={OpaType.to_pretty(select.ty)}\}"

  @private
  string_of_element(e:Bson.element): string =
    match e with
    | {value={Int32=i} ...} -> Int.to_string(i)
    | {value={Int64=i} ...} -> Int.to_string(i)
    | {value={String=s} ...} -> s
    | {value={Boolean=b} ...} -> Bool.to_string(b)
    | {value={Document=d} ...} -> String.concat(".",List.map(string_of_element,d))
    | {value={Array=a} ...} -> String.concat("_",List.map(string_of_element,a))
    | {value={Null=_} ...} -> "" // <-- ???
    | _ -> @fail

  @private
  string_of_key(key:MongoDb.key): string =
    match key with
    | {IntKey=i} -> Int.to_string(i)
    | {StringKey=s} -> s
    | {AbstractKey=a} -> String.concat(".",List.map(string_of_element,a))

  dot_path(path:MongoDb.path): string =
    String.concat(".",List.map(string_of_key,path))

/* Type support */
  @private tempty = {TyRecord_row=[]}
  @private tvar(tv) = {TyRecord_row=[]; TyRecord_rowvar=tv}
  @private nottvar(ty) = match ty with | {TyRecord_row=[]; TyRecord_rowvar=_} -> true | _ -> false
  @private tvoid = {TyName_args=[]; TyName_ident="void"}
  @private tint = {TyConst={TyInt={}}}
  @private tstring = {TyConst={TyString={}}}
  @private tfloat = {TyConst={TyFloat={}}}
  @private tbool = {TyName_args=[]; TyName_ident="bool"}
  @private tdate = {TyName_args=[]; TyName_ident="Date.date"}
  @private tdoc = {TyName_args=[]; TyName_ident="Bson.document"}
  @private ttup2(ty1:OpaType.ty,ty2:OpaType.ty):OpaType.ty = {TyName_args=[ty1, ty2]; TyName_ident="tuple_2"}
  @private tlist(ty:OpaType.ty):OpaType.ty = {TyName_args=[ty]; TyName_ident="list"}
  @private trec(label, ty) = {TyRecord_row=[~{label; ty}]}
  @private taddrec(rty,label,ty) =
    match rty with
    | {TyRecord_row=row} -> {TyRecord_row=List.sort_by((r -> r.label),[~{label; ty}|row])}
    | _ -> @fail
  @private order_field(f1, f2): Order.ordering = String.ordering(f1.label,f2.label)
  @private FieldSet = Set_make(((Order.make(order_field):order(OpaType.field,Order.default))))
  @private diff(s1,s2) = FieldSet.fold(FieldSet.remove,s2,s1)
  @private tmrgrecs(rec1, rec2) =
    //do println("rec1={OpaType.to_pretty(rec1)}\nrec2={OpaType.to_pretty(rec2)}")
    if rec1 == rec2
    then rec1
    else
      match (rec1,rec2) with
      | ({TyRecord_row=row1},{TyRecord_row=row2}) ->
        s1 = FieldSet.From.list(row1)
        s2 = FieldSet.From.list(row2)
        i = FieldSet.intersection(s1,s2)
        //do println("  i={OpaType.to_pretty({TyRecord_row=FieldSet.To.list(i)})}")
        if FieldSet.is_empty(i)
        then {TyRecord_row=List.sort_by((r -> r.label),List.flatten([row1,row2]))}
        else
          ii = FieldSet.fold((f, l ->
                                match (FieldSet.get(f,s1),FieldSet.get(f,s2)) with
                                | ({some=f1},{some=f2}) ->
                                   [{label=f1.label; ty=tmrgrecs(f1.ty,f2.ty)}|l]
                                | _ -> @fail),i,[])
          d = FieldSet.To.list(FieldSet.union(diff(s1,s2),diff(s2,s1)))
          //do println("  ii={OpaType.to_pretty({TyRecord_row=ii})}")
          //do println("  d={OpaType.to_pretty({TyRecord_row=d})}")
          res = {TyRecord_row=List.sort_by((r -> r.label),List.flatten([ii,d]))}
          //do println("  res={OpaType.to_pretty(res)}")
          res
      | _ -> @fail
  @private taddcol(cty,row) =
    match (cty,row) with
    | ({TySum_col=cols},{TyRecord_row=row}) -> {TySum_col=[row|cols]}
/* End of type support */

  empty(): select = {select=[]; ty=tempty}

  key(name:string, s:select): select = { select=[H.doc(name,s.select)]; ty=taddrec(tempty,name,s.ty) }
  keyop(name:string, s:select): select = { select=[H.doc(name,s.select)]; ty=s.ty }

  path(path:list(string), s:select): select =
    List.fold_right((s, name -> { s with select=[H.doc(name,s.select)] }),path,s)

  dot(mpath:MongoDb.path, s:select): select =
    path(List.map(string_of_key,mpath), s)

  double(s:select, name:string, d:float): select = { select=[H.dbl(name,d)|s.select]; ty=taddrec(s.ty,name,tfloat) }
  doubleop(s:select, name:string, d:float): select =
    if s.ty != tfloat && s.ty != tempty && nottvar(s.ty)
    then @fail
    else { select=[H.dbl(name,d)|s.select]; ty=tfloat }
  string(s:select, name:string, str:string): select = { select=[H.str(name,str)|s.select]; ty=taddrec(s.ty,name,tstring) }
  stringop(s:select, name:string, str:string): select =
    if s.ty != tstring && s.ty != tempty && nottvar(s.ty)
    then @fail
    else { select=[H.str(name,str)|s.select]; ty=tstring }
  doc(s:select, name:string, d:Bson.document): select = { s with select=[H.doc(name,d)|s.select] } //typeofdoc(d)!!!
  docop(s:select, name:string, d:Bson.document): select = { select=[H.doc(name,d)|s.select]; ty=s.ty }
  array(s:select, name:string, l:list('b)): select =
    ty = @typeval('b)
    d = (List.flatten(List.mapi((i, v -> MongoDb.opa_to_bson("{i}",v,{some=ty})),l)):Bson.document)
    { select=[H.arr(name,d)|s.select]; ty=taddrec(s.ty,name,tlist(ty)) }
  arrayop(s:select, name:string, l:list('b)): select =
    ty = @typeval('b)
    if s.ty != tlist(ty) && s.ty != tempty && nottvar(s.ty) && nottvar(ty)
    then @fail
    else
      d = (List.flatten(List.mapi((i, v -> MongoDb.opa_to_bson("{i}",v,{some=ty})),l)):Bson.document)
      { select=[H.arr(name,d)|s.select]; ty=tlist(ty) }
  binary(s:select, name:string, bin:string): select = { select=[H.binary(name,bin)|s.select]; ty=taddrec(s.ty,name,tstring) }
  id(s:select, name:string, id:string): select =
    { select=[H.oid(name,Bson.oid_of_string(id))|s.select]; ty=taddrec(s.ty,name,tstring) }
  newid(s:select, name:string): select = { select=[H.oid(name,Bson.new_oid(void))|s.select]; ty=taddrec(s.ty,name,tstring) }
  bool(s:select, name:string, b:bool): select = { select=[H.bool(name,b)|s.select]; ty=taddrec(s.ty,name,tbool) }
  boolop(s:select, name:string, b:bool): select =
    if s.ty != tbool && s.ty != tempty && nottvar(s.ty)
    then @fail
    else { select=[H.bool(name,b)|s.select]; ty=tbool }
  date(s:select, name:string, d:Date.date): select = { select=[H.date(name,d)|s.select]; ty=taddrec(s.ty,name,tdate) }
  null(s:select, name:string): select = { select=[H.null(name)|s.select]; ty=taddrec(s.ty,name,tvoid) }
  regexp(s:select, name:string, re:string, opts:string): select =
    { select=[H.regexp(name,(re,opts))|s.select]; ty=taddrec(s.ty,name,ttup2(tstring,tstring)) }
  code(s:select, name:string, c:string): select = { select=[H.code(name,c)|s.select]; ty=taddrec(s.ty,name,tstring) }
  symbol(s:select, name:string, sym:string): select = { select=[H.symbol(name,sym)|s.select]; ty=taddrec(s.ty,name,tstring) }
  codescope(s:select, name:string, c:string, sc:Bson.document): select =
    { s with select=[H.codescope(name,(c,sc))|s.select]; ty=taddrec(s.ty,name,ttup2(tstring,tdoc)) }
  int32(s:select, name:string, i:int): select = { select=[H.i32(name,i)|s.select]; ty=taddrec(s.ty,name,tint) }
  int32op(s:select, name:string, i:int): select =
    if s.ty != tint && s.ty != tempty && nottvar(s.ty)
    then @fail
    else { select=[H.i32(name,i)|s.select]; ty=tint }
  ts(s:select, name:string, t:int, i:int): select = { s with select=[H.timestamp(name,(t,i))|s.select] }
  int64(s:select, name:string, i:int): select = { select=[H.i64(name,i)|s.select]; ty=taddrec(s.ty,name,tint) }
  int64op(s:select, name:string, i:int): select =
    if s.ty != tint && s.ty != tempty && nottvar(s.ty)
    then @fail
    else { select=[H.i64(name,i)|s.select]; ty=tint }

  gti32(i:int, s:select): select = int32op(s, "$gt", i)
  lti32(i:int, s:select): select = int32op(s, "$lt", i)
  gtei32(i:int, s:select): select = int32op(s, "$gte", i)
  ltei32(i:int, s:select): select = int32op(s, "$lte", i)
  nei32(i:int, s:select): select = int32op(s, "$ne", i)

  gti64(i:int, s:select): select = int64op(s, "$gt", i)
  lti64(i:int, s:select): select = int64op(s, "$lt", i)
  gtei64(i:int, s:select): select = int64op(s, "$gte", i)
  ltei64(i:int, s:select): select = int64op(s, "$lte", i)
  nei64(i:int, s:select): select = int64op(s, "$ne", i)

  gtd(d:float, s:select): select = doubleop(s, "$gt", d)
  ltd(d:float, s:select): select = doubleop(s, "$lt", d)
  gted(d:float, s:select): select = doubleop(s, "$gte", d)
  lted(d:float, s:select): select = doubleop(s, "$lte", d)
  ned(d:float, s:select): select = doubleop(s, "$ne", d)

  gts(str:string, s:select): select = stringop(s, "$gt", str)
  lts(str:string, s:select): select = stringop(s, "$lt", str)
  gtes(str:string, s:select): select = stringop(s, "$gte", str)
  ltes(str:string, s:select): select = stringop(s, "$lte", str)
  nes(str:string, s:select): select = stringop(s, "$ne", str)

  set_op(s:select, op:string): select =
    select =
      ((match s.select with
        | [] -> []
        | [e] -> [H.doc(Bson.key(e),[Bson.set_key(e,op)])]
        | l -> List.map((e -> H.doc(Bson.key(e),[Bson.set_key(e,op)])),l)):Bson.document)
    { s with ~select }

  gt(s:select): select = set_op(s, "$gt")
  lt(s:select): select = set_op(s, "$lt")
  gte(s:select): select = set_op(s, "$gte")
  lte(s:select): select = set_op(s, "$lte")
  ne(s:select): select = set_op(s, "$ne")

  @private
  boolop_private(op:string, s1:select, s2:select): select =
    { select=([H.arr(op,([H.doc("0",s1.select),H.doc("1",s2.select)]:Bson.document))]:Bson.document);
      ty=tmrgrecs(s1.ty,s2.ty) }

  @private
  lboolop_private(op:string, ss:list(select)): select =
    match ss with
    | [] -> empty()
    | [s|t] ->
       (doc,ty) = List.fold_index((i, ss, (doc,ty) -> ([H.doc("{i}",ss.select)|doc],tmrgrecs(ss.ty,ty))),[s|t],([],s.ty))
       { select=[H.arr(op,(doc:Bson.document))]; ~ty }

  and(s1:select, s2:select): select = boolop_private("$and",s1,s2)
  andalso(ss:list(select)): select = lboolop_private("$and",ss)
  or(s1:select, s2:select): select = boolop_private("$or",s1,s2)
  orelse(ss:list(select)): select = lboolop_private("$or",ss)
  nor(s1:select, s2:select): select = boolop_private("$nor",s1,s2)
  noreither(ss:list(select)): select = lboolop_private("$nor",ss)

  all(s:select, a:list('b)): select = arrayop(s, "$all", a)
  in(s:select, a:list('b)): select = arrayop(s, "$in", a)
  nin(s:select, a:list('b)): select = arrayop(s, "$nin", a)

  @private docbool(s:select, name:string, op:string, tf:bool): select = doc(s,name,[H.bool(op,tf)])

  exists(s:select, name:string, tf:bool): select = docbool(s, name, "$exists", tf)

  mod(s:select, x:'b, y:'b): select = arrayop(s, "$mod", [x,y])

  size(s:select, x:int): select = int64op(s, "$size", x)
  typ(s:select, t:int): select = int64op(s, "$type", t)

  regex(s:select, re:string, opts:string): select = { s with select=[H.regexp("$regex",(re,opts))|s.select] }

  inc(s:select): select = keyop("$inc",s)
  set(s:select): select = keyop("$set",s)
  unset(s:select): select = keyop("$unset",s)
  push(s:select): select = keyop("$push",s)
  pushAll(s:select): select = keyop("$pushAll",s)
  addToSet(s:select): select = keyop("$addToSet",s)
  pop(s:select): select = keyop("$pop",s)
  pull(s:select): select = keyop("$pull",s)
  pullAll(s:select): select = keyop("$pullAll",s)
  rename(s:select): select = keyop("$rename",s)
  bit(s:select): select = keyop("$bit",s)

  elemMatch(s:select): select = keyop("$elemMatch",s)

  not(s:select): select = keyop("$not",s)

  where(s:select, whr:string): select = { s with select=[H.code("$where",whr)|s.select] }

  returnKey(s:select, tf:bool): select = boolop(s, "$returnKey", tf)
  maxScan(s:select, i:int): select = int64op(s, "$maxScan", i)
  query(s:select, d:Bson.document): select = docop(s, "$query", d)
  orderby(s:select, d:Bson.document): select = docop(s, "$orderby", d)
  explain(s:select, tf:bool): select = boolop(s, "$explain", tf)
  snapshot(s:select, tf:bool): select = boolop(s, "$snapshot", tf)
  min(s:select, d:Bson.document): select = docop(s, "$min", d)
  max(s:select, d:Bson.document): select = docop(s, "$max", d)
  showDiskLoc(s:select, tf:bool): select = boolop(s, "$showDiskLoc", tf)
  hint(s:select, d:Bson.document): select = docop(s, "$hint", d)
  comment(s:select, c:string): select = stringop(s, "$comment", c)

}}

/*
 * Collection {{ ... }}:
 *   
 * Implements the collection type presented to the user and also the target
 * for the new db syntax.  I would have preferred "Set" to "Collection"
 * because it would have been easier to type but that name is already occupied
 * in the namespace.
 *
 * Essentially, this datatype is simply a "typed" view of the low-level MongoDB
 * driver routines.  The currency here is OPA values, not BSON documents hence
 * we need to give a type to the collection.
 *
 **/

type batch = list(Bson.document)

type collection('a) = {
  db: mongodb;
}

type collection_cursor('a) = {
  collection: collection('a);
  cursor: cursor;
  query: select;
  ty: OpaType.ty;
}

type group('a) = { retval:list('a); count:int; keys:int; ok:int }
type group_result('a) = outcome(group('a),Mongo.failure)

type Collection = {{
  // TODO: Documentation
  create : mongodb -> collection('value)
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
  update : collection('value), select, select -> bool
  delete : collection('value), select -> bool
  find_one : collection('value), select -> outcome('value,Mongo.failure)
  query : collection('value), select -> outcome(collection_cursor('value),Mongo.failure)
  first : collection_cursor('value) -> outcome(collection_cursor('value),Mongo.failure)
  next : collection_cursor('value) -> (collection_cursor('value),outcome('value,Mongo.failure))
  find_all : collection('value), select -> outcome(list('value),Mongo.failure)
  has_more : collection_cursor('value) -> bool
  count : collection('value), option(select) -> outcome(int,Mongo.failure)
  distinct : collection('value), string, option(select) -> outcome(list('b),Mongo.failure)
  group : collection('value), Bson.document, string, Bson.document, option(Bson.document), option(string) -> Mongo.result
  analyze_group : Mongo.result -> group_result('a)
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

  create(db:mongodb): collection('value) = { db=MDB.clone(db) }

  destroy(c:collection('value)): void = MDB.close(c.db)

  skip(c:collection('value), skip:int): collection('value) = {c with db={ c.db with ~skip }}
  limit(c:collection('value), limit:int): collection('value) = {c with db={ c.db with ~limit }}
  fields(c:collection('value), fields:option(Bson.document)): collection('value) = {c with db={ c.db with ~fields }}

  continueOnError(c:collection('value)): collection('value) =
    {c with db={ c.db with insert_flags=Bitwise.land(c.db.insert_flags,Mongo.ContinueOnErrorBit) }}
  upsert(c:collection('value)): collection('value)
    = {c with db={ c.db with update_flags=Bitwise.land(c.db.update_flags,Mongo.UpsertBit) }}
  multiUpdate(c:collection('value)): collection('value)
    = {c with db={ c.db with update_flags=Bitwise.land(c.db.update_flags,Mongo.MultiUpdateBit) }}
  singleRemove(c:collection('value)): collection('value)
    = {c with db={ c.db with delete_flags=Bitwise.land(c.db.delete_flags,Mongo.SingleRemoveBit) }}
  tailableCursor(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.land(c.db.query_flags,Mongo.TailableCursorBit) }}
  slaveOk(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.land(c.db.query_flags,Mongo.SlaveOkBit) }}
  oplogReplay(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.land(c.db.query_flags,Mongo.OplogReplayBit) }}
  noCursorTimeout(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.land(c.db.query_flags,Mongo.NoCursorTimeoutBit) }}
  awaitData(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.land(c.db.query_flags,Mongo.AwaitDataBit) }}
  exhaust(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.land(c.db.query_flags,Mongo.ExhaustBit) }}
  partial(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.land(c.db.query_flags,Mongo.PartialBit) }}

  insert(c:collection('value), v:'value): bool =
    ns = c.db.dbname^"."^c.db.collection
    b = MDB.opa2doc(v)
    Mongo.insert(c.db.mongo,c.db.insert_flags,ns,b)

  insert_batch(c:collection('value), b:batch): bool =
    ns = c.db.dbname^"."^c.db.collection
    Mongo.insert_batch(c.db.mongo,c.db.insert_flags,ns,b)

  update(c:collection('value), select:select, update:select): bool =
    ns = c.db.dbname^"."^c.db.collection
    Mongo.update(c.db.mongo,c.db.update_flags,ns,select.select,update.select)

  delete(c:collection('value), select:select): bool =
    ns = c.db.dbname^"."^c.db.collection
    Mongo.delete(c.db.mongo,c.db.delete_flags,ns,select.select)

  find_one(c:collection('value), select:select): outcome('value,Mongo.failure) =
    ns = c.db.dbname^"."^c.db.collection
    (match Cursor.find_one(c.db.mongo,ns,select.select,c.db.fields) with
     | {success=doc} ->
       //do println("  doc={Bson.string_of_bson(doc)}\n  ty={OpaType.to_pretty(ty)}")
       (match MongoDb.bson_to_opa(doc, @typeval('value), c.db.valname) with
        | {some=v} -> {success=(Magic.id(v):'value)}
        | {none} -> {failure={Error="Collection.find_one: not found"}})
     | {~failure} -> {~failure})

  query(c:collection('value), select:select): outcome(collection_cursor('value),Mongo.failure) =
    ns = c.db.dbname^"."^c.db.collection
    match Cursor.find(c.db.mongo,ns,select.select,c.db.fields,c.db.limit,c.db.skip,c.db.query_flags) with
    | {success=cursor} -> {success={collection=c; ~cursor; query=select; ty=@typeval('value) }}
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

  find_all(c:collection('value), select:select): outcome(list('value),Mongo.failure) =
    match query(c,select) with
    | {success=cc} ->
       (cc,l) =
         while((cc,{success=[]}),
               ((cc,l) ->
                  match l with
                  | {success=l} ->
                     (match next(cc) with
                      | (cc,{success=v}) ->
                         //do println("  v={v}")
                         ((cc,{success=[v|l]}),has_more(cc))
                      | (cc,{~failure}) ->
                         //do println("  err(query)={Bson.string_of_failure(failure)}")
                         ((cc,{~failure}),false))
                  | {~failure} -> ((cc,{~failure}),false)))
       _ = kill(cc)
       l
    | {~failure} -> {~failure}

  count(c:collection('value), query_opt:option(select)): outcome(int,Mongo.failure) =
    Cursor.count(c.db.mongo, c.db.dbname, c.db.collection, (Option.map((s -> s.select),query_opt)))

  distinct(c:collection('value), key:string, query_opt:option(select)): outcome(list('b),Mongo.failure) =
    match Cursor.distinct(c.db.mongo, c.db.dbname, c.db.collection, key, (Option.map((s -> s.select),query_opt))) with
    | {success=doc} ->
       // possibly: get the type from 'value and get the key type out of there???
       ty = {TyName_args=[@typeval('b)]; TyName_ident="list"}
       (match MongoDb.bson_to_opa(doc, ty, "values") with
        | {some=v} -> {success=(Magic.id(v):list('b))}
        | {none} -> {failure={Error="Collection.distinct: not found"}})
    | {~failure} -> {~failure}

  /**
   * Note that for group to work ints have to match, Int32 will not match Int64!!!
   **/
  group(c:collection('value), key:Bson.document, reduce:string, initial:Bson.document,
        cond_opt:option(Bson.document), finalize_opt:option(string)): Mongo.result =
    Cursor.group(c.db.mongo, c.db.dbname, c.db.collection, key, reduce, initial, cond_opt, finalize_opt)

  analyze_group(res:Mongo.result): group_result('a) =
    match res with
    | {success=doc} ->
      (match Bson.find(doc,"retval") with
       | {some=[{name=k; value={Array=arr}}]} ->
          ty = {TyName_args=[@typeval('a)]; TyName_ident="list"}
          (match MongoDb.bson_to_opa([H.arr(k,List.rev(arr))], ty, k) with
           | {some=v} ->
              retval = (Magic.id(v):list('a))
              (match Cursor.find_int(doc, "count") with
               | {success=count} ->
                  (match Cursor.find_int(doc, "keys") with
                   | {success=keys} ->
                      (match Cursor.find_int(doc, "ok") with
                       | {success=ok} ->
                          {success=~{retval; count; keys; ok}}
                       | {~failure} -> {~failure})
                   | {~failure} -> {~failure})
               | {~failure} -> {~failure})
           | {none} -> {failure={Error="Collection.analyze_group: not found"}})
       | _ -> {failure={Error="Collection.analyze_group: no retval value in reply"}})
    | {~failure} -> {~failure}

  kill(cc:collection_cursor('value)): collection_cursor('value) = { cc with cursor=Cursor.reset(cc.cursor) }

}}

/** Later:
 *
 * MongoMap = {{
 *   // Implementation of map using underlying MongoDB.
 * }}
 * 
 * MongoArray = {{
 *   // Implementation of Array using underlying MongoDB.
 * }}
 * 
 * MongoTree = {{
 *   // Implementation of Tree using underlying MongoDB.
 *   // Note that there is an interesting page on the MongoDB
 *   // website on embedding trees in MongoDB:
 *   // http://www.mongodb.org/display/DOCS/Trees+in+MongoDB
 * }}
 * 
 * Db = {{
 *   - This will look like the current Db module (as far as we can implement it).  Mostly you
 *     will get: read/write/remove/{int/string}map_search/{int/string}map_fold_range.  Note that
 *     implementing history with MongoDB could prove heavy-weight and potentially disastrous
 *     for performance although I'm sure it could be done.  A primitive form of transaction,
 *     however, might be a good option.  Modification time could be handled low-level in
 *     the driver at the expense of some peformance.
 * }}
 *  
 **/

// End of file MongoDb.opa
