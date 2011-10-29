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

import stdlib.core.{compare}

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
  orderby: option(Bson.document);
  limit: int;
  skip: int;
  insert_flags: int;
  update_flags: int;
  delete_flags: int;
  query_flags: int;
}

type MDB = {{
  // TODO: Documentation
  open : int, string, int -> outcome(mongodb,Mongo.failure)
  repl : string, int, list(mongo_host) -> outcome(mongodb,Mongo.failure)
  clone : mongodb -> mongodb
  namespace : mongodb, string, string -> mongodb
  log : mongodb, bool -> mongodb
  close : mongodb -> void
  getLastError : mongodb -> Mongo.result
  err : mongodb, string -> void
  limit : mongodb, int -> mongodb
  skip : mongodb, int -> mongodb
  fields : mongodb, option(Bson.document) -> mongodb
  orderby : mongodb, option(Bson.document) -> mongodb
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
}}

MDB : MDB = {{

  @private
  open_(dbo:outcome(Mongo.db,Mongo.failure)): outcome(mongodb,Mongo.failure) =
    match dbo with
    | {success=mongo} ->
       (match mongo.primary.get() with
        | {some=(addr,port)} ->
           db = {~mongo; bufsize=mongo.bufsize; ~addr; ~port; link_count=Mutable.make(1);
                 keyname="key"; valname="value"; idxname="index";
                 dbname="db"; collection="collection";
                 fields={none}; orderby={none}; limit=0; skip=0;
                 insert_flags=0; update_flags=0; delete_flags=0; query_flags=0;
                }
           do System.at_exit( ->
                               if db.link_count.get() > 0
                               then
                                 do ML.info("MDB.open","closing mongo (exit) {db.link_count.get()}",void)
                                 _ = Mongo.close(db.mongo) 
                                 void
                               else void)
           {success=db}
        | {none} -> {failure={Error="MDB.open: no primary"}})
    | {~failure} -> {~failure}

  open(bufsize:int, addr:string, port:int): outcome(mongodb,Mongo.failure) =
    open_(Mongo.open(bufsize,addr,port,false))

  repl(name:string, bufsize:int, seeds:list(mongo_host)): outcome(mongodb,Mongo.failure) =
    open_(ReplSet.connect(ReplSet.init(name,bufsize,false,seeds)))

  clone(db:mongodb): mongodb =
    do db.link_count.set(db.link_count.get()+1)
    db

  namespace(db:mongodb, dbname:string, collection:string): mongodb =
    do db.link_count.set(db.link_count.get()+1)
    { db with ~dbname; ~collection }

  log(db:mongodb, log:bool): mongodb =
    { db with mongo={ db.mongo with ~log } }

  close(db:mongodb): void =
    lc = db.link_count.get()
    if lc > 0
      then
        do db.link_count.set(lc-1)
        if lc <= 1
        then
          do ML.info("MDB.close","closing mongo (close) {db.link_count.get()}",void)
          _ = Mongo.close(db.mongo)
          void
        else void
      else void

  getLastError(db:mongodb): Mongo.result = Commands.getLastError(db.mongo, db.dbname)

  err(db:mongodb, n:string): void =
    err = Commands.getLastError(db.mongo, db.dbname)
    if Mongo.isError(err) then println("Error({n})={Mongo.string_of_result(err)}")

  skip(db:mongodb, skip:int): mongodb = { db with ~skip }
  limit(db:mongodb, limit:int): mongodb = { db with ~limit }
  fields(db:mongodb, fields:option(Bson.document)): mongodb = { db with ~fields }
  orderby(db:mongodb, orderby:option(Bson.document)): mongodb = { db with ~orderby }

  continueOnError(db:mongodb): mongodb = { db with insert_flags=Bitwise.lor(db.insert_flags,Mongo.ContinueOnErrorBit) }
  upsert(db:mongodb): mongodb = { db with update_flags=Bitwise.lor(db.update_flags,Mongo.UpsertBit) }
  multiUpdate(db:mongodb): mongodb = { db with update_flags=Bitwise.lor(db.update_flags,Mongo.MultiUpdateBit) }
  singleRemove(db:mongodb): mongodb = { db with delete_flags=Bitwise.lor(db.delete_flags,Mongo.SingleRemoveBit) }
  tailableCursor(db:mongodb): mongodb = { db with query_flags=Bitwise.lor(db.query_flags,Mongo.TailableCursorBit) }
  slaveOk(db:mongodb): mongodb = { db with query_flags=Bitwise.lor(db.query_flags,Mongo.SlaveOkBit) }
  oplogReplay(db:mongodb): mongodb = { db with query_flags=Bitwise.lor(db.query_flags,Mongo.OplogReplayBit) }
  noCursorTimeout(db:mongodb): mongodb = { db with query_flags=Bitwise.lor(db.query_flags,Mongo.NoCursorTimeoutBit) }
  awaitData(db:mongodb): mongodb = { db with query_flags=Bitwise.lor(db.query_flags,Mongo.AwaitDataBit) }
  exhaust(db:mongodb): mongodb = { db with query_flags=Bitwise.lor(db.query_flags,Mongo.ExhaustBit) }
  partial(db:mongodb): mongodb = { db with query_flags=Bitwise.lor(db.query_flags,Mongo.PartialBit) }

}}

/* Type support */
TypeSelect = {{

  tempty = {TyRecord_row=[]}
  tvar(tv) = {TyRecord_row=[]; TyRecord_rowvar=tv}
  istvar(ty) = match ty with | {TyRecord_row=[]; TyRecord_rowvar=_} -> true | _ -> false
  tvoid = {TyName_args=[]; TyName_ident="void"}
  tint = {TyConst={TyInt={}}}
  tstring = {TyConst={TyString={}}}
  tfloat = {TyConst={TyFloat={}}}
  tbool = {TyName_args=[]; TyName_ident="bool"}
  tnumeric = {TyName_args=[]; TyName_ident="Bson.numeric"} // pseudo type
  tdate = {TyName_args=[]; TyName_ident="Date.date"}
  toid = {TyName_args=[]; TyName_ident="Bson.oid"}
  tbinary = {TyName_args=[]; TyName_ident="Bson.binary"}
  tregexp = {TyName_args=[]; TyName_ident="Bson.regexp"}
  tcode = {TyName_args=[]; TyName_ident="Bson.code"}
  tsymbol = {TyName_args=[]; TyName_ident="Bson.symbol"}
  tcodescope = {TyName_args=[]; TyName_ident="Bson.codescope"}
  ttimestamp = {TyName_args=[]; TyName_ident="Bson.timestamp"}
  tvalue = {TyName_args=[]; TyName_ident="Bson.value"}
  telement = {TyName_args=[]; TyName_ident="Bson.element"}
  tdoc = {TyName_args=[]; TyName_ident="Bson.document"}
  ttup2(ty1:OpaType.ty,ty2:OpaType.ty):OpaType.ty = {TyName_args=[ty1, ty2]; TyName_ident="tuple_2"}
  tlist(ty:OpaType.ty):OpaType.ty = {TyName_args=[ty]; TyName_ident="list"}
  trec(label, ty) = {TyRecord_row=[~{label; ty}]}
  tsortrec(ty) =
    match ty with
    | {TyRecord_row=row; ...} -> {ty with TyRecord_row=List.sort_by((r -> r.label),row)}
    | ty -> ty
  order_field(f1, f2): Order.ordering = String.ordering(f1.label,f2.label)
  FieldSet = Set_make(((Order.make(order_field):order(OpaType.field,Order.default))))
  diff(s1,s2) = FieldSet.fold(FieldSet.remove,s2,s1)
  tmrgrecs(rec1, rec2) =
    //dbg do println("rec1={OpaType.to_pretty(rec1)}\nrec2={OpaType.to_pretty(rec2)}")
    if rec1 == rec2 || rec2 == tempty
    then rec1
    else if rec1 == tempty
    then rec2
    else
      match (rec1,rec2) with
      | ({TyRecord_row=row1},{TyRecord_row=row2}) ->
        s1 = FieldSet.From.list(row1)
        s2 = FieldSet.From.list(row2)
        i = FieldSet.intersection(s1,s2)
        //dbg do println("  i={OpaType.to_pretty({TyRecord_row=FieldSet.To.list(i)})}")
        if FieldSet.is_empty(i)
        then {TyRecord_row=List.sort_by((r -> r.label),List.flatten([row1,row2]))}
        else
          ii = FieldSet.fold((f, l ->
                                match (FieldSet.get(f,s1),FieldSet.get(f,s2)) with
                                | ({some=f1},{some=f2}) -> [{label=f1.label; ty=tmrgrecs(f1.ty,f2.ty)}|l]
                                | _ -> @fail/*Can't happen*/),i,[])
          d = FieldSet.To.list(FieldSet.union(diff(s1,s2),diff(s2,s1)))
          //dbg do println("  ii={OpaType.to_pretty({TyRecord_row=ii})}")
          //dbg do println("  d={OpaType.to_pretty({TyRecord_row=d})}")
          res = {TyRecord_row=List.sort_by((r -> r.label),List.flatten([ii,d]))}
          //dbg do println("  res={OpaType.to_pretty(res)}")
          res
      | _ ->
        rec1str = OpaType.to_pretty(rec1)
        rec2str = OpaType.to_pretty(rec2)
        ML.fatal("TypeSelect.tmrgrecs","Attempt to merge non-record types {rec1str} and {rec2str}",-1)
  taddcol(cty,row) =
    match (cty,row) with
    | ({TySum_col=cols},{TyRecord_row=row}) -> {TySum_col=[row|cols]}
  in_row(label,row) =
    List.exists((f -> f.label == label),row)
  find_label_in_col(label,col) =
    List.find((crow -> in_row(label,crow)),col)
  find_row_in_col(row,col) =
    all_in_row(row1,row2) = List.for_all((f -> in_row(f.label,row2)),row1)
    List.find((crow -> all_in_row(row,crow)),col)
  rec naive_type_compare(ty1:OpaType.ty, ty2:OpaType.ty): bool =
    compare_consts(c1,c2) =
      (match (c1,c2) with
       | ({TyInt={}},{TyInt={}}) -> true
       | ({TyString={}},{TyString={}}) -> true
       | ({TyFloat={}},{TyFloat={}}) -> true
       | _ -> false)
    compare_rows(row1,row2) =
       (match List.for_all2((f1, f2 -> f1.label == f2.label && naive_type_compare(f1.ty,f2.ty)),row1,row2) with
        | {result=tf} -> tf
        | _ -> false)
    match (ty1,ty2) with
    | ({TyConst=const1},{TyConst=const2}) -> compare_consts(const1,const2)
    | ({TyName_args=[]; TyName_ident=i1},{TyName_args=[]; TyName_ident=i2}) -> i1 == i2
    | ({TyName_args=a1; TyName_ident=i1},_) -> naive_type_compare(OpaType.type_of_name(i1, a1),ty2)
    | (_,{TyName_args=a2; TyName_ident=i2}) -> naive_type_compare(ty1,OpaType.type_of_name(i2, a2))
    | ({TyRecord_row=row1 ...},{TyRecord_row=row2 ...}) -> compare_rows(row1,row2)
    | ({TySum_col=col1 ...},{TySum_col=col2 ...}) ->
       (match List.for_all2((r1, r2 -> compare_rows(r1,r2)),col1,col2) with | {result=tf} -> tf | _ -> false)
    | _ -> ML.fatal("TypeSelect.naive_type_compare","Can't compare {OpaType.to_pretty(ty1)} and {OpaType.to_pretty(ty2)}",-1)
  name_type(ty:OpaType.ty): OpaType.ty =
    match ty with
    | {TyName_args=tys; TyName_ident=tyid} -> OpaType.type_of_name(tyid, tys)
    | ty -> ty

}} /* End of type support */

/*
 * SU {{ ... }}:
 *   
 *   A program-level method for constructing Bson documents for select
 *   and update in a friendly manner.
 *   
 */

type su_status =
    {su_select} // specific to select, $gt
  / {su_update} // specific to update, $inc
  / {su_either} // applies to either select or update, $comment
  / {su_key}    // neither, a valid key, "a"

type SU = {{
  // TODO: Documentation

  //dot_path : MongoDb.path -> string
  //dot : MongoDb.path, Bson.document -> Bson.document

  empty : -> Bson.document

  key : string, Bson.document -> Bson.document
  path : list(string), Bson.document -> Bson.document

  double : Bson.document, string, float -> Bson.document
  string : Bson.document, string, string -> Bson.document
  doc : Bson.document, string, Bson.document -> Bson.document
  array : Bson.document, string, list('b) -> Bson.document
  binary : Bson.document, string, Bson.binary -> Bson.document
  id : Bson.document, string, Bson.oid -> Bson.document
  newid : Bson.document, string -> Bson.document
  bool : Bson.document, string, bool -> Bson.document
  date : Bson.document, string, Date.date -> Bson.document
  null : Bson.document, string -> Bson.document
  regexp : Bson.document, string, Bson.regexp -> Bson.document
  code : Bson.document, string, Bson.code -> Bson.document
  symbol : Bson.document, string, Bson.symbol -> Bson.document
  codescope : Bson.document, string, Bson.codescope -> Bson.document
  int32 : Bson.document, string, int -> Bson.document
  timestamp : Bson.document, string, Bson.timestamp -> Bson.document
  int64 : Bson.document, string, int -> Bson.document

  oppoly : 'a, Bson.document, string -> Bson.document

  gti32 : int, Bson.document -> Bson.document
  lti32 : int, Bson.document -> Bson.document
  gtei32 : int, Bson.document -> Bson.document
  ltei32 : int, Bson.document -> Bson.document
  nei32 : int, Bson.document -> Bson.document

  gti64 : int, Bson.document -> Bson.document
  lti64 : int, Bson.document -> Bson.document
  gtei64 : int, Bson.document -> Bson.document
  ltei64 : int, Bson.document -> Bson.document
  nei64 : int, Bson.document -> Bson.document

  gtd : float, Bson.document -> Bson.document
  ltd : float, Bson.document -> Bson.document
  gted : float, Bson.document -> Bson.document
  lted : float, Bson.document -> Bson.document
  ned : float, Bson.document -> Bson.document

  gts : string, Bson.document -> Bson.document
  lts : string, Bson.document -> Bson.document
  gtes : string, Bson.document -> Bson.document
  ltes : string, Bson.document -> Bson.document
  nes : string, Bson.document -> Bson.document

  gtdate : Date.date, Bson.document -> Bson.document
  ltdate : Date.date, Bson.document -> Bson.document
  gtedate : Date.date, Bson.document -> Bson.document
  ltedate : Date.date, Bson.document -> Bson.document
  nedate : Date.date, Bson.document -> Bson.document

  gtts : Bson.timestamp, Bson.document -> Bson.document
  ltts : Bson.timestamp, Bson.document -> Bson.document
  gtets : Bson.timestamp, Bson.document -> Bson.document
  ltets : Bson.timestamp, Bson.document -> Bson.document
  nets : Bson.timestamp, Bson.document -> Bson.document

  set_op : Bson.document, string -> Bson.document

  gt : Bson.document -> Bson.document
  lt : Bson.document -> Bson.document
  gte : Bson.document -> Bson.document
  lte : Bson.document -> Bson.document
  ne : Bson.document -> Bson.document

  and : Bson.document, Bson.document -> Bson.document
  andalso : list(Bson.document) -> Bson.document
  or : Bson.document, Bson.document -> Bson.document
  orelse : list(Bson.document) -> Bson.document
  nor : Bson.document, Bson.document -> Bson.document
  noreither : list(Bson.document) -> Bson.document

  all : Bson.document, list('b) -> Bson.document
  in : Bson.document, list('b) -> Bson.document
  nin : Bson.document, list('b) -> Bson.document

  exists : Bson.document, string, bool -> Bson.document

  mod : Bson.document, 'b, 'b -> Bson.document

  size : Bson.document, int -> Bson.document
  typ : Bson.document, int -> Bson.document

  regex : Bson.document, string, string -> Bson.document

  inc : Bson.document -> Bson.document
  set : Bson.document -> Bson.document
  unset : Bson.document -> Bson.document
  push : Bson.document -> Bson.document
  pushAll : Bson.document -> Bson.document
  addToSet : Bson.document -> Bson.document
  pop : Bson.document -> Bson.document
  pull : Bson.document -> Bson.document
  pullAll : Bson.document -> Bson.document
  rename : Bson.document -> Bson.document
  bit : Bson.document -> Bson.document

  elemMatch : Bson.document -> Bson.document

  not : Bson.document -> Bson.document

  where : Bson.document, string -> Bson.document

  returnKey : Bson.document, bool -> Bson.document
  maxScan : Bson.document, int -> Bson.document
  query : Bson.document, Bson.document -> Bson.document
  orderby : Bson.document, Bson.document -> Bson.document
  explain : Bson.document, bool -> Bson.document
  snapshot : Bson.document, bool -> Bson.document
  min : Bson.document, Bson.document -> Bson.document
  max : Bson.document, Bson.document -> Bson.document
  showDiskLoc : Bson.document, bool -> Bson.document
  hint : Bson.document, Bson.document -> Bson.document
  comment : Bson.document, string -> Bson.document
  natural : Bson.document, int -> Bson.document

  check_strict_select_value_against_type : Bson.document, OpaType.ty, su_status -> void
}}

SU : SU = {{

/*
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

  dot(mpath:MongoDb.path, s:Bson.document): Bson.document = path(List.map(string_of_key,mpath), s)
*/

  empty(): Bson.document = []

  key(name:string, s:Bson.document): Bson.document = [H.doc(name,s)]

  path(path:list(string), s:Bson.document): Bson.document = List.fold_right((s, name -> [H.doc(name,s)]),path,s)

  double(s:Bson.document, name:string, d:float): Bson.document = [H.dbl(name,d)|s]
  string(s:Bson.document, name:string, str:string): Bson.document = [H.str(name,str)|s]
  doc(s:Bson.document, name:string, d:Bson.document): Bson.document = [H.doc(name,d)|s]
  array(s:Bson.document, name:string, l:list('b)): Bson.document = List.flatten([Bson.list_to_bson(name,l,@typeval('b)),s])
  binary(s:Bson.document, name:string, bin:Bson.binary): Bson.document = [H.binary(name,bin)|s]
  id(s:Bson.document, name:string, id:Bson.oid): Bson.document = [H.oid(name,Bson.oid_of_string(id))|s]
  newid(s:Bson.document, name:string): Bson.document = [H.oid(name,Bson.new_oid(void))|s]
  bool(s:Bson.document, name:string, b:bool): Bson.document = [H.bool(name,b)|s]
  date(s:Bson.document, name:string, d:Date.date): Bson.document = [H.date(name,d)|s]
  null(s:Bson.document, name:string): Bson.document = [H.null(name)|s]
  regexp(s:Bson.document, name:string, re:Bson.regexp): Bson.document = [H.regexp(name,re)|s]
  code(s:Bson.document, name:string, c:Bson.code): Bson.document = [H.code(name,c)|s]
  symbol(s:Bson.document, name:string, sym:Bson.symbol): Bson.document = [H.symbol(name,sym)|s]
  codescope(s:Bson.document, name:string, cs:Bson.codescope): Bson.document = [H.codescope(name,cs)|s]
  int32(s:Bson.document, name:string, i:int): Bson.document = [H.i32(name,i)|s]
  timestamp(s:Bson.document, name:string, ts:Bson.timestamp): Bson.document = [H.timestamp(name,ts)|s]
  int64(s:Bson.document, name:string, i:int): Bson.document = [H.i64(name,i)|s]

  oppoly(v:'a, s:Bson.document, op:string): Bson.document =
    rec aux(ty:OpaType.ty) =
      match ty with
      | {TyName_args=[]; TyName_ident="void"} -> null(s,op)
      | {TyConst={TyInt={}}} -> int64(s,op,@unsafe_cast(v))
      | {TyConst={TyString={}}} -> string(s,op,@unsafe_cast(v))
      | {TyConst={TyFloat={}}} -> double(s,op,@unsafe_cast(v))
      | {TyName_args=[]; TyName_ident="bool"} -> bool(s,op,@unsafe_cast(v))
      | {TyName_args=[_]; TyName_ident="list"} -> array(s,op,@unsafe_cast(v))
      | {TyName_args=[]; TyName_ident="Date.date"} -> date(s,op,@unsafe_cast(v))
      | {TyName_args=[]; TyName_ident="Bson.timestamp"} -> timestamp(s,op,@unsafe_cast(v))
      | {TyName_args = tys; TyName_ident = tyid} -> aux(OpaType.type_of_name(tyid,tys))
      | _ -> doc(s,op,Bson.opa2doc(v))
    aux(@typeval('a))

  gti32(i:int, s:Bson.document): Bson.document = int32(s, "$gt", i)
  lti32(i:int, s:Bson.document): Bson.document = int32(s, "$lt", i)
  gtei32(i:int, s:Bson.document): Bson.document = int32(s, "$gte", i)
  ltei32(i:int, s:Bson.document): Bson.document = int32(s, "$lte", i)
  nei32(i:int, s:Bson.document): Bson.document = int32(s, "$ne", i)

  gti64(i:int, s:Bson.document): Bson.document = int64(s, "$gt", i)
  lti64(i:int, s:Bson.document): Bson.document = int64(s, "$lt", i)
  gtei64(i:int, s:Bson.document): Bson.document = int64(s, "$gte", i)
  ltei64(i:int, s:Bson.document): Bson.document = int64(s, "$lte", i)
  nei64(i:int, s:Bson.document): Bson.document = int64(s, "$ne", i)

  gtd(d:float, s:Bson.document): Bson.document = double(s, "$gt", d)
  ltd(d:float, s:Bson.document): Bson.document = double(s, "$lt", d)
  gted(d:float, s:Bson.document): Bson.document = double(s, "$gte", d)
  lted(d:float, s:Bson.document): Bson.document = double(s, "$lte", d)
  ned(d:float, s:Bson.document): Bson.document = double(s, "$ne", d)

  gts(str:string, s:Bson.document): Bson.document = string(s, "$gt", str)
  lts(str:string, s:Bson.document): Bson.document = string(s, "$lt", str)
  gtes(str:string, s:Bson.document): Bson.document = string(s, "$gte", str)
  ltes(str:string, s:Bson.document): Bson.document = string(s, "$lte", str)
  nes(str:string, s:Bson.document): Bson.document = string(s, "$ne", str)

  gtdate(dt:Date.date, s:Bson.document): Bson.document = date(s, "$gt", dt)
  ltdate(dt:Date.date, s:Bson.document): Bson.document = date(s, "$lt", dt)
  gtedate(dt:Date.date, s:Bson.document): Bson.document = date(s, "$gte", dt)
  ltedate(dt:Date.date, s:Bson.document): Bson.document = date(s, "$lte", dt)
  nedate(dt:Date.date, s:Bson.document): Bson.document = date(s, "$ne", dt)

  gtts(ts:Bson.timestamp, s:Bson.document): Bson.document = timestamp(s, "$gt", ts)
  ltts(ts:Bson.timestamp, s:Bson.document): Bson.document = timestamp(s, "$lt", ts)
  gtets(ts:Bson.timestamp, s:Bson.document): Bson.document = timestamp(s, "$gte", ts)
  ltets(ts:Bson.timestamp, s:Bson.document): Bson.document = timestamp(s, "$lte", ts)
  nets(ts:Bson.timestamp, s:Bson.document): Bson.document = timestamp(s, "$ne", ts)

  set_op(s:Bson.document, op:string): Bson.document =
    ((match s with
      | [] -> []
      | [e] -> [H.doc(Bson.key(e),[Bson.set_key(e,op)])]
      | l -> List.map((e -> H.doc(Bson.key(e),[Bson.set_key(e,op)])),l)):Bson.document)

  gt(s:Bson.document): Bson.document = set_op(s, "$gt")
  lt(s:Bson.document): Bson.document = set_op(s, "$lt")
  gte(s:Bson.document): Bson.document = set_op(s, "$gte")
  lte(s:Bson.document): Bson.document = set_op(s, "$lte")
  ne(s:Bson.document): Bson.document = set_op(s, "$ne")

  @private
  boolop_private(op:string, s1:Bson.document, s2:Bson.document): Bson.document =
    [H.arr(op,([H.doc("0",s1),H.doc("1",s2)]:Bson.document))]

  @private
  lboolop_private(op:string, ss:list(Bson.document)): Bson.document =
    match ss with
    | [] -> empty()
    | [s|t] ->
       doc = List.fold_index((i, ss, doc -> [H.doc("{i}",ss)|doc]),[s|t],[])
       [H.arr(op,(doc:Bson.document))]

  and(s1:Bson.document, s2:Bson.document): Bson.document = boolop_private("$and",s1,s2)
  andalso(ss:list(Bson.document)): Bson.document = lboolop_private("$and",ss)
  or(s1:Bson.document, s2:Bson.document): Bson.document = boolop_private("$or",s1,s2)
  orelse(ss:list(Bson.document)): Bson.document = lboolop_private("$or",ss)
  nor(s1:Bson.document, s2:Bson.document): Bson.document = boolop_private("$nor",s1,s2)
  noreither(ss:list(Bson.document)): Bson.document = lboolop_private("$nor",ss)

  all(s:Bson.document, a:list('b)): Bson.document = array(s, "$all", a)
  in(s:Bson.document, a:list('b)): Bson.document = array(s, "$in", a)
  nin(s:Bson.document, a:list('b)): Bson.document = array(s, "$nin", a)

  @private docbool(s:Bson.document, name:string, op:string, tf:bool): Bson.document = doc(s,name,[H.bool(op,tf)])

  exists(s:Bson.document, name:string, tf:bool): Bson.document = docbool(s, name, "$exists", tf)

  mod(s:Bson.document, x:'b, y:'b): Bson.document = array(s, "$mod", [x,y])

  size(s:Bson.document, x:int): Bson.document = int64(s, "$size", x)
  typ(s:Bson.document, t:int): Bson.document = int64(s, "$type", t)

  regex(s:Bson.document, re:string, opts:string): Bson.document = [H.regexp("$regex",(re,opts))|s]

  inc(s:Bson.document): Bson.document = key("$inc",s)
  set(s:Bson.document): Bson.document = key("$set",s)
  unset(s:Bson.document): Bson.document = key("$unset",s)
  push(s:Bson.document): Bson.document = key("$push",s)
  pushAll(s:Bson.document): Bson.document = key("$pushAll",s)
  addToSet(s:Bson.document): Bson.document = key("$addToSet",s)
  pop(s:Bson.document): Bson.document = key("$pop",s)
  pull(s:Bson.document): Bson.document = key("$pull",s)
  pullAll(s:Bson.document): Bson.document = key("$pullAll",s)
  rename(s:Bson.document): Bson.document = key("$rename",s)
  bit(s:Bson.document): Bson.document = key("$bit",s)

  elemMatch(s:Bson.document): Bson.document = key("$elemMatch",s)

  not(s:Bson.document): Bson.document = key("$not",s)

  where(s:Bson.document, whr:string): Bson.document = [H.code("$where",whr)|s]

  returnKey(s:Bson.document, tf:bool): Bson.document = bool(s, "$returnKey", tf)
  maxScan(s:Bson.document, i:int): Bson.document = int64(s, "$maxScan", i)
  query(s:Bson.document, d:Bson.document): Bson.document = doc(s, "$query", d)
  orderby(s:Bson.document, d:Bson.document): Bson.document = doc(s, "$orderby", d)
  explain(s:Bson.document, tf:bool): Bson.document = bool(s, "$explain", tf)
  snapshot(s:Bson.document, tf:bool): Bson.document = bool(s, "$snapshot", tf)
  min(s:Bson.document, d:Bson.document): Bson.document = doc(s, "$min", d)
  max(s:Bson.document, d:Bson.document): Bson.document = doc(s, "$max", d)
  showDiskLoc(s:Bson.document, tf:bool): Bson.document = bool(s, "$showDiskLoc", tf)
  hint(s:Bson.document, d:Bson.document): Bson.document = doc(s, "$hint", d)
  comment(s:Bson.document, c:string): Bson.document = string(s, "$comment", c)
  natural(s:Bson.document, i:int): Bson.document = int32(s, "$natural", i)

  @private T = TypeSelect

  @private union(ss) = List.fold(StringSet.union,ss,StringSet.empty)
  // I'm not guaranteeing that all of these have been classified correctly!!!
  @private update_names =
    StringSet.From.list(["$inc", "$set", "$unset", "$push", "$pushAll", "$addToSet",
                         "$pop", "$pull", "$pullAll", "$rename", "$bit"])
  @private no_array_select_names =
    StringSet.From.list(["$gt", "$lt", "$gte", "$lte", "$ne",
                         "$regex", "$mod",
                         "$not", "$elemMatch",
                         "$where",
                         "$query", "$orderby"])
  @private transparent_select_names =
    StringSet.From.list(["$exists", "$type", "$size", ])
  @private array_select_names =
    StringSet.From.list(["$and", "$or", "$nor", "$all", "$in", "$nin"])
  @private select_names = union([no_array_select_names,transparent_select_names,array_select_names])
  @private select_or_update_names =
    StringSet.From.list(["$returnKey", "$maxScan", "$explain", "$snapshot",
                         "$min", "$max", "$showDiskLoc", "$hint", "$comment"])
  //@private all_names = union([update_names,select_names,select_or_update_names])
  //@private string_of_set(set) = "[{String.concat(", ",StringSet.fold((s, strs -> [s|strs]),set,[]))}]"

  @private string_of_su_status(sut:su_status): string =
    match sut with
    | {su_select} -> "select"
    | {su_update} -> "update"
    | {su_either} -> "either"
    | {su_key} -> "key"

  // Note there will be shenanigans here, you can get both reduce and $reduce!!!
  @private status(name:string): su_status =
    if StringSet.mem(name,select_names)
    then {su_select}
    else if StringSet.mem(name,update_names)
    then {su_update}
    else if StringSet.mem(name,select_or_update_names)
    then {su_either}
    else {su_key}

  // Ordering update >> select >> anything else
  @private merge(sus1:su_status, sus2:su_status): su_status =
    match (sus1, sus2) with
    | ({su_update},_) -> {su_update}
    | (_,{su_update}) -> {su_update}
    | ({su_select},_) -> {su_select}
    | (_,{su_select}) -> {su_select}
    | (_,_) -> {su_either}

  // We should have removed keys before calling this
  @private sutok(sus1:su_status, sus2:su_status): bool =
    match (sus1, sus2) with
    | ({su_either},_) -> true
    | (_,{su_either}) -> true
    | ({su_select},{su_select}) -> true
    | ({su_update},{su_update}) -> true
    | _ -> false // we don't get su_key here

  @private
  type_of_bson_value(value:Bson.value): (su_status, OpaType.ty) =
    match value with
    | {Double=_} -> ({su_either},T.tfloat)
    | {String=_} -> ({su_either},T.tstring)
    | {Document=d} -> type_of_bson_document(d)
    | {Array=[]} -> ({su_either},T.tempty) // or maybe list('a) or list({})???
    | {Array=[{name=_; ~value}|_]} -> // comes from an OPA list or intmap so all same type
       (sut,ty) = type_of_bson_value(value)
       (sut,T.tlist(ty))
    | {Binary=_} -> ({su_either},T.tbinary)
    | {ObjectID=_} -> ({su_either},T.toid)
    | {Boolean=_} -> ({su_either},T.tbool)
    | {Date=_} -> ({su_either},T.tdate)
    | {Null=_} -> ({su_either},T.tvoid)
    | {Regexp=_} -> ({su_either},T.tregexp)
    | {Code=_} -> ({su_either},T.tcode)
    | {Symbol=_} -> ({su_either},T.tsymbol)
    | {CodeScope=_} -> ({su_either},T.tcodescope)
    | {Int32=_} -> ({su_either},T.tint)
    | {Timestamp=_} -> ({su_either},T.ttimestamp)
    | {Int64=_} -> ({su_either},T.tint)
    | {Min=_} -> ({su_select},T.tvoid)
    | {Max=_} -> ({su_select},T.tvoid)

  @private sutymrg((sut,ty), (asut,aty)) = (merge(sut,asut),T.tmrgrecs(ty,aty))

  @private
  type_of_bson_element(element:Bson.element): (su_status, OpaType.ty) =
    stat = status(element.name)
    if StringSet.mem(element.name,transparent_select_names)
    then (stat,T.tempty)
    else if StringSet.mem(element.name,array_select_names)
    then
      match element.value with
      | {Array=adoc} -> List.fold(sutymrg,List.map(type_of_bson_value,List.map((e -> e.value),adoc)),(stat,T.tempty))
      | _ -> ML.fatal("SU.type_of_bson_element",
                      "key {element.name} requires an array value, actually {Bson.to_pretty([element])}",-1)
    else
      match element.name with
      | "$mod" -> (stat,T.tnumeric)
      | _ ->
         (sut,ty) = type_of_bson_value(element.value)
         sut1 = merge(sut,stat)
         if stat == {su_key}
         then (sut1,{TyRecord_row=[{label=element.name; ~ty}]})
         else (sut1,ty)

  @private
  type_of_bson_document(doc:Bson.document): (su_status, OpaType.ty) =
    List.fold(sutymrg,List.map(type_of_bson_element,doc),({su_either},T.tempty))

  @private empty_ty(ty) = ty == T.tempty || T.istvar(ty)

  @private
  explode_dot(ty:OpaType.ty): OpaType.ty =
    explode_row(row) =
         List.map((f -> 
                   match String.explode(".",f.label) with
                   | [] | [_] -> f
                   | [dot|dots] ->
                      rec aux(dots) =
                        match dots with
                        | [] -> @fail
                        | [label] -> {TyRecord_row=[{~label; ty=(f.ty:OpaType.ty)}]}
                        | [label|dots] -> {TyRecord_row=[{~label; ty=aux(dots)}]}
                      {label=dot; ty=aux(dots)}),row)
    match ty with
    | {TyRecord_row=row} -> {TyRecord_row=explode_row(row)}
    | {TyRecord_row=row; TyRecord_rowvar=rowvar} -> {TyRecord_row=explode_row(row); TyRecord_rowvar=rowvar}
    | _ -> ty

  @private /*improper*/subtype(sty:OpaType.ty, ty:OpaType.ty): bool =
    //dbg do println("subtype: sty={OpaType.to_pretty(sty)}\n         ty={OpaType.to_pretty(ty)}")
    missing_label(row, label) =
      labels = List.list_to_string((s -> s),List.map((f -> f.label),row))
      ML.warning("SU.subtype","Missing label {label} in row {labels}",false)
    incomparable() =
      ML.warning("SU.subtype","Incomparable types {OpaType.to_pretty(sty)} and {OpaType.to_pretty(ty)}",false)
    sty = explode_dot(sty)
    //dbg do println("explode={OpaType.to_pretty(sty)}")
    esty = empty_ty(sty)
    if sty == ty || esty
    then true
    else if empty_ty(ty)
    then esty
    else
      match (sty,ty) with
      | ({TyRecord_row=strow; ...},{TySum_col=tcol; ...}) ->
         // We never get a sum type from type_of_bson_document
         (match T.find_row_in_col(strow,tcol) with
          | {some=trow} -> subtype(sty,{TyRecord_row=trow})
          | {none} -> incomparable())
      | ({TyRecord_row=strow; ...},{TyRecord_row=trow; ...}) ->
         List.fold((stf, isty ->
                     isty &&
                     (match List.find((tf -> tf.label == stf.label),trow) with
                      | {some=tf} -> subtype(stf.ty,tf.ty)
                      | {none} -> missing_label(trow, stf.label))),strow,true)
      | ({TyName_args=_; TyName_ident="Bson.numeric"},{TyConst={TyInt={}}})
      | ({TyName_args=_; TyName_ident="Bson.numeric"},{TyConst={TyFloat={}}}) ->
         true // Some arithmetic ops, $mod
      | ({TyName_args=[]; TyName_ident="Bson.regexp"},_)
      | ({TyName_args=[]; TyName_ident="Bson.code"},_)
      | ({TyName_args=[]; TyName_ident="Bson.codescope"},_) ->
         true // For now, until we get types from RE's and Javascript
      | ({TyName_args=tys; TyName_ident=tyid},_) -> subtype(OpaType.type_of_name(tyid, tys),ty)
      | (_,{TyName_args=tys; TyName_ident=tyid}) -> subtype(sty,OpaType.type_of_name(tyid, tys))
      | _ -> incomparable()

  /**
   * Validate the given document agains the type of the document
   * and the select/update status.
   *
   * Currently, we log a warning.
   **/
  check_strict_select_value_against_type(doc:Bson.document, ty:OpaType.ty, sut:su_status): void =
    //dbg do println("check_strict_select_value_against_type:\n  doc={Bson.to_pretty(doc)}\n  ty={OpaType.to_pretty(ty)}")
    //dbg do println("  status={sut}")
    (dsut, dty) = type_of_bson_document(doc)
    //dbg do println("  dsut={dsut}  dty={OpaType.to_pretty(dty)}")
    if sutok(dsut,sut)
    then
      is_subtype = subtype(dty,ty)
      //dbg do println("is_subtype={is_subtype}")
      if is_subtype
      then void
      else
        sutstr = string_of_su_status(sut)
        dtystr = OpaType.to_pretty(dty)
        tystr = OpaType.to_pretty(ty)
        ML.warning("SU.check","Inappropriate {sutstr} type {dtystr} for collection({tystr})",void)
    else ML.warning("SU.check","Applying {string_of_su_status(dsut)} to {string_of_su_status(sut)}",void)

}}

@abstract type select('a) = Bson.document

Select = {{

  to_pretty(select:select('a)): string = "{Bson.to_pretty(select)}"

  unsafe_create(s : Bson.document): select('a) = s

  unsafe_make(x:'b): select('a) = unsafe_create(Bson.opa2doc(x))

  create(s : Bson.document): select('a) =
    do SU.check_strict_select_value_against_type(s, @typeval('a), {su_select}) 
    s

  make(x:'b): select('a) = create(Bson.opa2doc(x))

  empty() : select('a) = SU.empty()

}}

@abstract type update('a) = Bson.document

Update = {{

  to_pretty(update:update('a)): string = "{Bson.to_pretty(update)}"

  unsafe_create(u : Bson.document): update('a) = u

  unsafe_make(x:'b): update('a) = unsafe_create(Bson.opa2doc(x))

  create(u : Bson.document): update('a) =
    do SU.check_strict_select_value_against_type(u, @typeval('a), {su_update}) 
    u

  make(x:'b): update('a) = create(Bson.opa2doc(x))

  empty() : update('a) = SU.empty()

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

@abstract type batch = list(Bson.document)

Batch = {{
  empty = ([]:batch)
  add(b:batch, v:'a): batch = [Bson.opa2doc(v)|b]
  one(v:'a): batch = [Bson.opa2doc(v)]
  add2(b:batch, (v1:'a, v2:'b)): batch = [Bson.opa2doc(v1)|[Bson.opa2doc(v2)|b]]
  two(v1:'a, v2:'b): batch = [Bson.opa2doc(v1)|[Bson.opa2doc(v2)]]
  add3(b:batch, (v1:'a, v2:'b, v3:'c)): batch = [Bson.opa2doc(v1)|[Bson.opa2doc(v2)|[Bson.opa2doc(v3)|b]]]
  three(v1:'a, v2:'b, v3:'c): batch = [Bson.opa2doc(v1)|[Bson.opa2doc(v2)|[Bson.opa2doc(v3)]]]
  list(b:batch, vs:list('a)): batch = List.flatten([List.map(Bson.opa2doc,vs),b])
  of_list(vs:list('a)) = list(empty,vs)
  merge(b1:batch, b2:batch): batch = List.flatten([b1, b2])
}}

@abstract type fields = Bson.document

Fields = {{
  empty = ([]:fields)
  add(f:fields, name:string, incexc:Bson.int32): fields = [H.i32(name,incexc)|f]
  one(name:string, incexc:Bson.int32): fields = [H.i32(name,incexc)]
  list(f:fields, fs:list((string,Bson.int32))): fields = List.flatten([List.map(((n,ie) -> H.i32(n,ie)),fs),f])
  of_list(fs:list((string,Bson.int32))) = list(empty,fs)
  merge(f1:fields, f2:fields): fields = List.flatten([f1, f2])
  validate(fields:fields): bool =
    (zeros, ones, others) = List.fold((e, (z, o, g) ->
                                       if e.name == "_id"
                                       then (z,o,g)
                                       else
                                         match Bson.int_of_element(e) with 
                                         | {some=0} -> (z+1,o,g)
                                         | {some=1} -> (z,o+1,g)
                                         | {some=_} | {none} -> (z,o,g+1)),
                                      fields,(0,0,0))
    if zeros > 0 && ones > 0
    then ML.warning("Fields.validate","Can't mix include and exclude fields {Bson.to_pretty(fields)}",false)
    else if others > 0
    then ML.warning("Fields.validate","Can only use 0 and 1 in fields {Bson.to_pretty(fields)}",false)
    else true
}}

type collection('a) = {
  db: mongodb;
  pty: OpaType.ty; // type of the parent collection
  vty: OpaType.ty; // type of the view collection
  view: bool; // flag for view status
}

type collection_cursor('a) = {
  collection: collection('a);
  cursor: Cursor.cursor;
  query: select('a);
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
  orderby : collection('value), option(Bson.document) -> collection('value)
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
  update : collection('value), select('value), update('value) -> bool
  delete : collection('value), select('value) -> bool
  find_one : collection('value), select('value) -> outcome('value,Mongo.failure)
  find_one_unsafe : collection('value), select('value) -> outcome('value,Mongo.failure)
  query : collection('value), select('value) -> outcome(collection_cursor('value),Mongo.failure)
  query_unsafe : collection('value), select('value) -> outcome(collection_cursor('value),Mongo.failure)
  first : collection_cursor('value) -> outcome(collection_cursor('value),Mongo.failure)
  next : collection_cursor('value) -> (collection_cursor('value),outcome('value,Mongo.failure))
  find_all : collection('value), select('value) -> outcome(list('value),Mongo.failure)
  find_all_unsafe : collection('value), select('value) -> outcome(list('value),Mongo.failure)
  has_more : collection_cursor('value) -> bool
  view : collection('value), Bson.document -> collection('newvalue)
  count : collection('value), option(select('value)) -> outcome(int,Mongo.failure)
  distinct : collection('value), string, option(select('value)) -> outcome(list('b),Mongo.failure)
  group : collection('value), Bson.document, string, Bson.document, option(Bson.document), option(string) -> Mongo.result
  analyze_group : Mongo.result -> group_result('a)
  kill : collection_cursor('value) -> collection_cursor('value)
}}

Collection : Collection = {{

  create(db:mongodb): collection('value) =
    { db=MDB.clone(db);
      pty=@typeval('value);
      vty=@typeval('value);
      view=false;
    }

  destroy(c:collection('value)): void = MDB.close(c.db)

  skip(c:collection('value), skip:int): collection('value) = {c with db={ c.db with ~skip }}
  limit(c:collection('value), limit:int): collection('value) = {c with db={ c.db with ~limit }}
  fields(c:collection('value), fields:option(Bson.document)): collection('value) = {c with db={ c.db with ~fields }}
  orderby(c:collection('value), orderby:option(Bson.document)): collection('value) = {c with db={ c.db with ~orderby }}

  continueOnError(c:collection('value)): collection('value) =
    {c with db={ c.db with insert_flags=Bitwise.lor(c.db.insert_flags,Mongo.ContinueOnErrorBit) }}
  upsert(c:collection('value)): collection('value)
    = {c with db={ c.db with update_flags=Bitwise.lor(c.db.update_flags,Mongo.UpsertBit) }}
  multiUpdate(c:collection('value)): collection('value)
    = {c with db={ c.db with update_flags=Bitwise.lor(c.db.update_flags,Mongo.MultiUpdateBit) }}
  singleRemove(c:collection('value)): collection('value)
    = {c with db={ c.db with delete_flags=Bitwise.lor(c.db.delete_flags,Mongo.SingleRemoveBit) }}
  tailableCursor(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,Mongo.TailableCursorBit) }}
  slaveOk(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,Mongo.SlaveOkBit) }}
  oplogReplay(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,Mongo.OplogReplayBit) }}
  noCursorTimeout(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,Mongo.NoCursorTimeoutBit) }}
  awaitData(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,Mongo.AwaitDataBit) }}
  exhaust(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,Mongo.ExhaustBit) }}
  partial(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,Mongo.PartialBit) }}

  @private
  read_only(c:collection('value), from:string): void =
    if c.view then ML.fatal("Collection.{from}","In order to prevent type inconsistencies, you can't write to a view",-1)

  insert(c:collection('value), v:'value): bool =
    do read_only(c, "insert")
    ns = c.db.dbname^"."^c.db.collection
    b = Bson.opa_to_bson(v,{some=c.pty})
    Mongo.insert(c.db.mongo,c.db.insert_flags,ns,b)

  insert_batch(c:collection('value), b:batch): bool =
    do read_only(c, "insert_batch")
    ns = c.db.dbname^"."^c.db.collection
    Mongo.insert_batch(c.db.mongo,c.db.insert_flags,ns,b)

  update(c:collection('value), select:select('value), update:update('value)): bool =
    do read_only(c, "update")
    ns = c.db.dbname^"."^c.db.collection
    Mongo.update(c.db.mongo,c.db.update_flags,ns,select,update)

  delete(c:collection('value), select:select('value)): bool =
    do read_only(c, "delete")
    ns = c.db.dbname^"."^c.db.collection
    Mongo.delete(c.db.mongo,c.db.delete_flags,ns,select)

  find_one_unsafe(c:collection('value), select:select('value)): outcome('value,Mongo.failure) =
    ns = c.db.dbname^"."^c.db.collection
    (match Cursor.find_one(c.db.mongo,ns,select,c.db.fields,c.db.orderby) with
     | {success=doc} ->
       //do println("  doc={Bson.string_of_bson(doc)}\n  ty={OpaType.to_pretty(ty)}")
       (match Bson.bson_to_opa(doc, c.vty) with
        | {some=v} -> {success=(Magic.id(v):'value)}
        | {none} -> {failure={Error="Collection.find_one: not found"}})
     | {~failure} -> {~failure})

  find_one(c:collection('value), select:select('value)): outcome('value,Mongo.failure) =
    do if c.view then verify_type_match(@typeval('value), c.vty, "Collection.find_one","View type does not match return type")
    find_one_unsafe(c, select)

  query_unsafe(c:collection('value), select:select('value)): outcome(collection_cursor('value),Mongo.failure) =
    ns = c.db.dbname^"."^c.db.collection
    match Cursor.find(c.db.mongo,ns,select,c.db.fields,c.db.orderby,c.db.limit,c.db.skip,c.db.query_flags) with
    | {success=cursor} -> {success={collection=c; ~cursor; query=select; ty=c.vty }}
    | {~failure} -> {~failure}

  query(c:collection('value), select:select('value)): outcome(collection_cursor('value),Mongo.failure) =
    do if c.view then verify_type_match(@typeval('value), c.vty, "Collection.query","View type does not match return type")
    query_unsafe(c, select)

  first(cc:collection_cursor('value)): outcome(collection_cursor('value),Mongo.failure) =
    _ = Cursor.reset(cc.cursor)
    query(cc.collection, cc.query)

  next(cc:collection_cursor('value)): (collection_cursor('value),outcome('value,Mongo.failure)) =
    cursor = Cursor.next(cc.cursor)
    match Cursor.check_cursor_error(cursor) with
    | {success=doc} ->
       //do println("next:\n  doc={Bson.to_pretty(doc)}\n  ty={OpaType.to_pretty(cc.ty)}")
       (match Bson.bson_to_opa(doc, cc.ty) with
        | {some=v} -> ({cc with ~cursor},{success=(Magic.id(v):'value)})
        | {none} -> ({cc with ~cursor},{failure={Error="Collection.next: not found"}}))
    | {~failure} ->
       cursor = Cursor.reset(cursor)
       ({cc with ~cursor},{~failure})

  has_more(cc:collection_cursor('value)): bool = Cursor.valid(cc.cursor)

  find_all_unsafe(c:collection('value), select:select('value)): outcome(list('value),Mongo.failure) =
    //do println("find_all: 'value={OpaType.to_pretty(@typeval('value))}")
    match query_unsafe(c,select) with
    | {success=cc} ->
       (cc,l) =
         while((cc,{success=[]}),
               ((cc,l) ->
                  match l with
                  | {success=l} ->
                     (match next(cc) with
                      | (cc,{success=v}) ->
                         //do println("  v={(v:'value)}")
                         ((cc,{success=[Magic.id(v):'value|l]}),has_more(cc))
                      | (cc,{~failure}) ->
                         //do println("  err(query)={Mongo.string_of_failure(failure)}")
                         ((cc,{~failure}),false))
                  | {~failure} -> ((cc,{~failure}),false)))
       _ = kill(cc)
       l
    | {~failure} -> {~failure}

  find_all(c:collection('value), select:select('value)): outcome(list('value),Mongo.failure) =
    do if c.view then verify_type_match(@typeval('value), c.vty, "Collection.find_all","View type does not match return type")
    find_all_unsafe(c, select)

  @private
  rec map_field(ty, f) =
    map_row(row) = List.map((fld -> f({fld with ty=map_field(fld.ty, f)})),row)
    match ty with
    | {TyRecord_row=row}
    | {TyRecord_row=row; TyRecord_rowvar=_} -> {TyRecord_row=map_row(row)}
    | {TySum_col=col}
    | {TySum_col=col; TySum_colvar=_} -> {TySum_col=List.map(map_row,col)}
    | {TyName_args=tys; TyName_ident=tyid} -> map_field(OpaType.type_of_name(tyid, tys), f)
    | ty -> ty

  @private
  make_reg(fld) = {fld with ty={TyName_args=[fld.ty]; TyName_ident="Bson.register"}}

  @private
  type_from_fields(pty:OpaType.ty, fields:fields): OpaType.ty =
    if not(Fields.validate(fields))
    then ML.fatal("Collection.type_from_fields","Fields failed to validate",-1)
    else
      rec filter_field(pty, label, num) =
        ie = match num with {some=0} -> (f -> f.label != label) | _ -> (f -> f.label == label)
        filter_row(row) = List.filter(ie,row)
        match pty with
        | {TyRecord_row=row}
        | {TyRecord_row=row; TyRecord_rowvar=_} -> {TyRecord_row=filter_row(row)}
        | {TySum_col=col}
        | {TySum_col=col; TySum_colvar=_} -> 
           (match List.filter((r -> r != []),List.map(filter_row,col)) with
            | [] -> {TyRecord_row=[]}
            | [r] -> {TyRecord_row=r}
            | col -> {TySum_col=col})
        | {TyName_args=tys; TyName_ident=tyid} -> filter_field(OpaType.type_of_name(tyid, tys), label, num)
        | ty -> ty
      List.fold((e, ty -> filter_field(ty, e.name, Bson.int_of_value(e.value))),fields,pty)

  @private
  verify_type_match(ty1:OpaType.ty, ty2:OpaType.ty, from:string, msg:string): void =
    //do println("ty1={OpaType.to_pretty(TypeSelect.name_type(ty1))}")
    //do println("ty2={OpaType.to_pretty(TypeSelect.name_type(ty2))}")
    // We can't use the fancy caching in compare_ty since our altered types mess with the caching
    if not(TypeSelect.naive_type_compare(ty1, ty2))
    then ML.fatal(from,"{msg} {OpaType.to_pretty(ty1)} and {OpaType.to_pretty(ty2)}",-1)
    else void

  view(c:collection('value), vfields:fields): collection('newvalue) =
    v = fields(c, {some=vfields})
    pty = @typeval('value)
    do verify_type_match(pty, c.pty, "Collection.view","Attempt to create view from non-matching parent types")
    fvty = type_from_fields(pty, vfields)
    rfvty = map_field(fvty, make_reg)
    cvty = @typeval('newvalue)
    //do println("fvty={OpaType.to_pretty(fvty)}")
    //do println("rfvty={OpaType.to_pretty(rfvty)}")
    //do println("cvty={OpaType.to_pretty(cvty)}")
    do verify_type_match(rfvty, cvty, "Collection.view","Attempt to create view with incompatible view types")
    { v with vty=rfvty; view=true }

  count(c:collection('value), query_opt:option(select('value))): outcome(int,Mongo.failure) =
    Commands.count(c.db.mongo, c.db.dbname, c.db.collection, (Option.map((s -> s),query_opt)))

  distinct(c:collection('value), key:string, query_opt:option(select('value))): outcome(list('b),Mongo.failure) =
    match Commands.distinct(c.db.mongo, c.db.dbname, c.db.collection, key, (Option.map((s -> s),query_opt))) with
    | {success=doc} ->
       // possibly: get the type from 'value and get the key type out of there???
       ty = {TyName_args=[@typeval('b)]; TyName_ident="list"}
       (match Bson.bson_to_opa(doc, ty) with
        | {some=v} -> {success=(Magic.id(v):list('b))}
        | {none} -> {failure={Error="Collection.distinct: not found"}})
    | {~failure} -> {~failure}

  /**
   * Note that for group to work ints have to match, Int32 will not match Int64!!!
   **/
  group(c:collection('value), key:Bson.document, reduce:string, initial:Bson.document,
        cond_opt:option(Bson.document), finalize_opt:option(string)): Mongo.result =
    Commands.group(c.db.mongo, c.db.dbname, c.db.collection, key, reduce, initial, cond_opt, finalize_opt)

  // TODO: use Command types and doc2opa
  analyze_group(res:Mongo.result): group_result('a) =
    match res with
    | {success=doc} ->
      (match Bson.find(doc,"retval") with
       | {some=[{name=k; value={Array=arr}}]} ->
          ty = {TyName_args=[@typeval('a)]; TyName_ident="list"}
          (match Bson.bson_to_opa([H.arr(k,List.rev(arr))], ty) with
           | {some=v} ->
              retval = (Magic.id(v):list('a))
              (match Bson.find_int(doc, "count") with
               | {some=count} ->
                  (match Bson.find_int(doc, "keys") with
                   | {some=keys} ->
                      (match Bson.find_int(doc, "ok") with
                       | {some=ok} ->
                          {success=~{retval; count; keys; ok}}
                       | {none} -> {failure={Error="Collection.analyze_group: ok not found"}})
                   | {none} -> {failure={Error="Collection.analyze_group: keys not found"}})
               | {none} -> {failure={Error="Collection.analyze_group: count not found"}})
           | {none} -> {failure={Error="Collection.analyze_group: retval not found"}})
       | _ -> {failure={Error="Collection.analyze_group: no retval value in reply"}})
    | {~failure} -> {~failure}

  // TODO: map-reduce

  kill(cc:collection_cursor('value)): collection_cursor('value) = { cc with cursor=Cursor.reset(cc.cursor) }

}}

/**
 * UtilsDb is a set of utility functions to make programming
 * in Bson and Mongo easier.
 **/

UtilsDb = {{

   /** A safe operation checking the error (still have to check if the
    *  last error is really the last error, using eg. findAndModify).
    **/
   @private
   safe_(c:collection('value),f:'a->bool,a:'a,msg:string): bool =
     if not(f(a))
     then (do println("{msg}: Fatal error message not sent to server") false)
     else
       (match MDB.getLastError(c.db) with
        | {~success} ->
           (match Bson.find_string(success, "err") with
            | {some=""} | {none} -> true
            | {some=err} -> do println("{msg}: {err}") false)
        | {~failure} ->  do println("{msg}: fatal error {Mongo.string_of_failure(failure)}") false)

   safe_insert(c,v) = safe_(c,((c,v) -> Collection.insert(c,v)),(c,v),"Collection.insert")
   safe_insert_batch(c,b) = safe_(c,((c,b) -> Collection.insert_batch(c,b)),(c,b),"Collection.insert_batch")
   safe_update(c,s,v) = safe_(c,((c,s,v) -> Collection.update(c,s,v)),(c,s,v),"Collection.update")
   safe_delete(c,s) = safe_(c,((c,s) -> Collection.delete(c,s)),(c,s),"Collection.delete")

    // It's easier to deal with options
    find_result_to_opt(result) : option('a) =
       match result with
       | {success=v} -> {some=v}
       | _ -> {none}
   
    // Idem with list and empty list
    find_all_result_to_list(result) : list('a) =
       match result with
       | {success=v} -> v
       | _ -> []

    find(c,r) = find_result_to_opt(Collection.find_one(c,Select.unsafe_make(r)))
    find_all(c,r) = find_all_result_to_list(Collection.find_all(c,Select.unsafe_make(r)))

    // Delete by id by default
    delete(c,id) = Collection.delete(c,Select.unsafe_make({_id = id}))

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
