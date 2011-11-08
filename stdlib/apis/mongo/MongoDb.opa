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
 * Helper modules are Batch{{}} which allows building a list of documents for
 * batch insert and Fields{{}} which is used to define field select documents.
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
  @private ML = MongoLog
  @private H = Bson.Abbrevs
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
  db: Mongo.mongodb;
  ty: OpaType.ty; // type of the collection
}

type view('a,'b) = {
  coll: collection('a);
  vty: OpaType.ty; // type of the view collection
  is_opa: bool; // if true, we assume an OPA type and ignore incomplete documents
}

type foreign('a,'b,'c,'d,'e) = {
  primary: view('a,'b); // the parent view
  foreign: view('c,'d); // the foreign view
  pkey: string;
  fkey: string;
}

type collection_cursor('a) = {
  collection: collection('a);
  cursor: Mongo.cursor;
  query: Mongo.select('a);
  ty: OpaType.ty;
  ignore_incomplete: bool;
}

type group('a) = { retval:list('a); count:int; keys:int; ok:int }
type group_result('a) = outcome(group('a),Mongo.failure)

Collection = {{

  @private H = Bson.Abbrevs

  create(db:Mongo.mongodb): collection('value) = { db=MongoConnection.clone(db); ty=@typeval('value); }

  destroy(c:collection('value)): void = MongoConnection.close(c.db)

  skip(c:collection('value), skip:int): collection('value) = {c with db={ c.db with ~skip }}
  limit(c:collection('value), limit:int): collection('value) = {c with db={ c.db with ~limit }}
  fields(c:collection('value), fields:option(Bson.document)): collection('value) = {c with db={ c.db with ~fields }}
  orderby(c:collection('value), orderby:option(Bson.document)): collection('value) = {c with db={ c.db with ~orderby }}

  continueOnError(c:collection('value)): collection('value) =
    {c with db={ c.db with insert_flags=Bitwise.lor(c.db.insert_flags,MongoDriver.ContinueOnErrorBit) }}
  upsert(c:collection('value)): collection('value)
    = {c with db={ c.db with update_flags=Bitwise.lor(c.db.update_flags,MongoDriver.UpsertBit) }}
  multiUpdate(c:collection('value)): collection('value)
    = {c with db={ c.db with update_flags=Bitwise.lor(c.db.update_flags,MongoDriver.MultiUpdateBit) }}
  singleRemove(c:collection('value)): collection('value)
    = {c with db={ c.db with delete_flags=Bitwise.lor(c.db.delete_flags,MongoDriver.SingleRemoveBit) }}
  tailableCursor(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,MongoDriver.TailableCursorBit) }}
  slaveOk(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,MongoDriver.SlaveOkBit) }}
  oplogReplay(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,MongoDriver.OplogReplayBit) }}
  noCursorTimeout(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,MongoDriver.NoCursorTimeoutBit) }}
  awaitData(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,MongoDriver.AwaitDataBit) }}
  exhaust(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,MongoDriver.ExhaustBit) }}
  partial(c:collection('value)): collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,MongoDriver.PartialBit) }}

  insert(c:collection('value), v:'value): bool =
    ns = c.db.dbname^"."^c.db.collection
    b = Bson.opa_to_bson(v,{some=@typeval('value)})
    MongoDriver.insert(c.db.mongo,c.db.insert_flags,ns,b)

  insert_batch(c:collection('value), b:batch): bool =
    ns = c.db.dbname^"."^c.db.collection
    MongoDriver.insert_batch(c.db.mongo,c.db.insert_flags,ns,b)

  update(c:collection('value), select:Mongo.select('value), update:Mongo.update('value)): bool =
    ns = c.db.dbname^"."^c.db.collection
    MongoDriver.update(c.db.mongo,c.db.update_flags,ns,select,update)

  delete(c:collection('value), select:Mongo.select('value)): bool =
    ns = c.db.dbname^"."^c.db.collection
    MongoDriver.delete(c.db.mongo,c.db.delete_flags,ns,select)

  find_one_doc(c:collection('value), select:Mongo.select('value)): Mongo.result =
    ns = c.db.dbname^"."^c.db.collection
    MongoCursor.find_one(c.db.mongo,ns,select,c.db.fields,c.db.orderby)

  find_one_unsafe(c:collection('value), select:Mongo.select('value), ignore_incomplete:bool): outcome('result,Mongo.failure) =
    ns = c.db.dbname^"."^c.db.collection
    (match MongoCursor.find_one(c.db.mongo,ns,select,c.db.fields,c.db.orderby) with
     | {success=doc} ->
        (match Bson.b2o_incomplete(doc, @typeval('result), ignore_incomplete) with
         | {found=v} -> {success=(Magic.id(v):'result)}
         | {not_found} -> {failure={Error="Collection.find_one: not found"}}
         | {incomplete} -> {failure={Incomplete}})
     | {~failure} -> {~failure})

  find_one(c:collection('value), select:Mongo.select('value)): outcome('value,Mongo.failure) =
    find_one_unsafe(c, select, false)

  query_unsafe(c:collection('value), select:Mongo.select('value), ignore_incomplete:bool)
             : outcome(collection_cursor('result),Mongo.failure) =
    ns = c.db.dbname^"."^c.db.collection
    //do println("query_unsafe:\n'value={OpaType.to_pretty(@typeval('value))}\n'result={OpaType.to_pretty(@typeval('result))}")
    match MongoCursor.find(c.db.mongo,ns,select,c.db.fields,c.db.orderby,c.db.limit,c.db.skip,c.db.query_flags) with
    | {success=cursor} ->
       {success={collection=@unsafe_cast(c); ~cursor; query=@unsafe_cast(select); ty=@typeval('result); ~ignore_incomplete}}
    | {~failure} -> {~failure}

  query(c:collection('value), select:Mongo.select('value)): outcome(collection_cursor('value),Mongo.failure) =
    query_unsafe(c, select, false)

  first(cc:collection_cursor('value)): outcome(collection_cursor('value),Mongo.failure) =
    _ = MongoCursor.reset(cc.cursor)
    query(cc.collection, cc.query)

  next(cc:collection_cursor('value)): (collection_cursor('value),outcome('value,Mongo.failure)) =
    cursor = MongoCursor.next(cc.cursor)
    match MongoCursor.check_cursor_error(cursor) with
    | {success=doc} ->
       //do println("next:\n  doc={Bson.to_pretty(doc)}\n  ty={OpaType.to_pretty(cc.ty)}")
       (match Bson.b2o_incomplete(doc, cc.ty, cc.ignore_incomplete) with
        | {found=v} -> ({cc with ~cursor},{success=(Magic.id(v):'value)})
        | {not_found} -> ({cc with ~cursor},{failure={Error="Collection.next: not found"}})
        | {incomplete} ->  ({cc with ~cursor},{failure={Incomplete}}))
    | {~failure} ->
       cursor = MongoCursor.reset(cursor)
       ({cc with ~cursor},{~failure})

  has_more(cc:collection_cursor('value)): bool = MongoCursor.valid(cc.cursor)

  find_all_unsafe(c:collection('value), select:Mongo.select('value), ignore_incomplete:bool): outcome(list('result),Mongo.failure) =
    //do println("find_all:\n  'value={OpaType.to_pretty(@typeval('value))}\n  'result={OpaType.to_pretty(@typeval('result))}")
    match (query_unsafe(c,select,ignore_incomplete): outcome(collection_cursor('result),Mongo.failure)) with
    | {success=cc} ->
       (cc,l) =
         while((cc,{success=[]}),
               ((cc,l) ->
                  match l with
                  | {success=l} ->
                     (match next(cc) with
                      | (cc,{success=v}) ->
                         //do println("  v={(v:'result)}")
                         ((cc,{success=[Magic.id(v):'result|l]}),has_more(cc))
                      | (cc,{failure={Incomplete}}) -> ((cc,{success=l}),has_more(cc))
                      | (cc,{~failure}) ->
                         //do println("  err(query)={MongoDriver.string_of_failure(failure)}")
                         ((cc,{~failure}),false))
                  | {~failure} -> ((cc,{~failure}),false)))
       _ = kill(cc)
       l
    | {~failure} -> {~failure}

  find_all(c:collection('value), select:Mongo.select('value)): outcome(list('value),Mongo.failure) =
    find_all_unsafe(c, select, false)

  count(c:collection('value), query_opt:option(Mongo.select('value))): outcome(int,Mongo.failure) =
    MongoCommands.count(c.db.mongo, c.db.dbname, c.db.collection, (Option.map((s -> s),query_opt)))

  distinct(c:collection('value), key:string, query_opt:option(Mongo.select('value))): outcome(list('b),Mongo.failure) =
    match MongoCommands.distinct(c.db.mongo, c.db.dbname, c.db.collection, key, (Option.map((s -> s),query_opt))) with
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
    MongoCommands.group(c.db.mongo, c.db.dbname, c.db.collection, key, reduce, initial, cond_opt, finalize_opt)

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

  kill(cc:collection_cursor('value)): collection_cursor('value) = { cc with cursor=MongoCursor.reset(cc.cursor) }

}}

/**
 * View {{ ... }}:
 *
 *  Alternative way of handling queries, we allow the return type to be different
 *  from, but a sub-type of, the parent collection's type.  We can't enforce
 *  type-safety at compile time but we do insert runtime type-checks into the
 *  view, both at creation time and at query time.  If you are 100% sure that your
 *  types are correct, you can eliminate the run-time type-check by simply using
 *  the Collection module "unsafe" operations.
 *
 *  The fields are selected by building a [fields] value using the Fields module.
 *  The result has to be cast to the return type which is derived from the collection
 *  type with the required fields included/excluded and turned into Bson.register values,
 *  for example, collection type {a:int; b:string} and field selector {b:1} results in
 *  the result type: {b:Bson.register(string)}.  This is checked at runtime.  Once the
 *  view has been created, you simply substitute the View query functions for the
 *  Collection functions.
 *
 *  Note that we are obliged to turn all the fields into Bson.register types because
 *  MongoDB will return a record with missing fields for documents which match the query
 *  but which do not have all of the fields selected.
 *
 **/
View = {{

  @private ML = MongoLog

  @private make_reg(fld) = {fld with ty={TyName_args=[fld.ty]; TyName_ident="Bson.register"}}

  @private
  type_from_fields(pty:OpaType.ty, fields:fields): OpaType.ty =
    if not(Fields.validate(fields))
    then ML.fatal("View.type_from_fields","Fields failed to validate",-1)
    else
      tst =
        match List.unique_list_of(List.map((e -> Bson.int_of_value(e.value)),fields)) with
        | [{some=num}] -> (match num with 0 -> not | _ -> (tf -> tf))
        | _ -> ML.fatal("View.type_from_fields","Bad fields value {fields}",-1)
      dfields = List.map((e -> String.explode(".",e.name)),fields)
      MongoTypeSelect.filter_field(pty, (fs -> tst(List.mem(fs,dfields))))

  @private
  verify_type_match(ty1:OpaType.ty, ty2:OpaType.ty, from:string, msg:string): void =
    //do println("ty1={OpaType.to_pretty(MongoTypeSelect.name_type(ty1))}")
    //do println("ty2={OpaType.to_pretty(MongoTypeSelect.name_type(ty2))}")
    // We can't use the fancy caching in compare_ty since our altered types mess with the caching
    if not(MongoTypeSelect.naive_type_compare(ty1, ty2))
    then ML.fatal(from,"{msg} {OpaType.to_pretty(ty1)} and {OpaType.to_pretty(ty2)}",-1)
    else void

  create(c:collection('collection), vfields:fields, is_opa:bool): view('collection,'view) =
    coll = Collection.fields(c, {some=vfields})
    pty = @typeval('collection)
    do verify_type_match(pty, coll.ty, "Collection.view","Attempt to create view from non-matching parent type")
    fvty = type_from_fields(pty, vfields)
    vty = if is_opa then fvty else MongoTypeSelect.map_field(fvty, make_reg)
    cvty = @typeval('view)
    //do println("pty={OpaType.to_pretty(pty)}")
    //do println("fvty={OpaType.to_pretty(fvty)}")
    //do println("vty={OpaType.to_pretty(vty)}")
    //do println("cvty={OpaType.to_pretty(cvty)}")
    do verify_type_match(vty, cvty, "Collection.view","Attempt to create view with incompatible view types")
    { ~coll; ~vty; ~is_opa; }

  of_collection(c:collection('collection), is_opa:bool): view('collection,'collection) = { coll=c; vty=c.ty; ~is_opa; }

  @private
  runtime_view_type_check(v:view('value,'view), from:string): void =
    do verify_type_match(@typeval('value), v.coll.ty, from, "Collection type does not match view type")
    do verify_type_match(@typeval('view), v.vty, from, "View type does not match result type")
    void

  find_one(v:view('value,'view), select:Mongo.select('value)): outcome('view,Mongo.failure) =
    do runtime_view_type_check(v, "View.find_one")
    Collection.find_one_unsafe(v.coll, select, v.is_opa)

  query(v:view('value,'view), select:Mongo.select('value)): outcome(collection_cursor('view),Mongo.failure) =
    do runtime_view_type_check(v, "View.query")
    Collection.query_unsafe(v.coll, select, v.is_opa)

  find_all(v:view('value,'view), select:Mongo.select('value)): outcome(list('view),Mongo.failure) =
    do runtime_view_type_check(v, "View.find_all")
    Collection.find_all_unsafe(v.coll, select, v.is_opa)

}}

Foreign = {{

  @private ML = MongoLog

  create(primary:view('ps,'pr), foreign:view('fs,'fr), pkey:string, fkey:string)
       : foreign('ps,'pr,'fs,'fr,('pr,Bson.register('fr))) =
    pty = @typeval('ps)
    pkt = MongoTypeSelect.find_label_in_row(pty,pkey)
    do if not(Option.is_some(pkt))
       then ML.fatal("Foreign.create","Can't find primary key {pkey} in type {OpaType.to_pretty(pty)}",-1)
    fty = @typeval('fs)
    fkt = MongoTypeSelect.find_label_in_row(fty,fkey)
    do if not(Option.is_some(fkt))
       then ML.fatal("Foreign.create","Can't find foreign key {fkey} in type {OpaType.to_pretty(fty)}",-1)
    do if not(MongoTypeSelect.naive_type_compare((Option.get(pkt)).ty,(Option.get(fkt)).ty))
       then ML.fatal("Foreign.create","Mismatching primary {OpaType.to_pretty(pty)} and foreign {OpaType.to_pretty(fty)}",-1)
    { ~primary; ~foreign; ~pkey; ~fkey }

  find_one(f:foreign('ps,'pr,'fs,'fr,'view), select:Mongo.select('ps)): outcome('view,Mongo.failure) =
    match Collection.find_one_doc(f.primary.coll, select) with
    | {success=pdoc} ->
       (match Bson.bson_to_opa(pdoc, @typeval('pr)) with
        | {some=pv} ->
           pv = (Magic.id(pv):'pr)
           (match Bson.dot_element(pdoc,f.pkey) with
            | {some=e} ->
               //do println("Foreign.find_one: e={Bson.to_pretty([e])}")
               (match Collection.find_one_doc(f.foreign.coll, ([{e with name=f.fkey}]:Mongo.select('fr))) with
                | {success=fdoc} ->
                   //do println("Foreign.find_one: fdoc={Bson.to_pretty(fdoc)}")
                   (match Bson.bson_to_opa(fdoc, @typeval('fr)) with
                    | {some=fv} -> {success=(pv,{present=Magic.id(fv):'fr})}
                    | {none} -> {failure={Error="Foreign.find_one: Bson to OPA conversion error for foreign value"}})
                | {failure=_} -> {success=(pv,{absent})})
            | {none} -> {failure={Error="Foreign.find_one: Can't find primary key {f.pkey}"}})
        | {none} -> {failure={Error="Foreign.find_one: Bson to OPA conversion error for primary value"}})
    | {~failure} -> {~failure}

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
       (match MongoConnection.getLastError(c.db) with
        | {~success} ->
           (match Bson.find_string(success, "err") with
            | {some=""} | {none} -> true
            | {some=err} -> do println("{msg}: {err}") false)
        | {~failure} -> do println("{msg}: fatal error {MongoDriver.string_of_failure(failure)}") false)

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

    find(c,r) = find_result_to_opt(Collection.find_one(c,MongoSelect.unsafe_make(r)))
    find_all(c,r) = find_all_result_to_list(Collection.find_all(c,MongoSelect.unsafe_make(r)))

    // Delete by id by default
    delete(c,id) = Collection.delete(c,MongoSelect.unsafe_make({_id = id}))

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
