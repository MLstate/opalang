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


/**
 * View {{ ... }}:
 *
 *  Alternative way of handling queries, we allow the return type to be different
 *  from, but a sub-type of, the parent collection's type.  We can't enforce
 *  type-safety at compile time but we do insert runtime type-checks into the
 *  view, both at creation time and at query time.  If you are 100% sure that your
 *  types are correct, you can eliminate the run-time type-check by simply using
 *  the MongoCollection module "unsafe" operations.
 *
 *  The fields are selected by building a [fields] value using the Fields module.
 *  The result has to be cast to the return type which is derived from the collection
 *  type with the required fields included/excluded and turned into Bson.register values,
 *  for example, collection type {a:int; b:string} and field selector {b:1} results in
 *  the result type: {b:Bson.register(string)}.  This is checked at runtime.  Once the
 *  view has been created, you simply substitute the View query functions for the
 *  MongoCollection functions.
 *
 *  Note that we are obliged to turn all the fields into Bson.register types because
 *  MongoDB will return a record with missing fields for documents which match the query
 *  but which do not have all of the fields selected.
 *
 **/

type view('a,'b) = {
  coll: Mongo.collection('a);
  vty: OpaType.ty; // type of the view collection
  is_opa: bool; // if true, we assume an OPA type and ignore incomplete documents
}

type foreign('a,'b,'c,'d,'e) = {
  primary: view('a,'b); // the parent view
  foreign: view('c,'d); // the foreign view
  pkey: string;
  fkey: string;
}

View = {{

  @private ML = MongoLog

  @private make_reg(fld) = {fld with ty={TyName_args=[fld.ty]; TyName_ident="Bson.register"}}

  @private
  type_from_fields(pty:OpaType.ty, fields:Mongo.fields): OpaType.ty =
    if not(MongoCollection.Fields.validate(fields))
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

  create(c:Mongo.collection('collection), vfields:Mongo.fields, is_opa:bool): view('collection,'view) =
    coll = MongoCollection.fields(c, {some=vfields})
    pty = @typeval('collection)
    do verify_type_match(pty, coll.ty, "View.create","Attempt to create view from non-matching parent type")
    fvty = type_from_fields(pty, vfields)
    vty = if is_opa then fvty else MongoTypeSelect.map_field(fvty, make_reg)
    cvty = @typeval('view)
    //do println("pty={OpaType.to_pretty(pty)}")
    //do println("fvty={OpaType.to_pretty(fvty)}")
    //do println("vty={OpaType.to_pretty(vty)}")
    //do println("cvty={OpaType.to_pretty(cvty)}")
    do verify_type_match(vty, cvty, "View.create","Attempt to create view with incompatible view types")
    { ~coll; ~vty; ~is_opa; }

  of_collection(c:Mongo.collection('collection), is_opa:bool): view('collection,'collection) = { coll=c; vty=c.ty; ~is_opa; }

  @private
  runtime_view_type_check(v:view('value,'view), from:string): void =
    do verify_type_match(@typeval('value), v.coll.ty, from, "Collection type does not match view type")
    do verify_type_match(@typeval('view), v.vty, from, "View type does not match result type")
    void

  find_one(v:view('value,'view), select:Mongo.select('value)): outcome('view,Mongo.failure) =
    do runtime_view_type_check(v, "View.find_one")
    MongoCollection.find_one_unsafe(v.coll, select, v.is_opa)

  query(v:view('value,'view), select:Mongo.select('value)): outcome(Mongo.collection_cursor('view),Mongo.failure) =
    do runtime_view_type_check(v, "View.query")
    MongoCollection.query_unsafe(v.coll, select, v.is_opa)

  find_all(v:view('value,'view), select:Mongo.select('value)): outcome(list('view),Mongo.failure) =
    do runtime_view_type_check(v, "View.find_all")
    MongoCollection.find_all_unsafe(v.coll, select, v.is_opa)

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
    match MongoCollection.find_one_doc(f.primary.coll, select) with
    | {success=pdoc} ->
       (match Bson.bson_to_opa(pdoc, @typeval('pr)) with
        | {some=pv} ->
           pv = (Magic.id(pv):'pr)
           (match Bson.dot_element(pdoc,f.pkey) with
            | {some=e} ->
               //do println("Foreign.find_one: e={Bson.to_pretty([e])}")
               (match MongoCollection.find_one_doc(f.foreign.coll, ([{e with name=f.fkey}]:Mongo.select('fr))) with
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
   safe_(c:Mongo.collection('value),f:'a->bool,a:'a,msg:string): bool =
     if not(f(a))
     then (do println("{msg}: Fatal error message not sent to server") false)
     else
       (match MongoConnection.getLastError(c.db) with
        | {~success} ->
           (match Bson.find_string(success, "err") with
            | {some=""} | {none} -> true
            | {some=err} -> do println("{msg}: {err}") false)
        | {~failure} -> do println("{msg}: fatal error {MongoDriver.string_of_failure(failure)}") false)

   safe_insert(c,v) = safe_(c,((c,v) -> MongoCollection.insert(c,v)),(c,v),"Collection.insert")
   safe_insert_batch(c,b) = safe_(c,((c,b) -> MongoCollection.insert_batch(c,b)),(c,b),"Collection.insert_batch")
   safe_update(c,s,v) = safe_(c,((c,s,v) -> MongoCollection.update(c,s,v)),(c,s,v),"Collection.update")
   safe_delete(c,s) = safe_(c,((c,s) -> MongoCollection.delete(c,s)),(c,s),"Collection.delete")

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

    find(c,r) = find_result_to_opt(MongoCollection.find_one(c,MongoSelect.unsafe_make(r)))
    find_all(c,r) = find_all_result_to_list(MongoCollection.find_all(c,MongoSelect.unsafe_make(r)))

    // Delete by id by default
    delete(c,id) = MongoCollection.delete(c,MongoSelect.unsafe_make({_id = id}))

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
