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
 * MongoDB binding for OPA.
 *
 * @destination public
 * @stabilization work in progress
 **/

/**
 * {1 About this module}
 *
 * Module [MongoCollection] is an intermediate-level module providing a layer
 * of type-safety for the MongoDB driver low-level code.
 *
 * It implements the [Mongo.collection] type and it is also the target
 * for the new db syntax.
 *
 * Essentially, this datatype is simply a "typed" view of the low-level MongoDB
 * driver routines.  The currency here is OPA values, not BSON documents hence
 * we need to give a type to the collection.
 *
 * To use this module you need to construct [Mongo.select] and [Mongo.update]
 * values for queries and updates.  These are provided by the [MongoSelect]
 * and [MongoUpdate] modules, respectively.  These provide additional safety
 * checks over and above the runtime type-checking provided by this module.
 *
 * Helper modules are [Batch] which allows building a list of documents for
 * batch insert and [Fields] which is used to define field select documents.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

/**
 * The [Mongo.batch] type is used to build up lists
 * of documents for [insert_batch].
 **/
@abstract type Mongo.batch('a) = list(Bson.document)

/**
 * The [Mongo.fields] type is used to create field select
 * documents.  These should be used in conjunction with the
 * [MongoView] module.
 **/
@abstract type Mongo.fields = Bson.document

/**
 * The main collection type.  Just contains the [Mongo.mongodb]
 * connection object plus the (run-time) type of the collection.
 * The type is itself parametrised by the collection type but
 * this has to be cast by the user to the type of the collection.
 * {b Warning: If this type is incorrectly cast then serious problems
 * will result.  SegFaults will be the least of your worries.}
 **/
//@abstract
type Mongo.collection('a) = {
  db: Mongo.mongodb;
  ty: OpaType.ty; // type of the collection
}

/**
 * The type of a cursor associated with a collection.
 * This is also parametrised and cast as for the parent
 * collection type.  It contains the collection and the cursor plus some
 * additional information which enables run-time type checks upon
 * data as it is received from the MongoDB server.
 **/
//@abstract
type Mongo.collection_cursor('a) = {
  collection: Mongo.collection('a);
  cursor: Mongo.cursor;
  query: Mongo.select('a);
  ty: OpaType.ty;
  ignore_incomplete: bool;
}

/**
 * The [Mongo.group_result] type is used by the [MongoCollection.group] command.
 **/
type Mongo.group('a) = { retval:list('a); count:int; keys:int; ok:int }
type Mongo.group_result('a) = outcome(Mongo.group('a),Mongo.failure)

/**
 * Return a package of pre-typed values.
 **/
type Mongo.pkg('value) =
  (Mongo.collection('value),
   {select:(Bson.document -> Mongo.select('value));
    update:(Bson.document -> Mongo.update('value));
    sempty:Mongo.select('value);
    uempty:Mongo.update('value)})

MongoCollection = {{

  @private ML = MongoLog
  @private H = Bson.Abbrevs

  /**
   * Module [Batch] allows the management of a list of [Bson.document] values for
   * the [MongoCollection.insert_batch] command.
   **/
  Batch = {{
    empty() = ([]:Mongo.batch('a))
    add(b:Mongo.batch('a), v:'a): Mongo.batch('a) = [Bson.opa2doc(v)|b]
    one(v:'a): Mongo.batch('a) = [Bson.opa2doc(v)]
    add2(b:Mongo.batch('a), (v1:'a, v2:'a)): Mongo.batch('a) = [Bson.opa2doc(v1)|[Bson.opa2doc(v2)|b]]
    two(v1:'a, v2:'a): Mongo.batch('a) = [Bson.opa2doc(v1)|[Bson.opa2doc(v2)]]
    add3(b:Mongo.batch('a), (v1:'a, v2:'a, v3:'a)): Mongo.batch('a) = [Bson.opa2doc(v1)|[Bson.opa2doc(v2)|[Bson.opa2doc(v3)|b]]]
    three(v1:'a, v2:'a, v3:'a): Mongo.batch('a) = [Bson.opa2doc(v1)|[Bson.opa2doc(v2)|[Bson.opa2doc(v3)]]]
    list(b:Mongo.batch('a), vs:list('a)): Mongo.batch('a) = List.flatten([List.map(Bson.opa2doc,vs),b])
    of_list(vs:list('a)): Mongo.batch('a) = list(empty(),vs)
    merge(b1:Mongo.batch('a), b2:Mongo.batch('a)): Mongo.batch('a) = List.flatten([b1, b2])
  }}

  /**
   * Module [Fields] allows the construction and verification of field select documents.
   *
   * In [Bson.document] format, the field select is just a list of field names (possibly
   * in "dot" format) of value "0" or "1" for exclusion or inclusion of the field.  Note that
   * you can't mix included and excluded fields.
   *
   * The [Mongo.fields] type is just a synonym for [Bson.document] but the [validate]
   * function here encapsulates the restrictions indicated above.
   **/
  Fields = {{
    empty = ([]:Mongo.fields)
    add(f:Mongo.fields, name:string, incexc:Bson.int32): Mongo.fields = [H.i32(name,incexc)|f]
    one(name:string, incexc:Bson.int32): Mongo.fields = [H.i32(name,incexc)]
    list(f:Mongo.fields, fs:list((string,Bson.int32))): Mongo.fields = List.flatten([List.map(((n,ie) -> H.i32(n,ie)),fs),f])
    of_list(fs:list((string,Bson.int32))) = list(empty,fs)
    merge(f1:Mongo.fields, f2:Mongo.fields): Mongo.fields = List.flatten([f1, f2])
    validate(fields:Mongo.fields): bool =
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

  /**
   * Create a collection from a [Mongo.mongodb] connection.  The type of the connection
   * is remembered here and used to check the types of values returned from the MongoDB server.
   * Note, however, that we clone the connection so that we will be using the same connection
   * to the server as the parent connection.
   **/
  create(db:Mongo.mongodb): Mongo.collection('value) = { db=MongoConnection.clone(db); ty=@typeval('value); }

  /**
   * Open a connection and create a collection on top of it.  Unlike [create] the connection
   * is encapsulated in the collection object so that when the collection is destroyed the
   * connection is closed.
   **/
  open(name:string, dbname:string, collection:string): outcome(Mongo.collection('value),Mongo.failure) =
    match MongoConnection.open(name) with
    | {success=mongo} -> {success={ db=MongoConnection.namespace(mongo,dbname,collection); ty=@typeval('value); }}
    | {~failure} -> {~failure}

  /** Same as [open] but treat a failure to open the connection as a fatal error. **/
  openfatal(name:string, dbname:string, collection:string): Mongo.collection('value) =
    match open(name, dbname, collection) with
    | {success=coll} -> coll
    | {~failure} -> ML.fatal("MongoCollection.openfatal","Can't connect: {MongoDriver.string_of_failure(failure)}",-1)

  /** Supply a set of useful values associated with a collection **/
  makepkg(c:Mongo.collection('value)): Mongo.pkg('value) =
    (c,
     {select=MongoSelect.create;
      update=MongoUpdate.create;
      sempty=(MongoSelect.create(MongoSelectUpdate.empty()));
      uempty=(MongoUpdate.create(MongoSelectUpdate.empty()))})

  /** Same as [open] but returning pre-typed select and update functions **/
  openpkg(name:string, dbname:string, collection:string): outcome(Mongo.pkg('value),Mongo.failure) =
    MongoDriver.map_success(open(name,dbname,collection),makepkg)

  /** Same as [openfatal] but returning pre-typed select and update functions **/
  openpkgfatal(name:string, dbname:string, collection:string) : Mongo.pkg('value) =
    makepkg(openfatal(name,dbname,collection))

  /**
   * Destroy a collection.  Actually just close the cloned connection.
   **/
  destroy(c:Mongo.collection('value)): void = MongoConnection.close(c.db)

  /**
   * We adopt a functional view of the collection so that we can set various
   * parameters in separate copies of the connection.  This allows both globally
   * setting these parameters by setting them when the collection is created or
   * locally by updating them when a collection operation is performed.
   **/

  /** Set the "skip" number in the collection. **/
  skip(c:Mongo.collection('value), skip:int): Mongo.collection('value) =
    {c with db={ c.db with ~skip }}

  /** Set the "limit" number in the collection. **/
  limit(c:Mongo.collection('value), limit:int): Mongo.collection('value) =
    {c with db={ c.db with ~limit }}

  /** Set the "fields" parameter in the collection. This is a low-level field select
   *  document, care should be taken with it's use since you need to cast the
   *  collection type manually to match the fields which will be returned.
   *  Use the [MongoView] module for greater type safety.
   **/
  fields(c:Mongo.collection('value), fields:option(Bson.document)): Mongo.collection('value) =
    {c with db={ c.db with ~fields }}

  /** Set the "orderby" document in the collection. **/
  orderby(c:Mongo.collection('value), orderby:option(Bson.document)): Mongo.collection('value) =
    {c with db={ c.db with ~orderby }}

  /** Set the "continueOnError" flag for all [insert] calls. **/
  continueOnError(c:Mongo.collection('value)): Mongo.collection('value) =
    {c with db={ c.db with insert_flags=Bitwise.lor(c.db.insert_flags,MongoDriver.ContinueOnErrorBit) }}

  /** Set the "Upsert" flag for all [update] calls. **/
  upsert(c:Mongo.collection('value)): Mongo.collection('value)
    = {c with db={ c.db with update_flags=Bitwise.lor(c.db.update_flags,MongoDriver.UpsertBit) }}

  /** Set the "multiUpdate" flag for all [update] calls. **/
  multiUpdate(c:Mongo.collection('value)): Mongo.collection('value)
    = {c with db={ c.db with update_flags=Bitwise.lor(c.db.update_flags,MongoDriver.MultiUpdateBit) }}

  /** Set the "singleRemove" flag for all [delete] calls. **/
  singleRemove(c:Mongo.collection('value)): Mongo.collection('value)
    = {c with db={ c.db with delete_flags=Bitwise.lor(c.db.delete_flags,MongoDriver.SingleRemoveBit) }}

  /** Set the "tailableCursor" flag for all [query] calls. **/
  tailableCursor(c:Mongo.collection('value)): Mongo.collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,MongoDriver.TailableCursorBit) }}

  /** Set the "slaveOk" flag for all [query] calls. **/
  slaveOk(c:Mongo.collection('value)): Mongo.collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,MongoDriver.SlaveOkBit) }}

  /** Set the "oplogReplay" flag for all [query] calls. **/
  oplogReplay(c:Mongo.collection('value)): Mongo.collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,MongoDriver.OplogReplayBit) }}

  /** Set the "noCursorTimeout" flag for all [query] calls. **/
  noCursorTimeout(c:Mongo.collection('value)): Mongo.collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,MongoDriver.NoCursorTimeoutBit) }}

  /** Set the "awaitData" flag for all [query] calls. **/
  awaitData(c:Mongo.collection('value)): Mongo.collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,MongoDriver.AwaitDataBit) }}

  /** Set the "exhaust" flag for all [query] calls. **/
  exhaust(c:Mongo.collection('value)): Mongo.collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,MongoDriver.ExhaustBit) }}

  /** Set the "partial" flag for all [query] calls. **/
  partial(c:Mongo.collection('value)): Mongo.collection('value)
    = {c with db={ c.db with query_flags=Bitwise.lor(c.db.query_flags,MongoDriver.PartialBit) }}

  @private reply_to_result(from:string, reply_opt: option(Mongo.reply)): Mongo.result =
    match reply_opt with
    | {some=reply} ->
       (match MongoDriver.reply_document(reply,0) with
        | {some=doc} -> {success=doc}
        | {none} -> {failure={Error="{from}: no document in reply"}})
    | {none} -> {failure={Error="{from}: no reply"}}

  /**
   * Insert an OPA value into a collection.
   **/
  insert(c:Mongo.collection('value), v:'value): bool =
    ns = c.db.dbname^"."^c.db.collection
    b = Bson.opa_to_bson(v,{some=@typeval('value)})
    MongoDriver.insert(c.db.mongo,c.db.insert_flags,ns,b)

  /** insert with getlasterror **/
  inserte(c:Mongo.collection('value), v:'value): Mongo.result =
    ns = c.db.dbname^"."^c.db.collection
    b = Bson.opa_to_bson(v,{some=@typeval('value)})
    reply_to_result("MongoConnection.insert",MongoDriver.inserte(c.db.mongo,c.db.insert_flags,ns,c.db.dbname,b))

  /**
   * Batch insert, you need to build the batch using the [Batch] module.
   **/
  insert_batch(c:Mongo.collection('value), b:Mongo.batch('value)): bool =
    ns = c.db.dbname^"."^c.db.collection
    MongoDriver.insert_batch(c.db.mongo,c.db.insert_flags,ns,b)

  /** insert_batch with getlasterror **/
  insert_batche(c:Mongo.collection('value), b:Mongo.batch('value)): Mongo.result =
    ns = c.db.dbname^"."^c.db.collection
    reply_to_result("MongoConnection.insert_barch",MongoDriver.insert_batche(c.db.mongo,c.db.insert_flags,ns,c.db.dbname,b))

  /**
   * Update a value in a collection.
   *
   * Example: [update(c, select, update)]
   *
   * The [select] and [update] parameters should be built by the [MongoSelect]
   * and [MongoUpdate] modules and cast to the type of the collection.
   * Doing it this way allows a run-time type check to be performed on
   * the [select] and [update] values.  Suitability for either select or update
   * is also enforced.
   **/
  update(c:Mongo.collection('value), select:Mongo.select('value), update:Mongo.update('value)): bool =
    ns = c.db.dbname^"."^c.db.collection
    MongoDriver.update(c.db.mongo,c.db.update_flags,ns,select,update)

  /** update with getlasterror **/
  updatee(c:Mongo.collection('value), select:Mongo.select('value), update:Mongo.update('value)): Mongo.result =
    ns = c.db.dbname^"."^c.db.collection
    reply_to_result("MongoConnection.update",MongoDriver.updatee(c.db.mongo,c.db.update_flags,ns,c.db.dbname,select,update))

  /**
   * Delete values in a collection according to a select value.
   **/
  delete(c:Mongo.collection('value), select:Mongo.select('value)): bool =
    ns = c.db.dbname^"."^c.db.collection
    MongoDriver.delete(c.db.mongo,c.db.delete_flags,ns,select)

  /** delete with getlasterror **/
  deletee(c:Mongo.collection('value), select:Mongo.select('value)): Mongo.result =
    ns = c.db.dbname^"."^c.db.collection
    reply_to_result("MongoConnection.delete",MongoDriver.deletee(c.db.mongo,c.db.delete_flags,ns,c.db.dbname,select))

  /**
   * Return the [Bson.document] representation of a single value selected from
   * a collection.  This might facilitate more efficient handling of values
   * rather than converting to an OPA type.
   **/
  find_one_doc(c:Mongo.collection('value), select:Mongo.select('value)): Mongo.result =
    ns = c.db.dbname^"."^c.db.collection
    MongoCursor.find_one(c.db.mongo,ns,select,c.db.fields,c.db.orderby)

  /**
   * Find a single collection value according to a select value but allow the type
   * of the value to be different from the collection type.
   *
   * This effectively short-circuits the static type-check on the return value
   * but is essential for cases where the return type is different, for example
   * the "$explain" data and [View.view] operations.
   *
   * Example: [find_one_unsafe(c, select, ignore_incomplete)]
   *
   * @param [ignore_incomplete] if [true] will cause any documents selected
   * which do not have all of the fields defined in the result type to be
   * filtered out.  Since this finds a single value, if it has missing fields
   * it will be flagged as not found.
   * @return The return outcome [failure] type includes the [\{Incomplete\}]
   * value.  If [ignore_incomplete] is [false] and fields are missing, then
   * this specific failure is returned.
   **/
  find_one_unsafe(c:Mongo.collection('value), select:Mongo.select('value), ignore_incomplete:bool)
    : outcome('result,Mongo.failure) =
    ns = c.db.dbname^"."^c.db.collection
    (match MongoCursor.find_one(c.db.mongo,ns,select,c.db.fields,c.db.orderby) with
     | {success=doc} ->
        (match Bson.b2o_incomplete(doc, @typeval('result), ignore_incomplete) with
         | {found=v} -> {success=(Magic.id(v):'result)}
         | {not_found} -> {failure={Error="MongoCollection.find_one: not found"}}
         | {incomplete} -> {failure={Incomplete}})
     | {~failure} -> {~failure})

  /**
   * A safer version of [find_one_unsafe].  The return type must match the collection type
   * and [ignore_incomplete] will always be [false].
   **/
  find_one(c:Mongo.collection('value), select:Mongo.select('value)): outcome('value,Mongo.failure) =
    find_one_unsafe(c, select, false)

  /**
   * Perform a fully-parametrised query (from the parameters in the collection) and cast any
   * returned values to the return type specified here.
   *
   * @param [ignore_incomplete] as for [find_one_unsafe].
   * @return A [Mongo.collection_cursor] type which retains all of the parameters
   * used in the query.  The [MongoCollection.next] function will always return
   * values according to the parameters at this query call.
   **/
  query_unsafe(c:Mongo.collection('value), select:Mongo.select('value), ignore_incomplete:bool)
             : outcome(Mongo.collection_cursor('result),Mongo.failure) =
    ns = c.db.dbname^"."^c.db.collection
    match MongoCursor.find(c.db.mongo,ns,select,c.db.fields,c.db.orderby,c.db.limit,c.db.skip,c.db.query_flags) with
    | {success=cursor} ->
       {success={collection=@unsafe_cast(c); ~cursor; query=@unsafe_cast(select); ty=@typeval('result); ~ignore_incomplete}}
    | {~failure} -> {~failure}

  /**
   * The safer version of "query_unsafe".
   **/
  query(c:Mongo.collection('value), select:Mongo.select('value)): outcome(Mongo.collection_cursor('value),Mongo.failure) =
    query_unsafe(c, select, false)

  /**
   * Reset the cursor and re-issue the original query.
   **/
  first(cc:Mongo.collection_cursor('value)): outcome(Mongo.collection_cursor('value),Mongo.failure) =
    _ = MongoCursor.reset(cc.cursor)
    query(cc.collection, cc.query)

  /**
   * Return the next value from a [Mongo.collection_cursor].
   * The values are always returned according to the original [query] or [query_unsafe] call.
   *
   * {b Warning: [Mongo.collection_cursor] values are functional values, you have to retain the
   * modified cursor if you want the cursor to correctly track the MongoDB server cursor.}
   **/
  next(cc:Mongo.collection_cursor('value)): (Mongo.collection_cursor('value),outcome('value,Mongo.failure)) =
    cursor = MongoCursor.next(cc.cursor)
    match MongoCursor.check_cursor_error(cursor) with
    | {success=doc} ->
       (match Bson.b2o_incomplete(doc, cc.ty, cc.ignore_incomplete) with
        | {found=v} -> ({cc with ~cursor},{success=(Magic.id(v):'value)})
        | {not_found} -> ({cc with ~cursor},{failure={Error="MongoCollection.next: not found"}})
        | {incomplete} ->  ({cc with ~cursor},{failure={Incomplete}}))
    | {~failure} ->
       cursor = MongoCursor.reset(cursor)
       ({cc with ~cursor},{~failure})

  /**
   * Test if there is more data in a cursor.
   **/
  has_more(cc:Mongo.collection_cursor('value)): bool = MongoCursor.valid(cc.cursor)

  /**
   * Create a [Mongo.collection_cursor], scan in all the selected values according to
   * the select provided here and return a list of valid values according to the result
   * type.  Incomplete values are filtered out according to [ignore_incomplete].  The
   * cursor is killed once the operation is complete.  Remember you can use the "skip"
   * and "limit" functions to control which values are returned out of the list of possible
   * values.
   **/
  find_all_unsafe(c:Mongo.collection('value), select:Mongo.select('value), ignore_incomplete:bool)
    : outcome(list('result),Mongo.failure) =
    match (query_unsafe(c,select,ignore_incomplete): outcome(Mongo.collection_cursor('result),Mongo.failure)) with
    | {success=cc} ->
       (cc,l) =
         while((cc,{success=[]}),
               ((cc,l) ->
                  match l with
                  | {success=l} ->
                     (match next(cc) with
                      | (cc,{success=v}) -> ((cc,{success=[Magic.id(v):'result|l]}),has_more(cc))
                      | (cc,{failure={Incomplete}}) -> ((cc,{success=l}),has_more(cc))
                      | (cc,{~failure}) -> ((cc,{~failure}),false))
                  | {~failure} -> ((cc,{~failure}),false)))
       _ = kill(cc)
       l
    | {~failure} -> {~failure}

  /**
   * Safer version of [find_all_unsafe].
   **/
  find_all(c:Mongo.collection('value), select:Mongo.select('value)): outcome(list('value),Mongo.failure) =
    find_all_unsafe(c, select, false)

  /**
   * Count the number of documents matching the given optional select value (\{none\} means
   * count all documents.
   **/
  count(c:Mongo.collection('value), select_opt:option(Mongo.select('value))): outcome(int,Mongo.failure) =
    MongoCommands.count(c.db, c.db.dbname, c.db.collection, (Option.map((s -> s),select_opt)))

  /**
   * List the distinct values matching the optional select.
   * Example: [distinct(c, key, select_opt)]
   * @return The return type must be cast to a list of the type of the field indicated
   * by the [key] parameter.
   **/
  distinct(c:Mongo.collection('value), key:string, select_opt:option(Mongo.select('value))): outcome(list('b),Mongo.failure) =
    match MongoCommands.distinct(c.db, c.db.dbname, c.db.collection, key, (Option.map((s -> s),select_opt))) with
    | {success=doc} ->
       // possibly: get the type from 'value and get the key type out of there???
       ty = {TyName_args=[@typeval('b)]; TyName_ident="list"}
       (match Bson.bson_to_opa(doc, ty) with
        | {some=v} -> {success=(Magic.id(v):list('b))}
        | {none} -> {failure={Error="MongoCollection.distinct: not found"}})
    | {~failure} -> {~failure}

  /**
   * Perform a MongoDB "group" operation.
   * Here we just perform the group command and return the resulting [Mongo.result] value.
   * Use the [analyze_group] function to extract the results, unless you wish to scan
   * the result as a [Bson.document] value.
   *
   * Example: [group(c, key, reduce, initial, cond_opt, finalize_opt)]
   *
   * The parameters are as for the MongoDB documentation, for example, [reduce] is a
   * Javascript function as a string, converted to BSON Code for the function call.
   * Don't forget to escape the curly brackets in the Javascript strings.
   *
   * Note that for group to work ints have to match, Int32 will not match Int64!!!
   **/
  group(c:Mongo.collection('value), key:Bson.document, reduce:string, initial:Bson.document,
        cond_opt:option(Bson.document), finalize_opt:option(string)): Mongo.result =
    MongoCommands.group(c.db, c.db.dbname, c.db.collection, key, reduce, initial, cond_opt, finalize_opt)

  /**
   * Analyze the result of a [group] call.  The result is returned as a [Mongo.group_result]
   * value but this time, you have to cast the type parameter to the type of the field of the [key],
   * but with the field [\{count:float\}] added.
   **/
  // TODO: use Command types and doc2opa
  analyze_group(res:Mongo.result): Mongo.group_result('a) =
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
                       | {none} -> {failure={Error="MongoCollection.analyze_group: ok not found"}})
                   | {none} -> {failure={Error="MongoCollection.analyze_group: keys not found"}})
               | {none} -> {failure={Error="MongoCollection.analyze_group: count not found"}})
           | {none} -> {failure={Error="MongoCollection.analyze_group: retval not found"}})
       | _ -> {failure={Error="MongoCollection.analyze_group: no retval value in reply"}})
    | {~failure} -> {~failure}

  // TODO: map-reduce

  /**
   * Kill a [Mongo.collection_cursor] value.
   **/
  kill(cc:Mongo.collection_cursor('value)): Mongo.collection_cursor('value) =
    { cc with cursor=MongoCursor.reset(cc.cursor) }

}}

// End of file collection.opa
