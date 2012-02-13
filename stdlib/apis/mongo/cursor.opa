/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
package stdlib.apis.mongo

/**
 * MongoDB binding for OPA.
 *
 * @destination public
 * @stabilization work in progress
 **/

/**
 * {1 About this module}
 *
 * Module [MongoCursor] has the cursor handling routines.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

/**
 * type [Mongo.cursor]:
 *
 * Contains all the parameters for an OP_QUERY call.
 *
 * Stores the reply.
 *
 * Handles indexes into the list of documents returned and
 * keeps a note of the last document parsed.
 *
 * Also handles the cursor ID.
 *
 * Use [MongoCursor.reset] when not needed, this will generate a
 * [kill_cursors] call to the server to clean up.
 **/
@abstract
type Mongo.cursor = {
  mongo : Mongo.db;
  ns : string;
  flags : int;
  skip : int;
  limit : int;
  query : option(Bson.document);
  fields : option(Bson.document);
  orderby : list((string,int));
  query_sent : bool;
  cid : Mongo.cursorID;
  reply : option(Mongo.reply);
  returned : int;
  current : int;
  doc : Bson.document;
  killed : bool;
  error : Mongo.failure;
}

@server_private
MongoCursor = {{

  @private ML = MongoLog
  @private H = Bson.Abbrevs

  @private error_document(err:string, code:int): Bson.document = [H.str("$err",err), H.i32("code",code)]

  /**
   * Bare cursor initialize.
   *
   * {b Warning:} Remember to cleanup cursor objects with [MongoCursor.reset].
   **/
  init(mongo:Mongo.db, ns:string): Mongo.cursor =
  { ~mongo;
    ~ns;
    flags = 0;
    skip = 0;
    limit = 1;
    query = {none};
    fields = {none};
    orderby = [];
    query_sent = {false};
    cid = MongoCommon.null_cursorID(void);
    reply = {none};
    returned = 0;
    current = 0;
    doc = error_document("Uninitialised document",-1);
    killed = {false};
    error = {OK}
  }

  /**
   * Set cursor parameters.
   *
   * These are named as for the arguments to a [MongoDriver.query] call.
   **/
  set_flags(c:Mongo.cursor, flags:int): Mongo.cursor = { c with ~flags }
  set_skip(c:Mongo.cursor, skip:int): Mongo.cursor = { c with ~skip }
  set_limit(c:Mongo.cursor, limit:int): Mongo.cursor = { c with ~limit }
  set_query(c:Mongo.cursor, query:option(Bson.document)): Mongo.cursor = { c with ~query }
  set_fields(c:Mongo.cursor, fields:option(Bson.document)): Mongo.cursor = { c with ~fields }
  set_orderby(c:Mongo.cursor, orderby:list((string,int))): Mongo.cursor = { c with ~orderby }

  tailable(c:Mongo.cursor): Mongo.cursor = { c with flags=Bitwise.lor(c.flags, MongoCommon.TailableCursorBit) }

  @private
  set_error(c:Mongo.cursor, error:Mongo.failure): Mongo.cursor = { c with ~error; killed={true} }

  @private
  reply(c:Mongo.cursor, reply_opt:option(Mongo.reply), name:string, query_sent:bool): Mongo.cursor =
    match reply_opt with
    | {some=reply} ->
      cursorID = MongoCommon.reply_cursorID(reply)
      { c with
          cid = cursorID;
          reply = {some=reply};
          ~query_sent;
          returned = MongoCommon.reply_numberReturned(reply);
          current = 0;
          doc = error_document("Uninitialised document",-1);
      }
    | {none} -> set_error(c,{Error="MongoCursor.{name}: no reply"})

  /**
   * Perform an OP_QUERY call to the database server based on the parameters
   * stored in the cursor object.
   *
   * Will only work on valid cursors, ie. those which have not received an error.
   *
   * Note that no tests are performed on the reply, there are other routines which
   * examine the content of the reply.  You may, however, get a comms error here.
   **/
  op_query(c:Mongo.cursor): Mongo.cursor =
    if not(c.killed) && Option.is_some(c.query)
    then
      query = (match c.orderby with
               | [] -> Option.get(c.query)
               | orderby -> [H.doc("$query",Option.get(c.query)), H.doc("$orderby",List.map(((f,o) -> H.i32(f,o)),orderby))])
      reply(c,MongoDriver.query(c.mongo, c.flags, c.ns, c.skip, c.limit, query, c.fields),"op_query",{true})
    else set_error(c,{Error=(if c.killed
                             then "MongoCursor.op_query: already killed"
                             else "MongoCursor.op_query: no query")})

  /**
   * Perform an OP_GETMORE call, if a valid cursor ID exists in the cursor.
   **/
  get_more(c:Mongo.cursor): Mongo.cursor =
    if not(c.killed) && not(MongoCommon.is_null_cursorID(c.cid))
    then reply(c,MongoDriver.get_more(c.mongo, c.ns, c.limit, c.cid),"get_more",c.query_sent)
    else set_error(c,{Error="MongoCursor.get_more: attempt to get more with dead cursor"})

  /**
   * Return the [n]'th document in the reply stored in a cursor.
   *
   * This is a low-level routine, use [MongoCursor.next] to scan the returned values
   * while sending additional OP_GETMORE calls when necessary.
   */
  document(c:Mongo.cursor, n:int): Mongo.result =
    if n >= c.returned
    then {failure={Error="MongoCursor.document: document index out of range {n}"}}
    else
      match c.reply with
      | {some=reply} ->
        (match MongoCommon.reply_document(reply,n) with
         | {some=doc} -> {success=doc}
         | {none} -> {failure={Error="MongoCursor.document: no document"}})
      | {none} -> {failure={Error="MongoCursor.document: no reply"}}

  /**
   * Return all the documents in the reply stored in a cursor.
   **/
  all_documents(c:Mongo.cursor): Mongo.results =
    match c.reply with
    | {some=reply} ->
      rec aux(n:int) =
       if n >= c.returned
       then []
       else (match MongoCommon.reply_document(reply,n) with
             | {some=doc} -> (doc +> (aux(n+1)))
             | {none} -> (aux(n+1)))
      {success=aux(0)}
    | {none} -> {failure={Error="MongoCursor.document: no reply"}}

  @private
  destroy(c:Mongo.cursor): Mongo.cursor =
    { c with
        error={Error="reset"};
        doc=error_document("Dead cursor",-1);
        killed={true};
        cid=MongoCommon.null_cursorID(void)
    }

  /**
   * Destroy a cursor object.
   *
   * Deletes buffer storage and sends a OP_KILL_CURSOR call if a valid cursor
   * ID still exists in the cursor.
   **/
  reset(c:Mongo.cursor): Mongo.cursor =
    if not(MongoCommon.is_null_cursorID(c.cid))
    then
      if MongoDriver.kill_cursors(c.mongo, [c.cid])
      then destroy(c)
      else set_error(destroy(c),{Error="MongoCursor.reset: error killing cursor"})
    else destroy(c)

  /**
   * Get the next returned object from a cursor.
   *
   * If an OP_QUERY has not been sent then it will be sent with the current
   * cursor parameters.
   *
   * The current document in the cursor is set to the next available document,
   * calling OP_GETMORE as required.
   *
   * On error, the current document will be set to a (fabricated) error BSON document.
   *
   * {b Warning:} Does not implement tailable cursors, yet.
   *
   * {b Warning:} Does not check the return flags.
   */
  rec next(c:Mongo.cursor): Mongo.cursor =
    c = if not(c.query_sent) then op_query(c) else c
    if Option.is_none(c.reply)
    then set_error(c,{Error="MongoCursor.next: no reply"})
    else
      // TODO: analyze return flags
      // TODO: tailable cursors
      if c.returned <= 0
      then
        tags = MongoCommon.reply_tags(MongoCommon.reply_responseFlags(Option.get(c.reply)))
        tags = List.filter((tag -> tag != {rtag={AwaitCapable}}),tags)
        if List.is_empty(tags)
        then set_error(c,{NotFound})
        else set_error(c,{Error="MongoCursor.next: no data returned tags={MongoCommon.string_of_tags(tags)}"})
      else
        if c.current >= c.returned
        then
          if MongoCommon.is_null_cursorID(c.cid)
          then set_error({c with doc = error_document("Read past end of data",-1)},{Error="MongoCursor.next: end of data"})
          else next(get_more(c))
        else {c with
                current=c.current+1;
                doc=(match MongoCommon.reply_document(Option.get(c.reply),c.current) with
                     | {some=doc} -> doc
                     | {none} -> error_document("Reply parse error",-1))}

  /** Identical to for() except it allows update of the state in the conditional **/
  for(init:'state, next:'state -> 'state, cond:'state -> ('state, bool)): 'state =
    (v,continue) = cond(init)
    if continue
    then for(next(v),next,cond)
    else v

  /**
   * Create and initialise cursor with given query and default options.
   *
   * Example: [start(m, ns, query, limit)]
   *
   * Intended to form a set of functions to enable the idiom: [for(start(...),(c -> ... next(c)),valid)].
   *
   * Note: you can't actually use the OPA initial for() function with this idiom without
   * missing the last element (you can't update the loop variable within the conditional for end of loop).
   * Use the MongoCursor.for() function instead.
   *
   * Note: MongoDB seems to interpret limit=1 as "just send me one document".
   * If you want this loop to scan all the documents you can't use limit=1.
   * This routine prints a warning message because if you only want one document you
   * would be better using [MongoCursor.find_one].
   **/
  start(m:Mongo.db, ns:string, query:Bson.document, limit:int): Mongo.cursor =
    c = init(m,ns)
    do if limit == 1 then ML.warning("MongoCursor.start","Cursor with limit==1 will only return one document.",void)
    c = set_limit(c,limit)
    c = set_query(c,{some=query})
    next(c)

  /**
   * Test if there is still data in a cursor.
   **/
  valid(c:Mongo.cursor): bool =
    not(c.killed)
    && ((not(c.query_sent) && Option.is_some(c.query)) // initialised but not run
        || ((c.returned > 0 && (c.current < c.returned))) // run and still has data
        || not(MongoCommon.is_null_cursorID(c.cid))) // non-zero cursor, more data on server

  /**
   * Full [find] function with all parameters.
   *
   * Creates a cursor with the given parameters and calls OP_QUERY to
   * initiate communications.
   *
   * The cursor value is then returned, you can then use [MongoCursor.next] to
   * scan along from there.
   **/
  find(m:Mongo.db, ns:string, query:Bson.document, fields:option(Bson.document), orderby:list((string,int)),
       limit:int, skip:int, flags:int): outcome(Mongo.cursor,Mongo.failure) =
    c = init(m, ns)
    c = set_query(c, {some=query})
    c = set_fields(c, fields)
    c = set_orderby(c, orderby)
    c = set_limit(c, limit)
    c = set_skip(c, skip)
    c = set_flags(c, flags)
    c = op_query(c)
    if c.killed
    then {failure={Error="find: query error"}}
    else {success=c}

  @private
  check_err(b:Bson.document): Mongo.result =
    match Bson.find(b,"$err") with
    | {some=err_doc} -> {failure={DocError=err_doc}}
    | {none} -> {success=b}

  /**
   * If a cursor is valid then return an [outcome] with the current
   * document.  Will return a [failure] document if "$err" exists in
   * the document.
   **/
  check_cursor_error(c:Mongo.cursor): Mongo.result =
    if not(c.killed)
    then check_err(c.doc)
    else {failure=c.error}

  /**
   * Find the first matching document for the given namespace.
   *
   * Example: [find_one(m, ns, query, fields, orderby)]
   *
   * Creates and destroys a cursor.
   **/
  find_one(m:Mongo.db, ns:string,
           query:Bson.document, fields:option(Bson.document), orderby:list((string,int))): Mongo.result =
    c = init(m, ns)
    c = set_query(c, {some=query})
    c = set_fields(c, fields)
    c = set_orderby(c, orderby)
    c = set_limit(c, 1)
    c = next(c)
    outcome = check_cursor_error(c)
    _ = reset(c)
    outcome

  /**
   * Find all matching documents for the given namespace.
   *
   * Example: [find_all(m, ns, query, limit)]
   *
   * Creates and destroys a cursor.
   *
   * {b NOTE: reverses the order.}
   **/
  find_all(m:Mongo.db, ns:string, query:Bson.document,
           fields:option(Bson.document), orderby:list((string,int)), limit:int): Mongo.results =
    cursor = init(m, ns)
    cursor = set_query(cursor, {some=query})
    cursor = set_fields(cursor, fields)
    cursor = set_orderby(cursor, orderby)
    cursor = set_limit(cursor, limit)
    (cursor,results) =
      for((cursor,{success=[]}),
          ((cursor,results) ->
            match results with
            | {success=docs} ->
               (match check_cursor_error(cursor) with
                | {success=doc} -> (cursor,{success=[doc|docs]})
                | {~failure} -> (cursor,{~failure}))
            | {~failure} -> (cursor,{~failure})
          ),
          ((cursor,results) ->
            match results with
            | {success=_} ->
               if valid(cursor)
               then ((next(cursor),results),true)
               else ((cursor,results),false)
            | {failure=_} -> ((cursor,results),false)
          ))
    _ = reset(cursor)
    results

}}

// End of file cursor.opa
