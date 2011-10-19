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
 * Module [Cursor] has the cursor handling routines.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

/* Major TODOs, there are minor ones elsewhere. */
// TODO: sort function for cursors

/**
 * type Cursor.cursor:
 *   - Contains all the parameters for an op_query call.
 *   - Stores the reply.
 *   - Handles indexes into the list of documents returned and
 *     keeps a note of the last document parsed.
 *   - Also handles the cursor ID.
 *   - Use Cursor.reset when not needed, this will generate a
 *     kill_cursors call to the server to clean up.
 **/
type Cursor.cursor = {
     mongo : Mongo.db;
     ns : string;
     flags : int;
     skip : int;
     limit : int;
     query : option(Bson.document);
     fields : option(Bson.document);
     orderby : option(Bson.document);
     query_sent : bool;
     cid : cursorID;
     reply : option(reply);
     returned : int;
     current : int;
     doc : Bson.document;
     killed : bool;
     error : string
}

@server_private
Cursor = {{

  @private
  error_document(err:string, code:int): Bson.document = [H.str("$err",err), H.i32("code",code)]

  /**
   * Bare cursor initialize.
   *
   *   {b Warning:} Note that each time you create a cursor it generates buffers to talk to
   *   the MongoDB server.  Remember to cleanup cursor objects with [Cursor.reset].
   **/
  init(mongo:Mongo.db, ns:string): Cursor.cursor =
  { mongo = Mongo.copy(mongo);
    ~ns;
    flags = 0;
    skip = 0;
    limit = 1;
    query = {none};
    fields = {none};
    orderby = {none};
    query_sent = {false};
    cid = Mongo.null_cursorID(void);
    reply = {none};
    returned = 0;
    current = 0;
    doc = error_document("Uninitialised document",-1);
    killed = {false};
    error = "<ok>"
  }

  /**
   * Set cursor parameters.
   *
   * These are named as for the arguments to a [Mongo.query] call.
   **/
  set_flags(c:Cursor.cursor, flags:int): Cursor.cursor = { c with ~flags }
  set_skip(c:Cursor.cursor, skip:int): Cursor.cursor = { c with ~skip }
  set_limit(c:Cursor.cursor, limit:int): Cursor.cursor = { c with ~limit }
  set_query(c:Cursor.cursor, query:option(Bson.document)): Cursor.cursor = { c with ~query }
  set_fields(c:Cursor.cursor, fields:option(Bson.document)): Cursor.cursor = { c with ~fields }
  set_orderby(c:Cursor.cursor, orderby:option(Bson.document)): Cursor.cursor = { c with ~orderby }

  @private
  set_error(c:Cursor.cursor, error:string): Cursor.cursor = { c with ~error; killed={true} }

  @private
  reply(c:Cursor.cursor, reply_opt:option(reply), name:string, query_sent:bool): Cursor.cursor =
    match reply_opt with
    | {some=reply} ->
      cursorID = Mongo.reply_cursorID(reply)
      { c with
          cid = cursorID;
          reply = {some=reply};
          ~query_sent;
          returned = Mongo.reply_numberReturned(reply);
          current = 0;
          doc = error_document("Uninitialised document",-1);
      }
    | {none} -> set_error(c,"Cursor.{name}: no reply")

  /**
   * Perform an OP_QUERY call to the database server based on the parameters
   * stored in the cursor object.
   *
   * Will only work on valid cursors, ie. those which have not received an error.
   *
   * Note that no tests are performed on the reply, there are other routines which
   * examine the content of the reply.  You may, however, get a comms error here.
   **/
  op_query(c:Cursor.cursor): Cursor.cursor =
    if not(c.killed) && Option.is_some(c.query)
    then
      query = (match c.orderby with
               | {some=orderby} -> [H.doc("$query",Option.get(c.query)), H.doc("$orderby",orderby)]
               | {none} -> Option.get(c.query))
      //do println("op_query: query={Bson.to_pretty(query)}") <-- redundant, we've got logging now
      reply(c,Mongo.query(c.mongo, c.flags, c.ns, c.skip, c.limit, query, c.fields),"op_query",{true})
    else set_error(c,(if c.killed
                      then "Cursor.op_query: already killed"
                      else "Cursor.op_query: no query"))

  /**
   * Perform an OP_GETMORE call, if a valid cursor ID exists in the cursor.
   **/
  get_more(c:Cursor.cursor): Cursor.cursor =
    if not(c.killed) && not(Mongo.is_null_cursorID(c.cid))
    then reply(c,Mongo.get_more(c.mongo, c.ns, c.limit, c.cid),"get_more",c.query_sent)
    else set_error(c,"Cursor.get_more: attempt to get more with dead cursor")

  /**
   * Return the [n]'th document in the reply stored in a cursor.
   *
   * This is a low-level routine, use [Cursor.next] to scan the returned values
   * while sending additional OP_GETMORE calls when necessary.
   */
  document(c:Cursor.cursor, n:int): Mongo.result =
    if n >= c.returned
    then {failure={Error="Cursor.document: document index out of range {n}"}}
    else
      match c.reply with
      | {some=reply} ->
        (match Mongo.reply_document(reply,n) with
         | {some=doc} -> {success=doc}
         | {none} -> {failure={Error="Cursor.document: no document"}})
      | {none} -> {failure={Error="Cursor.document: no reply"}}

  /**
   * Return all the documents in the reply stored in a cursor.
   **/
  all_documents(c:Cursor.cursor): outcome(list(Bson.document), Mongo.failure) =
    match c.reply with
    | {some=reply} ->
      rec aux(n:int) =
       if n >= c.returned
       then []
       else (match Mongo.reply_document(reply,n) with
             | {some=doc} -> (doc +> (aux(n+1)))
             | {none} -> (aux(n+1)))
      {success=aux(0)}
    | {none} -> {failure={Error="Cursor.document: no reply"}}

  @private
  destroy(c:Cursor.cursor): Cursor.cursor =
    do Mongo.close_copy(c.mongo)
    { c with
        error="<reset>";
        doc=error_document("Dead cursor",-1);
        killed={true};
        cid=Mongo.null_cursorID(void)
    }

  /**
   * Destroy a cursor object.
   *
   * Deletes buffer storage and sends a OP_KILL_CURSOR call if a valid cursor
   * ID still exists in the cursor.
   **/
  reset(c:Cursor.cursor): Cursor.cursor =
    if not(Mongo.is_null_cursorID(c.cid))
    then
      if Mongo.kill_cursors(c.mongo, [c.cid])
      then destroy(c)
      else set_error(destroy(c),"Cursor.reset: error killing cursor")
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
  rec next(c:Cursor.cursor): Cursor.cursor =
    c = if not(c.query_sent) then op_query(c) else c
    if Option.is_none(c.reply)
    then set_error(c,"Cursor.next: no reply")
    else
      // TODO: analyze return flags
      // TODO: tailable cursors
      if c.returned <= 0
      then
        tags = Mongo.reply_tags(Mongo.reply_responseFlags(Option.get(c.reply)))
        set_error(c,"Cursor.next: no data returned tags={Mongo.string_of_tags(tags)}")
      else
        if c.current >= c.returned
        then
          if Mongo.is_null_cursorID(c.cid)
          then set_error({c with doc = error_document("Read past end of data",-1)},"Cursor.next: end of data")
          else next(get_more(c))
        else {c with
                current=c.current+1;
                doc=(match Mongo.reply_document(Option.get(c.reply),c.current) with
                     | {some=doc} -> doc
                     | {none} -> error_document("Reply parse error",-1))}


  /**
   * Create and initialise cursor with given query and default options.
   * Intended to form a set of functions to enable the idiom: [for(start(...),(c -> ... next(c)),valid)].
   **/
  start(m:Mongo.db, ns:string, query:Bson.document, limit:int): Cursor.cursor =
    c = Cursor.init(m,ns)
    /* Note: MongoDB seems to interpret limit=1 as "just send me one document".
     * If you want this loop to scan all the documents you can't use limit=1.
     */
    c = Cursor.set_limit(c,(max(2,limit))) // FIXME: destroys order (-2 etc.)
    c = Cursor.set_query(c,{some=query})
    Cursor.next(c)

  /**
   * Test if there is still data in a cursor.
   **/
  valid(c:Cursor.cursor): bool =
    not(c.killed) && c.query_sent && ((c.returned > 0 && (c.current < c.returned)) /*|| not(Mongo.is_null_cursorID(c.cid))*/ )

  /**
   * Full find function with all parameters.
   *
   * Creates a cursor with the given parameters and calls OP_QUERY to
   * initiate communications.
   *
   * The cursor value is then returned, you can then use [Cursor.next] to
   * scan along from there.
   **/
  find(m:Mongo.db, ns:string, query:Bson.document, fields:option(Bson.document), orderby:option(Bson.document),
       limit:int, skip:int, flags:int): outcome(Cursor.cursor,Mongo.failure) =
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
  check_cursor_error(c:Cursor.cursor): Mongo.result =
    if not(c.killed)
    then check_err(c.doc)
    else {failure={Error=c.error}}

  /**
   * Find the first matching document for the given namespace.
   *
   * Creates and destroys a cursor.
   **/
  find_one(m:Mongo.db, ns:string,
           query:Bson.document, fields:option(Bson.document), orderby:option(Bson.document)): Mongo.result =
    c = init(m, ns)
    c = set_query(c, {some=query})
    c = set_fields(c, fields)
    c = set_orderby(c, orderby)
    c = set_limit(c, 1)
    c = next(c)
    outcome = check_cursor_error(c)
    _ = Cursor.reset(c)
    outcome

  check_ok(bson:Bson.document): Mongo.result =
    match Bson.find(bson,"ok") with
    | {some=[{name="ok"; value={Double=ok}}]} ->
       if ok == 1.0
       then {success=bson}
       else
         (match Bson.find(bson,"errmsg") with
          | {some=[{name="errmsg"; value={String=errmsg}}]} -> {failure={Error=errmsg}}
          | _ -> {failure={Error="ok:{ok}"}})
    | _ -> {success=bson}

}}

// End of file cursor.opa
