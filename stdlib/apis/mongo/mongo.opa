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
 * This is a binding for MongoDB for OPA, loosely based around the C drivers.
 *
 * Module [Mongo] has low-level routines to talk to the database server, the only
 * routines you should need are the [Mongo.open] and [Mongo.close] functions.
 *
 * Module [Cursor] has the cursor handling routines but since commands, authentication,
 * etc. are written using cursors they are also in this module.
 *
 * Module [Indexes] has a couple of routines for creating indexes.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

import stdlib.core.{date}
import stdlib.io.socket
import stdlib.crypto
import stdlib.system

type mongo_buf = external
type cursorID = external
type mailbox = external
type reply = external

/**
 * OPA representation of a BSON object.
 *
 * These are called elements in BSON terminology.
 **/
@opacapi
type RPC.Bson.element =
    { Double: (string, float) }
  / { String: (string, string) }
  / { Document: (string, RPC.Bson.document) }
  / { Array: (string, RPC.Bson.document) }
  / { Binary: (string, string) }
  / { ObjectID: (string, string) }
  / { Boolean: (string, bool) }
  / { Date: (string, Date.date) }
  / { Null: (string, void) }
  / { Regexp: (string, (string, string)) }
  / { Code: (string, string) }
  / { Symbol: (string, string) }
  / { CodeScope: (string, (string, RPC.Bson.document)) }
  / { Int32: (string, int) }
  / { Timestamp: (string, (int, int)) }
  / { Int64: (string, int) }

/**
 * The main exported type, a document is just a list of elements.
 */
@opacapi
type RPC.Bson.document = list(RPC.Bson.element)

@abstract type Mongo.db = {
     conn : Socket.connection;
     mbuf : mongo_buf;
     mailbox : mailbox;
     bufsize : int
  }

type Mongo.failure =
    {Error : string}
  / {DocError : RPC.Bson.document}

type Mongo.success = RPC.Bson.document

type Mongo.result = outcome(Mongo.success, Mongo.failure)

@server_private
Bson = {{

  /**
   * Type codes as per BSON spec.
   **/
  tEoo = 0x00
  tDouble = 0x01
  tString = 0x02
  tDocument = 0x03
  tArray = 0x04
  tBinary = 0x05
  tObjectID = 0x07
  tBoolean = 0x08
  tDate = 0x09
  tNull = 0x0a
  tRegexp = 0x0b
  tCode = 0x0d
  tSymbol = 0x0e
  tCodeScope = 0x0f
  tInt32 = 0x10
  tTimestamp = 0x11
  tInt64 = 0x12

  /** Convenience function, dump string as hex and ascii */
  dump = (%% BslMongo.Bson.dump %%: int, string -> string)

  /** Return new Bson Object ID */
  new_oid = (%% BslMongo.Bson.new_oid %%: void -> string)

  /** Get OID from string */
  oid_of_string = (%% BslMongo.Bson.oid_of_string %%: string -> string)

  /** Get string from OID */
  oid_to_string = (%% BslMongo.Bson.oid_to_string %%: string -> string)

  /**
   * Return the type number (BSON) of an element.
   **/
  etype(b0:RPC.Bson.element): int =
    match b0 with
    | {Double=(_, _)} -> tDouble
    | {String=(_, _)} -> tString
    | {Document=(_, _)} -> tDocument
    | {Array=(_, _)} -> tArray
    | {Binary=(_, _)} -> tBinary
    | {ObjectID=(_, _)} -> tObjectID
    | {Boolean=(_, _)} -> tBoolean
    | {Date=(_, _)} -> tDate
    | {Null=(_, _)} -> tNull
    | {Regexp=(_, _)} -> tRegexp
    | {Code=(_, _)} -> tCode
    | {Symbol=(_, _)} -> tSymbol
    | {CodeScope=(_, _)} -> tCodeScope
    | {Int32=(_, _)} -> tInt32
    | {Timestamp=(_, _)} -> tTimestamp
    | {Int64=(_, _)} -> tInt64

  /**
   * Return the key of an element.
   **/
  key(b0:RPC.Bson.element): string =
    match b0 with
    | { Double=(key, _) } -> key
    | { String=(key, _) } -> key
    | { Document=(key, _) } -> key
    | { Array=(key, _) } -> key
    | { Binary=(key, _) } -> key
    | { ObjectID=(key, _) } -> key
    | { Boolean=(key, _) } -> key
    | { Date=(key, _) } -> key
    | { Null=(key, _) } -> key
    | { Regexp=(key, _) } -> key
    | { Code=(key, _) } -> key
    | { Symbol=(key, _) } -> key
    | { CodeScope=(key, _) } -> key
    | { Int32=(key, _) } -> key
    | { Timestamp=(key, _) } -> key
    | { Int64=(key, _) } -> key

  /**
   * Find an element by key in a bson object.
   **/
  find_element(bson:RPC.Bson.document, name:string): option(RPC.Bson.element) =
    List.find((b0 -> key(b0) == name),bson)

  /**
   * Find key of given name in bson object.
   * We only look at the current level, mostly it's for finding
   * "ok" or "errval" etc. in replies.
   **/
  find(bson:RPC.Bson.document, name:string): option(RPC.Bson.document) =
    Option.map((b -> [b]),find_element(bson, name))

  /**
   * Some type-specific versions of [find], search for [key]
   * in [bson] object and return required type, if possible.
   * Note that if the key exists but is of the wrong type
   * then you will still get [\{none\}].
   **/

  find_bool(bson:RPC.Bson.document, name:string): option(bool) =
    match find(bson, name) with
    | {some=[{Boolean=(_,tf)}]} -> {some=tf}
    | _ -> {none}

  find_int(bson:RPC.Bson.document, name:string): option(int) =
    match find(bson, name) with
    | {some=[{Int32=(_,i)}]} -> {some=i}
    | {some=[{Int64=(_,i)}]} -> {some=i}
    | {some=[{Double=(_,d)}]} -> {some=Float.to_int(d)}
    | _ -> {none}

  find_string(bson:RPC.Bson.document, name:string): option(string) =
    match find(bson, name) with
    | {some=[{String=(_,str)}]} -> {some=str}
    | _ -> {none}

  find_doc(bson:RPC.Bson.document, name:string): option(RPC.Bson.document) =
    match find(bson, name) with
    | {some=[{Document=(_,doc)}]} -> {some=doc}
    | _ -> {none}

  /**
   * Return the type of a matching Bson key.
   **/
  find_type(bson:RPC.Bson.document, name:string): option(int) = Option.map(etype,find_element(bson,name))

  /**
   * Return a list of the keys in a bson object.
   **/
  keys(bson:RPC.Bson.document): list(string) = List.map(key, bson)

  /**
   * Iterate over the elements in a bson object.
   **/
  iter(f:(RPC.Bson.element -> void), bson:RPC.Bson.document) : void =
    List.iter((b0 -> f(b0)),bson)

  /**
   * Map over the elements in a bson object.
   **/
  map(f:(RPC.Bson.element -> RPC.Bson.element), bson:RPC.Bson.document) : RPC.Bson.document =
    List.map((b0 -> f(b0)),bson)

  /**
   * Fold over the elements in a bson object.
   **/
  fold(f:(RPC.Bson.element, 'a -> 'a), bson:RPC.Bson.document, acc:'a) : 'a =
    List.fold((b0, acc -> f(b0, acc)),bson,acc)

  /**
   * Attempt to turn a bson element into a string which looks like
   * the mongo shell syntax.
   * Note: still only partial.
   **/
  rec string_of_element(element:RPC.Bson.element): string =
    match element with
    | {Double=(k, v)} -> "\{ \"{k}\" : Double {v} \}"
    | {String=(k, v)} -> "\{ \"{k}\" : String {v} \}"
    | {Document=(k, v)} -> "\{ \"{k}\" : Document {string_of_bson(v)} \}"
    | {Array=(k, v)} -> "\{ \"{k}\" : Array {string_of_bson(v)} \}"
    | {Binary=(k, v)} -> "\{ \"{k}\" : Binary {v} \}"
    | {ObjectID=(k, v)} -> "\{ \"{k}\" : ObjectID {oid_to_string(v)} \}"
    | {Boolean=(k, v)} -> "\{ \"{k}\" : Boolean {v} \}"
    | {Date=(k, v)} -> "\{ \"{k}\" : Date {v} \}"
    | {Null=(k, v)} -> "\{ \"{k}\" : Null {v} \}"
    | {Regexp=(k, v)} -> "\{ \"{k}\" : Regexp {v} \}"
    | {Code=(k, v)} -> "\{ \"{k}\" : Code {v} \}"
    | {Symbol=(k, v)} -> "\{ \"{k}\" : Symbol {v} \}"
    | {CodeScope=(k, v)} -> "\{ \"{k}\" : CodeScope {v} \}"
    | {Int32=(k, v)} -> "\{ \"{k}\" : Int32 {v} \}"
    | {Timestamp=(k, (t,i))} -> "\{ \"{k}\" : Timestamp \{ \"t\" : {t}, \"i\" : {i} \}\}"
    | {Int64=(k, v)} -> "\{ \"{k}\" : Int64 {v} \}"

  /**
   * Same for a bson object (just a list of elements).
   **/
  string_of_bson(bson:RPC.Bson.document): string = "\{ "^(String.concat(", ",List.map(string_of_element,bson)))^" \}"

}}

@server_private
Mongo = {{

  /* Flags */

  /* OP_INSERT */
  _ContinueOnError  = 0x00000001

  /* OP_UPDATE */
  _Upsert           = 0x00000001
  _MultiUpdate      = 0x00000002

  /* OP_QUERY */
  _TailableCursor   = 0x00000002
  _SlaveOk          = 0x00000004
  _OplogReplay      = 0x00000008
  _NoCursorTimeout  = 0x00000010
  _AwaitData        = 0x00000020
  _Exhaust          = 0x00000040
  _Partial          = 0x00000080

  /* OP_DELETE */
  _SingleRemove     = 0x00000001

  /* OP_REPLY */
  _CursorNotFound   = 0x00000001
  _QueryFailure     = 0x00000002
  _ShardConfigStale = 0x00000003
  _AwaitCapable     = 0x00000004

  /** Allocate new buffer of given size **/
  @private create_ = (%% BslMongo.Mongo.create %%: int -> mongo_buf)

  /** Build OP_INSERT message in buffer **/
  @private insert_ = (%% BslMongo.Mongo.insert %%: mongo_buf, int, string, 'a -> void)

  /** Build OP_INSERT message in buffer **/
  @private insert_batch_ = (%% BslMongo.Mongo.insert_batch %%: mongo_buf, int, string, list('a) -> void)

  /** Build OP_UPDATE message in buffer **/
  @private update_ = (%% BslMongo.Mongo.update %%: mongo_buf, int, string, 'a, 'a -> void)

  /** Build OP_QUERY message in buffer **/
  @private query_ = (%% BslMongo.Mongo.query %%: mongo_buf, int, string, int, int, 'a, option('a) -> void)

  /** Build OP_GET_MORE message in buffer **/
  @private get_more_ = (%% BslMongo.Mongo.get_more %%: mongo_buf, string, int, cursorID -> void)

  /** Build OP_DELETE message in buffer **/
  @private delete_ = (%% BslMongo.Mongo.delete %%: mongo_buf, int, string, 'a -> void)

  /** Build OP_KILL_CURSORS message in buffer **/
  @private kill_cursors_ = (%% BslMongo.Mongo.kill_cursors %%: mongo_buf, list('a) -> void)

  /** Build OP_MSG message in buffer **/
  @private msg_ = (%% BslMongo.Mongo.msg %%: mongo_buf, string -> void)

  /** Copies string out of buffer. **/
  @private get_ = (%% BslMongo.Mongo.get %%: mongo_buf -> string)

  /** Access the raw string and length **/
  @private export_ = (%% BslMongo.Mongo.export %%: mongo_buf -> (string, int))

  /** Clear out any data in the buffer, leave buffer allocated **/
  @private clear_ = (%% BslMongo.Mongo.clear %%: mongo_buf -> void)

  /** Reset the buffer, unallocate storage **/
  @private reset_ = (%% BslMongo.Mongo.reset %%: mongo_buf -> void)

  /** Mailbox so we can use the streaming parser **/
  @private new_mailbox_ = (%% BslMongo.Mongo.new_mailbox %%: int -> mailbox)
  @private reset_mailbox_ = (%% BslMongo.Mongo.reset_mailbox %%: mailbox -> void)

  /**
   * Specialised read, read until the size equals the (little endian)
   * 4-byte int at the start of the reply.
   **/
  @private read_mongo_ = (%% BslMongo.Mongo.read_mongo %%: Socket.connection, mailbox -> reply)

  @private
  send_no_reply(m,_name): bool =
    match export_(m.mbuf) with
    | (str, len) ->
      s = String.substring(0,len,str)
      //do println("{_name}: s=\n{dump(10,s)}")
      cnt = Socket.write_len(m.conn,s,len)
      //do println("cnt={cnt} len={len}")
      (cnt==len)

  @private
  send_with_reply(m,_name): option(reply) =
    match export_(m.mbuf) with
    | (str, len) ->
      s = String.substring(0,len,str)
      //do println("{_name}: s=\n{dump(10,s)}")
      cnt = Socket.write_len(m.conn,s,len)
      //do println("cnt={cnt} len={len}")
      if (cnt==len)
      then {some=read_mongo_(m.conn,m.mailbox)}
      else {none}

  /**
   *  Create new mongo object:
   *    - Open connection to mongo server at addr:port
   *    - Allocate buffer of given size
   *    - Primitive error handling in case of mongo server malfunction
   **/
  open(bufsize:int, addr:string, port:int): Mongo.db =
    err_cont = Continuation.make((s:string ->
                                   do prerrln("Mongo.open: exn={s}")
                                   System.exit(-1)))
    {
      conn = Socket.connect_with_err_cont(addr,port,err_cont);
      mbuf = create_(bufsize);
      mailbox = new_mailbox_(bufsize);
      ~bufsize
    }

  /**
   * We are only concurrent-safe within a connection.
   * We need this to create a new buffer for each cursor.
   **/
  copy(m:Mongo.db): Mongo.db =
    { conn=m.conn;
      mbuf = create_(m.bufsize);
      mailbox = new_mailbox_(m.bufsize);
      bufsize = m.bufsize
    }

  /**
   *  Send OP_INSERT with given collection name:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  insert(m:Mongo.db, flags:int, ns:string, documents:RPC.Bson.document): bool =
    do insert_(m.mbuf,flags,ns,documents)
    send_no_reply(m,"insert")

  /**
   *  Send OP_INSERT with given collection name and multiple documents:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  insert_batch(m:Mongo.db, flags:int, ns:string, documents:list(RPC.Bson.document)): bool =
    do insert_batch_(m.mbuf,flags,ns,documents)
    send_no_reply(m,"insert")

  /**
   *  Send OP_UPDATE with given collection name:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  update(m:Mongo.db, flags:int, ns:string, selector:RPC.Bson.document, update:RPC.Bson.document): bool =
    do update_(m.mbuf,flags,ns,selector,update)
    send_no_reply(m,"update")

  /**
   *  Send OP_QUERY and get reply:
   **/
  query(m:Mongo.db, flags:int, ns:string, numberToSkip:int, numberToReturn:int,
        query:RPC.Bson.document, returnFieldSelector_opt:option(RPC.Bson.document)): option(reply) =
    do query_(m.mbuf,flags,ns,numberToSkip,numberToReturn,query,returnFieldSelector_opt)
    send_with_reply(m,"query")

  /**
   *  Send OP_GETMORE and get reply:
   **/
  get_more(m:Mongo.db, ns:string, numberToReturn:int, cursorID:cursorID): option(reply) =
    do get_more_(m.mbuf,ns,numberToReturn,cursorID)
    send_with_reply(m,"getmore")

  /**
   *  Send OP_DELETE:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  delete(m:Mongo.db, flags:int, ns:string, selector:RPC.Bson.document): bool =
    do delete_(m.mbuf,flags,ns,selector)
    send_no_reply(m,"delete")

  /**
   *  Send OP_KILL_CURSORS:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  kill_cursors(m:Mongo.db, cursors:list(cursorID)): bool =
    do kill_cursors_(m.mbuf,cursors)
    send_no_reply(m,"kill_cursors")

  /**
   *  Send OP_MSG:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  msg(m:Mongo.db, msg:string): bool =
    do msg_(m.mbuf,msg)
    send_no_reply(m,"msg")

  /**
   *  Close mongo connection and deallocate buffer.
   **/
  close(m:Mongo.db) =
    do Socket.close(m.conn)
    do reset_(m.mbuf)
    do reset_mailbox_(m.mailbox)
    void

  /**
   *  Close mongo copy, deallocate buffer but leave connection open.
   **/
  close_copy(m:Mongo.db) =
    do reset_(m.mbuf)
    do reset_mailbox_(m.mailbox)
    void

  /** Access components of the reply value **/
  reply_messageLength = (%% BslMongo.Mongo.reply_messageLength %% : reply -> int)
  reply_requestId = (%% BslMongo.Mongo.reply_requestId %% : reply -> int)
  reply_responseTo = (%% BslMongo.Mongo.reply_responseTo %% : reply -> int)
  reply_opCode = (%% BslMongo.Mongo.reply_opCode %% : reply -> int)
  reply_responseFlags = (%% BslMongo.Mongo.reply_responseFlags %% : reply -> int)
  reply_cursorID = (%% BslMongo.Mongo.reply_cursorID %% : reply -> cursorID)
  reply_startingFrom = (%% BslMongo.Mongo.reply_startingFrom %% : reply -> int)
  reply_numberReturned = (%% BslMongo.Mongo.reply_numberReturned %% : reply -> int)

  /** Return the n'th document attached to the reply **/
  reply_document = (%% BslMongo.Mongo.reply_document %% : reply, int -> option(RPC.Bson.document))

  /** Debug routine, export the internal representation of the reply **/
  export_reply = (%% BslMongo.Mongo.export_reply %%: reply -> string)

  /** Null cursor value **/
  null_cursorID = (%% BslMongo.Mongo.null_cursorID %% : void -> cursorID)

  /** Return a string representation of a cursor (it's an int64) **/
  string_of_cursorID = (%% BslMongo.Mongo.string_of_cursorID %% : cursorID -> string)

  /** Predicate for end of query, when the cursorID is returned as zero **/
  is_null_cursorID = (%% BslMongo.Mongo.is_null_cursorID %% : cursorID -> bool)

}}

/**
 * type cursor:
 *   - Contains all the parameters for an op_query call.
 *   - Stores the reply.
 *   - Handles indexes into the list of documents returned and
 *     keeps a note of the last document parsed.
 *   - Also handles the cursor ID.
 *   - Use Cursor.reset when not needed, this will generate a
 *     kill_cursors call to the server to clean up.
 **/
type cursor = {
     mongo : Mongo.db;
     ns : string;
     flags : int;
     skip : int;
     limit : int;
     query : option(RPC.Bson.document);
     fields : option(RPC.Bson.document);
     query_sent : bool;
     cid : cursorID;
     reply : option(reply);
     returned : int;
     current : int;
     doc : RPC.Bson.document;
     killed : bool;
     error : string
}

@server_private
Cursor = {{

  @private
  error_document(err:string, code:int) = [{String=("$err",err)}, {Int32=("code",code)}]

  /**
   * Bare cursor initialize.
   *
   *   {b Warning:} Note that each time you create a cursor it generates buffers to talk to
   *   the MongoDB server.  Remember to cleanup cursor objects with [Cursor.reset].
   **/
  init(mongo:Mongo.db, ns:string): cursor =
  { mongo = Mongo.copy(mongo);
    ~ns;
    flags = 0;
    skip = 0;
    limit = 1;
    query = {none};
    fields = {none};
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
  set_flags(c:cursor, flags:int): cursor = { c with ~flags }
  set_skip(c:cursor, skip:int): cursor = { c with ~skip }
  set_limit(c:cursor, limit:int): cursor = { c with ~limit }
  set_query(c:cursor, query:option(RPC.Bson.document)): cursor = { c with ~query }
  set_fields(c:cursor, fields:option(RPC.Bson.document)): cursor = { c with ~fields }

  @private
  set_error(c:cursor, error:string): cursor = { c with ~error; killed={true} }

  @private
  reply(c:cursor, reply_opt:option(reply), name:string, query_sent:bool): cursor =
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
  op_query(c:cursor): cursor =
    if not(c.killed) && Option.is_some(c.query)
    then reply(c,Mongo.query(c.mongo, c.flags, c.ns, c.skip, c.limit, Option.get(c.query), c.fields),"op_query",{true})
    else set_error(c,(if c.killed
                      then "Cursor.op_query: already killed"
                      else "Cursor.op_query: no query"))

  /**
   * Perform an OP_GETMORE call, if a valid cursor ID exists in the cursor.
   **/
  get_more(c:cursor): cursor =
    if not(c.killed) && not(Mongo.is_null_cursorID(c.cid))
    then reply(c,Mongo.get_more(c.mongo, c.ns, c.limit, c.cid),"get_more",c.query_sent)
    else set_error(c,"Cursor.get_more: attempt to get more with dead cursor")

  /**
   * Return the [n]'th document in the reply stored in a cursor.
   *
   * This is a low-level routine, use [Cursor.next] to scan the returned values
   * while sending additional OP_GETMORE calls when necessary.
   */
  document(c:cursor, n:int): Mongo.result =
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
  all_documents(c:cursor): outcome(list(RPC.Bson.document), Mongo.failure) =
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
  destroy(c:cursor): cursor =
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
  reset(c:cursor): cursor =
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
  rec next(c:cursor): cursor =
    c = if not(c.query_sent) then op_query(c) else c
    if Option.is_none(c.reply)
    then set_error(c,"Cursor.next: no reply")
    else
      // TODO: analyze return flags
      // TODO: tailable cursors
      if c.returned <= 0
      then set_error(c,"Cursor.next: no data returned")
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
  start(m:Mongo.db, ns:string, query:RPC.Bson.document): cursor =
    c = Cursor.init(m,ns)
    c = Cursor.set_query(c,{some=query})
    Cursor.next(c)

  /**
   * Test if there is still data in a cursor.
   **/
  valid(c:cursor): bool = not(c.killed) && c.query_sent && (c.returned > 0 && (c.current <= c.returned))

  /**
   * Full find function with all parameters.
   *
   * Creates a cursor with the given parameters and calls OP_QUERY to
   * initiate communications.
   *
   * The cursor value is then returned, you can then use [Cursor.next] to
   * scan along form there.
   **/
  find(m:Mongo.db, ns:string, query:RPC.Bson.document, fields:option(RPC.Bson.document),
       limit:int, skip:int, flags:int): outcome(cursor,Mongo.failure) =
    c = init(m, ns)
    c = set_query(c, {some=query})
    c = set_fields(c, fields)
    c = set_limit(c, limit)
    c = set_skip(c, skip)
    c = set_flags(c, flags)
    c = op_query(c)
    if c.killed
    then {failure={Error="find: query error"}}
    else {success=c}

  @private
  check_err(b:RPC.Bson.document): Mongo.result =
    match Bson.find(b,"$err") with
    | {some=err_doc} -> {failure={DocError=err_doc}}
    | {none} -> {success=b}

  /**
   * If a cursor is valid then return an [outcome] with the current
   * document.  Will return a [failure] document if "$err" exists in
   * the document.
   **/
  check_cursor_error(c:cursor): Mongo.result =
    if not(c.killed)
    then check_err(c.doc)
    else {failure={Error=c.error}}

  /**
   * Find the first matching document for the given namespace.
   *
   * Creates and destroys a cursor.
   **/
  find_one(m:Mongo.db, ns:string, query:RPC.Bson.document, fields:option(RPC.Bson.document)): Mongo.result =
    c = init(m, ns)
    c = set_query(c, {some=query})
    c = set_fields(c, fields)
    c = set_limit(c, 1)
    c = next(c)
    outcome = check_cursor_error(c)
    _ = Cursor.reset(c)
    outcome

  @private
  check_ok(bson:RPC.Bson.document): Mongo.result =
    match Bson.find(bson,"ok") with
    | {some=[{Double=("ok",ok)}]} ->
       if ok == 1.0
       then {success=bson}
       else
         (match Bson.find(bson,"errmsg") with
          | {some=[{String=("errmsg",errmsg)}]} -> {failure={Error=errmsg}}
          | _ -> {failure={Error="ok:{ok}"}})
    | _ -> {success=bson}

  /**
   * Run a "$cmd" command.
   *
   * Normally you will get {ok: 0/1} as a reply but sometimes there
   * are other elements in the reply.
   **/
  run_command(m:Mongo.db, ns:string, command:RPC.Bson.document): Mongo.result =
    match find_one(m, ns^".$cmd", command, {none}) with
    | {success=bson} -> check_ok(bson)
    | {~failure} -> {~failure}

  /**
   * Perform a simple integer command, eg. [{ ping : 1 }]
   **/
  simple_int_command(m:Mongo.db, ns:string, cmd:string, arg:int): Mongo.result =
    run_command(m, ns, [{Int32=(cmd,arg)}])

  /**
   * Perform a simple integer command, eg. [{ drop : "collection" }]
   **/
  simple_str_command(m:Mongo.db, ns:string, cmd:string, arg:string): Mongo.result =
    run_command(m, ns, [{String=(cmd,arg)}])

  /**
   * Predicate for connection alive.  Peforms an admin "ping" command.
   **/
  check_connection(m:Mongo.db): outcome(bool,Mongo.failure) =
    match simple_int_command(m, "admin", "ping", 1) with
    | {success=_} -> {success=true}
    | {~failure} -> {~failure}

  /**
   * Drop a database
   **/
  drop_db(m:Mongo.db, db:string): Mongo.result =
    simple_int_command(m, db, "dropDatabase", 1)

  /**
   * Drop a collection from a database [drop_collection("db","collection")]
   **/
  drop_collection(m:Mongo.db, db:string, collection:string): Mongo.result =
    simple_str_command(m, db, "drop", collection)

  /**
   * Call a "reseterror" command.
   **/
  reset_error(m:Mongo.db, db:string): Mongo.result =
    simple_int_command(m, db, "reseterror", 1)

  @private
  find_int(bson:RPC.Bson.document, name:string): outcome(int,Mongo.failure) =
    match Bson.find(bson,name) with
    | {some=[{Int32=(_,n)}]} -> {success=n}
    | {some=[{Int64=(_,n)}]} -> {success=n}
    | {some=[{Double=(_,n)}]} -> {success=Float.to_int(n)}
    | _ -> {failure={Error="Missing {name} Int32/Int64/Double"}}

  /**
   * Count the number of matching elements.
   *
   * [count(mongo, "db", "ns", query_opt)] returns the number of elements
   * matching [query] or the whole collection if [query_opt] is [\{none\}].
   *
   * Strictly speaking MongoDB returns an Int64 value but it is unlikely that
   * a database will be able to overrun the OCaml restriction, so the value is
   * just an int.
   **/
  count(m:Mongo.db, db:string, ns:string, query_opt:option(RPC.Bson.document)): outcome(int,Mongo.failure) =
    cmd = List.flatten([[{String=("count",ns)}],
                        (match query_opt with | {some=query} -> [{Document=("query",query)}] | {none} -> [])])
    match run_command(m, db, cmd) with
    | {success=bson} -> find_int(bson, "n")
    | {~failure} -> {~failure}

  /**
   * Predicate for master status.
   **/
  ismaster(m:Mongo.db): outcome(bool,Mongo.failure) =
    match simple_int_command(m, "admin", "ismaster", 1) with
    | {success=bson} ->
      (match Bson.find(bson,"ismaster") with
       | {some=[{Boolean=("ismaster",ismaster)}]} -> {success=ismaster}
       | _ -> {failure={Error="Missing ismaster Boolean"}})
    | {~failure} -> {~failure}

  @private pass_digest(user:string, pass:string): string = Crypto.Hash.md5("{user}:mongo:{pass}")

  /**
   * Authentication: [add_user(mongo, "db", "user", "pass")] creates a
   * user for the given db.
   **/
  add_user(m:Mongo.db, db:string, user:string, pass:string): bool =
    digest = pass_digest(user,pass)
    bselector = [{String=("user",user)}]
    bupdate = [{Document=("$set",[{String=("pwd",digest)}])}]
    Mongo.update(m,Mongo._Upsert,(db^".system.users"),bselector,bupdate)

  /**
   * Authenticate a user for the given database.
   *
   * The password must match the users password.
   **/
  authenticate(m:Mongo.db, db:string, user:string, pass:string): Mongo.result =
    match simple_int_command(m, db, "getnonce", 1) with
    | {success=bson} ->
      (match Bson.find(bson,"nonce") with
       | {some=[{String=("nonce",nonce)}]} ->
         digest = pass_digest(user,pass)
         hash = Crypto.Hash.md5("{nonce}{user}{digest}")
         cmd = [{Int32=("authenticate",1)}, {String=("user",user)}, {String=("nonce",nonce)}, {String=("key",hash)}]
         run_command(m,db,cmd)
       | _ -> {failure={Error="Missing nonce String"}})
    | {~failure} -> {~failure}

}}

@server_private
Indexes = {{

  /**
   * Flags used by the index routines.
   **/
  _Unique = 0x00000001
  _DropDups = 0x00000002
  _Background = 0x00000004
  _Sparse = 0x00000008

  /**
   * [create_index(mongo, "ns", key, flags)] adds an index to a collection.
   *
   * [key] is a bson object defining the fields to be indexed, eg. [\[\{Int32=("age",1)\}, \{Int32=("name",1)\}\]]
   **/
  create_index(m:Mongo.db, ns:string, key:RPC.Bson.document, options:int): bool =
    keys = Bson.keys(key)
    name = "_"^(String.concat("",keys))
    b = List.flatten([[{Document=("key",key)}, {String=("ns",ns)}, {String=("name",name)}],
                      (if Bitwise.land(options,_Unique) != 0 then [{Boolean=("unique",true)}] else []),
                      (if Bitwise.land(options,_DropDups) != 0 then [{Boolean=("dropDups",true)}] else []),
                      (if Bitwise.land(options,_Background) != 0 then [{Boolean=("background",true)}] else []),
                      (if Bitwise.land(options,_Sparse) != 0 then [{Boolean=("sparse",true)}] else [])])
    idxns=(match String.index(".",ns) with | {some=p} -> String.substring(0,p,ns) | {none} -> ns)^".system.indexes"
    Mongo.insert(m,0,idxns,b)

  /**
   * Simpler version of the [create_index] function, for a single named field.
   **/
  create_simple_index(m:Mongo.db, ns:string, field:string, options:int): bool =
    create_index(m, ns, [{Int32=(field,1)}], options)

}}

  // TODO: replica sets
  // TODO: backups


/* Test code */
/*
_ =
  mongo = Mongo.open(1024,"www.localhost.local",27017)
  //b = [{ObjectID=("_id",Mongo.oid_of_string("333333333333333333333333"))}, {String=("name","Joe1")}, {Int32=("age",44)}]
  //_success = Mongo.insert(mongo,0,"tutorial.people",b)
  //b = [{ObjectID=("_id",Mongo.oid_of_string("343434343434343434343434"))}, {String=("name","Joe2")}, {Int32=("age",55)}]
  //_success = Mongo.insert(mongo,0,"tutorial.people",b)
  do println("mongo.opa")
  // Check cursor concurrency
  cursor = Cursor.init(mongo,"tutorial.people")
  cursor1 = Cursor.init(mongo,"tutorial.people")
  cursor = Cursor.set_query(cursor,{some=[{String=("name","Joe")}]})
  cursor1 = Cursor.set_query(cursor1,{some=[{Int32=("age",55)}]})
  cursor = Cursor.set_limit(cursor,3)
  cursor1 = Cursor.set_limit(cursor1,3)
  cursor = Cursor.op_query(cursor)
  cursor1 = Cursor.op_query(cursor1)
  do println("cursor.cid={Mongo.string_of_cursorID(cursor.cid)}")
  do println("cursor1.cid={Mongo.string_of_cursorID(cursor1.cid)}")
  do println("error=\"{cursor.error}\" returned={cursor.returned}")
  do println("error=\"{cursor1.error}\" returned={cursor1.returned}")
  doc0 = Cursor.document(cursor, 0)
  doc01 = Cursor.document(cursor1, 0)
  do println("doc0={doc0}")
  do println("doc01={doc01}")
  docs = Cursor.all_documents(cursor)
  do println("docs={docs}")
  cursor = Cursor.next(cursor)
  do println("0) error=\"{cursor.error}\" doc={cursor.doc}")
  cursor = Cursor.next(cursor)
  do println("1) error=\"{cursor.error}\" doc={cursor.doc}")
  cursor = Cursor.next(cursor)
  do println("2) error=\"{cursor.error}\" doc={cursor.doc}")
  cursor = Cursor.next(cursor)
  do println("3) error=\"{cursor.error}\" doc={cursor.doc}")
  cursor = Cursor.next(cursor)
  do println("4) error=\"{cursor.error}\" doc={cursor.doc}")
  cursor = Cursor.next(cursor)
  do println("5) error=\"{cursor.error}\" doc={cursor.doc}")
  cursor = Cursor.reset(cursor)
  cursor1 = Cursor.reset(cursor1)
  do println("error=\"{cursor.error}\"")
  do println("error1=\"{cursor1.error}\"")
  doc1 = Cursor.find_one(mongo,"tutorial.people",[{Int32=("age",44)}],{none})
  do println("doc1={doc1}")
  res = Cursor.run_command(mongo, "test", [{String=("create","cursors")}, {Boolean=("capped",true)}, {Int32=("size",1000000)}])
  do println("res={res}")
  b = [{ObjectID=("_id",Bson.oid_of_string("353535353535353535353535"))}, {Int32=("a",0)}]
  _success = Mongo.insert(mongo,0,"test.cursors",b)
  b = [{ObjectID=("_id",Bson.oid_of_string("363636363636363636363636"))}, {Int32=("a",1)}]
  _success = Mongo.insert(mongo,0,"test.cursors",b)
  res = Cursor.count(mongo,"test","cursors",{none})
  do println("count={res}")
  res = Cursor.count(mongo,"test","cursors",{some=[{Document=("a",[{Int32=("$gt",0)}])}]})
  do println("count={res}")
  ismaster = Cursor.ismaster(mongo)
  do println("ismaster={ismaster}")
  success = Cursor.add_user(mongo,"test","norman","abc123")
  do println("success={success}")
  res = Cursor.authenticate(mongo,"test","norman","abc123")
  do println("authenticate={res}")
  drop = Cursor.drop_collection(mongo,"test","cursors")
  do println("drop={drop}")
  res = Cursor.check_connection(mongo)
  do println("check_connection={res}")
  key = [{Int32=("age",1)}, {Int32=("name",1)}]
  success = Indexes.create_index(mongo,"tutorial.people",key,0)
  do println("create_index={success}")
  doc = Cursor.find_one(mongo,"tutorial.system.indexes",[{Document=("key",key)}],{none})
  do println("doc={doc}")
  key = [{Int32=("fog",1)}]
  success = Indexes.create_index(mongo,"test.bar",key,(Indexes._Sparse+Indexes._Unique))
  do println("create_index={success}")
  doc = Cursor.find_one(mongo,"test.system.indexes",[{Document=("key",key)}],{none})
  do println("doc={doc}")
  //do println("cursor={cursor}")
  do Mongo.close(mongo)
  void
*/

