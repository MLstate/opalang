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
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

/* Major TODOs, there are minor ones elsewhere. */
// TODO: replica sets
// TODO: backups

import stdlib.core.{date}
import stdlib.io.socket
import stdlib.crypto
import stdlib.system

/** Some external types **/

type mongo_buf = external
type cursorID = external
type mailbox = external
type reply = external

@abstract type Mongo.db = {
  conn : Socket.connection;
  mbuf : mongo_buf;
  mailbox : mailbox;
  bufsize : int
}

type Mongo.failure =
    {Error : string}
  / {DocError : Bson.document}

type Mongo.success = Bson.document

type Mongo.result = outcome(Mongo.success, Mongo.failure)

/* Flag tags */

/* OP_INSERT */
type insert_tag =
  {ContinueOnError}

/* OP_UPDATE */
type update_tag =
  {Upsert} /
  {MultiUpdate}

/* OP_QUERY */
type query_tag =
  {TailableCursor} /
  {SlaveOk} /
  {OplogReplay} /
  {NoCursorTimeout} /
  {AwaitData} /
  {Exhaust} /
  {Partial}

/* OP_DELETE */
type delete_tag =
  {SingleRemove}

/* OP_REPLY */
type reply_tag =
  {CursorNotFound} /
  {QueryFailure} /
  {ShardConfigStale} /
  {AwaitCapable}

/**
 *  We wrap the tags so that we can tell if it is an insert tag,
 *  query tag etc.  We don't want to send SingleRemove to an update.
 **/
type mongo_tag =
  {itag:insert_tag} /
  {utag:update_tag} /
  {qtag:query_tag} /
  {dtag:delete_tag} /
  {rtag:reply_tag}

/* Tags for indices */
type index_tag =
  {Unique} /
  {DropDups} /
  {Background} /
  {Sparse}

@server_private
Mongo = {{

  /**
   * Some routines for manipulating outcomes from Mongo commands.
   **/

  string_of_failure(failure:Mongo.failure): string =
    match failure with
    | {Error=str} -> str
    | {DocError=doc} -> Bson.string_of_doc(doc)

  string_of_result(result:Mongo.result): string =
    match result with
    | {success=doc} -> Bson.string_of_doc(doc)
    | {~failure} -> string_of_failure(failure)

  pretty_of_result(result:Mongo.result): string =
    match result with
    | {success=doc} -> Bson.to_pretty(doc)
    | {~failure} -> "\{failure={string_of_failure(failure)}\}"

  /**
   * outcome-wrapped versions of find_xxx etc.
   **/
  result_(result:Mongo.result,key:string,find:(Bson.document, string -> option('a))): option('a) =
    match result with
    | {success=doc} -> find(doc,key)
    | {failure=_} -> {none}

  result_bool(result:Mongo.result,key:string): option(bool) = result_(result, key, Bson.find_bool)
  result_int(result:Mongo.result,key:string): option(int) = result_(result, key, Bson.find_int)
  result_float(result:Mongo.result,key:string): option(float) = result_(result, key, Bson.find_float)
  result_string(result:Mongo.result,key:string): option(string) = result_(result, key, Bson.find_string)
  result_doc(result:Mongo.result,key:string): option(Bson.document) = result_(result, key, Bson.find_doc)

  /**
   * Same as outcome-wrapped versions but allowing dot notation.
   **/
  dotresult_(result:Mongo.result,key:string,find:(Bson.document, string -> option('a))): option('a) =
    match result with
    | {success=doc} -> Bson.find_dot(doc,key,find)
    | {failure=_} -> {none}

  dotresult_bool(result:Mongo.result,key:string): option(bool) = dotresult_(result, key, Bson.find_bool)
  dotresult_int(result:Mongo.result,key:string): option(int) = dotresult_(result, key, Bson.find_int)
  dotresult_float(result:Mongo.result,key:string): option(float) = dotresult_(result, key, Bson.find_float)
  dotresult_string(result:Mongo.result,key:string): option(string) = dotresult_(result, key, Bson.find_string)
  dotresult_doc(result:Mongo.result,key:string): option(Bson.document) = dotresult_(result, key, Bson.find_doc)

  result_to_opa(result:Mongo.result): option('a) =
    match result with
    | {success=doc} -> (Bson.doc2opa(doc):option('a))
    | {failure=_} -> {none}

  /* Flags */

  /* OP_INSERT */
  ContinueOnErrorBit  = 0x00000001

  /* OP_UPDATE */
  UpsertBit           = 0x00000001
  MultiUpdateBit      = 0x00000002

  /* OP_QUERY */
  TailableCursorBit   = 0x00000002
  SlaveOkBit          = 0x00000004
  OplogReplayBit      = 0x00000008
  NoCursorTimeoutBit  = 0x00000010
  AwaitDataBit        = 0x00000020
  ExhaustBit          = 0x00000040
  PartialBit          = 0x00000080

  /* OP_DELETE */
  SingleRemoveBit     = 0x00000001

  /* OP_REPLY */
  CursorNotFoundBit   = 0x00000001
  QueryFailureBit     = 0x00000002
  ShardConfigStaleBit = 0x00000004
  AwaitCapableBit     = 0x00000008

  /**
   *  flag_of_tag:  Turn a list of tags into a bit-wise flag suitable
   *  for sending to MongoDB.  We have an extra layer of types to allow
   *  forcing of tags to belong to a particular operation.
   **/
  flag_of_tag(tag:mongo_tag): int =
    match tag with
      /* OP_INSERT */
    | {itag={ContinueOnError}} -> ContinueOnErrorBit

      /* OP_UPDATE */
    | {utag={Upsert}} -> UpsertBit
    | {utag={MultiUpdate}} -> MultiUpdateBit

      /* OP_QUERY */
    | {qtag={TailableCursor}} -> TailableCursorBit
    | {qtag={SlaveOk}} -> SlaveOkBit
    | {qtag={OplogReplay}} -> OplogReplayBit
    | {qtag={NoCursorTimeout}} -> NoCursorTimeoutBit
    | {qtag={AwaitData}} -> AwaitDataBit
    | {qtag={Exhaust}} -> ExhaustBit
    | {qtag={Partial}} -> PartialBit

      /* OP_DELETE */
    | {dtag={SingleRemove}} -> SingleRemoveBit

      /* OP_REPLY */
    | {rtag={CursorNotFound}} -> CursorNotFoundBit
    | {rtag={QueryFailure}} -> QueryFailureBit
    | {rtag={ShardConfigStale}} -> ShardConfigStaleBit
    | {rtag={AwaitCapable}} -> AwaitCapableBit

  flags(tags:list(mongo_tag)): int =
    List.fold_left((flag, tag -> Bitwise.land(flag,flag_of_tag(tag))),0,tags)

  /**
   *  Extract the tags from a given bit-wise flag.  These are specific
   *  to each operation, you need to know which operation the flag was for/from
   *  before you can give meaning to the bits.
   **/
  insert_tags(flag:int): list(mongo_tag) =
    if Bitwise.lor(flag,ContinueOnErrorBit) != 0 then [{itag={ContinueOnError}}] else []

  update_tags(flag:int): list(mongo_tag) =
    tags = if Bitwise.lor(flag,UpsertBit) != 0 then [{utag={Upsert}}] else []
    if Bitwise.lor(flag,MultiUpdateBit) != 0 then [{utag={MultiUpdate}}|tags] else tags

  query_tags(flag:int): list(mongo_tag) =
    tags = if Bitwise.lor(flag,TailableCursorBit) != 0 then [{qtag={TailableCursor}}] else []
    tags = if Bitwise.lor(flag,SlaveOkBit) != 0 then [{qtag={SlaveOk}}|tags] else tags
    tags = if Bitwise.lor(flag,OplogReplayBit) != 0 then [{qtag={OplogReplay}}|tags] else tags
    tags = if Bitwise.lor(flag,NoCursorTimeoutBit) != 0 then [{qtag={NoCursorTimeout}}|tags] else tags
    tags = if Bitwise.lor(flag,AwaitDataBit) != 0 then [{qtag={AwaitData}}|tags] else tags
    tags = if Bitwise.lor(flag,ExhaustBit) != 0 then [{qtag={Exhaust}}|tags] else tags
    if Bitwise.lor(flag,PartialBit) != 0 then [{qtag={Partial}}|tags] else tags

  delete_tags(flag:int): list(mongo_tag) =
    if Bitwise.lor(flag,SingleRemoveBit) != 0 then [{dtag={SingleRemove}}] else []

  reply_tags(flag:int): list(mongo_tag) =
    tags = if Bitwise.lor(flag,CursorNotFoundBit) != 0 then [{rtag={CursorNotFound}}] else []
    tags = if Bitwise.lor(flag,QueryFailureBit) != 0 then [{rtag={QueryFailure}}|tags] else tags
    tags = if Bitwise.lor(flag,ShardConfigStaleBit) != 0 then [{rtag={ShardConfigStale}}|tags] else tags
    if Bitwise.lor(flag,AwaitCapableBit) != 0 then [{rtag={AwaitCapable}}|tags] else tags

  /* Allocate new buffer of given size */
  @private create_ = (%% BslMongo.Mongo.create %%: int -> mongo_buf)

  /* Build OP_INSERT message in buffer */
  @private insert_ = (%% BslMongo.Mongo.insert %%: mongo_buf, int, string, 'a -> void)

  /* Build OP_INSERT message in buffer */
  @private insert_batch_ = (%% BslMongo.Mongo.insert_batch %%: mongo_buf, int, string, list('a) -> void)

  /* Build OP_UPDATE message in buffer */
  @private update_ = (%% BslMongo.Mongo.update %%: mongo_buf, int, string, 'a, 'a -> void)

  /* Build OP_QUERY message in buffer */
  @private query_ = (%% BslMongo.Mongo.query %%: mongo_buf, int, string, int, int, 'a, option('a) -> void)

  /* Build OP_GET_MORE message in buffer */
  @private get_more_ = (%% BslMongo.Mongo.get_more %%: mongo_buf, string, int, cursorID -> void)

  /* Build OP_DELETE message in buffer */
  @private delete_ = (%% BslMongo.Mongo.delete %%: mongo_buf, int, string, 'a -> void)

  /* Build OP_KILL_CURSORS message in buffer */
  @private kill_cursors_ = (%% BslMongo.Mongo.kill_cursors %%: mongo_buf, list('a) -> void)

  /* Build OP_MSG message in buffer */
  @private msg_ = (%% BslMongo.Mongo.msg %%: mongo_buf, string -> void)

  /* Copies string out of buffer. */
  @private get_ = (%% BslMongo.Mongo.get %%: mongo_buf -> string)

  /* Access the raw string and length */
  @private export_ = (%% BslMongo.Mongo.export %%: mongo_buf -> (string, int))

  /* Clear out any data in the buffer, leave buffer allocated */
  @private clear_ = (%% BslMongo.Mongo.clear %%: mongo_buf -> void)

  /* Reset the buffer, unallocate storage */
  @private reset_ = (%% BslMongo.Mongo.reset %%: mongo_buf -> void)

  /* Free the buffer, return buffer for later use */
  @private free_ = (%% BslMongo.Mongo.free %%: mongo_buf -> void)

  /* Mailbox so we can use the streaming parser */
  @private new_mailbox_ = (%% BslMongo.Mongo.new_mailbox %%: int -> mailbox)
  @private reset_mailbox_ = (%% BslMongo.Mongo.reset_mailbox %%: mailbox -> void)

  /*
   * Specialised read, read until the size equals the (little endian)
   * 4-byte int at the start of the reply.
   */
  @private read_mongo_ = (%% BslMongo.Mongo.read_mongo %%: Socket.connection, mailbox -> reply)

  @private
  send_no_reply(m,_name): bool =
    match export_(m.mbuf) with
    | (str, len) ->
      s = String.substring(0,len,str)
      //do println("{name}: s=\n{Bson.dump(16,s)}")
      cnt = Socket.write_len(m.conn,s,len)
      do free_(m.mbuf)
      (cnt==len)

  @private
  send_with_reply(m,name): option(reply) =
    if send_no_reply(m,name)
    then {some=read_mongo_(m.conn,m.mailbox)}
    else {none}

  /**
   *  Create new mongo object:
   *    - Open connection to mongo server at addr:port
   *    - Allocate buffer of given size
   *    - Primitive error handling in case of mongo server malfunction
   **/
  open(bufsize:int, addr:string, port:int): Mongo.db =
    //do println("Mongo.open")
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
    //do println("Mongo.copy")
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
  insert(m:Mongo.db, flags:int, ns:string, documents:Bson.document): bool =
    //do println("Mongo.insert")
    m = { m with mbuf = create_(m.bufsize) }
    do insert_(m.mbuf,flags,ns,documents)
    send_no_reply(m,"insert")

  /**
   *  insertf:  same as insert but using tags instead of bit-wise flags.
   **/
  insertf(m:Mongo.db, tags:list(insert_tag), ns:string, documents:Bson.document): bool =
    flags = flags(List.map((t -> {itag=t}),tags))
    insert(m,flags,ns,documents)

  /**
   *  Send OP_INSERT with given collection name and multiple documents:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  insert_batch(m:Mongo.db, flags:int, ns:string, documents:list(Bson.document)): bool =
    //do println("Mongo.insert_batch")
    m = { m with mbuf = create_(m.bufsize) }
    do insert_batch_(m.mbuf,flags,ns,documents)
    send_no_reply(m,"insert")

  /**
   *  insert_batchf:  same as insert_batch but using tags instead of bit-wise flags.
   **/
  insert_batchf(m:Mongo.db, tags:list(insert_tag), ns:string, documents:list(Bson.document)): bool =
    flags = flags(List.map((t -> {itag=t}),tags))
    insert_batch(m,flags,ns,documents)

  /**
   *  Send OP_UPDATE with given collection name:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  update(m:Mongo.db, flags:int, ns:string, selector:Bson.document, update:Bson.document): bool =
    //do println("Mongo.update")
    m = { m with mbuf = create_(m.bufsize) }
    do update_(m.mbuf,flags,ns,selector,update)
    send_no_reply(m,"update")

  /**
   *  updatef:  same as update but using tags instead of bit-wise flags.
   **/
  updatef(m:Mongo.db, tags:list(update_tag), ns:string, selector:Bson.document, update_doc:Bson.document): bool =
    flags = flags(List.map((t -> {utag=t}),tags))
    update(m,flags,ns,selector,update_doc)

  /**
   *  Send OP_QUERY and get reply:
   **/
  query(m:Mongo.db, flags:int, ns:string, numberToSkip:int, numberToReturn:int,
        query:Bson.document, returnFieldSelector_opt:option(Bson.document)): option(reply) =
    //do println("Mongo.query")
    m = { m with mbuf = create_(m.bufsize) }
    do query_(m.mbuf,flags,ns,numberToSkip,numberToReturn,query,returnFieldSelector_opt)
    send_with_reply(m,"query")

  /**
   *  queryf:  same as query but using tags instead of bit-wise flags.
   **/
  queryf(m:Mongo.db, tags:list(query_tag), ns:string, numberToSkip:int, numberToReturn:int,
         query_doc:Bson.document, returnFieldSelector_opt:option(Bson.document)): option(reply) =
    flags = flags(List.map((t -> {qtag=t}),tags))
    query(m,flags,ns,numberToSkip,numberToReturn,query_doc,returnFieldSelector_opt)

  /**
   *  Send OP_GETMORE and get reply:
   **/
  get_more(m:Mongo.db, ns:string, numberToReturn:int, cursorID:cursorID): option(reply) =
    //do println("Mongo.get_more")
    m = { m with mbuf = create_(m.bufsize) }
    do get_more_(m.mbuf,ns,numberToReturn,cursorID)
    send_with_reply(m,"getmore")

  /**
   *  Send OP_DELETE:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  delete(m:Mongo.db, flags:int, ns:string, selector:Bson.document): bool =
    //do println("Mongo.delete")
    m = { m with mbuf = create_(m.bufsize) }
    do delete_(m.mbuf,flags,ns,selector)
    send_no_reply(m,"delete")

  /**
   *  deletef:  same as delete but using tags instead of bit-wise flags.
   **/
  deletef(m:Mongo.db, tags:list(delete_tag), ns:string, selector:Bson.document): bool =
    flags = flags(List.map((t -> {dtag=t}),tags))
    delete(m,flags,ns,selector)

  /**
   *  Send OP_KILL_CURSORS:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  kill_cursors(m:Mongo.db, cursors:list(cursorID)): bool =
    //do println("Mongo.kill_cursors")
    m = { m with mbuf = create_(m.bufsize) }
    do kill_cursors_(m.mbuf,cursors)
    send_no_reply(m,"kill_cursors")

  /**
   *  Send OP_MSG:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  msg(m:Mongo.db, msg:string): bool =
    m = { m with mbuf = create_(m.bufsize) }
    do msg_(m.mbuf,msg)
    send_no_reply(m,"msg")

  /**
   *  Close mongo connection and deallocate buffer.
   **/
  close(m:Mongo.db) =
    //do println("Mongo.close")
    do Socket.close(m.conn)
    do free_(m.mbuf)
    do reset_mailbox_(m.mailbox)
    void

  /**
   *  Close mongo copy, deallocate buffer but leave connection open.
   **/
  close_copy(m:Mongo.db) =
    //do println("Mongo.close_copy")
    do free_(m.mbuf)
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
  reply_document = (%% BslMongo.Mongo.reply_document %% : reply, int -> option(Bson.document))

  /** Debug routine, export the internal representation of the reply **/
  export_reply = (%% BslMongo.Mongo.export_reply %%: reply -> string)

  /** Null cursor value **/
  null_cursorID = (%% BslMongo.Mongo.null_cursorID %% : void -> cursorID)

  /** Return a string representation of a cursor (it's an int64) **/
  string_of_cursorID = (%% BslMongo.Mongo.string_of_cursorID %% : cursorID -> string)

  /** Predicate for end of query, when the cursorID is returned as zero **/
  is_null_cursorID = (%% BslMongo.Mongo.is_null_cursorID %% : cursorID -> bool)

  /**
   * Flags used by the index routines.
   **/
  UniqueBit     = 0x00000001
  DropDupsBit   = 0x00000002
  BackgroundBit = 0x00000004
  SparseBit     = 0x00000008

  /**
   * [create_index(mongo, "ns", key, flags)] adds an index to a collection.
   *
   * [key] is a bson object defining the fields to be indexed, eg. [\[\{Int32=("age",1)\}, \{Int32=("name",1)\}\]]
   **/
  @private create_index_(m:Mongo.db, ns:string, key:Bson.document, opts:Bson.document): bool =
    keys = Bson.keys(key)
    name = "_"^(String.concat("",keys))
    b = List.flatten([[H.doc("key",key), H.str("ns",ns), H.str("name",name)],opts])
    idxns=(match String.index(".",ns) with | {some=p} -> String.substring(0,p,ns) | {none} -> ns)^".system.indexes"
    insert(m,0,idxns,b)

  create_index(m:Mongo.db, ns:string, key:Bson.document, options:int): bool =
    opts =
      List.flatten([(if Bitwise.land(options,UniqueBit) != 0 then [H.bool("unique",true)] else []),
                    (if Bitwise.land(options,DropDupsBit) != 0 then [H.bool("dropDups",true)] else []),
                    (if Bitwise.land(options,BackgroundBit) != 0 then [H.bool("background",true)] else []),
                    (if Bitwise.land(options,SparseBit) != 0 then [H.bool("sparse",true)] else [])])
    create_index_(m, ns, key, opts)

  create_indexf(m:Mongo.db, ns:string, key:Bson.document, tags:list(index_tag)): bool =
    opts =
      List.map((t ->
                 match t with
                 | {Unique} -> H.bool("unique",true)
                 | {DropDups} -> H.bool("dropDups",true)
                 | {Background} -> H.bool("background",true)
                 | {Sparse} -> H.bool("sparse",true)),tags)
    create_index_(m, ns, key, opts)

  /**
   * Simpler version of the [create_index] function, for a single named field.
   **/
  create_simple_index(m:Mongo.db, ns:string, field:string, options:int): bool =
    create_index(m, ns, [H.i32(field,1)], options)

}}

// End of file mongo.opa
