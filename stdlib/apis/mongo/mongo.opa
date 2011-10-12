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

/* Major TODOs, there are minor ones elsewhere. */
// TODO: replica sets
// TODO: backups

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
type Bson.value =
    { Double: float }
  / { String: string }
  / { Document: Bson.document }
  / { Array: Bson.document }
  / { Binary: string }
  / { ObjectID: string }
  / { Boolean: bool }
  / { Date: Date.date }
  / { Null: void } // TODO: ditch the void
  / { Regexp: (string, string) }
  / { Code: string }
  / { Symbol: string }
  / { CodeScope: (string, Bson.document) }
  / { Int32: int }
  / { Timestamp: (int, int) }
  / { Int64: int }

@opacapi
type Bson.element = { name:string; value:Bson.value }

/**
 * The main exported type, a document is just a list of elements.
 */
@opacapi
type Bson.document = list(Bson.element)

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

/**
 * Helper functions for constructing Bson values.
 *
 * Short names intended to be abbreviations for {name=...; value={Xyz=...}}.
 **/
@server_private
H = {{
  v(n:string,v:Bson.value):Bson.element = {name=n; value=v}
  dbl(n:string,d:float):Bson.element = {name=n; value={Double=d}}
  str(n:string,s:string):Bson.element = {name=n; value={String=s}}
  doc(n:string,d:Bson.document):Bson.element = {name=n; value={Document=d}}
  arr(n:string,d:Bson.document):Bson.element = {name=n; value={Array=d}}
  docarr(n:string,l:list(Bson.document)):Bson.element =
    {name=n; value={Array=List.mapi((i, d -> ({name="{i}"; value={Document=d}}:Bson.element)),l)}}
  binary(n:string,b:string):Bson.element = {name=n; value={Binary=b}}
  oid(n:string,id:string):Bson.element = {name=n; value={ObjectID=id}}
  bool(n:string,b:bool):Bson.element = {name=n; value={Boolean=b}}
  date(n:string,d:Date.date):Bson.element = {name=n; value={Date=d}}
  regexp(n:string,re:(string,string)):Bson.element = {name=n; value={Regexp=re}}
  null(n:string):Bson.element = {name=n; value={Null=void}}
  code(n:string,c:string):Bson.element = {name=n; value={Code=c}}
  symbol(n:string,s:string):Bson.element = {name=n; value={Symbol=s}}
  codescope(n:string,cs:(string,Bson.document)):Bson.element = {name=n; value={CodeScope=cs}}
  i32(n:string,i:int):Bson.element = {name=n; value={Int32=i}}
  timestamp(n:string,ts:(int,int)):Bson.element = {name=n; value={Timestamp=ts}}
  i64(n:string,i:int):Bson.element = {name=n; value={Int64=i}}
  de(e:Bson.element):Bson.document = [e]
  dl(l:list(Bson.element)):Bson.document = l
  dd(n:string,d:Bson.document):Bson.document = [{name=n; value={Document=d}}]
}}

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
  etype(element:Bson.element): int =
    match element.value with
    | {Double=_} -> tDouble
    | {String=_} -> tString
    | {Document=_} -> tDocument
    | {Array=_} -> tArray
    | {Binary=_} -> tBinary
    | {ObjectID=_} -> tObjectID
    | {Boolean=_} -> tBoolean
    | {Date=_} -> tDate
    | {Null=_} -> tNull
    | {Regexp=_} -> tRegexp
    | {Code=_} -> tCode
    | {Symbol=_} -> tSymbol
    | {CodeScope=_} -> tCodeScope
    | {Int32=_} -> tInt32
    | {Timestamp=_} -> tTimestamp
    | {Int64=_} -> tInt64

  /**
   * Return the key of an element.
   **/
  key(element:Bson.element): string = element.name

  /**
   * Update the key of an element.
   **/
  set_key(element:Bson.element, name:string): Bson.element = { element with ~name }

  /**
   * Find an element by key in a bson object.
   **/
  find_element(bson:Bson.document, name:string): option(Bson.element) =
    List.find((b0 -> key(b0) == name),bson)

  /**
   * Find the first of one of a list of keys in a document.
   **/
  find_elements(bson:Bson.document, names:list(string)): option((string, Bson.element)) =
    rec aux(d:list(Bson.element)) =
      match d with
      | {hd=e; tl=rest} ->
        ekey = key(e)
        (match List.find((n -> n == ekey),names) with
         | {some=k} -> {some=(k,e)}
         | {none} -> aux(rest))
      | {nil} -> {none}
    aux(bson)

  /**
   * Find key of given name in bson object.
   * We only look at the current level, mostly it's for finding
   * "ok" or "errval" etc. in replies.
   **/
  find(bson:Bson.document, name:string): option(Bson.document) =
    Option.map((b -> [b]),find_element(bson, name))

  /**
   * Some type-specific versions of [find], search for [key]
   * in [bson] object and return required type, if possible.
   * Note that if the key exists but is of the wrong type
   * then you will still get [\{none\}].
   **/

  find_bool(bson:Bson.document, name:string): option(bool) =
    match Option.map((e -> e.value),find_element(bson, name)) with
    | {some={Boolean=tf}} -> {some=tf}
    | _ -> {none}

  find_int(bson:Bson.document, name:string): option(int) =
    match Option.map((e -> e.value),find_element(bson, name)) with
    | {some={Int32=i}} -> {some=i}
    | {some={Int64=i}} -> {some=i}
    | {some={Double=d}} -> {some=Float.to_int(d)}
    | _ -> {none}

  find_string(bson:Bson.document, name:string): option(string) =
    match Option.map((e -> e.value),find_element(bson, name)) with
    | {some={String=str}} -> {some=str}
    | {some={Null=_}} -> {some=""}
    | _ -> {none}

  find_doc(bson:Bson.document, name:string): option(Bson.document) =
    match Option.map((e -> e.value),find_element(bson, name)) with
    | {some={Document=doc}} -> {some=doc}
    | _ -> {none}

  /**
   * Return the type of a matching Bson key.
   **/
  find_type(bson:Bson.document, name:string): option(int) = Option.map(etype,find_element(bson,name))

  /**
   * Return a list of the keys in a bson object.
   **/
  keys(bson:Bson.document): list(string) = List.map(key, bson)

  /**
   * Iterate over the elements in a bson object.
   **/
  iter(f:(Bson.element -> void), bson:Bson.document) : void =
    List.iter(f,bson)

  /**
   * Map over the elements in a bson object.
   **/
  map(f:(Bson.element -> Bson.element), bson:Bson.document) : Bson.document =
    List.map(f,bson)

  /**
   * Fold over the elements in a bson object.
   **/
  fold(f:(Bson.element, 'a -> 'a), bson:Bson.document, acc:'a) : 'a =
    List.fold(f,bson,acc)

  /**
   * Find a particular element.
   **/
  find_raw(f:(Bson.element -> bool), bson:Bson.document): option(Bson.element) =
    List.find(f,bson)

  /**
   * Remove the ObjectID element from a document.
   **/
  remove_id(doc:Bson.document): Bson.document =
    List.filter((e -> match e.value with | {ObjectID=_} -> {false} | _ -> {true}),doc)

  /**
   * Sort the elements in a document by lexicographic order on keys.
   **/
  sort_document(doc:Bson.document): Bson.document = List.sort_by(Bson.key,doc)

  /**
   * Attempt to turn a bson element into a string which looks like
   * the mongo shell syntax.
   * Note: still only partial.
   **/
  rec string_of_value(value:Bson.value): string =
    match value with
    | {Double=v} -> "Double {v}"
    | {String=v} -> "String {v}"
    | {Document=v} -> "Document {string_of_bson(v)}"
    | {Array=v} -> "Array {string_of_bson(v)}"
    | {Binary=v} -> "Binary {v}"
    | {ObjectID=v} -> "ObjectID {oid_to_string(v)}"
    | {Boolean=v} -> "Boolean {v}"
    | {Date=v} -> "Date {v}"
    | {Null=_} -> "Null"
    | {Regexp=v} -> "Regexp {v}"
    | {Code=v} -> "Code {v}"
    | {Symbol=v} -> "Symbol {v}"
    | {CodeScope=v} -> "CodeScope {v}"
    | {Int32=v} -> "Int32 {v}"
    | {Timestamp=(t,i)} -> "Timestamp \{ \"t\" : {t}, \"i\" : {i}"
    | {Int64=v} -> "Int64 {v}"

  string_of_element(element:Bson.element): string = "\"{element.name}\" : {string_of_value(element.value)}"

  /**
   * Same for a bson object (just a list of elements).
   **/
  string_of_bson(bson:Bson.document): string = "\{ "^(String.concat(", ",List.map(string_of_element,bson)))^" \}"

  /**
   * Same as string_of_bson except we miss out the tags showing the
   * actual name of the element.
   **/
  rec pretty_of_value(value:Bson.value): string =
    match value with
    | {Double=v} -> "{v}"
    | {String=v} -> "\"{v}\""
    | {Document=v} -> "{pretty_of_bson(v)}"
    | {Array=v} -> "{pretty_of_array(v)}"
    | {Binary=_} -> "<BINARY>"
    | {ObjectID=v} -> "{oid_to_string(v)}"
    | {Boolean=v} -> "{v}"
    | {Date=v} -> "{v}" // <-- TODO: format this
    | {Null=_} -> "null"
    | {Regexp=(re,opts)} -> "REGEXP(/{re}/{opts})"
    | {Code=v} -> "CODE({v})"
    | {Symbol=v} -> "SYMBOL({v})"
    | {CodeScope=v} -> "{v}"
    | {Int32=v} -> "{v}"
    | {Timestamp=(t,i)} -> "\{ \"t\" : {t}, \"i\" : {i} \}"
    | {Int64=v} -> "{v}L"

  pretty_of_element(element:Bson.element): string =
    "\"{element.name}\" : {pretty_of_value(element.value)}"

  pretty_of_array(a:Bson.document): string =
    "["^(String.concat(", ",List.map((e -> pretty_of_value(e.value)),a)))^"]"

  pretty_of_bson(bson:Bson.document): string = "\{ "^(String.concat(", ",List.map(pretty_of_element,bson)))^" \}"

  /**
   * Convert a result value into a more friendly string.
   * Errors can be internal (just a string) or could be document
   * returned by mongo.  Which may be an error even if the
   * outcome is "success".
   * TODO: Mongo sends all sorts of rubbish here, we need to parse all that stuff.
   **/
  @private
  string_of_doc(doc:Bson.document): string =
    ok =
      match find_int(doc,"ok") with
      | {some=0} -> "<not ok>"
      | {some=1} -> "<ok>"
      | {some=n} -> "<not ok> (Weird ok number {n})"
      | {none} -> "<unknown ok status>"
    err = match find_string(doc, "err") with | {some=""} -> "" | {some=err} -> "<err=\"{err}\">" | {none} -> ""
    code = match find_int(doc, "code") with | {some=code} -> "<code={code}>" | {none} -> ""
    n = match find_int(doc, "n") with | {some=n} -> "<n={n}>" | {none} -> ""
    errmsg = match find_string(doc, "errmsg") with | {some=""} -> "" | {some=errmsg} -> "<errmsg=\"{errmsg}\">" | {none} -> ""
    String.concat(" ",List.filter((s -> s != ""),[ok,err,code,n,errmsg]))

  string_of_failure(failure:Mongo.failure): string =
    match failure with
    | {Error=str} -> str
    | {DocError=doc} -> string_of_doc(doc)

  string_of_result(result:Mongo.result): string =
    match result with
    | {success=doc} -> string_of_doc(doc)
    | {~failure} -> string_of_failure(failure)

}}

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

@server_private
Mongo = {{

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
  ShardConfigStaleBit = 0x00000003
  AwaitCapableBit     = 0x00000004

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
  insert(m:Mongo.db, flags:int, ns:string, documents:Bson.document): bool =
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
    m = { m with mbuf = create_(m.bufsize) }
    do get_more_(m.mbuf,ns,numberToReturn,cursorID)
    send_with_reply(m,"getmore")

  /**
   *  Send OP_DELETE:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  delete(m:Mongo.db, flags:int, ns:string, selector:Bson.document): bool =
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
  reply_document = (%% BslMongo.Mongo.reply_document %% : reply, int -> option(Bson.document))

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
     query : option(Bson.document);
     fields : option(Bson.document);
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
  set_query(c:cursor, query:option(Bson.document)): cursor = { c with ~query }
  set_fields(c:cursor, fields:option(Bson.document)): cursor = { c with ~fields }

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
  all_documents(c:cursor): outcome(list(Bson.document), Mongo.failure) =
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
  start(m:Mongo.db, ns:string, query:Bson.document, limit:int): cursor =
    c = Cursor.init(m,ns)
    /* Note: MongoDB seems to interpret limit=1 as "just send me one document".
     * If you want this loop to scan all the documents you can't use limit=1.
     */
    c = Cursor.set_limit(c,(max(2,limit)))
    c = Cursor.set_query(c,{some=query})
    Cursor.next(c)

  /**
   * Test if there is still data in a cursor.
   **/
  valid(c:cursor): bool =
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
  find(m:Mongo.db, ns:string, query:Bson.document, fields:option(Bson.document),
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
  check_err(b:Bson.document): Mongo.result =
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
  find_one(m:Mongo.db, ns:string, query:Bson.document, fields:option(Bson.document)): Mongo.result =
    c = init(m, ns)
    c = set_query(c, {some=query})
    c = set_fields(c, fields)
    c = set_limit(c, 1)
    c = next(c)
    outcome = check_cursor_error(c)
    _ = Cursor.reset(c)
    outcome

  @private
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

  /**
   * Run a "$cmd" command.
   *
   * Normally you will get {ok: 0/1} as a reply but sometimes there
   * are other elements in the reply.
   **/
  run_command(m:Mongo.db, ns:string, command:Bson.document): Mongo.result =
    match find_one(m, ns^".$cmd", command, {none}) with
    | {success=bson} -> check_ok(bson)
    | {~failure} -> {~failure}

  /**
   * Perform a simple integer command, eg. [{ ping : 1 }]
   **/
  simple_int_command(m:Mongo.db, ns:string, cmd:string, arg:int): Mongo.result =
    run_command(m, ns, [H.i32(cmd,arg)])

  /**
   * Perform a simple integer command, eg. [{ drop : "collection" }]
   **/
  simple_str_command(m:Mongo.db, ns:string, cmd:string, arg:string): Mongo.result =
    run_command(m, ns, [H.str(cmd,arg)])

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
   * Return the last error from database.
   **/
  last_error(m:Mongo.db, db:string): Mongo.result =
    simple_int_command(m, db, "getlasterror", 1)

  /**
   * Reset database error status.
   **/
  reset_error(m:Mongo.db, db:string): Mongo.result =
    simple_int_command(m, db, "reseterror", 1)

  /**
   * Force a db error.
   **/
  force_error(m:Mongo.db, db:string): Mongo.result =
    simple_int_command(m, db, "forceerror", 1)

  find_int(bson:Bson.document, name:string): outcome(int,Mongo.failure) =
    match Bson.find_element(bson,name) with
    | {some={value={Int32=n} ...}} -> {success=n}
    | {some={value={Int64=n} ...}} -> {success=n}
    | {some={value={Double=n} ...}} -> {success=Float.to_int(n)}
    | _ -> {failure={Error="Missing {name} Int32/Int64/Double"}}

  /**
   * Count the number of matching elements.
   *
   * [count(mongo, "db", "collection", query_opt)] returns the number of elements
   * matching [query] or the whole collection if [query_opt] is [\{none\}].
   *
   * Strictly speaking MongoDB returns an Int64 value but it is unlikely that
   * a database will be able to overrun the OCaml restriction, so the value is
   * just an int.
   **/
  count(m:Mongo.db, db:string, coll:string, query_opt:option(Bson.document)): outcome(int,Mongo.failure) =
    cmd = List.flatten([[H.str("count",coll)],
                        (match query_opt with | {some=query} -> [H.doc("query",query)] | {none} -> [])])
    match run_command(m, db, cmd) with
    | {success=bson} ->
       //do println("Cursor.count: bson={Bson.pretty_of_bson(bson)}")
       find_int(bson, "n")
    | {~failure} -> {~failure}

  distinct(m:Mongo.db, db:string, coll:string, key:string, query_opt:option(Bson.document)): Mongo.result =
    cmd = List.flatten([[H.str("distinct",coll), H.str("key",key)],
                        (match query_opt with | {some=query} -> [H.doc("query",query)] | {none} -> [])])
    match run_command(m, db, cmd) with
    | {success=bson} ->
       //do println("Cursor.distinct: bson={Bson.pretty_of_bson(bson)}")
       // TODO: stats
       (match Bson.find(bson,"values") with
        | {some=[{name=k; value={Array=d}}]} -> {success=[H.arr(k,List.rev(d))]}
        | _ -> {failure={DocError=bson}})
    | {~failure} -> {~failure}

  group(m:Mongo.db, db:string, coll:string, key:Bson.document, reduce:string, initial:Bson.document,
        cond_opt:option(Bson.document), finalize_opt:option(string)): Mongo.result =
    group =
      [H.doc("group",
         List.flatten([
           [H.str("ns",coll), H.doc("key",key), H.code("$reduce",reduce), H.doc("initial",initial)],
           (match cond_opt with | {some=cond} -> [H.doc("cond",cond)] | {none} -> [H.null("cond")]),
           (match finalize_opt with | {some=finalize} -> [H.code("finalize",finalize)] | {none} -> [])]))]
    //do println("Cursor.group: group={Bson.pretty_of_bson(group)}")
    match run_command(m, db, group) with
    | {success=bson} ->
       //do println("Cursor.group: bson={Bson.pretty_of_bson(bson)}")
       {success=bson}
    | {~failure} -> {~failure}

  /**
   * Predicate for master status.
   **/
  ismaster(m:Mongo.db): outcome(bool,Mongo.failure) =
    match simple_int_command(m, "admin", "ismaster", 1) with
    | {success=bson} ->
      (match Bson.find(bson,"ismaster") with
       | {some=[{name="ismaster"; value={Boolean=ismaster}}]} -> {success=ismaster}
       | _ -> {failure={Error="Missing ismaster Boolean"}})
    | {~failure} -> {~failure}

  @private pass_digest(user:string, pass:string): string = Crypto.Hash.md5("{user}:mongo:{pass}")

  /**
   * Authentication: [add_user(mongo, "db", "user", "pass")] creates a
   * user for the given db.
   **/
  add_user(m:Mongo.db, db:string, user:string, pass:string): bool =
    digest = pass_digest(user,pass)
    bselector = [H.str("user",user)]
    bupdate = [H.doc("$set",[H.str("pwd",digest)])]
    Mongo.update(m,Mongo.UpsertBit,(db^".system.users"),bselector,bupdate)

  /**
   * Authenticate a user for the given database.
   *
   * The password must match the users password.
   **/
  authenticate(m:Mongo.db, db:string, user:string, pass:string): Mongo.result =
    match simple_int_command(m, db, "getnonce", 1) with
    | {success=bson} ->
      (match Bson.find_element(bson,"nonce") with
       | {some={name="nonce"; value={String=nonce}}} ->
         digest = pass_digest(user,pass)
         hash = Crypto.Hash.md5("{nonce}{user}{digest}")
         cmd = [H.i32("authenticate",1), H.str("user",user), H.str("nonce",nonce), H.str("key",hash)]
         run_command(m,db,cmd)
       | _ -> {failure={Error="Missing nonce String"}})
    | {~failure} -> {~failure}

}}

/* Tags for indices */
type index_tag =
  {Unique} /
  {DropDups} /
  {Background} /
  {Sparse}

@server_private
Indexes = {{

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
    Mongo.insert(m,0,idxns,b)

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
