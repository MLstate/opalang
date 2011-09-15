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
import stdlib.core.{date}
import stdlib.io.socket
import stdlib.crypto
import stdlib.system

type mongo_buf = external
type cursorID = external
type mailbox = external
type reply = external

type RPC.Bson.bson0('bson) =
    { Double: (string, float) }
  / { String: (string, string) }
  / { Document: (string, 'bson) }
  / { Array: (string, 'bson) }
  / { Binary: (string, string) }
  / { ObjectID: (string, string) }
  / { Boolean: (string, bool) }
  / { Date: (string, Date.date) }
  / { Null: (string, void) }
  / { Regexp: (string, (string, string)) }
  / { Code: (string, string) }
  / { Symbol: (string, string) }
  / { CodeScope: (string, (string, 'bson)) }
  / { Int32: (string, int) }
  / { Timestamp: (string, (int, int)) }
  / { Int64: (string, int) }

@opacapi
type RPC.Bson.bson = list(RPC.Bson.bson0(RPC.Bson.bson))

type mongo = {
     conn : Socket.connection;
     mbuf : mongo_buf;
     mailbox : mailbox;
     bufsize : int
  }

type Mongo.failure =
    {Error : string}
  / {MongoError : (string, int)}

//type Mongo.success = RPC.Bson.bson

//type Mongo.result = outcome(Mongo.success, Mongo.failure)

Bson = {{

  /** Convenience function, dump string as hex and ascii */
  dump = (%% BslMongo.Bson.dump %%: int, string -> string)

  /** Return new Bson Object ID */
  new_oid = (%% BslMongo.Bson.new_oid %%: void -> string)

  /** Get OID from string */
  oid_of_string = (%% BslMongo.Bson.oid_of_string %%: string -> string)

  /** Get string from OID */
  oid_to_string = (%% BslMongo.Bson.oid_to_string %%: string -> string)

  /**
   * Find key of given name in bson object.
   * We only look at the current level, mostly it's for finding
   * "ok" or "errval" etc. in replies.
   **/
  find(bson:RPC.Bson.bson, name:string): option(RPC.Bson.bson) =
    Option.map((b -> [b]),
               List.find((b ->
                            match b with
                            | { Double=(key, _) } -> key == name
                            | { String=(key, _) } -> key == name
                            | { Document=(key, _) } -> key == name
                            | { Array=(key, _) } -> key == name
                            | { Binary=(key, _) } -> key == name
                            | { ObjectID=(key, _) } -> key == name
                            | { Boolean=(key, _) } -> key == name
                            | { Date=(key, _) } -> key == name
                            | { Null=(key, _) } -> key == name
                            | { Regexp=(key, _) } -> key == name
                            | { Code=(key, _) } -> key == name
                            | { Symbol=(key, _) } -> key == name
                            | { CodeScope=(key, _) } -> key == name
                            | { Int32=(key, _) } -> key == name
                            | { Timestamp=(key, _) } -> key == name
                            | { Int64=(key, _) } -> key == name),
                         bson))

}}

//@both <-- ??? why doesn't this work ???
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
  open(bufsize,addr,port): mongo =
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
  copy(m:mongo): mongo =
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
  insert(m,flags,ns,documents): bool =
    do insert_(m.mbuf,flags,ns,documents)
    send_no_reply(m,"insert")

  /**
   *  Send OP_UPDATE with given collection name:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  update(m,flags,ns,selector,update): bool =
    do update_(m.mbuf,flags,ns,selector,update)
    send_no_reply(m,"update")

  /**
   *  Send OP_QUERY and get reply:
   **/
  query(m,flags,ns,numberToSkip,numberToReturn,query,returnFieldSelector_opt): option(reply) =
    do query_(m.mbuf,flags,ns,numberToSkip,numberToReturn,query,returnFieldSelector_opt)
    send_with_reply(m,"query")

  /**
   *  Send OP_GETMORE and get reply:
   **/
  get_more(m,ns,numberToReturn,cursorID): option(reply) =
    do get_more_(m.mbuf,ns,numberToReturn,cursorID)
    send_with_reply(m,"getmore")

  /**
   *  Send OP_DELETE:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  delete(m,flags,ns,selector): bool =
    do delete_(m.mbuf,flags,ns,selector)
    send_no_reply(m,"delete")

  /**
   *  Send OP_KILL_CURSORS:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  kill_cursors(m,cursors:list(cursorID)): bool =
    do kill_cursors_(m.mbuf,cursors)
    send_no_reply(m,"kill_cursors")

  /**
   *  Send OP_MSG:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  msg(m,msg): bool =
    do msg_(m.mbuf,msg)
    send_no_reply(m,"msg")

  /**
   *  Close mongo connection and deallocate buffer.
   **/
  close(m) =
    do Socket.close(m.conn)
    do reset_(m.mbuf)
    do reset_mailbox_(m.mailbox)
    void

  /**
   *  Close mongo copy, deallocate buffer but leave connection open.
   **/
  close_copy(m) =
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
  reply_document = (%% BslMongo.Mongo.reply_document %% : reply, int -> option(RPC.Bson.bson))

  /** Debug routine, export the internal representation of the reply **/
  export_reply = (%% BslMongo.Mongo.export_reply %%: reply -> string)

  /** Null cursor value **/
  null_cursorID = (%% BslMongo.Mongo.null_cursorID %% : void -> cursorID)

  /** Return a string representation of a cursor (it's an int64) **/
  string_of_cursorID = (%% BslMongo.Mongo.string_of_cursorID %% : cursorID -> string)

  /** Predicate for end of query, when the cursorID is returned as zero **/
  is_null_cursorID = (%% BslMongo.Mongo.is_null_cursorID %% : cursorID -> bool)

}}

type cursor = {
     mongo : mongo;
     ns : string;
     flags : int;
     skip : int;
     limit : int;
     query : option(RPC.Bson.bson);
     fields : option(RPC.Bson.bson);
     query_sent : bool;
     cid : cursorID;
     reply : option(reply);
     returned : int;
     current : int;
     doc : RPC.Bson.bson;
     killed : bool;
     error : string
}

Cursor = {{

  error_document(err:string, code:int) = [{String=("$err",err)}, {Int32=("code",code)}]

  init(mongo:mongo, ns:string): cursor =
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

  set_flags(c:cursor, flags:int): cursor = { c with ~flags }
  set_skip(c:cursor, skip:int): cursor = { c with ~skip }
  set_limit(c:cursor, limit:int): cursor = { c with ~limit }
  set_query(c:cursor, query:option(RPC.Bson.bson)): cursor = { c with ~query }
  set_fields(c:cursor, fields:option(RPC.Bson.bson)): cursor = { c with ~fields }

  set_error(c:cursor, error:string): cursor = { c with ~error; killed={true} }

  @private
  reply(c:cursor, reply_opt:option(reply), name:string, query_sent:bool): cursor =
    match reply_opt with
    | {some=reply} ->
      // TODO: Check for $err
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

  op_query(c:cursor): cursor =
    if not(c.killed) && Option.is_some(c.query)
    then reply(c,Mongo.query(c.mongo, c.flags, c.ns, c.skip, c.limit, Option.get(c.query), c.fields),"op_query",{true})
    else set_error(c,(if c.killed
                      then "Cursor.op_query: already killed"
                      else "Cursor.op_query: no query"))

  get_more(c:cursor): cursor =
    if not(c.killed) && not(Mongo.is_null_cursorID(c.cid))
    then reply(c,Mongo.get_more(c.mongo, c.ns, c.limit, c.cid),"get_more",c.query_sent)
    else set_error(c,"Cursor.get_more: attempt to get more with dead cursor")

  document(c:cursor, n:int): outcome(RPC.Bson.bson, Mongo.failure) =
    if n >= c.returned
    then {failure={Error="Cursor.document: document index out of range {n}"}}
    else
      match c.reply with
      | {some=reply} ->
        (match Mongo.reply_document(reply,n) with
         | {some=doc} -> {success=doc}
         | {none} -> {failure={Error="Cursor.document: no document"}})
      | {none} -> {failure={Error="Cursor.document: no reply"}}

  all_documents(c:cursor): outcome(list(RPC.Bson.bson), Mongo.failure) =
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

  reset(c:cursor): cursor =
    if not(Mongo.is_null_cursorID(c.cid))
    then
      if Mongo.kill_cursors(c.mongo, [c.cid])
      then destroy(c)
      else set_error(destroy(c),"Cursor.reset: error killing cursor")
    else destroy(c)

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

  find(m:mongo, ns:string, query:RPC.Bson.bson, fields:option(RPC.Bson.bson),
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

  get_err(b:RPC.Bson.bson): option((string,int)) =
    match b with
    | [{String=("$err",err)}, {Int32=("code",code)}] -> {some=(err,code)}
    | _ -> {none}

  find_one(m:mongo, ns:string, query:RPC.Bson.bson, fields:option(RPC.Bson.bson)): outcome(RPC.Bson.bson,Mongo.failure) =
    c = init(m, ns)
    c = set_query(c, {some=query})
    c = set_fields(c, fields)
    c = set_limit(c, 1)
    c = next(c)
    outcome=
      if not(c.killed)
      then
        (match get_err(c.doc) with
         | {some=(err,code)} -> {failure={MongoError=(err,code)}}
         | {none} -> {success=c.doc})
      else {failure={Error=c.error}}
    _ = Cursor.reset(c)
    outcome

  @private
  check_ok(bson:RPC.Bson.bson): outcome(RPC.Bson.bson,Mongo.failure) =
    match Bson.find(bson,"ok") with
    | {some=[{Double=("ok",ok)}]} ->
       if ok == 1.0
       then {success=bson}
       else
         (match Bson.find(bson,"errmsg") with
          | {some=[{String=("errmsg",errmsg)}]} -> {failure={Error=errmsg}}
          | _ -> {failure={Error="ok:{ok}"}})
    | _ -> {success=bson}

  run_command(m:mongo, ns:string, command:RPC.Bson.bson): outcome(RPC.Bson.bson,Mongo.failure) =
    match find_one(m, ns^".$cmd", command, {none}) with
    | {success=bson} -> check_ok(bson)
    | {~failure} -> {~failure}

  simple_int_command(m:mongo, ns:string, cmd:string, arg:int): outcome(RPC.Bson.bson,Mongo.failure) =
    run_command(m, ns, [{Int32=(cmd,arg)}])

  simple_str_command(m:mongo, ns:string, cmd:string, arg:string): outcome(RPC.Bson.bson,Mongo.failure) =
    run_command(m, ns, [{String=(cmd,arg)}])

  check_connection(m:mongo): outcome(bool,Mongo.failure) =
    match simple_int_command(m, "admin", "ping", 1) with
    | {success=_} -> {success=true}
    | {~failure} -> {~failure}

  drop_db(m:mongo, db:string): outcome(RPC.Bson.bson,Mongo.failure) =
    simple_int_command(m, db, "dropDatabase", 1)

  drop_collection(m:mongo, db:string, collection:string): outcome(RPC.Bson.bson,Mongo.failure) =
    simple_str_command(m, db, "drop", collection)

  reset_error(m:mongo, db:string): outcome(RPC.Bson.bson,Mongo.failure) =
    simple_int_command(m, db, "reseterror", 1)

  @private
  find_int(bson:RPC.Bson.bson, name:string): outcome(int,Mongo.failure) =
    match Bson.find(bson,name) with
    | {some=[{Int32=(_,n)}]} -> {success=n}
    | {some=[{Int64=(_,n)}]} -> {success=n}
    | {some=[{Double=(_,n)}]} -> {success=Float.to_int(n)}
    | _ -> {failure={Error="Missing {name} Int32/Int64/Double"}}

  count(m:mongo, db:string, ns:string, query_opt:option(RPC.Bson.bson)): outcome(int,Mongo.failure) =
    cmd = List.flatten([[{String=("count",ns)}],
                        (match query_opt with | {some=query} -> [{Document=("query",query)}] | {none} -> [])])
    match run_command(m, db, cmd) with
    | {success=bson} -> find_int(bson, "n")
    | {~failure} -> {~failure}

  ismaster(m:mongo): outcome(bool,Mongo.failure) =
    match simple_int_command(m, "admin", "ismaster", 1) with
    | {success=bson} ->
      (match Bson.find(bson,"ismaster") with
       | {some=[{Boolean=("ismaster",ismaster)}]} -> {success=ismaster}
       | _ -> {failure={Error="Missing ismaster Boolean"}})
    | {~failure} -> {~failure}

  @private pass_digest(user:string, pass:string): string =
    hash = Crypto.Hash.md5("{user}:mongo:{pass}")
    //do println("user=\"{user}\"  pass=\"{pass}\"  hash=\"{hash}\"")
    hash

  add_user(m:mongo, db:string, user:string, pass:string): bool =
    digest = pass_digest(user,pass)
    bselector = [{String=("user",user)}]
    bupdate = [{Document=("$set",[{String=("pwd",digest)}])}]
    Mongo.update(m,Mongo._Upsert,(db^".system.users"),bselector,bupdate)

  authenticate(m:mongo, db:string, user:string, pass:string): outcome(RPC.Bson.bson,Mongo.failure) =
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

  // TODO: indexes
  // TODO: replica sets
  // TODO: backups

}}

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
  //do println("cursor={cursor}")
  do Mongo.close(mongo)
  void
*/

