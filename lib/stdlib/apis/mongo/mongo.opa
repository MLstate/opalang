/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
 * This is a binding for MongoDB for OPA, loosely based around the C drivers.
 *
 * Module [MongoDriver] has low-level routines to talk to the database server, the only
 * routines you should need are the [MongoDriver.open] and [MongoDriver.close] functions.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

import stdlib.core.{date,rpc.core}
import stdlib.io.socket
import stdlib.system

/** Some (formerly) external types **/

type Mongo.mongo_buf = list(WireProtocol.Message)
type Mongo.cursorID = int64
type Mongo.mailbox = Mailbox.t
type Mongo.reply = WireProtocol.Message

/**
 * Main connection type.
 * Stores the socket connection plus other parameters such as
 * the seeds, timing parameters for reconnection and a limiter for recursion depth.
 **/
@abstract
type Mongo.db = {
  conn : option(Socket.t);
  reconncell : Cell.cell(Mongo.reconnectmsg,Mongo.reconnectresult);
  pool : SocketPool.t;
  name : string;
  seeds : list(Mongo.mongo_host);
  depth : int;
  max_depth : int;
  conf : Mongo.conf
}

/**
 * Configuration to opening a database.
 *
 */
type Mongo.conf = {
  bufsize      : int
  poolmax      : int
  slaveok      : bool
  attempt      : int
  verbose      : bool
  auths        : Mongo.auths
  timeout      : int
  waiting      : int
  replica      : option(string)
}

/** Outgoing Cell messages **/
@private
type Mongo.sr =
    {send:(Mongo.db,Mongo.mongo_buf)} // Send and forget
  / {sendrecv:(Mongo.db,Mongo.mongo_buf)} // Send and expect reply
  / {senderror:(Mongo.db,Mongo.mongo_buf)} // Send and call getlasterror
  / {stop} // Stop the cell

/** Incoming Cell messages **/
@private
type Mongo.srr =
    {sendresult:bool}
  / {sndrcvresult:option(Mongo.reply)}
  / {snderrresult:option(Mongo.reply)}
  / {stopresult}
  / {reconnect}
  / {noconnection}

/** Messages for socket pool **/
@private type Mongo.reconnectmsg =
   {reconnect:(string,Mongo.db)}
 / {getseeds}
 / {stop}
@private type Mongo.reconnectresult =
   {reconnectresult:option(Mongo.db)}
 / {getseedsresult:list(Mongo.mongo_host)}
 / {stopresult}

@server_private
MongoDriver = {{

  @private H = Bson.Abbrevs
  @private C = MongoCommon
  @private WP = WireProtocol

  @private
  Log = {{

    @private
    @expand
    gen(f, m, fn:string, msg) =
      if m.conf.verbose then f("MongoDriver({m.name}).{fn}", msg)
      else void

    @expand
    info(m, fn, msg) = gen(@toplevel.Log.info, m, fn, msg)

    @expand
    debug(m, fn, msg) = gen(@toplevel.Log.debug, m, fn, msg)

    @expand
    error(m, fn, msg) = gen(@toplevel.Log.error, m, fn, msg)

  }}

  /** The MongoDB default port number **/
  default_port = 27017

  /* Free the buffer, return buffer for later use */
  // We'll keep the free calls in the code for now, they might be re-activated later
  // Knowing exactly where they go can be tricky
  @private free_(_) = void

  @package
  do_authenticate_ll(m : Mongo.db) =
    List.fold(
      (auth, outcome ->
        match outcome with
        | {success=_} ->
          match MongoCommands.authenticate_ll(m, m.name, auth.user, auth.password) with
          | {success=_} ->
            do Log.info(m, "authenticate", "success")
            outcome
          | {~failure} ->
            do Log.info(m, "authenticate", "fail: {failure}")
            {~failure}
          end
        | {~failure} -> {~failure}
      ), m.conf.auths, {success=m})

  /*
   * We have the possibility of unbounded recursion here since we
   * call MongoReplicaSet.connect, which calls us for ismaster.  Probably
   * won't ever be used but we limit the depth of the recursion.
   */
  @private
  doreconnect(from:string, m:Mongo.db): option(Mongo.db) =
    do Log.debug(m, "doreconnect", "depth={m.depth} allowslaveok={m.conf.slaveok}")
    if m.depth > m.max_depth
    then
      do Log.error(m, "reconnect","from '{from}': max depth exceeded")
      none
    else
      m = {m with depth=m.depth+1}
      rec aux(attempts) =
        if attempts > m.conf.attempt
        then none
        else
          (match MongoReplicaSet.connect(m) with
           | {success=m} ->
              match do_authenticate_ll(m) with
              | {success=m} ->
                do Log.info(m, "reconnect", "from '{from}: reconnected")
                {some=m}
              | {~failure} ->
                 do Log.info(m, "reconnect", "from '{from}: auth failure={C.string_of_failure(failure)}")
                 none
              end
           | {~failure} ->
              do Log.info(m, "reconnect", "from '{from}: failure={C.string_of_failure(failure)}")
              do Scheduler.wait(m.conf.waiting)
              aux(attempts+1))
        aux(0)

  @private
  reconfn(seeds, msg) =
    match msg with
    | {reconnect=(from,m)} ->
       match doreconnect(from,m) with
       | {some=nm} -> {return={reconnectresult={some=nm}}; instruction={set=nm.seeds}}
       | {none} -> {return={reconnectresult={none}}; instruction={unchanged}}
       end
    | {getseeds} -> {return={getseedsresult=seeds}; instruction={unchanged}}
    | {stop} -> {return={stopresult}; instruction={stop}}

  @private
  reconnect(from, m) =
    m =
      match (Cell.call(m.reconncell,({getseeds}:Mongo.reconnectmsg)):Mongo.reconnectresult) with
      | {getseedsresult=seeds} -> {m with seeds=MongoReplicaSet.mrg(seeds,m.seeds)}
      | res ->
        do Log.debug(m, "reconnect", "weird cell result {res}")
        m
      end
    match (Cell.call(m.reconncell,({reconnect=((from,m))}:Mongo.reconnectmsg)):Mongo.reconnectresult) with
    | {reconnectresult={none}} -> false
    | {reconnectresult={some=_}} ->
      true
    | _ -> @fail

  @private
  stoprecon(m) =
    match Cell.call(m.reconncell,({stop}:Mongo.reconnectmsg)):Mongo.reconnectresult with
    | {stopresult} -> void
    | _ -> @fail

  @private
  send_no_reply_(m:Mongo.db,mbuf:Mongo.mongo_buf,reply_expected): bool =
    match m.conn with
    | {some=conn} ->
      match WP.binary_export(mbuf) with
      | {success=binary} ->
         len = Binary.length(binary)
         do Log.debug(m, "send", "\n{WP.string_of_message_binary(binary)}")
         (match Socket.binary_write_len_with_err_cont(conn.conn,m.conf.timeout,binary,len) with
          | {success=cnt} ->
             do if not(reply_expected) then free_(mbuf) else void
             (cnt==len)
          | {failure=_} ->
             false)
      | {failure=_} ->
        do Log.error(m, "send", "Pack failure")
        false
      end
    | {none} ->
      do Log.error(m, "send", "No socket")
      false

  @private
  send_no_reply(m,mbuf:Mongo.mongo_buf): bool = send_no_reply_(m,mbuf,false)

  @private
  send_with_reply(m,mbuf:Mongo.mongo_buf): (option(Mailbox.t),option(Mongo.reply)) =
    mbuf = refresh_requestId(mbuf)
    mrid = reply_requestId(mbuf)
    match m.conn with
    | {some=conn} ->
       if send_no_reply_(m,mbuf,true)
       then
         match WP.read_mongo(conn.conn,m.conf.timeout,conn.mbox) with
         | {success=(mailbox,reply)} ->
            rrt = WP.reply_responseTo(reply)
            do free_(mbuf)
            do Log.debug(m, "receive", "\n{WP.string_of_Message(reply)}")
            if mrid != rrt
            then (
              do Log.error(m, "send_with_reply", "RequestId mismatch, expected {mrid}, got {rrt}")
              ({none},{none})
            ) else ({some=mailbox},{some=reply})
         | {~failure} ->
           do Log.info(m, "send_with_reply","failure={failure}")
           _ = Mailbox.reset(conn.mbox)
           ({none},{none})
         end
       else
         ({none},{none})
    | {none} ->
      do Log.error(m, "receive","No socket")
      ({none},{none})

  @private
  send_with_error(m,mbuf:Mongo.mongo_buf): (option(Mailbox.t),option(Mongo.reply)) =
    match m.conn with
    | {some=conn} ->
       mbuf2 = [WP.Query(0,if m.conf.slaveok then MongoCommon.SlaveOkBit else 0,m.name^".$cmd",[H.i32("getlasterror",1)],0,1,none)]
       mbuf2 = refresh_requestId(mbuf2)
       mrid = reply_requestId(mbuf2)
       if send_no_reply_(m,List.append(mbuf,mbuf2),true)
       then
         (match WP.read_mongo(conn.conn,m.conf.timeout,conn.mbox) with
          | {success=(mailbox,reply)} ->
             rrt = WP.reply_responseTo(reply)
             do free_(mbuf)
             do Log.debug(m, "send_with_error", "\n{WP.string_of_Message(reply)}")
             if mrid != rrt
             then
               do Log.error(m, "send_with_error",
               "RequestId mismatch, expected {Int.to_hex(mrid)}, got {Int.to_hex(rrt)}")
               ({none},{none})
             else ({some=mailbox},{some=reply})
          | {~failure} ->
             do Log.info(m, "send_with_error", "failure={failure}")
             _ = Mailbox.reset(conn.mbox)
             ({none},{none}))
       else
         ({none},{none})
    | {none} ->
       do Log.error(m, "send_with_error", "No socket")
       ({none},{none})


  @private
  sr_snr(m:Mongo.db,mbuf:Mongo.mongo_buf) =
    sr = send_no_reply(m,mbuf)
    ({none},if sr then {sendresult=sr} else {reconnect})

  @private
  sr_swr(m:Mongo.db,mbuf:Mongo.mongo_buf) =
    (mbox,swr) = send_with_reply(m,mbuf)
    (mbox,if Option.is_some(swr) && not(MongoCommon.reply_is_not_master(swr)) then {sndrcvresult=swr} else {reconnect})

  @private
  sr_swe(m,mbuf:Mongo.mongo_buf) =
    (mbox,swe) = send_with_error(m,mbuf)
    (mbox,if Option.is_some(swe) && not(MongoCommon.reply_is_not_master(swe)) then {snderrresult=swe} else {reconnect})

  @private appmbuf(mbuf:Mongo.mongo_buf,f) : Mongo.mongo_buf = match mbuf with | [h|t] -> [f(h)|t] | _ -> @fail
  @private getmbuf(mbuf:Mongo.mongo_buf,f:WireProtocol.Message->'a) : 'a = match mbuf with | [h|_] -> f(h) | _ -> @fail
  @private set_query_flags(mbuf:Mongo.mongo_buf,flags) = appmbuf(mbuf,WP.set_query_flags(_,flags))
  @private refresh_requestId(mbuf:Mongo.mongo_buf) = appmbuf(mbuf,WP.refresh_requestId)
  @private get_opCode(mbuf:Mongo.mongo_buf) = getmbuf(mbuf,WP.get_opCode)
  @private reply_requestId(mbuf:Mongo.mongo_buf) = getmbuf(mbuf,WP.reply_requestId)

  @private ssok(m, mbuf) = if m.conf.slaveok then set_query_flags(mbuf,MongoCommon.SlaveOkBit) else mbuf

  @private update_mbox(connection, mbox) =
    match mbox with
    | {some=mbox} -> {connection with ~mbox}
    | {none} -> connection
    end

  @private update_mongo(m, mbox) =
    match m.conn with
    | {some=connection} -> {m with conn={some=update_mbox(connection,mbox)}}
    | {none} -> m
    end

  @private ll_command_on_channel(m, cmd) =
    (mbox,result) = sr_swr(m,ssok(m, cmd))
    m = update_mongo(m, mbox)
    match result with
    | {noconnection} -> {failure={Error="ll_command_on_channel: no connection"}}
    | {reconnect} -> {failure={Error="ll_command_on_channel: can't reconnect"}}
    | {sndrcvresult={some={MsgBody={Reply={documents=[doc]; numberReturned=1; startingFrom=0; ...}}; ...}}} -> {success=(m,doc)}
    | _ -> {failure={Error="ll_command_on_channel: bad reply"}}
    end

  /* Dreadful hack.
   * If the socket pool decides to open a new socket we need to authenticate
   * it with mongo.  We use the authentication value stored in the socket pool
   * as a flag but we need to do the authentication outside of this driver's
   * normal computation path.  This function is a low-level authentication
   * routine for authenticating a newly opened connection without accessing
   * the socket pool.  It is not valid under concurrency but works only with
   * fresh sockets for which this routine has unique access which is guaranteed
   * by the enclosing cell.  Re-authenticating the socket later by the normal
   * channels does no harm.
   */
  @private authenticate_on_current_channel(m) =
    cmd = [WP.Query(0,0,"{m.name}.$cmd",[H.i32("getnonce",1)],0,1,{none})]
    match ll_command_on_channel(m, cmd) with
    | {success=(m,doc)} ->
      //do Ansi.jlog("doc: %g{doc}%d")
      if Bson.is_error(doc)
      then {failure={Error="authenticate_on_current_channel: bad nonce document"}}
      else
        match Bson.find_string(doc,"nonce") with
        | {some=nonce} ->
          //do Ansi.jlog("nonce=%c{nonce}%d")
          List.fold(
            (auth, outcome:outcome(Mongo.db,Mongo.failure) ->
              match outcome with
              | {success=m} -> {success=m}
              | {failure=_} ->
                digest = %%BslMisc.md5%%("{auth.user}:mongo:{auth.password}")
                hash = %%BslMisc.md5%%("{nonce}{auth.user}{digest}")
                cmd = [H.i32("authenticate",1), H.str("user",auth.user), H.str("nonce",nonce), H.str("key",hash)]
                cmd = [WP.Query(0,0,"{m.name}.$cmd",cmd,0,1,{none})]
                match ll_command_on_channel(m, cmd) with
                | {success=(m,doc)} ->
                  //do Ansi.jlog("doc: %g{doc}%d")
                  if Bson.is_error(doc)
                  then {failure={Error="authenticate_on_current_channel: authentication failure"}}
                  else {success=m}
                | {~failure} ->
                  do Log.info(m, "authenticate", "fail: {failure}")
                  {failure={Error="authenticate_on_current_channel: failure {failure}"}}
                end
              end), m.conf.auths, {failure={Error="authenticate_on_current_channel: no auths"}})
        | {none} -> {failure={Error="authenticate_on_current_channel: no nonce"}}
        end
    | {~failure} -> {failure={Error="authenticate_on_current_channel: failure {failure}"}}
    end

  @private check_auth(m:Mongo.db, authentication:SocketPool.auth) =
    match (m.conf.auths,authentication) with
    | ([],_) -> {success=(m, {authentication with authenticated=true})}
    | (_,{authenticated={true}; i1=_; i2=_}) -> {success=(m,authentication)}
    | _ ->
       match authenticate_on_current_channel(m) with
       | {success=m} -> {success=(m, {authentication with authenticated=true})}
       | {~failure} -> {~failure}
       end
    end

  @private
  srpool(m:Mongo.db,msg:Mongo.sr): Mongo.srr =
    match SocketPool.get_auth(m.pool) with
    | {success=(connection,authentication)} ->
       conn = {some=connection}
//       match check_auth({m with ~conn}, authentication) with
//       | {success=(m,authentication)} ->
         (mbox,result) =
           match msg with
           | {send=(m,mbuf)} -> sr_snr({m with ~conn},ssok(m,mbuf))
           | {sendrecv=(m,mbuf)} -> sr_swr({m with ~conn},ssok(m,mbuf))
           | {senderror=(m,mbuf)} -> sr_swe({m with ~conn},ssok(m,mbuf))
           | {stop} -> do Log.debug(m, "srpool","stop") @fail
         connection = update_mbox(connection, mbox)
         do SocketPool.release_auth(m.pool,connection,authentication)
         result
//       | {~failure} ->
//          do Log.error(m, "srpool","Can't authenticate {failure}")
//          {noconnection}
//       end
    | {~failure} ->
       do Log.error(m, "srpool","Can't get pool {failure}")
       if m.conf.attempt == 0
       then {noconnection}
       else {reconnect}
    end

  @private reconnect_and_continue(m, mbuf, f, default) =
    if reconnect("send_no_reply", m) then f(m, mbuf)
    else default

  @private
  snd(m,mbuf:Mongo.mongo_buf) =
    match srpool(m,{send=((m,mbuf))}) with
    | {noconnection} -> false
    | {reconnect} -> reconnect_and_continue(m, mbuf, snd, false)
    | {~sendresult} -> sendresult
    | _ -> @fail

  @private
  sndrcv(m,mbuf:Mongo.mongo_buf) =
    match srpool(m,{sendrecv=((m,mbuf))}) with
    | {noconnection} -> none
    | {reconnect} -> reconnect_and_continue(m, mbuf, sndrcv, none)
    | {~sndrcvresult} -> sndrcvresult
    | _ -> @fail

  @private
  snderr(m,mbuf:Mongo.mongo_buf) =
    match srpool(m,{senderror=((m,mbuf))}) with
    | {noconnection} -> none
    | {reconnect} -> reconnect_and_continue(m, mbuf, snderr, none)
    | {~snderrresult} -> snderrresult
    | _ -> @fail

  /**
   * Default configuration
   */
  default_conf = MongoConf.default.conf

  /**
   * Initialize a database connection.
   * @param name The name of the database to open
   * @param conf The configuration of the database connection to open. See
   * [default_conf].
   * @return A database connection
   **/
  init(name, conf:Mongo.conf): Mongo.db =
    ~{conf seeds} = MongoConf.get(conf, name)
    {
      ~name; ~conf ~seeds;
      conn={none};
      reconncell=(Cell.make([], reconfn):Cell.cell(Mongo.reconnectmsg,Mongo.reconnectresult));
      pool=SocketPool.make(("localhost",default_port),
        {max=conf.poolmax hint=conf.bufsize verbose = conf.verbose});
      depth=0;
      max_depth=2;
    }

  /**
   * Connect to the MongoDB server on [host:port].
   * Close any existing connection.
   * We should really check if we are master but that would get complicated
   * since this routine gets called from within reconnect.
   * The caller should verify the master status.
   * Example: [connect(m,addr,port)]
   * @param m A [Mongo.db] value, initialised by [init].
   * @param addr Address of the MongoDB server.
   * @param port Port number for the MongoDB server.
   **/
  connect(m:Mongo.db, addr:string, port:int): outcome(Mongo.db,Mongo.failure) =
    do Log.info(m, "connect","addr={addr} port={port}")
    do SocketPool.reconnect(m.pool,(addr,port))
    {success=m}

  /**
   * Convenience function, [initialize] and [connect] at the same time.
   * @param name The name of the database to open
   * @param conf The configuration of the database connection to open. See
   * [default_conf].
   * @param addr Address of the MongoDB server.
   * @param port Port number for the MongoDB server.
   * @return A database connection.
   */
  open(name, conf, addr, port) : outcome(Mongo.db,Mongo.failure) =
    m = init(name, conf)
    do Log.info(m, "open","{addr}:{port} auth={conf.auths}")
    match (connect(m, addr, port), conf.auths) with
    | ({~success},[]) -> {~success}
    | ({~success},_) -> do_authenticate_ll(success)
    | ({~failure},_) -> {~failure}

  /**
   * Check if the database connection is ready
   * @param m The database connection to check
   * @return
   */
  check(m:Mongo.db) =
    match SocketPool.get_auth(m.pool) with
    | ~{failure} -> {failure={Error=failure}}
    | {success=(c,a)} ->
      do SocketPool.release_auth(m.pool, c, a)
      {success=m}

  /**
   * Force a reconnection.  Should only be needed if the basic parameters have changed.
   *
   * Note that we can only reconnect to a replica set and that a failed reconnect is considered a fatal error.
   **/
  force_reconnect(m:Mongo.db): void =
    if m.conf.attempt > 0
    then
      do Log.info(m, "force_reconnect","{SocketPool.gethost(m.pool)}")
      if not(reconnect("force_reconnect",m))
      then Log.error(m, "force_reconnect","comms error (Can't reconnect)")
      else void
    else
      Log.info(m, "force_reconnect","connection is not reconnectable")

  /**
   *  Close mongo connection.
   **/
  /* Note: this may happen asynchronously with other tasks so we
   * have to be careful how we access the connection value.
   */
  close(m:Mongo.db): Mongo.db =
    do Log.info(m, "close","{SocketPool.gethost(m.pool)}")
    do SocketPool.stop(m.pool)
    m

  /**
   *  Send OP_INSERT with given collection name.
   *  MongoDB doesn't send any reply.
   *  Example: [insert(m, flags, ns, document)]
   *  @param m Mongo.db value
   *  @param flags Int value with MongoDB-defined bits
   *  @param ns MongoDB namespace
   *  @param document A Bson.document value to store in the DB
   *  @return a bool indicating whether the message was successfully sent or not.
   **/
  insert(m:Mongo.db, flags:int, ns:string, documents:Bson.document): bool =
    snd(m,[WP.Insert(0, flags, ns, [documents])])

  /**
   * Same as insert but piggyback a getlasterror command.
   **/
  inserte(m:Mongo.db, flags:int, ns:string, documents:Bson.document): option(Mongo.reply) =
    snderr(m,[WP.Insert(0, flags, ns, [documents])])

  /**
   *  [insertf]:  same as [insert] but using tags instead of bit-wise flags.
   **/
  insertf(m:Mongo.db, tags:list(Mongo.insert_tag), ns:string, documents:Bson.document): bool =
    flags = C.flags(List.map((t -> {itag=t}),tags))
    insert(m,flags,ns,documents)

  /**
   *  Send OP_INSERT with given collection name and multiple documents.
   *  Same parameters as for [insert].
   **/
  insert_batch(m:Mongo.db, flags:int, ns:string, documents:list(Bson.document)): bool =
    snd(m,[WP.Insert(0, flags, ns, documents)])

  /** insert_batch with added getlasterror query **/
  insert_batche(m:Mongo.db, flags:int, ns:string, documents:list(Bson.document)): option(Mongo.reply) =
    snderr(m,[WP.Insert(0, flags, ns, documents)])

  /**
   *  [insert_batchf]:  same as [insert_batch] but using tags instead of bit-wise flags.
   **/
  insert_batchf(m:Mongo.db, tags:list(Mongo.insert_tag), ns:string, documents:list(Bson.document)): bool =
    flags = C.flags(List.map((t -> {itag=t}),tags))
    insert_batch(m,flags,ns,documents)

  /**
   *  Send OP_UPDATE with given collection name.
   *  Example: [update(m,flags,ns,selector,update)]
   *  Same parameters and result as for [insert] except we also
   *  provide a [select] document.
   **/
  update(m:Mongo.db, flags:int, ns:string, selector:Bson.document, update:Bson.document): bool =
    snd(m,[WP.Update(0,flags,ns,selector,update)])

  /** update with added getlasterror query **/
  updatee(m:Mongo.db, flags:int, ns:string, selector:Bson.document, update:Bson.document): option(Mongo.reply) =
    snderr(m,[WP.Update(0,flags,ns,selector,update)])

  /**
   *  [updatef]:  same as [update] but using tags instead of bit-wise flags.
   **/
  updatef(m:Mongo.db, tags:list(Mongo.update_tag), ns:string, selector:Bson.document, update_doc:Bson.document): bool =
    flags = C.flags(List.map((t -> {utag=t}),tags))
    update(m,flags,ns,selector,update_doc)

  /**
   *  Send OP_QUERY and get reply.
   *  Example: [query(m, flags, ns, numberToSkip, numberToReturn, query, returnFieldSelector_opt)]
   *  @return reply The return value is an optional reply, this is an external type
   *  you need the functions in [MongoDriver], [reply_] etc. to handle this.
   **/
  query(m:Mongo.db, flags:int, ns:string, numberToSkip:int, numberToReturn:int,
        query:Bson.document, returnFieldSelector_opt:option(Bson.document)): option(Mongo.reply) =
    sndrcv(m,[WP.Query(0,flags,ns,query,numberToSkip,numberToReturn,returnFieldSelector_opt)])

  /**
   *  [queryf]:  same as [query] but using tags instead of bit-wise flags.
   **/
  queryf(m:Mongo.db, tags:list(Mongo.query_tag), ns:string, numberToSkip:int, numberToReturn:int,
         query_doc:Bson.document, returnFieldSelector_opt:option(Bson.document)): option(Mongo.reply) =
    flags = C.flags(List.map((t -> {qtag=t}),tags))
    query(m,flags,ns,numberToSkip,numberToReturn,query_doc,returnFieldSelector_opt)

  /**
   *  Send OP_GETMORE and get reply.
   *  Example: [get_more(m, ns, numberToReturn, cursorID)]
   *  @param cursorID You need to get the [cursorID] from a previous reply value.
   *  @return Exactly the same as [query].
   **/
  get_more(m:Mongo.db, ns:string, numberToReturn:int, cursorID:Mongo.cursorID): option(Mongo.reply) =
    sndrcv(m,[WP.GetMore(0,ns,numberToReturn,cursorID)])

  /**
   *  Send OP_DELETE.
   *  Example: [delete(m, ns, selector)]
   *  @return a bool indicating whether the message was successfully sent or not.
   **/
  delete(m:Mongo.db, flags:int, ns:string, selector:Bson.document): bool =
    snd(m,[WP.Delete(0,flags,ns,selector)])

  /** delete with added getlasterror query **/
  deletee(m:Mongo.db, flags:int, ns:string, selector:Bson.document): option(Mongo.reply) =
    snderr(m,[WP.Delete(0,flags,ns,selector)])

  /**
   *  [deletef]:  same as [delete] but using tags instead of bit-wise flags.
   **/
  deletef(m:Mongo.db, tags:list(Mongo.delete_tag), ns:string, selector:Bson.document): bool =
    flags = C.flags(List.map((t -> {dtag=t}),tags))
    delete(m,flags,ns,selector)

  /**
   *  Send OP_KILL_CURSORS.
   *  @return a bool indicating whether the message was successfully sent or not.
   **/
  kill_cursors(m:Mongo.db, cursors:list(Mongo.cursorID)): bool =
    snd(m,[WP.KillCursors(0,cursors)])

  /** kill_cursors with added getlasterror query **/
  kill_cursorse(m:Mongo.db, cursors:list(Mongo.cursorID)): option(Mongo.reply) =
    snderr(m,[WP.KillCursors(0,cursors)])

  /**
   *  Send OP_MSG.
   *  @return a bool indicating whether the message was successfully sent or not.
   **/
  msg(m:Mongo.db, msg:string): bool =
    snd(m,[WP.Msg(0,msg)])

  /** kill_cursors with added getlasterror query **/
  msge(m:Mongo.db, msg:string): option(Mongo.reply) =
    snderr(m,[WP.Msg(0,msg)])

  @private get_index_opts(options:int): Bson.document =
    List.flatten([(if Bitwise.land(options,C.UniqueBit) != 0 then [H.bool("unique",true)] else []),
                  (if Bitwise.land(options,C.DropDupsBit) != 0 then [H.bool("dropDups",true)] else []),
                  (if Bitwise.land(options,C.BackgroundBit) != 0 then [H.bool("background",true)] else []),
                  (if Bitwise.land(options,C.SparseBit) != 0 then [H.bool("sparse",true)] else [])])

  @private create_index_(ns:string, key:Bson.document, opts:Bson.document) =
    keys = Bson.keys(key)
    name = "_"^(String.concat("",keys))
    b = List.flatten([[H.doc("key",key), H.str("ns",ns), H.str("name",name)],opts])
    idxns=(match String.index(".",ns) with | {some=p} -> String.substring(0,p,ns) | {none} -> ns)^".system.indexes"
    (b,idxns)

  /**
   * Add an index to a collection.
   * Example: [create_index(mongo, "ns", key, flags)]
   * @param [key] is a bson object defining the fields to be indexed, eg. [\[\{Int32=("age",1)\}, \{Int32=("name",1)\}\]]
   **/
  create_index(m:Mongo.db, ns:string, key:Bson.document, options:int): bool =
    opts = get_index_opts(options)
    (b,idxns) = create_index_(ns, key, opts)
    insert(m,0,idxns,b)

  /** Add an index to a collection with follow-up getlasterror **/
  create_indexe(m:Mongo.db, ns:string, key:Bson.document, options:int): option(Mongo.reply) =
    opts = get_index_opts(options)
    (b,idxns) = create_index_(ns, key, opts)
    inserte(m,0,idxns,b)

  /**
   * [create_indexf]:  same as [create_index] but using tags instead of bit-wise flags.
   **/
  create_indexf(m:Mongo.db, ns:string, key:Bson.document, tags:list(Mongo.index_tag)): bool =
    opts =
      List.map((t ->
                 match t with
                 | {Unique} -> H.bool("unique",true)
                 | {DropDups} -> H.bool("dropDups",true)
                 | {Background} -> H.bool("background",true)
                 | {Sparse} -> H.bool("sparse",true)),tags)
    (b,idxns) = create_index_(ns, key, opts)
    insert(m,0,idxns,b)

  /**
   * Simpler version of the [create_index] function, for a single named field.
   **/
  create_simple_index(m:Mongo.db, ns:string, field:string, options:int): bool =
    create_index(m, ns, [H.i32(field,1)], options)

  to_iterator(m:Mongo.db, ns:string, reply:Mongo.reply):iter(Bson.document) =
    rec aux(i, size, reply) = (
      if i < size then
        match MongoCommon.reply_document(reply, i) with
        | {none} -> @fail("Unexpected error: can't retreive document {i}/{size}")
        | {some=doc} -> {some = (doc, {next = -> aux(i+1, size, reply)})}
      else
        cursor = MongoCommon.reply_cursorID(reply)
        if MongoCommon.is_null_cursorID(cursor) then {none}
        else
          match MongoDriver.get_more(m, ns, 0, cursor) with
          | {none} -> @fail("Can't get more document from cursor({MongoCommon.string_of_cursorID(cursor)})")
          | {some=reply} -> aux(0, MongoCommon.reply_numberReturned(reply), reply)
    )
    { next = -> aux(0, MongoCommon.reply_numberReturned(reply), reply) }

}}
