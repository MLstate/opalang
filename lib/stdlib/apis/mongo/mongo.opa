/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
import-plugin mongo
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
import stdlib.crypto
import stdlib.system

@private WP = WireProtocol

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
  conn : option(Socket.socket);
  reconncell : Cell.cell(Mongo.reconnectmsg,Mongo.reconnectresult);
  pool : SocketPool.t;
  pool_max : int;
  allow_slaveok : bool;
  bufsize : int;
  log : bool;
  name : string;
  seeds : list(Mongo.mongo_host);
  reconnect_wait : int;
  max_attempts : int;
  comms_timeout : int;
  reconnectable : bool;
  depth : int;
  max_depth : int;
  auth : Mongo.auths;
}

/** Outgoing Cell messages **/
@private
type Mongo.sr =
    {send:(Mongo.db,Mongo.mongo_buf,string)} // Send and forget
  / {sendrecv:(Mongo.db,Mongo.mongo_buf,string)} // Send and expect reply
  / {senderror:(Mongo.db,Mongo.mongo_buf,string,string)} // Send and call getlasterror
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
   {reconnectresult:option((bool,Mongo.db))}
 / {getseedsresult:list(Mongo.mongo_host)}
 / {stopresult}

MongoDriver = {{

  @private ML = MongoLog
  @private H = Bson.Abbrevs
  @private C = MongoCommon

  // Debug routines
  @private memdump = (%% BslPervasives.memdump %%: string -> string)
  @private bindump = (%% BslPervasives.bindump %%: binary -> string)

  /** The MongoDB default port number **/
  default_port = 27017

  /* Free the buffer, return buffer for later use */
  // We'll keep the free calls in the code for now, they might be re-activated later
  // Knowing exactly where they go can be tricky
  @private free_(_) = void

  do_authenticate_ll(db:outcome(Mongo.db,Mongo.failure)) =
    match db with
    | {success=m} ->
      List.fold((auth, outcome ->
                  match outcome with
                  | {success=_} ->
                     match MongoCommands.authenticate_ll(m, auth.dbname, auth.user, auth.password) with
                     | {success=_} ->
                        do if m.log then ML.info("MongoDriver.authenticate","authenticate success",void)
                        outcome
                     | {~failure} ->
                        do if m.log then ML.info("MongoDriver.authenticate","authenticate fail {failure}",void)
                        {~failure}
                     end
                  | {~failure} -> {~failure}
                ),m.auth,{success=m})
    | {~failure} -> {~failure}

  /*
   * We have the possibility of unbounded recursion here since we
   * call MongoReplicaSet.connect, which calls us for ismaster.  Probably
   * won't ever be used but we limit the depth of the recursion.
   */
  @private
  doreconnect(from:string, m:Mongo.db): option((bool,Mongo.db)) =
    do if m.log then ML.debug("MongoDriver.doreconnect","depth={m.depth} allowslaveok={m.allow_slaveok}",void)
    if m.depth > m.max_depth
    then
      do if m.log then ML.error("MongoDriver.reconnect({from})","max depth exceeded",void)
      none
    else
      m = {m with depth=m.depth+1}
      if m.reconnectable
      then
        rec aux(attempts) =
          if attempts > m.max_attempts
          then none
          else
            (match MongoReplicaSet.connect(m) with
             | {success=(slaveok,m)} ->
                match do_authenticate_ll({success=m}) with
                | {success=m} ->
                  do if m.log then ML.info("MongoDriver.reconnect({from})","reconnected",void)
                  {some=(slaveok,m)}
                | {~failure} ->
                   do if m.log then ML.info("MongoDriver.reconnect({from})","auth failure={C.string_of_failure(failure)}",void)
                   none
                end
             | {~failure} ->
                do if m.log then ML.info("MongoDriver.reconnect({from})","failure={C.string_of_failure(failure)}",void)
                do Scheduler.wait(m.reconnect_wait)
                aux(attempts+1))
        aux(0)
      else
        none

  @private
  reconfn(seeds, msg) =
    match msg with
    | {reconnect=(from,m)} ->
       match doreconnect(from,m) with
       | {some=(slaveok,nm)} -> {return={reconnectresult={some=(slaveok,nm)}}; instruction={set=nm.seeds}}
       | {none} -> {return={reconnectresult={none}}; instruction={unchanged}}
       end
    | {getseeds} -> {return={getseedsresult=seeds}; instruction={unchanged}}
    | {stop} -> {return={stopresult}; instruction={stop}}

  @private
  reconnect(from, m) =
    m =
      match (Cell.call(m.reconncell,({getseeds}:Mongo.reconnectmsg)):Mongo.reconnectresult) with
      | {getseedsresult=seeds} -> {m with seeds=MongoReplicaSet.mrg(seeds,m.seeds)}
      | res -> do ML.debug("MongoDriver.reconnect","weird cell result {res}",void) m
      end
    do if m.log then ML.debug("MongoDriver.reconnect","m={m}",void)
    match (Cell.call(m.reconncell,({reconnect=((from,m))}:Mongo.reconnectmsg)):Mongo.reconnectresult) with
    | {reconnectresult={none}} -> (false,false)
    | {reconnectresult={some=(slaveok,_)}} ->
       do SocketPool.setslaveok(m.pool,slaveok)
       (slaveok,true)
    | res -> do ML.debug("MongoDriver.reconnect","fail (result={res})",void) @fail

  @private
  stoprecon(m) =
    match Cell.call(m.reconncell,({stop}:Mongo.reconnectmsg)):Mongo.reconnectresult with
    | {stopresult} -> void
    | _ -> @fail

  @private
  send_no_reply_(m,mbuf:Mongo.mongo_buf,name,reply_expected): bool =
    match m.conn with
    | {some=conn} ->
       (match WP.binary_export(mbuf) with
        | {success=binary} ->
           len = Binary.length(binary)
           do if m.log then ML.debug("MongoDriver.send({name})","\n{WP.string_of_message_binary(binary)}",void)
           (match Socket.binary_write_len_with_err_cont(conn.f2,m.comms_timeout,binary,len) with
            | {success=cnt} ->
               do if not(reply_expected) then free_(mbuf) else void
               (cnt==len)
            | {failure=_} ->
               false)
        | {failure=_} -> ML.error("MongoDriver.send({name})","Pack failure",false))
    | {none} ->
       ML.error("MongoDriver.send({name})","No socket",false)

  @private
  send_no_reply(m,mbuf:Mongo.mongo_buf,name): bool = send_no_reply_(m,mbuf,name,false)

  @private
  send_with_reply(m,mbuf:Mongo.mongo_buf,name): (option(Mailbox.t),option(Mongo.reply)) =
    mbuf = refresh_requestId(mbuf)
    mrid = reply_requestId(mbuf)
    match m.conn with
    | {some=conn} ->
       if send_no_reply_(m,mbuf,name,true)
       then
         //mailbox = Mailbox.create(m.bufsize)
         (match WP.read_mongo(conn.f2,m.comms_timeout,conn.f1) with
          | {success=(mailbox,reply)} ->
             rrt = WP.reply_responseTo(reply)
             //m = {m with conn={some=(mailbox,conn.f2)}}
             //_ = Mailbox.reset(mailbox)
             do free_(mbuf)
             do if m.log then ML.debug("MongoDriver.receive({name})","\n{WP.string_of_Message(reply)}",void)
             if mrid != rrt
             then ML.error("MongoDriver.send_with_reply","RequestId mismatch, expected {mrid}, got {rrt}",({none},{none}))
             else ({some=mailbox},{some=reply})
          | {~failure} ->
             do if m.log then ML.info("send_with_reply","failure={failure}",void)
             _ = Mailbox.reset(conn.f1)
             ({none},{none}))
       else
         ({none},{none})
    | {none} ->
       ML.error("MongoDriver.receive({name})","No socket",({none},{none}))

  @private
  send_with_error(m,mbuf:Mongo.mongo_buf,name,ns,slaveok): (option(Mailbox.t),option(Mongo.reply)) =
    match m.conn with
    | {some=conn} ->
       mbuf2 = [WP.Query(0,if slaveok then MongoCommon.SlaveOkBit else 0,ns^".$cmd",[H.i32("getlasterror",1)],0,1,none)]
       mbuf2 = refresh_requestId(mbuf2)
       mrid = reply_requestId(mbuf2)
       if send_no_reply_(m,List.append(mbuf,mbuf2),name,true)
       then
         //mailbox = Mailbox.create(m.bufsize)
         (match WP.read_mongo(conn.f2,m.comms_timeout,conn.f1) with
          | {success=(mailbox,reply)} ->
             rrt = WP.reply_responseTo(reply)
             //m = {m with conn={some=(mailbox,conn.f2)}}
             //_ = Mailbox.reset(mailbox)
             do free_(mbuf)
             do if m.log then ML.debug("MongoDriver.send_with_error({name})","\n{WP.string_of_Message(reply)}",void)
             if mrid != rrt
             then
               do ML.error("MongoDriver.send_with_error",
                           "RequestId mismatch, expected {Int.to_hex(mrid)}, got {Int.to_hex(rrt)}",void)
               ({none},{none})
             else ({some=mailbox},{some=reply})
          | {~failure} ->
             do if m.log then ML.info("send_with_error","failure={failure}",void)
             _ = Mailbox.reset(conn.f1)
             ({none},{none}))
       else
         ({none},{none})
    | {none} ->
       ML.error("MongoDriver.send_with_error({name})","No socket",({none},{none}))


  @private
  sr_snr(m,mbuf:Mongo.mongo_buf,name) =
    sr = send_no_reply(m,mbuf,name)
    ({none},if sr then {sendresult=sr} else {reconnect})

  @private
  sr_swr(m,mbuf:Mongo.mongo_buf,name) =
    (mbox,swr) = send_with_reply(m,mbuf,name)
    (mbox,if Option.is_some(swr) && not(MongoCommon.reply_is_not_master(swr)) then {sndrcvresult=swr} else {reconnect})

  @private
  sr_swe(m,mbuf:Mongo.mongo_buf,name,ns,slaveok) =
    (mbox,swe) = send_with_error(m,mbuf,name,ns,slaveok)
    (mbox,if Option.is_some(swe) && not(MongoCommon.reply_is_not_master(swe)) then {snderrresult=swe} else {reconnect})

  check(m:Mongo.db) =
    match SocketPool.get(m.pool) with
    | ~{failure} -> ~{failure}
    | {success=(_,c)} -> do SocketPool.release(m.pool, c) {success = m}

  @private appmbuf(mbuf:Mongo.mongo_buf,f) : Mongo.mongo_buf = match mbuf with | [h|t] -> [f(h)|t] | _ -> @fail
  @private getmbuf(mbuf:Mongo.mongo_buf,f:WireProtocol.Message->'a) : 'a = match mbuf with | [h|_] -> f(h) | _ -> @fail
  @private set_query_flags(mbuf:Mongo.mongo_buf,flags) = appmbuf(mbuf,WP.set_query_flags(_,flags))
  @private refresh_requestId(mbuf:Mongo.mongo_buf) = appmbuf(mbuf,WP.refresh_requestId)
  @private get_opCode(mbuf:Mongo.mongo_buf) = getmbuf(mbuf,WP.get_opCode)
  @private reply_requestId(mbuf:Mongo.mongo_buf) = getmbuf(mbuf,WP.reply_requestId)

  @private
  srpool(m:Mongo.db,msg:Mongo.sr): Mongo.srr =
    match SocketPool.get(m.pool) with
    | {success=(slaveok,connection)} ->
       conn = {some=connection}
       ssok(mbuf) = if slaveok then set_query_flags(mbuf,MongoCommon.SlaveOkBit) else mbuf
       (mbox,result) =
         (match msg with
          | {send=(m,mbuf,name)} -> sr_snr({m with ~conn},ssok(mbuf),name)
          | {sendrecv=(m,mbuf,name)} -> sr_swr({m with ~conn},ssok(mbuf),name)
          | {senderror=(m,mbuf,name,ns)} -> sr_swe({m with ~conn},ssok(mbuf),name,ns,slaveok)
          | {stop} -> do ML.debug("MongoDriver.srpool","stop",void) @fail)
       connection =
         match mbox with
         | {some=mbox} -> (mbox,connection.f2)
         | {none} -> connection
       do SocketPool.release(m.pool,connection)
       result
    | {~failure} ->
       do if m.log then ML.error("MongoDriver.srpool","Can't get pool {C.string_of_failure(failure)}",void)
       if (failure == {Error="Got exception Scheduler.Connection_closed"})
       then {noconnection}
       else {reconnect}

  @private
  snd(m,mbuf:Mongo.mongo_buf,name) =
    recon() =
      mbuf2 = mbuf
      (slaveok,connectok) = reconnect("send_no_reply",m)
      if connectok
      then
        if not(slaveok) || (get_opCode(mbuf2) == MongoCommon._OP_QUERY)
        then
          mbuf2 = refresh_requestId(mbuf2) // Probably not necessary but we don't want unnecessary confusion
          snd(m,mbuf2,name)
        else false
      else ML.fatal("MongoDriver.snd({name}):","comms error (Can't reconnect)",-1)
    srr = srpool(m,{send=((m,mbuf,name))})
    match srr with
    | {noconnection} -> false
    | {reconnect} -> recon()
    | {~sendresult} -> sendresult
    | _ -> @fail

  @private
  sndrcv(m,mbuf:Mongo.mongo_buf,name) =
    recon() =
      mbuf2 = mbuf
      (slaveok,connectok) = reconnect("send_with_reply",m)
      if connectok
      then
        if not(slaveok) || (get_opCode(mbuf2) == MongoCommon._OP_QUERY)
        then
          mbuf2 = refresh_requestId(mbuf2)
          sndrcv(m,mbuf2,name)
        else none
      else ML.fatal("MongoDriver.sndrcv({name}):","comms error (Can't reconnect)",-1)
    srr = srpool(m,{sendrecv=((m,mbuf,name))})
    match srr with
    | {noconnection} -> none
    | {reconnect} -> recon()
    | {~sndrcvresult} -> sndrcvresult
    | _ -> @fail

  @private
  snderr(m,mbuf:Mongo.mongo_buf,name,ns) =
    recon() =
      mbuf2 = mbuf
      (slaveok,connectok) = reconnect("send_with_error",m)
      if connectok
      then
        if not(slaveok) || (get_opCode(mbuf2) == MongoCommon._OP_QUERY)
        then
          mbuf2 = refresh_requestId(mbuf2)
          snderr(m,mbuf2,name,ns)
        else none
      else ML.fatal("MongoDriver.snderr({name}):","comms error (Can't reconnect)",-1)
    srr = srpool(m,{senderror=((m,mbuf,name,ns))})
    match srr with
    | {noconnection} -> none
    | {reconnect} -> recon()
    | {~snderrresult} -> snderrresult
    | _ -> @fail

  /**
   * Due to the number of parameters we have a separate [init] routine
   * from [connect].  This feature is mostly used by replica set connection
   * and re-connection.
   * Example: [init(bufsize, pool_max, reconnectable, log)]
   * @param bufsize A hint to the driver for the initial buffer size.
   * @param pool_max For pool the number of open sockets per pool.
   * @param reconnectable Flag to enable reconnection logic.
   * @param log Whether to enable logging for the driver.
   **/
  init(bufsize:int, pool_max:int, allow_slaveok:bool, reconnectable:bool, log:bool, auth:Mongo.auths): Mongo.db =
    { conn={none};
      reconncell=(Cell.make([], reconfn):Cell.cell(Mongo.reconnectmsg,Mongo.reconnectresult));
      pool=SocketPool.make(("localhost",default_port),pool_max,bufsize,log);
      pool_max=Int.max(pool_max,1); ~allow_slaveok; ~bufsize; ~log;
      seeds=[]; name=""; ~reconnectable;
      reconnect_wait=2000; max_attempts=30; comms_timeout=3600000;
      depth=0; max_depth=2; ~auth;
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
    do if m.log then ML.info("MongoDriver.connect","bufsize={m.bufsize} addr={addr} port={port} log={m.log}",void)
    do SocketPool.reconnect(m.pool,(addr,port))
    {success=m}

  /**
   * Get the current SlaveOK status.
   **/
  get_slaveok(m:Mongo.db): bool = SocketPool.getslaveok(m.pool)

  /**
   * Force a reconnection.  Should only be needed if the basic parameters have changed.
   *
   * Note that we can only reconnect to a replica set and that a failed reconnect is considered a fatal error.
   **/
  force_reconnect(m:Mongo.db): void =
    if m.reconnectable
    then
      do if m.log then ML.info("MongoDriver.force_reconnect","{SocketPool.gethost(m.pool)}",void)
      (_slaveok, connectok) = reconnect("force_reconnect",m)
      if not(connectok)
      then ML.fatal("MongoDriver.force_reconnect:","comms error (Can't reconnect)",-1)
      else void
    else
      ML.warning("MongoDriver.force_reconnect","connection is not reconnectable",void)

  /**
   *  Convenience function, initialise and connect at the same time.
   *
   *  Example: [open(bufsize, pool_max, reconnectable, addr, port, log)]
   **/
  open(bufsize:int, pool_max:int, reconnectable:bool, allow_slaveok:bool, addr:string, port:int, log:bool, auth:Mongo.auths)
     : outcome(Mongo.db,Mongo.failure) =
    do if log then ML.info("MongoDriver.open","{addr}:{port} auth={auth}",void)
    match ((connect(init(bufsize,pool_max,allow_slaveok,reconnectable,log,auth),addr,port),auth)) with
    | ({~success},[]) -> {~success}
    | ({~success},_) -> do_authenticate_ll({~success})
    | ({~failure},_) -> {~failure}

  /**
   *  Close mongo connection.
   **/
  /* Note: this may happen asynchronously with other tasks so we
   * have to be careful how we access the connection value.
   */
  close(m:Mongo.db): Mongo.db =
    do if m.log then ML.info("MongoDriver.close","{SocketPool.gethost(m.pool)}",void)
    do SocketPool.stop(m.pool)
    m

  /**
   * Allow the user to update with the basic communications parameters.
   *
   * Note that some of these will have no effect until reconnection.
   **/
  set_pool_max(m:Mongo.db, pool_max:int): Mongo.db = { m with pool_max=Int.max(pool_max,1) }
  set_bufsize(m:Mongo.db, bufsize:int): Mongo.db = { m with ~bufsize }
  set_log(m:Mongo.db, log:bool): Mongo.db = { m with ~log }
  add_seed(m:Mongo.db, seed:Mongo.mongo_host): Mongo.db = { m with seeds=seed +> m.seeds }
  remove_seed(m:Mongo.db, seed:Mongo.mongo_host): Mongo.db = { m with seeds=List.filter((s -> s != seed),m.seeds) }
  set_reconnect_wait(m:Mongo.db, reconnect_wait:int): Mongo.db = { m with ~reconnect_wait }
  set_max_attempts(m:Mongo.db, max_attempts:int): Mongo.db = { m with ~max_attempts }
  set_comms_timeout(m:Mongo.db, comms_timeout:int): Mongo.db = { m with ~comms_timeout }

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
    snd(m,[WP.Insert(0, flags, ns, [documents])],"insert")

  /**
   * Same as insert but piggyback a getlasterror command.
   **/
  inserte(m:Mongo.db, flags:int, ns:string, dbname:string, documents:Bson.document): option(Mongo.reply) =
    snderr(m,[WP.Insert(0, flags, ns, [documents])],"insert",dbname)

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
    snd(m,[WP.Insert(0, flags, ns, documents)],"insert")

  /** insert_batch with added getlasterror query **/
  insert_batche(m:Mongo.db, flags:int, ns:string, dbname:string, documents:list(Bson.document)): option(Mongo.reply) =
    snderr(m,[WP.Insert(0, flags, ns, documents)],"insert",dbname)

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
    snd(m,[WP.Update(0,flags,ns,selector,update)],"update")

  /** update with added getlasterror query **/
  updatee(m:Mongo.db, flags:int, ns:string, dbname, selector:Bson.document, update:Bson.document): option(Mongo.reply) =
    snderr(m,[WP.Update(0,flags,ns,selector,update)],"update",dbname)

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
    //do ML.debug("MongoDriver.query","ns={ns} numberToSkip={numberToSkip} numberToReturn={numberToReturn} query={query}",void)
    sndrcv(m,[WP.Query(0,flags,ns,query,numberToSkip,numberToReturn,returnFieldSelector_opt)],"query")

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
    sndrcv(m,[WP.GetMore(0,ns,numberToReturn,cursorID)],"getmore")

  /**
   *  Send OP_DELETE.
   *  Example: [delete(m, ns, selector)]
   *  @return a bool indicating whether the message was successfully sent or not.
   **/
  delete(m:Mongo.db, flags:int, ns:string, selector:Bson.document): bool =
    snd(m,[WP.Delete(0,flags,ns,selector)],"delete")

  /** delete with added getlasterror query **/
  deletee(m:Mongo.db, flags:int, ns:string, dbname:string, selector:Bson.document): option(Mongo.reply) =
    snderr(m,[WP.Delete(0,flags,ns,selector)],"delete",dbname)

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
    snd(m,[WP.KillCursors(0,cursors)],"kill_cursors")

  /** kill_cursors with added getlasterror query **/
  kill_cursorse(m:Mongo.db, dbname:string, cursors:list(Mongo.cursorID)): option(Mongo.reply) =
    snderr(m,[WP.KillCursors(0,cursors)],"kill_cursors",dbname)

  /**
   *  Send OP_MSG.
   *  @return a bool indicating whether the message was successfully sent or not.
   **/
  msg(m:Mongo.db, msg:string): bool =
    snd(m,[WP.Msg(0,msg)],"msg")

  /** kill_cursors with added getlasterror query **/
  msge(m:Mongo.db, dbname:string, msg:string): option(Mongo.reply) =
    snderr(m,[WP.Msg(0,msg)],"msg",dbname)

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
  create_indexe(m:Mongo.db, ns:string, dbname:string, key:Bson.document, options:int): option(Mongo.reply) =
    opts = get_index_opts(options)
    (b,idxns) = create_index_(ns, key, opts)
    inserte(m,0,idxns,dbname,b)

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

}}

// End of file mongo.opa
