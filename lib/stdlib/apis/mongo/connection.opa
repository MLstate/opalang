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
 * Module [MongoConnection] is a low-level module allowing management of connections to MongoDB
 * servers.  To be used by higher-level modules so that only one
 * connection is opened to a given server whereas several interfaces can be attached to the open connection.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

/**
 * The main type defined by this module.
 * It's really just a [Mongo.db] connection with all the parameters
 * of a MongoDB query built in.
 **/
// TODO: Possibly arrange a map of address:port values to connections?
@abstract
type Mongo.mongodb = {
  mongo: Mongo.db;
  name: string;
  addr: string;
  port: int;
  dbname: string;
  collection: string;
  link_count: Mutable.t(int);
  keyname: string;
  valname: string;
  idxname: string;
  fields: option(Bson.document);
  orderby: list((string,int));
  limit: int;
  skip: int;
  insert_flags: int;
  update_flags: int;
  delete_flags: int;
  query_flags: int;
  index_flags: int;
}

MongoConnection = {{

  @private H = Bson.Abbrevs

  @private
  Log = {{

    @private
    @expand
    gen(f, m, fn:string, msg) =
      if m.mongo.conf.verbose then f("MongoConnection({m.name}).{fn}", msg)
      else void

    @expand
    info(m, fn, msg) = gen(@toplevel.Log.info, m, fn, msg)

    @expand
    debug(m, fn, msg) = gen(@toplevel.Log.debug, m, fn, msg)

    @expand
    error(m, fn, msg) = gen(@toplevel.Log.error, m, fn, msg)

  }}

  @private
  open_(dbo:outcome(Mongo.db,Mongo.failure),name:string): outcome(Mongo.mongodb,Mongo.failure) =
    match dbo with
    | {success=mongo} ->
       (match SocketPool.gethost(mongo.pool) with
        | (addr,port) ->
           db = {~mongo; ~name; ~addr; ~port; link_count=Mutable.make(1);
                 keyname="key"; valname="value"; idxname="index";
                 dbname="db"; collection="collection";
                 fields={none}; orderby=[]; limit=0; skip=0;
                 insert_flags=0; update_flags=0; delete_flags=0; query_flags=0; index_flags=0;
                }
           do System.at_exit( ->
                               if db.link_count.get() > 0
                               then
                                 do Log.info(db, "open", "closing mongo (exit) link_count={db.link_count.get()}")
                                 _ = MongoDriver.close(db.mongo)
                                 void
                               else void)
           {success=db})
    | {~failure} -> {~failure}

  /**
   * Perform authentication on a connection.
   * Normally handled automatically from command line options or built-in parameters but
   * you can perform authentications from other sources, here.
   **/
  authenticate(db:Mongo.mongodb, auth:Mongo.auths) =
    List.fold((auth, outcome ->
                match outcome with
                | {success=db} ->
                   match MongoCommands.authenticate(db, db.dbname, auth.user, auth.password) with
                   | {success=_} ->
                      do Log.info(db, "authenticate", "success")
                      {success={db with mongo.conf.auths = [auth|db.mongo.conf.auths]}}
                   | {~failure} ->
                      do Log.info(db, "authenticate","failure {failure}")
                      {~failure}
                   end
                | {~failure} -> {~failure}
              ),auth,{success=db})

  /**
   * Open a connection to a single server.  No check for primary status is
   * carried out and no reconnection is attempted.  Note that we need to
   * give a name to the connection even though it is dissociated from the
   * list of named connections.
   *
   * Example: [openraw(name, conf, host, port)]
   **/
  openraw(name:string, conf, addr:string, port:int) : outcome(Mongo.mongodb,Mongo.failure) =
    open_((match MongoDriver.open(name, conf, addr, port) with
           | {success=m} -> {success=m}
           | {~failure} -> {~failure}),name)

  /**
   * Open a connection to a replica set starting from the given list of seeds.
   *
   * Example: [replraw(name, conf, seeds)]
   *
   * This routine causes a serach for the current host list among the seeds
   * and then searches for the primary among the hosts.  Reconnection logic
   * is enabled.
   **/
  replraw(name:string, conf, seeds:list(Mongo.mongo_host))
      : outcome(Mongo.mongodb,Mongo.failure) =
    open_(MongoReplicaSet.connect(MongoReplicaSet.init(name, conf, seeds)),name)

  /**
   * Get the current SlaveOK status.
   **/
  get_slaveok(m:Mongo.mongodb): bool = m.mongo.conf.slaveok

  /**
   * Force a reconnection.
   *
   * {b Warning}: a failed reconnection is considered fatal.
   **/
  force_reconnect(m:Mongo.mongodb): void = MongoDriver.force_reconnect(m.mongo)

  /**
   * Open a connection according to the named parameters.
   *
   * Parameters are defined on the command line and can define any number
   * of connections:
   *
   * [prog.exe -ms localhost:27017 -mn blort -mr blort -ms localhost:10001 -mc true]
   *
   * The [-mn] option defines the name, following options apply to the most recent
   * name on the command line (the default name is "default").
   **/
  open(name:string): outcome(Mongo.mongodb,Mongo.failure) =
    ~{conf seeds} = MongoConf.get_default(name)
    match seeds with
    | [] -> {failure={Error="MongoConnection.open: No host for plain connection"}}
    | [(host,port)] -> openraw(name, conf, host, port)
    | seeds -> replraw(name, conf, seeds)

  /**
   * Open a named connection but cause a fatal error if a connection cannot be found.
   **/
  openfatal(name:string): Mongo.mongodb =
    match open(name) with
    | {success=rs} -> rs
    | {~failure} ->
      msg = "Can't connect: {MongoCommon.string_of_failure(failure)}"
      do @toplevel.Log.error("MongoConnection.openfatal", msg)
      @fail(msg)

  /**
   * Clone a connection.  We actually just bump the link count.  On close
   * the connection itself is only closed once the link count drops to zero.
   **/
  clone(db:Mongo.mongodb): Mongo.mongodb =
    do db.link_count.set(db.link_count.get()+1)
    db

  /**
   * Decrement the link count on a connection and close when zero.
   **/
  close(db:Mongo.mongodb): void =
    lc = db.link_count.get()
    if lc > 0
      then
        do db.link_count.set(lc-1)
        if lc <= 1
        then
          do Log.info(db, "close", "closing mongo (close) link_count={db.link_count.get()}")
          _ = MongoDriver.close(db.mongo)
          void
        else void
      else void

  /**
   * Change the namespace built into the connection.  The defaults are:
   * db="db" and collection="collection".
   **/
   //* Changing the namespace {b no longer} bumps the link count.
  namespace(db:Mongo.mongodb, dbname:string, collection:string): Mongo.mongodb =
    //do db.link_count.set(db.link_count.get()+1)
    { db with ~dbname; ~collection }

  /** Change the pool size **/
  pool_max(db:Mongo.mongodb, poolmax:int): Mongo.mongodb =
    { db with mongo.conf.poolmax = Int.max(poolmax,1) }

  /** Change the SlaveOk allowed status **/
  allow_slaveok(db:Mongo.mongodb, slaveok:bool): Mongo.mongodb =
    { db with mongo.conf.slaveok = slaveok }

  /** Change the bufsize hint (only applies to newly created buffers) **/
  bufsize(db:Mongo.mongodb, bufsize:int): Mongo.mongodb =
    { db with mongo.conf.bufsize = bufsize }

  /** Enable/disable logging for the given connection. **/
  log(db:Mongo.mongodb, verbose:bool): Mongo.mongodb =
    { db with mongo.conf.verbose = verbose }

  /** Add a seed (takes effect on reconnect) **/
  add_seed(db:Mongo.mongodb, seed:Mongo.mongo_host): Mongo.mongodb =
    { db with mongo.seeds = seed +> db.mongo.seeds }

  /** Remove a seed **/
  remove_seed(db:Mongo.mongodb, seed:Mongo.mongo_host): Mongo.mongodb =
    { db with mongo.seeds = List.filter((s -> s != seed),db.mongo.seeds) }

  /** Change the reconnect wait time (milliseconds) **/
  reconnect_wait(db:Mongo.mongodb, waiting:int): Mongo.mongodb =
    { db with mongo.conf.waiting = waiting }

  /** Change the maximum number of reconnection attempts before fail **/
  max_attempts(db:Mongo.mongodb, attempt:int): Mongo.mongodb =
    { db with mongo.conf.attempt = attempt }

  /** Change the basic communications timeout (default, 1 hour) **/
  comms_timeout(db:Mongo.mongodb, timeout:int): Mongo.mongodb =
    { db with mongo.conf.timeout = timeout }

  /** Return the name of the inbuilt db **/
  dbname(m:Mongo.mongodb): string = m.dbname

  /** Return the name of the inbuilt collection **/
  collection(m:Mongo.mongodb): string = m.collection

  /**
   * Return the last error on the given connection.
   **/
  getLastError(db:Mongo.mongodb): Mongo.result = MongoCommands.getLastError(db, db.dbname)

  /**
   * A simple error report.  Check the last error on the given database and log an error
   * if the reply is an actual error.
   **/
  err(db:Mongo.mongodb, msg:string): bool =
    err = MongoCommands.getLastError(db, db.dbname)
    status = MongoCommon.is_error(err)
    do Log.error(db, "err", "msg={msg} err={MongoCommon.string_of_result(err)}")
    status

  /** Set the "skip" number on the given connection. **/
  skip(db:Mongo.mongodb, skip:int): Mongo.mongodb = { db with ~skip }

  /** Set the "limit" number on the given connection. **/
  limit(db:Mongo.mongodb, limit:int): Mongo.mongodb = { db with ~limit }

  /** Set the "fields" document on the given connection. **/
  fields(db:Mongo.mongodb, fields:option(Bson.document)): Mongo.mongodb = { db with ~fields }

  /** Set the "orderby" document on the given connection. **/
  orderby(db:Mongo.mongodb, orderby:list((string,int))): Mongo.mongodb = { db with ~orderby }

  /** Set the "continueOnError" flag for all [insert] calls. **/
  continueOnError(db:Mongo.mongodb): Mongo.mongodb =
    { db with insert_flags=Bitwise.lor(db.insert_flags,MongoCommon.ContinueOnErrorBit) }

  /** Set the "Upsert" flag for all [update] calls. **/
  upsert(db:Mongo.mongodb): Mongo.mongodb =
    { db with update_flags=Bitwise.lor(db.update_flags,MongoCommon.UpsertBit) }

  /** Set the "multiUpdate" flag for all [update] calls. **/
  multiUpdate(db:Mongo.mongodb): Mongo.mongodb =
    { db with update_flags=Bitwise.lor(db.update_flags,MongoCommon.MultiUpdateBit) }

  /** Set the "singleRemove" flag for all [delete] calls. **/
  singleRemove(db:Mongo.mongodb): Mongo.mongodb =
    { db with delete_flags=Bitwise.lor(db.delete_flags,MongoCommon.SingleRemoveBit) }

  /** Set the "tailableCursor" flag for all [query] calls. **/
  tailableCursor(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoCommon.TailableCursorBit) }

  /** Set the "slaveOk" flag for all [query] calls. **/
  slaveOk(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoCommon.SlaveOkBit) }

  /** Set the "oplogReplay" flag for all [query] calls. **/
  oplogReplay(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoCommon.OplogReplayBit) }

  /** Set the "noCursorTimeout" flag for all [query] calls. **/
  noCursorTimeout(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoCommon.NoCursorTimeoutBit) }

  /** Set the "awaitData" flag for all [query] calls. **/
  awaitData(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoCommon.AwaitDataBit) }

  /** Set the "exhaust" flag for all [query] calls. **/
  exhaust(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoCommon.ExhaustBit) }

  /** Set the "partial" flag for all [query] calls. **/
  partial(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoCommon.PartialBit) }

  /** Set the "unique" flag for all [index] calls. **/
  unique(db:Mongo.mongodb): Mongo.mongodb =
    { db with index_flags=Bitwise.lor(db.index_flags,MongoCommon.UniqueBit) }

  /** Set the "dropdups" flag for all [index] calls. **/
  dropDups(db:Mongo.mongodb): Mongo.mongodb =
    { db with index_flags=Bitwise.lor(db.index_flags,MongoCommon.DropDupsBit) }

  /** Set the "background" flag for all [index] calls. **/
  background(db:Mongo.mongodb): Mongo.mongodb =
    { db with index_flags=Bitwise.lor(db.index_flags,MongoCommon.BackgroundBit) }

  /** Set the "sparse" flag for all [index] calls. **/
  sparse(db:Mongo.mongodb): Mongo.mongodb =
    { db with index_flags=Bitwise.lor(db.index_flags,MongoCommon.SparseBit) }

  /** Insert document into the defined database with inbuilt flags **/
  insert(m:Mongo.mongodb, documents:Bson.document): bool =
    MongoDriver.insert(m.mongo, m.insert_flags, "{m.dbname}.{m.collection}", documents)

  /** Insert document with getlasterror into the defined database with inbuilt flags **/
  inserte(m:Mongo.mongodb, documents:Bson.document): option(Mongo.reply) =
    MongoDriver.inserte(m.mongo, m.insert_flags, "{m.dbname}.{m.collection}", documents)

  /** Insert document with getlasterror converted into a result into the defined database with inbuilt flags **/
  insert_result(m:Mongo.mongodb, documents:Bson.document): Mongo.result =
    MongoCommon.reply_to_result("MongoConnection.insert_result",0,
                                MongoDriver.inserte(m.mongo, m.insert_flags, "{m.dbname}.{m.collection}", documents))

  /** Insert batch of documents into the defined database with inbuilt flags **/
  insert_batch(m:Mongo.mongodb, documents:list(Bson.document)): bool =
    MongoDriver.insert_batch(m.mongo, m.insert_flags, "{m.dbname}.{m.collection}", documents)

  /** Insert batch of documents with getlasterror into the defined database with inbuilt flags **/
  insert_batche(m:Mongo.mongodb, documents:list(Bson.document)): option(Mongo.reply) =
    MongoDriver.insert_batche(m.mongo, m.insert_flags, "{m.dbname}.{m.collection}", documents)

  /** Insert batch of documents with getlasterror converted into a result into the defined database with inbuilt flags **/
  insert_batch_result(m:Mongo.mongodb, documents:list(Bson.document)): Mongo.result =
    MongoCommon.reply_to_result("MongoConnection.insert_batch_result",0,
                                MongoDriver.insert_batche(m.mongo, m.insert_flags, "{m.dbname}.{m.collection}", documents))

  /** Update document in the defined database with inbuilt flags **/
  update(m:Mongo.mongodb, selector:Bson.document, update:Bson.document): bool =
    MongoDriver.update(m.mongo, m.update_flags, "{m.dbname}.{m.collection}", selector, update)

  /** Update document with getlasterror in the defined database with inbuilt flags **/
  updatee(m:Mongo.mongodb, selector:Bson.document, update:Bson.document): option(Mongo.reply) =
    MongoDriver.updatee(m.mongo, m.update_flags, "{m.dbname}.{m.collection}", selector, update)

  /** Update document with getlasterror converted into a result in the defined database with inbuilt flags **/
  update_result(m:Mongo.mongodb, selector:Bson.document, update:Bson.document): Mongo.result =
    MongoCommon.reply_to_result("MongoConnection.update_result",0,
                                MongoDriver.updatee(m.mongo, m.update_flags, "{m.dbname}.{m.collection}",
                                                    selector, update))

  /** Perform a query using inbuilt parameters.
   *  The functions to handle [Mongo.reply] are in [MongoDriver].
   **/
  query(m:Mongo.mongodb, query:Bson.document): option(Mongo.reply) =
    query = (match m.orderby with
             | [] -> query
             | orderby -> [H.doc("$query",query), H.doc("$orderby",List.map(((f,o) -> H.i32(f,o)),orderby))])
    MongoDriver.query(m.mongo, m.query_flags, "{m.dbname}.{m.collection}", m.skip, m.limit, query, m.fields)

  /** Perform a get_more using inbuilt parameters **/
  get_more(m:Mongo.mongodb, cursorID:Mongo.cursorID): option(Mongo.reply) =
    MongoDriver.get_more(m.mongo, "{m.dbname}.{m.collection}", m.limit, cursorID)

  /** Delete documents from the defined database with inbuilt flags **/
  delete(m:Mongo.mongodb, selector:Bson.document): bool =
    MongoDriver.delete(m.mongo, m.delete_flags, "{m.dbname}.{m.collection}", selector)

  /** Delete documents with getlasterror from the defined database with inbuilt flags **/
  deletee(m:Mongo.mongodb, selector:Bson.document): option(Mongo.reply) =
    MongoDriver.deletee(m.mongo, m.delete_flags, "{m.dbname}.{m.collection}", selector)

  /** Delete documents with getlasterror converted into a result from the defined database with inbuilt flags **/
  delete_result(m:Mongo.mongodb, selector:Bson.document): Mongo.result =
    MongoCommon.reply_to_result("MongoConnection.delete_result",0,
                                MongoDriver.deletee(m.mongo, m.delete_flags, "{m.dbname}.{m.collection}", selector))

  /** Perform a kill_cursors operation **/
  kill_cursors(m:Mongo.mongodb, cursors:list(Mongo.cursorID)): bool =
    MongoDriver.kill_cursors(m.mongo, cursors)

  /** Perform a kill_cursors operation with getlasterror **/
  kill_cursorse(m:Mongo.mongodb, cursors:list(Mongo.cursorID)): option(Mongo.reply) =
    MongoDriver.kill_cursorse(m.mongo, cursors)

  /** Perform a kill_cursors operation with getlasterror converted into a result **/
  kill_cursors_result(m:Mongo.mongodb, cursors:list(Mongo.cursorID)): Mongo.result =
    MongoCommon.reply_to_result("MongoConnection.kill_cursors_result",0,
                                MongoDriver.kill_cursorse(m.mongo, cursors))

  /** Perform a msg operation **/
  msg(m:Mongo.mongodb, msg:string): bool =
    MongoDriver.msg(m.mongo, msg)

  /** Perform a msg operation with getlasterror **/
  msge(m:Mongo.mongodb, msg:string): option(Mongo.reply) =
    MongoDriver.msge(m.mongo, msg)

  /** Perform a msg operation with getlasterror converted into a result **/
  msg_result(m:Mongo.mongodb, msg:string): Mongo.result =
    MongoCommon.reply_to_result("MongoConnection.msg_result",0,
                                MongoDriver.msge(m.mongo, msg))

  /** Add an index to the inbuilt collection **/
  create_index(m:Mongo.mongodb, key:Bson.document): bool =
    MongoDriver.create_index(m.mongo, "{m.dbname}.{m.collection}", key, m.index_flags)

  /** Add an index to the inbuilt collection with getlasterror **/
  create_indexe(m:Mongo.mongodb, key:Bson.document): option(Mongo.reply) =
    MongoDriver.create_indexe(m.mongo, "{m.dbname}.{m.collection}", key, m.index_flags)

  Cursor = {{

     /** Initialise bare cursor **/
     init(m:Mongo.mongodb): Mongo.cursor =
       MongoCursor.init(m.mongo, "{m.dbname}.{m.collection}")

     set_flags(c:Mongo.cursor, flags:int): Mongo.cursor = MongoCursor.set_flags(c,flags)
     set_skip(c:Mongo.cursor, skip:int): Mongo.cursor = MongoCursor.set_skip(c,skip)
     set_limit(c:Mongo.cursor, limit:int): Mongo.cursor = MongoCursor.set_limit(c,limit)
     set_query(c:Mongo.cursor, query:option(Bson.document)): Mongo.cursor = MongoCursor.set_query(c,query)
     set_fields(c:Mongo.cursor, fields:option(Bson.document)): Mongo.cursor = MongoCursor.set_fields(c,fields)
     set_orderby(c:Mongo.cursor, orderby:list((string,int))): Mongo.cursor = MongoCursor.set_orderby(c,orderby)
     tailable(c:Mongo.cursor): Mongo.cursor = MongoCursor.tailable(c)
     op_query(c:Mongo.cursor): Mongo.cursor = MongoCursor.op_query(c)
     get_more(c:Mongo.cursor): Mongo.cursor = MongoCursor.get_more(c)
     document(c:Mongo.cursor, n:int): Mongo.result = MongoCursor.document(c,n)
     all_documents(c:Mongo.cursor): Mongo.results = MongoCursor.all_documents(c)
     reset(c:Mongo.cursor): Mongo.cursor = MongoCursor.reset(c)
     next(c:Mongo.cursor): Mongo.cursor = MongoCursor.next(c)
     for(init:'state, next:'state -> 'state, cond:'state -> ('state, bool)): 'state = MongoCursor.for(init,next,cond)
     valid(c:Mongo.cursor): bool = MongoCursor.valid(c)
     check_cursor_error(c:Mongo.cursor): Mongo.result = MongoCursor.check_cursor_error(c)

     /** Start a simple cursor query **/
     start(m:Mongo.mongodb, query:Bson.document): Mongo.cursor =
       MongoCursor.start(m.mongo, "{m.dbname}.{m.collection}", query, m.limit)

     /** Perform a query according to inbuilt parameters, return cursor **/
     find(m:Mongo.mongodb, query:Bson.document): outcome(Mongo.cursor,Mongo.failure) =
       MongoCursor.find(m.mongo, "{m.dbname}.{m.collection}", query, m.fields, m.orderby, m.limit, m.skip, m.query_flags)

     /** Perform a query according to inbuilt parameters, return first match **/
     find_one(m:Mongo.mongodb, query:Bson.document): Mongo.result =
       MongoCursor.find_one(m.mongo, "{m.dbname}.{m.collection}", query, m.fields, m.orderby)

     /** Perform a query according to inbuilt parameters, return up to [limit] matches **/
     find_all(m:Mongo.mongodb, query:Bson.document): Mongo.results =
       MongoCursor.find_all(m.mongo, "{m.dbname}.{m.collection}", query, m.fields, m.orderby, m.limit)

  }} // Cursor

  /**
   * Save a function into the system.js collection for the named database.
   *
   * Example: [save_function(mongodb, dbname, name, code)]
   * @param mongodb The mongo connection
   * @param dbname The name of the database
   * @param name The name of the function
   * @param code The code for the function
   **/
  save_function(m:Mongo.mongodb, dbname:string, name:string, code:Bson.code): Mongo.result =
    insert_result(namespace(m,dbname,"system.js"),Bson.opa2doc({_id=name; value=code}))

}}

// End of file connection.opa
