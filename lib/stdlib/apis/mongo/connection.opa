/*
    Copyright © 2011, 2012 MLstate

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
 * We also handle the command line arguments here.  When we call [open] we parse (one time)
 * the command line which sets up the variable [params].  We implement a system of named
 * connections and we build up a description of each named connection:
 *
 * [prog.exe -mn conn_name -mr replname -ms localhost:12345 -ms localhost:54321]
 *
 * We can then open connections by name: [MongoConnection.open("conn_name")], the default
 * connection name is "default".
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
  bufsize: int;
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

type Mongo.param = {
  name:string;
  replname:option(string);
  bufsize:int;
  pool_max:int;
  allow_slaveok:bool;
  log:bool;
  seeds:list(Mongo.mongo_host);
  auth:Mongo.auths;
}
type Mongo.params = list(Mongo.param)

MongoConnection = {{

  @private ML = MongoLog
  @private H = Bson.Abbrevs

  @private default_seeds = ([("localhost",MongoDriver.default_port)]:list(Mongo.mongo_host))

  @private init_param = ({
    name="default";
    replname={none};
    bufsize=50*1024;
    pool_max=2;
    allow_slaveok=false;
    log=false;
    seeds=default_seeds;
    auth=[];
  }:Mongo.param)

  @private last_name = Mutable.make("default")

  @private params = Mutable.make(([init_param]:Mongo.params))

  @private params_done = Mutable.make(false)

  /**
   * Add a named connection to the list of named connections.
   * If this function is called {b before} the first call to [MongoConnection.open]
   * then the command line parameters can update the value we add here.  If it is
   * called {b after} the first [open] call then we override the command line parameters
   * and set them here.
   **/
  add_named_connection(p:Mongo.param): void =
    rec add(l) =
      match l with
      | [] -> [p]
      | [h|t] ->
         if h.name == p.name
         then [p|t]
         else [h|add(t)]
    params.set(add(params.get()))

  @private
  add_param(f,p:Mongo.params) =
    ln = last_name.get()
    rec updt(l) =
      match l with
      | [p|rest] ->
         if p.name == ln
         then [f(p)|rest]
         else [p|updt(rest)]
      | [] -> [f({ init_param with name=ln })]
    updt(p)

  @private
  auth_parser = parser
  | user=((![:] .)+)[:] password=((![@] .)+) [@] dbname=(.+) ->
    {user=Text.to_string(user); dbname=Text.to_string(dbname); password=Text.to_string(password)}

  @private
  get_params = ->
    // TODO: put in options for reconnection times etc.
    do if not(params_done.get())
       then params.set(CommandLine.filter({
         title = "MongoDB connection parameters";
         init = params.get() : Mongo.params;
         anonymous = [];
         parsers = [
           {CommandLine.default_parser with
              names = ["--mongo-name", "--mongoname", "--mn", "-mn"]
              description = "Name for the MongoDB server connection"
              param_doc = "<string>"
              on_param(p) = parser name={Rule.consume} ->
                do last_name.set(name)
                {no_params = add_param((p -> { p with ~name }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-repl-name", "--mongoreplname", "--mr", "-mr"]
              description = "Replica set name for the MongoDB server"
              param_doc = "<string>"
              on_param(p) = parser s={Rule.consume} ->
                {no_params = add_param((p -> { p with replname={some=s} }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-buf-size", "--mongobufsize", "--mb", "-mb"]
              description = "Hint for initial MongoDB connection buffer size"
              param_doc = "<int>"
              on_param(p) = parser n={Rule.natural} -> {no_params = add_param((p -> { p with bufsize=n }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-socket-pool", "--mongosocketpool", "--mp", "-mp"]
              description = "Number of sockets in socket pool (>=2 enables socket pool)"
              param_doc = "<int>"
              on_param(p) = parser n={Rule.natural} -> {no_params = add_param((p -> { p with pool_max=Int.max(n,1) }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-allow-slaveok", "--mongoallowslaveok", "--mk", "-mk"]
              description = "Allow SlaveOK mode (ReplSet only)"
              param_doc = "<bool>"
              on_param(p) = parser b={Rule.bool} -> {no_params = add_param((p -> { p with allow_slaveok=b }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-log", "--mongolog", "--ml", "-ml"]
              description = "Enable MongoLog logging"
              param_doc = "<bool>"
              on_param(p) = parser b={Rule.bool} -> {no_params = add_param((p -> { p with log=b }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-seed", "--mongoseed", "--ms", "-ms"]
              description = "Add a seed to a replica set, allows multiple seeds"
              param_doc = "<host>\{:<port>\}"
              on_param(p) =
                parser s={Rule.consume} ->
                  {no_params = add_param((p ->
                    seeds = if p.seeds == default_seeds then [] else p.seeds
                    { p with seeds=[MongoReplicaSet.mongo_host_of_string(s)|seeds] }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-host", "--mongohost", "--mh", "-mh"]
              description = "Host name of a MongoDB server, overwrites any previous addresses for this name"
              param_doc = "<host>\{:<port>\}"
              on_param(p) =
                parser s={Rule.consume} ->
                  {no_params = add_param((p ->
                    { p with seeds=[MongoReplicaSet.mongo_host_of_string(s)] }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-log-type", "--mongologtype", "--mt", "-mt"]
              description = "Type of logging: stdout, stderr, logger, none"
              param_doc = "<string>"
              on_param(p) = parser s={Rule.consume} ->
                logtype =
                  ((match s with
                    | "stdout" -> {stdout}
                    | "stderr" -> {stderr}
                    | "logger" -> {logger}
                    | "none" | "nomongolog" -> {nomongolog}
                    | _ -> ML.fatal("MongoConnection.get_params","Unknown Mongo log type string {s}",-1)):Mongo.logtype)
                {no_params = add_param((p -> do MongoLog.logtype.set(logtype) p),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-auth", "--mongoauth", "--ma", "-ma"]
              description = "MongoDB user authentication"
              param_doc = "user:password@dbname"
              on_param(p) = parser s={Rule.consume} ->
                  match Parser.try_parse(auth_parser,s) with
                  | {some=auth} ->
                     {no_params = add_param((p -> {p with auth=[auth|p.auth]}),p)}
                  | {none} ->
                     ML.fatal("MongoConnection.get_params","Invalid auth string {s}",{no_params=add_param((p -> p),p)})
           },
         ];
       }))
  params_done.set(true)

  @private
  open_(dbo:outcome((bool,Mongo.db),Mongo.failure),name:string): outcome(Mongo.mongodb,Mongo.failure) =
    match dbo with
    | {success=(slaveok,mongo)} ->
       do SocketPool.setslaveok(mongo.pool,slaveok)
       (match SocketPool.gethost(mongo.pool) with
        | (addr,port) ->
           db = {~mongo; ~name; bufsize=mongo.bufsize; ~addr; ~port; link_count=Mutable.make(1);
                 keyname="key"; valname="value"; idxname="index";
                 dbname="db"; collection="collection";
                 fields={none}; orderby=[]; limit=0; skip=0;
                 insert_flags=0; update_flags=0; delete_flags=0; query_flags=0; index_flags=0;
                }
           do System.at_exit( ->
                               if db.link_count.get() > 0
                               then
                                 do if db.mongo.log
                                    then ML.info("MongoConnection.open",
                                                 "closing mongo (exit) link_count={db.link_count.get()}",void)
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
                   match MongoCommands.authenticate(db, auth.dbname, auth.user, auth.password) with
                   | {success=_} ->
                      do if db.mongo.log
                         then ML.info("MongoConnection.authenticate","success",void)
                      {success={db with mongo={db.mongo with
                                               auth=[{dbname=auth.dbname;
                                                      user=auth.user;
                                                      password=auth.password}|db.mongo.auth]}}}
                   | {~failure} ->
                      do if db.mongo.log
                         then ML.info("MongoConnection.authenticate","failure {failure}",void)
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
   * Example: [openraw(name, bufsize, pool_max, log, host, port)]
   **/
  openraw(name:string, bufsize:int, pool_max:int, allow_slaveok:bool, log:bool, auth:Mongo.auths, addr:string, port:int)
        : outcome(Mongo.mongodb,Mongo.failure) =
    open_((match MongoDriver.open(bufsize,pool_max,allow_slaveok,false,addr,port,log,auth) with
           | {success=m} -> {success=(false,m)}
           | {~failure} -> {~failure}),name)

  /**
   * Open a connection to a replica set starting from the given list of seeds.
   *
   * Example: [replraw(name, bufsize, pool_max, log, seeds)]
   *
   * This routine causes a serach for the current host list among the seeds
   * and then searches for the primary among the hosts.  Reconnection logic
   * is enabled.
   **/
  replraw(name:string, bufsize:int, pool_max:int, allow_slaveok:bool, log:bool, auth:Mongo.auths, seeds:list(Mongo.mongo_host))
      : outcome(Mongo.mongodb,Mongo.failure) =
    open_(MongoReplicaSet.connect(MongoReplicaSet.init(name,bufsize,pool_max,allow_slaveok,log,auth,seeds)),name)

  /**
   * Get the current SlaveOK status.
   **/
  get_slaveok(m:Mongo.mongodb): bool = MongoDriver.get_slaveok(m.mongo)

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
    do get_params()
    match List.find((p -> p.name == name),params.get()) with
    | {some=p} ->
       (match p.replname with
        | {some=rn} -> replraw(rn,p.bufsize,p.pool_max,p.allow_slaveok,p.log,p.auth,p.seeds)
        | {none} ->
           (match p.seeds with
            | [] -> {failure={Error="MongoConnection.open: No host for plain connection"}}
            | [(host,port)] -> openraw(name,p.bufsize,p.pool_max,p.allow_slaveok,p.log,p.auth,host,port)
            | _ -> {failure={Error="MongoConnection.open: Multiple hosts for plain connection"}}))
    | {none} -> {failure={Error="MongoConnection.open: No such replica name {name}"}}

  /**
   * Open a named connection but cause a fatal error if a connection cannot be found.
   **/
  openfatal(name:string): Mongo.mongodb =
    match open(name) with
    | {success=rs} -> rs
    | {~failure} -> ML.fatal("MongoConnection.openfatal","Can't connect: {MongoCommon.string_of_failure(failure)}",-1)

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
          do if db.mongo.log
             then ML.info("MongoConnection.close","closing mongo (close) link_count={db.link_count.get()}",void)
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
  pool_max(db:Mongo.mongodb, pool_max:int): Mongo.mongodb =
    { db with mongo={ db.mongo with pool_max=Int.max(pool_max,1) } }

  /** Change the SlaveOk allowed status **/
  allow_slaveok(db:Mongo.mongodb, allow_slaveok:bool): Mongo.mongodb =
    { db with mongo={ db.mongo with ~allow_slaveok } }

  /** Change the bufsize hint (only applies to newly created buffers) **/
  bufsize(db:Mongo.mongodb, bufsize:int): Mongo.mongodb =
    { db with mongo={ db.mongo with ~bufsize } }

  /** Enable/disable logging for the given connection. **/
  log(db:Mongo.mongodb, log:bool): Mongo.mongodb =
    { db with mongo={ db.mongo with ~log } }

  /** Add a seed (takes effect on reconnect) **/
  add_seed(db:Mongo.mongodb, seed:Mongo.mongo_host): Mongo.mongodb =
    { db with mongo={ db.mongo with seeds=seed +> db.mongo.seeds } }

  /** Remove a seed **/
  remove_seed(db:Mongo.mongodb, seed:Mongo.mongo_host): Mongo.mongodb =
    { db with mongo={ db.mongo with seeds=List.filter((s -> s != seed),db.mongo.seeds) } }

  /** Change the reconnect wait time (milliseconds) **/
  reconnect_wait(db:Mongo.mongodb, reconnect_wait:int): Mongo.mongodb =
    { db with mongo={ db.mongo with ~reconnect_wait } }

  /** Change the maximum number of reconnection attempts before fail **/
  max_attempts(db:Mongo.mongodb, max_attempts:int): Mongo.mongodb =
    { db with mongo={ db.mongo with ~max_attempts } }

  /** Change the basic communications timeout (default, 1 hour) **/
  comms_timeout(db:Mongo.mongodb, comms_timeout:int): Mongo.mongodb =
    { db with mongo={ db.mongo with ~comms_timeout } }

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
    do if db.mongo.log && status
       then ML.error("MongoConnection.err({db.dbname}.{db.collection})","msg={msg} err={MongoCommon.string_of_result(err)}",void)
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
    MongoDriver.inserte(m.mongo, m.insert_flags, "{m.dbname}.{m.collection}", m.dbname, documents)

  /** Insert document with getlasterror converted into a result into the defined database with inbuilt flags **/
  insert_result(m:Mongo.mongodb, documents:Bson.document): Mongo.result =
    MongoCommon.reply_to_result("MongoConnection.insert_result",0,
                                MongoDriver.inserte(m.mongo, m.insert_flags, "{m.dbname}.{m.collection}", m.dbname, documents))

  /** Insert batch of documents into the defined database with inbuilt flags **/
  insert_batch(m:Mongo.mongodb, documents:list(Bson.document)): bool =
    MongoDriver.insert_batch(m.mongo, m.insert_flags, "{m.dbname}.{m.collection}", documents)

  /** Insert batch of documents with getlasterror into the defined database with inbuilt flags **/
  insert_batche(m:Mongo.mongodb, documents:list(Bson.document)): option(Mongo.reply) =
    MongoDriver.insert_batche(m.mongo, m.insert_flags, "{m.dbname}.{m.collection}", m.dbname, documents)

  /** Insert batch of documents with getlasterror converted into a result into the defined database with inbuilt flags **/
  insert_batch_result(m:Mongo.mongodb, documents:list(Bson.document)): Mongo.result =
    MongoCommon.reply_to_result("MongoConnection.insert_batch_result",0,
                                MongoDriver.insert_batche(m.mongo, m.insert_flags, "{m.dbname}.{m.collection}",
                                                          m.dbname, documents))

  /** Update document in the defined database with inbuilt flags **/
  update(m:Mongo.mongodb, selector:Bson.document, update:Bson.document): bool =
    MongoDriver.update(m.mongo, m.update_flags, "{m.dbname}.{m.collection}", selector, update)

  /** Update document with getlasterror in the defined database with inbuilt flags **/
  updatee(m:Mongo.mongodb, selector:Bson.document, update:Bson.document): option(Mongo.reply) =
    MongoDriver.updatee(m.mongo, m.update_flags, "{m.dbname}.{m.collection}", m.dbname, selector, update)

  /** Update document with getlasterror converted into a result in the defined database with inbuilt flags **/
  update_result(m:Mongo.mongodb, selector:Bson.document, update:Bson.document): Mongo.result =
    MongoCommon.reply_to_result("MongoConnection.update_result",0,
                                MongoDriver.updatee(m.mongo, m.update_flags, "{m.dbname}.{m.collection}",
                                                    m.dbname, selector, update))

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
    MongoDriver.deletee(m.mongo, m.delete_flags, "{m.dbname}.{m.collection}", m.dbname, selector)

  /** Delete documents with getlasterror converted into a result from the defined database with inbuilt flags **/
  delete_result(m:Mongo.mongodb, selector:Bson.document): Mongo.result =
    MongoCommon.reply_to_result("MongoConnection.delete_result",0,
                                MongoDriver.deletee(m.mongo, m.delete_flags, "{m.dbname}.{m.collection}", m.dbname, selector))

  /** Perform a kill_cursors operation **/
  kill_cursors(m:Mongo.mongodb, cursors:list(Mongo.cursorID)): bool =
    MongoDriver.kill_cursors(m.mongo, cursors)

  /** Perform a kill_cursors operation with getlasterror **/
  kill_cursorse(m:Mongo.mongodb, cursors:list(Mongo.cursorID)): option(Mongo.reply) =
    MongoDriver.kill_cursorse(m.mongo, m.dbname, cursors)

  /** Perform a kill_cursors operation with getlasterror converted into a result **/
  kill_cursors_result(m:Mongo.mongodb, cursors:list(Mongo.cursorID)): Mongo.result =
    MongoCommon.reply_to_result("MongoConnection.kill_cursors_result",0,
                                MongoDriver.kill_cursorse(m.mongo, m.dbname, cursors))

  /** Perform a msg operation **/
  msg(m:Mongo.mongodb, msg:string): bool =
    MongoDriver.msg(m.mongo, msg)

  /** Perform a msg operation with getlasterror **/
  msge(m:Mongo.mongodb, msg:string): option(Mongo.reply) =
    MongoDriver.msge(m.mongo, m.dbname, msg)

  /** Perform a msg operation with getlasterror converted into a result **/
  msg_result(m:Mongo.mongodb, msg:string): Mongo.result =
    MongoCommon.reply_to_result("MongoConnection.msg_result",0,
                                MongoDriver.msge(m.mongo, m.dbname, msg))

  /** Add an index to the inbuilt collection **/
  create_index(m:Mongo.mongodb, key:Bson.document): bool =
    MongoDriver.create_index(m.mongo, "{m.dbname}.{m.collection}", key, m.index_flags)

  /** Add an index to the inbuilt collection with getlasterror **/
  create_indexe(m:Mongo.mongodb, key:Bson.document): option(Mongo.reply) =
    MongoDriver.create_indexe(m.mongo, "{m.dbname}.{m.collection}", m.dbname, key, m.index_flags)

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
