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
  orderby: option(Bson.document);
  limit: int;
  skip: int;
  insert_flags: int;
  update_flags: int;
  delete_flags: int;
  query_flags: int;
}

type Mongo.param = {
  name:string;
  replname:option(string);
  bufsize:int;
  concurrency:Mongo.concurrency;
  pool_max:int;
  close_socket:bool;
  log:bool;
  seeds:list(Mongo.mongo_host);
}
type Mongo.params = list(Mongo.param)

MongoConnection = {{

  @private ML = MongoLog

  @private default_seeds = ([("localhost",MongoDriver.default_port)]:list(Mongo.mongo_host))

  @private init_param = ({
    name="default";
    replname={none};
    bufsize=50*1024;
    concurrency={pool};
    pool_max=2;
    close_socket=false;
    log=false;
    seeds=default_seeds;
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
  get_params = ->
    do if not(params_done.get())
       then params.set(CommandLine.filter({
         title = "MongoDB connection parameters";
         init = params.get() : Mongo.params;
         anonymous = [];
         parsers = [
           {CommandLine.default_parser with
              names = ["--mongo-name", "--mongoname", "-mn"]
              description = "Name for the MongoDB server connection"
              param_doc = "<string>"
              on_param(p) = parser name={Rule.consume} ->
                do last_name.set(name)
                {no_params = add_param((p -> { p with ~name }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-repl-name", "--mongoreplname", "-mr"]
              description = "Replica set name for the MongoDB server"
              param_doc = "<string>"
              on_param(p) = parser s={Rule.consume} ->
                {no_params = add_param((p -> { p with replname={some=s} }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-buf-size", "--mongobufsize", "-mb"]
              description = "Hint for initial MongoDB connection buffer size"
              param_doc = "<int>"
              on_param(p) = parser n={Rule.natural} -> {no_params = add_param((p -> { p with bufsize = n }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-concurrency", "--mongoconcurrency", "-mx"]
              description = "Concurrency type, 'pool', 'cell' or 'singlethreaded'"
              param_doc = "<string>"
              on_param(p) = parser s={Rule.consume} ->
                concurrency =
                  ((match s with
                    | "pool" -> {pool}
                    | "cell" -> {cell}
                    | "singlethreaded" -> {singlethreaded}
                    | _ -> ML.fatal("MongoConnection.get_params","Unknown Mongo concurrency string {s}",-1)):Mongo.concurrency)
                {no_params = add_param((p -> { p with ~concurrency }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-socket-pool", "--mongosocketpool", "-mp"]
              description = "Number of sockets in socket pool (>=2 enables socket pool)"
              param_doc = "<int>"
              on_param(p) = parser n={Rule.natural} -> {no_params = add_param((p -> { p with pool_max = n }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-close-socket", "--mongoclosesocket", "-mc"]
              description = "Maintain MongoDB server sockets in a closed state"
              param_doc = "<bool>"
              on_param(p) = parser b={Rule.bool} -> {no_params = add_param((p -> { p with close_socket = b }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-log", "--mongolog", "-ml"]
              description = "Enable MongoLog logging"
              param_doc = "<bool>"
              on_param(p) = parser b={Rule.bool} -> {no_params = add_param((p -> { p with log = b }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-seed", "--mongoseed", "-ms"]
              description = "Add a seed to a replica set, allows multiple seeds"
              param_doc = "<host>\{:<port>\}"
              on_param(p) =
                parser s={Rule.consume} ->
                  {no_params = add_param((p ->
                    seeds = if p.seeds == default_seeds then [] else p.seeds
                    { p with seeds=[MongoReplicaSet.mongo_host_of_string(s)|seeds] }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-host", "--mongohost", "-mh"]
              description = "Host name of a MongoDB server, overwrites any previous addresses for this name"
              param_doc = "<host>\{:<port>\}"
              on_param(p) =
                parser s={Rule.consume} ->
                  {no_params = add_param((p ->
                    { p with seeds=[MongoReplicaSet.mongo_host_of_string(s)] }),p)}
           },
           {CommandLine.default_parser with
              names = ["--mongo-log-type", "--mongologtype", "-mt"]
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
         ];
       }))
  params_done.set(true)

  @private
  open_(dbo:outcome(Mongo.db,Mongo.failure),name:string): outcome(Mongo.mongodb,Mongo.failure) =
    match dbo with
    | {success=mongo} ->
       (match mongo.primary.get() with
        | {some=(addr,port)} ->
           db = {~mongo; ~name; bufsize=mongo.bufsize; ~addr; ~port; link_count=Mutable.make(1);
                 keyname="key"; valname="value"; idxname="index";
                 dbname="db"; collection="collection";
                 fields={none}; orderby={none}; limit=0; skip=0;
                 insert_flags=0; update_flags=0; delete_flags=0; query_flags=0;
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
           {success=db}
        | {none} -> {failure={Error="MongoConnection.open: no primary"}})
    | {~failure} -> {~failure}

  /**
   * Open a connection to a single server.  No check for primary status is
   * carried out and no reconnection is attempted.  Note that we need to
   * give a name to the connection even though it is dissociated from the
   * list of named connections.
   *
   * Example: [openraw(name, bufsize, close_socket, log, host, port)]
   **/
  openraw(name:string, bufsize:int, concurrency:Mongo.concurrency,
          pool_max:int, close_socket:bool, log:bool, addr:string, port:int)
        : outcome(Mongo.mongodb,Mongo.failure) =
    open_(MongoDriver.open(bufsize,concurrency,pool_max,close_socket,addr,port,log),name)

  /**
   * Open a connection to a replica set starting from the given list of seeds.
   *
   * Example: [replraw(name, bufsize, close_socket, log, seeds)]
   *
   * This routine causes a serach for the current host list among the seeds
   * and then searches for the primary among the hosts.  Rconnection logic
   * is enabled.
   **/
  replraw(name:string, bufsize:int, concurrency:Mongo.concurrency,
          pool_max:int, close_socket:bool, log:bool, seeds:list(Mongo.mongo_host))
      : outcome(Mongo.mongodb,Mongo.failure) =
    open_(MongoReplicaSet.connect(MongoReplicaSet.init(name,bufsize,concurrency,pool_max,close_socket,log,seeds)),name)

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
        | {some=rn} -> replraw(rn,p.bufsize,p.concurrency,p.pool_max,p.close_socket,p.log,p.seeds)
        | {none} ->
           (match p.seeds with
            | [] -> {failure={Error="MongoConnection.open: No host for plain connection"}}
            | [(host,port)] -> openraw(name,p.bufsize,p.concurrency,p.pool_max,p.close_socket,p.log,host,port)
            | _ -> {failure={Error="MongoConnection.open: Multiple hosts for plain connection"}}))
    | {none} -> {failure={Error="MongoConnection.open: No such replica name {name}"}}

  /**
   * Open a named connection but cause a fatal error if a connection cannot be found.
   **/
  openfatal(name:string): Mongo.mongodb =
    match open(name) with
    | {success=rs} -> rs
    | {~failure} -> ML.fatal("MongoConnection.openfatal","Can't connect: {MongoDriver.string_of_failure(failure)}",-1)

  /**
   * Clone a connection.  We actually just bump the link count.  On close
   * the connection itself is only closed once the link count drops to zero.
   **/
  clone(db:Mongo.mongodb): Mongo.mongodb =
    do db.link_count.set(db.link_count.get()+1)
    db

  /**
   * Change the namespace built into the connection.  The defaults are:
   * db="db" and collection="collection".
   **/
   //* Changing the namespace {b no longer} bumps the link count.
  namespace(db:Mongo.mongodb, dbname:string, collection:string): Mongo.mongodb =
    //do db.link_count.set(db.link_count.get()+1)
    { db with ~dbname; ~collection }

  /**
   * Enable/disable logging for the given connection.  Only applies to
   * the connection returned.
   **/
  log(db:Mongo.mongodb, log:bool): Mongo.mongodb =
    { db with mongo={ db.mongo with ~log } }

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
    status = MongoDriver.is_error(err)
    do if db.mongo.log && status
       then ML.error("MongoConnection.err({db.dbname}.{db.collection})","msg={msg} err={MongoDriver.string_of_result(err)}",void)
    status

  /** Set the "skip" number on the given connection. **/
  skip(db:Mongo.mongodb, skip:int): Mongo.mongodb = { db with ~skip }

  /** Set the "limit" number on the given connection. **/
  limit(db:Mongo.mongodb, limit:int): Mongo.mongodb = { db with ~limit }

  /** Set the "fields" document on the given connection. **/
  fields(db:Mongo.mongodb, fields:option(Bson.document)): Mongo.mongodb = { db with ~fields }

  /** Set the "orderby" document on the given connection. **/
  orderby(db:Mongo.mongodb, orderby:option(Bson.document)): Mongo.mongodb = { db with ~orderby }

  /** Set the "continueOnError" flag for all [insert] calls. **/
  continueOnError(db:Mongo.mongodb): Mongo.mongodb =
    { db with insert_flags=Bitwise.lor(db.insert_flags,MongoDriver.ContinueOnErrorBit) }

  /** Set the "Upsert" flag for all [update] calls. **/
  upsert(db:Mongo.mongodb): Mongo.mongodb =
    { db with update_flags=Bitwise.lor(db.update_flags,MongoDriver.UpsertBit) }

  /** Set the "multiUpdate" flag for all [update] calls. **/
  multiUpdate(db:Mongo.mongodb): Mongo.mongodb =
    { db with update_flags=Bitwise.lor(db.update_flags,MongoDriver.MultiUpdateBit) }

  /** Set the "singleRemove" flag for all [delete] calls. **/
  singleRemove(db:Mongo.mongodb): Mongo.mongodb =
    { db with delete_flags=Bitwise.lor(db.delete_flags,MongoDriver.SingleRemoveBit) }

  /** Set the "tailableCursor" flag for all [query] calls. **/
  tailableCursor(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoDriver.TailableCursorBit) }

  /** Set the "slaveOk" flag for all [query] calls. **/
  slaveOk(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoDriver.SlaveOkBit) }

  /** Set the "oplogReplay" flag for all [query] calls. **/
  oplogReplay(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoDriver.OplogReplayBit) }

  /** Set the "noCursorTimeout" flag for all [query] calls. **/
  noCursorTimeout(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoDriver.NoCursorTimeoutBit) }

  /** Set the "awaitData" flag for all [query] calls. **/
  awaitData(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoDriver.AwaitDataBit) }

  /** Set the "exhaust" flag for all [query] calls. **/
  exhaust(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoDriver.ExhaustBit) }

  /** Set the "partial" flag for all [query] calls. **/
  partial(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoDriver.PartialBit) }

  insert(m:Mongo.mongodb, flags:int, ns:string, documents:Bson.document): bool =
    MongoDriver.insert(m.mongo, flags, ns, documents)
  inserte(m:Mongo.mongodb, flags:int, ns:string, dbname:string, documents:Bson.document): option(Mongo.reply) =
    MongoDriver.inserte(m.mongo, flags, ns, dbname, documents)

}}

// End of file connection.opa
