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
 * Module [Commands] has a set of command implementations to make MongoDB
 * management easier.  We also have some OPA types which match the structure
 * of some of the values returned by the commands.  You just have to cast
 * the result of doc2opa to these types to get OPA values which have all the
 * command reply data in them.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

/* Major TODOs, there are minor ones elsewhere. */
// TODO: Build up a complete set of result types

/**
 * Command types:
 *   These are types which are intended to match the results of MongoDB
 *   command results.  If we just want a single value out of the result,
 *   then it is probably more efficient to use the dotresult functions.
 *   If we want to manipulate the whole thing in OPA, however, we can just
 *   doc2opa the result and cast to these types.  Note that these types
 *   may vary between one MongoDB version and another, these are for 1.8.3.
 *   Also, some of the results have floating elements which we 
 *   map to the Bson.register type. 
 **/

type Commands.getLastErrorOptions = {
  fsync : Bson.register(bool);
  j : Bson.register(bool);
  w : Bson.register(string); // Oh yes, use "1" for ints...
  wtimeout : Bson.register(Bson.int32);
}

type Commands.isMaster = {
  ismaster : bool;
  msg : Bson.register(string);
  setName : Bson.register(string);
  primary : Bson.register(string);
  secondary : Bson.register(bool);
  hosts : Bson.register(list(string));
  passives : Bson.register(list(string));
  arbiters : Bson.register(list(string));
  maxBsonObjectSize : int;
  ok : int;
}

type Commands.dropDatabaseType = { dropped : string; ok : int; }

type Commands.listDatabasesType = {
  databases : list({ name : string; sizeOnDisk : float; empty : bool; })
  totalSize : float;
  ok : int;
}

type Commands.serverStatusType = {
  host : string;
  version : string;
  process : string;
  uptime : float;
  uptimeEstimate : float;
  localTime : Date.date;
  globalLock : { totalTime : float;
                 lockTime : float;
                 ratio : float;
                 currentQueue : { total : int; readers : int; writers : int; }
                 activeClients : { total : int; readers : int; writers : int; } };
  mem : { bits : int; resident : int; virtual : int; supported : bool; mapped : int; };
  connections : { current : int; available : int; };
  extra_info : { note : string; heap_usage_bytes : int; page_faults : int; };
  indexCounters : { btree : { accesses : int; hits : int; misses : int; resets : int; missRatio : float; } };
  backgroundFlushing : { flushes : int; total_ms : int; average_ms : float; last_ms : int; last_finished : Date.date };
  cursors : { totalOpen : int; clientCursors_size : int; timedOut : int; };
  network : { bytesIn : int; bytesOut : int; numRequests : int; };
  opcounters : { insert : int; query : int; update : int; delete : int; getmore : int; command : int; };
  asserts : { regular : int; warning : int; msg : int; user : int; rollovers : int; };
  writeBacksQueued : bool;
  repl : Bson.register({ setName : string;
                         ismaster : bool;
                         secondary : bool;
                         hosts : list(string);
                         primary : Bson.register(string);
                       });
  ok : int;
}

type Commands.collStatsType = {
  ns : string;
  count : Bson.int32;
  size : Bson.int32;
  avgObjSize : float;
  storageSize : Bson.int32;
  numExtents : Bson.int32;
  nindexes : Bson.int32;
  lastExtentSize : Bson.int32;
  paddingFactor : float;
  flags : Bson.int32;
  totalIndexSize : Bson.int32;
  indexSizes : { _id_ : Bson.int32; };
  ok : int;
}

type Commands.explainType =
  { cursor : string;
    nscanned : int;
    nscannedObjects : int;
    n : int;
    scanAndOrder : bool;
    millis : int;
    nYields : int;
    nChunkSkips : int;
    isMultiKey : bool;
    indexOnly : bool;
    indexBounds : Bson.document; // This type is variable (key names)
    allPlans : list({ cursor : string; indexBounds : Bson.document }) // and this
    // oldPlan: ... sometimes present
}

@server_private
Commands = {{

  @private H = Bson.Abbrevs

  /**
   * Run a "$cmd" command.
   *
   * Normally you will get {ok: 0/1} as a reply but sometimes there
   * are other elements in the reply.
   **/
  run_command(m:Mongo.db, ns:string, command:Bson.document): Mongo.result =
    match MongoCursor.find_one(m, ns^".$cmd", command, {none}, {none}) with
    | {success=bson} -> MongoDriver.check_ok(bson)
    | {~failure} -> {~failure}

  /**
   * Perform a simple integer command, eg. [{ ping : 1 }]
   **/
  simple_int_command(m:Mongo.db, ns:string, cmd:string, arg:int): Mongo.result =
    run_command(m, ns, [H.i32(cmd,arg)])

  /**
   * Same as simple integer command but with options, eg. [{ "getlasterror" : 1, w : 3, wtimeout : 10000 }]
   **/
  simple_int_command_opts(m:Mongo.db, ns:string, cmd:string, arg:int, opts:Bson.document): Mongo.result =
    run_command(m, ns, List.flatten([[H.i32(cmd,arg)],opts]))

  /**
   * Perform a simple integer command, eg. [{ drop : "collection" }]
   **/
  simple_str_command(m:Mongo.db, ns:string, cmd:string, arg:string): Mongo.result =
    run_command(m, ns, [H.str(cmd,arg)])

  /**
   * Perform a simple integer command, eg. [{ drop : "collection" }]
   **/
  simple_str_command_opts(m:Mongo.db, ns:string, cmd:string, arg:string, opts:Bson.document): Mongo.result =
    run_command(m, ns, List.flatten([[H.str(cmd,arg)],opts]))

  dbToOpa(m:Mongo.db, dbname:string, command:string): outcome('a,Mongo.failure) =
    match simple_int_command(m,dbname,command,1) with
    | {success=doc} ->
       (match MongoDriver.result_to_opa({success=doc}) with
        | {some=ism} -> {success=ism}
        | {none} -> {failure={Error="Commands.{command}: invalid document from db {dbname} ({Bson.to_pretty(doc)})"}})
    | {~failure} -> {~failure}

  adminToOpa(m:Mongo.db, command:string): outcome('a,Mongo.failure) = dbToOpa(m,"admin",command)
  configToOpa(m:Mongo.db, command:string): outcome('a,Mongo.failure) = dbToOpa(m,"config",command)

  /**
   * Predicate for connection alive.  Peforms an admin "ping" command.
   * We insist upon a single {ok:1} reply, anything else results in false.
   **/
  check_connection(m:Mongo.db): outcome(bool,Mongo.failure) =
    match simple_int_command(m, "admin", "ping", 1) with
    | {success=[e]} -> {success=Option.default(false,Bson.find_bool([e],"ok"))}
    | {success=doc} -> {failure={DocError=doc}}
    | {~failure} -> {~failure}

  /**
   * Drop a database
   **/
  dropDatabase(m:Mongo.db, db:string): Mongo.result =
    simple_int_command(m, db, "dropDatabase", 1)

  /**
   * Drop a collection from a database [drop("db","collection")]
   **/
  drop(m:Mongo.db, db:string, collection:string): Mongo.result =
    simple_str_command(m, db, "drop", collection)

  /**
   * Drop an index from a collection [dropIndexes("db","collection","index")]
   **/
  dropIndexes(m:Mongo.db, db:string, collection:string, index:string): Mongo.result =
    simple_str_command_opts(m, db, "drop", collection, [H.str("index",index)])

  /**
   * List valid commands for database.
   **/
  listCommands(m:Mongo.db, db:string): Mongo.result =
    simple_int_command(m, db, "listCommands", 1)

  /**
   * List all databases.
   **/
  listDatabases(m:Mongo.db): Mongo.result =
    simple_int_command(m, "admin", "listDatabases", 1)

  /**
   * Statistics for server.
   **/
  serverStatus(m:Mongo.db): Mongo.result =
    simple_int_command(m, "admin", "serverStatus", 1)

  /**
   * Rename a collection [renameCollection("db","collection","index")]
   **/
  renameCollection(m:Mongo.db, from:string, to:string): Mongo.result =
    simple_str_command_opts(m, "admin", "renameCollection", from, [H.str("to",to)])

  /**
   * Repair the database (slow, write-locked).
   **/
  repairDatabase(m:Mongo.db): Mongo.result =
    simple_int_command(m, "admin", "repairDatabase", 1)

  /**
   * Return the last error from database.
   **/
  getLastError(m:Mongo.db, db:string): Mongo.result = simple_int_command(m, db, "getlasterror", 1)
  getLastErrorOpa(m:Mongo.db, db:string): Mongo.error = MongoDriver.error_of_result(getLastError(m, db))

  /**
   * Return the last error from database, with full options.
   **/
  getLastErrorFull(m:Mongo.db, db:string, fsync:bool, j:bool, w:int, wtimeout:int): Mongo.result =
    simple_int_command_opts(m, db, "getlasterror", 1,
                            [H.bool("fsync",fsync), H.bool("j",j), H.i32("w",w), H.i32("wtimeout",wtimeout)])
  getLastErrorFullOpa(m:Mongo.db, db:string, fsync:bool, j:bool, w:int, wtimeout:int): Mongo.error =
    MongoDriver.error_of_result(getLastErrorFull(m, db, fsync, j, w, wtimeout))

  /**
   * Reset database error status.
   **/
  resetError(m:Mongo.db, db:string): Mongo.result =
    simple_int_command(m, db, "resetError", 1)

  /**
   * Force a db error.
   **/
  forceError(m:Mongo.db, db:string): Mongo.result =
    simple_int_command(m, db, "forceError", 1)

  /**
   * Return information about the server.
   **/
  buildInfo(m:Mongo.db): Mongo.result =
    simple_int_command(m, "admin", "buildInfo", 1)

  /**
   * Return collection statistics.
   **/
  collStats(m:Mongo.db, db:string, collection:string): Mongo.result =
    simple_str_command(m, db, "collStats", collection)
  collStatsOpa(m:Mongo.db, db:string, collection:string): outcome(Commands.collStatsType,Mongo.failure) =
    MongoDriver.resultToOpa(collStats(m, db, collection))

  /**
   * Create a collection.
   * TODO:  There is no such command.  See how the mongo shell does it and copy...
   **/
  createCollection(m:Mongo.db, db:string, collection:string, capped:option({capped:bool; size:int;})): Mongo.result =
    match capped with
    | {some=~{capped; size}} ->
       simple_str_command_opts(m, db, "createCollection", collection, [H.bool("capped",capped), H.i32("size",size)])
    | {none} ->
       simple_str_command(m, db, "createCollection", collection)

  /**
   * Cap a collection.
   **/
  convertToCapped(m:Mongo.db, db:string, collection:string, size:int): Mongo.result =
    simple_str_command_opts(m, db, "convertToCapped", collection, [H.i32("size",size)])

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
       (match Bson.find_int(bson, "n") with
        | {some=n} -> {success=n} 
        | {none} -> {failure={Error="Missing n value in count reply"}})
    | {~failure} -> {~failure}

  distinct(m:Mongo.db, db:string, coll:string, key:string, query_opt:option(Bson.document)): Mongo.result =
    cmd = List.flatten([[H.str("distinct",coll), H.str("key",key)],
                        (match query_opt with | {some=query} -> [H.doc("query",query)] | {none} -> [])])
    match run_command(m, db, cmd) with
    | {success=bson} ->
       //do println("MongoCursor.distinct: bson={Bson.to_pretty(bson)}")
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
    //do println("MongoCursor.group: group={Bson.to_pretty(group)}")
    match run_command(m, db, group) with
    | {success=bson} ->
       //do println("MongoCursor.group: bson={Bson.to_pretty(bson)}")
       {success=bson}
    | {~failure} -> {~failure}

  /**
   * Predicate for master status.
   **/
  ismasterp(m:Mongo.db): outcome(bool,Mongo.failure) =
    match simple_int_command(m, "admin", "ismaster", 1) with
    | {success=bson} ->
      (match Bson.find(bson,"ismaster") with
       | {some=[{name="ismaster"; value={Boolean=ismaster}}]} -> {success=ismaster}
       | _ -> {failure={Error="Missing ismaster Boolean"}})
    | {~failure} -> {~failure}

  /**
   * ismaster command as a Bson.document.
   **/
  isMaster(m:Mongo.db): Mongo.result = simple_int_command(m, "admin", "ismaster", 1)

  /**
   * Return the isMaster document as an OPA type.
   **/
  isMasterOpa(m:Mongo.db): outcome(Commands.isMaster,Mongo.failure) = adminToOpa(m,"ismaster")

  /**
   * Query the config.shards database, gives a list of shards.
   **/
  findShards(m:Mongo.db, query:Bson.document): Mongo.results = MongoCursor.find_all(m, "config.shards", query, 100)

  /**
   * Query the config.databases database, gives a list of shard information about databases.
   **/
  findDatabases(m:Mongo.db, query:Bson.document): Mongo.results = MongoCursor.find_all(m, "config.databases", query, 100)

  /**
   * Query the config.locks database, gives information about the shard balancer.
   **/
  findBalancer(m:Mongo.db): Mongo.results = MongoCursor.find_all(m, "config.locks", [H.str("_id","balancer")], 100)

  /**
   * Low-level, set config.settings balancer value.  Valid objects are "stopped" and "start/stop".
   **/
  setBalancer(m:Mongo.db, param:Bson.document): bool =
    MongoDriver.update(m,MongoDriver.UpsertBit,"config.settings",[H.str("_id","balancer")],[H.doc("$set",param)])

  /**
   * Update the balancer settings, true=stopped
   **/
  pauseBalancer(m:Mongo.db, stopped:bool): bool =
    setBalancer(m, [H.bool("stopped",stopped)])

  /**
   * Set balancer window (eg. start="09:00" stop="21:00")
   **/
  setBalancerWindow(m:Mongo.db, start:string, stop:string): bool =
    setBalancer(m, [H.doc("activeWindow",[H.str("start",start), H.str("stop",stop)])])

  /**
   * Set chunksize in MB.
   **/
  setChunkSize(m:Mongo.db, size:int): bool =
    MongoDriver.update(m,MongoDriver.UpsertBit,"config.settings",[H.str("_id","chunksize")],[H.doc("$set",[H.i32("value",size)])])

  /**
   * Query the config.chunks database, gives a information about shard distribution.
   **/
  findChunks(m:Mongo.db, query:Bson.document): Mongo.results = MongoCursor.find_all(m, "config.chunks", query, 100)

  /**
   * Add a shard to a database.
   * addShard(mongo, shard_address, optional_name, allowLocal, optional_maxSize).
   **/
  addShard(m:Mongo.db, shard:string, nameOpt:option(string), allowLocal:bool, maxSizeOpt:option(int)): Mongo.result =
    opts = List.flatten([match nameOpt with {some=name} -> [H.str("name",name)] | _ -> [],
                         if allowLocal then [H.bool("allowLocal",true)] else [],
                         match maxSizeOpt with {some=maxSize} -> [H.i32("maxSize",maxSize)] | _ -> []])
    simple_str_command_opts(m, "admin", "addShard", shard, opts)

  /**
   * Basic and practically useless remove shard command.
   * Unless the shard happens to have no databases in it, you will
   * just leave the shard in a "draining" state.  You have to
   * manually move the  chunks on the shard to another shard before
   * removal will complete.
   **/
  removeShard(m:Mongo.db, shard:string): Mongo.result =
    simple_str_command(m, "admin", "removeShard", shard)

  /** Map a list of outcomes onto an outcome of a list. **/
  Outcome_map(f:'a->outcome('b,'c), l:list('a)): outcome(list('b),'c) =
    rec aux(l) =
      match l with
      | [a|t] ->
         (match f(a) with
          | {success=b} ->
             (match aux(t) with
              | {success=l} -> {success=[b|l]}
              | {~failure} -> {~failure})
          | {~failure} -> {~failure})
      | [] -> {success=[]}
    aux(l)

  /**
   * Find a non-draining shard in a list of shards.
   * You get the list of shards from config.shards.
   **/
  find_non_draining_shard(shards:list(Bson.document)): option(string) =
    rec aux(shards) =
      match shards with
      | [shard|shards] ->
         (match Bson.find_bool(shard,"draining") with 
          | {some={true}} -> aux(shards)
          | _ -> Bson.find_string(shard,"_id"))
      | [] -> {none}
   aux(shards)

  /**
   * Complicated and possibly dangerous routine.
   * We remove a shard and detect whether it has completed or not.
   * If there are databases still left associated with the shard
   * we try to move them to a vacant shard.
   * We have to wait in a loop, we define a retry time in milliseconds
   * and a maximum number of retries.
   * Warning, moving a large chunk might take a little time.
   **/
  reallyRemoveShard(m:Mongo.db, shard:string, retryTime:int, maxRetries:int): Mongo.result =
    rec aux(time,retries) =
      if retries > maxRetries
      then {failure={Error="Commands.reallyRemoveShard: retry count exceeded"}}
      else
        do if time != 0 then Scheduler.wait(time)
        match removeShard(m, shard) with
        | {success=doc} ->
           (match Bson.dot_string(doc,"state") with
            | {some="started"} -> aux(retryTime,retries+1)
            | {some="ongoing"} ->
               (match Bson.dot_int(doc,"remaining.chunks") with
                | {some=0} | {none} ->
                   (match Bson.dot_int(doc,"remaining.dbs") with
                    | {some=0} | {none} -> {success=doc}//??failure
                    | _ ->
                       (match MongoCursor.find_all(m, "config.databases", [H.str("primary",shard)], 100) with
                        | {success=dbs} ->
                           (match MongoCursor.find_all(m, "config.shards", [], 100) with
                            | {success=[]} -> {failure={Error="No shards to move primary"}}
                            | {success=shards} ->
                               do println("dbs={Bson.to_pretty_list(dbs)}\nshards={Bson.to_pretty_list(shards)}")
                               (match find_non_draining_shard(shards) with
                                | {some=shardid} ->
                                   do println("shardid={shardid}")
                                   res = Outcome_map((dbdoc ->
                                                      (match Bson.find_string(dbdoc,"_id") with
                                                       | {some=dbname} -> movePrimary(m, dbname, shardid)
                                                       | {none} -> {failure={Error="no db _id"}})),dbs)
                                   (match res with
                                    | {success=_} -> aux(retryTime,retries+1)
                                    | {~failure} -> {~failure})
                                | {none} -> {success=doc})
                            | {~failure} -> {~failure})
                        | {~failure} -> {~failure}))
                | _ -> aux(retryTime,retries+1))
            | {some="completed"} -> {success=doc}
            | _ -> {success=doc})
        | {~failure} -> {~failure}
    aux(0,0)

  /**
   * Return the current shard version for this collection.
   **/
  getShardVersion(m:Mongo.db, collection:string): Mongo.result =
    simple_str_command(m, "admin", "getShardVersion", collection)

  /**
   * Return a list of shards.
   **/
  listShards(m:Mongo.db): Mongo.result =
    simple_int_command(m, "admin", "listShards", 1)

  /**
   * Enable sharding on the given database.
   **/
  enableSharding(m:Mongo.db, dbname:string): Mongo.result =
    simple_str_command(m, "admin", "enableSharding", dbname)

  /**
   * Enable sharding on a collection, giving the optional sharding key.
   * The bool is the unique flag.
   **/
  shardCollection(m:Mongo.db, collection:string, keyOpt:option(Bson.document), unique:bool): Mongo.result =
    opts = List.flatten([match keyOpt with {some=key} -> [H.doc("key",key)] | _ -> [],
                         if unique then [H.bool("unique",true)] else []])
    simple_str_command_opts(m, "admin", "shardCollection", collection, opts)

  /**
   * Actually split an existing chunk (does a split(find)).
   **/
  split(m:Mongo.db, collection:string, find:Bson.document): Mongo.result =
    simple_str_command_opts(m, "admin", "split", collection, [H.doc("find",find)])

  /**
   * Presplit, we just define the split point (does a split(middle)).
   **/
  presplit(m:Mongo.db, collection:string, middle:Bson.document): Mongo.result =
    simple_str_command_opts(m, "admin", "split", collection, [H.doc("middle",middle)])

  /**
   * Move a chunk from the given collection for which the select document would
   * select a document from and move to the named shard.
   **/
  moveChunk(m:Mongo.db, collection:string, find:Bson.document, to:string): Mongo.result =
    simple_str_command_opts(m, "admin", "moveChunk", collection, [H.doc("find",find), H.str("to",to)])

  /**
   * Move the primary for the given db name to the named shard.
   **/
  movePrimary(m:Mongo.db, dbname:string, to:string): Mongo.result =
    simple_str_command_opts(m, "admin", "movePrimary", dbname, [H.str("to",to)])

  /**
   * Runs an "isdbgrid" command.  Can be used to tell if we are running mongos or mongod.
   **/
  isDBGrid(m:Mongo.db): Mongo.result =
    simple_int_command(m, "admin", "isdbgrid", 1)

  /**
   * Boolean predicate for connection to a mongos (uses isDBGrid).
   **/
  isMongos(m:Mongo.db): bool =
    match isDBGrid(m) with
    | {success=doc} ->
       (match Bson.find_int(doc,"isdbgrid") with
        | {some=n} -> n != 0
        | {none} -> false)
    | _ -> false

  @private pass_digest(user:string, pass:string): string = Crypto.Hash.md5("{user}:mongo:{pass}")

  /**
   * Authentication: [add_user(mongo, "db", "user", "pass")] creates a
   * user for the given db.
   **/
  add_user(m:Mongo.db, db:string, user:string, pass:string): bool =
    digest = pass_digest(user,pass)
    bselector = [H.str("user",user)]
    bupdate = [H.doc("$set",[H.str("pwd",digest)])]
    MongoDriver.update(m,MongoDriver.UpsertBit,(db^".system.users"),bselector,bupdate)

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

// End of file commands.opa

