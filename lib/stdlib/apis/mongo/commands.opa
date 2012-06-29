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
 * Module [MongoCommands] has a set of command implementations to make MongoDB
 * management easier.  We also have some OPA types which match the structure
 * of some of the values returned by the commands.  You just have to cast
 * the result of [Bson.doc2opa] to these types to get OPA values which have all the
 * command reply data in them.
 *
 * Some of the functions occur as "fn" and "fnOpa" variants, the latter are
 * just pre-cast to the relevant return type.
 *
 * Currently, this list of commands is incomplete and the types even more so.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

/**
 * MongoCommand types:
 *
 *   These are types which are intended to match the results of MongoDB
 *   command results.  The names of the types match the names of the
 *   MongoDB server commands.
 *
 *   If we just want a single value out of the result,
 *   then it is probably more efficient to use the [MongoCommon.dotresult_type] functions.
 *   If we want to manipulate the whole thing in OPA, however, we can just
 *   [Bosn.doc2opa] the result and cast to these types.
 *
 *   Note that these types
 *   may vary between one MongoDB version and another, these are for 1.8.3.
 *
 *   Note also that some of the results have floating elements which we 
 *   map to the Bson.register type. 
 **/
//TODO: missing commands [currentOp, killOp]

type Mongo.getLastErrorOptions = {
  fsync : Bson.register(bool);
  j : Bson.register(bool);
  w : Bson.register(string); // Oh yes, use "1" for ints...
  wtimeout : Bson.register(Bson.int32);
}

type Mongo.lastError = {
  ok : int; //- true indicates the getLastError command completed successfully. This does NOT indicate there wasn't a last error.
  err : Bson.register(string); //- if non-null, indicates an error occurred. Value is a textual description of the error.
  code : Bson.register(int); //- if set, indicates the error code which occurred.
  connectionId : Bson.register(int); //- the id of the connection
  lastOp : Bson.register(Bson.document); //- the op-id from the last operation (varies, not always just an int)
  n : Bson.register(int);
}

type Mongo.ok = {
  ok : int;
}

type Mongo.isMaster = {
  ismaster : bool;
  msg : Bson.register(string);
  me : Bson.register(string);
  setName : Bson.register(string);
  primary : Bson.register(string);
  secondary : Bson.register(bool);
  hosts : Bson.register(list(string));
  passives : Bson.register(list(string));
  arbiters : Bson.register(list(string));
  maxBsonObjectSize : int;
  ok : int;
}

type Mongo.dropDatabaseType = { dropped : string; ok : int; }

type Mongo.listDatabasesType = {
  databases : list({ name : string; sizeOnDisk : float; empty : bool; })
  totalSize : float;
  ok : int;
}

type Mongo.serverStatusType = {
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
  repl : Bson.register({ setName : Bson.register(string);
                         ismaster : bool;
                         secondary : Bson.register(bool);
                         hosts : Bson.register(list(string));
                         primary : Bson.register(string);
                       });
  ok : int;
}

type Mongo.collStatsType = {
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

type Mongo.explainType =
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
    allPlans : list({ cursor : string; indexBounds : Bson.document }); // and this
    shards : Bson.register(Bson.document);
    // oldPlan: ... sometimes present
}

type Mongo.mapReduceType =
  { result : Bson.meta;
    timeMillis : int;
    counts : { input : int; emit : int; output : int; reduce : Bson.register(int) };
    ok : int
  }

type Mongo.mapReduceOptions = {
  query: option(Bson.document);
  sort: option(Bson.document);
  limit: option(int);
  keeptemp: option(bool);
  finalize: option(string);
  scope: option(Bson.document);
  jsMode: option(bool);
  verbose: option(bool)
}

@server_private
MongoCommands = {{

  @private H = Bson.Abbrevs

  /**
   * Run a "$cmd" command.
   *
   * Normally you will get [\{ok: 0/1\}] as a reply but sometimes there
   * are other elements in the reply.
   **/
  run_command_ll(m:Mongo.db, ns:string, command:Bson.document): Mongo.result =
    match MongoCursor.find_one(m, ns^".$cmd", command, {none}, []) with
    | {success=bson} -> MongoCommon.check_ok(bson)
    | {~failure} -> {~failure}

  run_command(m:Mongo.mongodb, ns:string, command:Bson.document): Mongo.result =
    run_command_ll(m.mongo, ns, command)

  /**
   * Perform a simple integer command, eg. [\{ ping : 1 \}]
   **/
  simple_int_command_ll(m:Mongo.db, ns:string, cmd:string, arg:int): Mongo.result =
    run_command_ll(m, ns, [H.i32(cmd,arg)])
  simple_int_command(m:Mongo.mongodb, ns:string, cmd:string, arg:int): Mongo.result =
    run_command(m, ns, [H.i32(cmd,arg)])

  /**
   * Same as simple integer command but with options, eg. [\{ "getlasterror" : 1, w : 3, wtimeout : 10000 \}]
   **/
  simple_int_command_opts(m:Mongo.mongodb, ns:string, cmd:string, arg:int, opts:Bson.document): Mongo.result =
    run_command(m, ns, List.flatten([[H.i32(cmd,arg)],opts]))

  /**
   * Perform a simple integer command, eg. [\{ drop : "collection" \}]
   **/
  simple_str_command(m:Mongo.mongodb, ns:string, cmd:string, arg:string): Mongo.result =
    run_command(m, ns, [H.str(cmd,arg)])

  /**
   * Perform a simple integer command, eg. [\{ drop : "collection" \}]
   **/
  simple_str_command_opts(m:Mongo.mongodb, ns:string, cmd:string, arg:string, opts:Bson.document): Mongo.result =
    run_command(m, ns, List.flatten([[H.str(cmd,arg)],opts]))

  /**
   * Run a [simple_int_command] but pass the result through [Bson.doc2opa],
   * using the type that this function is cast to at the point of call.
   **/
  dbToOpa(m:Mongo.mongodb, dbname:string, command:string): outcome('a,Mongo.failure) =
    match simple_int_command(m,dbname,command,1) with
    | {success=doc} ->
       (match MongoCommon.result_to_opa({success=doc}) with
        | {some=ism} -> {success=ism}
        | {none} -> {failure={Error="Mongo.{command}: invalid document from db {dbname} ({Bson.to_pretty(doc)})"}})
    | {~failure} -> {~failure}

  /** Perform [dbToOpa] on the "admin" database. **/
  adminToOpa(m:Mongo.mongodb, command:string): outcome('a,Mongo.failure) = dbToOpa(m,"admin",command)

  /** Perform [dbToOpa] on the "config" database. **/
  configToOpa(m:Mongo.mongodb, command:string): outcome('a,Mongo.failure) = dbToOpa(m,"config",command)

  /**
   * Predicate for connection alive.  Peforms an admin "ping" command.
   * We insist upon a single [\{ok:1\}] reply, anything else results in [false].
   **/
  check_connection(m:Mongo.mongodb): outcome(bool,Mongo.failure) =
    match simple_int_command(m, "admin", "ping", 1) with
    | {success=[e]} -> {success=Option.default(false,Bson.find_bool([e],"ok"))}
    | {success=doc} -> {failure={DocError=doc}}
    | {~failure} -> {~failure}

  /**
   * Drop a database.
   **/
  dropDatabase(m:Mongo.mongodb, db:string): Mongo.result =
    simple_int_command(m, db, "dropDatabase", 1)

  /**
   * Drop a collection from a database [drop("db","collection")]
   **/
  drop(m:Mongo.mongodb, db:string, collection:string): Mongo.result =
    simple_str_command(m, db, "drop", collection)

  /**
   * Drop an index from a collection [dropIndexes("db","collection","index")]
   **/
  dropIndexes(m:Mongo.mongodb, db:string, collection:string, index:string): Mongo.result =
    simple_str_command_opts(m, db, "drop", collection, [H.str("index",index)])

  /**
   * List valid commands for database.
   **/
  listCommands(m:Mongo.mongodb, db:string): Mongo.result =
    simple_int_command(m, db, "listCommands", 1)

  /**
   * List all databases.
   **/
  listDatabases(m:Mongo.mongodb): Mongo.result =
    simple_int_command(m, "admin", "listDatabases", 1)

  /**
   * Statistics for server.
   **/
  serverStatus(m:Mongo.mongodb): Mongo.result =
    simple_int_command(m, "admin", "serverStatus", 1)

  /**
   * Rename a collection [renameCollection("db","collection","index")]
   **/
  renameCollection(m:Mongo.mongodb, from:string, to:string): Mongo.result =
    simple_str_command_opts(m, "admin", "renameCollection", from, [H.str("to",to)])

  /**
   * Repair the database (slow, write-locked).
   **/
  repairDatabase(m:Mongo.mongodb): Mongo.result =
    simple_int_command(m, "admin", "repairDatabase", 1)

  /**
   * Return the last error from database.
   **/
  getLastError(m:Mongo.mongodb, db:string): Mongo.result = simple_int_command(m, db, "getlasterror", 1)
  getLastErrorOpa(m:Mongo.mongodb, db:string): Mongo.error = MongoCommon.error_of_result(getLastError(m, db))

  /**
   * Return the last error from database, with full options.
   *
   * Example: [getLastErrorFull(m, db, fsync, j, w, wtimeout)]
   **/
  getLastErrorFull(m:Mongo.mongodb, db:string, fsync:bool, j:bool, w:int, wtimeout:int): Mongo.result =
    simple_int_command_opts(m, db, "getlasterror", 1,
                            [H.bool("fsync",fsync), H.bool("j",j), H.i32("w",w), H.i32("wtimeout",wtimeout)])
  getLastErrorFullOpa(m:Mongo.mongodb, db:string, fsync:bool, j:bool, w:int, wtimeout:int): Mongo.error =
    MongoCommon.error_of_result(getLastErrorFull(m, db, fsync, j, w, wtimeout))

  /**
   * Reset database error status.
   **/
  resetError(m:Mongo.mongodb, db:string): Mongo.result =
    simple_int_command(m, db, "resetError", 1)

  /**
   * Force a db error.
   **/
  forceError(m:Mongo.mongodb, db:string): Mongo.result =
    simple_int_command(m, db, "forceError", 1)

  /**
   * Return information about the server.
   **/
  buildInfo(m:Mongo.mongodb): Mongo.result =
    simple_int_command(m, "admin", "buildInfo", 1)

  /**
   * Return collection statistics.
   *
   * Example: [collStats(m, db, collection)]
   **/
  collStats(m:Mongo.mongodb, db:string, collection:string): Mongo.result =
    simple_str_command(m, db, "collStats", collection)
  collStatsOpa(m:Mongo.mongodb, db:string, collection:string): outcome(Mongo.collStatsType,Mongo.failure) =
    MongoCommon.resultToOpa(collStats(m, db, collection))

  /**
   * Create a collection.
   **/
  createCollection(m:Mongo.mongodb, dbname:string, collection:string, capped:option(bool), size:option(int)): Mongo.result =
    opts = List.flatten([match capped with {some=tf} -> [H.bool("capped",tf)] | _ -> [],
                         match size with {some=size} -> [H.i32("size",size)] | _ -> []])
    simple_str_command_opts(m, dbname, "create", collection, opts)

  /**
   * Cap a collection.  Example: [convertToCapped(m, db, collection, size)]
   **/
  convertToCapped(m:Mongo.mongodb, db:string, collection:string, size:int): Mongo.result =
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
   *
   * Example: [count(m, db, coll, query_opt)]
   **/
  count(m:Mongo.mongodb, db:string, coll:string, query_opt:option(Bson.document)): outcome(int,Mongo.failure) =
    cmd = List.flatten([[H.str("count",coll)],
                        (match query_opt with | {some=query} -> [H.doc("query",query)] | {none} -> [])])
    match run_command(m, db, cmd) with
    | {success=bson} ->
       (match Bson.find_int(bson, "n") with
        | {some=n} -> {success=n} 
        | {none} -> {failure={Error="Missing n value in count reply"}})
    | {~failure} -> {~failure}

  /**
   * MongoDB aggregation.
   *
   * Example: [distinct(m, db, collection, key, query_opt)]
   * @return An array document with the list of values.
   *
   * Currently, the [stats] field is ignored.
   **/
  distinct(m:Mongo.mongodb, db:string, coll:string, key:string, query_opt:option(Bson.document)): Mongo.result =
    cmd = List.flatten([[H.str("distinct",coll), H.str("key",key)],
                        (match query_opt with | {some=query} -> [H.doc("query",query)] | {none} -> [])])
    match run_command(m, db, cmd) with
    | {success=bson} ->
       // TODO: stats
       (match Bson.find(bson,"values") with
        | {some=[{name=k; value={Array=d}}]} -> {success=[H.arr(k,List.rev(d))]}
        | _ -> {failure={DocError=bson}})
    | {~failure} -> {~failure}

  /**
   * A more advanced aggregation function.
   *
   * Example: [group(m, db, collection, key, reduce, initial, cond_opt, finalize_opt)]
   * @param reduce is Javascript in the form of a string
   * @param finalize_opt as is finalize.
   **/
  group(m:Mongo.mongodb, db:string, coll:string, key:Bson.document, reduce:string, initial:Bson.document,
        cond_opt:option(Bson.document), finalize_opt:option(string)): Mongo.result =
    group =
      [H.doc("group",
         List.flatten([
           [H.str("ns",coll), H.doc("key",key), H.code("$reduce",reduce), H.doc("initial",initial)],
           (match cond_opt with | {some=cond} -> [H.doc("cond",cond)] | {none} -> [H.null("cond")]),
           (match finalize_opt with | {some=finalize} -> [H.code("finalize",finalize)] | {none} -> [])]))]
    run_command(m, db, group)

  /**
   * Predicate for master status.  Runs an "ismaster" command.
   **/
  ismasterp(m:Mongo.mongodb): outcome(bool,Mongo.failure) =
    match simple_int_command(m, "admin", "ismaster", 1) with
    | {success=bson} ->
      (match Bson.find(bson,"ismaster") with
       | {some=[{name="ismaster"; value={Boolean=ismaster}}]} -> {success=ismaster}
       | _ -> {failure={Error="Missing ismaster Boolean"}})
    | {~failure} -> {~failure}

  /**
   * [ismaster] command result as a [Bson.document].
   **/
  isMaster(m:Mongo.mongodb): Mongo.result = simple_int_command(m, "admin", "ismaster", 1)

  /**
   * Return the [isMaster] document as an OPA type.
   **/
  isMasterOpa(m:Mongo.mongodb): outcome(Mongo.isMaster,Mongo.failure) = adminToOpa(m,"ismaster")

  /**
   * Query the "config.shards" database, gives a list of shards.
   **/
  findShards(m:Mongo.mongodb, query:Bson.document, limit:int): Mongo.results =
    MongoCursor.find_all(m.mongo, "config.shards", query, {none}, [], limit)

  /**
   * Query the "config.databases" database, gives a list of shard information about databases.
   **/
  findDatabases(m:Mongo.mongodb, query:Bson.document, limit:int): Mongo.results =
    MongoCursor.find_all(m.mongo, "config.databases", query, {none}, [], limit)

  /**
   * Query the "config.locks" database, gives information about the shard balancer.
   **/
  findBalancer(m:Mongo.mongodb, limit:int): Mongo.results =
    MongoCursor.find_all(m.mongo, "config.locks", [H.str("_id","balancer")], {none}, [], limit)

  /**
   * Low-level, set "config.settings" balancer value.  Valid objects are "stopped" and "start/stop".
   **/
  setBalancer(m:Mongo.mongodb, param:Bson.document): bool =
    MongoDriver.update(m.mongo,MongoCommon.UpsertBit,"config.settings",[H.str("_id","balancer")],[H.doc("$set",param)])

  /**
   * Update the balancer settings, [true]=stopped
   **/
  pauseBalancer(m:Mongo.mongodb, stopped:bool): bool =
    setBalancer(m, [H.bool("stopped",stopped)])

  /**
   * Set balancer window (eg. start="09:00" stop="21:00")
   **/
  setBalancerWindow(m:Mongo.mongodb, start:string, stop:string): bool =
    setBalancer(m, [H.doc("activeWindow",[H.str("start",start), H.str("stop",stop)])])

  /**
   * Set chunksize in MB.
   **/
  setChunkSize(m:Mongo.mongodb, size:int): bool =
    MongoDriver.update(m.mongo,MongoCommon.UpsertBit,"config.settings",
                       [H.str("_id","chunksize")],[H.doc("$set",[H.i32("value",size)])])

  /**
   * Query the "config.chunks" database, gives a information about shard distribution.
   **/
  findChunks(m:Mongo.mongodb, query:Bson.document, limit:int): Mongo.results =
    MongoCursor.find_all(m.mongo, "config.chunks", query, {none}, [], limit)

  /**
   * Add a shard to a database.
   *
   * Example: [addShard(mongo, shard_address, name_opt, allowLocal, maxSizeOpt)].
   **/
  addShard(m:Mongo.mongodb, shard:string, nameOpt:option(string), allowLocal:bool, maxSizeOpt:option(int)): Mongo.result =
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
  removeShard(m:Mongo.mongodb, shard:string): Mongo.result =
    simple_str_command(m, "admin", "removeShard", shard)

  /**
   * Find a non-draining shard in a list of shards.
   * You get the list of shards from the "config.shards" collection.
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
   * Since we have to wait in a loop, we define a retry time in milliseconds
   * and a maximum number of retries.
   *
   * {b Warning, moving a large chunk might take a little time.}
   *
   * Example: [reallyRemoveShard(m, shard, retryTime, maxRetries)]
   **/
  reallyRemoveShard(m:Mongo.mongodb, shard:string, retryTime:int, maxRetries:int): Mongo.result =
    rec aux(time,retries) =
      if retries > maxRetries
      then {failure={Error="Mongo.reallyRemoveShard: retry count exceeded"}}
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
                       (match MongoCursor.find_all(m.mongo, "config.databases",
                                                   [H.str("primary",shard)], {none}, [], 100) with
                        | {success=dbs} ->
                           (match MongoCursor.find_all(m.mongo, "config.shards", [], {none}, [], 100) with
                            | {success=[]} -> {failure={Error="No shards to move primary"}}
                            | {success=shards} ->
                               do println("dbs={Bson.to_pretty_list(dbs)}\nshards={Bson.to_pretty_list(shards)}")
                               (match find_non_draining_shard(shards) with
                                | {some=shardid} ->
                                   do println("shardid={shardid}")
                                   res = MongoCommon.Outcome_list((dbdoc ->
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
  getShardVersion(m:Mongo.mongodb, collection:string): Mongo.result =
    simple_str_command(m, "admin", "getShardVersion", collection)

  /**
   * Return a list of shards.
   **/
  listShards(m:Mongo.mongodb): Mongo.result =
    simple_int_command(m, "admin", "listShards", 1)

  /**
   * Enable sharding on the given database.
   **/
  enableSharding(m:Mongo.mongodb, dbname:string): Mongo.result =
    simple_str_command(m, "admin", "enableSharding", dbname)

  /**
   * Enable sharding on a collection, giving the optional sharding key.
   * The bool is the unique flag.
   *
   * Example: [shardCollection(m, collection, keyOpt, unique)]
   **/
  shardCollection(m:Mongo.mongodb, collection:string, keyOpt:option(Bson.document), unique:bool): Mongo.result =
    opts = List.flatten([match keyOpt with {some=key} -> [H.doc("key",key)] | _ -> [],
                         if unique then [H.bool("unique",true)] else []])
    simple_str_command_opts(m, "admin", "shardCollection", collection, opts)

  /**
   * Actually split an existing chunk (does a split(find)).
   * Example: [split(m, collection, find)]
   **/
  split(m:Mongo.mongodb, collection:string, find:Bson.document): Mongo.result =
    simple_str_command_opts(m, "admin", "split", collection, [H.doc("find",find)])

  /**
   * Presplit, we just define the split point (does a split(middle)).
   * Example: [split(m, collection, middle)]
   **/
  presplit(m:Mongo.mongodb, collection:string, middle:Bson.document): Mongo.result =
    simple_str_command_opts(m, "admin", "split", collection, [H.doc("middle",middle)])

  /**
   * Move a chunk from the given collection for which the select document would
   * select a document from and move to the named shard.
   * Example: [moveChunk(m, collection, find, to)]
   **/
  moveChunk(m:Mongo.mongodb, collection:string, find:Bson.document, to:string): Mongo.result =
    simple_str_command_opts(m, "admin", "moveChunk", collection, [H.doc("find",find), H.str("to",to)])

  /**
   * Move the primary for the given db name to the named shard.
   * Example: [movePrimary(m, dbname, to)]
   **/
  movePrimary(m:Mongo.mongodb, dbname:string, to:string): Mongo.result =
    simple_str_command_opts(m, "admin", "movePrimary", dbname, [H.str("to",to)])

  /**
   * Runs an "isdbgrid" command.  Can be used to tell if we are running mongos or mongod.
   **/
  isDBGrid(m:Mongo.mongodb): Mongo.result =
    simple_int_command(m, "admin", "isdbgrid", 1)

  /**
   * Boolean predicate for connection to a mongos (uses [isDBGrid]).
   **/
  isMongos(m:Mongo.mongodb): bool =
    match isDBGrid(m) with
    | {success=doc} ->
       (match Bson.find_int(doc,"isdbgrid") with
        | {some=n} -> n != 0
        | {none} -> false)
    | _ -> false

  @private
  fAM(m:Mongo.mongodb, dbname:string, collection:string, query:Bson.document,
      update_opt:option(Bson.document), new_opt:option(bool),
      remove_opt:option(bool), sort_opt:option(Bson.document)): Mongo.result =
    cmd = List.flatten([[H.str("findAndModify",collection), H.doc("query",query)],
                        (match update_opt with | {some=update} -> [H.doc("update",update)] | {none} -> []),
                        (match new_opt with | {some=new} -> [H.bool("new",new)] | {none} -> []),
                        (match remove_opt with | {some=remove} -> [H.bool("remove",remove)] | {none} -> []),
                        (match sort_opt with | {some=sort} -> [H.doc("sort",sort)] | {none} -> [])])
    match run_command(m, dbname, cmd) with
    | {success=doc} ->
       if (match Bson.find_int(doc,"ok") with | {some=ok} -> ok == 1 | {none} -> false)
       then
         (match Bson.find_doc(doc,"value") with
          | {some=value} -> {success=value}
          | _ -> {failure={Error="MongoCommands.findAndModify: No value field in reply"}})
       else {failure={DocError=doc}}
    | {~failure} -> {~failure}

  /**
   * Atomic operation, perform query and update document if found.
   * This is a generic version, you can only have either "update" or "remove".
   * @param dbname name of the database
   * @param collection name of the collection
   * @param query query document
   * @param update optional update document
   * @param new optional bool, when [true] return the updated document, otherwise the pre-updated document
   * @param remove if [true], delete the selected document
   * @param sort optional sort document
   * @return an outcome of either failure or the "value" field in the reply
   **/
  findAndModify(m,dbname,collection,query,update_opt,new_opt,remove_opt,sort_opt) =
    if Option.is_some(update_opt) && Option.is_some(remove_opt)
    then {failure={Error="MongoCommands.findAndModify: Can't have both update and remove"}}
    else if Option.is_none(update_opt) && Option.is_none(remove_opt)
    then {failure={Error="MongoCommands.findAndModify: Either update or remove are required"}}
    else fAM(m,dbname,collection,query,update_opt,new_opt,remove_opt,sort_opt)

  /** Same as [findAndModify] but convert to OPA type **/
  findAndModifyOpa(m,dbname,collection,query,update_opt,new_opt,remove_opt,sort_opt): outcome('a,Mongo.failure) =
    MongoCommon.resultToOpa(findAndModify(m,dbname,collection,query,update_opt,new_opt,remove_opt,sort_opt))

  /** Update-specific version of the [findAndModify] command **/
  findAndUpdate(m,dbname,collection,query,update,new_opt,sort_opt) =
    fAM(m,dbname,collection,query,{some=update},new_opt,{none},sort_opt)

  /** Same as [findAndUpdate] but convert to OPA type **/
  findAndUpdateOpa(m,dbname,collection,query,update,new_opt,sort_opt) =
    MongoCommon.resultToOpa(findAndUpdate(m,dbname,collection,query,update,new_opt,sort_opt))

  /** Remove-specific version of the [findAndModify] command **/
  findAndRemove(m,dbname,collection,query,remove,new_opt,sort_opt) =
    fAM(m,dbname,collection,query,{none},new_opt,{some=remove},sort_opt)

  /** Same as [findAndRemove] but convert to OPA type **/
  findAndRemoveOpa(m,dbname,collection,query,remove,new_opt,sort_opt) =
    MongoCommon.resultToOpa(findAndRemove(m,dbname,collection,query,remove,new_opt,sort_opt))

  /**
   * A bare no-frills mapReduce function.  We ask for all of the options, including
   * the complicated "out" option and we do minimal processing on the result, basically
   * if [ok] is "1" then we return the entire reply as a success, otherwise we return
   * the whole document as a failure.  Some processing on the options and results may
   * be added later.
   **/
  mapReduce(m:Mongo.mongodb, map:string, reduce:string,
            query_opt:option(Bson.document),
            sort_opt:option(Bson.document),
            limit_opt:option(int),
            out_opt:option(Bson.meta),
            keeptemp_opt:option(bool),
            finalize_opt:option(string),
            scope_opt:option(Bson.document),
            jsMode_opt:option(bool),
            verbose_opt:option(bool)): Mongo.result =
    cmd = List.flatten([[H.str("mapReduce",m.collection), H.code("map",map), H.code("reduce",reduce) ],
                        (match query_opt with | {some=query} -> [H.doc("query",query)] | {none} -> []),
                        (match sort_opt with | {some=sort} -> [H.doc("sort",sort)] | {none} -> []),
                        (match limit_opt with | {some=limit} -> [H.i32("limit",limit)] | {none} -> []),
                        (match out_opt with | {some=value} -> [H.v("out",value)] | {none} -> []),
                        (match keeptemp_opt with | {some=keeptemp} -> [H.bool("keeptemp",keeptemp)] | {none} -> []),
                        (match finalize_opt with | {some=finalize} -> [H.code("finalize",finalize)] | {none} -> []),
                        (match scope_opt with | {some=scope} -> [H.doc("scope",scope)] | {none} -> []),
                        (match jsMode_opt with | {some=jsMode} -> [H.bool("jsMode",jsMode)] | {none} -> []),
                        (match verbose_opt with | {some=verbose} -> [H.bool("verbose",verbose)] | {none} -> [])])
    run_command(m, m.dbname, cmd)

  /**
   * Default options for [mapReduce], all [\{none\}].
   **/
  mapReduceOptions =
    ({ query={none};
       sort={none};
       limit={none};
       keeptemp={none};
       finalize={none};
       scope={none};
       jsMode={none};
       verbose={none};
     }:Mongo.mapReduceOptions)

  /** Abbreviation for mapReduceOpts **/
  mRO = mapReduceOptions

  /**
   * Full mapReduce but handling options with the [Mongo.mapReduceOpts] type.
   **/
  mapReduceOpts(m:Mongo.mongodb, map:string, reduce:string, out:Bson.meta, opts:Mongo.mapReduceOptions)
              : Mongo.result =
    mapReduce(m, map, reduce,
              opts.query, opts.sort, opts.limit,
              {some=out}, opts.keeptemp, opts.finalize, opts.scope, opts.jsMode, opts.verbose)

  /**
   * Simplest possible mapReduce, only define [map], [reduce] and [out].
   **/
  mapReduceSimple(m:Mongo.mongodb, map:string, reduce:string, out:Bson.meta): Mongo.result =
    mapReduceOpts(m, map, reduce, out, mapReduceOptions)

  /**
   * Convert the return value from mapReduce into an OPA type.
   * Probably not very useful since mapReduce only returns "ok", "result" (where
   * the output was placed) and some statistics.
   **/
  mapReduceOpa(m, map, reduce,
               query_opt, sort_opt, limit_opt, out_opt, keeptemp_opt,
               finalize_opt, scope_opt, jsMode_opt, verbose_opt): Mongo.valresult(Mongo.mapReduceType) =
    match mapReduce(m, map, reduce,
                    query_opt, sort_opt, limit_opt, out_opt, keeptemp_opt,
                    finalize_opt, scope_opt, jsMode_opt, verbose_opt) with
    | {success=doc} ->
       (match MongoCommon.result_to_opa({success=doc}) with
        | {some=mRT} -> {success=mRT}
        | {none} -> {failure={Error="Mongo.mapReduceOpa: invalid document ({Bson.to_pretty(doc)})"}})
    | {~failure} -> {~failure}

  /** Same as [mapReduceOpts] but returns OPA type. **/
  mapReduceOptsOpa(m:Mongo.mongodb, map:string, reduce:string, out:Bson.meta, opts:Mongo.mapReduceOptions)
                 : Mongo.valresult(Mongo.mapReduceType) =
    mapReduceOpa(m, map, reduce,
              opts.query, opts.sort, opts.limit,
              {some=out}, opts.keeptemp, opts.finalize, opts.scope, opts.jsMode, opts.verbose)

  /** Same as [mapReduceSimple] but returns OPA type. **/
  mapReduceSimpleOpa(m:Mongo.mongodb, map:string, reduce:string, out:Bson.meta)
                   : Mongo.valresult(Mongo.mapReduceType) =
    mapReduceOptsOpa(m, map, reduce, out, mapReduceOptions)

  /**
   * Evaluate Javascript code.
   *
   * Example: [eval(mongodb, dbname, code, args_opt, nolock_opt)]
   **/
  eval(m:Mongo.mongodb, dbname:string, code:Bson.code, args_opt:option(list(Bson.value)), nolock_opt:option(bool))
     : Mongo.result =
    cmd = List.flatten([[H.code("$eval",code)],
                        (match args_opt with | {some=args} -> [H.valarr("args",args)] | {none} -> []),
                        (match nolock_opt with | {some=nolock} -> [H.bool("nolock",nolock)] | {none} -> [])])
    run_command(m, dbname, cmd)

  @private pass_digest(user:string, pass:string): string = Crypto.Hash.md5("{user}:mongo:{pass}")

  /**
   * Authentication: [add_user(mongo, "db", "user", "pass")] creates a
   * user for the given db.
   **/
  add_user(m:Mongo.mongodb, db:string, user:string, pass:string): bool =
    digest = pass_digest(user,pass)
    bselector = [H.str("user",user)]
    bupdate = [H.doc("$set",[H.str("pwd",digest)])]
    MongoDriver.update(m.mongo,MongoCommon.UpsertBit,(db^".system.users"),bselector,bupdate)

  /**
   * Authenticate a user for the given database.
   *
   * The password must match the users password.
   **/
  authenticate(m:Mongo.mongodb, db:string, user:string, pass:string): Mongo.result =
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

  /**
   * Authenticate low-level, needed by the driver during reconnect (and reauthenticate).
   **/
  authenticate_ll(m:Mongo.db, db:string, user:string, pass:string): Mongo.result =
    match simple_int_command_ll(m, db, "getnonce", 1) with
    | {success=bson} ->
      (match Bson.find_element(bson,"nonce") with
       | {some={name="nonce"; value={String=nonce}}} ->
         digest = pass_digest(user,pass)
         hash = Crypto.Hash.md5("{nonce}{user}{digest}")
         cmd = [H.i32("authenticate",1), H.str("user",user), H.str("nonce",nonce), H.str("key",hash)]
         run_command_ll(m,db,cmd)
       | _ -> {failure={Error="Missing nonce String"}})
    | {~failure} -> {~failure}

}}

// End of file commands.opa

