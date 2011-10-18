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
  ok : int
}

@server_private
Commands = {{

  /**
   * Run a "$cmd" command.
   *
   * Normally you will get {ok: 0/1} as a reply but sometimes there
   * are other elements in the reply.
   **/
  run_command(m:Mongo.db, ns:string, command:Bson.document): Mongo.result =
    match Cursor.find_one(m, ns^".$cmd", command, {none}, {none}) with
    | {success=bson} -> Cursor.check_ok(bson)
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
  getLastError(m:Mongo.db, db:string): Mongo.result =
    simple_int_command(m, db, "getLastError", 1)

  /**
   * Return the last error from database, with full options.
   **/
  getLastErrorFull(m:Mongo.db, db:string, fsync:bool, j:bool, w:int, wtimeout:int): Mongo.result =
    simple_int_command_opts(m, db, "getLastError", 1,
                            [H.bool("fsync",fsync), H.bool("j",j), H.i32("w",w), H.i32("wtimeout",wtimeout)])

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
       //do println("Cursor.distinct: bson={Bson.to_pretty(bson)}")
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
    //do println("Cursor.group: group={Bson.to_pretty(group)}")
    match run_command(m, db, group) with
    | {success=bson} ->
       //do println("Cursor.group: bson={Bson.to_pretty(bson)}")
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

// End of file commands.opa

