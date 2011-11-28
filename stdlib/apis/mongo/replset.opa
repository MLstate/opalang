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
 * Module [MongoReplicaSet] allows the management of replica sets.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

/**
 * Some additional OPA types mapped to [Bson.document] values
 * to help with interpreting MongoDB replies.
 **/

type Mongo.replSetGetStatus =
{
    set : string;
    date : Date.date;
    myState : Bson.int32;
    members : list({_id : Bson.register(int);
                    name : string;
                    self : Bson.register(bool);
                    errmsg : Bson.register(string);
                    health : Bson.register(Bson.int32);
                    state : Bson.register(Bson.int32);
                    stateStr : Bson.register(string);
                    uptime : Bson.register(Bson.int32);
                    optime : Bson.register(Bson.timestamp);
                    optimeDate : Bson.register(Date.date);
                    lastHeartbeat : Bson.register(Date.date);
                    pingMs : Bson.register(Bson.int32);
                   });
    ok : bool;
    errmsg : Bson.register(string);
}

type Mongo.member = {
  _id : Bson.int32;
  host : string;
  arbiterOnly : Bson.register(bool);
  buildIndexes : Bson.register(bool);
  hidden : Bson.register(bool);
  priority: Bson.register(float);
  tags: Bson.register(Bson.document); // Aaarghh {"any":...; "tag":...;}
  slaveDelay : Bson.register(Bson.int32);
  votes : Bson.register(Bson.int32);
}

type Mongo.replSetInitiate =
{
  _id : string;
  members: list(Mongo.member);
  settings: Bson.register({
    getLastErrorDefaults : Bson.register(Mongo.getLastErrorOptions);
    getlasterrormodes : Bson.register(Bson.document); // relates to tags
  });
}

MongoReplicaSet = {{

  @private ML = MongoLog
  @private H = Bson.Abbrevs

  /**
   * Freeze a replica set (can't become primary for the given number of seconds).
   * Note: unfreeze with 0.
   **/
  replSetFreeze(m:Mongo.mongodb, seconds:int): Mongo.result =
    MongoCommands.simple_int_command(m, "admin", "replSetFreeze", seconds)

  /**
   * Step down from primary status.  Same time value as for [replSetFreeze].
   **/
  replSetStepDown(m:Mongo.mongodb, seconds:int): Mongo.result =
    MongoCommands.simple_int_command(m, "admin", "replSetStepDown", seconds)

  /**
   * Get replica get status.
   **/
  replSetGetStatus(m:Mongo.mongodb): Mongo.result =
    MongoCommands.simple_int_command(m, "admin", "replSetGetStatus", 1)
  replSetGetStatusOpa(m:Mongo.mongodb): outcome(Mongo.replSetGetStatus,Mongo.failure) =
    MongoCommands.adminToOpa(m,"replSetGetStatus")

  /**
   * Initalise a replica set.
   **/
  // TODO: test this function
  simpleConfig(id:int, host:string): Mongo.member =
    { _id=id; ~host;
      arbiterOnly={absent}; buildIndexes={absent}; hidden={absent}; priority={absent};
      tags={absent}; slaveDelay={absent}; votes={absent}
    }

  /**
   * Initialize a replica set with the given list of members (host, port) pairs.
   * Example: [replSetInitiate(m, id, members)]
   **/
  replSetInitiate(m:Mongo.mongodb, id:string, members:list((int,string))): Mongo.result =
    config = Bson.opa2doc({ _id=id; members=List.map(((id,host) -> simpleConfig(id,host)),members); settings={absent} })
    MongoCommands.run_command(m, "admin", [H.doc("replSetInitiate",config)])

  /*
   * This one will be tricky to implement, it closes the connection.
   * TODO: implement a command_with_no_reply_and_reconnect() function.
   */
  //replSetReconfig(m:Mongo.db, id:string, members:list((int,string))): Mongo.result =
  //  config = Bson.opa2doc({ _id=id; members=List.map(((id,host) -> simpleConfig(id,host)),members); settings={absent} })
  //  MongoCommands.run_command(m, "admin", [H.doc("replSetReconfig",config)])

  /**
   * Add a seed to a [Mongo.db] value.  Doesn't perform any communications.
   * Example: [add_seed(m, host, port)]
   **/
  add_seed(m:Mongo.db, host:string, port:int): Mongo.db = {m with seeds=[(host,port)|m.seeds]}

  /**
   * Remove a seed from a [Mongo.db] value.  Doesn't perform any communications.
   * Example: [remove_seed(m, host, port)]
   **/
  remove_seed(m:Mongo.db, host:string, port:int): Mongo.db = {m with seeds=List.filter((s -> s != (host,port)),m.seeds)}

  /**
   * Initialize a [Mongo.db] connection using the given list of seeds.
   **/
  init(name:string, bufsize:int, pool_max:int, close_socket:bool, log:bool, seeds:list(Mongo.mongo_host)): Mongo.db =
    m = MongoDriver.init(bufsize, pool_max, close_socket, log)
    {m with ~seeds; hosts=Mutable.make([]); ~name}

  /**
   * Initialize a [Mongo.db] connection using a single seed.
   **/
  init_single(name:string, bufsize:int, pool_max:int, close_socket:bool, log:bool, seed:Mongo.mongo_host): Mongo.db =
    init(name,bufsize,pool_max,close_socket,log,[seed])

  /**
   * Generate a [Mongo.mongo_host] value from a string: "host:port".
   * If the port is missing it will be set to [Mongo.default_port].
   **/
  mongo_host_of_string(s:string): Mongo.mongo_host =
    match String.explode(":",s) with
    | [host|[port|[]]] -> (host,Int.of_string(port))
    | _ -> (s,MongoDriver.default_port)

  @private
  adminCommandOpaLL(m:Mongo.db, cmd:string): outcome('a,Mongo.failure) =
    match MongoCommands.simple_int_command_ll(m,"admin",cmd,1) with
    | {success=doc} ->
       (match MongoCommon.result_to_opa({success=doc}) with
        | {some=a} -> {success=a}
        | {none} -> {failure={Error="MongoReplicaSet.adminCommandOpaLL: invalid document from db admin ({Bson.to_pretty(doc)})"}})
    | {~failure} -> {~failure}

  @private
  adminCommandLL(m:Mongo.db, cmd:string): Mongo.result =
    MongoCommands.simple_int_command_ll(m,"admin",cmd,1)

  isMasterOpaLL(m:Mongo.db): outcome(Mongo.isMaster,Mongo.failure) = adminCommandOpaLL(m,"ismaster")
  resetErrorOpaLL(m:Mongo.db): outcome(Mongo.ok,Mongo.failure) = adminCommandOpaLL(m,"reseterror")
  getLastErrorOpaLL(m:Mongo.db): outcome(Mongo.lastError,Mongo.failure) = adminCommandOpaLL(m,"getlasterror")
  getLastErrorLL(m:Mongo.db): Mongo.result = adminCommandLL(m,"getlasterror")

  /**
   * Try to get the list of hosts from a given list of seeds by connecting
   * in turn to each seed until we find a live one.
   **/
  check_seed(m:Mongo.db): Mongo.db =
    match isMasterOpaLL(m) with
    | {success=ism} ->
       (match ism.hosts with
        | {present=hosts} ->
           hosts = (List.filter(((_,p) -> p != 0),List.map(mongo_host_of_string,hosts)))
           do if m.log then ML.info("MongoReplicaSet.check_seed","hosts={hosts}",void)
           do m.hosts.set(hosts)
           m
        | {absent} -> m)
    | {failure=_} -> m

  /**
   * Connect (and reconnect) to a replica set.
   *
   * Follows the procedure indicated by the MongoDB website.  Try each seed
   * in turn until a list of hosts is found.  Then try each host in turn until
   * the primary server is found.  Non-primary hosts usually give you the name
   * of the primary host so we can jump straight to it.
   *
   * Implementation note.  We recurse between this routine and [MongoDriver.reconnect].
   * In theory, we could have unbounded recursion so the recursion depth is limited.
   * In practice, this should never happen.
   **/
  connect(m:Mongo.db): outcome(Mongo.db,Mongo.failure) =
    do if m.seeds == [] then ML.fatal("MongoReplicaSet.connect","Tried to connect with no seeds",-1) else void
    rec aux(m, seeds) =
      match seeds with
      | [seed|rest] ->
        (match MongoDriver.connect(m, seed.f1, seed.f2) with
         | {success=m} ->
            m = check_seed(m)
            if m.hosts.get() == []
            then aux(m,rest)
            else {success=m}
         | {failure=_} ->
            aux(m,rest))
      | [] -> {failure={Error="MongoReplicaSet.connect: No connecting seeds"}}
    match aux(m, m.seeds) with
    | {success=m} ->
       rec aux2(m, hosts) =
         (match hosts with
          | [host|rest] ->
            (match MongoDriver.connect(m, host.f1, host.f2) with
             | {success=m} ->
                (match isMasterOpaLL(m) with
                 | {success=ism} ->
                    if ism.ismaster && (Bson.Register.default("...",ism.setName) == m.name)
                    then
                      do m.reconnect.set({some=connect})
                      {success=m}
                    else
                      (match ism.primary with
                       | {present=primary} ->
                          primary_host = mongo_host_of_string(primary)
                          (match List.extract_p((host -> host == primary_host),rest) with
                           | ({some=p},rest) ->
                              do if m.log then ML.info("MongoReplicaSet.connect","jump to primary",void)
                              aux2(m,[p|rest])
                           | ({none},rest) -> aux2(m,rest))
                       | {absent} -> aux2(m,rest))
                 | {failure=_} -> aux2(m,rest))
             | {failure=_} -> aux2(m,rest))
          | [] -> {failure={Error="MongoReplicaSet.connect: No master hosts"}})
       aux2(m, m.hosts.get())
    | {~failure} -> {~failure}

}}

// End of file replset.opa
