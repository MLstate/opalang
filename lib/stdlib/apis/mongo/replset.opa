/*
    Copyright © 2011 MLstate

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
    syncingTo : Bson.register(string);
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
  init(name:string, bufsize:int, pool_max:int, allow_slaveok:bool, log:bool,
       auth:Mongo.auths, seeds:list(Mongo.mongo_host)): Mongo.db =
    m = MongoDriver.init(bufsize, pool_max, allow_slaveok, true, log, auth)
    {m with ~seeds; ~name}

  /**
   * Initialize a [Mongo.db] connection using a single seed.
   **/
  init_single(name:string, bufsize:int, pool_max:int, allow_slaveok:bool, log:bool,
              auth:Mongo.auths, seed:Mongo.mongo_host): Mongo.db =
    init(name,bufsize,pool_max,allow_slaveok,log,auth,[seed])

  /**
   * Generate a [Mongo.mongo_host] value from a string: "host:port".
   * If the port is missing it will be set to [Mongo.default_port].
   **/
  mongo_host_of_string(s:string): Mongo.mongo_host =
    match String.explode(":",s) with
    | [host|[port|[]]] -> (host,Int.of_string(port))
    | _ -> (s,MongoDriver.default_port)

  @private adminCommandLL(m:Mongo.db, cmd:string): Mongo.result = MongoCommands.simple_int_command_ll(m,"admin",cmd,1)
  @private isMasterLL(m:Mongo.db): Mongo.result = adminCommandLL(m,"ismaster")

  /**
   * Try to get the list of hosts from a given list of seeds by connecting
   * in turn to each seed until we find a live one.
   **/
  check_seed(m:Mongo.db): (Mongo.db,list(Mongo.mongo_host)) =
    match isMasterLL(m) with
    | {success=doc} ->
       (match Bson.find(doc,"hosts") with
        | {some=hosts_doc} ->
           (match (Bson.doc2opa(hosts_doc):option({hosts:list(string)})) with
            | {some={~hosts}} ->
               hosts = (List.filter(((_,p) -> p != 0),List.map(mongo_host_of_string,hosts)))
               do if m.log then ML.info("MongoReplicaSet.check_seed","hosts={hosts}",void)
               (m,hosts)
            | {none} -> (m,[]))
        | {none} -> (m,[]))
    | {failure=_} -> (m,[])

  hostname = System.gethostname()
  is_localhost(h:string) : bool = match h with | "localhost" -> true | "127.0.0.1" -> true | _ -> h == hostname
  same_host(h1,h2) : bool = (is_localhost(h1) && is_localhost(h2)) || System.gethostbyname(h1) == System.gethostbyname(h2)
  same_mongo_host((h1,p1):Mongo.mongo_host)((h2,p2):Mongo.mongo_host) = p1 == p2 && same_host(h1,h2)

  mrg(l1:list(Mongo.mongo_host), l2:list(Mongo.mongo_host)) : list(Mongo.mongo_host) =
    rec aux(l1:list(Mongo.mongo_host), l2:list(Mongo.mongo_host)) =
      match (l2:list(Mongo.mongo_host)) with
      | [] -> (l1:list(Mongo.mongo_host))
      | [h|t] ->
        match List.index_p(same_mongo_host(h), l1) with
        | {some=_} -> (aux(l1, t):list(Mongo.mongo_host))
        | {none} -> (aux([h|l1], t):list(Mongo.mongo_host))
        end
    aux(l1, l2)

  @private do_authenticate(slaveok:bool, m:Mongo.db): outcome((bool,Mongo.db),Mongo.failure) =
    match m.auth with
    | [] -> {success=(slaveok,m)}
    | _ ->
       match MongoDriver.do_authenticate_ll({success=m}) with
       | {success=m} -> {success=(slaveok,m)}
       | {~failure} -> {~failure}
       end
    end


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
  connect(m:Mongo.db): outcome((bool,Mongo.db),Mongo.failure) =
    //m = {m with log=true}
    do if m.log then ML.debug("MongoReplicaSet.connect","depth={m.depth} allowslaveok={m.allow_slaveok}",void)
    do if m.seeds == [] then ML.fatal("MongoReplicaSet.connect","Tried to connect with no seeds",-1) else void
    rec aux(m, seeds) =
      match seeds with
      | [seed|rest] ->
        (match MongoDriver.connect(m, seed.f1, seed.f2) with
         | {success=m} ->
            (m,hosts) = check_seed(m)
            if hosts == []
            then aux(m,rest)
            else {success=({m with seeds=mrg(hosts,seeds)},hosts)}
         | {failure=_} ->
            aux(m,rest))
      | [] -> {failure={Error="MongoReplicaSet.connect: No connecting seeds"}}
    match aux(m, m.seeds) with
    | {success=(m,hosts)} ->
       do if m.log then ML.info("MongoReplicaSet.connect","got hosts {hosts}",void)
       rec aux2(m, hosts) =
         (match hosts with
          | [host|rest] ->
            (match MongoDriver.connect(m, host.f1, host.f2) with
             | {success=m} ->
                do if m.log then ML.info("MongoReplicaSet.connect","connected to host {host}\nm={m}",void)
                (match isMasterLL(m) with
                 | {success=doc} ->
                    do if m.log then ML.info("MongoReplicaSet.connect","isMasterLL: doc={doc}",void)
                    (match (Bson.find_bool(doc,"ismaster"),Bson.find_string(doc,"setName")) with
                     | ({some=ismaster},setName) ->
                        do if m.log then ML.info("MongoReplicaSet.connect","ismaster={ismaster} setName={setName}",void)
                        if ismaster && (Option.default("...",setName) == m.name)
                        then
                          do_authenticate(false,m)
                        else
                          (match Bson.find_string(doc,"primary") with
                           | {some=primary} ->
                              primary_host = mongo_host_of_string(primary)
                              (match List.extract_p((host -> host == primary_host),rest) with
                               | ({some=p},rest) ->
                                  do if m.log then ML.info("MongoReplicaSet.connect","jump to primary",void)
                                  aux2(m,[p|rest])
                               | ({none},rest) -> aux2(m,rest))
                           | {none} ->
                              do if m.log then ML.info("MongoReplicaSet.connect","no primary",void)
                              if m.allow_slaveok
                              then
                                do if m.log then ML.info("MongoReplicaSet.connect","using secondary",void)
                                do_authenticate(true,m)
                              else aux2(m,rest)
                          )
                     | _ -> aux2(m,rest))
                 | {failure=_} -> aux2(m,rest))
             | {failure=_} -> aux2(m,rest))
          | [] -> {failure={Error="MongoReplicaSet.connect: No master hosts"}})
       aux2(m, hosts)
    | {~failure} -> {~failure}

}}

// End of file replset.opa
