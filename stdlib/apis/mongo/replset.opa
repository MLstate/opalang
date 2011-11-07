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
 * This is a binding for MongoDB for OPA, loosely based around the C drivers.
 *
 * Module [ReplSet] allows the management of replica sets.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

/* Major TODOs, there are minor ones elsewhere. */

type ReplSet.replSetGetStatus =
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

type ReplSet.member = {
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

type ReplSet.replSetInitiate =
{
  _id : string;
  members: list(ReplSet.member);
  settings: Bson.register({
    getLastErrorDefaults : Bson.register(Commands.getLastErrorOptions);
    getlasterrormodes : Bson.register(Bson.document); // relates to tags
  });
}

ReplSet = {{

  @private ML = MongoLog

  /**
   * Freeze in replica set (can't become primary for given number of seconds.
   * Note unfreeze with 0.
   **/
  replSetFreeze(m:Mongo.db, seconds:int): Mongo.result =
    Commands.simple_int_command(m, "admin", "replSetFreeze", seconds)

  /**
   * Step down from primary status.
   **/
  replSetStepDown(m:Mongo.db, seconds:int): Mongo.result =
    Commands.simple_int_command(m, "admin", "replSetStepDown", seconds)

  /**
   * Get replica get status.
   **/
  replSetGetStatus(m:Mongo.db): Mongo.result = Commands.simple_int_command(m, "admin", "replSetGetStatus", 1)
  replSetGetStatusOpa(m:Mongo.db): outcome(ReplSet.replSetGetStatus,Mongo.failure) = Commands.adminToOpa(m,"replSetGetStatus")

  /**
   * Initalise a replica set.
   * TODO: test
   **/
  simpleConfig(id:int, host:string): ReplSet.member =
    { _id=id; ~host;
      arbiterOnly={absent}; buildIndexes={absent}; hidden={absent}; priority={absent};
      tags={absent}; slaveDelay={absent}; votes={absent}
    }

  replSetInitiate(m:Mongo.db, id:string, members:list((int,string))): Mongo.result =
    config = Bson.opa2doc({ _id=id; members=List.map(((id,host) -> simpleConfig(id,host)),members); settings={absent} })
    Commands.run_command(m, "admin", [H.doc("replSetInitiate",config)])

  /**
   * This one will be tricky to implement, it closes the connection.
   * TODO: implement a command_with_no_reply_and_reconnect() function.
   **/
  //replSetReconfig(m:Mongo.db, id:string, members:list((int,string))): Mongo.result =
  //  config = Bson.opa2doc({ _id=id; members=List.map(((id,host) -> simpleConfig(id,host)),members); settings={absent} })
  //  Commands.run_command(m, "admin", [H.doc("replSetReconfig",config)])

  add_seed(mdb:Mongo.db, host:string, port:int): Mongo.db = {mdb with seeds=[(host,port)|mdb.seeds]}
  remove_seed(mdb:Mongo.db, host:string, port:int): Mongo.db = {mdb with seeds=List.filter((s -> s != (host,port)),mdb.seeds)}

  init(name:string, bufsize:int, log:bool, seeds:list(mongo_host)): Mongo.db =
    mdb = Mongo.init(bufsize, log)
    {mdb with ~seeds; hosts=Mutable.make([]); ~name}

  init_single(name:string, bufsize:int, log:bool, seed:mongo_host): Mongo.db =
    init(name,bufsize,log,[seed])

  mongo_host_of_string(s:string): mongo_host =
    match String.explode(":",s) with
    | [host|[port|[]]] -> (host,Int.of_string(port))
    | _ -> (s,27017)

  check_seed(mdb:Mongo.db): Mongo.db =
    match Commands.isMasterOpa(mdb) with
    | {success=ism} ->
       (match ism.hosts with
        | {present=hosts} ->
           hosts = (List.filter(((_,p) -> p != 0),List.map(mongo_host_of_string,hosts)))
           do if mdb.log then ML.info("ReplSet.check_seed","hosts={hosts}",void)
           do mdb.hosts.set(hosts)
           mdb
        | {absent} -> mdb)
    | {failure=_} -> mdb

  connect(mdb:Mongo.db): outcome(Mongo.db,Mongo.failure) =
    mdb = {mdb with log=false}
    //do println("ReplSet.connect: seeds={mdb.seeds}")
    do if mdb.seeds == [] then ML.fatal("ReplSet.connect","Tried to connect with no seeds",-1) else void
    rec aux(mdb, seeds) =
      match seeds with
      | [seed|rest] ->
        (match Mongo.connect(mdb, seed.f1, seed.f2) with
         | {success=mdb} ->
            mdb = check_seed(mdb)
            if mdb.hosts.get() == []
            then aux(Mongo.close(mdb),rest)
            else {success=Mongo.close(mdb)}
         | {failure=_} ->
            aux(mdb,rest))
      | [] -> {failure={Error="ReplSet.connect: No connecting seeds"}}
    match aux(mdb, mdb.seeds) with
    | {success=mdb} ->
       //do println("ReplSet.connect: hosts={mdb.hosts.get()}")
       rec aux2(mdb, hosts) =
         (match hosts with
          | [host|rest] ->
            (match Mongo.connect(mdb, host.f1, host.f2) with
             | {success=mdb} ->
                (match Commands.isMasterOpa(mdb) with
                 | {success=ism} ->
                    //do println("ReplSet.connect: ism={ism}")
                    if ism.ismaster && (Bson.Register.default("...",ism.setName) == mdb.name)
                    then
                      do mdb.reconnect.set({some=connect})
                      {success=mdb}
                    else
                      (match ism.primary with
                       | {present=primary} ->
                          primary_host = mongo_host_of_string(primary)
                          (match List.extract_p((host -> host == primary_host),rest) with
                           | ({some=p},rest) ->
                              do if mdb.log then ML.info("ReplSet.connect","jump to primary",void)
                              aux2(Mongo.close(mdb),[p|rest])
                           | ({none},rest) -> aux2(Mongo.close(mdb),rest))
                       | {absent} -> aux2(Mongo.close(mdb),rest))
                 | {failure=_} -> aux2(Mongo.close(mdb),rest))
             | {failure=_} -> aux2(mdb,rest))
          | [] -> {failure={Error="ReplSet.connect: No master hosts"}})
       aux2(mdb, mdb.hosts.get())
    | {~failure} -> {~failure}

}}

