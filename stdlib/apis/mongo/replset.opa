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

ReplSet = {{

/*
  init(db:Mongo.db, name:string): outcome(Mongo.db,Mongo.failure) =
    if Option.is_some(db.replset)
    then {failure={Error="Attempt to re-initialise a replica set"}}
    else if Option.is_some(db.conn)
    then {failure={Error="Attempt to make a connected mongodb a replica set"}}
    else {success={db with replset={some={seeds=[]; hosts=[]; ~name; primary_connected=false;}}}}

  add_seed(db:Mongo.db, host:string, port:int): outcome(Mongo.db,Mongo.failure) =
    match db.replset with
    | {some=replset} -> {success={db with replset={some={replset with seeds=[(host,port)|replset.seeds]}}}}
    | {none} -> {failure={Error="Attempt to add seed ({host},{port}) to unintialised replica set"}}

  add_host(db:Mongo.db, host:string, port:int): outcome(Mongo.db,Mongo.failure) =
    match db.replset with
    | {some=replset} -> {success={db with replset={some={replset with hosts=[(host,port)|replset.hosts]}}}}
    | {none} -> {failure={Error="Attempt to add host ({host},{port}) to unintialised replica set"}}
*/

  init(name:string, bufsize:int, log:bool, seeds:list(mongo_host)): Mongo.db =
    db = Mongo.init(bufsize, log)
    {db with replset={~seeds; hosts=[]; ~name; primary_connected=false;}}

  init_single(name:string, bufsize:int, log:bool, seed:mongo_host): Mongo.db =
    init(name,bufsize,log,[seed])

  mongo_host_of_string(s:string): mongo_host =
    match String.explode(":",s) with
    | [host|[port|[]]] -> (host,Int.of_string(port))
    | _ -> (s,27017)

  check_seed(db:Mongo.db): Mongo.db =
    match (Mongo.result_to_opa(Commands.simple_int_command(db,"admin","ismaster",1)):option(Commands.isMaster)) with
    | {some=ism} ->
       hosts = (List.filter(((_,p) -> p != 0),List.map(mongo_host_of_string,ism.hosts)))
       do println("check_seed: hosts={hosts}")
       {db with replset={db.replset with ~hosts}}
    | {none} -> db

  check_host(db:Mongo.db): option(Commands.isMaster) =
    res =
    (Mongo.result_to_opa(Commands.simple_int_command(db,"admin","ismaster",1)):option(Commands.isMaster))
    do println("check_host: res={res}")
    res

  connect(db:Mongo.db): outcome(Mongo.db,Mongo.failure) =
    db = {db with log=false}
    do println("ReplSet.connect: seeds={db.replset.seeds}")
    do if db.replset.seeds == [] then ML.fatal("ReplSet.connect","Tried to connect with no seeds",-1) else void
    rec aux(db, seeds) =
      match seeds with
      | [seed|rest] ->
        (match Mongo.connect(db, seed.f1, seed.f2) with
         | {success=db} ->
            db = check_seed(db)
            if db.replset.hosts == []
            then aux(Mongo.close(db),rest)
            else {success=Mongo.close(db)}
         | {failure=_} ->
            aux(db,rest))
      | [] -> {failure={Error="ReplSet.connect: No connecting seeds"}}
    match aux(db, db.replset.seeds) with
    | {success=db} ->
       do println("ReplSet.connect: hosts={db.replset.hosts}")
       rec aux2(db, hosts) =
         (match hosts with
          | [host|rest] ->
            (match Mongo.connect(db, host.f1, host.f2) with
             | {success=db} ->
                (match check_host(db) with
                 | {some=ism} ->
                    do println("ReplSet.connect: ism={ism}")
                    if ism.ismaster && (ism.setName == db.replset.name)
                    then {success=db}
                    else aux2(Mongo.close(db),rest)
                 | {none} ->
                    aux2(Mongo.close(db),rest))
             | {failure=_} ->
                aux2(db,rest))
          | [] -> {failure={Error="ReplSet.connect: No master hosts"}})
       aux2(db, db.replset.hosts)
    | {~failure} -> {~failure}

}}

