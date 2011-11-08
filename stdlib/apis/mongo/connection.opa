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
 * Note that you have to be careful with concurrency, here.  This mechanism is not intended
 * to block access to shared resources.
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
type Mongo.mongodb = {
  mongo: Mongo.db;
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

MongoConnection = {{

  @private ML = MongoLog

  @private
  open_(dbo:outcome(Mongo.db,Mongo.failure)): outcome(Mongo.mongodb,Mongo.failure) =
    match dbo with
    | {success=mongo} ->
       (match mongo.primary.get() with
        | {some=(addr,port)} ->
           db = {~mongo; bufsize=mongo.bufsize; ~addr; ~port; link_count=Mutable.make(1);
                 keyname="key"; valname="value"; idxname="index";
                 dbname="db"; collection="collection";
                 fields={none}; orderby={none}; limit=0; skip=0;
                 insert_flags=0; update_flags=0; delete_flags=0; query_flags=0;
                }
           do System.at_exit( ->
                               if db.link_count.get() > 0
                               then
                                 do ML.info("MongoConnection.open","closing mongo (exit) {db.link_count.get()}",void)
                                 _ = MongoDriver.close(db.mongo) 
                                 void
                               else void)
           {success=db}
        | {none} -> {failure={Error="MongoConnection.open: no primary"}})
    | {~failure} -> {~failure}

  open(bufsize:int, addr:string, port:int): outcome(Mongo.mongodb,Mongo.failure) =
    open_(MongoDriver.open(bufsize,addr,port,false))

  repl(name:string, bufsize:int, seeds:list(Mongo.mongo_host)): outcome(Mongo.mongodb,Mongo.failure) =
    open_(MongoReplicaSet.connect(MongoReplicaSet.init(name,bufsize,false,seeds)))

  clone(db:Mongo.mongodb): Mongo.mongodb =
    do db.link_count.set(db.link_count.get()+1)
    db

  namespace(db:Mongo.mongodb, dbname:string, collection:string): Mongo.mongodb =
    do db.link_count.set(db.link_count.get()+1)
    { db with ~dbname; ~collection }

  log(db:Mongo.mongodb, log:bool): Mongo.mongodb =
    { db with mongo={ db.mongo with ~log } }

  close(db:Mongo.mongodb): void =
    lc = db.link_count.get()
    if lc > 0
      then
        do db.link_count.set(lc-1)
        if lc <= 1
        then
          do ML.info("MongoConnection.close","closing mongo (close) {db.link_count.get()}",void)
          _ = MongoDriver.close(db.mongo)
          void
        else void
      else void

  getLastError(db:Mongo.mongodb): Mongo.result = MongoCommands.getLastError(db.mongo, db.dbname)

  err(db:Mongo.mongodb, n:string): void =
    err = MongoCommands.getLastError(db.mongo, db.dbname)
    if MongoDriver.isError(err) then println("Error({n})={MongoDriver.string_of_result(err)}")

  skip(db:Mongo.mongodb, skip:int): Mongo.mongodb = { db with ~skip }
  limit(db:Mongo.mongodb, limit:int): Mongo.mongodb = { db with ~limit }
  fields(db:Mongo.mongodb, fields:option(Bson.document)): Mongo.mongodb = { db with ~fields }
  orderby(db:Mongo.mongodb, orderby:option(Bson.document)): Mongo.mongodb = { db with ~orderby }

  continueOnError(db:Mongo.mongodb): Mongo.mongodb =
    { db with insert_flags=Bitwise.lor(db.insert_flags,MongoDriver.ContinueOnErrorBit) }
  upsert(db:Mongo.mongodb): Mongo.mongodb =
    { db with update_flags=Bitwise.lor(db.update_flags,MongoDriver.UpsertBit) }
  multiUpdate(db:Mongo.mongodb): Mongo.mongodb =
    { db with update_flags=Bitwise.lor(db.update_flags,MongoDriver.MultiUpdateBit) }
  singleRemove(db:Mongo.mongodb): Mongo.mongodb =
    { db with delete_flags=Bitwise.lor(db.delete_flags,MongoDriver.SingleRemoveBit) }
  tailableCursor(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoDriver.TailableCursorBit) }
  slaveOk(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoDriver.SlaveOkBit) }
  oplogReplay(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoDriver.OplogReplayBit) }
  noCursorTimeout(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoDriver.NoCursorTimeoutBit) }
  awaitData(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoDriver.AwaitDataBit) }
  exhaust(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoDriver.ExhaustBit) }
  partial(db:Mongo.mongodb): Mongo.mongodb =
    { db with query_flags=Bitwise.lor(db.query_flags,MongoDriver.PartialBit) }

}}

// End of file connection.opa
