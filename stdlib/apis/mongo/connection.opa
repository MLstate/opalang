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
//@abstract
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

  /**
   * Open a connection to a single server.  No check for primary status is
   * carried out and no reconnection is attempted.
   *
   * Example: [open(bufsize, host, port)]
   **/
  open(bufsize:int, addr:string, port:int): outcome(Mongo.mongodb,Mongo.failure) =
    open_(MongoDriver.open(bufsize,addr,port,false))

  /**
   * Open a connection to a replica set starting from the given list of seeds.
   *
   * Example: [open(name, bufsize, seeds)]
   *
   * This routine causes a serach for the current host list among the seeds
   * and then searches for the primary among the hosts.  Rconnection logic
   * is enabled.
   **/
  repl(name:string, bufsize:int, seeds:list(Mongo.mongo_host)): outcome(Mongo.mongodb,Mongo.failure) =
    open_(MongoReplicaSet.connect(MongoReplicaSet.init(name,bufsize,false,seeds)))

  /**
   * Clone a connection.  We actually just bump the link count.  On close
   * the connection itself is only closed once the link count drops to zero.
   **/
  clone(db:Mongo.mongodb): Mongo.mongodb =
    do db.link_count.set(db.link_count.get()+1)
    db

  /**
   * Change the namespace built into the connection.  The defaults are:
   * db="db" and collection="collection".  Changing the namespace bumps
   * the link count.
   **/
  namespace(db:Mongo.mongodb, dbname:string, collection:string): Mongo.mongodb =
    do db.link_count.set(db.link_count.get()+1)
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
          do ML.info("MongoConnection.close","closing mongo (close) link_count={db.link_count.get()}",void)
          _ = MongoDriver.close(db.mongo)
          void
        else void
      else void

  /**
   * Return the last error on the given connection.
   **/
  getLastError(db:Mongo.mongodb): Mongo.result = MongoCommands.getLastError(db.mongo, db.dbname)

  /**
   * A simple error report.  Check the last error on the given database and log an error
   * if the reply is an actual error.
   **/
  err(db:Mongo.mongodb, msg:string): bool =
    err = MongoCommands.getLastError(db.mongo, db.dbname)
    status = MongoDriver.isError(err)
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

}}

// End of file connection.opa
