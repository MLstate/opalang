
package stdlib.database.mongo

/* Minimal implementation required for db.opa */

type NodeMongo.server = external
type NodeMongo.db = external
type NodeMongo.collection = external

NodeMongo = {{

  @private server = (%% BslMongo.NodeMongo.server %%: string, int, bool, int -> NodeMongo.server)

  @private replset = (%% BslMongo.NodeMongo.replset %%: list(NodeMongo.server) -> NodeMongo.server)

  @private db = (%% BslMongo.NodeMongo.db %%: NodeMongo.server, string, bool -> NodeMongo.db)

  @private open = (%% BslMongo.NodeMongo.open %%: NodeMongo.db, continuation((string, NodeMongo.db)) -> void)

  @private close = (%% BslMongo.NodeMongo.close %%: NodeMongo.db, continuation((string, NodeMongo.db)) -> void)

  @private collection = (%% BslMongo.NodeMongo.collection %%:
                         NodeMongo.db, string, continuation((string, NodeMongo.collection)) -> void)

  @private query = (%% BslMongo.NodeMongo.query %%:
                    NodeMongo.collection, int, int, Bson.document, option(Bson.document),
                    continuation((string,Mongo.reply)) -> void)

  @private count = (%% BslMongo.NodeMongo.count %%: Mongo.reply, continuation(int) -> void)

  @private nextObject = (%% BslMongo.NodeMongo.nextObject %%: Mongo.reply, continuation((string, Bson.document)) -> void)

  @private insert = (%% BslMongo.NodeMongo.insert %%:
                     NodeMongo.collection, list(Bson.document), bool, bool, continuation((string,int)) -> void)

  @private update = (%% BslMongo.NodeMongo.update %%:
                     NodeMongo.collection, Bson.document, Bson.document, bool, bool, bool, continuation((string,int)) -> void)

  @private remove = (%% BslMongo.NodeMongo.remove %%:
                     NodeMongo.collection, Bson.document, bool, continuation((string,int)) -> void)

  @private createIndex = (%% BslMongo.NodeMongo.createIndex %%:
                          NodeMongo.collection, Bson.document, bool, bool, bool, bool, bool,
                          continuation((string,string)) -> void)

  MongoCommon = {{

    // Warning: this does a db access
    reply_numberReturned(reply): int = @callcc(cont -> count(reply, cont))

    // Warning: this only ever returns the next document, NOT the nth document in the reply
    reply_document(_, reply): option(Bson.document) = 
      match @callcc(cont -> nextObject(reply, cont)) with
      | ("", doc) -> {some:doc}
      | (err, _) -> {none}

  }}

  MongoDriver = {{

    // Note that we don't open any connection here, we just instantiate a Server object
    // Actually opening the connection is done by the namespace checks on each command
    open(_bufsize:int, pool_max:int, reconnectable:bool, allow_slaveok:bool, addr:string, port:int, log:bool)
       : outcome(Mongo.db,Mongo.failure) =
      do if log then ML.info("MongoDriver.open","{addr}:{port}",void)
      { server=server(addr, port, reconnectable, pool_max);
        db=none; collection=none; allow_slaveok=allow_slaveok; ~log;
        // These will never be accessed
        conn=none; reconncell=@unsafe_cast(0); pool=@unsafe_cast(0); ~pool_max; bufsize=0; name=""; seeds=[];
        reconnect_wait=0; max_attempts=0; comms_timeout=0; reconnectable:false; depth:0; max_depth:0;
      }

    close(db:Mongo.db): outcome(Mongo.db,Mongo.failure) =
      match db.db with
      | {some=(_,odb)} ->
        (match @callcc(k -> close(odb, k)) with
         | "" -> {failure={Error=err}}
         | err -> {success={db with db={some:("",odb)}}})
      | {none} -> {success=db}

    reopen(db:Mongo.db, dbname:string): outcome(Mongo.db,Mongo.failure) =
      match close(db) with
      | {success=db} ->
        ndb = db(db.server, dbname, db.allow_slaveok)
        (match @callcc(cont -> open(ndb, cont)) with
         | ("", ndb) -> {success={db with db={some=(dbname, ndb)}}}
         | (err, _) -> {failure={Error=err}})
      | {~failure} -> {~failure}

    check_dbname(db:Mongo.db, dbname:string): outcome(Mongo.db,Mongo.failure) =
      match db.db with
      | {some=(dbn, _)} -> if dbn == dbname then db else reopen(db, dbname)
      | {none} -> reopen(db, dbname)

    redocollection(db:Mongo.db, collection:string): outcome(Mongo.db,Mongo.failure) =
      odb = match db.db with | {some=(_,odb)} -> odb | {none} -> @fail // db must be open
      match @callcc(cont -> collection(db.server, dbname, cont)} with
      | ("", coll) -> {success={db with collection={some=(collection, coll)}}}
      | (err, _) -> {failure={Error=err}}

    check_collection(db:Mongo.db, collection:string): outcome(Mongo.db,Mongo.failure) =
      match db.collection with
      | {some:(cname, coll)} -> if cname == collection then {success=db} else redocollection(db, collection)
      | {none} -> redocollection(db, collection)

    check_namespace(db:Mongo.db, dbname:string, collection:string): outcome(Mongo.db,Mongo.failure) =
      match check_dbname(db, dbname) with
      | {success=db} -> check_collection(db, collection)
      | {~failure} -> {~failure}

    check_ns(m:Mongo.db, ns:string): outcome(Mongo.db,Mongo.failure) =
      match String.explode(".",ns) with
      | [] -> {failure:{Error:"bad namespace \"{ns}\""}}
      | [dbname|coll] -> check_namespace(db, dbname, String.concat(".",coll))

    // TODO: db.opa uses the unsafe routines so we should log errors internally here

    // Note: we don't actually get access to the raw reply with the node.js driver
    // Here, the reply is actually the cursor
    query(m:Mongo.db, flags:int, ns:string, numberToSkip:int, numberToReturn:int,
          query:Bson.document, returnFieldSelector_opt:option(Bson.document)): option(Mongo.reply) =
      match check_ns(m, ns) with
      | {success:db} ->
        (match db.collection with
         | {some:(_,coll)} ->
           (match @callcc(cont -> query(coll, numberToSkip, numberToReturn, query, returnFieldSelector_opt, cont)) with
            | ("", cursor) -> {some:cursor}
            | (err, _) -> {none})
         | {none} -> {none})
      | {~failure} -> {none}

    insert_batch(m:Mongo.db, flags:int, ns:string, documents:list(Bson.document)): bool =
      match check_ns(m, ns) with
      | {success:db} ->
        (match db.collection with
         | {some:(_,coll)} ->
           keepGoing = Bitwise.land(flag,MongoCommon.ContinueOnErrorBit) != 0
           (match @callcc(cont -> insert(coll, documents, keepGoing, false/*safe*/, cont)) with
            | ("", _result) -> true // numberInserted???
            | (err, _) -> false)
         | {none} -> false)
      | {~failure} -> false

    update(m:Mongo.db, flags:int, ns:string, selector:Bson.document, update:Bson.document): bool =
      match check_ns(m, ns) with
      | {success:db} ->
        (match db.collection with
         | {some:(_,coll)} ->
           upsert = Bitwise.land(flag,MongoCommon.UpsertBit) != 0
           multi = Bitwise.land(flag,MongoCommon.MultiUpdateBit) != 0
           (match @callcc(cont -> update(coll, selector, update, upsert, multi, false/*safe*/, cont)) with
            | ("", _numberUpdated) -> true
            | (err, _) -> false)
         | {none} -> false)
      | {~failure} -> false

    delete(m:Mongo.db, flags:int, ns:string, selector:Bson.document): bool =
      match check_ns(m, ns) with
      | {success:db} ->
        (match db.collection with
         | {some:(_,coll)} ->
           (match @callcc(cont -> remove(coll, selector, false/*safe*/, cont)) with
            | ("", _numberRemoved) -> true
            | (err, _) -> false)
         | {none} -> false)
      | {~failure} -> false

    create_index(m:Mongo.db, ns:string, key:Bson.document, options:int): bool =
      match check_ns(m, ns) with
      | {success:db} ->
        (match db.collection with
         | {some:(_,coll)} ->
           unique = Bitwise.land(options,MongoCommon.UniqueBit) != 0
           sparse = Bitwise.land(options,MongoCommon.SparseBit) != 0
           background = Bitwise.land(options,MongoCommon.BackgroundBit) != 0
           dropDups = Bitwise.land(options,MongoCommon.DropDupsBit) != 0
           (match @callcc(cont -> createIndex(coll, key, unique, sparse, background, dropDups, false/*safe*/, cont)) with
            | ("", _indexName) -> true
            | (err, _) -> false)
         | {none} -> false)
      | {~failure} -> false

  }}

  MongoReplicaSet = {{

    init(name:string, bufsize:int, pool_max:int, allow_slaveok:bool, log:bool, seeds:list(Mongo.mongo_host)): Mongo.db =
      do if log then ML.info("MongoReplicaSet.init","seeds={seeds}",void)
      servers = List.map((((host, port)) -> server(host, port, true, pool_max)), seeds)
      replset = replset(servers)
      { server=replset; db=none; collection=none; allow_slaveok=allow_slaveok; ~log;
        conn=none; reconncell=@unsafe_cast(0); pool=@unsafe_cast(0); ~pool_max; bufsize=0; name=""; seeds=[];
        reconnect_wait=0; max_attempts=0; comms_timeout=0; reconnectable:false; depth:0; max_depth:0;
      }

    connect(m:Mongo.db): outcome((bool,Mongo.db),Mongo.failure) =
      // TODO: find out how node.js knows it's connected to a secondary, if not do an isMaster call
      {success:(false,m)}

  }}

}}

