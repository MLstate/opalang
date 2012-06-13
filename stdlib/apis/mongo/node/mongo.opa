import-plugin mongo
import stdlib.apis.mongo.common
/* Minimal implementation required for db.opa */

type NodeMongo.server = external
type NodeMongo.db = external
type NodeMongo.collection = external
type Mongo.reply = external

type NodeBson.document = external
type NodeBson.value = external

/**
 * Main connection type.
 * Stores the socket connection plus other parameters such as
 * the seeds, timing parameters for reconnection and a limiter for recursion depth.
 **/
@abstract
type Mongo.db = {
  log : bool;
  name : string;
  allow_slaveok : bool;

  // For node.js
  server : NodeMongo.server;
  db : option((string, NodeMongo.db));
  collection : option((string, NodeMongo.collection));
}

@private
NodeMongo = {{

  server = (%% BslMongo.NodeMongo.server %%: string, int, bool, int -> NodeMongo.server)

  replset = (%% BslMongo.NodeMongo.replset %%: list(NodeMongo.server) -> NodeMongo.server)

  db = (%% BslMongo.NodeMongo.db %%: NodeMongo.server, string, bool -> NodeMongo.db)

  open = (%% BslMongo.NodeMongo.open %%: NodeMongo.db, continuation((string, NodeMongo.db)) -> void)

  get_database = %%BslMongo.NodeMongo.get_database%% : NodeMongo.server, string, bool -> (string, NodeMongo.db)

  close = (%% BslMongo.NodeMongo.close %%: NodeMongo.db, continuation(string) -> void)

  collection = (%% BslMongo.NodeMongo.collection %%:
                         NodeMongo.db, string, continuation((string, NodeMongo.collection)) -> void)

  query = (%% BslMongo.NodeMongo.query %%:
                    NodeMongo.collection, int, int, NodeBson.document, option(NodeBson.document),
                    continuation((string,Mongo.reply)) -> void)

  count = (%% BslMongo.NodeMongo.count %%: Mongo.reply, continuation(int) -> void)

  nextObject = (%% BslMongo.NodeMongo.nextObject %%: Mongo.reply, continuation((string, NodeBson.document)) -> void)

  insert = (%% BslMongo.NodeMongo.insert %%:
                     NodeMongo.collection, list(NodeBson.document), bool, bool, continuation((string,list(NodeBson.document))) -> void)

  update = (%% BslMongo.NodeMongo.update %%:
                     NodeMongo.collection, NodeBson.document, NodeBson.document, bool, bool, bool, continuation((string,int)) -> void)

  remove = (%% BslMongo.NodeMongo.remove %%:
                     NodeMongo.collection, NodeBson.document, bool, continuation((string,int)) -> void)

  createIndex = (%% BslMongo.NodeMongo.createIndex %%:
                          NodeMongo.collection, NodeBson.document, bool, bool, bool, bool, bool,
                          continuation((string,string)) -> void)

}}

@private
NodeBson = {{
  @private
  Document = {{
    empty = %%BslMongo.NodeBson.empty_document%%
    add   = %%BslMongo.NodeBson.add_element%%

    array = %%BslMongo.NodeBson.empty_array%%
  }}

  @private
  Value = {{
    float(f)         = %%BslMongo.NodeBson.float_value%%(f)
    string(s)        = %%BslMongo.NodeBson.string_value%%(s)
    of_bool(b)       = %%BslMongo.NodeBson.bool_value%%(b)
    of_int32(i)      = %%BslMongo.NodeBson.int32_value%%(i)
    of_timestamp(t)  = %%BslMongo.NodeBson.timestamp_value%%(t.f1, t.f2)
    of_int(i)        = %%BslMongo.NodeBson.int_value%%(i)
    of_int64(i)      = %%BslMongo.NodeBson.int64_value%%(i)

    of_doc(d)        = %%BslMongo.NodeBson.document_value%%(d)
    of_array(a)      = %%BslMongo.NodeBson.array_value%%(a)
    of_binary(b)     = %%BslMongo.NodeBson.binary_value%%(b)
    of_object_id(o)  = %%BslMongo.NodeBson.object_id_value%%(o)
    of_date(d)       = %%BslMongo.NodeBson.date_value%%(Date.in_milliseconds(d))
    null()           = %%BslMongo.NodeBson.null_value%%()
    regexp(r)        = %%BslMongo.NodeBson.regexp_value%%(r.f1, r.f2)
    of_code(c)       = %%BslMongo.NodeBson.code_value%%(c)
    of_symbol(s)     = %%BslMongo.NodeBson.symbol_value%%(s)
    of_code_scope(s) = %%BslMongo.NodeBson.code_scope_value%%(s.f1, of_document(s.f2))
    min_key()        = %%BslMongo.NodeBson.min_value%%()
    max_key()        = %%BslMongo.NodeBson.max_value%%()
  }}

  @private
  fill_document(init:NodeBson.document, doc : Bson.document) =
    do List.iter(~{name value} -> Document.add(init, name, of_value(value)), doc)
    init

  of_document(doc : Bson.document):NodeBson.document =
    fill_document(Document.empty(), doc)

  of_array(doc : Bson.document):NodeBson.document =
    fill_document(Document.array(), doc)

  of_value(value:Bson.value):NodeBson.value =
    match value with
    | { Double    = d } -> Value.float(d)
    | { String    = s } -> Value.string(s)
    | { Boolean   = b } -> Value.of_bool(b)
    | { RealInt32 = i } -> Value.of_int32(i:int32)
    | { Timestamp = t } -> Value.of_timestamp(t)
    | { Int32     = i }
    | { Int64     = i } -> Value.of_int(i)
    | { RealInt64 = i } -> Value.of_int64(i)
    | { Array     = a } -> Value.of_array(of_array(a))
    | { Document  = d } -> Value.of_doc(of_document(d))
    | { Binary    = b } -> Value.of_binary(b)
    | { ObjectID  = o } -> Value.of_object_id(o)
    | { Date      = d } -> Value.of_date(d)
    | { Null }          -> Value.null()
    | { Regexp    = r } -> Value.regexp(r)
    | { Code      = c } -> Value.of_code(c)
    | { Symbol    = s } -> Value.of_symbol(s)
    | { CodeScope = c } -> Value.of_code_scope(c)
    | { Min }           -> Value.min_key()
    | { Max }           -> Value.max_key()

  to_document(doc : NodeBson.document) : Bson.document =
    @fail
}}

MongoCommon = {{


  /* OP_INSERT */
  ContinueOnErrorBit  = 0x00000001

  /* OP_UPDATE */
  UpsertBit           = 0x00000001
  MultiUpdateBit      = 0x00000002

  /* OP_QUERY */
  TailableCursorBit   = 0x00000002
  SlaveOkBit          = 0x00000004
  OplogReplayBit      = 0x00000008
  NoCursorTimeoutBit  = 0x00000010
  AwaitDataBit        = 0x00000020
  ExhaustBit          = 0x00000040
  PartialBit          = 0x00000080

  /* OP_DELETE */
  SingleRemoveBit     = 0x00000001

  /* OP_REPLY */
  CursorNotFoundBit   = 0x00000001
  QueryFailureBit     = 0x00000002
  ShardConfigStaleBit = 0x00000004
  AwaitCapableBit     = 0x00000008

  /* Flags used by the index routines. */
  UniqueBit     = 0x00000001
  DropDupsBit   = 0x00000002
  BackgroundBit = 0x00000004
  SparseBit     = 0x00000008

  // Warning: this does a db access
  reply_numberReturned(reply): int = @callcc(cont -> NodeMongo.count(reply, cont))

  // Warning: this only ever returns the next document, NOT the nth document in the reply
  reply_document(reply, _): option(Bson.document) =
    match @callcc(cont -> NodeMongo.nextObject(reply, cont)) with
    | ("", doc) -> {some=NodeBson.to_document(doc)}
    | (err, _) -> {none}

}}

MongoDriver = {{

  // Note that we don't open any connection here, we just instantiate a Server object
  // Actually opening the connection is done by the namespace checks on each command
  open(_bufsize:int, pool_max:int, reconnectable:bool, allow_slaveok:bool, addr:string, port:int, log:bool)
     : outcome(Mongo.db,Mongo.failure) =
    do if log then MongoLog.info("MongoDriver.open","{addr}:{port}",void)
    {success =
      { server=NodeMongo.server(addr, port, reconnectable, pool_max);
        db=none;
        collection=none;
        ~log;
        ~allow_slaveok
        name="";
      }
    }

  close(db:Mongo.db): outcome(Mongo.db,Mongo.failure) =
    match db.db with
    | {some=(_,odb)} ->
      match @callcc(k -> NodeMongo.close(odb, k)) with
      | "" -> {success={db with db={some=("",odb)}}}
      | err -> {failure={Error=err}}
      end
    | {none} -> {success=db}

  reopen(db:Mongo.db, dbname:string): outcome(Mongo.db,Mongo.failure) =
    match close(db) with
    | {success=db} ->
      match NodeMongo.get_database(db.server, dbname, db.allow_slaveok) with
      | ("", ndb) -> {success={db with db={some=(dbname, ndb)}}}
      | (err, _) -> {failure={Error=err}}
      end
    | {~failure} -> {~failure}

  check_dbname(db:Mongo.db, dbname:string): outcome(Mongo.db,Mongo.failure) =
    match db.db with
    | {some=(dbn, _)} ->
      if dbn == dbname then
        {success = db}
      else
        do Log.notice("Reopen", "{dbn} vs {dbname}")
        reopen(db, dbname)
    | {none} -> reopen(db, dbname)

  redocollection(db:Mongo.db, collection:string): outcome(Mongo.db,Mongo.failure) =
    odb = match db.db with | {some=(_,odb)} -> odb | {none} -> @fail // db must be open
    match @callcc(cont -> NodeMongo.collection(odb, collection, cont)) with
    | ("", coll) -> {success={db with collection={some=(collection, coll)}}}
    | (err, _) -> {failure={Error=err}}

  check_collection(db:Mongo.db, collection:string): outcome(Mongo.db,Mongo.failure) =
    match db.collection with
    | {some=(cname, coll)} -> if cname == collection then {success=db} else redocollection(db, collection)
    | {none} -> redocollection(db, collection)

  check_namespace(db:Mongo.db, dbname:string, collection:string): outcome(Mongo.db,Mongo.failure) =
    match check_dbname(db, dbname) with
    | {success=db} -> check_collection(db, collection)
    | {~failure} -> {~failure}

  check_ns(db:Mongo.db, ns:string): outcome(Mongo.db,Mongo.failure) =
    match String.explode(".",ns) with
    | [] -> {failure={Error="bad namespace \"{ns}\""}}
    | [dbname|coll] -> check_namespace(db, dbname, String.concat(".",coll))

  // TODO: db.opa uses the unsafe routines so we should log errors internally here

  // Note: we don't actually get access to the raw reply with the node.js driver
  // Here, the reply is actually the cursor
  query(m:Mongo.db, flags:int, ns:string, numberToSkip:int, numberToReturn:int,
        query:Bson.document, returnFieldSelector_opt:option(Bson.document)): option(Mongo.reply) =
    query = NodeBson.of_document(query)
    returnFieldSelector_opt = Option.map(NodeBson.of_document, returnFieldSelector_opt)
    match check_ns(m, ns) with
    | {success=db} ->
      (match db.collection with
       | {some=(_,coll)} ->
         (match @callcc(cont -> NodeMongo.query(coll, numberToSkip, numberToReturn, query, returnFieldSelector_opt, cont)) with
          | ("", cursor) -> {some=cursor}
          | (err, _) -> {none})
       | {none} -> {none})
    | {~failure} -> {none}

  insert_batch(m:Mongo.db, flags:int, ns:string, documents:list(Bson.document)): bool =
    documents = List.map(NodeBson.of_document, documents)
    match check_ns(m, ns) with
    | {success=db} ->
      (match db.collection with
       | {some=(_,coll)} ->
         keepGoing = Bitwise.land(flags,MongoCommon.ContinueOnErrorBit) != 0
         (match @callcc(cont -> NodeMongo.insert(coll, documents, keepGoing, false/*safe*/, cont)) with
          | ("", _result) -> true // numberInserted???
          | (err, _) -> false)
       | {none} -> false)
    | {~failure} -> false

  update(m:Mongo.db, flags:int, ns:string, selector:Bson.document, update:Bson.document): bool =
    selector = NodeBson.of_document(selector)
    update = NodeBson.of_document(update)
    match check_ns(m, ns) with
    | {success=db} ->
      (match db.collection with
       | {some=(_,coll)} ->
         upsert = Bitwise.land(flags,MongoCommon.UpsertBit) != 0
         multi = Bitwise.land(flags,MongoCommon.MultiUpdateBit) != 0
         (match @callcc(cont -> NodeMongo.update(coll, selector, update, upsert, multi, false/*safe*/, cont)) with
          | ("", _numberUpdated) -> true
          | (err, _) -> false)
       | {none} -> false)
    | {~failure} -> false

  delete(m:Mongo.db, flags:int, ns:string, selector:Bson.document): bool =
    selector = NodeBson.of_document(selector)
    match check_ns(m, ns) with
    | {success=db} ->
      (match db.collection with
       | {some=(_,coll)} ->
         (match @callcc(cont -> NodeMongo.remove(coll, selector, false/*safe*/, cont)) with
          | ("", _numberRemoved) -> true
          | (err, _) -> false)
       | {none} -> false)
    | {~failure} -> false

  create_index(m:Mongo.db, ns:string, key:Bson.document, options:int): bool =
    key = NodeBson.of_document(key)
    match check_ns(m, ns) with
    | {success=db} ->
      (match db.collection with
       | {some=(_,coll)} ->
         unique = Bitwise.land(options,MongoCommon.UniqueBit) != 0
         sparse = Bitwise.land(options,MongoCommon.SparseBit) != 0
         background = Bitwise.land(options,MongoCommon.BackgroundBit) != 0
         dropDups = Bitwise.land(options,MongoCommon.DropDupsBit) != 0
         (match @callcc(cont -> NodeMongo.createIndex(coll, key, unique, sparse, background, dropDups, false/*safe*/, cont)) with
          | ("", _indexName) -> true
          | (err, _) -> false)
       | {none} -> false)
    | {~failure} -> false

}}

MongoReplicaSet = {{

  init(name:string, bufsize:int, pool_max:int, allow_slaveok:bool, log:bool, seeds:list(Mongo.mongo_host)): Mongo.db =
    do if log then MongoLog.info("MongoReplicaSet.init","seeds={seeds}",void)
    servers = List.map((((host, port)) -> NodeMongo.server(host, port, true, pool_max)), seeds)
    replset = NodeMongo.replset(servers)
    { server=replset;
      db=none;
      collection=none;
      ~log;
      name=name;
      ~allow_slaveok
    }

  connect(m:Mongo.db): outcome((bool,Mongo.db),Mongo.failure) =
    // TODO: find out how node.js knows it's connected to a secondary, if not do an isMaster call
    {success=(false,m)}

}}
