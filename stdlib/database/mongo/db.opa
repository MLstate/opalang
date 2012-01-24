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
 *
 */

import stdlib.core.{date,map,parser}
import stdlib.apis.mongo
import stdlib.system

/**
 * {1 About this module}
 *
 * {1 Where do I start?}
 *
 * {1 What if I need more?}
 *
 */

/**
 * {1 Types defined in this module}
 */

@opacapi type badoplink_database = external
@opacapi type badoplink_transaction = external
type badoplink_revision = external
@opacapi type badoplink_db_path_key = external
@opacapi type badoplink_data_d = external
@opacapi type badoplink_db_partial_key = external

@opacapi type badoplink_node_config = external

@opacapi type badop_engine_database_options = external
@opacapi type badop_engine_t = external

@opacapi type path_t('kind, 'data) = {
  id : string;
  read : -> option('data);
  default : 'data;
  more : 'kind;
}
@opacapi type path_embedded_obj = external
@opacapi type path_embed_info = external

@abstract @opacapi type path_val_p = void
@abstract @opacapi type path_ref_p('data) = {
  write : 'data -> bool
}

@opacapi type opa_transaction_t('a) = external
@opacapi type dbgraph_diff = external


/**
 * Value paths
 *
 * Value paths are obtained with the syntax [!/path/to/data]. They are used by most
 * read operations present in this interface, and are bound to a point in time and to
 * the currently running transaction, if any -- this is by opposition to reference
 * paths.
 *
 * That means that binding [p = !/path] and later on [x = Db.read(p)] will give you
 * the value at the time of the binding, just like with [p = /path] and then [x = p].
 *
 */
@opacapi type val_path('a) = path_t(path_val_p, 'a)

/**
 * Reference paths
 *
 * A reference path is obtained with the syntax [@/path/to/data]. In itself, this syntax
 * just builds a pointer to the database without performing any database operations.
 *
 * Reference paths are not bound to any time or transaction, and reading them gives the
 * result at the time of the read: by opposition to value paths, [p = @/path] followed
 * later on by [x = Db.read(p)] will show any change on the value stored in the database
 * performed in the meantime, possibly concurrently by another user.
 *
 */
@opacapi type ref_path('a) = path_t(path_ref_p('a), 'a)

/**
 * {1 Interface}
 */

S = MongoSelectUpdate

Db = {{


   gen_read(id, query, uncap) =
     // TODO - ...
     value2opa(value:Bson.value):option('a) = Bson.doc2opa([{name="value"; ~value}])
     match query with
         | {none} ->
           do Log.error("DbGen/Mongo", "(failure) read to {id} doesn't returns anything")
           none
         | {some=reply} ->
           match MongoCommon.reply_document(reply, 0) with
           | {none} ->
             do Log.error("DbGen/Mongo", "(failure) read to {id} doesn't returns the document")
             none
           | {some=document} ->
             #<Ifstatic:DBGEN_DEBUG>
             do Log.notice("DbGen/Mongo", "(success) read bson document from mongo db returned")
             #<End>
             match uncap(document) with
             | {none} ->
               do Log.error("DbGen/Mongo", "(failure) uncap bson document")
               none
             | {some=val} ->
               #<Ifstatic:DBGEN_DEBUG>
               do Log.notice("DbGen.Mongo", "(success) uncap bson document")
               #<End>
               match value2opa(val) with
               | {none} ->
                 do Log.error("DbGen/Mongo", "(failure) unserialize bson value")
                 none
               | r ->
                 #<Ifstatic:DBGEN_DEBUG>
                 do Log.notice("DbGen.Mongo", "(success) unserialize bson value")
                 #<End>
                 r
             end
           end
         end

   build_vpath_raw2(db:Mongo.db, path:list(string), default:'data, selector:Bson.document)
     : val_path('data) =
     id = Uri.to_string(~{path fragment=none query=[] is_directory=false is_from_root=true})
     read() =
       uncap(doc) = match doc with
         | [{name = "data"; ~value}] -> some(value)
         | _ -> none
       query = MongoDriver.query(db, 0, "test.{id}", 0, 1, selector, none)
       gen_read(id, query, uncap)
     { ~id ~default ~read more = void }

//   build_rpath_raw2(db:Mongo.db, path:list(string), default:'data, selector:Bson.document,


   build_path_raw(_:Mongo.db, path:list(string), default:'data,
                   read:-> option('data), more:'any) : path_t(black, 'data) =
     id = Uri.to_string(~{path fragment=none query=[] is_directory=false is_from_root=true})
     {
       ~id ~default ~read ~more
     }

   build_vpath_sub(db:Mongo.db, path:list(string), default:'data, rpath:list(string), partial:list(string))
   : val_path('data) =
     rid = Uri.to_string(~{path=rpath fragment=none query=[] is_directory=false is_from_root=true})
     id = Uri.to_string(~{path fragment=none query=[] is_directory=false is_from_root=true})
     docpath = "data" +> partial
     selector = [{name = "_id"; value = {String = rid}}]
     filter = [
       {name = "_id"; value = {Boolean = false}},
       {name = List.to_string_using("", "", ".", docpath); value = {Boolean = true}}
     ]
     uncap(doc:Bson.document) =
       rec aux(path, value:Bson.value) =
         match path with
         | [] -> some(value)
         | [field|tail] ->
           match value with
           | {Document = [~{name value}]} ->
             do @assert(name == field)
             aux(tail, value)
           | _ ->
             do Log.error("DbGen/Mongo",
               "(failure) unserialize on {path}::{partial} failed @ {tail} got {value}")
             none
           end
         end
       match doc with
       | [{name = "data"; ~value}] -> aux(partial, value)
       | _ ->
         do Log.error("DbGen/Mongo", "Unexpected structure of document : {doc}")
         none
     {
       ~id ~default
       more = void
       read() =
         query = MongoDriver.query(db, 0, "test.default", 0, 1, selector, some(filter))
         gen_read(id, query, uncap)

     }

   build_vpath(db:Mongo.db, path:list(string), default:'data):val_path('data) =
     id = Uri.to_string(~{path fragment=none query=[] is_directory=false is_from_root=true})
     selector = [{name = "_id"; value = {String = id}}]
     filter = [{name = "_id"; value = {Boolean = false}}]
     read() =
       uncap(doc) = match doc with
         | [{name = "data"; ~value}] -> some(value)
         | _ -> none
       query = MongoDriver.query(db, 0, "test.default", 0, 1, selector, some(filter))
       gen_read(id, query, uncap)
     ~{id read default more=void}

   build_rpath(db, path:list(string), default:'data):ref_path('data) =
     vpath = build_vpath(db, path, default) : val_path('data)
     selector = [{name = "_id"; value = {String = vpath.id}}]
     tags = Bitwise.lor(0, MongoCommon.UpsertBit)
     write(data:'data) =
       update = Bson.opa2doc({_id = vpath.id; data=data})
       MongoDriver.update(db, tags, "test.default", selector, update)
     { vpath with more=~{write} }

  /**
   * Writes data at the given path in the database.
   * [do write(@/path,42)] is equivalent to [do /path <- 42]
   *
   * @example [write(@/path,42)] initializes or updates the data at path [/path]
   */
  write(path:ref_path('data), data:'data) =
    (path.more:path_ref_p('data)).write(data)

  `<-`(d,a) = write(d,a)

  /**
   * Reads the data currently held at a reference path.
   *
   * @example [read(@/path)] is equivalent to [/path]
   */
  read(path:path_t('any, 'data)):'data = path.read() ? path.default

  /**
   * Reads the data currently held at a reference path.
   *
   * @example [read(@/path)] is equivalent to [/path]
   */
  option(path:path_t('any, 'data)):option('data) = path.read()

  /**
   * Turns a reference-path into a value-path, in fact taking a snapshot.
   *
   * @example [get_val(@/path)] is equivalent to [!/path]
   */
  get_val(rpath:ref_path('data)):val_path('data) =
    {rpath with more = void}

  /**
   * Removes the data held at a path in the database. It won't be visible in the
   * current revision anymore, but will still be present in the history.
   */
  remove = %%path_remove%%

  /**
   * Given an intmap from the database, returns a key that is guaranteed not to be used
   * in that intmap. Successive calls return different results. */
  fresh_key = %%path_fresh_key%%

  /**
   * Checks a path for existence in the low-level db.
   */
  exists = %%path_exists%%

  /**
   * Searches words in all data held in a database map ; returns a list of keys in that map.
   * Only reference paths are accepted as arguments: searches in the past are not possible
   * in the current version of the database.
   */
  intmap_search = %%path_intmap_search%%

  stringmap_search = %%path_stringmap_search%%

  /**
   * Folds on a map with a range and a filter;
   * @param [f] function to be applied to every element of the map.
   * @param [start] where to start the fold, boundary included. 0 is the first key of the map.
   * @param [ending] where to stop, boundary not included. [none] is ending at the last key of the map,
   * [some(3)] is stopping at the second key of the map.
   * @param [filter] which keys are accepted for the fold. (x -> {true}) is all the keys of the map
   * are accepted.
   */

  intmap_fold_range = %%path_intmap_fold_range%%

  stringmap_fold_range = %%path_stringmap_fold_range%%

  /**
   * Queries the history of data that have been present at a path.
   * @param [from] where to start the history. 0 and negative is in the past from the
   * current revision, 1 and forward is from the first revision.
   * @param [length] how many revisions to return, from then on if positive, from then
   * backwards if negative. If 0, return all revisions from [from].
   * @example [history(@/p, 0, -10)] returns the last 10 revisions in reverse order.
   * @example [history(@/p, -9, 10)] returns the last 10 revisions in chronological order.
   * @example [history(@/p, 1, 0)] returns all revisions in chronological order.
   */
  history = %%path_history%%

  /**
   * Queries the history by timestamps.
   * returns a list of pairs [(timestamp, value)] with the values that have been held at
   * a path between the two given timestamps.
   */
  history_time(path, from : Date.date, to : Date.date) =
    bp = %%path_history_time%%
    r = bp(path, Date.ll_export(from), Date.ll_export(to))
    List.map((val, time) -> (Date.ll_import(time), val), r)

  /**
   * Returns the last modification date of a path.
   */
  modification_time(path) : Date.date =
    bp = %%path_modification_time%%
    Date.ll_import(bp(path))

  /**
   * Simple transaction handling: db-atomic execution of a function.
   * @param [db] the database to run the transaction in.
   * @param [f] a function that will be executed within a single transaction.
   * @return the return value of f as option, or [{none}] if the transaction commit failed.
   */
  transaction(_f) = error("")
    // tr = Transaction.new()
    // r = tr.try(-> some(f()), -> none)
    // match tr.commit() with {success} -> r | {failure} -> none

  /**
   * Dumps the contents of the whole database into an XML file.
   * @param [db] the database to dump.
   * @param [file] a file name to dump to.
   *
   * {2 XML format specification:}
   * - the database contents are exported inside a [<opa_database_root version="1.0"/>] node
   * - ints and floats are exported as strings representing their values
   * - text is in exported to escaped XML strings, in double-quotes, or the same way as binary
   *   data if it contains null characters.
   * - binary data is converted to base64 and inserted into a [<base64/>] node
   * - records [r] with fields [f1...fn] are exported as [<f1>r.f1</f1>...<fn>r.fn</fn>]
   * - maps [m] are exported as [<map></map>] nodes, containing a list of nodes of the form
   *   [<entry><key>k</key><value>v</value></entry>] for each key-value pair [k], [v].
   *
   * The resulting file can be used for import in another database with [opa-db-tool --import]
   * (note that the export can also be done from [opa-db-tool]).
   */
  /* todo when we make them available:
   * - sets are unsupported at the moment
   */
  export_to_xml = %%dbser.dump_db%%

  open(_name, host, port) =
    match MongoDriver.open(50*1024, 2, true, host, port, true) with
    | {success = db} -> db
    | ~{failure} ->
      do Log.error("Database", "Error on openning mongo database \"{host}\"\n{failure}")
      System.exit(1)

}}


`<-` = Db.`<-`


@opacapi
@abstract
type dbset('a) =
  / { empty }
  / { reply: Mongo.reply }


/**
 * {1 Interface}
 */

DbSet = {{

  build(db:Mongo.db, path:list(string), selector):dbset('a) =
    id = List.to_string_using("", "", ".", path)
    reply=MongoDriver.query(db, 0, "test.{id}", 0, 5000, selector, none)
    match reply with
    | {none} ->
      do Log.error("DbGen/Query", "(failure) Read tn {id} set doesn't returns anything")
      error("DbSet build error")
    | {some=reply} -> ~{reply}

  update(db:Mongo.db, path:list(string), selector, update) =
    id = List.to_string_using("", "", ".", path)
    tag = Bitwise.lor(0, MongoCommon.UpsertBit)
    tag = Bitwise.lor(tag, MongoCommon.MultiUpdateBit)
    reply=MongoDriver.update(db, tag, "test.{id}", selector, update)
    match reply with
    | {false} ->
      do Log.error("DbGen/Query", "(failure) Read tn {id} set doesn't returns anything")
      error("DbSet update error")
    | {true} -> void

  //fold = %%badoplink_fold_dbset%% : dbset('a), 'acc, ('acc, 'a -> 'acc) -> 'acc

  to_list(dbset:dbset('a)) =
    opavalue(reply, i) =
      match MongoCommon.reply_document(reply, i) with
      | {none} ->
        do Log.error("DbGen/Mongo", "Unexpected error : can't retreive document {i}")
        none
      | {some=doc} ->
        match Bson.doc2opa(doc) with
        | {none} ->
          do Log.error("DbGen/Mongo", "(failure) dbset unserialize {doc} from {@typeval('a)}")
          {none}
        | opa -> opa:option('a)
    rec aux(reply, i, acc) =
      if i < 0 then acc
      else
        match opavalue(reply, i) with
        | {none} -> aux(reply, i-1, acc)
        | {some=opa} -> aux(reply, i-1, opa +> acc)
    match dbset with
    | {empty} -> []
    | ~{reply} -> aux(reply, MongoCommon.reply_numberReturned(reply) - 1, [])

}}

@opacapi @abstract type DbSet.query = Bson.document

@opacapi Db_open = Db.open
@opacapi Db_build_path_raw  = Db.build_path_raw
@opacapi Db_build_vpath = Db.build_vpath
@opacapi Db_build_vpath_sub = Db.build_vpath_sub
@opacapi Db_build_rpath = Db.build_rpath
@opacapi Db_read = Db.read
@opacapi Db_write = Db.write
@opacapi Db_option = Db.option
@opacapi DbSet_build = DbSet.build
@opacapi DbSet_update = DbSet.update
@opacapi DbSet_empty = {empty}
@opacapi DbSet_opa2doc = Bson.opa2doc
