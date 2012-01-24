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

package stdlib.database.mongo

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

@opacapi type badoplink_database = Db.mongo
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
  remove : -> void
}

@opacapi type opa_transaction_t('a) = external
@opacapi type dbgraph_diff = external

type Db.mongo = {
  name : string;
  db : Mongo.db;
  cols : list(string)
}


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


   gen_read(id, query, uncap, default:'a) =
     // TODO - ...
     value2opa(value:Bson.value):option('a) =
       Bson.doc2opa_default([{name="value"; ~value}], default)
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
               do Log.error("DbGen/Mongo", "(failure) uncap bson document {document}")
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

   undotdoc(doc:Bson.document, next:Bson.document -> option('a)):option('a) =
     match doc with
     | [~{name value}] ->
       match Parser.try_parse(
               Rule.parse_list_sep(false, Rule.ident, Rule.of_string(".")), name) with
       | {none} ->
         do Log.error("DbGen/Mongo",
            "(failure) uncap0 unexpected structure of document : {doc}")
         none
       | {some = [_]} | {some = []} ->
         do Log.error("DbGen/Mongo", "(failure) undotdoc can't undot")
         none
       | {some = fields} ->
         match List.foldr(name, a -> {Document = [{~name value=a}]}, fields, value) with
         | {Document = doc} -> next(doc)
         | _ ->
           do Log.error("DbGen/Mongo", "Undot Document (assert false)")
           none
         end
       end
     | _ ->
       do Log.error("DbGen/Mongo",
          "(failure) undot unexpected structure of document : {doc}")
       none
     end

  defaultns(db:Db.mongo) = "{db.name}._default"

   /* ********************************************
    * {3 Builder of composed path}
    * *******************************************/
   build_vpath_compose(_:Db.mongo, path:list(string), default:'data,
                       elements:list((string, path_t(black, 'data)))):val_path('data) =
     id = Uri.to_string(~{path fragment=none query=[] is_directory=false is_from_root=true})
     read() = (
       #<Ifstatic:DBGEN_DEBUG>
       do Log.notice("DbGen/Mongo", "Db.build_vpath_compose : Start reading")
       #<End>
       (value, nothing) =
         List.fold(
           (field, path), (acc, nothing) ->
             #<Ifstatic:DBGEN_DEBUG>
             do Log.notice("DbGen/Mongo",
                  "Db.build_vpath_compose : field {field} start reading")
             #<End>
             backfield = OpaValue.Record.field_of_name_unsafe(field)
             match Db.option(path) with
             | {none} ->
               data = OpaValue.Record.unsafe_dot(default, backfield)
               (OpaValue.Record.add_field(acc, backfield, data), nothing)
             | {some=data} ->
               (OpaValue.Record.add_field(acc, backfield, data), false)
           , elements, (OpaValue.Record.empty_constructor(), true)
         )
       if nothing then {none} else {some = OpaValue.Record.make_record(value)}
     )
     {~id ~default ~read more}

   build_rpath_compose(db:Db.mongo, path:list(string), default:'data,
                       elements:list((string, ref_path(black)))):ref_path('data) =
     vpath = build_vpath_compose(db, path, default, @unsafe_cast(elements))
     write(data) =
       List.fold(
         (field, path), b ->
           backfield = OpaValue.Record.field_of_name_unsafe(field)
           data = OpaValue.Record.unsafe_dot(data, backfield)
           b && Db.write_option(path, data)
         , elements, true
       )
     remove() = List.iter((_field, path) -> Db.remove(path), elements)
     { vpath with more=~{write remove} }

   /* ********************************************
    * {3 Builder of sub path}
    * *******************************************/
   build_vpath_sub(db:Db.mongo, path:list(string), default:'data, rpath:list(string), partial:list(string))
   : val_path('data) =
     ns = defaultns(db)
     rid = Uri.to_string(~{path=rpath fragment=none query=[] is_directory=false is_from_root=true})
     id = Uri.to_string(~{path fragment=none query=[] is_directory=false is_from_root=true})
     docpath = "value" +> partial
     selector = [{name = "_id"; value = {String = rid}}]
     filter = [
       {name = "_id"; value = {Boolean = false}},
       {name = List.to_string_using("", "", ".", docpath); value = {Boolean = true}}
     ]
     rec uncap(doc:Bson.document) =
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
       | [{name = "value"; ~value}] -> aux(partial, value)
       | _ -> undotdoc(doc, uncap)
       end
     {
       ~id ~default
       more = void
       read() =
         query = MongoDriver.query(db.db, 0, ns, 0, 1, selector, some(filter))
         gen_read(id, query, uncap, default)
     }

   build_rpath_sub(db:Db.mongo, path:list(string), default:'data, rpath:list(string), partial:list(string)) : ref_path('data) =
     ns = defaultns(db)
     rid = Uri.to_string(~{path=rpath fragment=none query=[] is_directory=false is_from_root=true})
     vpath = build_vpath_sub(db, path, default, rpath, partial)
     selector = [{name = "_id"; value = {String = rid}}]
     tags = Bitwise.lor(0, MongoCommon.UpsertBit)
     field = "data.{List.to_string_using("", "", ".", partial)}"
     write(data) =
       update = Bson.opa_to_document(field, data, @typeof(default))
       update = [{name="$set"; value={Document = update}}]
       #<Ifstatic:DBGEN_DEBUG>
       do Log.notice("DbGen/Mongo", "Db.build_rpath_sub : Selector {selector}")
       do Log.notice("DbGen/Mongo", "Db.build_rpath_sub : Update {update}")
       #<End>
       MongoDriver.update(db.db, tags, ns, selector, update)
     remove() =
       unset = [{name="$unset" value={Document = [{name=field value={Int32 = 0}}]}}]
       if not(MongoDriver.update(db.db, 0, ns, selector, unset)) then
         Log.error("DbGen/Mongo", "(failure) An error occurs when removing subpath '{path}'")
       #<Ifstatic:DBGEN_DEBUG>
       else Log.notice("DbGen/Mongo", "(success) subpath '{path}' removed ")
       #<End>
     {vpath with more=~{write remove}}


   /* ********************************************
    * {3 Builder of declared path}
    * *******************************************/
   build_vpath(db:Db.mongo, path:list(string), default:'data):val_path('data) =
     ns = defaultns(db)
     id = Uri.to_string(~{path fragment=none query=[] is_directory=false is_from_root=true})
     selector = [{name = "_id"; value = {String = id}}]
     filter = [{name = "_id"; value = {Boolean = false}}]
     read() =
       rec uncap(doc) = match doc with
         | [{name = "value"; ~value}] -> some(value)
         | _ -> undotdoc(doc, uncap)
       query = MongoDriver.query(db.db, 0, ns, 0, 1, selector, some(filter))
       gen_read(id, query, uncap, default)
     ~{id read default more=void}

   build_rpath(db, path:list(string), default:'data):ref_path('data) =
     ns = defaultns(db)
     vpath = build_vpath(db, path, default) : val_path('data)
     selector = [{name = "_id"; value = {String = vpath.id}}]
     tags = Bitwise.lor(0, MongoCommon.UpsertBit)
     write(data:'data) =
       update = Bson.opa2doc({_id = vpath.id; data=data})
       MongoDriver.update(db.db, tags, ns, selector, update)
     remove() =
       if not(MongoDriver.delete(db.db, 0, ns, selector)) then
         Log.error("DbGen/Mongo", "(failure) An error occurs when removing subpath '{path}'")
       #<Ifstatic:DBGEN_DEBUG>
       else Log.notice("DbGen/Mongo", "(success) subpath '{path}' removed ")
       #<End>
     { vpath with more=~{write remove} }

   update_path(db:Db.mongo, path:list(string), update) =
     ns = defaultns(db)
     id = Uri.to_string(~{path fragment=none query=[] is_directory=false is_from_root=true})
     selector = [{name = "_id"; value = {String = id}}]
     tags = Bitwise.lor(0, MongoCommon.UpsertBit)
     MongoDriver.update(db.db, tags, ns, selector, update)


  /**
   * Writes data at the given path in the database.
   * [do write(@/path,42)] is equivalent to [do /path <- 42]
   *
   * @example [write(@/path,42)] initializes or updates the data at path [/path]
   */
  write(path:ref_path('data), data:'data) =
    if not((path.more:path_ref_p('data)).write(data)) then
      Log.error("DbGen/Mongo", "Write error on path {path.id}")

  write_option(path, data) =
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
  remove(rpath:ref_path('data)):void =
    rpath.more.remove()

  // /**
  //  * Given an intmap from the database, returns a key that is guaranteed not to be used
  //  * in that intmap. Successive calls return different results. */
  // fresh_key = %%path_fresh_key%%

  // /**
  //  * Checks a path for existence in the low-level db.
  //  */
  // exists = %%path_exists%%

  // /**
  //  * Searches words in all data held in a database map ; returns a list of keys in that map.
  //  * Only reference paths are accepted as arguments: searches in the past are not possible
  //  * in the current version of the database.
  //  */
  // intmap_search = %%path_intmap_search%%

  // stringmap_search = %%path_stringmap_search%%

  // /**
  //  * Folds on a map with a range and a filter;
  //  * @param [f] function to be applied to every element of the map.
  //  * @param [start] where to start the fold, boundary included. 0 is the first key of the map.
  //  * @param [ending] where to stop, boundary not included. [none] is ending at the last key of the map,
  //  * [some(3)] is stopping at the second key of the map.
  //  * @param [filter] which keys are accepted for the fold. (x -> {true}) is all the keys of the map
  //  * are accepted.
  //  */

  // intmap_fold_range = %%path_intmap_fold_range%%

  // stringmap_fold_range = %%path_stringmap_fold_range%%

  // /**
  //  * Queries the history of data that have been present at a path.
  //  * @param [from] where to start the history. 0 and negative is in the past from the
  //  * current revision, 1 and forward is from the first revision.
  //  * @param [length] how many revisions to return, from then on if positive, from then
  //  * backwards if negative. If 0, return all revisions from [from].
  //  * @example [history(@/p, 0, -10)] returns the last 10 revisions in reverse order.
  //  * @example [history(@/p, -9, 10)] returns the last 10 revisions in chronological order.
  //  * @example [history(@/p, 1, 0)] returns all revisions in chronological order.
  //  */
  // history = %%path_history%%

  // /**
  //  * Queries the history by timestamps.
  //  * returns a list of pairs [(timestamp, value)] with the values that have been held at
  //  * a path between the two given timestamps.
  //  */
  // history_time(path, from : Date.date, to : Date.date) =
  //   bp = %%path_history_time%%
  //   r = bp(path, Date.ll_export(from), Date.ll_export(to))
  //   List.map((val, time) -> (Date.ll_import(time), val), r)

  // /**
  //  * Returns the last modification date of a path.
  //  */
  // modification_time(path) : Date.date =
  //   bp = %%path_modification_time%%
  //   Date.ll_import(bp(path))

  // /**
  //  * Simple transaction handling: db-atomic execution of a function.
  //  * @param [db] the database to run the transaction in.
  //  * @param [f] a function that will be executed within a single transaction.
  //  * @return the return value of f as option, or [{none}] if the transaction commit failed.
  //  */
  // transaction(_f) = error("")
  //   // tr = Transaction.new()
  //   // r = tr.try(-> some(f()), -> none)
  //   // match tr.commit() with {success} -> r | {failure} -> none

  // /**
  //  * Dumps the contents of the whole database into an XML file.
  //  * @param [db] the database to dump.
  //  * @param [file] a file name to dump to.
  //  *
  //  * {2 XML format specification:}
  //  * - the database contents are exported inside a [<opa_database_root version="1.0"/>] node
  //  * - ints and floats are exported as strings representing their values
  //  * - text is in exported to escaped XML strings, in double-quotes, or the same way as binary
  //  *   data if it contains null characters.
  //  * - binary data is converted to base64 and inserted into a [<base64/>] node
  //  * - records [r] with fields [f1...fn] are exported as [<f1>r.f1</f1>...<fn>r.fn</fn>]
  //  * - maps [m] are exported as [<map></map>] nodes, containing a list of nodes of the form
  //  *   [<entry><key>k</key><value>v</value></entry>] for each key-value pair [k], [v].
  //  *
  //  * The resulting file can be used for import in another database with [opa-db-tool --import]
  //  * (note that the export can also be done from [opa-db-tool]).
  //  */
  // /* todo when we make them available:
  //  * - sets are unsupported at the moment
  //  */
  // export_to_xml(file:string) =


  open(name, host, port) =
    match MongoDriver.open(50*1024, 2, true, host, port, true) with
    | {success = db} -> ~{db name cols=["default"]}
    | ~{failure} ->
      do Log.error("Database", "Error on openning mongo database \"{host}\"\n{failure}")
      System.exit(1)

  drop(db:Db.mongo) =
    List.iter(
      col ->
        if MongoDriver.delete(db.db, 0, "{db.name}.{col}", []) then
          Log.notice("DbGen/Mongo", "(success) Remove {col} at database {db.name}")
        else
          Log.error("DbGen/Mongo", "(failure) Error while removing {col} at database {db.name}")
    , db.cols)

}}


`<-` = Db.`<-`


@opacapi
@abstract
type dbset('a) = { reply: Mongo.reply default : 'a}


/**
 * {1 Interface}
 */

DbSet = {{

  index(db:Db.mongo, path:list(string), idx) =
    id = List.to_string_using("", "", ".", path)
    key = List.map((name -> ~{name value={Int32=1}}), idx)
    opt = 0
    opt = Bitwise.lor(opt, MongoCommon.UniqueBit)
    match MongoDriver.create_index(db.db, "{db.name}.{id}", key, opt) with
    | {true} ->
      do Log.notice("DbGen/Mongo", "(success) Index {idx} at {path} as been created")
      void
    | {false} ->
      do Log.error("DbGen/Mongo", "(failure) Error when creating index {idx} at {path}")
      error("Error when creating index")

  indexes(db:Db.mongo, path:list(string), idxs) =
    List.iter(index(db, path, _), idxs)

  build(db:Db.mongo, path:list(string), selector, default:'a, nb):dbset('a) =
    #<Ifstatic:DBGEN_DEBUG>
    do Log.notice("DbGen/Mongo", "DbSet.build : Selector {selector}")
    #<End>
    id = List.to_string_using("", "", ".", path)
    reply=MongoDriver.query(db.db, 0, "{db.name}.{id}", 0, nb, selector, none)
    match reply with
    | {none} ->
      do Log.error("DbGen/Query", "(failure) Read tn {id} set doesn't returns anything")
      error("DbSet build error")
    | {some=reply} -> ~{reply default}

  update(db:Db.mongo, path:list(string), selector, update) =
    id = List.to_string_using("", "", ".", path)
    tag = Bitwise.lor(0, MongoCommon.UpsertBit)
    //tag = Bitwise.lor(tag, MongoCommon.MultiUpdateBit)
    #<Ifstatic:DBGEN_DEBUG>
    do Log.notice("DbGen/Mongo", "DbSet.update : Selector {selector}")
    do Log.notice("DbGen/Mongo", "DbSet.update : Update {update}")
    #<End>
    reply=MongoDriver.update(db.db, tag, "{db.name}.{id}", selector, update)
    match reply with
    | {false} ->
      do Log.error("DbGen/Query", "(failure) Read tn {id} set doesn't returns anything")
      error("DbSet update error")
    | {true} -> void

  @private fold_doc(init, dbset:dbset, f) =
    match dbset with
    | ~{reply default=_} ->
      size = MongoCommon.reply_numberReturned(reply)
      rec aux(i, acc) =
        if i == size then acc
        else
          match MongoCommon.reply_document(reply, i) with
          | {none} ->
            do Log.error("DbGen/Mongo", "Unexpected error : can't retreive document {i}")
            aux(i+1, acc)
          | {some=doc} -> aux(i+1, f(acc, doc))
      aux(0, init)

  fold(init:'acc, dbset:dbset('a))(f:'acc, 'a -> 'acc) =
    fopa(acc, doc, f) =
      match Bson.doc2opa_default(doc, dbset.default) with
      | {none} ->
        do Log.error("DbGen/Mongo", "(failure) dbset unserialize {doc} from {OpaType.to_pretty(@typeval('a))} with default value : {dbset.default}")
        acc
      | {some=opa} -> f(acc, opa:'a)
    fold_doc(init, dbset, fopa(_, _, f))

  to_list(dbset:dbset('a)) = fold([], dbset)(acc, a -> a +> acc)

  to_map(dbset:dbset('a)):map('key, 'value) =
    fold_doc(Map.empty, dbset, (map:map('key, 'value), doc:Bson.document ->
        match List.extract_p(x -> x.name == "_id", doc) with
        | ({some=kdoc}, vdoc) ->
          //Traditional hack... (We really need bson.opa rewriting)
          kdoc = {kdoc with name="value"}
          match Bson.doc2opa([kdoc]):option('key) with
          | {none} ->
            do Log.error("DbGen/Mongo",
                 "(failure) map unserialize key {kdoc} from {@typeval('key)}")
            map
          | {some=key} ->
            match Bson.doc2opa_default(vdoc, @unsafe_cast(dbset.default)):option('value) with
            | {none} ->
              do Log.error("DbGen/Mongo",
                   "(failure) map unserialize value {vdoc} from {@typeval('value)}")
              map
            | {some=val} -> Map.add(key, val, map)
            end
          end
        | ({none}, _) ->
          do Log.error("DbGen/Mongo",
             "(failure) map unserialize key not found inside {doc}")
          map
        end
      )
    )

  map_to_uniq(dbset:dbset('a)):option('value) =
    fold_doc(none, dbset, (opt, doc ->
        do @assert(Option.is_none(opt))
        match List.extract_p(x -> x.name == "_id", doc) with
        | (_, vdoc) ->
          match Bson.doc2opa_default(vdoc, @unsafe_cast(dbset.default)):option('value) with
          | {none} ->
            do Log.error("DbGen/Mongo",
                 "(failure) map_to_uniq unserialize value {vdoc} from {@typeval('value)}")
            {none}
          | {some=_} as res -> res
          end
      )
    )

  map_to_uniq_def(dbset:dbset('a)):'value =
    match map_to_uniq(dbset) with
    | {some=v:'value} -> v
    | {none} -> @unsafe_cast(dbset.default)

  set_to_uniq(dbset:dbset('a)):option('a) =
    match to_list(dbset) with
    | [] -> none
    | [uniq] -> some(uniq)
    | _ -> do @assert(false) error("___")

  set_to_uniq_def(dbset:dbset('a)):'a =
    match set_to_uniq(dbset) with
    | {none} -> dbset.default
    | {some = uniq} -> uniq



  add_to_document(doc, name, value, ty):Bson.document =
    List.append(doc, Bson.opa_to_document(name, value, ty))


}}

@opacapi @abstract type DbSet.query = Bson.document

@opacapi Db_open = Db.open
@opacapi Db_update_path = Db.update_path
@opacapi Db_build_vpath = Db.build_vpath
@opacapi Db_build_rpath = Db.build_rpath

@opacapi Db_build_rpath_compose = Db.build_rpath_compose
@opacapi Db_build_vpath_compose = Db.build_vpath_compose

@opacapi Db_build_vpath_sub = Db.build_vpath_sub
@opacapi Db_build_rpath_sub = Db.build_rpath_sub
@opacapi Db_read = Db.read
@opacapi Db_write = Db.write
@opacapi Db_option = Db.option
@opacapi DbSet_build = DbSet.build
@opacapi DbSet_update = DbSet.update
@opacapi DbSet_opa2doc = Bson.opa2doc
@opacapi DbSet_add_to_document = DbSet.add_to_document
@opacapi DbSet_indexes = DbSet.indexes
@opacapi DbSet_to_map = DbSet.to_map
@opacapi DbSet_map_to_uniq = DbSet.map_to_uniq
@opacapi DbSet_map_to_uniq_def = DbSet.map_to_uniq_def
@opacapi DbSet_set_to_uniq = DbSet.set_to_uniq
@opacapi DbSet_set_to_uniq_def = DbSet.set_to_uniq_def
@opacapi DbSet_default = Option.default
@opacapi DbSet_empty = {empty}

