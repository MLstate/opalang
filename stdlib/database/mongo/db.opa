/*
    Copyright © 2011, 2012 MLstate

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

@opacapi @abstract type DbMongo.t = {
  name : string;
  db : Mongo.db;
  cols : list(string)
}

@opacapi @abstract type DbMongo.engine = void

type DbMongo.val_path('a) = Db.val_path('a, DbMongo.engine)

type DbMongo.ref_path('a) = Db.ref_path('a, DbMongo.engine)

// Private type
type DbMongo.private.path_t('kind, 'data) = {
  id : string;
  read : -> option('data);
  default : 'data;
  more : 'kind;
}

@opacapi type DbMongo.private.val_path('a) = DbMongo.private.path_t(void, 'a)

@opacapi type DbMongo.private.ref_path('a) = DbMongo.private.path_t({
  write : 'a -> bool
  remove : -> void
}, 'a)

/**
 * {1 Interface}
 */

DbMongo = {{

   /**
    * {2 Utils}
    */

   @package gen_read(id, query, uncap, default:'a) =
     // TODO - ...
     value2opa(value:Bson.value):option('a) =
       match value with
       | {Document = doc} -> Bson.doc2opa_default(doc, default)
       | _ -> Bson.doc2opa_default([{name="value"; ~value}], default)
     match query with
         | {none} ->
           do Log.error("DbGen/Mongo", "(failure) read from {id} didn't return anything")
           none
         | {some=reply} ->
           match MongoCommon.reply_document(reply, 0) with
           | {none} ->
             do Log.error("DbGen/Mongo", "(failure) read from {id} didn't return any document")
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

   @package undotdoc(doc:Bson.document, next:Bson.document -> option('a)):option('a) =
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

  @package defaultns(db:DbMongo.t) = "{db.name}._default"

  @package path_to_id(path:list(string)) =
    Uri.to_string(~{path fragment=none query=[] is_directory=false is_from_root=true})

   /* ********************************************
    * {3 Builder of composed path}
    * *******************************************/
   @package build_vpath_compose(_:DbMongo.t, path:list(string), default:'data,
                       elements:list((string, DbMongo.private.path_t(black, 'data)))):DbMongo.private.val_path('data) =
     id = path_to_id(path)
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
             match path.read() with
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

   @package build_rpath_compose(db:DbMongo.t, path:list(string), default:'data,
                       elements:list((string, DbMongo.private.ref_path(black)))):DbMongo.private.ref_path('data) =
     vpath = build_vpath_compose(db, path, default, @unsafe_cast(elements))
     write(data) =
       List.fold(
         (field, path), b ->
           backfield = OpaValue.Record.field_of_name_unsafe(field)
           data = OpaValue.Record.unsafe_dot(data, backfield)
           b && path.more.write(data)
         , elements, true
       )
     remove() = List.iter((_field, path) -> path.more.remove(), elements)
     { vpath with more=~{write remove} }

   /* ********************************************
    * {3 Builder of sub path}
    * *******************************************/
   @package build_vpath_sub(db:DbMongo.t, path:list(string), default:'data, rpath:list(string), partial:list(string))
   : DbMongo.private.val_path('data) =
     ns = defaultns(db)
     rid = path_to_id(rpath)
     id = path_to_id(path)
     selector = [{name = "_id"; value = {String = rid}}]
     filter = [
       {name = "_id"; value = {Boolean = false}},
       {name = List.to_string_using("", "", ".", partial); value = {Boolean = true}}
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
         aux(partial, {Document = doc})
     {
       ~id ~default
       more = void
       read() =
         #<Ifstatic:DBGEN_DEBUG>
         do Log.notice("DbGen/Mongo", "Db.build_vpath_sub : Selector {selector}")
         do Log.notice("DbGen/Mongo", "Db.build_vpath_sub : Filter {filter}")
         #<End>
         query = MongoDriver.query(db.db, 0, ns, 0, 1, selector, some(filter))
         gen_read(id, query, uncap, default)
     }

   @package build_rpath_sub(db:DbMongo.t, path:list(string), default:'data, rpath:list(string), partial:list(string)) : DbMongo.private.ref_path('data) =
     ns = defaultns(db)
     rid = path_to_id(rpath)
     vpath = build_vpath_sub(db, path, default, rpath, partial)
     selector = [{name = "_id"; value = {String = rid}}]
     tags = Bitwise.lor(0, MongoCommon.UpsertBit)
     field = List.to_string_using("", "", ".", partial)
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
   @package build_vpath(db:DbMongo.t, path:list(string), default:'data, const:bool):DbMongo.private.val_path('data) =
     ns = defaultns(db)
     id = path_to_id(path)
     selector = [{name = "_id"; value = {String = id}}]
     filter = [{name = "_id"; value = {Boolean = false}}]
     uncap =
       if const then (
         rec uncap(doc:Bson.document) = match doc with
           | [{name = "value"; ~value}] -> some(value)
           | _ -> undotdoc(doc, uncap)
         uncap
       ) else (
         doc ->
           some({Document = doc})
       )
     read() =
       query = MongoDriver.query(db.db, 0, ns, 0, 1, selector, some(filter))
       gen_read(id, query, uncap, default)
     ~{id read default more=void}

   @package build_rpath(db, path:list(string), default:'data, const:bool):DbMongo.private.ref_path('data) =
     ns = defaultns(db)
     vpath = build_vpath(db, path, default, const) : DbMongo.private.val_path('data)
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

   @package update_path(db:DbMongo.t, path:list(string), update) =
     ns = defaultns(db)
     id = path_to_id(path)
     selector = [{name = "_id"; value = {String = id}}]
     tags = Bitwise.lor(0, MongoCommon.UpsertBit)
     if not(MongoDriver.update(db.db, tags, ns, selector, update)) then
       Log.error("DbGen/Mongo", "(failure) An error occurs while updating path '{path}' with {update}")
     #<Ifstatic:DBGEN_DEBUG>
     else Log.notice("DbGen/Mongo", "(success) path '{path}' updated with {update}")
     #<End>


  /**
   * Writes data at the given path in the database.
   * [do write(@/path,42)] is equivalent to [do /path <- 42]
   *
   * @example [write(@/path,42)] initializes or updates the data at path [/path]
   */
  @package write(path:DbMongo.private.ref_path('data), data:'data) =
    if not((path.more).write(data)) then
      Log.error("DbGen/Mongo", "Write error on path {path.id}")

  @package write_option(path:DbMongo.private.ref_path('data), data) =
    (path.more).write(data)

  @package `<-`(d,a) = write(d,a)

  /**
   * Reads the data currently held at a reference path.
   *
   * @example [read(@/path)] is equivalent to [/path]
   */
  @package read(path:DbMongo.private.path_t('any, 'data)):'data = path.read() ? path.default

  /**
   * Reads the data currently held at a reference path.
   *
   * @example [read(@/path)] is equivalent to [/path]
   */
  @package option(path:DbMongo.private.path_t('any, 'data)):option('data) = path.read()

  /**
   * Turns a reference-path into a value-path, in fact taking a snapshot.
   *
   * @example [get_val(@/path)] is equivalent to [!/path]
   */
  @package get_val(rpath:DbMongo.private.ref_path('data)):DbMongo.private.val_path('data) =
    {rpath with more = void}

  /**
   * Removes the data held at a path in the database. It won't be visible in the
   * current revision anymore, but will still be present in the history.
   */
  @package remove(rpath:DbMongo.private.ref_path('data)):void =
    rpath.more.remove()

  @private Init = {{

    error(msg) = do Log.error("DbGen/Mongo", msg) System.exit(1)

    platform_error() =
      error("Can't auto initialize MongoDB for your platform.
Please download and init MongoDB yourself.
Download : http://www.mongodb.org/downloads
Quick start: http://www.mongodb.org/display/DOCS/Quickstart
Then use option --db-remote instead of --db-local.
")

    jlog(msg) = Log.notice("DbGen/Mongo", msg)

    default_archive =
      #<Ifstatic:IS_LINUX 1>
        some("mongodb-linux-i686-2.0.2")
      #<Else>#<Ifstatic:IS_MAC 1>
        some("mongodb-osx-x86_64-2.0.2")
      #<Else>
        none
      #<End>#<End>

    default_url =
      Option.map(default_archive ->
        "http://fastdl.mongodb.org/linux/{default_archive}.tgz",
        default_archive)

    default_remote = {
      seeds=[] : list((string, int))
      bufsize = 50*1024
      poolsize = 2
      log = true
      auth = [] : Mongo.auths
    }

    default_local() = "{%%BslFile.mlstate_dir%%(void)}/mongo"

    open_remote(name, args, default_seed) =
      args = match args.seeds with
        | [] -> {args with seeds = [default_seed]}
        | _ -> args
      connect() =
        match args.seeds with
        | [(host, port)] ->
          MongoDriver.open(args.bufsize, args.poolsize, false/*allowslaveok*/, true, host, port, args.log, args.auth)
        | seeds ->
          mdb = MongoReplicaSet.init(name, args.bufsize, args.poolsize, false/*allowslaveok*/, args.log, args.auth, seeds)
          match MongoReplicaSet.connect(mdb) with
          | {success=(_/*slaveok*/,m)} -> {success=m}
          | {~failure} -> {~failure}
          end
      match connect() with
      | {success = db} -> ~{db name cols=["default"]}
      | ~{failure} ->
        do Log.error("DbGen/Mongo", "Error while opening database \"{name}\"\n{failure}")
        System.exit(1)


    open_local(name, args, seed) =
      match default_archive with
      | {none} -> platform_error()
      | {some=default_archive} ->
        default_url = default_url ? platform_error()
        path = args.path
        /* Check the given path */
        do (
          if %%BslFile.exists%%(path) then (
            if not(%%BslFile.is_directory%%(path)) then
              error("File \'{path}\' exists but is not a directory")
          ) else if not(%%BslFile.make_dir%%(path)) then
              error("Can not create directory \'{path}\'")
        )
        /* Check for MongoDB binary */
        do jlog("Looking for MongoDB in directory \'{path}\'")
        binpath = "{args.path}/{default_archive}/bin"
        tgzpath = "{args.path}/mongo.tgz"
        do (
          if %%BslFile.exists%%(binpath) then (
            if not(%%BslFile.is_directory%%(binpath)) then
              error("File \'{path}\' exists but is not a directory")
          ) else (
            do jlog("MongoDB does not seem to be installed in '{path}'")
            do jlog("Please wait while Opa downloading MongoDB from '{default_url}'...")
            _ = System.exec("wget {default_url} -O {tgzpath}", "")
            do jlog("MongoDB was downloaded ({tgzpath})")
            tarcmd = "tar -xvzf {tgzpath} -C {path}"
            do jlog("Uncompressing of MongoDB archive... ({tarcmd})")
            _ = System.exec(tarcmd, "")
            void
          )
        )
        /*Check for MongoDB server */
        pidpath = "/tmp/opaMongo.pid"
        datpath = "{path}/data"
        mngpath = "{binpath}/mongod"
        logpath = "{path}/mongo.log"
        mngargs = "--pidfilepath {pidpath} --noprealloc --bind_ip 0.0.0.0 --dbpath {datpath} --fork --logpath {logpath}"
        _ = %%BslFile.make_dir%%(datpath)
        do (
          rec aux(launch) =
            pid = %%BslFile.content_opt%%(pidpath)
            pid = Option.bind(x -> Parser.try_parse(Rule.natural, String.trim(x)), pid)
            match pid with
            | {some = pid} -> jlog("MongoDB seems launched (pid:{pid})")
            | {none} ->
              if launch then (
                do jlog("Launching MongoDB with {mngargs}")
                _ = System.exec("{mngpath} {mngargs}", "")
                aux(false)
              ) else (
                do Scheduler.wait(1000)
                aux(false)
              )
          aux(true)
        )
        open_remote(name, default_remote, seed)


    open(name:string, host, port) =
      seed = (host ? "localhost", port ? 27017)
      (suffix, msg) = match name with
      | "_no_name" -> ("", "")
      | _ -> (":{name}", "\"{name}\"")
      args = CommandLine.filter({
        title = "Options for Mongo database {msg}"
        init = { remote = default_remote }
        anonymous = []
        parsers = [
          {CommandLine.default_parser with
            names = ["--db-remote{suffix}"]
            description = "Use a remote mongo database(s), (default: {seed.f1}:{seed.f2})"
            param_doc = "host[:<port>][,host[:<port>]]*"
            on_param(acc) =
              do jlog("Hello")
              seed_parser = parser
                host=((![:] .)*)[:] port={Rule.natural} -> (Text.to_string(host), port)
              parser seeds={Rule.parse_list(seed_parser, Rule.of_string(","))} ->
                acc = match acc with
                  | {remote = acc} -> acc
                  | _ -> default_remote
                do jlog("Parsing seeds {seeds}")
                {no_params = {remote = {acc with seeds = List.append(acc.seeds, seeds)}}}
          },
          {CommandLine.default_parser with
            names = ["--db-remote-auth{suffix}"]
            description = "Set authorisation for databases"
            param_doc = "<user>@<dbname>:<password> (Note: no '@' in <user> and no ':' in <dbname>)"
            on_param(acc) =
              parser user=((![@] .)+)[@] dbname=((![:] .)+) [:] password=(.+) ->
                auth = {user=Text.to_string(user); dbname=Text.to_string(dbname); password=Text.to_string(password)}:Mongo.auth
                acc = match acc with
                  | {remote = acc} -> acc
                  | _ -> default_remote
                do jlog("Parsing auth {auth}")
                {no_params = {remote = {acc with auth = List.append(acc.auth, [auth])}}}
          },
          {CommandLine.default_parser with
            names = ["--db-local{suffix}"]
            description = "Use a local mongo database, (default: {default_local()})"
            param_doc = "path"
            on_encounter(_acc) =
              {opt_params = {local = {path = default_local()}}}
            on_param(_acc) =
              parser path=(.*) -> {no_params = {local = {path = Text.to_string(path)}}}
          },
        ]
      })
      match args with
      | ~{remote} -> open_remote(name, remote, seed)
      | ~{local} -> open_local(name, local, seed)
  }}

  open = Init.open

  @package drop(db:DbMongo.t) =
    List.iter(
      col ->
        if MongoDriver.delete(db.db, 0, "{db.name}.{col}", []) then
          Log.notice("DbGen/Mongo", "(success) Remove {col} at database {db.name}")
        else
          Log.error("DbGen/Mongo", "(failure) Error while removing {col} at database {db.name}")
    , db.cols)

}}



@abstract
@opacapi
type DbMongoSet.engine('a) = { reply: Mongo.reply default : 'a}

@opacapi
type DbMongoSet.t('a) = dbset('a, DbMongoSet.engine('a))


/**
 * {1 Interface}
 */

DbSet = {{

  @package path_to_id(path) = List.to_string_using("", "", ".", path)

  @package index(db:DbMongo.t, path:list(string), idx) =
    id = DbSet.path_to_id(path)
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

  @package indexes(db:DbMongo.t, path:list(string), idxs) =
    List.iter(index(db, path, _), idxs)

  @package build(db:DbMongo.t, path:list(string), selector, default:'a, skip, limit):DbMongoSet.engine('a) =
    #<Ifstatic:DBGEN_DEBUG>
    do Log.notice("DbGen/Mongo", "DbSet.build : Selector {selector}")
    #<End>
    id = DbSet.path_to_id(path)
    reply=MongoDriver.query(db.db, 0, "{db.name}.{id}", skip, limit, selector, none)
    match reply with
    | {none} ->
      do Log.error("DbGen/Query", "(failure) Read tn {id} set doesn't returns anything")
      error("DbSet build error")
    | {some=reply} -> ~{reply default}

  @package update(db:DbMongo.t, path:list(string), selector, update) =
    id = DbSet.path_to_id(path)
    tag = Bitwise.lor(0, MongoCommon.UpsertBit)
    tag = Bitwise.lor(tag, MongoCommon.MultiUpdateBit)
    #<Ifstatic:DBGEN_DEBUG>
    do Log.notice("DbGen/Mongo", "DbSet.update {db.name}.{id} : Selector {selector}")
    do Log.notice("DbGen/Mongo", "DbSet.update {db.name}.{id} : Update {update}")
    #<End>
    reply=MongoDriver.update(db.db, tag, "{db.name}.{id}", selector, update)
    match reply with
    | {false} ->
      do Log.error("DbGen/Query", "(failure) Read tn {id} set doesn't returns anything")
      error("DbSet update error")
    | {true} ->
      #<Ifstatic:DBGEN_DEBUG>
      do Log.notice("DbGen/Mongo", "(success) DbSet.update")
      #<End>
      void
    // TODO - Needed for error reporting
    // reply=MongoDriver.updatee(db.db, tag, ns, db.name, selector, update)
    // match MongoCommon.reply_to_result("DbGen/Mongo", 0, reply) with
    // | ~{success} ->
    //   #<Ifstatic:DBGEN_DEBUG>
    //   do Log.notice("DbGen/Mongo", "(success) DbSet.update \"{ns}\" {success}")
    //   #<End>
    //   void
    // | ~{failure} ->
    //   Log.error("DbGen/Mongo", "(failure) DbSet.update \"{ns}\" {failure}")
    // end


  @private fold_doc(init, dbset:DbMongoSet.engine, f) =
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

  @package iterator(dbset:DbMongoSet.engine('a)):iter('a) =
    match dbset with
    | ~{reply default=_} ->
      size = MongoCommon.reply_numberReturned(reply)
      rec aux(i) =
        if i == size then none
        else
          match MongoCommon.reply_document(reply, i) with
          | {none} ->
            do Log.error("DbGen/Mongo", "Unexpected error : can't retreive document {i}")
            aux(i+1)
          | {some=doc} ->
              match Bson.doc2opa_default(doc, dbset.default) with
              | {none} ->
                 // Note: we should really test for error before unserialize but it would have an adverse effect on performance.
                 if Bson.is_error(doc)
                 then
                   do Log.error("DbGen/Mongo",
                        "(failure) Unexpected error : MongoDB returned error document {Bson.string_of_doc_error(doc)}")
                   aux(i+1)
                 else
                   do Log.error("DbGen/Mongo",
                        "(failure) dbset unserialize {doc} from {OpaType.to_pretty(@typeval('a))} with default value : {dbset.default}")
                   aux(i+1)

              | {some=opa} -> {some = (opa, {next=->aux(i+1)})}
              end
          end
      {next= -> aux(0)}

  @private fold(init, dbset, f) =
    Iter.fold(f, iterator(dbset), init)

  @package to_list(dbset:DbMongoSet.engine('a)) = fold([], dbset, (a, acc -> a +> acc))

  @package to_map(dbset:DbMongoSet.engine('a)):map('key, 'value) =
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

  @package map_to_uniq(dbset:DbMongoSet.engine('a)):option('value) =
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

  @package map_to_docs(map:map('key, 'value)):list(Bson.document) =
    Map.fold(k, v, docs ->
      {hd = List.append(
        Bson.opa_to_document("_id", k, @typeval('key)),
        Bson.opa2doc(v))
       tl = docs}, map, [])

  @package set_to_docs(dbset:DbMongoSet.engine('a)):list(Bson.document) =
    fold_doc([], dbset, (l, d -> {hd=d tl=l}))

  @package map_to_uniq_def(dbset:DbMongoSet.engine('a)):'value =
    match map_to_uniq(dbset) with
    | {some=v:'value} -> v
    | {none} -> @unsafe_cast(dbset.default)

  @package set_to_uniq(dbset:DbMongoSet.engine('a)):option('a) =
    match to_list(dbset) with
    | [] -> none
    | [uniq] -> some(uniq)
    | _ -> do @assert(false) error("___")

  @package set_to_uniq_def(dbset:DbMongoSet.engine('a)):'a =
    match set_to_uniq(dbset) with
    | {none} -> dbset.default
    | {some = uniq} -> uniq

  @package add_to_document(doc, name, value, ty):Bson.document =
    List.append(doc, Bson.opa_to_document(name, value, ty))

  @package build_vpath(db:DbMongo.t, path:list(string), selector, default:'b, skip, limit,
              read_map:DbMongoSet.engine('a) -> option('b)):DbMongo.private.val_path('b) =
    {
      id = DbSet.path_to_id(path)
      read() = read_map(build(db, path, selector, @unsafe_cast(default), skip, limit)):option('b)
      default = default
      more = void
    }

  @package build_rpath(db:DbMongo.t, path:list(string), selector, default:'b, skip, limit,
              read_map:DbMongoSet.engine('a) -> option('b), write_map:'b -> Bson.document):DbMongo.private.ref_path('b) =
    id = DbSet.path_to_id(path)
    vpath = build_vpath(db, path, selector, default, skip, limit, read_map)
    write(data) =
      do update(db, path, selector,
           [{name="$set"; value={Document = write_map(data)}}]
         )
      true
    remove() =
      if not(MongoDriver.delete(db.db, 0, "{db.name}.{id}", selector)) then
        Log.error("DbGen/Mongo", "(failure) An error occurs when removing inside set '{path}'")
      #<Ifstatic:DBGEN_DEBUG>
      else Log.notice("DbGen/Mongo", "(success) removing inside set '{path}' removed ")
      #<End>
    {vpath with more=~{write remove}}

  @package build_rpath_collection(
             db:DbMongo.t,
             path:list(string),
             selector,
             default:'b,
             skip,
             limit,
             read_map:DbMongoSet.engine('a) -> option('b),
             write_map:'b -> list(Bson.document)):DbMongo.private.ref_path('b) =
    id = DbSet.path_to_id(path)
    vpath = build_vpath(db, path, selector, default, skip, limit, read_map)
    remove() =
      if not(MongoDriver.delete(db.db, 0, "{db.name}.{id}", selector)) then
        Log.error("DbGen/Mongo", "(failure) An error occurs when removing inside set '{path}'")
      #<Ifstatic:DBGEN_DEBUG>
      else Log.notice("DbGen/Mongo", "(success) removing inside set '{path}' removed ")
      #<End>
    write(datas) =
      do remove()
      #<Ifstatic:DBGEN_DEBUG>
      do Log.notice("DbGen/Mongo", "Write in collection {db.name}.{id}")
      #<End>
      MongoDriver.insert_batch(db.db, 0, "{db.name}.{id}", write_map(datas))
    {vpath with more=~{write remove}}



}}

@opacapi DbMongo_path_to_path(mongopath:DbMongo.private.path_t('kind, 'a)) = Db.build({
  id = mongopath.id;
  read() = mongopath.read() ? mongopath.default
  exists() = Option.is_some(mongopath.read())
  more = mongopath.more
  engine = void;
})

@opacapi @abstract type DbSet.query = Bson.document

@opacapi DbMongo_open = DbMongo.open

@opacapi DbMongo_update_path = DbMongo.update_path
@opacapi DbMongo_build_vpath = DbMongo.build_vpath
@opacapi DbMongo_build_rpath = DbMongo.build_rpath

@opacapi DbMongo_build_rpath_compose = DbMongo.build_rpath_compose
@opacapi DbMongo_build_vpath_compose = DbMongo.build_vpath_compose

@opacapi DbMongo_build_vpath_sub = DbMongo.build_vpath_sub
@opacapi DbMongo_build_rpath_sub = DbMongo.build_rpath_sub
@opacapi DbMongo_read = DbMongo.read
@opacapi DbMongo_write = DbMongo.write
@opacapi DbMongo_option = DbMongo.option
@opacapi DbSet_build = DbSet.build
@opacapi DbSet_update = DbSet.update
@opacapi DbSet_opa2doc = Bson.opa2doc
@opacapi DbSet_add_to_document = DbSet.add_to_document
@opacapi DbSet_indexes = DbSet.indexes
@opacapi DbSet_to_map = DbSet.to_map
@opacapi DbSet_iterator = DbSet.iterator
@opacapi DbSet_map_to_uniq = DbSet.map_to_uniq
@opacapi DbSet_map_to_uniq_def = DbSet.map_to_uniq_def
@opacapi DbSet_map_to_docs = DbSet.map_to_docs
@opacapi DbSet_set_to_uniq = DbSet.set_to_uniq
@opacapi DbSet_set_to_uniq_def = DbSet.set_to_uniq_def
@opacapi DbSet_set_to_docs = DbSet.set_to_docs
@opacapi DbSet_build_vpath = DbSet.build_vpath
@opacapi DbSet_build_rpath = DbSet.build_rpath
@opacapi DbSet_build_rpath_collection = DbSet.build_rpath_collection
@opacapi DbSet_default = Option.default
@opacapi DbSet_empty = {empty}

