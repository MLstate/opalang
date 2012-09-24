/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 *
 */

import-plugin unix

package stdlib.database.mongo

import stdlib.core.{date,map,parser}
//# <Ifstatic:OPA_BACKEND_QMLJS>
//import stdlib.apis.mongo.node
//# <Else>
import stdlib.apis.mongo
//# <End>
import stdlib.apis.mongo.common
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

@abstract
type DbMongo.t2 = {
  name : string;
  db : Mongo.db;
  cols : list(string);
}

@opacapi @abstract
type DbMongo.t = {
     get : -> DbMongo.t2
     sync_operation : (DbMongo.t2 -> void) -> void
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

    @package updateerr(db:DbMongo.t2, flags:int, ns:string, selector:Bson.document, update:Bson.document, id , upsert): void =
    reply = MongoDriver.updatee(db.db, flags, ns, db.name, selector, update)
//     # <Ifstatic:OPA_BACKEND_QMLJS>
//       _=id
//       _=upsert
//       if reply then void
//       else error("Update error")
//     # <Else>
       match reply with
       | {none} ->
         do Log.error("DbGen/Query", "(failure) Read {id} didn't return anything")
         error("DbGen/Mongo: Network Error")
       | {some = reply} ->
         match MongoCommon.reply_document(reply, 0) with
         | {none} -> error("DbGen/Mongo: Protocol Error (1)")
         | {some=doc} ->
           match Bson.find_float(doc, "ok") with
           | {none} -> error("DbGen/Mongo: Protocol Error (2)")
           | {some = ok} ->
             if ok != 1.0 then error("DbGen/Mongo: GetLastError Error")
             else match Bson.find_element(doc, "err") with
             | {none} -> void
             | {some = {value = {String = str} ...}} -> error("DbGen/Mongo: {str}")
             | {some = {value = {Null} ...}} ->
               if not(upsert) &&
                  not(Bson.find_bool(doc, "updatedExisting")
                      ? error("DbGen/Mongo: Protocol Error (4)"))
               then error("DbGen/Mongo: Update Error")
               else
                 #<Ifstatic:DBGEN_DEBUG>
                 do Log.notice("DbGen/Mongo", "(success) DbSet.update {doc}")
                 #<End>
                 void
             | {some = err} -> error("DbGen/Mongo: Protocol Error (3) {err}")
//     # <End>

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
//             # <Ifstatic:OPA_BACKEND_QMLJS>
//             do MongoDriver.cclose(reply)
//             # <End>
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

  @package defaultns(db:DbMongo.t2) = "{db.name}._default"

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
     db = db.get()
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
     vpath = build_vpath_sub(db, path, default, rpath, partial)
     db = db.get()
     ns = defaultns(db)
     rid = path_to_id(rpath)
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
       do updateerr(db, tags, ns, selector, update, rid, true)
       true
     remove() =
       unset = [{name="$unset" value={Document = [{name=field value={Int32 = 0}}]}}]
       do updateerr(db, 0, ns, selector, unset, rid, true)
       #<Ifstatic:DBGEN_DEBUG>
       do Log.notice("DbGen/Mongo", "(success) subpath '{path}' removed ")
       #<End>
       void
     {vpath with more=~{write remove}}


   /* ********************************************
    * {3 Builder of declared path}
    * *******************************************/
   @package build_vpath(db:DbMongo.t, path:list(string), default:'data, const:bool):DbMongo.private.val_path('data) =
     db = db.get()
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
     vpath = build_vpath(db, path, default, const) : DbMongo.private.val_path('data)
     db = db.get()
     ns = defaultns(db)
     selector = [{name = "_id"; value = {String = vpath.id}}]
     tags = Bitwise.lor(0, MongoCommon.UpsertBit)
     write(data:'data) =
       update = Bson.opa2doc({_id = vpath.id; value=data})
       do updateerr(db, tags, ns, selector, update, vpath.id, true)
       true
     remove() =
       if not(MongoDriver.delete(db.db, 0, ns, selector)) then
         Log.error("DbGen/Mongo", "(failure) An error occurs when removing subpath '{path}'")
       #<Ifstatic:DBGEN_DEBUG>
       else Log.notice("DbGen/Mongo", "(success) subpath '{path}' removed ")
       #<End>
     { vpath with more=~{write remove} }

   @package update_path(db:DbMongo.t, path:list(string), update) =
     db = db.get()
     ns = defaultns(db)
     id = path_to_id(path)
     selector = [{name = "_id"; value = {String = id}}]
     tags = Bitwise.lor(0, MongoCommon.UpsertBit)
     @catch(_ ->
       Log.error("DbGen/Mongo", "(failure) An error occurs while updating path '{path}' with {update}"),
       do updateerr(db, tags, ns, selector, update, id, true)

       #<Ifstatic:DBGEN_DEBUG>
       do Log.notice("DbGen/Mongo", "(success) path '{path}' updated with {update}")
       #<End>
       void
     )


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

    os =
      #<Ifstatic:IS_LINUX 1>
        some("linux")
      #<Else>#<Ifstatic:IS_MAC 1>
        some("osx")
      #<Else>
        none
      #<End>#<End>

    wget(from, to) =
      #<Ifstatic:IS_MAC 1>
        "curl {from} > {to}"
      #<Else>
        "wget {from} -O {to}"
      #<End>

    default_archive =
      arch = "x86_64" // TODO 32 BITS
      ver  = "2.0.2"
      Option.map(os -> "mongodb-{os}-{arch}-{ver}", os)

    default_url =
      Option.map(default_archive ->
        "http://fastdl.mongodb.org/{Option.get(os)}/{default_archive}.tgz",
        default_archive)

    default_remote = {
      seeds=[] : list((string, int))
      bufsize = 50*1024
      poolsize = 100
      log = false
      auth = [] : Mongo.auths
    }

    default_local() = "{%%BslFile.mlstate_dir%%(void)}/mongo"

    open_remote_aux(name, args, default_seed) =
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
          | {success=(_/*slaveok*/,m)} -> MongoDriver.check(m)
          | {~failure} -> {~failure}
          end
      match connect() with
      | {success = db} ->
        ~{db name cols=["default"]}
      | ~{failure} ->
        do Log.error("DbGen/Mongo", "Error while opening database \"{name}\"\n{failure}")
        System.exit(1)

    @private
    open_local_aux(name, args, seed) =
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
        which_mongod = %%BslSys.which%%("mongod")
        mngpath =
          match which_mongod with
          {none} ->
            do jlog("Looking for MongoDB in directory \'{path}\'")
            binpath = "{args.path}/{default_archive}/bin"
            tgzpath = "{args.path}/mongo.tgz"
            do (
              if %%BslFile.exists%%(binpath) then (
                if not(%%BslFile.is_directory%%(binpath)) then
                  error("File \'{path}\' exists but is not a directory")
              ) else (
                do jlog("MongoDB does not seem to be installed in '{path}'")
                do jlog("Please wait while Opa downloads MongoDB from '{default_url}'...")
                _ = System.exec(wget(default_url, tgzpath), "")
                do jlog("MongoDB was downloaded ({tgzpath})")
                tarcmd = "tar -xvzf {tgzpath} -C {path}"
                do jlog("Uncompressing of MongoDB archive... ({tarcmd})")
                _ = System.exec(tarcmd, "")
                void
              )
            )
            "{binpath}/mongod"
          {some=mongod} ->
            do jlog("MongoDB found at \'{mongod}\'")
            mongod
        /*Check for MongoDB server */
        pidpath = "/tmp/opaMongo.pid"
        datpath = "{path}/data"
        logpath = "{path}/mongo.log"
        mngargs = "--pidfilepath {pidpath} --noprealloc --bind_ip 0.0.0.0 --dbpath {datpath} --fork --logpath {logpath}"
        _ = %%BslFile.make_dir%%(datpath)
        do (
          rec aux(launch) =
            pid = %%BslFile.content_opt%%(pidpath)
            pid = Option.bind(x -> Parser.try_parse(Rule.natural, String.trim(string_of_binary(x))), pid)
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
        open_remote_aux(name, default_remote, seed)

   @private
   open_local(name, args, seed) : DbMongo.t = open_synchronisation(name, -> open_local_aux(name, args, seed) )

   @private
   open_remote(name, args, seed) : DbMongo.t = open_synchronisation(name, -> open_remote_aux(name, args, seed) )

   @private
   open_synchronisation(name, open_: -> DbMongo.t2 ) : DbMongo.t =
      #<Ifstatic:OPA_BACKEND_QMLJS>
      log(s) = Log.info(name^" DbGen/Mongo/SynchroStart", s)
      ref = Reference.create({psyncops=false wsyncops=[]:list(DbMongo.t2->void) status={wait=[]:list(continuation(DbMongo.t2))} }:{...})
      // psyncops : processing sync ops, wsyncops: waiting sync ops
      process_wait(db,wait) =
        do log("Process {List.length(wait)} operations on the db wait list, start")
        do List.iter(Continuation.return(_, db), wait)
        do log("Process {List.length(wait)} operations on the db wait list, finished")
        void
      psyncops(b) = @atomic(
        s = Reference.get(ref)
        Reference.set(ref, {s with psyncops=b})
      ) //if b && s.psyncops then log("psyncops incoherent")
      try_process_one_syncops(db) =
        ops = @atomic(
          s = Reference.get(ref)
          match s
          {psyncops={false} wsyncops=[ops|wsyncops] ...} ->
            do Reference.set(ref, {s with ~wsyncops})
            do psyncops(true)
            some( (ops,wsyncops) )
          _ -> none
        )
        (Option.iter(_,ops)){ (ops,wsyncops) ->
          id = Random.string(16)
          do log("Processing sync op {List.length(wsyncops)}, start {id}")
          do ops(db):void
          do psyncops(false)
          log("Processing sync op, finished {id}")
        }
      set_open(x) =
        opened = @atomic(
          s = Reference.get(ref)
          match s
          {psyncops={false} wsyncops=[] status={wait=_}} ->
            do Reference.set(ref, {s with status={db=x}})
            true
          _ -> false
        )
        if not(opened) then error("set_open")

      rec declare_open(x,i) =
       match Reference.get(ref)
       {status={db=_} ...} -> error("Multiple initialization of local database")
       {psyncops={true} ...} ->
         do if mod(i,30)==0 then log("Waiting sync operation (declare_open)")
         Scheduler.sleep(100, -> declare_open(x,i+1))
       {wsyncops=[] status={~wait} ...} ->
         do set_open(x)
         do log("Db is ready")
         do process_wait(x,List.rev(wait))
         void
       {wsyncops=_ ...} ->
         do try_process_one_syncops(x)
         declare_open(x,i+1)
       end

      do Scheduler.sleep(0, ->
        do log("Opening database")
        declare_open(open_():DbMongo.t2,0)
      )
       {{
        // TODO factor get and sync_operation maintaining get efficiency
        get():DbMongo.t2 = @callcc((k:continuation(DbMongo.t2)) ->
          s = @atomic(Reference.get(ref))
          match s
          {status={~wait} ...} ->
            do @atomic(Reference.set(ref, {s with status={wait =[k|wait]}}))
            log("Operation on db wait list")
          {psyncops={true} ...} ->
            do log("Waiting sync operation (get)")
            Scheduler.sleep(1000, -> Continuation.return(k, get()) )
          {wsyncops=[] status={~db} ...} ->
            Continuation.return(k,db)
          {wsyncops=_ status={~db} ...} ->
            do try_process_one_syncops(db)
            Continuation.return(k,get())
          )

        sync_operation(f) =
          s = @atomic(Reference.get(ref))
          match s
          {status={wait=_} ~wsyncops ...}
          {psyncops={true} ~wsyncops ...} ->
            do @atomic(Reference.set(ref, {s with wsyncops=[f|wsyncops]}))
            log("Sync op on db syncops list")
          {wsyncops=[] status={~db} ...} ->
            do psyncops(true) ; do f(db) ; do psyncops(false)
            log("Sync op on fly")
          {wsyncops=_ status={~db} ...} ->
            do try_process_one_syncops(db)
            sync_operation(f)
      }}
      #<Else>
      _ = name
      dbp = open_()
      {{
        get() = dbp
        sync_operation(f : (DbMongo.t2 -> void)) = f(get())
      }}
      #<End>

    @private seeds_parser(name, f) =
      auth_parser = parser user=((![:] .)+)[:] password=((![@] .)+) [@] ->
       {user=Text.to_string(user); password=Text.to_string(password); dbname=name}
      seed_parser = parser
       | auth=auth_parser? host=((![:] .)*)[:] port={Rule.natural} ->
         (auth, (Text.to_string(host), port))
      parser seeds={Rule.parse_list(seed_parser, Rule.of_string(","))} -> f(seeds)

    open(name:string, host, port) =
      seed = (host ? "localhost", port ? 27017)
      (suffix, msg) = match name with
      | "_no_name" -> ("", "")
      | _ -> (":{name}", "\"{name}\"")
      args = CommandLine.filter({
        title = "Options for Mongo database {msg}"
        init = none
        anonymous = []
        parsers = [
          {CommandLine.default_parser with
            names = ["--db-remote{suffix}"]
            description = "Use a remote mongo database(s), (default: {seed.f1}:{seed.f2})"
            param_doc = "[user:password@]host[:<port>][,[user:password@]host[:<port>]]*"
            on_param(acc) =
              seeds_parser(name, (seeds ->
                acc = match acc with
                  | {some = {remote = acc}} -> acc
                  | _ -> default_remote
                (auths,seeds) = List.unzip(seeds)
                auths = List.filter_map((a -> a),auths)
                {no_params = {some = {remote = {acc with seeds = List.append(acc.seeds, seeds); auth = List.append(acc.auth, auths)}}}}
              ))
          },
          {CommandLine.default_parser with
            names = ["--db-local{suffix}"]
            description = "Use a local mongo database, (default: {default_local()})"
            param_doc = "path"
            on_encounter(_acc) =
              {opt_params = {some = {local = {path = default_local()}}}}
            on_param(_acc) =
              parser path=(.*) -> {no_params = {some = {local = {path = Text.to_string(path)}}}}
          },
        ]
      })
      match args with
      | {some = ~{remote}} -> open_remote(name, remote, seed)
      | {some = ~{local}} -> open_local(name, local, seed)
      | {none} ->
        match Db.default_cmdline with
        | {none} -> open_local(name, {path = default_local()}, seed)
        | {some = {local = {none}}} -> open_local(name, {path = default_local()}, seed)
        | {some = {local = {some = path}}} -> open_local(name, ~{path}, seed)
        | {some = {remote = {some = remote}}} ->
          Parser.try_parse(
            seeds_parser(name, (seeds ->
                (auth,seeds) = List.unzip(seeds)
                auth = List.filter_map(x->x, auth)
                open_remote(name, {default_remote with ~seeds ~auth}, seed)))
            , remote) ?
            do Log.error("DbGen/Mongo", "Invalid --db-remote params for the database '{name}' (Mongo)")
          System.exit(1)
        | {some = {remote = {none}}} -> open_remote(name, default_remote, seed)

  }}

  open = Init.open

  @package drop(db:DbMongo.t) =
    db = db.get()
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
type DbMongoSet.engine('a) = {
    reply: Mongo.reply;
    default : 'a;
    limit : int;
    next : int, Mongo.reply -> option(DbMongoSet.engine('a))
}

@opacapi
type DbMongoSet.t('a) = dbset('a, DbMongoSet.engine('a))


/**
 * {1 Interface}
 */

DbSet = {{

  @package path_to_id(path) = List.to_string_using("", "", ".", path)

  @package index(db:DbMongo.t2, path:list(string), idx) =
    id = DbSet.path_to_id(path)
    key = List.map((name -> ~{name value={Int32=1}}), idx)
    opt = 0
    opt = Bitwise.lor(opt, MongoCommon.UniqueBit)
    match MongoDriver.create_index(db.db, "{db.name}.{id}", key, opt) with
    | {true} ->
      do Log.notice("DbGen/Mongo", "(success) Index {idx} at {path} has been created")
      void
    | {false} ->
      do Log.error("DbGen/Mongo", "(failure) Error while creating index {idx} at {path}")
      error("Error when creating index")

  @package indexes(db:DbMongo.t, path:list(string), idxs) =
    db.sync_operation( db ->
      List.iter(idx -> index(db, path, idx), idxs)
    )

  @package genbuild(db, ns, id, default:'a, reply, limit):DbMongoSet.engine('a) =
    match reply with
    | {none} ->
      do Log.error("DbGen/Mongo", "(failure) Read from {id} set doesn't returns anything")
      error("DbSet build error")
    | {some=reply} ->
      rec next(consummed, reply) =
//        # <Ifstatic:OPA_BACKEND_QMLJS>
//        _ = consummed
//        do MongoDriver.cclose(reply)
//        {none}
//        # <Else>
        cursor = MongoCommon.reply_cursorID(reply)
        if MongoCommon.is_null_cursorID(cursor) then
          none
        else if limit != 0 && consummed >= limit then none
        else
          limit = if limit == 0 then 0 else consummed - limit
          match MongoDriver.get_more(db.db, ns, limit, cursor) with
          | {none} ->
            do Log.error("DbGen/Mongo",
               "(failure) Get more data from {id} set doesn't returns anything")
            {none}
          | {some = reply} ->
            next(c, r) = next(consummed+c, r)
            {some = ~{reply default next limit}}
          end
//        # <End>
       ~{reply default next limit}

  @package build(db:DbMongo.t, path:list(string), selector, default:'a, skip, limit, filter):DbMongoSet.engine('a) =
    #<Ifstatic:DBGEN_DEBUG>
    do Log.notice("DbGen/Mongo", "DbSet.build : Selector {selector}")
    do Log.notice("DbGen/Mongo", "DbSet.build : Filter {filter}")
    #<End>
    db = db.get()
    id = DbSet.path_to_id(path)
    ns = "{db.name}.{id}"
    filter =
        match filter with
        // It's a hack for empty filter, if the filter is really empty mongo
        // returns all fields instead of any
        | {some=[]} -> {some=[{name="`" value={Int32=1}}]}
        | _ -> filter
    genbuild(db, ns, id, default,
             MongoDriver.query(db.db, 0, ns, skip, limit, selector, filter), limit)

  // @package build_and_modify(db:DbMongo.t, path:list(string), selector, default:'a,
  //                           skip, limit, filter, update):DbMongoSet.engine('a) =
  //   #<Ifstatic:DBGEN_DEBUG>
  //   do Log.notice("DbGen/Mongo", "DbSet.build_and_modify : Selector {selector}")
  //   do Log.notice("DbGen/Mongo", "DbSet.build_and_modify : Filter {filter}")
  //   do Log.notice("DbGen/Mongo", "DbSet.build_and_modify : Update {update}")
  //   #<End>
  //   id = DbSet.path_to_id(path)
  //   ns = "{db.name}.{id}"
  //   genbuild(db, ns, id, default,
  //            MongoCommands.findAndUpdate(db.db, db.name, ns, selector, update, none, noneskip, limit, selector, filter), limit)

  @package update(db:DbMongo.t, path:list(string), selector, update, upsert) =
    db = db.get()
    id = DbSet.path_to_id(path)
    tag = if upsert then Bitwise.lor(0, MongoCommon.UpsertBit) else 0
    tag = Bitwise.lor(tag, MongoCommon.MultiUpdateBit)
    #<Ifstatic:DBGEN_DEBUG>
    do Log.notice("DbGen/Mongo", "DbSet.update {db.name}.{id} : Selector {selector}")
    do Log.notice("DbGen/Mongo", "DbSet.update {db.name}.{id} : Update({upsert}) {update}")
    #<End>
    DbMongo.updateerr(db, tag, "{db.name}.{id}", selector, update, id, upsert)


  @private fold_doc(init, dbset:DbMongoSet.engine, f) =
    rec aux(size, dbset, i, acc) =
      if i == size then
        match dbset.next(size, dbset.reply):option(DbMongoSet.engine('a)) with
        | {some = dbset} -> aux(MongoCommon.reply_numberReturned(dbset.reply), dbset, 0, acc)
        | _ -> acc
      else
        match MongoCommon.reply_document(dbset.reply, i) with
        | {none} ->
          do Log.error("DbGen/Mongo", "Unexpected error : can't retreive document {i}")
          aux(size, dbset, i+1, acc)
        | {some=doc} -> aux(size, dbset, i+1, f(acc, doc))
    aux(MongoCommon.reply_numberReturned(dbset.reply), dbset, 0, init)

  @package iterator(dbset:DbMongoSet.engine('a)):iter('a) =
    rec aux(size, dbset, i) =
      if i == size then
        match dbset.next(size, dbset.reply):option(DbMongoSet.engine('a)) with
        | {some = dbset} -> aux(MongoCommon.reply_numberReturned(dbset.reply), dbset, 0)
        | _ -> {none}
      else
        match MongoCommon.reply_document(dbset.reply, i) with
        | {none} ->
          do Log.error("DbGen/Mongo", "Unexpected error : can't retreive document {i}")
          aux(size, dbset, i+1)
        | {some=doc} ->
            #<Ifstatic:DBGEN_DEBUG>
            do Log.notice("DbGen/Mongo", "Unserialize {doc} with {@typeval('a)} {dbset.default}")
            #<End>
            match Bson.doc2opa_default(doc, dbset.default) with
            | {none} ->
               // Note: we should really test for error before unserialize but it would have an adverse effect on performance.
               if Bson.is_error(doc)
               then
                 do Log.error("DbGen/Mongo",
                      "(failure) Unexpected error : MongoDB returned error document {Bson.string_of_doc_error(doc)}")
                 aux(size, dbset, i+1)
               else
                 do Log.error("DbGen/Mongo",
                      "(failure) dbset unserialize {doc} from {OpaType.to_pretty(@typeval('a))} with default value : {dbset.default}")
                 aux(size, dbset, i+1)

            | {some=opa} ->
              #<Ifstatic:DBGEN_DEBUG>
              do Log.notice("DbGen/Mongo", "Unserialize success {opa}")
              #<End>
              {some = (opa, {next=->aux(size, dbset, i+1)})}
            end
        end
      Iter.cache({next = ->
         nb = MongoCommon.reply_numberReturned(dbset.reply)
         nb = if dbset.limit != 0 then min(nb, dbset.limit) else nb
         if nb == 0 then none
         else aux(nb, dbset, 0)})

  @private fold(init, dbset, f) =
    Iter.fold(f, iterator(dbset), init)

  @package to_list(dbset:DbMongoSet.engine('a)) = fold([], dbset, (a, acc -> a +> acc))

  @package to_map(dbset:DbMongoSet.engine('a), f : 'v0 -> 'v1):map('key, 'v1) =
    #<Ifstatic:DBGEN_DEBUG>
      do Log.notice("DbGen/Mongo", "DbSet.to_map key:{@typeval('key)} v0:{@typeval('v0)} v1:{@typeval('v1)}")
    #<End>
    fold_doc(Map.empty, dbset, (map:map('key, 'v1), doc:Bson.document ->
        #<Ifstatic:DBGEN_DEBUG>
        do Log.notice("DbGen/Mongo", "Map fold doc {doc}")
        #<End>
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
            match Bson.doc2opa_default(vdoc, @unsafe_cast(dbset.default)):option('v0) with
            | {none} ->
              do Log.error("DbGen/Mongo",
                   "(failure) map unserialize value {vdoc} from {@typeval('value)}")
              map
            | {some=val} -> Map.add(key, f(val), map)
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

  @package build_vpath(db:DbMongo.t, path:list(string), selector, default:'b, skip, limit, filter,
              read_map:DbMongoSet.engine('a) -> option('b)):DbMongo.private.val_path('b) =
    {
      id = DbSet.path_to_id(path)
      read() = read_map(build(db, path, selector, @unsafe_cast(default), skip, limit, filter)):option('b)
      default = default
      more = void
    }

  @package build_rpath(db:DbMongo.t, path:list(string), selector, default:'b, skip, limit, filter,
                       read_map:DbMongoSet.engine('a) -> option('b), write_map:'b -> Bson.document,
                       embed:option(string)):DbMongo.private.ref_path('b) =
    id = DbSet.path_to_id(path)
    vpath = build_vpath(db, path, selector, default, skip, limit, filter, read_map)
    write(data) =
      do update(db, path, selector,
           [{name="$set"; value={Document = write_map(data)}}], true
         )
      true
    remove =
      match embed with
      | {none} ->
        ->
          db = db.get()
          if not(MongoDriver.delete(db.db, 0, "{db.name}.{id}", selector)) then
             Log.error("DbGen/Mongo", "(failure) An error occurs when removing inside set '{path}'")
          #<Ifstatic:DBGEN_DEBUG>
          else Log.notice("DbGen/Mongo", "(success) removing inside set '{path}' removed ")
          #<End>
      | {some=embed} ->
        ->
          update(db, path, selector,
            [{name="$unset"; value={Document = [{name=embed; value={Int32 = 1}}]}}]
            , false)
    {vpath with more=~{write remove}}

  @package build_rpath_collection(
             db:DbMongo.t,
             path:list(string),
             selector,
             default:'b,
             skip,
             limit,
             filter,
             read_map:DbMongoSet.engine('a) -> option('b),
             write_map:'b -> list(Bson.document),
             embed:option(string)):DbMongo.private.ref_path('b) =
    id = DbSet.path_to_id(path)
    vpath = build_vpath(db, path, selector, default, skip, limit, filter, read_map)
    db0 = db.get()
    remove =
      match embed with
      | {none} ->
        ->
          if not(MongoDriver.delete(db0.db, 0, "{db0.name}.{id}", selector)) then
            Log.error("DbGen/Mongo", "(failure) An error occurs when removing inside set '{path}'")
          #<Ifstatic:DBGEN_DEBUG>
          else Log.notice("DbGen/Mongo", "(success) removing inside set '{path}' removed ")
          #<End>
      | {some=embed} ->
        ->
          update(db, path, selector,
            [{name="$unset"; value={Document = [{name=embed; value={Int32 = 1}}]}}]
            , false)
    write(datas) =
      do remove()
      #<Ifstatic:DBGEN_DEBUG>
      do Log.notice("DbGen/Mongo", "Write in collection {db0.name}.{id}")
      #<End>
      MongoDriver.insert_batch(db0.db, 0, "{db0.name}.{id}", write_map(datas))
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
@opacapi DbSet_iterator_map = Iter.map
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
@opacapi DbMongo_expr_to_field(x) = Bson.encode_field(OpaSerialize.serialize(x))
