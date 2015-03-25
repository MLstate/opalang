/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa.  If not, see <http://www.gnu.org/licenses/>.
*/


package stdlib.database.dropbox

import stdlib.core.{date,map,parser}
import stdlib.system
import stdlib.database.common
import stdlib.apis.dropbox
import stdlib.core.rpc.core
import stdlib.io.file

/**
 * Dropbox database backend
 * Looking for Dropbox API? Find it in stdlib.apis.dropbox
 *
 * @category database
 * @author Cedric Soulas
 * @destination experimental
 */


/**
 * {1 Types defined in this module}
 */

type DbDropbox.User.status =
   {no_credentials}
 / {request_secret : string; request_token : string}
 / {authenticated : Dropbox.creds}

@opacapi @abstract type DbDropbox.t = {
  root : string;
  appkey : string;
  appsecret : string;
  context : UserContext.t(DbDropbox.User.status)
}

@opacapi @abstract type DbDropbox.engine = void

@opacapi @abstract type DbDropboxSet.engine = void
@opacapi @abstract type DbDropboxSet.t = void

type DbDropbox.val_path('a) = Db.val_path('a, DbDropbox.engine)

type DbDropbox.ref_path('a) = Db.ref_path('a, DbDropbox.engine)

type DbDropbox.value = string
type DbDropbox.query = void // not yet implemented
type DbDropbox.update = string // not yet implemented

// Private type
type DbDropbox.private.path_t('kind, 'data) = {
  id : string;
  read : -> option('data);
  default : 'data;
  more : 'kind;
}

@opacapi type DbDropbox.private.val_path('a) = DbDropbox.private.path_t(void, 'a)

@opacapi type DbDropbox.private.ref_path('a) = DbDropbox.private.path_t({
  write : 'a -> bool
  remove : -> void
}, 'a)

type Dropbox.context =
    {no_credentials}
  / {request_secret : string; request_token : string}
  / {authenticated : Dropbox.creds}

/**
 * {1 Interface}
 */

DbDropbox = {{

   /**
    * {2 Utils}
    */

   error(s) = Log.error("DbGen/Dropbox:", s)
   notice(s) = Log.notice("DbGen/Dropbox:", s)
   uri_to_string(path) = Uri.to_string(~{path fragment=none query=[] is_directory=false is_from_root=false})
   build_path(path) =
    path = uri_to_string(path)
    "/{path}.json"
   build_folder_path(path) =
    path = uri_to_string(path)
    "/{path}"


   @private _User(db : DbDropbox.t) = {{

    get_status() = UserContext.execute(s -> s, db.context)
    is_authenticated() = match get_status() with { authenticated = _ } -> true | _ -> false
    set_status(r) = UserContext.change(_ -> r, db.context)
    set_request(token, secret) = set_status({request_secret = secret; request_token = token})
    set_authenticated(token, secret) = set_status({authenticated = { token = token; secret = secret}})

   }}

   @private _Auth(db : DbDropbox.t) = {{

    conf = {app_key = db.appkey app_secret = db.appsecret}
    Auth = Dropbox(conf).Auth(_User(db))
    get_login_url = Auth.get_login_url
    get_access = Auth.get_access
    are_valid_creds = Auth.are_valid_creds

  }}

  /**
    Wrapper around Dropbox Auth module (stdlib.apis.dropbox)
     to manipulate the User crendentials,
     based on the context stored in the database in parameter.

    Example:
    [
      database db_dropbox @dropbox {
        stringmap(int) /test
      }

      DbU = DbDropbox.User(db_dropbox)
      url = DbU.get_login_url(redirect)
    ]
  */
  User(db : DbDropbox.t) = {{

    get_status = _User(db).get_status
    is_authenticated = _User(db).is_authenticated
    set_status = _User(db).set_status
    set_request = _User(db).set_request
    set_authenticated = _User(db).set_authenticated
    get_login_url = _Auth(db).get_login_url
    get_access = _Auth(db).get_access
    are_valid_creds = _Auth(db).are_valid_creds

  }}

   D(db) = Dropbox({app_key = db.appkey app_secret = db.appsecret})

   /* ********************************************
    * {Operations over Dropbox API}
    * *******************************************/

    get_path_list(db, creds, path) =
      db_files = D(db).Files(db.root, path).metadata(D(db).default_metadata_options, creds)
      f(e : Dropbox.element) =
        (match (e) with
         | {file; ~metadata; ...} -> { some = metadata.path }
         | {folder; ...} -> do error("There is an invalid folder inside {path}"); none)
      match (db_files) with
      | { success = {~contents; ...} } -> List.filter_map(f, contents ? [])
      | { success = {file=_; ...} } -> do error("{path} is a file, not a folder"); []
      | { failure = { not_found }} -> do error("Unbound path {path}"); []
      | { failure = failure } -> do error("Unexepected error: {failure}"); []

   @package @server read_map(db:DbDropbox.t, creds, path:string, kty, dty):stringmap('data) =
     /**
      - all elements are requested in parallel (would be too slow in sequence!)
      - responses are received in a random order (random time response)
      - an element is safely added to the map via a cell
      - a counter is used to know when all elements are retrieved
      - no callback argument: the map is return thanks to @callcc
     */
     Map = Map_make(Order.make_unsafe(OpaValue.compare_with_ty(_, _, kty)))
     l = get_path_list(db, creds, path)
     if l == [] then Map.empty else
     cont(map_add, k) =
       size = List.length(l)
       on_message((counter, map), r) =
         map = match r with
               | { some = (path, value)} -> (
                    basename = File.basename(path, none)
                    key = String.substring_opt(0, String.length(basename) - 5, basename) ? ""
                    map_add(key, value, map)
                    )
               | { none } -> map
         instruction = {set = (counter+1, map)}
         if counter == size then {~instruction return = Continuation.return(k, map)}
         else {~instruction return = void}
       cell = Cell.make((1, Map.empty), on_message)
       add(r) = (Cell.call(cell, r))
       iterator(path) =
         f() = match read_record(db, creds, path, dty) with
              | {none} -> add(none)
              | {some=r} -> add({some = (path, r)})
         Scheduler.push(f)
       List.iter(iterator, l)
     invalid_key(map, key) = do error("Invalid map key '{key}' regarding type {kty}"); map
     add_parsed(key, value, map, parse) = Option.switch(Map.add(_, value, map), invalid_key(map, key), @unsafe_cast(parse(key)))
     match kty with
     | {TyConst={TyString={}}} -> f(key, value, map) = Map.add(key, value, map)                  @callcc(cont(@unsafe_cast(f), _))
     | {TyConst={TyInt={}}}    -> f(key, value, map) = add_parsed(key, value, map, Parser.int)   @callcc(cont(@unsafe_cast(f), _))
     | {TyConst={TyFloat={}}}  -> f(key, value, map) = add_parsed(key, value, map, Parser.float) @callcc(cont(@unsafe_cast(f) , _))
     | _ -> do error("Unsupported type {kty} for map keys"); Map.empty


   @package @server read_record(db:DbDropbox.t, creds, path:string, ty):option('data) =
    match D(db).Files(db.root, path).get(none, creds) with
    | { success = file } -> (
      value : DbDropbox.value = string_of_binary(file.content)
      match Json.deserialize(value) with
      | { some = json } -> (
        s = OpaSerialize.Json.unserialize_with_ty(json, ty)
        match s with
        | { some = r } -> some(r)
        | { none } -> do error("Invalid json: '{value}'") none )
      | { none } -> do error("Invalid value: '{value}'") none )
    | { failure = { not_found }} -> none
    | { failure = failure } -> do error("Impossible to read {path}: {failure}") none

   @package @server gen_read(db:DbDropbox.t, path):option('data) =
    match User(db).get_status() with
    | {authenticated = creds} -> (
        ty = @typeval('data)
        read_map_opt(kty, dty) = { some = @unsafe_cast(read_map(db, creds, build_folder_path(path), kty, dty)) }
        match ty
        | {TyName_args=[kty, dty, _]; TyName_ident="ordered_map"}
        | {TyName_args=[kty, dty]; TyName_ident="map"}      -> read_map_opt(kty, dty)
        | {TyName_args = [dty]; TyName_ident = "stringmap"} -> read_map_opt({TyConst={TyString={}}}, dty)
        | {TyName_args = [dty]; TyName_ident = "intmap"}    -> read_map_opt({TyConst={TyInt={}}}, dty)
        | _                                                 -> read_record(db, creds, build_path(path), ty)
      )
    | {no_credentials} -> do error("Impossible to read {path}. The user is not authenticated") none
    | _ -> do error("Impossible to read {path}. The user is not authenticated, but in request mode") none

    @package @server gen_write(db:DbDropbox.t, path, data:string) =
      match User(db).get_status() with
      | {authenticated = creds} -> (
          json_value : binary = binary_of_string(data : DbDropbox.value)
          match D(db).Files(db.root, build_path(path)).put("application/json", json_value, true, none, creds) with
          | { success = _ } -> true
          | { failure = failure } -> do error("Impossible to write to {path}: {failure}") false )
      | {no_credentials} -> do error("Impossible to write to {path}. The user is not authenticated") false
      | _ -> do error("Impossible to write to {path}. The user is not authenticated, but in request mode") false

    @package @server gen_remove(db:DbDropbox.t, path) =
      match User(db).get_status() with
      | {authenticated = creds} -> (
          match D(db).FileOps.delete(db.root, build_path(path), creds) with
          | { success = _ } -> void
          | { failure = failure } -> error("Impossible to remove {path}: {failure}") )
      | {no_credentials} -> error("Impossible to remove {path}. The user is not authenticated")
      | _ -> error("Impossible to remove {path}. The user is not authenticated, but in request mode")

   /* ********************************************
    * {3 Builder of composed path}
    * *******************************************/
   @package build_vpath_compose(_:DbDropbox.t, path:list(string), default:'data,
                       elements:list((string, DbDropbox.private.path_t(black, 'data)))):DbDropbox.private.val_path('data) =
     id = DbCommon.path_to_id(path)
     read() = (
       #<Ifstatic:DBGEN_DEBUG>
       do notice("Db.build_vpath_compose : start reading")
       #<End>
        DbCommon.Compose.read(elements)
     )
     {~id ~default ~read more}

   @package build_rpath_compose(db:DbDropbox.t, path:list(string), default:'data,
                       elements:list((string, DbDropbox.private.ref_path(black)))):DbDropbox.private.ref_path('data) =
     vpath = build_vpath_compose(db, path, default, @unsafe_cast(elements))
     write(data) = DbCommon.Compose.write(elements, data)
     remove() = DbCommon.Compose.remove(elements)
     { vpath with more=~{write remove} }

   /* ********************************************
    * {3 Builder of sub path}
    * *******************************************/
   @package build_vpath_sub(_db:DbDropbox.t, path:list(string), _default:'data, _rpath:list(string), _partial:list(string))
   : DbDropbox.private.val_path('data) =
      @fail("subpath not supported: {path}")

   @package build_rpath_sub(_db:DbDropbox.t, path:list(string), _default:'data, _rpath:list(string), _partial:list(string)) : DbDropbox.private.ref_path('data) =
      @fail("subpath not supported: {path}")


   /* ********************************************
    * {3 Builder of declared path}
    * *******************************************/
   @package build_vpath(db:DbDropbox.t, path:list(string), default:'data):DbDropbox.private.val_path('data) =
     id = DbCommon.path_to_id(path)
     read() = gen_read(db, path)
     ~{id read default more=void}

   @package build_rpath(db, path:list(string), default:'data):DbDropbox.private.ref_path('data) =
     vpath = build_vpath(db, path, default) : DbDropbox.private.val_path('data)
     write(data:'data) = gen_write(db, path, OpaSerialize.serialize(data))
     remove() = gen_remove(db, path)
     { vpath with more=~{write remove} }

    @package update_path(db:DbDropbox.t, path:list(string), data:DbDropbox.update):void =
     ignore(gen_write(db, path, data))

  /**
   * Writes data at the given path in the database.
   * [do write(@/path,42)] is equivalent to [do /path <- 42]
   *
   * @example [write(@/path,42)] initializes or updates the data at path [/path]
   */
  @package write(path:DbDropbox.private.ref_path('data), data:'data) =
    if not((path.more).write(data)) then
      error("Write error on path {path.id}")

  @package write_option(path:DbDropbox.private.ref_path('data), data) =
    (path.more).write(data)

  @package `<-`(d,a) = write(d,a)

  /**
   * Reads the data currently held at a reference path.
   *
   * @example [read(@/path)] is equivalent to [/path]
   */
  @package read(path:DbDropbox.private.path_t('any, 'data)):'data = path.read() ? path.default

  /**
   * Reads the data currently held at a reference path.
   *
   * @example [read(@/path)] is equivalent to [/path]
   */
  @package option(path:DbDropbox.private.path_t('any, 'data)):option('data) = path.read()

  /**
   * Turns a reference-path into a value-path, in fact taking a snapshot.
   *
   * @example [get_val(@/path)] is equivalent to [!/path]
   */
  @package get_val(rpath:DbDropbox.private.ref_path('data)):DbDropbox.private.val_path('data) =
    {rpath with more = void}

  /**
   * Removes the data held at a path in the database. It won't be visible in the
   * current revision anymore, but will still be present in the history.
   */
  @package remove(rpath:DbDropbox.private.ref_path('data)):void =
    rpath.more.remove()

  @private Init = {{

    error(msg) = do Log.error("DbGen/Dropbox:", msg) System.exit(1)

    open(name) =
      (suffix, msg) = match name with
      | "_no_name" -> ("", "")
      | _ -> (":{name}", "\"{name}\"")
      args = CommandLine.filter({
      title = "Options for Dropbox database {msg}"
      init = none
      anonymous = []
      parsers = [
        {CommandLine.default_parser with
          names = ["--db-remote{suffix}"]
          description = "Use a user dropbox account as a database. Specify your dropbox apps parameters."
          param_doc = "appkey:appsecret"
          on_param(_) = parser appkey=((![:] .)+)[:] appsecret=(.+)
                        -> {no_params = { some = {appkey=Text.to_string(appkey)
                                                  appsecret=Text.to_string(appsecret)
                                                  root="sandbox"
                                                  context=UserContext.make({no_credentials})
                            }}}
        }]
      })
      match args with
      | {some = conf} -> conf
      | {none} -> error("You have to specify your dropbox app parameters: --db-remote{suffix} appkey:appsecret")
  }}

  open = Init.open

  @package drop(_db:DbDropbox.t) = error("Drop not yep implemented")

}}

@opacapi DbDropbox_path_to_path(dropboxpath:DbDropbox.private.path_t('kind, 'a)) = Db.build({
  id = dropboxpath.id;
  read() = dropboxpath.read() ? dropboxpath.default
  exists() = Option.is_some(dropboxpath.read())
  more = dropboxpath.more
  engine = void;
})

@opacapi DbDropbox_open = DbDropbox.open

@opacapi DbDropbox_update_path = DbDropbox.update_path
@opacapi DbDropbox_build_vpath = DbDropbox.build_vpath
@opacapi DbDropbox_build_rpath = DbDropbox.build_rpath

@opacapi DbDropbox_build_rpath_compose = DbDropbox.build_rpath_compose
@opacapi DbDropbox_build_vpath_compose = DbDropbox.build_vpath_compose

@opacapi DbDropbox_build_vpath_sub = DbDropbox.build_vpath_sub
@opacapi DbDropbox_build_rpath_sub = DbDropbox.build_rpath_sub
@opacapi DbDropbox_read = DbDropbox.read
@opacapi DbDropbox_write = DbDropbox.write
@opacapi DbDropbox_option = DbDropbox.option
@opacapi DbDropbox_expr_to_field = magicToString

