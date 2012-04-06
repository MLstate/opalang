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

package stdlib.database.dropbox

import stdlib.core.{date,map,parser}
import stdlib.system
import stdlib.database.common
import stdlib.apis.dropbox
import stdlib.core.rpc.core
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

@opacapi @abstract type DbDropbox.t = {
  root : string;
  appkey : string;
  appsecret : string;
  cols : list(string)
}

@opacapi @abstract type DbDropbox.engine = void

type DbDropbox.val_path('a) = Db.val_path('a, DbDropbox.engine)

type DbDropbox.ref_path('a) = Db.ref_path('a, DbDropbox.engine)

type DbDropbox.value = string
type DbDropbox.query = void // not yet implemented

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

   error(s) = Log.error("DbGen/Dropbox", s)
   notice(s) = Log.notice("DbGen/Dropbox", s)
   build_path(path) =
    path = DbCommon.path_to_id(path)
    "/{path}.json"

   build_conf(db) = {app_key = db.appkey app_secret = db.appsecret}

  Context = {{

    dropbox_context = UserContext.make({no_credentials})
    get() = UserContext.execute(s -> s, dropbox_context)
    set(r) = UserContext.change(_ -> r, dropbox_context)
    set_request(token, secret) = set({request_secret = secret; request_token = token})
    set_authenticated(token, secret) = set({authenticated = { token = token; secret = secret}})

  }}

   @package gen_read(db:DbDropbox.t, path):option('data) =
    match Context.get() with
    | {authenticated = creds} -> (
      match Dropbox(build_conf(db)).Files(db.root, build_path(path)).get(none, creds) with
      | { success = file } -> (
        value : DbDropbox.value = file.content
        match Json.deserialize(value) with
        | { some = json } -> (
          match OpaSerialize.Json.unserialize(json) with
          | { some = r } -> some(r)
          | { none } ->
                do error("Invalid json: '{value}'") none )
        | { none } ->
                do error("Invalid value: '{value}'") none )
      | { failure = failure } -> do error("Impossible to read {path}: {failure}") none )
    | {no_credentials} -> do error("Impossible to read {path}. The user is not authenticated.") none
    | _ -> do error("Impossible to read {path}. The user is not authenticated, but in request mode.") none

    @package gen_write(db:DbDropbox.t, path, data:'data) =
      match Context.get() with
      | {authenticated = creds} -> (
          json_value : DbDropbox.value = Json.serialize(OpaSerialize.Json.serialize(data))
          match Dropbox(build_conf(db)).Files(db.root, build_path(path)).put("application/json", json_value, true, none, creds) with
          | { success = _ } -> true
          | { failure = failure } -> do error("Impossible to write to {path}: {failure}") false )
      | {no_credentials} -> do error("Impossible to write to {path}. The user is not authenticated.") false
      | _ -> do error("Impossible to write to {path}. The user is not authenticated, but in request mode.") false


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
     remove() = error("remove not yet implemented")
     { vpath with more=~{write remove} }

   @package update_path(db:DbDropbox.t, path:list(string), data) =
     gen_write(db, path, data)

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

    error(msg) = do Log.error("DbGen/Dropbox", msg) System.exit(1)

    jlog(msg) = Log.notice("DbGen/Dropbox", msg)

    // todo
    open(appkey, appsecret, root) = ~{appkey appsecret root; cols=[]}

  }}

  open = Init.open

  @package drop(db:DbDropbox.t) =
    List.iter(
      _col -> error("Drop not yep implemented")
    , db.cols)

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


