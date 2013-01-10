/*
    Copyright © 2011, 2012, 2013 MLstate

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

import stdlib.apis.postgres
package stdlib.database.postgres

@opacapi
abstract type DbPostgres.t = Cps.future(Postgres.db)

@opacapi
abstract type DbPostgres.engine = void

@opacapi
abstract type DbPostgresSet.engine = void

@opacapi
abstract type DbPostgresSet.t('a) = dbset('a, DbPostgresSet.engine)

@opacapi type DbPostgres.private.val_path('a) = void

@opacapi type DbPostgres.private.ref_path('a) = void

type DbPostgres.val_path('a) = Db.val_path('a, DbPostgres.engine)

type DbPostgres.ref_path('a) = Db.ref_path('a, DbPostgres.engine)

/**
 * PostgreSQL database backend
 *
 * @category database
 * @author Quentin Bourgerie
 * @stability experimental
 * @destination internal
 */
module DbPostgres{

  private
  module Log{

    private @expand
    function gen0(f, name, msg){
      f("DbPostgres({name})", msg)
    }

    private @expand
    function gen(f, db, msg){
      gen0(f, Postgres.get_name(db), msg)
    }

    @expand
    function info(db, msg){
      gen(@toplevel.Log.info, db, msg)
    }

    @expand
    function debug(db, msg){
      gen(@toplevel.Log.debug, db, msg)
    }

    @expand
    function error(db, msg){
      gen(@toplevel.Log.error, db, msg)
    }

    @expand
    function error0(name, msg){
      gen0(@toplevel.Log.error, name, msg)
    }

  }

  /**
   * Opening a database and prepare statements.
   * @param name Name of the database to open
   * @param statements A list of statements to prepare
   * @return A initialized database
   */
  function DbPostgres.t open(name, statements){
    @spawn(
      /* 1 - Opening the postgres database */
      match(Postgres.make(name, none, name)){
      case ~{failure} :
        Log.error0(name, "Can't open Postgres database {failure}")
        @fail("DbPostgres.open")
      case {success : db} :
        Log.info(db, "opened")
        /* 2 - Prepare statements */
        c = Postgres.connect(db)
        List.iter(
          function(~{id, query, types}){
            c = Postgres.parse(c, id, query, types)
            match(Postgres.get_error(c)){
            case {none} : void
            case {some: e} :
              Log.error(db, "An error occurs while prepare statements
error: {e}
id: {id}
query: {query}
")
              @fail
            }
          }
          , statements)
        db
      }
    )
  }

}

@opacapi DbPostgres_open = DbPostgres.open


