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
abstract type DbPostgres.t = Cps.future(Postgres.connection)

@opacapi
abstract type DbPostgres.engine = void

@opacapi
abstract type DbPostgresSet.engine = void

@opacapi
type DbPostgresSet.t('a) = dbset('a, DbPostgresSet.engine)

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
    function warning(db, msg){
      gen(@toplevel.Log.warning, db, msg)
    }

    @expand
    function error0(name, msg){
      gen0(@toplevel.Log.error, name, msg)
    }

  }

  /**
   * Opening a database and prepare statements.
   * @param name Name of the database to open
   * @param tables A list of queries to initialize tables
   * @param statements A list of statements to prepare
   * @param queries A list of queries to initialize
   * @return A initialized database
   */
  function DbPostgres.t open(name, tables, statements, queries){
    @spawn(
      /* 1 - Opening the postgres database */
      match(Postgres.connect(name, none, name)){
      case ~{failure} :
        Log.error0(name, "Can't open Postgres database {failure}")
        @fail("DbPostgres.open")
      case {success : c0} :
        c = c0
        Log.info(c, "opened")
        c = Postgres.authenticate(c)
        check_error("Authentication", c)
        /* 2 - Init tables
         TODO: Add a way to unactivate table initialization */
        List.iter(
          function(table){
            (c, _) = Postgres.query(c, void, table, function(_, _, _){void})
            match(Postgres.get_error(c)){
            case {none} :
              Log.debug(c, "Initial query: {table}")
            case {some: e} :
              Log.error(c, "An error occurs while processing an initial query:
error: {e}
query: {table}
")
            }
          }
          , tables)
        /* 3 - Prepare statements */
        List.iter(
          function(~{id, query, types}){
            c = Postgres.parse(c, id, query, types)
            match(Postgres.get_error(c)){
            case {none} :
              Log.debug(c, "Prepared statement: {id}")
            case {some: e} :
              Log.error(c, "An error occurs while prepare statements
error: {e}
id: {id}
query: {query}
")
              @fail
            }
          }
          , statements)
        /* 4 - Init queries */
        List.iter(
          function(query){
            (c, _) = Postgres.query(c, void, query, function(_, _, _){void})
            match(Postgres.get_error(c)){
            case {none} :
              Log.debug(c, "Initial query: {query}")
            case {some: e} :
              Log.error(c, "An error occurs while processing an initial query:
error: {e}
query: {query}
")
            }
          }
          , queries)
        Postgres.release(c)
        c0
      }
    )
  }

  private
  @expand
  function check_error(msg, c){
    match(Postgres.get_error(c)){
    case {some: e} :
      Log.error(c, "{msg} failure {e}")
      @fail
    case {none} :
      Log.debug(c, "{msg} success")
    }
  }

  /**
   * Create a database set from a prepared statement. The type of value in
   * database set is inferred by the compiler and should match with the prepared
   * statement.
   * @param db The postgres database to request.
   * @param name Name of the prepared statement.
   * @param args Arguments of prepared statement, a list of pre-packed values.
   * @return A database set.
   */
  function DbPostgresSet.t('a) build_dbset(DbPostgres.t db, name, list(Postgres.data) args, 'a def){
    c = @wait(db)
    c = Postgres.bind(c, "", name, args)
    check_error("Bind", c)
    c = Postgres.describe(c, {statement}, name)
    check_error("Describe", c)
    /* For moment we fetch all rows, then we create an iterator with it. But
     we have other alternatives. (Lazy execute, PG Cursor?)
     */
    (c, rows) =
      Postgres.execute(c, [], "", 0, function(conn, msg, acc){
        match(msg){
        case {DataRow:row}:
          match(option('a) PostgresTypes.to_opa_default(conn, row, def)){
          case {some:data}: [data|acc]
          case {none}:
            Log.error(c, "A row can't be unserialized, skip it")
            acc
          }
        default: acc
        }
      })
    Postgres.release(c)
    check_error("Execute", c)
    iter = Iter.of_list(List.rev(rows))
    DbSet.build(iter, void)
  }

  /**
   * As [build_dbset] but for queries which returns only one value.
   * @param db The postgres database to request.
   * @param name Name of the prepared statement.
   * @param args Arguments of prepared statement, a list of pre-packed values.
   * @return A value.
   */
  function 'a build_uniq(DbPostgres.t db, name, list(Postgres.data) args, 'a def){
    build_option(db, name, args, def) ? def
  }

  /**
   * As [build_uniq] but for queries which returns an optionnal value.
   * @param db The postgres database to request.
   * @param name Name of the prepared statement.
   * @param args Arguments of prepared statement, a list of pre-packed values.
   * @return A value.
   */
  function option('a) build_option(DbPostgres.t db, name, list(Postgres.data) args, 'a def){
    match(Iter.to_list(DbSet.iterator(build_dbset(db, name, args, def)))){
    case [] : none
    case [v]: some(v)
    case [t|_]:
      Log.error(@wait(db), "Multiple value was returned while expecting strictly one")
      some(t)
    }
  }

  private
  function void execute_procedure(DbPostgres.t db, string procname, list(Postgres.data) args, string name){
    c = @wait(db)
    args = List.map(PostgresTypes.string_of_field_value, args)
    args = List.to_string_using("(", ")", ",", args)
    command = "SELECT {procname}{args}"
    (c, _) = Postgres.query(c, void, command, function(_,_,_){void})
    Postgres.release(c)
    check_error("{name}({command})", c)
  }

  function void update_or_insert(DbPostgres.t db, string procname, list(Postgres.data) args){
    execute_procedure(db, procname, args, "UpdateInsert")
  }

  /**
   * Execute the prepared statement [name] to remove datas.
   * @param db The postgres database to request.
   * @param name Name of the prepared statement.
   * @param args Arguments of prepared statement, a list of pre-packed values.
   */
  function void remove(DbPostgres.t db, string name, list(Postgres.data) args){
    c = @wait(db)
    c = Postgres.bind(c, "", name, args)
    check_error("Bind", c)
    (c, _) =
      Postgres.execute(c, void, "", 0, function(c, msg, acc){
        match(msg){
        case {DataRow:_}: Log.warning(c, "Receive rows on delete command")
        default: acc
        }
      })
    check_error("Execute", c)
    Postgres.release(c)
  }

  function sum_to_enum('a value){
    OpaValue.Record.get_uniq_field_name(value) ? @fail
  }

}

@opacapi DbPostgres_open = DbPostgres.open
@opacapi DbPostgres_build_dbset = DbPostgres.build_dbset
@opacapi DbPostgres_build_uniq = DbPostgres.build_uniq
@opacapi DbPostgres_build_option = DbPostgres.build_option
@opacapi DbPostgres_update_or_insert = DbPostgres.update_or_insert
@opacapi DbPostgres_remove = DbPostgres.remove
@opacapi DbPostgres_sum_to_enum = DbPostgres.sum_to_enum
@opacapi Option_map = Option.map


