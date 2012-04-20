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
 * Utility functions on database paths.
 *
 * @category DATABASE
 * @author Louis Gesbert, 2010
 * @destination PUBLIC
 * @stability STABILIZING
 */
import-plugin badop
import stdlib.core.{date,map,parser}

/**
 * {1 About this module}
 *
 * This module defines the primitive types and functions for persistent data handling,
 * as well as utility functions on data. It contains all database functions that are
 * not built-in.
 *
 * {1 Where do I start?}
 *
 * Check the type definitions for data paths. A {!val_path} is a data that
 * have been read from the database and holds database-specific information, allowing
 * specific operations on them (like full-text searches). A {!ref_path} or reference
 * path, is a pointer to the database that can be passed as argument to a function and
 * written to.
 *
 * {1 What if I need more?}
 *
 * The functions that take reference paths as arguments either perform side-effects on
 * them ({!Db.write}, {!Db.remove}), or use them without being bound to the current version of
 * the database ({!Db.history}). Also included are low-level functions for handling database
 * transactions.
 *
 */

/**
 * {1 Types defined in this module}
 */

/*
  Private types

  These types come from the opabsl/mlbsl. They need to be defined because they
  are manipulated by the compiler.

  Not to be used by hand.
*/
@opacapi type Db3.t = badoplink_database
@opacapi type badoplink_database = external
@opacapi type badoplink_transaction = external
type badoplink_revision = external
@opacapi type badoplink_db_path_key = external
@opacapi type badoplink_path = external
@opacapi type badoplink_data_d = external
@opacapi type badoplink_db_partial_key = external

@opacapi type badoplink_node_config = external

@opacapi type badop_engine_database_options = external
@opacapi type badop_engine_t = external

@opacapi type path_t('a,'path_kind) = external
@opacapi type path_embedded_obj = external
@opacapi type path_embed_info = external

@opacapi type path_val_p = external
@opacapi type path_ref_p = external

@opacapi type opa_transaction_t('a) = external
@opacapi type dbgraph_diff = external

@opacapi Db_write = Db.write


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
type Db3.val_path('a) = Db.val_path('a, path_t(path_val_p, 'a))
@opacapi type val_path('a) = Db3.val_path('a)


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
type Db3.ref_path('a) = Db.ref_path('a, path_t(path_ref_p, 'a))
@opacapi type ref_path('a) = Db3.ref_path('a)

/**
 * {1 Interface}
 */

Db3 = {{

  @private unwrap(path) = Db.get_engine(path)

  /**
   * Given an intmap from the database, returns a key that is guaranteed not to be used
   * in that intmap. Successive calls return different results. */
  fresh_key(p) = %%path_fresh_key%%(unwrap(p))

  /**
   * Searches words in all data held in a database map ; returns a list of keys in that map.
   * Only reference paths are accepted as arguments: searches in the past are not possible
   * in the current version of the database.
   */
  intmap_search(p, word) = %%path_intmap_search%%(unwrap(p), word)

  stringmap_search(p, word) = %%path_stringmap_search%%(unwrap(p), word)

  /**
   * Folds on a map with a range and a filter;
   * @param [f] function to be applied to every element of the map.
   * @param [start] where to start the fold, boundary included. 0 is the first key of the map.
   * @param [ending] where to stop, boundary not included. [none] is ending at the last key of the map,
   * [some(3)] is stopping at the second key of the map.
   * @param [filter] which keys are accepted for the fold. (x -> {true}) is all the keys of the map
   * are accepted.
   */
  intmap_fold_range(p, f, init, start, ending, filter) =
    %%path_intmap_fold_range%%(unwrap(p), f, init, start, ending, filter)

  stringmap_fold_range(p, f, init, start, ending, filter) =
    %%path_stringmap_fold_range%%(unwrap(p), f, init, start, ending, filter)

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
  history(p, from, length) = %%path_history%%(unwrap(p), from, length)

  /**
   * Queries the history by timestamps.
   * returns a list of pairs [(timestamp, value)] with the values that have been held at
   * a path between the two given timestamps.
   */
  history_time(path, from : Date.date, to : Date.date) =
    bp = %%path_history_time%%
    r = bp(unwrap(path), Date.ll_export(from), Date.ll_export(to))
    List.map((val, time) -> (Date.ll_import(time), val), r)

  /**
   * Returns the last modification date of a path.
   */
  modification_time(path) : Date.date =
    bp(p) = %%path_modification_time%%(unwrap(p))
    Date.ll_import(bp(path))

  /**
   * Simple transaction handling: db-atomic execution of a function.
   * @param [db] the database to run the transaction in.
   * @param [f] a function that will be executed within a single transaction.
   * @return the return value of f as option, or [{none}] if the transaction commit failed.
   */
  transaction(f) =
    tr = Transaction.new()
    r = tr.try(-> some(f()), -> none)
    match tr.commit() with {success} -> r | {failure} -> none

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

}}


/* ideally, we don't want the 'a parameter, and try should be a type-forall('a).
   However, at the time being functions with types-forall can't be poperly
   serialised: count this as a temporary workaround */
type transaction('a) = {

  /** Calls a function within the transaction. On error, the execution is
      skipped (the error status can be checked by calls to [try] or [commit]) */
  in: (-> void) -> void;

  /** [try(f,fallback)] applies function [f] within the transaction,
      triggering [fallback] in case of problem */
  try: (-> 'a), (-> 'a) -> 'a;

  /** Attempts to commit the given transaction. If successful, the transaction
      is reset. */
  commit: -> outcome(void,void);

  /** Aborts the transaction ; any further calls to [try] will trigger
      the error case, any further calls to [in] will be ignored */
  rollback: -> void;

}


/* Implementation note:
   providing Transaction as an object, where opa_transaction_t doesn't appear
   explicitely makes passing transactions between client and server possible
   (because we actually only pass handles to server closures)
*/
Transaction = {{

  @private @server_private Make(tr : opa_transaction_t) : transaction = {
    in(f) = %%opa_transaction_continue%%(tr,f, (-> void))
    try = %%opa_transaction_continue%%(tr,_,_)
    commit() = %%opa_transaction_commit%%(tr)
    rollback() = %%opa_transaction_abort%%(tr)
  }

  /** Start a new, empty transaction */
  new() = Make(%%opa_transaction_start%%())

  /** Starts a new transaction, synchronously initialised on the given
      databases (it will still be extended to other databases if and when
      needed, as with [new]) */
  new_on(databases) =
    tr = %%opa_transaction_start%%()
    do %%opa_transaction_init%%(tr,databases)
    Make(tr)

  /** Force the failing of the transaction currently in the execution
      context */
  fail() = %%opa_transaction_fail%%()

  /**
    Note on nested transactions: if a new transaction is started within
    another one (during a call to [try] or [in]), their execution flows
    are merged. Errors will always trigger the topmost handler, and only
    the top-level commit will actually commit to the database.

    Moreover, writes done in child transaction are visible to the parent
    even if they have not been committed. In other words, the commit of
    the inner transaction does'nt have an effect on the database.
  */
}}



@opacapi Db3_val_to_val(db3path:path_t(path_val_p, 'a)) = Db.build({
  id = "db3path";
  read() = %%path_read%%(db3path)
  exists() = %%path_exists%%(db3path)
  more = void
  engine = db3path;
}) : val_path('a)

@opacapi Db3_ref_to_ref(db3path:path_t(path_ref_p, 'a)) = Db.build({
  id = "db3path";
  read() = %%path_read%%(db3path)
  exists() = %%path_exists%%(db3path)
  more = {
    remove()    = %%path_remove%%(db3path)
    write(data) = do %%path_write%%(db3path, data) true
  }
  engine = db3path;
}) : ref_path('a)

@private _init_default_options = match Db.default_cmdline with
  | {none} -> void
  | {some = {local = {some = local}}} -> %%badop_engine.set_default_local%%(local)
  | {some = {local = {none}}} -> %%badop_engine.set_default_local%%(%%BslFile.mlstate_dir%%(void))
  | {some = {remote = {some = remote}}} -> %%badop_engine.set_default_remote%%(remote)
  | {some = {remote = {none}}} -> %%badop_engine.set_default_remote%%("localhost")
