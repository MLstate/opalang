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
 * Utility functions on database paths.
 *
 * @category DATABASE
 * @author Louis Gesbert, 2010
 * @destination PUBLIC
 * @stability STABILIZING
 */

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
type val_path('a) = path_t(path_val_p, 'a)

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
type ref_path('a) = path_t(path_ref_p, 'a)

/**
 * {1 Interface}
 */

Db = {{

  /**
   * Writes data at the given path in the database.
   * [do write(@/path,42)] is equivalent to [do /path <- 42]
   *
   * @example [write(@/path,42)] initializes or updates the data at path [/path]
   */
  write = %%path_write%%

  `<-`(d,a) = write(d,a)

  /**
   * Reads the data currently held at a reference path.
   *
   * @example [read(@/path)] is equivalent to [/path]
   */
  read = %%path_read%%

  /**
   * Turns a reference-path into a value-path, in fact taking a snapshot.
   *
   * @example [get_val(@/path)] is equivalent to [!/path]
   */
  get_val = %%path_ref_to_val%%

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
  transaction(db,f) =
    tr = Db_private.Transactions.start(db,{})
    tr = Db_private.Transactions.continue(tr,(_ -> some(f())),(_ -> none))
    Db_private.Transactions.commit(tr)


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


`<-` = Db.`<-`

// interface needed by the compiler
type dbgenlink_database = external
type dbgenlink_db_path_key = external
type dbgenlink_path = external
type dbgenlink_trans = external
type dbgenlink_path_trans = external
type dbgenlink_data_d = external
