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
 * @stability UNSTABLE
 */

/**
 * This contains bindings that are needed by the database access generation
 * engine of OPA. Shouldn't be used by hand.
 */

/* These types come from libqml's mlbsl. We just ensure OPA knows about them here
 * so that it won't complain when encoutering them in intermediate code */
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

@opacapi type transactions_t('a) = external
@opacapi type dbgraph_diff = external


// The following needs to be defined for dbgen !
exception_transaction_failure = { Transaction_failure }

Db_private = {{

/**
 * Low-level transaction handling.
 *
 * Database operations can only be performed inside a transaction. If none is specified,
 * the compiler automatically introduces them -- for each operations, for the time being.
 * This is inefficient, and transactions can be added explicitly with this module.
 *
 * Note that writes are asynchronous and only verified at the time of commit.
 */
Transactions = {{

  /**
   * Starts a new transaction from a database and a value
   */
  start = %%transactions_start%%

  /**
   * [continue(tr,f,fallback)] applies [f] to the data held in transaction [tr],
   * or [fallback] if either [f] fails or there was already a failure in previous
   * operations in [tr]. Database operations performed by [f] will be done within
   * the transaction
   */
  continue = %%transactions_continue%%

  /**
   * Commits the transaction to the database, validating its operations and making
   * them permanent and visible to the others. Returns the value held in the transaction.
   * If there was a failure, the transaction is rolled back and performs no changes ; the
   * returned value is that of [fallback] functions
   */
  commit = %%transactions_commit%%

  /**
   * Wipe out the transaction and return its fallback value
   */
  abort = %%transactions_abort%%

  /**
   * From within a transaction, forces it to fail. Outside of a transaction, returns its
   * argument unchanged.
   */
  fail =
    bp = %%transactions_fail%%
    db,x -> bp(db,"Transaction manually aborted",x)

  /**
   * Gives out the value within a transaction. Use wisely, since this contradicts the
   * global monadic design for transactions.
   */
  get_value = %%transactions.get_value%%
}}

  /**
   * Same as Db.export_to_xml . For backwards-compat only
   */
  export_to_xml = %%dbser.dump_db%%

}}
