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
/*
    @author Louis Gesbert
**/

/**
 * Manipulating transactions.
 *
 * This is a module on top of sessions that allows handling database transactions.
 *
 * @category database
 * @author Louis Gesbert, 2010
 * @destination public
 * @stability unstable
 */

/**
 * {1 About this module}
 *
 * This module provides the functions for dealing with transactions. Transaction
 * controllers are built on top of sessions, and allow for consistent sequences of
 * operations on the database. In particular, they are protected from external
 * modifications of the database, and can be consistently rolled back, manually
 * or in the case of an error.
 *
 * {1 Where do I start?}
 *
 * [Transaction.make] creates a transaction unique to the server; the
 * function given as parameter will receive messages sent to the channel, and
 * is guaranteed to be executed within the transaction.
 *
 * [Transaction.make_multi], on the other hand, allows multiple concurrent
 * transactions (typically, indexed by user). The messages received must
 * contain an id that will be used to retrieve the transaction.
 */


/**
 * {1 Types defined in this module}
 */

/**
 * The type that the user-provided protocol must return, triggering
 * the next action on the transaction.
 */
type Transaction.instruction =
    { hold }
    /** Keep the transaction running, and re-use it in the next call; don't
        update the DB */
  / { commit }
    /** Commit everything that has been written to the database so that other
        users can see the changes. The result-handler will be then called on the
        current message with the result ([commit] may fail) */
  / { abort }
    /** Abort everything that has been done since the last commit. Next time
        the protocol is called, it will start fresh from the current state of the
        database. Always calls the result handler on [{fail}], since this makes
        the transaction fail. */

/**
 * The result of a database operation. Used on the result-handler.
 */
type Transaction.result = { ok } / { fail }

Transaction =
  Bypass = Db_private.Transactions
  in_transaction(protocol,result,tr) : option(transactions_t(void)) =
    // execute the protocol within the transaction
    tr =
      Bypass.continue(
        tr,
        ({} -> { success = protocol() }),
        ({} -> { failure })
      )
    // make a decision depending on what the protocol returned
    match Bypass.get_value(tr) with
      { success = { hold } } ->
        clean_tr = Bypass.continue(tr, (_ -> {}), (_ -> {}))
        { some = clean_tr }
      { success = { commit } } ->
        success = Bypass.commit(Bypass.continue(tr,(_ -> {ok}),(_ -> {fail})))
        do sleep(0, (-> result(success)))
        { none }
      { success = { abort } } ->
        do sleep(0, (-> _ = Bypass.abort(tr); result({ fail })))
        { none }
      { failure } ->
        do sleep(0, (-> result({ fail })))
        { none }
{{

  /**
   * {2 Creating transactions}
   */

  /** Starts a session bound to a single transaction. Use it either for
      whole-server transactions (logging...) or with multiple instances.
      Messages received will update the transaction linearly;
      the transaction is synchronised on the current state of the database
      when receiving their first message after a [{commit}] or [{abort}] */
  make(
    database: badoplink_database,
      /** The database in which the transaction should take place. Use
          [database] if you didn't name your database, it's the default
          name. */
    protocol: 'msg -> Transaction.instruction,
      /** User-provided protocol: receives messages sent to the session.
          All uses of the database executed within [protocol] are done in
          the current transaction. */
    result: 'msg, Transaction.result -> void
      /** This function is called after a [{commit}], an [{abort}] or when
          a transaction failed. It's given the last message and the result */
  ) : channel('msg) =
    init = { none }
    on_message(status,msg) =
      tr = match status with
        { some = tr } -> tr
        { none } -> Bypass.start(database,{})
      status = in_transaction((-> protocol(msg)),result(msg,_),tr)
      { set = status }
    Session.make(init, on_message)

  /** Same as [make], but with multiple, possibly concurrent transactions.
      The channel receives values of type [{ id; msg }], and the [protocol]
      and [result] functions receive the [id] as well. */
  make_multi(
    database: badoplink_database,
    protocol: 'id, 'msg -> Transaction.instruction,
    result: 'id, 'msg, Transaction.result -> void
  ) : channel({ id: 'id msg: 'msg }) =
    init = Map.empty : map('id, transactions_t)
    on_message(status, ~{ id msg }) =
      tr = match Map.get(id,status) with
        { some = tr } -> tr
        { none } -> Bypass.start(database,{})
      tropt = in_transaction((-> protocol(id,msg)), result(id,msg,_), tr)
      status = match tropt with
        { some = tr } -> Map.add(id, tr, status)
        { none } -> Map.remove(id, status)
      { set = status }
    Session.make(init, on_message)

}}
