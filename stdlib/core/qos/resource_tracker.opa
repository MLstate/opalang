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
import stdlib.core.{rpc.core, fresh}
import-plugin qos
/**
 * Manager of collectible resources, binding with QOS (appserver)
 *
 * @category WEB
 * @author Mathieu Barbin, 2011
 * @destination PUBLIC
 * @stability UNTESTED
**/

/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

#<Ifstatic:OPA_BACKEND_QMLJS>
@private
type ResourceTracker.msg('message) =
  {msg : 'message}
/ {signal : ResourceTracker.signal}
/ {expire}

@abstract
type ResourceTracker.manager('message, 'result) =
  Cell.cell(ResourceTracker.msg('message), {result :'result} / {})

type ResourceTracker.signal =
  {expiration}

/**
 * {1 Interface}
 */
@server_private
ResourceTracker = {{

  @private
  managers : Hashtbl.t(int, ResourceTracker.manager('message, 'result)) =
    @nonexpansive(Hashtbl.create(16))

  @private
  fresh = Fresh.server(identity)

  /**
   * Some Signal
  **/
  Signal = {{
    EXPIRATION = {expiration} : ResourceTracker.signal
  }}

  /**
   * Creating a new manager.
   */
  create(
    state:'state,
    handler:'state, 'message -> ('state, 'result),
    expire:'state -> option(ResourceTracker.signal),
    collect:'state, ResourceTracker.signal -> void
  ) : ResourceTracker.manager('message, 'result) =
    index = fresh()
    manager = Cell.make(state, (state, msg ->
        stop(signal) =
          do Scheduler.push(-> collect(state, signal))
          {return={} instruction={stop}}
        match msg with
        | {msg = msg} ->
          (set, result) = handler(state, msg)
          {return=~{result} instruction=~{set}}
        | ~{signal} -> stop(signal)
        | {expire} ->
          match expire(state) with
          | {none} -> {return={} instruction={unchanged}}
          | {some = signal} -> stop(signal)
          end
      )
    )
    do @atomic(Hashtbl.add(managers, index, manager))
    manager

  /**
   * Trying to access to the resource associated to its manager.
  **/
  call(
    manager : ResourceTracker.manager('message, 'result),
    message : 'message
  ) : 'result =
    match Cell.call(manager, {msg = message}) with
    | ~{result} -> result
    | _ -> @fail

  /**
   * Send a termination signal to a manager.
  **/
  term(manager : ResourceTracker.manager, signal : ResourceTracker.signal) =
    Scheduler.push(-> ignore(Cell.call(manager, ~{signal})))

  /**
   * Execute manually a step of garbage collector.
  **/
  garbage_collector() =
    // TODO
    void

}}

#<Else>
type ResourceTracker.manager('message, 'result) = external
type ResourceTracker.signal = external

/**
 * {1 Interface}
 */

ResourceTracker = {{

  /**
   * Some Signal
  **/
  Signal = {{
    EXPIRATION = %%ResourceTracker.Signal.expiration%% : ResourceTracker.signal
  }}

  /**
   * Creating a new manager.
  **/
  create = %%ResourceTracker.create%% :
    /*state :*/       'state,
    /*on_message :*/  ('state, 'message -> ('state, 'result)),
    /*expire :*/      ('state -> option(ResourceTracker.signal)),
    /*collect :*/     ('state, ResourceTracker.signal -> void)
    -> ResourceTracker.manager('message, 'result)

  /**
   * Trying to access to the resource associated to its manager.
  **/
  call = %%ResourceTracker.call%% :
    /*manager*/ ResourceTracker.manager('message, 'result),
    /*message*/ 'message
    /*result*/ -> 'result

  /**
   * Send a termination signal to a manager.
  **/
  term = %%ResourceTracker.term%% : ResourceTracker.manager, ResourceTracker.signal -> void

  /**
   * Execute manually a step of garbage collector.
  **/
  garbage_collector = %%ResourceTracker.garbage_collector%% : -> void

}}
#<End>
