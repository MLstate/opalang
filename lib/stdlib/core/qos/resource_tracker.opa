/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import stdlib.core.{rpc.core, fresh}

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

@private
type ResourceTracker.msg('message) =
  {msg : 'message}
/ {signal : ResourceTracker.signal}
/ {expire}

@abstract
type ResourceTracker.manager('message, 'result) =
  Cell.cell(ResourceTracker.msg('message), {result :'result} / {} / {stop})

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

  @private
  collect = Reference.create(false)

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
          do @atomic(Hashtbl.remove(managers, index))
          do Scheduler.push(-> collect(state, signal))
          {return={stop} instruction={stop}}
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
    do GC.timer.start()
    manager

  /**
   * Trying to access to the resource associated to its manager.
   */
  call(
    manager : ResourceTracker.manager('message, 'result),
    message : 'message
  ) : 'result =
    match Cell.call(manager, {msg = message}) with
    | ~{result} -> result
    | _ -> @fail

  /**
   * Send a termination signal to a manager.
   */
  term(manager : ResourceTracker.manager, signal : ResourceTracker.signal) =
    Scheduler.push(-> ignore(Cell.call(manager, ~{signal})))

  // TODO - Move on to the cell module
  @private
  call_or_error(cell, message) =
    @callcc(k ->
      herror() = Continuation.return(k, {failure})
      cbs = ~{herror hsuccess=-> void}
      Continuation.return(k,
        {success = Cell_private.llcall_more(cell, message, {none}, {none}, cbs)}
      )
    )

  /**
   * {3 Garbage collection of tracked resources}
   */
  GC = {{
    /**
     * Execute manually a step of garbage collector.
     */
    step() =
      match @atomic(
        match Reference.get(collect) with
        | {true} -> false
        | {false} ->
          do Reference.set(collect, true)
          true
      ) with
      | {false} ->
        /* The garbage collection is already in progress */
        void
      | {true}  ->
        /* No collection in progress start a new one */
        @atomic(Reference.set(collect, false))

    /**
     * The GC timer
     */
    timer = Scheduler.make_timer(
      // This is a stupid constant, we should have a resource management policy that
      // force the garbage collector or/and dynamically set the delay
      19*60*1000,
      step
    )
  }}

}}

