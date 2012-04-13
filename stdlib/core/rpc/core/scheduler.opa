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
    @author David Rajchenbach-Teller
**/


/**
 * Functions related to scheduling
 *
 * @author David Rajchenbach-Teller
 */


/**
 * {1 About this module}
 *
 * Scheduling functions.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Interface}
 */

Scheduler =
{{

  /**
   * {2 Timers}
   */

  @private lltimer = @may_cps(%% BslScheduler.timer %%) : int, (-> void) -> void

  /**
   * Start an infinite timer, i.e. a function that will be triggered at regular intervals
   *
   * Note: This timer cannot be stopped.
   *
   * @param delay_ms The number of milliseconds to wait between two calls
   * @param cb The function to trigger at the end of each interval
   */
  timer(delay_ms: int, cb: -> void): void =
    lltimer(delay_ms, cb)

  /**
   * Create (but do not start) a timer that can be started and resumed at will
   *
   * @return A timer (initially not started)
   */
  make_timer(delay, cb) =
    init = { tick  = 0
             ~delay
             ~cb }
    rec val me = Session.make(init,
      (state, msg ->
        match msg with
          | { start }      ->
                do sleep(state.delay, -> send(me, { tick = state.tick }))
                { unchanged }
          | { stop }       ->
                { set = { state with tick = state.tick + 1 } }
          | { ~tick }      ->
            match Int.compare(tick, state.tick) with
              | {eq} ->
                do sleep(state.delay, -> send(me, msg))
                do sleep(0, state.cb)
                { unchanged }
              | _ -> { unchanged } /*Obsolete tick, drop it*/
            end
          | {~change} ->
            tick = state.tick + 1
            do sleep(change, -> send(me, { ~tick }))
            {set = {state with ~tick delay=change}}
      )
    );
    {
      start() = Session.send(me, {start});
      stop()  = Session.send(me, {stop});
      change(i) = Session.send(me, {change=i})
    }

  /**
   * Produce a function that will behave as a given function but will default to another function in case of timeout
   *
   * Note: this implementation is server-only for the moment
   *
   * @param f The original function. If this function takes too long to return, the fallback will be executed.
   * @param timeout_ms The amount of time to wait for the function to return before calling the fallback.
   * @param fallback
   * @return A function which behaves as [f], but executes [fallback] whenever [f] takes too long to return.
   */
  @server make_timeoutable(f:'a -> 'b, timeout_ms:int, fallback:-> 'b): 'a -> 'b = a ->
    ref = Reference.create(false)
    res = @callcc(k ->
        failure() = ( Continuation.return(k, fallback()))
        do Scheduler.sleep(timeout_ms, failure)
        do Continuation.return(k, f(a))
        void)
    if Reference.compare_and_swap(ref, false, true)
      then res
      else stop()


  /**
   * Execute the function and return the result if the computation has finished before the timeout.
   *
   * Note: this implementation is server-only for the moment
   *
   * @param f The function to execute. If this function takes too long to return, it returns a {failure={timeout}}.
   * @param timeout_ms The amount of time to wait for the function to return before calling the fallback.
   * @return The result of the function or a failure if it takes too long to return.
   */
  @server
  timeout(f : -> 'a, delay:int) :  -> outcome('a, {timeout}) = ->
    (make_timeoutable(
      (_ -> {success= f()})
      , delay
      , (->{failure={timeout}}) ))(void)

  /**
   *  Stop the current thread
   */
  @server
  stop() = @callcc(_k -> void)

  /**
   * {2 Sleep}
   */

  /**
   * Trigger a function after at least a given number of milliseconds.
   */
  sleep = @may_cps(%%BslScheduler.sleep%%)

  /**
   * Interrupt the current thread for a given number of milliseconds.
   *
   * Warning: this function is server-only.
   */
  @server wait(n:int): void =
    @callcc(k ->
      sleep(n, (->
        Continuation.return(k, void)
      ))
    )

  /**
   * Push a task on scheduler.
   */
  push  = @may_cps(%%BslScheduler.push%%)

  /**
   * {1 GC}
   */

  /**
   * Registers a finalize function for a given value. Finalize
   * functions are called when the corresponding value is collected.
   */
  finalize = @may_cps(%%BslScheduler.finalize%%) : ('a -> void), 'a -> void

  /**
   * Set the maximum of compute the scheduler can do before scheduling
   * Use it with caution! 
   */
  set_max_compute_successive = %%BslScheduler.set_max_compute_successive%%

  /**
   * Set the number of Apply for each compute
   * Use it with caution! 
   */
  set_nb_step_apply = %%BslScheduler.set_nb_step_apply%%

}}

sleep = Scheduler.sleep
timer = Scheduler.timer
