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


Scheduler =
{{

  /**
   * {1 Timers}
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
  @server make_timeoutable(f:'a -> 'b, timeout_ms:int, fallback:-> 'b): 'a -> 'b =
     g(x) =
       @callcc(k ->
         s = Session.make({},
           (_, msg -> do Continuation.return(k, msg); {stop})
           )
         do sleep(0, -> send(s, f(x)))
         do sleep(timeout_ms, -> send(s, fallback()))
         void
        )
     g


  /**
   * {1 Sleep}
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

}}

sleep = Scheduler.sleep
timer = Scheduler.timer
