/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Functions related to scheduling
 *
 * @author David Rajchenbach-Teller
 * @author Quentin Bourgerie
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
 * Tyoe of keys associated of scheduler jobs.
 */
type Scheduler.key = external

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
    delay=Reference.create(delay)
    state=Reference.create({stop}):reference({stop}/{pending:Scheduler.key})
    set_state(v) = Reference.set(state,v)
    state() = Reference.get(state)
    not_stopped() = state()!={stop}
    rec loop() =
      k = asleep(Reference.get(delay), next)
      do set_state({pending=k})
      cb()
    and next() = if not_stopped() then loop()
    {
      start() = if not_stopped() then void else loop()
      stop() =
        match state()
        {stop} -> void
        {pending=k} ->
          do set_state({stop})
          abort(k)
        end
      change(ms) = Reference.set(delay,ms)
    } : { ... }

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
  @server_private make_timeoutable(f:'a -> 'b, timeout_ms:int, fallback:-> 'b): 'a -> 'b = a ->
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
  @server_private
  timeout(f : -> 'a, delay:int) :  -> outcome('a, {timeout}) = ->
    (make_timeoutable(
      (_ -> {success= f()})
      , delay
      , (->{failure={timeout}}) ))(void)

  /**
   *  Stop the current thread
   */
  @server_private
  stop() = @callcc(_k -> void)

  /**
   * {2 Sleep}
   */

  /**
   * Trigger a function after at least a given number of milliseconds.
   */
  sleep(i, f) = @may_cps(%%BslScheduler.sleep%%)(i, f)

  /**
   * As sleep but returns the associated jobs key.
   */
  asleep(i, f) = @may_cps(%%BslScheduler.asleep%%)(i, f)

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
   * Introduce a scheduling point.
   *
   * Warning: this function is server-only.
   */
  @server_private
  point() = @callcc(Continuation.return(_,void))

  /**
   * Reschedule the current computation for later.
   *
   * Warning: this function is server-only.
   */
  @server_private
  yield() = @callcc(k -> Scheduler_push( -> Continuation.return(k,void)))

  /**
   * Switch to the task provided by the function while rescheduling the current computation
   *
   * Warning: this function is server-only.
   */
  @server_private
  switch(f) = @callcc(k ->
    do Scheduler_push( -> Continuation.return(k,void))
	  f()
  )

  /**
   * {2 Jobs management}
   */

  /**
   * Abort the jobs associated of the [key].
   */
  abort(key:Scheduler.key) = %%BslScheduler.abort%%(key)

  #<Ifstatic:OPA_BACKEND_QMLJS>
  #<Else>
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

  #<End>
}}

sleep = Scheduler.sleep
timer = Scheduler.timer
