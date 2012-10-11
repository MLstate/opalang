/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Defining API on external continuation.
 *
 * This file provides the definition of a continuation, and define
 * an API to manipulate them.
 *
 * @category general
 * @author Mathieu Barbin, 2010
 * @destination public
 * @stability wip, unstable
 */


/**
 * {1 About this module}
 *
 * With the apparition of {[ @callcc : ( continuation('a) -> void ) : void }
 * there is a need for the user to apply a continuation to a value of type ['a]
 *
 * Since the continuation will also be the native support of transactions,
 * there is a need for an API.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
**/

/**
 * {1 Types defined in this module}
**/

type bycps__unit = external

/**
 * The abstract type of a continuation
 */
@opacapi
type continuation('a) = external

/**
 * {1 Interface}
 */

Continuation = {{

  /**
   * Return a value using a continuation : apply the continuation to the value.
   * This is the standard function to use when you use @callcc
   *
   * {[
   *   f(x) =
   *     call(k) = Continuation.return(k, x)
   *     @callcc(call)
   * }
   *
   * Do not use {!Continuation.execute} in standard cases.
  **/
  return = %%bslcps.user_return%% : continuation('a), 'a -> void

  /**
   * Add a translation step to the input value of a continuation.
   */
  map(f:'new -> 'old, k) = Continuation.make{
    input -> return(k,f(input))
  }

  /**
   * Like {!Continuation.return} but can't be scheduled before to return the value
   * Hack for synchronous situations.
   * Not for casual user.
  **/
  execute = %%bslcps.execute%% : continuation('a), 'a -> void

  make = @may_cps(%%bslcps.user_cont%%) : ('a -> void) -> continuation('a)

  /**
   * This bypass is defined on both sides, but the client implementation is dummy.
   * This is done so that the code may not be sliced differently if we add some debug
   * print of the stack trace in the middle of a client-side code.
  **/
  print_trace_of = %% bslcps.print_trace %% : continuation('a) -> void

  /**
   * see {!Continuation.print_trace_of}
   * applied on the current continuation.
  **/
  print_trace() : void =
    @sliced_expr({
      server =
        @callcc(k ->
          do print_trace_of(k)
          return(k,void)
      )
     client = void
   })
}}


/**
 * The type of a future.
 * Currently, used by generated code only.
 * May become used by the user, if we export the
 * concurrency primitives spawn, wait.
**/
@opacapi
type Cps.future('a) = external
