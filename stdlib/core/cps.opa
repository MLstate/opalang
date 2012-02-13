/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
     client =
       error("Continuation.print_trace was called on the client side")
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
