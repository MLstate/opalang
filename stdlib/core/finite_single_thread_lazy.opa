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
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

@opacapi
type finite_single_thread_lazy('a) = Mutable.t(
  { delayed : -> 'a } /* a lazy not yet evaluated */
/ { evaluating } /* a value that is being forced right now */
/ { value : 'a } /* a lazy that has been computed before */
)

/**
 * {1 Interface}
 */

/** Use by the compiler to implement non lambda recursive values, where recursions are lambda guarded (hence evaluating the recursion has a chance to be finite).
    Do not use it for any other purpose then finite lazy values with non concurrent force calls */
FiniteSingleThreadLazy = {{
  make(f : -> 'a) : finite_single_thread_lazy('a) =
    Mutable.make({ delayed = f })

  /** force evaluation of the lazy value
      generate in case of infinite forcing loop and concurrency forcing */
  force(lazy:finite_single_thread_lazy('a)) : 'a =
    match lazy.get() with
    | ~{ value } -> value
    | { evaluating } ->
      // for position, it is probably simpler to just rely on
      // stack traces
      @fail("One recursive value definition is ill defined.")
    | { delayed = fun } ->
      do lazy.set({ evaluating })
      value = fun()
      do lazy.set(~{ value })
      value
}}

@opacapi FiniteSingleThreadLazy_force = FiniteSingleThreadLazy.force
