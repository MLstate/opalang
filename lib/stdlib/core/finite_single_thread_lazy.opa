/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
