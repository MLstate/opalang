/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Basic functions on booleans.
 *
 * @category data
 * @destination public
 * @author David Rajchenbach-Teller, 2010 (documentation)
 * @stability stable
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

/**
 * Order type label for bool
 */
type Bool.order = Order.default

/**
 * {1 Interface}
 */

Bool =
{{
  /**
   * Convert a boolean to a string representation.
   *
   * @return ["true"] or ["false"]
   */
  @stringifier(bool) to_string(b: bool) = if b then "true" else "false"

  /**
   * Convert a string to a boolean representation.
   */
  of_string =
    | "true"  -> some(true)
    | "false" -> some(false)
    | _       -> none

  /**
   * Order on boolean values ([false] < [true])
   */
  order = Order.make(ordering) : order(bool, Bool.order)

  /**
   * Comparing boolean values ([false] < [true])
   */
  ordering(b1, b2) : Order.ordering =
    // TODO: Do we want BSL for that? Guess not...
    match (b1: bool, b2: bool) with
    | ({false}, {true}) -> {lt}
    | ({true}, {false}) -> {gt}
    | ({false}, {false})
    | ({true}, {true}) -> {eq}

}}
