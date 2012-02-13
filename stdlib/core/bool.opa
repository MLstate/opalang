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
