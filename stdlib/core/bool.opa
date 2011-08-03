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
