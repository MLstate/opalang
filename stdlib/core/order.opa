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
 * Ordering elements.
 *
 * Functions from this module are used to define maps, sets, etc.
 *
 * @author David Rajchenbach-Teller, Raja Boujbel 2010
 */

/**
 * {1 About this module}
 *
 * A total order on elements of a given type.
 *
 * Total orders are characterized both by the type of elements they can order
 * and by an additional type label which may be used to ensure that two
 * orderings on elements of the same type cannot be confused.
 *
 * For instance, it is possible to define three totally incompatible orderings on strings,
 * one which differentiates between uppercase and lowercase ([String.order]),
 * one which mixes uppercase and lowercase ([String.order_ci]) and one which attempts to
 * interpret numbers embedded in texts ([String.order_natural]). If we define
 * a set to which we add elements "hello 1", "Hello 1" and "Hello 01", three
 * behaviors are possible: if the set has been defined with [String.order],
 * the result will contain 3 elements; if the set has been defined with [String.order_ci],
 * the result will contain 2 elements; if the set has been defined with [String.order_natural],
 * the result will contain only 1 element.
 *
 * To ensure that the three orderings cannot be confused, hence resulting in unspecified
 * set contents, the sets defined from these distinct orders have different types, respectively
 * [set(string, {String.order})], [set(string, {String.order_ci})] and [set(string, {String.order_natural})].
 *
 * @param 'item  The type of elements compared
 * @param 'label A label used to differentiate incompatible orderings
 * on elements of the same type.
 *
 * {1 Where should I start ?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

@abstract type order('item, 'label) = ('item, 'item -> Order.ordering)
@abstract type Order.default = void
@opacapi
type Order.comparison = {lt} / {gt} / {eq} / {neq}
type Order.ordering   = {lt} / {gt} / {eq}

/**
 * {1 Interface}
 */

Order = {{

  string_of_comparison(x:Order.comparison) =
    match x with
    | {lt} -> "lt"
    | {eq} -> "eq"
    | {gt} -> "gt"
    | {neq}-> "neq"

  string_of_ordering(x:Order.ordering) =
    string_of_comparison(x <: Order.comparison)

/**
 * Define an order from a comparison function.
 *
 * @param compare A function used to compare two elements of the same type.
 * @return An order on elements of a given type. You should coerce this with an ordering label to ensure
 * safety of data structures built from this order.
 */
make_unsafe(cmp: ('a, 'a -> Order.comparison)) =
    //TODO: Using @typeof, reject ['a] for which the comparison does not create a total order
    sanitized(a,b) = match cmp(a,b) with
      | {lt} -> {lt}
      | {gt} -> {gt}
      | {eq} -> {eq}
      | {neq}-> error("[Order.compare] These two elements cannot be compared")
    sanitized : order('a, 'b)

make(cmp: ('a, 'a -> Order.ordering)) =
    cmp : order('a, 'b)

make_by(f: 'a -> 'b, o: order('b, 'c)) : order('a, 'd) =
  (x, y -> compare(f(x), f(y), o))

reverse(order: order('item, 'label)):order('item, 'other_label) =
  reversed(x, y) = match order(x,y) with
     | {gt} -> {lt}
     | {lt} -> {gt}
     | {eq} -> {eq}
  make(reversed)

product(order1:order('item1,'label1),order2:order('item2,'label2)) : order(('item1,'item2),('label1,'label2)) =
  aux((l1,l2),(r1,r2)) =
    match compare(l1,r1,order1) with
    | {eq} -> compare(l2,r2,order2)
    | c -> c
  make(aux)

/**
 * The default ordering on items of any type.
 *
 * Do not attempt to use this ordering on items which contain functions
 */
default =
    @nonexpansive(make_unsafe(@toplevel.compare)) : order('a, Order.default)

/**
 * Compare two elements using a given order.
 */
ordering(x:'a, y:'a, order:order('a, 'b)): Order.ordering =
    order(x,y)


is_smaller(x:'a, y:'a, order:order('a, 'b)): bool =
    match compare(x,y,order) with
      |  {lt} -> true
      |  _    -> false

is_smallereq(x:'a, y:'a, order:order('a, 'b)): bool =
    match compare(x,y,order) with
      |  {lt} | {eq} -> true
      |  _    -> false

is_greater(x:'a, y:'a, order:order('a, 'b)): bool =
    match compare(x,y,order) with
      |  {gt} -> true
      |  _    -> false

is_greatereq(x:'a, y:'a, order:order('a, 'b)): bool =
    match compare(x,y,order) with
      |  {gt} | {eq } -> true
      |  _    -> false

/**
 * Checks the equality of two elements using a given order
 */
equals(x:'item,y:'item,order:order('a,'order)): bool =
  match order(x,y) with {eq} -> {true} | _ -> {false}

/**
 * Checks the inequality of two elements using a given order
 */
not_equal(x:'item,y:'item,order:order('a,'order)) =
  match order(x,y) with {eq} -> {false} | _ -> {true}


compose(order1:order('a, 'b), order2:order('a, 'b)) : order('a, 'b) =
  comp(x, y) =
    match order1(x,y) with
      | {eq} -> order2(x, y)
      | r -> r
  comp

of_int(i) : Order.ordering =
  match i with
  | -1 -> {lt}
  |  0 -> {eq}
  |  1 -> {gt}
  | _ -> @fail

to_int(o:Order.ordering) =
  match o with
  | {lt} -> -1
  | {eq} -> 0
  | {gt} -> 1

make_from_low_level_compare(cmp: ('a, 'a -> int)): order('a, 'b) =
(
  order(x,y) = of_int(cmp(x,y))
  order
)

/*
  BEGIN: Transitional block -- progressively remove these functions
*/

//@deprecated({use="equals"})
equal = equals

//@deprecated({use="ordering"})
compare = ordering//Doesn't match our naming conventions ([compare] vs. [ordering])

/*
 END: Transitional block
*/
}}
