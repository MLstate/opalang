/*
    Copyright © 2011, 2012 MLstate

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
    @authors ?
**/

import stdlib.core.{iter,map}

/**
 * Sets and utilities to manipulate them.
 *
 * @author David Rajchenbach-Teller 2010 (review, clean-up and documentation)
 * @author Raja Boujbel
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
 * A set of elements
 */
type set('elem) = ordered_set('elem,Order.default)
type ordered_set('elem,'order) = ordered_map('elem,void,'order)

type stringset = ordered_set(string, String.order)
type intset    = ordered_set(int, Int.order)

type Set('elem,'order) =
{{

  /**
   * The empty set.
   */
  empty : ordered_set('elem,'order)

  /**
   * Return true if the given set is empty, false otherwise
   */
  is_empty : ordered_set('elem,'order) ->  bool

  /**
   * Return a set containing an single element, the given one
   */
  singleton : 'elem -> ordered_set('elem,'order)

  /**
   * Return the height of given set
   */
  height : ordered_set('elem,'order) ->  int

  /**
   * Return the size of given set
   */
  size : ordered_set('elem,'order) ->  int

  /**
   * Add an element a set.
   *
   * @param elem the element to add. If the element exists in the set,
   *             it is not added
   * @param set  the set to which to add
   * @return A set containing all the element of set plus the element [elem],
   *  if it doesn't exist on the set
   */
  add : 'elem, ordered_set('elem,'order) -> ordered_set('elem,'order)

  /**
   * Return true if an element satisfying the given predicate exists in the given set
   */
  exists : ('elem -> bool), ordered_set('elem,'order) -> bool

  /**
   * Return true if given element exists in the given set
   */
  mem : 'elem, ordered_set('elem,'order) -> bool

  /**
   * Return an existing element in the given set
   */
  get : 'elem, ordered_set('elem,'order) -> option('elem)

  /**
   * Find an element in the given set
   */
  find : ('elem -> bool), ordered_set('elem,'order) -> option('elem)

  /**
   * Filter a set of elements
   */
  filter : ('elem -> bool), ordered_set('elem,'order) -> ordered_set('elem,'order)

  /**
   * Loop through a set, collecting data from the set.
   *
   * This function guarantees that all elements of the set will be visited.
   * The order in which elements are visited, however, is not specified.
   *
   * @param f   A function invoked at each element of the
   *              set to update the element.
   * @param s   The set to visit.
   * @param acc The initial data. If the set is empty, this
   *                will also be the result.
   */
  fold : ('elem, 'acc -> 'acc), ordered_set('elem,'order), 'acc -> 'acc

  /**
   * Remove an element from the set.
   *
   * @param elem a element, possibly in the set
   * @param s    a set
   * @return [s] if the set doesn't contain [elem]. Otherwise, a set obtained
   *              by removing the element [elem]
   */
  remove : 'elem, ordered_set('elem,'order) -> ordered_set('elem,'order)

  /**
   * Change all values of the set by applying a change function to all element stored in it
   * The resulting set has the same order as initial set and the same kind of element.
   * Use [map_to_different_order] or [map_to_different_set] to change order or element type.
   *
   * @param f a function invoked at each element of the set, returns a new element
   * @param s the set to visit
   */
  //map : ('elem -> 'new_elem), ordered_set('elem,'order) -> ordered_set('new_elem, 'order)
  // in general, the previous type doesn't make sense since we don't know how to compare
  // the type 'new_elem
  // however, the polymorphic set could actually provide this function
  // with the previous type
  map : ('elem -> 'elem), ordered_set('elem,'order) -> ordered_set('elem, 'order)


  /** Same as map but with no order and element type restrictions.
      You can provide the new order.
      e.g. [StringSet.map_to_different_order(Int.of_string, Order.default, set)]
  */
  map_to_different_order : ('elem -> 'new_elem), order('new_elem,'new_order), ordered_set('elem,'order) -> ordered_set('new_elem,'new_order)

  /** Same as map but with no order and element type restrictions.
      You can provide the new Set module.
      e.g. [StringSet.map_to_different_order(Int.of_string, IntSet set)]
  */
  map_to_different_set   : ('elem -> 'new_elem), Set('new_elem,'new_order),   ordered_set('elem,'order) -> ordered_set('new_elem,'new_order)

  /**
   * Iterate on all value of the set
   *
   * @param f a function invoked at each element of the set, returns [void]
   * @param s the set to visit
   */
  iter : ('elem -> void), ordered_set('elem,'order) ->  void

  /**
   * Return true if given sets are equals, false otherwise.
   * Two sets are equal if their arrangement are similar,
   * and if their element are equal one by one
   */
  equal : ordered_set('elem,'order), ordered_set('elem,'order) -> bool

  /**
   * Intersection of two sets
   *
   * @param s1, s2 sets to intersect
   * @result [s1] if s2 is empty,
             [s2] if s1 is empty,
             [s] a set containing all elements contained in s1 and s2
   */
  intersection : ordered_set('elem,'order), ordered_set('elem,'order) -> ordered_set('elem,'order)

  /**
   * Union of two sets
   *
   * @param s1, s2 sets to merge, possibly empty
   * @return a set containing all element contained either in [s1] or in [s2].
   *         If an element appears in both sets, it appears once on result set
   */
  union : ordered_set('elem,'order),  ordered_set('elem,'order) -> ordered_set('elem,'order)

  /**
   * Get smallest element from a map
   *
   * @param s a set, possibly empty
   * @return [{none}] if [s] is empty. Otherwise, {some = elem} where [elem] is
   *         the smallest element in the set
   */
  min_binding : ordered_set('elem,'order) -> option('elem)

  /**
   * Same behavior than [min_binding], instead of returning an option, it returns the element.
   * Raise an error message if the given set is empty.
   */
  unsafe_min_binding : ordered_set('elem,'order) -> 'elem

  /**
   * Remove smallest element
   *
   * @param s a set, possibly empty
   * @return [s] if it is empty, otherwise a set minus its smallest element
   */
  remove_min_binding : ordered_set('elem,'order) -> ordered_set('elem,'order)

  /**
   * Get greatest element from a map
   *
   * @param s a set, possibly empty
   * @return [{none}] if [s] is empty. Otherwise, {some = elem} where [elem] is
   *         the greatest element in the set
   */
  max_binding : ordered_set('elem,'order) -> option('elem)

  /**
   * Same behavior than [max_binding], instead of returning an option, it returns the element.
   * Raise an error message if the given set is empty.
   */
  unsafe_max_binding : ordered_set('elem,'order) -> 'elem

  /**
   * Remove greatest element
   *
   * @param s a set, possibly empty
   * @return [s] if it is empty, otherwise a set minus its greatest element
   */
  remove_max_binding : ordered_set('elem,'order) -> ordered_set('elem,'order)

  /**
   * Subtract a part of a set
   *
   * @param l a lower bound
   * @param u an upper bound
   * @param s a set
   * @return a set containing all element appearing in [s] and  between [l] and [u]
   */
  subset : 'elem, 'elem, ordered_set('elem,'order) -> ordered_set('elem,'order)

  /**
   * Subtract a part of a set
   *
   * @param l a lower bound
   * @param s a set
   * @return a set containing all element appearing in [s] and greater than [l]
   */
  greater : 'elem, ordered_set('elem,'order) -> ordered_set('elem,'order)

  /**
   * Subtract a part of a set
   *
   * @param u an upper bound
   * @param s a set
   * @return a set containing all element appearing in [s] and smaller than [u]
   */
  less : 'elem, ordered_set('elem,'order) -> ordered_set('elem,'order)

  From :
    {{
        list : list('elem) -> ordered_set('elem,'order)
    }}

  To :
    {{
        list : ordered_set('elem,'order) -> list('elem)

        iter : ordered_set('elem,'order) -> iter('elem)
    }}


    // INTERNAL USE
  pop_max_binding : ordered_set('elem, 'order) -> (ordered_set('elem, 'order), option('elem));
  pop_min_binding : ordered_set('elem, 'order) -> (ordered_set('elem, 'order), option('elem));
  /**
   * Get a random element from the set
   * Picking distribution is approximately uniform
   */
  random_get : ordered_set('elem, 'order) -> option('elem)

}}

/**
 * {1 Interface}
 */

Set_make( order : order('elem,'order)  )  =
{{

  @private MapSet = Map_make(order)

  empty =
    MapSet.empty  : ordered_set('elem,'order)

  is_empty(set : ordered_set('elem,'order)) =
        MapSet.is_empty(set)
        : bool

  singleton(value : 'elem) =
    MapSet.singleton(value,void)
    : ordered_set('elem,'order)

  height(set : ordered_set('elem,'order)) : int =
    MapSet.height(set)

  size(set : ordered_set('elem,'order)) : int =
    MapSet.size(set)

  add(elem: 'elem, set: ordered_set('elem,'order)) =
    MapSet.add(elem,void,set)
    : ordered_set('elem,'order)

  exists( (f : 'elem -> bool), set : ordered_set('elem,'order))=
    MapSet.exists((key, _ -> f(key)), set)
    : bool

  mem( elem : 'elem, set : ordered_set('elem, 'order)) = MapSet.mem(elem, set)

  get( elem : 'elem, set : ordered_set('elem, 'order)) = Option.map(_.key,MapSet.get_key_val(elem, set))

  find( f : 'elem -> bool, set : ordered_set('elem, 'order)) =
    match MapSet.find((e,_->f(e)), set)
    {none} -> none
    {some={~key val=_}} -> some(key) : option('elem)

  filter( f : 'elem -> bool, set : ordered_set('elem, 'order)) =
    MapSet.fold((k,_v,a -> if f(k) then MapSet.add(k,void,a) else a), set, MapSet.empty)
    : ordered_set('elem, 'order)

  fold(fun: ('elem, 'acc -> 'acc), set: ordered_set('elem,'order), acc : 'acc) =
    MapSet.fold((k,_v,a -> fun(k,a)), set, acc)
    : 'acc

  remove(elem : 'elem, set : ordered_set('elem, 'order)) =
    MapSet.extract(elem, set).f1
    : ordered_set('elem, 'order)

  map(fun : ('elem -> 'elem), set : ordered_set('elem,'order))=
    fold((elem, acc -> add(fun(elem), acc)), set, empty)

  map_to_different_order( f:('elem -> 'new_elem), new_order:order('new_elem,'new_order), set:ordered_set('elem,'order) ) =
    // // Ideally
    // map_to_different_set(f, Set_make(new_order), set)
    // // But cannot recurse directly for both valid and invalid reasons (non uniform recursivity and forall problems)
    NewSet = Map_make(new_order)
    add(elem,acc) = NewSet.add(f(elem),void,acc)
    fold( add , set, NewSet.empty) : ordered_set('new_elem,'new_order)


  map_to_different_set(f:('elem -> 'new_elem), TargetSet:Set('new_elem,'new_order), set:ordered_set('elem,'order) ) =
    add(elem,acc) = TargetSet.add(f(elem),acc)
    fold( add , set, TargetSet.empty) : ordered_set('new_elem,'new_order)

  iter(fun: ('elem -> void), set: ordered_set('elem,'order)) =
    MapSet.iter((k, _v -> fun(k)), set)
    : void

  From =
    {{
        list(lst : list('elem)) =
            MapSet.From.assoc_list(List.map(((x: 'elem) -> (x,void)), lst))
            : ordered_set('elem,'order)
    }}

  To =
    {{
        list(set : ordered_set('elem,'order)) =
            MapSet.To.key_list(set)
            : list('elem)

        iter(set : ordered_set('elem,'order)) =
            Iter.map(((x: (('elem,void))) -> x.f1: 'elem), MapSet.To.iter(set))
            : iter('elem)
    }}

  equal(s1 : ordered_set('elem,'order), s2 : ordered_set('elem,'order)) =    // FIXME: Optimize
    match MapSet.compare((_, _ -> {eq}), s1, s2) with
      | { eq } -> true
      | _ -> false

  intersection((set1 : ordered_set('elem, 'order)),
               (set2 : ordered_set('elem, 'order))) =
    MapSet.intersection(set1, set2)
    : ordered_set('elem, 'order)

  union(set1: ordered_set('elem,'order), set2: ordered_set('elem,'order)) =
      MapSet.union(set1,set2)
      : ordered_set('elem,'order)

  pop_min_binding(set : ordered_set('elem, 'order))=
    (s,e) = MapSet.extract_min_binding(set)
    val = (match e with
    | { ~some } -> { some = some.f1 }
    | { none } -> { none })
    (s, val)
    : (ordered_set('elem,'order), option('elem))

  min_binding(s : ordered_set('elem, 'order)) =
    pop_min_binding(s).f2
    : option('elem)

  unsafe_min_binding(s) =
    (min_binding(s) |> Option.get_msg(->"Set.unsafe_min_binding",_))
    : 'elem

  remove_min_binding(s : ordered_set('elem, 'order)) =
    pop_min_binding(s).f1
    : ordered_set('elem, 'order)

  pop_max_binding(set : ordered_set('elem, 'order)) :
                 (ordered_set('elem,'order), option('elem))=
    (s,e) = MapSet.extract_max_binding(set)
    val = (match e with
    | { ~some } -> { some = some.f1 }
    | { none } -> { none })
    (s, val)

  max_binding(s : ordered_set('elem, 'order)) =
    pop_max_binding(s).f2
    : option('elem)

  unsafe_max_binding(s : ordered_set('elem, 'order)) : 'elem =
    max_binding(s) |> Option.get_msg(->"Set.unsafe_max_binding", _)

  remove_max_binding(s : ordered_set('elem, 'order)) =
    pop_max_binding(s).f1
    : ordered_set('elem, 'order)

  subset(lowerbound, upperbound, (set : ordered_set('elem, 'order))) =
    lower = key -> Order.is_greatereq(key, lowerbound, order)
    upper = key -> Order.is_smallereq(key, upperbound, order)
    MapSet.sub_map_gen(lower, upper, set)
    : ordered_set('elem, 'order)

  greater(lowerbound, (set : ordered_set('elem, 'order))) =
    lower = key -> Order.is_greatereq(key, lowerbound, order)
    upper = _elem -> true
    MapSet.sub_map_gen(lower, upper, set)
    : ordered_set('elem, 'order)

  less(upperbound, (set : ordered_set('elem, 'order))) =
    lower = _elem -> true
    upper = key -> Order.is_smallereq(key, upperbound, order)
    MapSet.sub_map_gen(lower, upper, set)
    : ordered_set('elem, 'order)

  random_get(m:ordered_set(_,'order)) = Option.map(_.f1,MapSet.random_get(m))

}} : Set('elem, 'order)

/**
 * {1 Functions and modules exported to the global namespace}
 */

/**
 * The default [Set] module.
 *
 * Chances are that you will use this module for most tasks. It uses the default comparison.
 */
Set = @nonexpansive(Set_make(Order.default)) : Set('elem, Order.default)

/**
 * A [Set] on strings, using alphabetical comparison on strings.
 *
 * This instance of [Set] differentiates uppercase from lowercase.
 * Otherwise, the order between strings is alphabetical.
 */
StringSet = Set_make(String.order) : Set(string, String.order)

/**
 * A [Set] on numbers, using numeric comparison.
 */
IntSet    = Set_make(Int.order) : Set(int, Int.order)
