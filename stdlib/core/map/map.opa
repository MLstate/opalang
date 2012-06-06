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

import stdlib.core.iter

/**
 * Maps from keys to values, also known as dictionaries or hashmaps.
 *
 * @author David Rajchenbach-Teller 2010 (review, clean-up and documentation)
 */

/**
 * {1 About this module}
 *
 * This module defines maps (also known as "dictionaries", "hashmaps" and close to "hashtables" in other languages),
 * an immutable data structure designed to associate values to keys. Typically, a real-world dictionary associates
 * definitions to words -- in terms of maps, the words are keys and the definitions are values. A directory associates
 * phone numbers and directions (composing together the value) to names (the key), etc.
 *
 *
 * {1 Where should I start?}
 *
 * The most common cases of maps are [stringmap] (a map from [string]s to values of some type) and [intmap] (a map
 * from numbers to values of some type). For most other types of data, you can use type [map].
 * To create an empty map of either type, use [Map.empty]. To add elements,
 * use [Map.add], to remove elements use [Map.remove] and to find an element, use [Map.get]. That's it.
 *
 *
 * {1 What if I need more?}
 *
 * You may need to create your own kind of maps. For instance, perhaps you have a set of keys you wish to consider
 * as case-independent. Or perhaps you are using your maps with a type you have designed yourself and for which there
 * is no simple way of comparing that OPA can guess. In either case, you should use function [Map_make], which lets
 * you define new types of maps.
 *
 * If you are looking for a data structure to associate a value to each integer in a given range 0..n, you should
 * rather use [Array], which is heavily optimized for that specific case.
 */

/**
 * {1 Types defined in this module}
 */

/**
 * The type of maps.
 *
 * @param 'key The type of keys used to access this map. Typically, arrays use integers as keys, while dictionaries use strings.
 * @param 'val The type of values contained by this map.
 * @param 'order The ordering used to define this map.
 */
@opacapi
type map('key,'val) = ordered_map('key,'val, Order.default)
@opacapi
@abstract type ordered_map('key,'val,'order) = (Map_private.map('key,'val))

/**
 * Implementation of maps.
 */
type Map_private.map('key,'val) =
    { empty } /**The empty map*/
/   { left : Map_private.map('key,'val); key : 'key ; value : 'val ; right : Map_private.map('key,'val) ; height : int }
/** height contains the height of the tree including the current node, i.e. > 0
    left tree contains keys inferior to field key, and vice versa for right tree
*/

/**
 * A map from strings to values, using the default ordering on strings.
 */
@opacapi
type stringmap('b) = ordered_map(string, 'b, String.order)

/**
 * A map from integers to values, using the default ordering on integers.
 */
type intmap('b)    = ordered_map(int, 'b, Int.order)

/**
 * The signature of the module.
 *
 * All the maps support the functions defined in this signature.
 */
type Map('key,'order) =
     {{
        /**
         * {1 Constructors}
         */

        /**
         * The empty map.
         */
        empty : ordered_map('key,'val,'order)
        singleton: 'key, 'val -> ordered_map('key, 'val, 'order)
        is_empty: ordered_map('key,'val,'order) -> bool

        /**
         * {1 Adding and accessing}
         */

        /**
         * Add a key/value to a map.
         *
         * @param key The key added to the map. If a value already exists with this key, the value is replaced.
         * @param val The value added to the map.
         * @param m   The map to which to add.
         * @return A map containing all the elements of [m] plus the association of [val] to [key]. If [m] already
         * contained an association of some value to [key], this association is replaced.
         *
         * [add(key:'key, val:'val, m: ordered_map('key,'val,'order)): ordered_map('key,'val,'order)]
         */
        add: 'key, 'val, ordered_map('key,'val, 'order) -> ordered_map('key,'val, 'order)

        /**
         * Add a key/value to a map, but only if the key doesn't appear in the map yet.
         *
         * @param key The key added to the map. If a value already exists with this key, nothing happens.
         * @param val The value added to the map.
         * @param m   The map to which to add.
         * @return An optional map containing all the elements of [m] plus the association of [val] to [key]. If [m] already
         * contained an association of some value to [key], {none} is returned.
         */
        add_without_erasing: 'key, 'val, ordered_map('key,'val,'order) -> option(ordered_map('key,'val,'order))

        /**
         * Get the element associated to a given key.
         *
         * Performance note: if you are looping through a map, you should rather use the looping functions,
         * which are much faster than successive calls to [get].
         *
         * @param k A key in the map.
         * @param m A map, possibly empty.
         * @return [{none}] if no element is associated to [k] in [m]. Otherwise, [{some = v}], where [v] is the
         * latest value such that [add(k,v,m)] was called.
         */
        get: 'key, ordered_map('key,'val,'order) -> option('val)

        get_key_val:  'key, ordered_map('key,'val,'order) -> option({key:'key val:'val})

        /**
         * Find the first occurence validating a testing function [f]
         */
        find: ('key, 'val -> bool), ordered_map('key,'val,'order) -> option({key:'key val:'val})

        /**
         * {1 Loops}
         */

     /**
      * Loop through a map, collecting data from the map.
      *
      * This function implements a general-purpose loop on map.
      * It is extremely powerful and could be used
      * to reimplement most of the functions in this module.
      *
      * This function guarantees that all elements of the map will be visited.
      * The order in which elements are visited, however, is not specified.
      *
      * @param f A function invoked at each element of the
      * map to update the data.
      * @param l The map to visit.
      * @param The initial data. If the map is empty, this
      * will also be the result.
      */
        fold : ('key,'val,'acc -> 'acc), ordered_map('key,'val,'order), 'acc -> 'acc
        rev_fold: ('key,'val,'acc -> 'acc), ordered_map('key,'val,'order), 'acc -> 'acc
        foldi : (int,'key,'val,'acc -> 'acc), ordered_map('key,'val,'order), 'acc -> 'acc
        rev_foldi : (int,'key,'val,'acc -> 'acc), ordered_map('key,'val,'order), 'acc -> 'acc

        filter : ('key, 'val -> bool), ordered_map('key, 'val, 'order) -> ordered_map('key, 'val, 'order)
        filter_map : (('value -> option('new_value)), ordered_map('key, 'value, 'order) -> ordered_map('key, 'new_value, 'order))

     /**
      * Change all values of the map
      * by applying a change function to all values stored in the map
      *
      * @param f A function invoked at each element of the
      * map to change its value.
      * @param l The map to visit.
      */
        map : ('val -> 'new_val), ordered_map('key,'val,'order) -> ordered_map('key,'new_val,'order)

        mapi: ('key, 'val -> 'new_val), ordered_map('key,'val,'order) -> ordered_map('key,'new_val,'order)

        iter: ('key, 'val -> void), ordered_map('key,'val,'order) -> void

        min_binding: ordered_map('key,'val,'order) -> ('key, 'val)

        max_binding: ordered_map('key,'val,'order) -> ('key, 'val)

        /**
         * Determine if a key appears in a map.
         *
         * @param k A key.
         * @param m A map.
         * @return [{true}] if [m] contains a value associated to [k], [{false}] otherwise.
         */
        mem  : 'key, ordered_map('key,'val,'order) -> bool

        /**
         * {1 Information}
         */

        /**
         * @return the number of values in a map
         */
        size  : ordered_map('key,'val,'order)->int

        height: ordered_map('key, 'val, 'order) -> int
        /**
         * Compare two maps
         *
         * @return [{eq}] if the two maps have the same sets of keys (as compared by
         * the given comparison function) and each key is associated to the same value in
         * both maps.
         */
        compare :('val,'val -> Order.comparison), ordered_map('key,'val,'order), ordered_map('key,'val,'order)-> Order.comparison

        /**
         * Order two maps
         *
         * @return [{eq}] if the two maps have the same sets of keys (as compared by
         * the given ordering function) and each key is associated to the same value in
         * both maps.
         */
        order_maps :('val,'val -> Order.ordering), ordered_map('key,'val,'order), ordered_map('key,'val,'order)-> Order.ordering

        /**
         * Check if a predicate is satisfied in a map.
         *
         * @param f A condition on keys and values
         * @param m A map, possibly empty.
         * @return A bool indicating if at least one binding satisfy the condition
         */
        exists : ('key, 'value -> bool), ordered_map('key,'value,'order) -> bool;

        /**
         * Extract all the key/value pairs matching some condition.
         *
         * @param f A condition on keys and values.
         * @param m A map, possibly empty.
         * @return A list (possibly empty) of all key/value pairs [(k,v)] such that [f(k,v)] returns [{true}].
         * The order of elements in the list is not specified.
         */
        retrieve : ('key, 'val -> bool), ordered_map('key,'val,'order) -> list(('key,'val))
        replace: 'key, ('val -> 'val), ordered_map('key,'val,'order) -> ordered_map('key,'val,'order)
        replace_or_add: 'key, (option('val) -> 'val), ordered_map('key,'val,'order) -> ordered_map('key,'val,'order)

        /**
         * Merge two maps.
         *
         * Important note: if both maps have been defined with distinct orderings, the result of this function
         * is unspecified.
         *
         * @param m1 A map, possibly empty.
         * @param m2 A map, possibly empty.
         * @return A map containing all the key value pairs appearing either in [m1] or in [m2]. If a key appears
         * in both maps with distinct values, one of the values is chosen arbitrarily.
         */
        union: ordered_map('key,'val,'order), ordered_map('key,'val,'order) -> ordered_map('key,'val,'order)
        intersection: ordered_map('key,'val,'order), ordered_map('key,'val,'order) -> ordered_map('key,'val,'order)

        /**
         * {1 Changing the contents of a map}
         *
         * Note that maps are immutable. In other words, every function that changes the contents of a map
         * actually returns a new map, without truly altering the contents of the old map. Maps are implemented
         * to ensure that this will not cause memory or database waste, e.g. all the unchanged parts of the map
         * are shared between the old map and the new map.
         */

          /**
           * Get and remove the element associated to a key in a map.
           *
           * @param k A key, possibly in the map.
           * @param m A map.
           * @return [({none}, m)] if the map doesn't contain any element associated to [k]. Otherwise,
           * [(r, {some = v})], where [v] is the element associated to [k] and [r] is the map obtained by
           * removing key [k]. .
           */
        extract: 'key, ordered_map('key,'val,'order) -> ( ordered_map('key,'val,'order), option('val) )

        /**
         * Get and remove any element from a map.
         *
         * @param m A map, possibly empty.
         * @return [(m, {none})] if the map was empty. Otherwise, [(r, {some = (k,v)})], where [(k,v)] is
         * a key/value pair extracted from [m] and [r] is the map obtained by removing [k] from [m].
         */
        extract_min_binding: ordered_map('key,'val,'order) -> (ordered_map('key, 'val,'order), option(('key, 'val)))
        extract_max_binding: ordered_map('key,'val,'order) -> (ordered_map('key, 'val,'order), option(('key, 'val)))
        /**
         * Get a random key,element couple from the map
         * Picking distribution is approximately uniform
         */
        random_get : ordered_map('key,'val,'order) -> option( ('key,'val) )
        sub_map_gen: ('key -> bool), ('key -> bool), ordered_map('key,'val,'order) -> ordered_map('key, 'val, 'order)
        submap: 'key, 'key, ordered_map('key, 'val, 'order) -> ordered_map('key, 'val, 'order)
        greater: 'key, ordered_map('key, 'val, 'order) -> ordered_map('key, 'val, 'order)
        lesser: 'key, ordered_map('key, 'val, 'order) -> ordered_map('key, 'val, 'order)

        /**
         * Remove an element from a map.
         *
         * @param k A key, possibly in the map.
         * @param m A map.
         * @return The same map, in which [k] is associated to no element. If [k] wasn't associated to any
         * value in the first place, this is the same map.
         */
        remove : 'key, ordered_map('key,'val,'order) -> ordered_map('key,'val,'order)


        /**
         * {1 Conversions}
         */

         /**
          * Converting from other data structures to maps.
          */
         From :
              {{
                  /**
                   * Convert a list of key/value pairs to a map.
                   *
                   * If the same key appears twice (or more) in the list, any occurrence of they after the first is discarded.
                   *
                   * @param l A list of key/value pairs.
                   * @return A map containing all the keys of [l]. Each key [k] is associated to the first value of to which
                   * it is also associated in [l].
                   */
                  assoc_list :   list( tuple_2('key,'val) ) -> ordered_map('key,'val,'order)
              }}

         /**
          * Converting from maps to other data structures.
          */
         To :
            {{
                   /**
                    * Convert a map to a list of key/values.
                    *
                    * @param m A map.
                    * @return A list containing all the (keys/value) pairs in [m], in no specific order.
                    */
                   assoc_list :  ordered_map('key,'val,'order) ->  list( tuple_2('key,'val) )

                   /**
                    * Convert a map to a list of keys.
                    *
                    * @param m A map.
                    * @return A list containing all the keys pairs in [m], in no specific order.
                    */
                   key_list   :  ordered_map('key,'val,'order) ->  list( 'key )

                   /**
                    * Convert a map to a list of values.
                    *
                    * @param m A map.
                    * @return A list containing all the values pairs in [m], in no specific order.
                    */
                   val_list   :  ordered_map('key,'val,'order) ->  list( 'val )

                   iter: ordered_map('key, 'val, 'order) -> iter(('key, 'val))
                   rev_iter: ordered_map('key, 'val, 'order) -> iter(('key, 'val))
            }}

    }}


Map_private =
{{
  empty = { empty }

  singleton(key : 'key, value : 'val) =
    { left = empty ; key = key ; value = value ;
      right = empty ; height = 1 } : Map_private.map('key, 'val)

  height(m : Map_private.map('key, 'val)) =
    match m with
       { empty } -> 0
     | { ~height left = _ key = _ value = _ right = _ } -> height

  node(l : Map_private.map('key, 'val), k, v,
       r : Map_private.map('key, 'val), h) =
    { left = l key = k value = v right = r height = h } :
    Map_private.map('key, 'val)

  create(l : Map_private.map('key, 'val), k, v,
         r : Map_private.map('key, 'val)) =
    hl = height(l)
    hr = height(r)
    h = max(hl, hr) + 1
    node(l, k, v, r, h) : Map_private.map('key, 'val)

  bal(l : Map_private.map('key, 'val), x, d, r : Map_private.map('key, 'val)) =
    hl = height(l)
    hr = height(r)
    if hl > hr + 2 then
      match l with
      | { ~left ~key ~value ~right ... } ->
          if height(left) >= height(right) then
            create(left, key, value, create(right, x, d, r))
          else
            match right with
            | { left = rleft ; key = rkey ; value = rvalue ;
                right = rright ; ... } ->
                create(create(left, key, value, rleft), rkey, rvalue,
                       create(rright, x, d, r))
            | { empty } -> empty
            end
       | { empty } -> empty
    else
      if hr > hl + 2 then
        match r with
        | { ~left ~key ~value ~right ... } ->
            if height(left) <= height(right) then
              create(create(l, x, d, left), key, value, right)
            else
              match left with
              | { left = rleft ; key = rkey ; value = rvalue ;
                  right = rright ; ... } ->
                  create(create(l, x, d, rleft), rkey, rvalue,
                         create(rright, key, value, right))
              | { empty } -> empty
              end
       | { empty } -> Map_private.empty
      else create(l, x, d, r) : Map_private.map('key, 'val)

  /* FIXME: add an assert(t1<t2) */
  /**
   * Be careful : this function concatenate two maps
   * without removing bindings having the same key
   */
  concat(t1:Map_private.map('key, 'val),t2:Map_private.map('key, 'val)) =
    match (t1,t2) with
    | ({empty},t      ) -> t
    | (   t   ,{empty}) -> t
    // nb: we could use simply _ below, but new typer goes crazy (bug #56)
    | ({ left=_ ; key=_ ; value=_ ; right=_ ; height=_ }, { left=_ ; key=_ ; value=_ ; right=_ ; height=_ }) ->
        (left, right) = min_binding(t2)
        bal( t1, left, right , remove_min_binding(t2) )


  min_binding(m : Map_private.map) =
   rec aux(a_map : Map_private.map) =
     match a_map with
      | { left = ({ empty } : map) ~key ~value right = _ ... } -> (key, value)
      | { ~left key = _ value = _ right = _ ... } -> aux(left)
      | { empty } -> error("Map.min_binding: Not Found")
    aux(m)

  max_binding(m : Map_private.map) =
   rec aux(a_map : Map_private.map) =
     match a_map with
      | { left = _ ~key ~value right = ({ empty } : map) ... } -> (key, value)
      | { left = _ key = _ value = _ ~right ... } -> aux(right)
      | { empty } -> error("Map.max_binding: Not Found")
    aux(m)

  remove_min_binding(m : Map_private.map) =
    rec aux(a_map : Map_private.map) =
      match a_map with
       | { empty = _ } -> empty
       | { left = { empty } key = _ value = _ ~right ... } -> right
       | { ~left ~key ~value ~right ... } -> bal(aux(left), key, value, right)
    aux(m)

  map(f:('val -> 'new_val),m:Map_private.map('key,'val)):Map_private.map('key,'new_val)=
    match m:Map_private.map
    {~left ~key ~value ~right ~height } -> node(map(f,left),key,f(value),map(f,right), height)
    {empty}                             -> empty

  add(order: order('key,'order), x,data,m) =
    rec aux(m:Map_private.map('key, 'val)) =
        match m
        | { empty } -> singleton(x,data):Map_private.map('key, 'val)
        | { ~left ~key ~value ~right ~height } ->
            match Order.compare(x,key,order) with
             | {eq} -> node(left,x,data,right,height)
             | {lt} -> bal( aux(left) , key, value, right)
             | {gt} -> bal(left,key,value,aux(right))
    aux(m)


  fold((f:('key, 'val, 'acc -> 'acc)), m:Map_private.map('key, 'val), acc:'acc) : 'acc = match m
    | { empty } -> acc:'acc
    | ~{ left key:'key value:'val right height=_ } ->
      fold(f, right, f(key, value, fold(f, left, acc)))

  rev_fold(f:('key, 'val, 'acc -> 'acc), m:Map_private.map('key, 'val), acc:'acc) : 'acc = match m
    | { empty } -> acc
    | { ~left ~key ~value ~right ... } ->
        rev_fold(f, left, f(key, value, rev_fold(f, right, acc)))

  foldi((f:(int, 'key, 'val, 'acc -> 'acc)), m:Map_private.map('key, 'val), acc:'acc) : 'acc =
    rec aux(m, acc, cpt) =
      match m
      | { empty } -> (acc:'acc, cpt)
      | ~{ left key:'key value:'val right height=_ } ->
        (acc, cpt) = aux(left, acc, cpt)
        aux(right, f(cpt+1, key, value, acc), cpt+1)
    aux(m, acc, 0).f1

  rev_foldi((f:(int, 'key, 'val, 'acc -> 'acc)), m:Map_private.map('key, 'val), acc:'acc) : 'acc =
    rec aux(m, acc, cpt) =
      match m
      | { empty } -> (acc:'acc, cpt)
      | ~{ left key:'key value:'val right height=_ } ->
        (acc, cpt) = aux(right, acc, cpt)
        aux(left, f(cpt+1, key, value, acc), cpt+1)
    aux(m, acc, 0).f1

  filter(order: order('key, _), f: ('key, 'val -> bool), m: Map_private.map('key, 'val)) : Map_private.map('key, 'val) =
    fold(
      (k, v, acc -> if f(k, v) then add(order, k, v, acc) else acc),
      m, empty
    )

  filter_map(order:order('key,_), f:('a -> option('b)), m:Map_private.map('key, 'a)) : Map_private.map('key,'b) =
    fold(
      (k, v, acc ->
        Option.switch(add(order, k, _, acc), acc, f(v))
      ), m, empty
    )


  mapi(f:('key, 'val -> 'new_val),m:Map_private.map('key,'val)) : Map_private.map('key,'new_val) =
    match m:Map_private.map
    {~left ~key ~value ~right ~height } -> node(mapi(f,left),key,f(key, value),mapi(f,right), height)
    {empty}                             -> empty


   to_iter(m, cont) =
        match m : Map_private.map with
         | { empty } -> cont
         | { ~left ~key ~value ~right ... } ->
            to_iter(left, { next() = some(((key, value), to_iter(right, cont))) } : iter)

   to_rev_iter(m, cont) =
        match m : Map_private.map with
         | { empty } -> cont
         | { ~left ~key ~value ~right ... } ->
           to_rev_iter(right,
           { next() = some( ((key, value), to_rev_iter(left, cont))) } : iter)


  random_get(m) =
    match m : Map_private.map
    | { empty } -> none
    | { ~left ~key ~value ~right ~height } ->
        approx_size = Bitwise.lsl(1,height/2)+1 // odd
        mid = approx_size/2 // the middle index
        i=Random.int(approx_size)
        // take left or right randomly if the current is not chosen
        r=
          if      i < mid then random_get(left)
          else if i > mid then random_get(right)
          else none
        // if it worked ok otherwise return current node
        match r
        {some=_}-> r
        {none}  -> some( (key,value) )

}}



/**
 * Create a module map from a specialized comparison function.
 *
 * @param compare_key A total order on keys.
 * @return A module defining all the functions specified in type [Map].
 */

// Rudy : This module is a mess, all implementations should go in Map_private, here should be only Map_private calls, wrap, unwrap and coercions
Map_make(order: order('key,'order) ) : Map =
{{
  empty:ordered_map('key, 'val, 'order) = { empty }

  is_empty(map:ordered_map('key, 'val, 'order)):bool=
      match map
      {empty} -> true
      _ -> false

  singleton(key:'key,value:'val):ordered_map('key, 'val, 'order) = Map_private.singleton(key, value)

  height(m:ordered_map('key, 'val, 'order)):int = Map_private.height(m)

  size(m:ordered_map('key, 'val, 'order)):int =
    rec aux(m_aux : Map_private.map('key, 'val)) =
      match m_aux with
      | { empty } -> 0
      | { ~left ~right value=_ key=_ height=_ } -> 1+aux(left)+aux(right)
    aux(m)

  find(f:('key, 'value -> bool), m:ordered_map('key,'val,'order)) : option({key:'key val:'val}) =
    rec aux(m:Map_private.map('key, 'val)) =
      match m with
      | { empty } -> none
      | { ~left ~key ~value ~right ... } ->
        match aux(left)
        | {some=res} -> some(res)
        | {none} ->
          if f(key, value) then some(~{key val=value})
          else aux(right)
    aux(m)

  get(x:'key, m:ordered_map('key,'val,'order)) : option('val) =
    rec aux(m:Map_private.map('key, 'val)) =
      match m with
      | { empty } -> none
      | { ~left ~key ~value ~right ... } ->
          match Order.compare(x,key,order) with
           | {eq} -> some(value)
           | {lt} -> aux(left)
           | {gt} -> aux(right)
    aux(m)

  get_key_val(x:'key, m:ordered_map('key,'val,'order)) : option({key:'key val:'val}) =
    rec aux(m:Map_private.map('key, 'val)) =
      match m with
      | { empty } -> none
      | { ~left ~key ~value ~right ... } ->
          match Order.compare(x,key,order) with
           | {eq} -> some(~{key val=value})
           | {lt} -> aux(left)
           | {gt} -> aux(right)
    aux(m)

  add(x:'key,data,m:ordered_map('key,'val,'order)) =
    rec aux(m:Map_private.map('key, 'val)) =
        match m
        | { empty } -> Map_private.singleton(x,data):Map_private.map('key, 'val)
        | { ~left ~key ~value ~right ~height } ->
            match Order.compare(x,key,order) with
             | {eq} -> Map_private.node(left,x,data,right,height)
             | {lt} -> Map_private.bal( aux(left) , key, value, right)
             | {gt} -> Map_private.bal(left,key,value,aux(right))
    aux(m)
    : ordered_map('key,'val,'order)

  add_without_erasing(x:'key,data:'val,m:ordered_map('key,'val,'order)): option(ordered_map('key,'val,'order)) =
    rec aux(m:Map_private.map('key, 'val)) = match m
    | { empty } -> some(Map_private.singleton(x,data))
    | { ~left ~key ~value ~right height=_ } ->
        match Order.compare(x,key,order) with
          | {eq} -> {none}
          | {lt} -> match aux(left)
              | {none}     -> {none} //Nothing inserted, don't rebuild the map
              | {some = x} -> some(Map_private.bal( x , key, value, right))
            end
          | {gt} -> match aux(right)
              | {none}     -> {none} //Nothing inserted, don't rebuild the map
              | {some = x} -> some(Map_private.bal(left,key,value,x))
            end
    //match
    aux(m)
    //   | {none} -> m
    //   | {some = x} -> x


  remove(x:'key,m:ordered_map('key, 'val, 'order)) :
        ordered_map('key, 'val, 'order) =
    rec aux(m : Map_private.map('key, 'val)) =
      match m
        | { empty = _ } -> Map_private.empty
        | { ~left ~key ~value ~right ... } ->
          match Order.compare(x, key, order) with
            | {eq} -> Map_private.concat(left, right)
            | {lt} -> Map_private.bal(aux(left), key, value, right)
            | {gt} -> Map_private.bal(left,key,value , aux(right))
          end
    aux(m) : ordered_map('key, 'val, 'order)
/*  update(x,f,m)=
   match m : map with
    | { empty = _ } -> match f(none)
                            {some}->singleton(some)// ADD
                            {none}->empty // EQUAL
    | { left ; key ; value ; right ; height } ->
        c = compare_key(x,key)
        if c == 0 then
            match f(some(value))
                  {some}-> { left ; key ; some ; right ; height; } // MAP
                  {none}-> Map_private.concat(left,right) // ADD
        else if c < 0 then bal( update(x,f,m) , key, value, right)
        else               bal(left,key,value , update(x,f,right)  ) */


  /*add(x,data,m)=update(x, (_->some(data)), m)
  remove(x,m)=update(x, (_->none), m)*/



  union(m1:ordered_map('key,'val,'order), m2:ordered_map('key,'val,'order)):ordered_map('key,'val,'order)  =
      fold(add, m1:ordered_map('key,'val,'order), m2:ordered_map('key,'val,'order))

//  add_map(k:list, data:map(string,'b), m:map(list, map(string, 'b))) =

  retrieve(f:('key, 'val -> bool),m:ordered_map('key,'val,'order))=fold( (k,v,acc-> if f(k,v) then List.add((k,v),acc) else acc) , m, [])

  replace(x:'key, f:('val -> 'val), m : ordered_map('key,'val,'order)) :
         ordered_map('key,'val,'order) =
    rec aux(m) = match m
    | { empty } -> (m : Map_private.map('key, 'val))
    | { ~left ~key ~value ~right ... } ->
        match Order.compare(x,key,order) with
         | {eq} -> Map_private.create(left, key, f(value), right)
         | {lt} -> Map_private.create(aux(left), key, value, right)
         | {gt} -> Map_private.create(left, key, value, aux(right))
     aux(m) : ordered_map('key,'val,'order)

  replace_or_add(x : 'key, f : (option('val) -> 'val),
                 m : ordered_map('key,'val,'order)) :
                ordered_map('key,'val,'order) =
    rec aux(m : Map_private.map('key, 'val)) =
    match m with
      | { empty = _ } ->
        Map_private.create(Map_private.empty, x, f(none), Map_private.empty)
      | { ~left ~key ~value ~right ... } ->
          match Order.compare(x,key,order) with
            | { eq } -> Map_private.create(left, key, f(some(value)), right)
            | { lt } -> Map_private.create(aux(left), key, value, right)
            | { gt } -> Map_private.create(left, key, value, aux(right))
     aux(m) : ordered_map('key,'val,'order)

  mem(x :'key, m : ordered_map('key, 'val, 'order)) : bool =
    rec aux(map_aux : Map_private.map('key, 'val)) =
      match map_aux with
      | { empty } -> false
      | { ~left ~key ~right ; ... } ->
         match Order.compare(x,key,order) with
           | { eq } -> { true }
           | { lt } -> aux(left)
           | { gt } -> aux(right)
    aux(m)

  exists(f, m:ordered_map('key, 'val, 'order)) : bool =
    rec aux(map_aux : Map_private.map('key, 'val)) =
      match map_aux with
      | { empty } -> false
      | { ~left ~key ~right ~value ; ... } ->
        satisfy = f(key, value)
        satisfy || aux(left) || aux(right)
    aux(m)

  rev_fold(f : 'key, 'val, 'acc ->
                 'acc, m : ordered_map('key, 'val, 'order),
                  acc : 'acc) : 'acc =
    Map_private.rev_fold(f, m, acc)

  fold(f : 'key, 'val, 'acc ->
             'acc, m : ordered_map('key, 'val, 'order), acc : 'acc) =
    Map_private.fold(f, m, acc) : 'acc

  foldi(f : int, 'key, 'val, 'acc ->
             'acc, m : ordered_map('key, 'val, 'order), acc : 'acc) =
    Map_private.foldi(f, m, acc) : 'acc

  rev_foldi(f : int, 'key, 'val, 'acc ->
             'acc, m : ordered_map('key, 'val, 'order), acc : 'acc) =
    Map_private.rev_foldi(f, m, acc) : 'acc

  map(f : ('val -> 'new_val), m : ordered_map('key, 'val, 'order)) :
     ordered_map('key, 'new_val, 'order) =
    Map_private.map(f,m)

  filter(f : 'key, 'val -> bool, m : ordered_map('key, 'val, 'order)) : ordered_map('key, 'val, 'order) =
    Map_private.filter(order, f, m)

  filter_map(f : ('a -> option('b)), m : ordered_map('key, 'a, 'order)) :
            ordered_map('key, 'b, 'order) =
    Map_private.filter_map(order, f, m)

  mapi(f : ('key, 'val -> 'new_val), m : ordered_map('key, 'val, 'order)) =
    Map_private.mapi(f, m) : ordered_map('key, 'new_val, 'order)

  iter(f, m : ordered_map('key, 'val, 'order)) =
    fold((k, v, _acc -> f(k, v)), m, void)

  min_binding(m : ordered_map('key, 'val, 'order)) =
    Map_private.min_binding(m)

  max_binding(m : ordered_map('key, 'val, 'order)) =
    Map_private.max_binding(m)

//  remove_min_binding(m) = @wrap(_remove_min_binding(@unwrap(m)))


  extract(x : 'key, m : ordered_map('key, 'val, 'order)) :
         (ordered_map('key, 'val, 'order), option('val)) =
    rec aux(a_map : Map_private.map('key, 'val)) =
      match a_map with
        | { empty = _ } -> (Map_private.empty, none)
        | { ~left ~key ~value ~right ... } ->
            match Order.compare(x, key, order) with
             | { eq } -> (Map_private.concat(left, right), some(value))
             | { lt } ->
                    (left, val) = aux(left)
                    (Map_private.bal(left, key, value, right), val)
             | { gt } ->
                    (right,val) = aux(right)
                    (Map_private.bal(left, key, value, right), val)
            end
    (map, result) = aux(m)
    (map, result)

  extract_min_binding(m : ordered_map('key, 'val, 'order)) :
                     (ordered_map('key, 'val, 'order), option(('key, 'val))) =
  /*Algorithm:
    - if the tree is empty, return [({empty}, none)]
    - if there's no left branch, we are seeing the smallest binding, extract it
    - otherwise, enter left branch, produce result, rebalance.*/
  rec aux(a_map : Map_private.map('key, 'val)) =
     match a_map with
      | { empty = _ } -> (Map_private.empty, none)
      | { left = { empty } ~right ~key ~value ... } ->
          (right, some((key, value)))
      | { ~left ~key ~value ~right height=_} ->
          (left, val) = aux(left)
          (Map_private.bal(left, key, value, right), val)
  (tree : Map_private.map, val) = aux(m)
  (tree, val) : (ordered_map, option)


  extract_max_binding(m : ordered_map('key, 'val, 'order)) :
                     (ordered_map('key, 'val, 'order), option(('key, 'val))) =
    rec aux(a_map : Map_private.map('key, 'val)) =
      match a_map with
        | { empty = _ } -> (Map_private.empty,none)
        | { ~left right = { empty } ~key ~value ... } ->
            (left, some((key, value)))
        | { ~left ~key ~value ~right height = _ } ->
            (right, val) = aux(right)
            (Map_private.bal(left, key, value, right), val)
    (tree : Map_private.map, val) = aux(m)
    (tree, val) : (ordered_map, option)



  random_get(m:ordered_map('key,'val,'order)) = Map_private.random_get(m) : option(('key,'val))

  sub_map_gen(min_key: 'key -> bool,
          max_key: 'key -> bool,
          map: ordered_map('key,'val,'order)) =
      rec aux(a_map : Map_private.map('key, 'val)) =
      match a_map with
       | { empty } -> (Map_private.empty, 0)
       | { ~left ~key ~value ~right height = _ } ->
           leftneeded  = min_key(key) ;   //TODO: check this
           rightneeded = max_key(key) ;
           if (leftneeded) then
              if (rightneeded) then
                 tmpl = aux(left) ;
                 tmpr = aux(right) ;
                 (Map_private.bal(tmpl.f1, key, value, tmpr.f1),
                  tmpl.f2 + tmpr.f2 + 1)
              else
                 aux(left)
           else
              if (rightneeded) then
                 aux(right)
              else
                 (Map_private.empty, 0)
      aux(map).f1 : ordered_map('key, 'val, 'order)



   submap(lowerbound, upperbound, map) =
       lower = key -> Order.is_greatereq(key, lowerbound, order)
       upper = key -> Order.is_smallereq(key, upperbound, order)
       sub_map_gen(lower, upper, map)
       : ordered_map('key, 'val, 'order)

   greater(lowerbound, map) =
      lower = key -> Order.is_greatereq(key, lowerbound, order)
      upper = _key -> true
      sub_map_gen(lower, upper, map)
      : ordered_map('key, 'val, 'order)

  lesser(upperbound, map) =
      lower = _key -> true
      upper = key -> Order.is_smallereq(key, upperbound, order)
      sub_map_gen(lower, upper, map) : ordered_map('key, 'val, 'order)



  /**
   * Intersection of two maps.
   * Return a map, possibly empty.
   *
   * Algorithm : cover the smallest map and verify if each key exists
   * in the second map, then add the binding to a result map.
   * If a key appears in both maps with distinct values, one of the values
   * is chosen arbitrarily.
   *
   * @TODO optimize
   */
  intersection(map1, map2) =
    rec aux(m1: Map_private.map('key, 'val),
            m2: ordered_map('key, 'val, 'orded),
            acc: ordered_map('key, 'val, 'order)) =
      match m1 with
        | { empty } -> acc
        | { ~left ~key ~value ~right height = _ } ->
            acc = aux(left, m2, acc)
            acc = aux(right, m2, acc)
            if mem(key : 'key, m2) then add(key : 'key, value : 'val, acc)
            else acc

    if is_empty(map1) || is_empty(map2) then empty
    else
      if height(map1) < height(map2) then aux(map1, map2, empty)
      else aux(map2, map1, empty)


  From = {{
     assoc_list(l) =
       List.fold(((k, v), acc -> add_without_erasing(k, v, acc) ? acc) ,
                 l,
                 empty : ordered_map('key, 'val, 'order)) :
       ordered_map('key, 'val, 'order)
  }}

  To = {{
      iter(m : ordered_map('key, 'val, 'order)) =
        Map_private.to_iter(m, Iter.empty)

      rev_iter(m : ordered_map('key, 'val, 'order)) =
        Map_private.to_rev_iter(m, Iter.empty)

      assoc_list(m : ordered_map('key, 'val, 'order)) =
        rev_fold(k, v, acc-> List.cons((k, v), acc) , m, List.empty)

      key_list(m : ordered_map('key, 'val, 'order)) =
        rev_fold(k, _v, acc-> List.cons(k, acc), m, List.empty)

      val_list(m : ordered_map('key, 'val, 'order)) =
        rev_fold(_k, v, acc-> List.cons(v, acc) , m, List.empty)
  }}

  compare(cmp:'val,'val -> Order.comparison, m1: ordered_map('key, 'val, 'order), m2: ordered_map('key, 'val, 'order)): Order.comparison =
    rec verif(it1 : iter, it2 : iter) =
      match (it1.next(), it2.next()) with
        | ({ none }, { none }) -> { eq }
        | ({ some = (v1, it1)}, { some = (v2, it2) }) ->
          match Order.compare(v1.f1, v2.f1, order) with
             | { eq } ->
               match cmp(v1.f2, v2.f2) with
                 | { eq } -> verif(it1, it2)
                 | c -> c
               end
             | { lt } -> { lt }
             | { gt } -> { gt }
          end
        | ({ none }, _) -> { lt }
        | (_, { none }) -> { gt }
      verif(To.iter(m1), To.iter(m2))

  order_maps(cmp:'val,'val -> Order.ordering, m1: ordered_map('key, 'val, 'order), m2: ordered_map('key, 'val, 'order)): Order.ordering =
     rec verif(it1:iter, it2:iter)=
       match (it1.next(), it2.next()) with
         | ({none},{none})        -> {eq}
         | ({some=(v1,it1)},{some=(v2,it2)})->
           match Order.compare(v1.f1,v2.f1,order) with
              | {eq} ->
                match cmp(v1.f2, v2.f2) with
                  | {eq} -> verif(it1,it2)
                  |  c   -> c
                end
              | {lt} -> {lt}
              | {gt} -> {gt}
           end
         | ({none}, _) -> {lt}
         | (_, {none}) -> {gt}
       verif(To.iter(m1), To.iter(m2))



}} //: Map_make

@stringifier(ordered_map('key, 'val, 'order)) map_to_string(k2s, v2s, _o2s, map) =
  tx = Map.fold(key, val, tx ->
    Text.insert_right(tx, k2s(key)) |>
    Text.insert_right(_, " => ")    |>
    Text.insert_right(_, v2s(val))  |>
    Text.insert_right(_, "\n")
    , map, Text.cons(""))
  Text.to_string(tx)

/**
 * {1 Functions and modules exported to the global namespace}
 */

/**
 * A [Map] on numbers, using numeric comparison.
 */
IntMap    = Map_make(Int.order):Map(int, Int.order)

/**
 * A [Map] on strings, using alphabetical comparison on strings.
 *
 * This instance of [Map] differentiates uppercase from lowercase.
 * Otherwise, the order between strings is alphabetical.
 */
StringMap = Map_make(String.order):Map(string, String.order)

/**
 * The default [Map] module.
 *
 * Chances are that you will use this module for most tasks. It uses the default comparison.
 */
Map = @nonexpansive(Map_make(Order.default)): Map('key, Order.default)



@opacapi IntMap_empty = IntMap.empty
@opacapi StringMap_empty = StringMap.empty

@opacapi IntMap_add = IntMap.add
@opacapi StringMap_add = StringMap.add

@opacapi IntMap_fold = IntMap.fold
@opacapi StringMap_fold = StringMap.fold
