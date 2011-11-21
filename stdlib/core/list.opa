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
/*
    @authors ?
**/

/**
 * Lists and utilities to manipulate them.
 *
 * @author David Rajchenbach-Teller 2010 (review, clean-up and documentation)
 */

/**
 * {1 Types defined in this module}
 */

/**
 * The type of lists
 * @param 'a The type of list elements
 *
 * {[{nil}} The empty list. As a syntactic shortcut, you can also write [[]].
 * {[{ hd ; tl }} A list with at least one element.
 * You can write equivalently [[1,2,3]] or
 * [ {hd = 1 ; tl = {hd = 2 ; tl = {hd = 3 ; tl = {nil} } } } ]
**/
@opacapi
type list('a) =
   { nil }
 / { hd : 'a ; tl : list('a) }

/**
 * {1 About this module}
 *
 * This module defines lists (also known as "linked lists"), an immutable data structure meant
 * to contain finite or infinite sets of elements of the same type.
 *
 *
 * {1 Where should I start?}
 *
 * To create a list containing elements 1, 2 and 3, the easiest is to write [[1,2,3]].
 * The empty list is [[]]
 * Once you have a list, typically, there may be a few things you wish to do with it:
 *
 *  {2 Transforming lists}
 *
 * Do you have a list of numbers you wish to double? Try {!List.map}. Or perhaps you only wish to keep
 * odd numbers? Look at {!List.filter}. Or perhaps you wish to keep this in odd positions? Look at
 * {!List.filteri}.
 *
 *
 *  {2 Collecting list data}
 *
 * Do you wish to sum all the elements of a list? Try {!List.reduce}. Or perhaps you only wish to find
 * the first even number? Try {!List.find}. Or perhaps you wish to compute its length? Try {!List.length}.
 *
 *  {2 Sorting a list}
 *
 * To sort a list, use {!List.sort}. This function can adapt to your sorting criteria.
 *
 * {1 Limitations of this module}
 *
 * {1 What if I need more?}
 *
 * This module contains numerous other functions, including the all-powerful {!List.foldi}, which will
 * let you apply arbitrary collections/transformations/loops based on the contents of a list.
 *
 * Lists are immutable, as are all base data structures in OPA -- this lets the compiler optimize numerous
 * operations, including concurrency and database access. If you need mutability, you should consider wrapping
 * your list in a {!Session}.
 *
 * List provide linear access time, i.e. don't use them if you frequently need to add data at the end
 * or if you frequently need to access the [n]th element. For this purpose, you should rather use module
 * [Array].
 */
List = {{

  /**
   * {1 Constructors}
  **/

  /**
   * The empty list
   *
   * Provided for compatibility with other collections.
   */
  empty = { nil } : list('a)

  /**
   * An alias for the empty list
   */
  nil = empty : list('a)

  /**
   * Add an element at the start of a list
   *
   * [List.cons(e,l)] returns the list whose first element is [e] and in which the rest of the list is [l]
   * You may also use the syntactic shorthands [[ e | l ]]
   *
   * You can combine several call to [cons] for creating a list with more elements,
   * [ List.cons(0, List.cons(1, tl)) ] is equivalent to [[ 0, 1 | tl ]]
  **/
  cons(e:'a, l:list('a) ) = { hd = e ; tl = l } : list('a)

  /**
   * An alias for {!List.cons}
   *
   * Provided for compatibility with other collections.
   */
  add = cons

  /**
   * A add in a list ordered by a function of comparison.
   * Not tail rec, used for small list only
   * in this case, this is more efficient than a Set.
  **/
  add_uniq(cmp: ('a, 'a -> Order.ordering), a, list) =
    rec aux(list) =
      match list with
      | [] -> [a]
      | [hd | tl] ->
        match cmp(a, hd) with
          | {eq} -> list
          | {lt} -> [a | list]
          | {gt} -> [ hd | aux(tl) ]
        end
    aux(list)

   /**
    * As {!List.cons}: adding an element at the head of a list.
   **/
  `+>` = cons


  /**
   * Create a list of [n] elements from a function.
   *
   * @param f A function, which will provide the contents of the list.
   * @param n The size of the result. If 0 or less, the result is empty.
   *
   * This function produces [[f(0), f(1), ... , f(n - 1)]].
   *
   * Note: This function does not guarantee in which order [f(0)], [f(1)], etc.
   * are called, so you should not use it if the order is important for the
   * behavior of your program. In such a case, use {!List.init_stable} (or {!List.rev_init}).
   */
  init(f: int -> 'a, n: int) =
    rec aux(i,acc) =
       if i < 0 then acc else aux(i - 1, cons(f(i), acc))
    aux(n-1,[]) : list('a)

  /**
   * A variant on {!List.init}.
   * Slower but guarantees order of function calls.
  **/
  init_stable(f:int -> 'a, n: int) = (rev_init(f,n) |> rev) : list('a)

  /**
   * As {!List.init} but in the reverse order.
  **/
  rev_init(f: int -> 'a, n: int) =
    rec aux(i,acc) =
       if i == n then acc else aux(i+1, cons( f(i) , acc))
    aux(0,[]): list('a)

  /**
  * [List.iterate(f, x, n)] builds [x, f(x), f(f(x)), ..., f(...f(x))] of length [n].
  * (so the last element of the list has [n - 1] applications of [f])
  */
  iterate(f: 'a -> 'a, init: 'a, n: int) =
    rec aux(i, x, l) =
       if i <= 0 then (i, x, l) else aux(i - 1, f(x), cons(x, l))
    (_, _, res) = aux(n, init, [])
    rev(res) : list('a)

  /**
   * Concatenate two lists.
   *
   * Performance note: if you have a list you often need to extend at the end, rather than {!List.append},
   * you should consider maintaining a list of lists, which will only be flattened once
   * using {!List.flatten}.
   *
   * @param l1 The beginning.
   * @param l2 The end.
   */
  append( l1: list('a), l2: list('a)) =
    match l2
    [] -> l1
    _ -> rev_append(rev(l1), l2) : list('a)


  /**
   * {2 Access functions}
  **/

  /**
   * Determine if a list is empty.
   *
   * @return [{true}] if the list is empty, [{false}] otherwise.
   */
  is_empty(l) =
    match l with
    | [] -> true
    | _ -> false

  /**
   * Compute the length of a list.
   *
   * Performance note: if you only need to check whether a list is empty, you should rather use
   * {!List.is_empty}, which is much faster than {!List.length}, in particular when your list is stored in
   * the database.
   *
   * @return The number of elements in the list, which is [0] if the list is [{nil}] or a strictly
   * positive number otherwise..
   */
  length(l) =
    rec aux( len, l ) =
      match l with
      | [] -> len
      | [_ | tl] -> aux(succ(len), tl)
    aux(0,l): int

  /**
   * Check that the list has the given length
   *
   * More efficient that computing the length of the list and then comparing
   * against the desired length, because this function returns false earlier
   * if the list is very long
   */
   check_length(l:list,n:int) : bool =
     if n == 0 then
       match l with
       | [] -> true
       | _ -> false
     else
       match l with
       | [] -> false
       | [_ | t] -> check_length(t,n-1)

  /**
   * Get the [nth] element of a list.
   *
   * Performance note: if you are looping through a list, you should rather use the looping functions,
   * which are much faster than calling [get(0,l)], [get(1,l)], ... [get(n,l)].
   *
   * @param n The index of an element of the list. The first element of the list is numbered [0].
   * @return [{none}] if the list contains less than [n+1] elements or if [n] < 0. Otherwise [{some = x}], where [x]
   * is the [nth] element of the list.
   */
  get( n:int, l: list('a)): option('a) =
    if Int.equals(Int.compare_raw(n, 0), -1) then none else
    match l : list with
    | [] -> none
    | [hd | tl ] -> if Int.equals(n, 0) then some(hd) else get(n - 1, tl)

  /**
   * Get and remove the [nth] element of a list.
   *
   * @param n The index of an element of the list.
   * @param l A list. If it contains less than [n+1] elements, the result will be [{none}],
   * @return [({none}, l)] if the list contains less than [n+1] elements or if [n] < 0. Otherwise,
   * [({some = x}, r)], where [x] is the [nth] element of the list and [r] is the list obtained by
   * removing the first occurrence of [x] from [l].
   */
  extract(n:int, l:list('a)) = extract_pi((j,_ -> n == j),l): (option('a), list('a))

 /**
  * {2 Loops}
  */



 /**
  * {3 Transforming lists}
  */

   /**
    * Transform a list by applying a function to each element.
    *
    * [map(f,[x1, x2, x3...])] will return [f(x1), f(x2), f(x3), ...]
    *
    * @param f A transformation function.
    * @param l A list, possibly empty.
    * @return A new list with the same size as [l].
    */
   map(f:'a -> 'b, l: list('a)) =
     rec aux(f, l, c) =
          match Int.compare(c, 100) with // tail rec if list size > 100
                | {gt} -> rev_map(f, l) |> rev
                | _ ->
                  match l : list with
                  | [] -> empty
                  | [hd | tl] -> add( f(hd), aux(f, tl, c+1) )
                  end
           end
     aux(f, l, 0): list('b)

  /**
   * As [map], but the transformation function is informed of the current position in the list.
   */
  mapi(f:int, 'a -> 'b, l: list('a)) =
    rev_mapi(f, l) |> rev

  /**
   * Transform a list by removing some elements.
   *
   * @param f A function deciding which items to keep. Wherever [f] returns [{true}], the
   * item is kept, otherwise it is discarded.
   * @param l The original list.
   * @return A list, possibly empty, containing only the elements of [l] for which [f] has
   * returned [{true}].
   */
  filter(f: 'a -> bool, l: list('a)) =
    filteri( _i,x -> f(x), l) : list('a)

  /**
   * As [filter] but the function is informed of the position of the current element.
   */
  filteri(f: int, 'a -> bool, l: list('a)) =
    rec aux(list, pos, accu) =
      match (list : list) with
        | [hd | tl ] -> aux(tl, (pos+1), if f(pos,hd) then cons(hd, accu) else accu)
        | [] -> rev(accu)
    aux(l, 0, []): list('a)


  /**
   * Transform a list, removing some elements, changing others.
   *
   * This function lets you loop through a list and transform it into a different list.
   * Transformations combine the power of [map] (applying a function to an element to
   * obtain a distinct element) and the power of [filter] (discarding elements that do
   * not fit some criterium).
   *
   * @param f A function returning for each item [{some = y}] if [y] should be added to
   * the destination list or [{none}] otherwise.
   * @param l A list to visit.
   * @return A list, containing the non-[{null}] images of items of [l] by [f], in the
   * same order.
   */
  filter_map(f:'a -> option('b), l:list('a)) =
     rec aux(f, l, c) =
          if c > 100 then rev_filter_map(f, l) |> rev
          else
               match l : list with
                | [] -> empty
                | [hd | tl] ->
                  match f(hd) with
                  | { none } -> aux(f, tl, c+1)
                  | { ~some } -> add(some, aux(f, tl, c+1))
                  end
     aux(f, l, 0): list('b)

/**
 * {3 Side-effects}
 */

  /**
   * Apply a function to all the elements of a list.
   *
   * By opposition to [fold], [map], [filter] and their variants, this function is meant to
   * perform side-effects (e.g. displaying information or sending data to a session) rather
   * than extracting some information or building a new data structure.
   *
   * Example: [iter(jlog("{_}"), [1,2,3,4,5])] will print the numbers
   * between 1 and 5.
   *
   * See also: [iteri] behaves as [iter] but also informs the function of the current
   * position in the list.
   *
   * @param f A function to apply to each element of the list. As a sanity check, this function must
   * return [void].
   * @param l A list, possibly empty.
   */
  iter(f: 'a -> void, l: list('a)):void =
    match l:list with
    | [hd | tl] -> do f(hd):void; iter(f,tl)
    | [] -> void : void

  /**
   * As [iter] but informs the function of the current position in the list.
   *
   * [iteri(f,[x0,x1,x2...])] will execute [f(0,x0)], [f(0,x1)], ...
   *
   * @param f A function to apply to each element of the list, along with its position.
   * @param l A list, possibly empty.
   */
  iteri(f: int, 'a -> void, l: list('a)) =
    rec aux(i,l) = match l:list with
        | [hd | tl] -> do f(i,hd):void; aux(i+1,tl)
        | [] -> void
    aux(0,l)




/**
 * {3 Collecting data}
 */

  /**
   * Loop through a list, collecting data from the list.
   *
   * This function implements a general-purpose loop on lists.
   * It is extremely powerful and could be used
   * to reimplement most of the functions in this module.
   *
   * [fold(f, [x0, x1, x2, ...], init)] will compute
   * [f(... f(x2, f(x1, f(x0, init))) ...)]
   *
   * Example: the following expression computes
   * the sum of all elements of a list [fold(_+_, l, 0)].
   *
   * Example: the following expression computes
   * the length of a list [fold((_,i -> i+1), l, 0)]
   *
   * @param f A function invoked at each element of the
   * list to update the data.
   * @param l The list to visit.
   * @param The initial data. If the list is empty, this
   * will also be the result.
   */
  fold(f:'item, 'acc -> 'acc, l: list('item), init:'acc ): 'acc =
//      do Log.notice("Fold", l)
      match l : list with
      | [] -> init
      | [hd | tl] -> fold(f, tl, f(hd,init)): 'acc

  /**
   * As {!List.fold} but the function is informed of the position of
   * the current element.
   */
  foldi(f:int, 'item, 'acc -> 'acc, l: list('item), init:'acc ) =
    rec aux(l: list, accu, ct: int) =
      match l with
        | { ~hd ~tl } -> aux(tl, f(ct,hd,accu), ct+1)
        | _ -> accu
    aux(l, init, 0): 'acc

  /**
   * As {!List.fold} but starting from the end.
   */
  fold_backwards(f:'item, 'acc -> 'acc, l: list('item), init:'acc ) =
    rev(l) |>  fold(  f, _, init )

  /**
   * As {!List.foldi} but starting from the end.
   */
  foldi_backwards(f:int, 'item, 'acc -> 'acc, l: list('item), init:'acc ) =
    rev(l) |>  foldi(  f, _, init )


/**
 * {2 Other transformations}
 */

  /**
   * Split a list in two.
   *
   * This function takes a decision criterium list and returns two list: one with all
   * the elements that match the criterium and one with all the elements that don't.
   *
   * Example:
   *
   * [
   *   is_even(x) = x mod 2 == 0
   *   partition(is_odd, [1,2,3,4,5,6,7,8])
   * ] returns [([2,4,6,8], [1,3,5,7])]
   *
   * @param f A function deciding in which lists elements will end. Items for which
   * [f] returns [{true}] will be placed in a first list, while items for which
   * [f] returns [{false}] will be placed in a second list.
   * @param l The original list.
   *
   * @return A pair of lists [(left, right)], where [left] contains all the elements
   * [x] of [l] such that [f(x)] returns [{true}] and [right] contains the other
   * elements. The order of elements is preserved.
   */
  partition(f: 'a -> bool, l: list('a)) =
    partitioni(_i,x -> f(x), l)

  /**
   * As {!List.partition} but the function is informed of the position of the current element.
   */
  partitioni(f: int, 'a -> bool, l: list('a)) =
    rec aux(l, pos, acc_left, acc_right) = match l: list with
      | { ~hd ~tl } -> if f(pos, hd)
        then aux(tl, (pos+1), cons(hd, acc_left), acc_right)
        else aux(tl, (pos+1), acc_left, cons(hd, acc_right))
      | _ -> (rev(acc_left),rev(acc_right))
    aux(l, 0, [], []) : (list('a), list('a))


 /**
  * {2 Searching}
  */

  /**
   * Search for an element in a list.
   *
   * This function searches the first occurrence of an element in a list and returns
   * its index.
   *
   * @param x The element to find.
   * @param l The list in which to find.
   * @return [{none}] if the element doesn't appear in the list or [{some = i}]
   * if the first occurrence of [x] appears at index [i].
   */
  index(x:'a, l: list('a)) = index_p(x == _,l) : option(int)

  /**
   * Search for an element in a list.
   *
   * This function searches the first occurrence of any element matching a given predicate
   * and returns its index.
   *
   * @param p A function deciding whether the element is the one we are looking for.
   * @param l The list in which to find.
   * @return [{none}] if no acceptable element doesn't appear in the list or [{some = i}]
   * if the first occurrence of [x] appears at index [i].
   */
  index_p(p:'a -> bool, l: list('a)) =
    rec aux(i,l) =
      match l with
      | [] -> none
      | [hd | tl] -> if p(hd) then some(i) else aux(i+1,tl)
    aux(0,l): option(int)


  /**
   * As [index_p] but returns the element itself, not its index.
   */
  find(f:'a -> bool , l:list('a)) =
    match l with
      | [] -> none
      | [hd | tl] -> if f(hd) then some(hd) else find(f, tl) : option('a)

  /**
   * As [index], but returns [true] if the element can be found, not its index.
   */
  mem(x,l) =
    exists(_ == x, l)


  /**
   * Check whether all elements of an index match a given condition.
   *
   * @param f The condition.
   * @param l A list, possibly empty.
   * @return [{true}] if [f] returns [{true}] for all elements of [l] (in particular if [l] is empty), [{false}] otherwise.
   */
  for_all(f, l) =
    match l with
    | [] -> true
    | [hd | tl] -> f(hd) && for_all(f, tl)

  /**
   * Check whether all pair of corresponding elements of the pair of list match a given condition
   * @param f The condition
   * @param l1 A list
   * @param l2 An other, with a possibly different length
   * @return {result={true}} if the predicate is satisfied everywhere and the lists have the same length
   *         {result={false}} if the predicate failed before the end of the shorter list
   *         {different_length=reason} if the predicate succeeded until the end of the shorter list
   *         (reason is either {longest_first} or {longest_second})
   * Note that if the lists have different lengths but the predicate fails before the end
   * of one of the list, then you will get {result=false} and not {different_length=...}
   */
  for_all2(f, l1, l2) =
    match l1
    | [] ->
      match l2 with
      | [] -> {result=true}
      | [_|_] -> {different_length={longest_second}}
      end
    | [h1|l1] ->
       match l2 with
       | [] -> {different_length={longest_first}}
       | [h2|l2] ->
         if f(h1,h2) then for_all2(f, l1, l2) else {result=false}

  /**
   * Check whether at least one element of an index matches a given condition.
   *
   * @param f The condition.
   * @param l A list, possibly empty.
   * @return [{true}] if [f] returns [{true}] for at least one element of [l], [{false}] otherwise (in particular if [l] is empty).
   */
  exists(f, l) =
    match l with
    | [] -> false
    | [hd | tl] -> f(hd) || exists(f, tl)



  /**
   * Find the first element of a list verifying some condition and return it after treatment.
   *
   * This function is equivalent to a composition of [find] and [Option.map].
   *
   * @param f A decision function returning [{none}] if the current item is not the one being
   * searched, or [{some = x}] if the current item is the one being searched and the result should
   * be [x].
   * @param l A list, possibly empty.
   * @return if [f] produced at least one result other than [{none}] on elements of [l], this result
   * otherwise [{none}].
   */
  find_map(f:'a -> option('b), l:list('a)) =
    match l with
    | [] -> none
    | [hd | tl] -> (match f(hd) with
        | {some=_} as res -> res
        | _             -> find_map(f, tl)): option('b)

/**
 * {2 Removing things}
 */

  /**
   * Separate a list in two.
   *
   * @param l The list to separate.
   * @param n The number of elements to put in the first list.
   * @return A pair [(beginning, end)] such that [append(beginning,end) == l]
   * If [l] has [n] elements or more, [beginning] contains the [n] first elements
   * of [l], otherwise [beginning] is [l].
   */
   split_at(l: list('a), n: int) =
      if n<=0 then (empty, l)
      else
        rec aux(debut, fin, n) =
          if n==0 then (rev(debut) , fin)
          else match (fin : list)
               | {~hd ~tl} -> aux(cons(hd,debut),tl,n-1)
               | _         -> (rev(debut) , fin)

        aux([],l,n): (list('a), list('a))

  /**
   * Separate a list in two
   *
   * @param l The list to separate.
   * @param n The number of elements to put in the first list.
   * If [l] has [n] elements or more, the output is identical to split_at
   * otherwise, returns [{none}]
   */
  split_at_opt(l:list('a), n:int) : option((list('a), list('a))) =
    if n < 0 then {none} else
    rec aux(acc,l,n) =
      if n == 0 then {some = (rev(acc),l)} else
      match l with
      | [] -> /* the list is shorter than n */ {none}
      | [h|t] -> aux([h|acc],t,n-1)
    aux([],l,n)

  /**
   * Separate a list in two
   *
   * @param l The list to separate.
   * @param min The minimum number of elements to put in the first list.
   * @param max The maximum number of elements to put in the first list.
   * if [min] > [max], return [{none}]
   * If [l] has less than [min] element, return [{none}]
   * If [l] has more than [max] element, return [split_at_opt(l,max)]
   * If [l] has between [min] and [max], returns [{some=(l,[])}]
   */
   split_between(l:list('a), min:int, max:int) : option((list('a),list('a))) =
     if max < 0 then {none} else
     rec aux(acc,l,min,max) =
       if max == 0 then
         if min <= 0 then
           {some = (rev(acc),l)}
         else
           {none}
       else
         match l with
         | [] -> /* the list is shorted than n */ {none}
         | [h|t] -> aux([h|acc],t,min-1,max-1)
     aux([],l,min,max)

  /**
   * Separate a list in two halves.
   *
   * @param l A list, possibly empty.
   * @return A pair [(beginning, end)] such that [(append(beginning, end) == l)] and
   * and such that either [beginning] and [end] have the same length or the length
   * of [end] is equal to the length of [beginning] + 1.
   */
  split_middle(l: list('a)) = split_at(l, length(l)/2): (list('a), list('a))


  /**
   * Remove the [n] first elements of a list.
   *
   * @param n The number of elements to remove. If n <= 0, the list is return unchanged.
   * @param l The original list. If the list contains at most [n] elements, the empty list is returned.
   */
  drop(n: int, l: list) =
    if n <= 0 then l
    else
      match l with
      | [] -> []
      | [_ | tl] -> drop(n - 1, tl)

  /**
   * Keep the [n] first elements of a list.
   *
   * @param n The number of elements to keep. If n <= 0, the result is [empty].
   * @param l The original list. If it contains less than [n] elements, the original list is returned.
   */
  take(n:int, li:list) =
    rec aux(i,rest,acc) = match rest : list with
      | [] -> li      /*List is shorter than [n], take returns the whole list.*/
      | [hd | tl] -> if i <= 0 then rev(acc)
                              else aux(i - 1, tl, cons(hd, acc))
    aux(n,li,[])

  /**
   * Extract an element from a list.
   *
   * @param f A decision function returning [{true}] if the item encountered is the item found, [{false}] otherwise.
   * @param l A list in which to look, possibly empty.
   * @return Either [({none}, l)] if no matching could be found or [({some = x}, rest)] if an element [x] could be found
   * and [rest] is equal to [l] minus the first occurrence of [x].
   */
  extract_p(f:'a -> bool, l:list('a)) =
    rec aux(r, (acc : list)) =
      match r : list with
      | [] -> (none,l)
      | [hd | tl] -> if f(hd) then (some(hd),rev_append(acc,tl)) else aux(tl,hd+>acc)
    aux(l,[]): (option('a), list('a))


  /**
   * As {!List.extract_p} but the function is informed of the position of the current element.
   */
  extract_pi(f, l) =
    rec aux(r, (acc : list), i) =
      match r : list with
      | []   -> (none,l)
      | [hd | tl] -> if f(i,hd) then (some(hd),rev_append(acc,tl)) else aux(tl,hd+>acc,i+1)
    aux(l,[],0)

  /**
   * As {!List.extract_p} but only returns the resulting list.
   */
  remove_p(p, l) = extract_p(p, l).f2

  /**
   * As {!List.extract_pi} but only returns the resulting list.
   */
  remove_pi(p, l) = extract_pi(p, l).f2

  /**
   * As {!List.extract} but only returns the resulting list.
   */
  remove_at(i,l) = extract(i,l).f2

  /**
   * Remove the first occurrence of an element in a list.
   *
   * @return the list unchanged if the element doesn't appear in the list, otherwise the list
   * minus the first occurrence of [x]
   */
  remove(x: 'a, l:list('a)) = remove_p(_ == x, l): list('a)

  /**
   * Remove duplicates from a list.
   *
   * @return a list containing all the elements of [l] exactly once. Note that the elements of
   * this list may appear in a different order than in [l].
   */
  unique_list_of(l:list('a)) =
    unique_list_of_sorted(sort(l)): list('a)



/**
 * {2 Looping on two lists at once}
 */

  map2_gen_aux(f:'a,'b -> 'c, acc:list('c), l1:list('a), l2:list('b)) : {result:list('c)} / {different_length:list('c)} =
    match l1 with
    | [] ->
      match l2 with
      | [] -> {result=rev(acc)}
      | [_|_] -> {different_length=rev(acc)}
      end
   | [h1|t1] ->
      match l2 with
      | [] -> {different_length=rev(acc)}
      | [h2|t2] -> map2_gen_aux(f, [f(h1,h2)|acc], t1, t2)
      end

  /**
   * Transforms two lists by applying a function to each pair of elements
   * @return a sum {result=l} when the list have the same length
   *            or {different_length=l} when list have different with the result so far
   */
  map2_gen(f, l1, l2) =
    map2_gen_aux(f, [], l1, l2)

   /**
    * Transform two lists by applying a function to each pair of elements at the same position.
    *
    * [map(f,[x1, x2, x3...],[y1, y2, y3, ...])] will return [f(x1,y1), f(x2,y2), f(x3,y3), ...]
    *
    * @param f A transformation function.
    * @param l1 A list, possibly empty.
    * @param l2 A list, possibly empty.
    * @return A new list with the same size as the smallest of [l1], [l2].
    */
  map2(f: 'a, 'b -> 'c, l1: list('a), l2: list('b)) =
    match map2_gen(f, l1, l2) with
    | {result=res} -> res
    | {different_length=res} -> res

  /**
   * Combine two lists into a list of pairs.
   *
   * [zip([x1, x2, x3, ...], [y1, y2, y3, ...])] will return [(x1, y1), (x2, y2), (x3, y3)...]
   * @return A new list with the same size as the smallest of [l1], [l2].
   */
  zip(l1: list('a), l2:list('b)) = map2(x, y -> (x, y), l1, l2) : list(('a,'b))

  /**
   * Separate a lists of pairs into two lists.
   */
  unzip(l: list(('a, 'b))) = fold_backwards( (v1,v2),(acc1,acc2)->(add(v1,acc1),add(v2,acc2)) , l, ([],[]) ) : (list('a), list('b))

  compare(cmp:'a,'a -> Order.comparison,l1:list('a),l2:list('a)):Order.comparison =
    match l1 with
    | [] ->
      match l2 with
      | [] -> {eq}
      | _ -> {gt}
      end
    | [h1|l1] ->
      match l2 with
      | [] -> {lt}
      | [h2|l2] ->
        match cmp(h1, h2) with
          | {eq} -> compare(cmp,l1,l2)
          |  c   -> c

  ordering(using:order('a, 'b),l1:list('a),l2:list('a)):Order.ordering =
    rec f(l1, l2) =
    match l1 with
    | [] ->
      match l2 with
      | [] -> {eq}
      | _ -> {gt}
      end
    | [h1|l1] ->
      match l2 with
      | [] -> {lt}
      | [h2|l2] ->
        match Order.compare(h1, h2, using) with
          | {eq} -> f(l1,l2)
          |  c   -> c
    f(l1, l2)

  make_order(using:order('a, 'b)):order(list('a), 'b) =
    Order.make(ordering(using, _, _))

/**
 * {2 Changing order}
 */

  /**
   * Revert a list.
   *
   * @param l A list, possibly empty.
   * @return The same list, reversed.
   */
  rev(l: list('a)) = rev_append(l,empty): list('a)

/**
 * {2 Association lists}
 *
 * The following functions work on lists of pairs of elements, typically
 * used as dictionaries (e.g. a key and a value). By definition, association
 * lists behave as if adding a new element with a given key masks all previous
 * elements with the same key.
 *
 * Note that module [Map] provides dictionaries which are generally much
 * faster than association lists. Use association lists if your dictionaries
 * are small or if you wish to guarantee that the order of elements does not
 * change.
 */

  /**
   * Find the value associated to a key.
   *
   * @param k A key.
   * @param l An association list.
   * @return [{none}] if the list did not contain any value associated to
   * this key, otherwise [{some = v}], where [v] is the value associated to [k].
   */
  assoc(k:'a, l:list(('a,'b)) ) = assoc_gen(eq, k, l): option('b)

  /**
   * As [assoc], but with control on the comparison function.
   *
   * Use this function rather than [assoc] if you wish to use a special-purpose
   * comparison function, e.g. to compare texts without case.
   *
   * @param equals A comparison function.
   * @param k A key.
   * @param l An association list.
   * @return [{none}] if the list did not contain any value associated to
   * this key, otherwise [{some = v}], where [v] is the value associated to [k].
   */
  assoc_gen(equals: 'a, 'a -> bool, k:'a, l:list(('a,'b)) ) =
    rec aux(tl) = match tl with
      | [] -> none
      | [(key, val) | tl] ->
          if equals(key,k) then some(val)
          else aux(tl): option('b)
    aux(l)


//FRS says: what follows is not about association lists

  /**
   * Insert an item in a list at a given position.
   *
   * @param item  The item to insert in the list.
   * @param index The position at which to insert.
   * @param list  The list in which to insert. If [index < 0] or [index >= length(list)],
   * the list is returned unchanged.
   */
  insert_at(item:'a, index:int, list: list('a)) =
     if index < 0 then list
     else rec aux(i, (acc : list), l) =
        if i == 0 then rev_append(acc, cons(item, l))
        else match l : list
             | [] -> list//List too short, don't insert
             | [hd | tl] -> aux(i - 1, cons(hd, acc), tl) : list('a)
     aux(index, [], list)


  /**
   * Transform a list or give up.
   *
   * This function is a variant on [filter_map] which returns [{none}] if any of the
   * calls to the decision function returns [{none}].
   *
   * @param f A function deciding whether to stop or how to transform the current item.
   * If [f] returns [{none}], the result of [map_while_opt] is [{none}]. Otherwise, put
   * the result of [f] into the resulting list.
   * @param l A list, possibly empty.
   * @return [{none}] if any of the calls to [f] returned [{none}]. Otherwise, [{some = l}],
   * where [l] is the list obtained from the successive [{some}] results of calls to [f].
   */
  map_while_opt(f:'a -> option('b), l:list('a)) =//TODO: This function will become useless with lazy lists.
    rec aux((acc : list), (li : list)) = match li
      | [] -> some(rev(acc))
      | [hd | tl] -> match f(hd)
           | {none}     -> {none}
           | {some = x} -> aux(cons(x,acc), tl)
       end
    end
    aux([], l) : option(list('b))



/**
 * {2 Special-purpose optimizations}
 *
 * The following functions are specialized counterparts
 * to more general functions, optimized for some specific
 * cases.
 *
 * These functions are typically useful if you do not care
 * about the order of elements in the list or if you know
 * exactly when and how you intend to revert the order of
 * such elements.
 */

  /**
   * Revert a list and append it to another one.
   *
   * Performance note: This function is provided solely for performance reasons, as it is much
   * faster than performing either [append(l1, l2)] or [append(rev(l1), l2)].
   *
   * @param l1 A list to revert.
   * @param l2 A list to append at then end of l1.
   */
  rev_append(l1: list('a), l2:list('a)) =
    match l1 with
    | [] -> l2
    | [hd|tl]-> rev_append(tl, [hd|l2])


   /**
    * As [filter_map], but reverses the list.
    */
   rev_filter_map(f,l) =
      rec aux(f,l, (acc : list)) =
         match l : list with
         | [] -> acc
         | [hd | tl] ->
            match f(hd) with
            | { none } -> aux(f, tl, acc)
            | { ~some } -> aux(f, tl, add(some, acc))
            end
      aux(f, l, [])

   /**
    * As [map], but reverses the list.
    */
   rev_map(f,(l : list)) =
        rec aux(f, (l: list), (acc : list)) =
               match l : list with
               | [] -> acc
               | [hd | tl] -> aux(f, tl , add(f(hd),acc) )
        aux(f,l,[])

   /**
    * As [mapi], but reverses the list.
    */
   rev_mapi(f,(l : list)) =
        rec aux(f,l, (acc : list),i) =
               match l : list with
               | [] -> acc
               | [hd | tl] -> aux(f, tl , add(f(i,hd),acc), i+1)
        aux(f,l,[],0)


  /**
   * Flatten a list of list into a list.
   *
   * Performance note: this function is much faster than successive calls to [append]
   *
   * @param l A list of lists of items.
   * @return A list of items, respecting the order of [l].
   */
  flatten(l: list(list('a))) : list('a) =
    rev(rev_flatten(l))

  /**
   * As [flatten], but reverses the list.
   */
  rev_flatten(l: list(list('a))) : list('a) =
    rec aux(l1, acc) =
      match l1 with
      | [] -> acc
      | [hd|tl] -> aux(tl, rev_append(hd, acc))
    aux(l,[])

  /**
   * Transform a list by applying a function to each element and flattening the result.
   *
   * [collect(f, [x1, x2, x3...])] will return [f(x1) ++ f(x2) ++ f(x3) ++ ...]
   *
   * @param f A transformation function (from items to lists).
   * @param l A list, possibly empty.
   * @return A new list, respecting the order of [l].
   */
  collect(f : 'a -> list('b), l : list('a)) : list('b) =
    rec aux(l, acc) =
      match l with
      | [] -> acc
      | [hd|tl] -> aux(tl, rev_append(f(hd), acc))
    rev(aux(l, []))

  /**
   * Puts a constant separator between each element of the list
   *
   * @param sep A separator
   * @param l A list, possibly empty.
   * @return A new list, of size [max(length(l), (2 * (length(l) - 1)))]
   */
  intersperse(sep : 'a, l : list('a)) : list('a) =
    rec aux(l) =
      match l with
      | [] | [_] -> l
      | [hd|tl] -> [hd, sep | aux(tl)]
    aux(l)

  /**
   * As [unique_list_of] but only works on sorted lists.
   */
  unique_list_of_sorted(l:list('a)) =
    rec aux(v,(last, (acc : list))) =
      v2 = some(v)
      if v2 == last then
        (v2,acc)
      else
        (v2,cons(v,acc))
    (_,r) = foldl(aux,l,(none,[]))
    rev(r)

 /**
  * {1 Unsafe access}
  */

  unsafe_get(i:int, l: list('a)) =
    match get(i,l) with
    | { none } -> error("List.unsafe_get")
    | { ~some } -> some: 'a

  tail(l) =
    match l with
    | [] -> error("List.tail on empty list")
    | [_ | tl] -> tl

  tail_opt(l) =
    match l with
    | [_ | tl] -> some(tl)
    | [] -> none


  head(lst) =
    match lst with
    | [hd | _] -> hd
    | [] -> error("List.head on empty list"): 'a

  head_opt(lst) =
    match lst with
      | [hd | _ ] -> some(hd)
      | [] -> none
/*
  pop(l:list('a)) = match l with
    | [] -> {none}
    | [hd | tl] -> {some = (hd, tl)}
*/
  foldl1(f: 'item, 'item -> 'item, l: list('item) ) = match l:list with
      | [ hd | tl] -> fold(f, tl, hd)
      | [] -> error("List.foldl1 on empty list"): 'item

  foldr1(f: 'item, 'item -> 'item, l: list('item) )  = (rev(l) |>  foldl1( f, _ )): 'item


  fold2(f: ('a, 'b, 'c -> 'c), l1: list('a), l2: list('b), accu: 'c) =
    match l1 with
      | [ h1 | t1 ] -> (
        match l2 with
          | [ h2 | t2 ] -> fold2(f, t1, t2, f(h1, h2, accu))
          | _ -> error("List.fold2 l1.length != l2.length")
        )
      | _ -> accu

  min(l) = foldl1(@toplevel.min, l)
  max(l) = foldl1(@toplevel.max, l)

  min_max(l : list) =
    rec aux(min1, max1, li) =
    match li : list with
    | [] -> (min1, max1)
    | [hd,hd2|tl] ->
      (min2, max2) = if hd < hd2 then (hd, hd2) else (hd2, hd)
        aux(@toplevel.min(min1, min2), @toplevel.max(max1, max2), tl)
    | [hd] -> (@toplevel.min(hd, min1), @toplevel.max(hd, max1))
    _ = aux : 'a, 'a, list('a) -> tuple_2('a, 'a)
    match l : list with
    | [] -> error("List.min_max on empty list")
    | [hd,hd2|tl] ->
      if hd < hd2
      then aux(hd, hd2, tl)
      else aux(hd2, hd, tl)
    | [hd] -> (hd, hd)



  /*inlist(v, l:list) =
    match l with
      | [ hd | tl ] -> if hd == v then true else inlist(v, tl)
      | [] -> false*/

 /**
  * {1 Deprecated functions}
  */

 /**
  * As {!List.get}.
  */
 nth = get

 /**
  * As {!List.unsafe_get}.
  */
 unsafe_nth = unsafe_get

 /**
  * As {!List.fold}
  */
  fold_left(f, accu, l) = g(a,b) = f(b,a) fold( g, l, accu )
  foldl = fold

 /**
  * As {!List.fold_backwards}.
  */
  fold_right(f, l, accu) = g(a,b) = f(b,a) fold_backwards( g, l, accu )

  foldr = fold_backwards

  /**
   * As {!List.foldi}.
   */
  fold_index = foldi

  /**
   * Map a list while propagating an accumulator
   * (l',accN) = fold_map(op, l, acc1)
   * is equivalent to
   * l' = List.map(map_elt,l)
   * accN = List.fold(fold_elt,l,acc1)
   * when
   * map_elt(x) = op(x,<acc>).f1
   * fold_elt(x,acc) = op(x,acc).f2
  */
  fold_map(f,l,acc) = fold_map_rev_append(f,l,[],acc)

  /** Same as fold_map but append a reversed list at the end
    * (l',accN) = fold_map_rev_append(f,l1,l2,acc)
    * <=>
    * (l'',accN) = fold_map(f,l1,acc)
    * l' = [l'' ++ List.rev(l2)]
    */
  @private
  fold_map_rev_append(f,l,rl,acc) =
    match l
    [x|l] -> (x,acc)=f(x,acc)
             fold_map_rev_append(f,l,[x|rl],acc)
    []    -> (List.rev(rl),acc)

/**
 * {2 Sorting}
 */

  /**
   * Sort a list by usual order.
   *
   * This function uses the order defined by function [compare]. The sort algorithm used may change with
   * future releases of OPA. In the current version, the sort algorithm used is merge sort.
   *
   * @param l A list, possibly empty.
   * @return A list with the same length, in which all elements are ordered by increasing order
   * using regular comparison.
   */
  sort(l:list('a)) = merge_sort_with_order(Order.default, l): list('a)

  /**
   * Check whether a list is sorted, by usual order.
   *
   * @param l A list, possibly empty.
   * @return [{true}] if the list is sorted by increasing order of [compare] (in particular if the list is empty),
   * [{false}] otherwise.
   */
  is_sorted(l: list('a)) = is_sorted_with(Order.compare(_,_,Order.default), l): bool

  /**
   * Sort a list by usual order, projecting elements before comparing them.
   *
   * This function first projects elements of the list before comparing them. You may use this
   * projection for instance to normalize list elements before comparing, e.g. to convert strings
   * to lower-case before comparison, so as to ensure that your list is sorted by purely alphabetical
   * order, without taking case into account. Similarly, if you sort lists of complex data structures, you
   * can ensure that only a subset of data is taken into account by comparison.
   *
   * @param f A projection function.
   * @param A list, possibly empty.
   */
  sort_by(f:'a -> 'b, l:list('a)) =
    merge_sort_with_order(Order.make_by(f, Order.default), l): list('a)

  /**
   * Check whether a list is sorted, projecting elements before comparing them.
   *
   * @param l A list, possibly empty.
   * @return [{true}] if the list is sorted by increasing order of [compare] (in particular if the list is empty),
   * [{false}] otherwise.
   */
  is_sorted_by(f:'a -> 'b, l: list('a)) = is_sorted_with(x, y -> Order.compare(f(x),f(y),Order.default), l): bool


  /**
   * Sort a list with a custom order.
   *
   * This is a more powerful variant of [sort_by].
   *
   * @param cmp A comparison function.
   * @param l A list, possibly empty.
   */
  sort_with(cmp:'a, 'a -> Order.ordering, l: list('a)) = merge_sort(cmp, l): list('a)

  /**
   * Sort a list with a custom order.
   *
   * This is a more powerful variant of [sort_by].
   *
   * @param cmp A comparison function.
   * @param l A list, possibly empty.
   */
  sort_with_order(cmp:order('a, _), l: list('a)) = merge_sort_with_order(cmp, l): list('a)

  /**
   * Check whether a list is sorted, using a custom order.
   *
   * @param cmp A ordering function.
   * @param l A list, possibly empty.
   * @return [{true}] if the list is sorted by increasing order of [compare] (in particular if the list is empty),
   * [{false}] otherwise.
   */
  is_sorted_with(order: ('a, 'a -> Order.ordering), l: list('a)) =
    rec aux =
    | [e1,e2|tl] ->
       match order(e1, e2) with
           | {lt} | {eq} -> aux([e2|tl])
           | _ -> {false}
       end
    | [] | [_] ->
      true
    aux(l): bool

  /**
   * Check whether a list is sorted, using a custom order.
   *
   * @param cmp A comparison function.
   * @param l A list, possibly empty.
   * @return [{true}] if the list is sorted by increasing order of [compare] (in particular if the list is empty),
   * [{false}] otherwise.
   */
  is_sorted_with_order(order: order('a, _), l: list('a)) =
    rec aux =
    | [e1,e2|tl] ->
      if Order.is_smallereq(e1, e2, order) then aux([e2|tl])
      else {false}
    | [] | [_] ->
      true
    aux(l): bool

  /**
   * Perform sorting on a list, using the merge sort algorithm.
   *
   * Performance note: This algorithm has complexity O(n log (n))
   */
  merge_sort(cmp:('a, 'a -> Order.ordering), l:list('a)) =
    merge_sort_with_order(Order.make(cmp), l)

  merge(order: order('a, _), a:list('a), b:list('a)) : list('a) =
    match (a,b) with
    | ([], f2) -> f2
    | (f1, []) -> f1
    | ([hd1 | tl1], [hd2 | tl2]) ->
      if Order.is_smaller(hd1, hd2, order) then [hd1|merge(order, tl1, b)]
      else [hd2|merge(order, a, tl2)]
    end

  merge_with_comparison(cmp:'a, 'a -> Order.comparison, a:list('a), b:list('a)) : list('a) =
    match (a,b) with
    | ([], f2) -> f2
    | (f1, []) -> f1
    | ([hd1 | tl1], [hd2 | tl2]) ->
      match cmp(hd1,hd2) with
      | {lt} -> [hd1|merge_with_comparison(cmp, tl1, b)]
      | _ -> [hd2|merge_with_comparison(cmp, a, tl2)]
    end


  /**
   * Perform sorting on a list, using the merge sort algorithm.
   *
   * Performance note: This algorithm has complexity O(n log (n))
   */
  merge_sort_with_order(order: order('a, _), l:list('a)) =
    rec aux_split =
    | [] -> ([],[])
    | [hd1] -> ([hd1],[])
    | [hd1,hd2|tl] ->
      (l1, l2) = aux_split(tl)
      (add(hd1, l1), add(hd2, l2))

    rec aux(l) =
      (l1,l2) = aux_split(l)
      match l2 with
      | [] -> l1
      | _ -> r1 = aux(l1)
             r2 = aux(l2)
             merge(order,r1,r2)
    aux(l)

/**
 * {2 Parsing and pretty-printing}
 */

  /**
   * [compose(concatenate, beg_symbol, end_symbol, sep_symbol, l)]
   * returns:
   *
   *   [beg_symbol]+[x_1]+[sep_symbol]+...+[sep_symbol]+[x_n]+[end_symbol]
   *
   * where [+] indicates application of [concatenate] function.
   * You'll most likely not use this function but one derived
   * from it: {!List.to_string_using}, {!List.to_string} or
   * {!XmlConvert.of_list_using}.
   */
  compose(concatenate : 'a, 'a -> 'a, beg_symbol : 'a, end_symbol : 'a,
    sep_symbol : 'a, l : list('a)) : 'a =
    `^` = concatenate
    rec aux = l ->
      match l with
      | [] -> end_symbol
      | [x] -> x ^ end_symbol
      | [x|xs] -> x ^ sep_symbol ^ aux(xs)
    beg_symbol ^ aux(l)

  /**
   * Pretty-printing a list of strings [[x_1, ... x_n]] as:
   *
   *   [beg_symbol][x_1][sep_symbol]...[sep_symbol][x_n][end_symbol]
   *
   * @param beg_symbol a string to be used at the beginning of
   * the output.
   * @param end_symbol a string to be used at the end of the
   * output.
   * @param sep_symbol a string to be used to separate list
   * entries
   * @param l a list of strings to be pretty-printed
   * @return a pretty-printed version of [l] as explained above.
   */
  to_string_using(beg_symbol, end_symbol, sep_symbol, l) =
    compose(`^`, beg_symbol, end_symbol, sep_symbol, l)

  /**
   * Pretty-printing a list of strings [[x_1, ... x_n]] as:
   *
   *   [[[x_1], ..., [x_n]]]
   *
   * @param l a list to be pretty-printed.
   */
  to_string(l) =
    to_string_using("[", "]", ", ", l)

  /**
   * Pretty-printing an arbitrary list [[x_1, ... x_n]] using a value printer [f] as:
   *
   *   [[[f(x_1)], ..., [f(x_n)]]]
   *
   * @param alpha_to_string a value printer
   * @param l a list to be pretty-printed.
   *
   * NB: this is the default list-printer
   */
  @stringifier(list('a)) list_to_string(alpha_to_string : 'a -> string, l : list('a)) : string =
    to_string(List.map(alpha_to_string, l))

}}

/**
 * {1 Values exported to the global namespace}
 */

/**
 * As {!List.nil}: the empty list.
 */
nil = List.empty

/**
 * As {!List.cons}: adding an element at the head of a list.
 */
`+>`=List.`+>`

/**
 * As {!List.append}: list concatenation.
**/
`++`(x, y) = List.append(x, y)

/**
 * An external type for manipulating OCaml lists.
**/
@opacapi
type caml_list('a) = external

@opacapi List_split_at_opt = List.split_at_opt
@opacapi List_split_between = List.split_between
