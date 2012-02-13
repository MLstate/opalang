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


/*
  Mathieu Wed Feb  2 15:47:11 CET 2011
  TODO: Add unit test in reftester
*/

/**
 * Queue
 *
 * @category algo
 * @author Maxime Audouin, 2010
 * @author Mathieu Barbin, 2011
 * @destination
 * @stability untested
**/

/**
 * {1 About this module}
 *
 * This file provides an implementation of functional queue.
 *
**/

/**
 * {1 Types defined in this module}
**/

/**
 * The abstract type of functionnal queue.
**/
@abstract type Queue.t('a) = (list('a), int, list('a), int)

Queue = {{

  /**
   * the empty queue
  **/
  empty : Queue.t('a) = ([], 0, [], 0)

  /**
   * Test if a queue has no element.
   * Complexity O(1)
  **/
  is_empty((_l1, s1, _l2, s2): Queue.t('a)): bool = s1 == 0 && s2 == 0

  /**
   * Get the number of element contained in the queue.
   * Complexity O(1)
  **/
  size( (_l1, s1, _l2, s2): Queue.t('a) ):int = s1 + s2

  /**
   * {1 Binding with List}
  **/

  /**
   * [Queue.of_list(li)]
   * Create a new queue from a list.
   * The {b first} element of the list is the {b top} of the queue.
   * More efficient than [List.fold(Queue.add, li, Queue.empty)]
   * Complexity O(n)
  **/
  of_list(li:list('a)) : Queue.t('a) = ([], 0, li, List.length(li))

  /**
   * [Queue.of_list_backwards(li)]
   * Create a new queue from a list.
   * The {b last} element of the list is the {b top} of the queue.
   * More efficient than [List.fold_backwards(Queue.add, li, Queue.empty)]
   * Complexity O(n)
  **/
  of_list_backwards(li:list('a)) : Queue.t('a) = (li, List.length(li), [], 0)

  /**
   * Create a new list from a queue.
   * The {b first} element of the list is the {b top} of the queue.
   * Complexity O(n)
  **/
  to_list((l1, _, l2, _) : Queue.t('a)) : list('a) =
    List.rev_append(List.rev(l2), List.rev(l1))

  /**
   * Create a new list from a queue.
   * The {b last} element of the list is the {b top} of the queue.
   * Complexity O(n)
  **/
  to_list_backwards((l1, _, l2, _) : Queue.t('a)) :list('a) =
    List.rev_append(List.rev(l1), List.rev(l2))

  /**
   * {1 Bingind with Set}
  **/

  /*
  of_set(set:ordered_set('a, _)) : Queue.t('a)
  to_set((l1, _, l2, _) : Queue.t('a)) : ordered_set('a, _)
  */

  /**
   * {1 Add / Remove elements}
  **/

  /**
   * Create a singleton queue, containing the given element
  **/
  singleton(item:'a) : Queue.t('a) = ([], 0, [item], 1)

  /**
   * Add an element to a queue.
   * Complexity O(1)
  **/
  add(item:'a , (l1, s1, l2, s2): Queue.t('a)) : Queue.t('a) =
    if s2 == 0 && s1 == 0
    then
      ([], 0, [item], 1)
    else
      (item +> l1, s1 + 1, l2, s2)

  /**
   * Get the top of the queue.
   * Complexity O(1) in better case, O(n) is worth case.
  **/
  top((l1, _s1, l2, _s2): Queue.t('a)): option('a) =
    match l2 with
    | [ hd | _ ] -> some(hd)
    | [] ->
      match List.rev(l1) with
      | [ hd | _ ] -> some(hd)
      | [] -> none
      end

  /**
   * get the top and the remaining queue.
  **/
  rem((l1, s1, l2, s2): Queue.t('a)) : (option('a), Queue.t('a)) =
    match l2 with
    | [ hd | tl ] ->
      if s2 > 1 then
        (some(hd), (l1, s1, tl, s2 - 1) )
      else
        (some(hd), ([], 0, List.rev(l1), s1))

    | [] ->
      nl = List.rev(l1)
      match nl with
      | [] -> (none, (l1, s1, l2, s2))
      | [ hd | tl ] -> (some(hd), ([], 0, tl, s1 - 1))

  // Take at most [n] element of [list], and return them, and the remaining list.
  @private
  list_take_n(n:int, list:list) =
    rec aux(pos, acc, l) =
      match l with
      | [] ->
        // n was greater than or equal to the number of element in the list
        // in this case, the function is stable, the initial list is returned
        (list, [])
      | [ hd | tl ] ->
        if pos == n
        then
          // acc contains already all the n first elements of the initial list
          (List.rev(acc), l)
        else
          aux(pos+1, hd +> acc, tl)
     aux(0, [], list)

  /**
   * [Queue.take_n(n, q)]
   * Take at most [n] element of [q], and the remaining queue.
   * The first element of the returned list was the top of the queue.
  **/
  take_n(n: int, q: Queue.t('a)) : (Queue.t('a), list('a)) =
    match q with
    | (l1, s1, l2, s2) ->
      if s2 >= n
      then
        (elts, l2) = list_take_n(n, l2)
        s2 = s2 - n
        ((l1, s1, l2, s2), elts)
      else
        // all elts of l2 will be full extracted and returned
        // adding also [n1] elts taken from l1
        n1 = n - s2
        if n1 >= s1
        then
          // that is all the queue
          (empty, to_list(q))
        else
          // it will remain [r1] elts of l1 after extraction
          // we can take benefits of the operation to reverse l1 to fill l2
          r1 = s1 - n1
          (l1, rest) =
            rec aux(pos, acc, l) =
              if pos >= r1 then (acc, l)
              else
                match l with
                | [ hd | tl ] ->
                  aux(pos+1, hd +> acc, tl)
                | [] -> @fail("internal error")
            aux(0, [], l1)
          elts = List.rev_append(List.rev(l2), List.rev(rest))
          queue = ([], 0, l1, r1)
          (queue, elts)

  /**
   * {1 Iterators}
  **/

  /**
   * {2 fold}
  **/

  /**
   * Create a new queue with element in reverse order
  **/
  /*
  rev((l1, s1, l2, s2): Queue.t('a)) : Queue.t('a)
  */

  fold(f : ('a, 'acc -> 'acc), (l1, _s1, l2, _s2) : Queue.t('a), acc : 'acc) : 'acc =
    List.foldr(f, l1, List.fold(f, l2, acc))
  /*
  foldi(f : (int, 'a, 'acc -> 'acc), (l1, s1, l2, s2) : Queue.t('a), acc : 'acc) : 'acc
  */

  /*
  fold_backwards(f : ('a, 'acc -> 'acc), (l1, s1, l2, s2) : Queue.t('a), acc : 'acc) : 'acc
  foldi_backwards(f : (int, 'a, 'acc -> 'acc), (l1, s1, l2, s2) : Queue.t('a), acc : 'acc) : 'acc
  */

  /**
   * {2 map}
  **/

  /*
  map(f : ('a -> 'a), (l1, s1, l2, s2) : Queue.t('a)) : Queue.t('a)
  mapi(f : (int, 'a -> 'a), (l1, s1, l2, s2) : Queue.t('a)) : Queue.t('a)
  */

  /*
  map_backwards(f : ('a -> 'a), (l1, s1, l2, s2) : Queue.t('a)) : Queue.t('a)
  mapi_backwards(f : (int, 'a -> 'a), (l1, s1, l2, s2) : Queue.t('a)) : Queue.t('a)
  */

  /**
   * {2 iter}
  **/

  /*
  iter(f : ('a -> void), (l1, s1, l2, s2) : Queue.t('a)) : void
  iteri(f : (int, 'a -> void), (l1, s1, l2, s2) : Queue.t('a)) : void
  */

  /*
  iter_backwards(f : ('a -> void), (l1, s1, l2, s2) : Queue.t('a)) : void
  iteri_backwards(f : (int, 'a -> void), (l1, s1, l2, s2) : Queue.t('a)) : void
  */

  /**
   * {2 find}
  **/

  /*
  find(f : ('a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : 'a
  findi(f : (int, 'a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : 'a
  */

  /*
  find_backwards(f : ('a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : 'a
  findi_backwards(f : (int, 'a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : 'a
  */

  /**
   * {2 exists}
  **/

  /*
  exists(f : ('a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : bool
  existsi(f : (int, 'a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : bool
  */

  /*
  exists_backwards(f : ('a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : bool
  existsi_backwards(f : (int, 'a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : bool
  */

  /**
   * {2 forall}
  **/

  /*
  forall(f : ('a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : bool
  foralli(f : (int, 'a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : bool
  */

  /**
   * {2 foldmap}
  **/

  /*
  foldmap
  foldmapi
  */

  /**
   * {2 filter}
  **/

  /*
  filter(f : ('a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : Queue.t('a)
  filteri(f : (int, 'a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : Queue.t('a)
  */

  /**
   * Export to a iter value.
   * @deprecated Not efficient, use rather [Queue.iter]
  **/
  @deprecated({use="Queue.iter"})
  to_iter((l1, _s1, l2, _s2): Queue.t('a)): iter('a) =
    Iter.append(Iter.of_list(l2), Iter.of_list(List.rev(l1)))
}}
