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
  /*
  to_list((l1, _, l2, _) : Queue.t('a)) :list('a) : Queue.t('a)
  */

  /**
   * Create a new list from a queue.
   * The {b first} element of the list is the {b top} of the queue.
   * Complexity O(n)
  **/
  /*
  to_list_backwards((l1, _, l2, _) : Queue.t('a)) :list('a) : Queue.t('a)
  */

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

  /**
   * [Queue.take_n(n, q)]
   * Take at most [n] element of [q], and the remaining queue.
   * The first element of the returned list was the top of the queue.
  **/
  /*
    Mathieu Wed Feb  2 15:41:42 CET 2011
    FIXME This implementation is naive.
    a correct implementation would not use [rem] which
    will build lots of totally unused quadruplet.
    (intermediate queues)
  */
  take_n(n: int, q: Queue.t('a)) : (Queue.t('a), list('a)) =
    rec aux(q, n, li) =
      if n == 0 then (q, List.rev(li))
      else
        (item, q2) = rem(q)
        match item with
        | {none} -> (q2, List.rev(li))
        | {~some} -> aux(q2, n - 1, some +> li)
    aux(q, n, [])

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
