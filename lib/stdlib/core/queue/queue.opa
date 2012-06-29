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
  Mathieu Wed Feb  2 15:47:11 CET 2011
  TODO: Add unit test in reftester
*/

/**
 * Queue
 *
 * @category algo
 * @author Maxime Audouin, 2010
 * @author Mathieu Barbin, 2011
 * @author Rudy Sicard, 2012
 * @destination
 * @stability untested
**/

/**
 * {1 About this module}
 *
 * This file provides an implementation of functional queue.
 * We provide 'average complexity', which means that on standard use you will observe the amortized given complexity.
 * But they are not strictly guaranteed amortized complexity. (malicious code can make complexity worse than that)
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
   * Complexity O(1)
  **/
  singleton(item:'a) : Queue.t('a) = ([], 0, [item], 1)

  /**
   * Add an element to a queue.
   * Complexity O(1)
  **/
  add(item:'a , (l1, s1, l2, s2): Queue.t('a)) : Queue.t('a) = ([item|l1], s1 + 1, l2, s2)

  /** making element of l1 available in l2 */
  @private
  flush_l1(l1,s1,s2): Queue.t('a)  = ([],0,List.rev(l1),s1+s2)

  /**
   * Get the top of the queue.
   * Average complexity O(1), but ranges from O(1) and  O(n)
  **/
  top((l1, s1, l2, s2): Queue.t('a)): option('a) =
    match l2 with
    | [ hd | _ ] -> some(hd)
    | [] ->
      if l1 == [] then none
      else top( flush_l1(l1,s1,s2) )
  /**
   * Get the top and the remaining queue.
   * Average complexity O(1), but ranges from O(1) and  O(n)
  **/
  rem((l1, s1, l2, s2): Queue.t('a)) : (option('a), Queue.t('a)) =
    match l2 with
    | [ hd | tl ] -> (some(hd), (l1, s1, tl, s2 - 1) )
    | [] ->
      if l1 == [] then (none,empty)
      else rem( flush_l1(l1,s1,s2) )

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
        (elts, l2) = List.split_at(l2,n)
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
          elts = List.append(l2, List.rev(rest))
          queue = ([], 0, l1, r1)
          (queue, elts)

  /**
   * {1 Iterators}
  **/


  /**
   * Reverse the queue
   * We provide no other reversed iterator since it is efficient.
   * Complexity O(1)
  **/
  rev((l1, s1, l2, s2): Queue.t('a)) : Queue.t('a) =
    (l2,s2,l1,s1)

  /**
   * {2 fold}
   * Fold from top (first added) to bottom (last added)
   * Complexity O(n)
  **/
  fold(f : ('a, 'acc -> 'acc), (l1, _s1, l2, _s2) : Queue.t('a), acc : 'acc) : 'acc =
    List.foldr(f, l1, List.fold(f, l2, acc))

  @private revindexl1(s1,s2,i) = s2+s1-(i+1)

  foldi(f : (int, 'a, 'acc -> 'acc), (l1, _s1, l2, s2) : Queue.t('a), acc : 'acc) : 'acc =
    acc = List.foldi(f, l2, acc)
    f_(i,v, acc) = f(s2+i ,v, acc)
    List.foldi(f_, List.rev(l1), acc)

  /*
  fold_backwards(f : ('a, 'acc -> 'acc), (l1, s1, l2, s2) : Queue.t('a), acc : 'acc) : 'acc
  foldi_backwards(f : (int, 'a, 'acc -> 'acc), (l1, s1, l2, s2) : Queue.t('a), acc : 'acc) : 'acc
  */

  /**
   * {2 map}
   * Complexity O(n)
  **/
  map(f : ('a -> 'b), (l1, s1, l2, s2) : Queue.t('a)) : Queue.t('b) =
    (List.map(f,l1),s1,List.map(f,l2),s2)

  mapi(f : (int, 'a -> 'b), (l1, s1, l2, s2) : Queue.t('a)) : Queue.t('b) =
    l1 = List.mapi(f,l1)
    f_(i,v) = f(revindexl1(s1,s2,i),v)
    l2 = List.mapi(f_,l2)
    (l1,s1,l2,s2)

  /*
  map_backwards(f : ('a -> 'a), (l1, s1, l2, s2) : Queue.t('a)) : Queue.t('a)
  mapi_backwards(f : (int, 'a -> 'a), (l1, s1, l2, s2) : Queue.t('a)) : Queue.t('a)
  */

  /**
   * {2 iter}
   * Iter from top (first added) to bottom (last added)
   * Complexity O(n)
  **/
  iter(f : ('a -> void), (l1, _s1, l2, _s2) : Queue.t('a)) : void =
    do List.iter(f, l2)
    do List.rev_iter(f, l1)
    void

  iteri(f : (int, 'a -> void), (l1, _s1, l2, s2) : Queue.t('a)) : void =
   do List.iteri(f, l2)
   f_(i,v) = f(s2+i,v)
   do List.iteri(f_, List.rev(l1))
   void

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
   * Complexity O(n)
  **/

  exists(f : ('a -> bool), (l1, _s1, l2, _s2) : Queue.t('a)) : bool =
    List.exists(f,l1) || List.exists(f,l2)
  /*
  existsi(f : (int, 'a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : bool
  */

  /*
  exists_backwards(f : ('a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : bool
  existsi_backwards(f : (int, 'a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : bool
  */

  /**
   * {2 forall}
   * Complexity O(n)
  **/

  forall(f : ('a -> bool), (l1, _s1, l2, _s2) : Queue.t('a)) : bool =
     List.for_all(f,l1) && List.for_all(f,l2)
  /*
  foralli(f : (int, 'a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : bool
  */

  /**
   * {2 foldmap}
   * Fold from top (first added) to bottom (last added)
   * Complexity O(n)
  **/

  foldmap(f, (l1,s1,l2,s2):Queue.t, acc:'acc):(Queue.t,'acc) =
    (l2,acc) = List.fold_map(f, l2, acc)
    (l1,acc) = List.fold_map(f, l1, acc)
    ( (l1,s1,l2,s2), acc )

  foldmapi(f, (l1,s1,l2,s2):Queue.t, acc) =
    f(v,(acc,n)) = (
      (e,acc) = f(n,v,acc)
      (e,(acc,n+1))
    )
    (l2,(acc, n)) = List.fold_map(f, l2,           (acc,0))
    (l1,(acc,_n)) = List.fold_map(f, List.rev(l1), (acc,n))
    ( (l1,s1,l2,s2), acc )

  /**
   * {2 filter}
  **/

  filter(f : ('a -> bool), (l1, _s1, l2, _s2) : Queue.t('a)) : Queue.t('a) =
    l1 = List.filter(f,l1)
    s1 = List.length(l1)
    l2 = List.filter(f,l2)
    s2 = List.length(l2)
    (l1,s1,l2,s2)

  //filteri(f : (int, 'a -> bool), (l1, s1, l2, s2) : Queue.t('a)) : Queue.t('a)


}}
