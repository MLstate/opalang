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
 * Native Imperative Array.
 *
 * @destination public
 * @stabilization untested
**/

/**
 * {1 About this module}
 *
 * This module contains an API for manipulating imperative native array.
 * You must be careful by using this module, there are some strong constraints:
 * + your array will not be serializable. Implementation is available on each side,
 * but llarray are not meant to be transported from a side to an other.
 * + this structure brings not any guarantee in case of parrallelism, and distribution.
 *
 * You may use this structure, if you know what you're doing.
 * Probably not for casual user.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
**/

/**
 * {1 About dependencies of this module}
 *
 * Be aware of the fact that the opa compiler introduce some @llarray directives.
**/

/**
 * {1 Types defined in this module}
**/

/**
 * The external type for manipulating low level arrays.
 * Implemented on both side.
**/
@opacapi
type llarray('a) = external

/**
 * {1 Module for manipulating llarray}
**/
LowLevelArray = {{

  size   = %% Lowlevelarray.size %%   : llarray('a) -> int

  /**
   * Element are indexed, started from index [0].
   * This function is unsafe, and fails in case of index out of bounds.
   * @error
  **/
  get    = %% Lowlevelarray.get %%    : llarray('a), int -> 'a

  /**
   * Element are indexed, started from index [0].
   * This function is unsafe, and fails in case of index out of bounds.
   * @error
  **/
  set    = %% Lowlevelarray.set %%    : llarray('a), int, 'a -> void

  /**
   * [LowLevelArray.create(size,v)] creates an llarray of the size [size] with each cell initialized to [v]
   * [llarray] of size zero returned are shared
   * You may also use the directive [@llarray(0, 1, 2, 3)] for building
   * static and small arrays.
  **/
  create(size,v) =
   bp_create = %% Lowlevelarray.create %% : int, 'a -> llarray('a)
   if size==0 then empty else bp_create(size,v)
  /* waiting sharing for @llarray to have the sharing invariant of zero size array */

  /** A polymorphic array of size zero */
  empty = @llarray()

  /**
   * An alias for {!LowLevelArray.size}
  **/
  length = size

  /**
   * {2 Iterators}
  **/

  iter(f : 'a -> void, a : llarray('a))=
    n = length(a)
    rec aux(i) = if i == n then void else do f(get(a, i)); aux(succ(i))
    aux(0)

  iteri(f : int, 'a -> void, a : llarray('a))=
    n = length(a)
    rec aux(i) = if i == n then void else do f(i, get(a, i)); aux(succ(i))
    aux(0)

  fold(f, a, acc)=
    n = length(a)
    rec aux(i, acc) =
      if i == n then acc
      else
        acc = f(get(a, i), acc)
        aux(succ(i), acc)
    aux(0, acc)

  foldi(f, a, acc)=
    n = length(a)
    rec aux(i, acc) =
      if i == n then acc
      else
        acc = f(i, get(a, i), acc)
        aux(succ(i), acc)
    aux(0, acc)


  /**
   * The order of the traversal of the list is not guaranteed
   * to be left to right
   */
  filter_map_to_list(f:'a -> option('b), array:llarray('a)) : list('b) =
    rec aux(f, array, i, acc) =
      if i < 0 then
        acc
      else
        acc =
          match f(get(array, i)) with
          | {none} -> acc
          | {some = v} -> [v|acc]
        aux(f, array, i-1, acc)
    aux(f, array, length(array) - 1, [])

  @private
  map_aux(a,na,f,i) =
   if i == 0 then na
   else
     v = f(i,get(a,i))
     do set(na,i,v)
     map_aux(a,na,f,i-1)

  /**
   * Remap an array using a function taking position and old value and returning new value
   * The order in which the function will be called is undefined
   */
  mapi(a)(f) =
   s = size(a)
   if s==0 then empty
   else
    na = create(s,f(0,get(a,0)))
    map_aux(a,na,f,s-1)

  @private
  of_list_map_aux(a,f,l,i) =
    match l
    [] -> a
    [hd|tl] ->
      do set(a,i,f(i,hd))
      of_list_map_aux(a,f,tl,i+1)

  /**
   * Same as mapi but with a list as input
   */
  of_list_mapi(l)(f) =
   match l
    [] -> empty
    [hd|tl] ->
      a = create(List.length(l),f(0,hd))
      of_list_map_aux(a,f,tl,1)

  /**
   * List to llarray conversion
   */
  @expand
  of_list(l) = l=l of_list_mapi(l)(_i,x->x)

  @private
  fold_mapi_aux(a,na,f,n,i,acc) =
   if i==n then (na,acc)
   else
     (na_i,acc) = f(i,get(a,i),acc)
     do set(na,i,na_i)
     fold_mapi_aux(a,na,f,n,i+1,acc)


  /**
    * Same as mapi but remapping an element use and propagate an accumulator
    * the mapping and propagation of accumulator is done following index order
    */
  fold_mapi(a:llarray('a),acc:'acc)(f:int,'a,'acc->('b,'acc)):(llarray('b),'acc) =
    s = size(a)
    if s==0 then (empty,acc)
    else
     (na_0,acc) = f(0,get(a,0),acc)
     na = create(s,na_0)
     fold_mapi_aux(a,na,f,s,1,acc)

  @private
  aux_for_all(a,f,i) =
     a_i = get(a,i)
     if i==0 then f(a_i)
     else f(a_i) && aux_for_all(a,f,i-1)

   @expand
  for_all(a)(f) =  aux_for_all(a,f,size(a)-1)

}}
