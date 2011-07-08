(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)
val length : 'a list -> int
val empty : 'a list
val is_empty: 'a list -> bool
val hd : 'a list -> 'a
val tl : 'a list -> 'a list
val nth : 'a list -> int -> 'a
val rev : 'a list -> 'a list
val append : 'a list -> 'a list -> 'a list
val rev_append : 'a list -> 'a list -> 'a list
val rev_map_append : ('a -> 'b) -> 'a list -> 'b list -> 'b list
val rev_filter_map_append : ('a -> 'b option) -> 'a list -> 'b list -> 'b list
val tail_append : 'a list -> 'a list -> 'a list
  (**
     Tail recursive version of [append]
  *)

val concat : 'a list list -> 'a list
val flatten : 'a list list -> 'a list
val iter : ('a -> unit) -> 'a list -> unit
val map : ('a -> 'b) -> 'a list -> 'b list
val map_right : ('a -> 'b) -> 'a list -> 'b list
val rev_map : ('a -> 'b) -> 'a list -> 'b list
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val fold_right2 :
  ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
val for_all : ('a -> bool) -> 'a list -> bool
val exists : ('a -> bool) -> 'a list -> bool
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val mem : 'a -> 'a list -> bool
val memq : 'a -> 'a list -> bool
val mem_eq : eq: ('a -> 'a -> bool) -> 'a -> 'a list -> bool
val find : ('a -> bool) -> 'a list -> 'a
val find_map : ('a -> 'b option) -> 'a list -> 'b option
val filter : ('a -> bool) -> 'a list -> 'a list
val tail_filter : ('a -> bool) -> 'a list -> 'a list
val filteri : (int -> 'a -> bool) -> 'a list -> 'a list
val find_all : ('a -> bool) -> 'a list -> 'a list
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
val assoc : 'a -> ('a * 'b) list -> 'b
val assq : 'a -> ('a * 'b) list -> 'b
val mem_assoc : 'a -> ('a * 'b) list -> bool
val mem_assq : 'a -> ('a * 'b) list -> bool
val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
val split : ('a * 'b) list -> 'a list * 'b list

(**
   Combine two list into a single list of couple.
   Cf function [combine_opt] if you do not want to deal
   with Exceptions
   @raise Invalid_argument if the 2 list do not have the same length
*)
val combine : 'a list -> 'b list -> ('a * 'b) list

val sort : ('a -> 'a -> int) -> 'a list -> 'a list
val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
exception Empty

(**
   [subtract l1 l2] returns the list of all elements of [l1] that do not appear in [l2]
*)
val subtract : 'a list -> 'a list -> 'a list
(**
   [substract_eq ~eq l1 l2] returns the list of all elements of [l1] that do
   not appear in [l2] using the custom [~eq] comparaison function. *)
val substract_eq : eq: ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list

val subset : 'a list -> 'a list -> bool
val subset_eq : eq:('a -> 'a -> bool) -> 'a list -> 'a list -> bool
val iter_right : ('a -> 'b) -> 'a list -> unit
val iteri : ('a -> int -> unit) -> 'a list -> unit
val rev_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val map_with_tail : ('a -> 'b) -> 'a list -> 'b list -> 'b list
val for_alli : (int -> 'a -> bool) -> 'a list -> bool

(**
   [init i fct]
   <!> images of [fct] are computed from [i-1] to [0].
   Beware if [fct] makes any side effects, elts will not
   be in the correct order. Use function [side_effect_init]
*)
val init : int -> (int -> 'a) -> 'a list

(**
   same than [init] but works ok with function having side effects :
   images of [fct] are computed from [0] to [i-1], and the implementation
   contains a [List.rev]
*)
val side_effect_init : int -> (int -> 'a) -> 'a list

val take : int -> 'a list -> 'a list
val last : 'a list -> 'a
val drop : int -> 'a list -> 'a list
val extract_last : 'a list -> 'a list * 'a

(**
   [split_at n list]
   Take at most [n] elements of [list] in fst and put the rest in snd.
   Does not fail if the [list] has less than [n] elements, simply returns
   (list, []) in that case.
*)
val split_at : int -> 'a list -> 'a list * 'a list
val rev_filter : ('a -> bool) -> 'a list -> 'a list

(**
   @raise Invalid_argument if the sum of the elements of the first list is not the length of the second list
*)
val split_ats : int list -> 'a list -> 'a list list

val split_at_sep : ('a -> bool) -> 'a list -> 'a list list
val splice : int -> int -> 'a list -> 'a list -> 'a list
val fold_left_i : ('a -> 'b -> int -> 'a) -> 'a -> 'b list -> 'a
val fold_right_i : ('a -> int -> 'b -> 'b) -> 'a list -> 'b -> 'b
val fold : ('a -> 'a -> 'a) -> 'a list -> 'a

(**
    @deprecated use [List.concat_map]
*)
(*val collect : ('a -> 'b list) -> 'a list -> 'b list*)


val to_string : ('a -> string) -> 'a list -> string
(** Print on list format; i.e. [a;b;c;...] *)
val print : ('a -> string) -> 'a list -> string
val max : 'a list -> 'a
val min : 'a list -> 'a
val minmax : 'a list -> 'a * 'a
val argmax : 'a list -> 'a
val argmin : 'a list -> 'a
val remove_all : 'a -> 'a list -> 'a list
val remove_first : 'a -> 'a list -> 'a list
(* ************************************************************************** *)
(** {b Descr}: Removes the first occurrence of an element from a list, using a
    custom equality test and return the list with this element removed. The
    element to remove is assumed to be present in the list: if no occurrence of
    it is found, hence removed, this function raises [Not_found].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
val remove_first_or_fail_eq :
  eq: ('a -> 'a -> bool) -> 'a -> 'a list -> 'a list
val remove_last : 'a list -> 'a list
(** [replace elt rep lst] replaces all occurrences of [elt] by given list [rep]. If [elt] does not occur in [lst], returns [lst]. *)
val replace : 'a -> 'a list -> 'a list -> 'a list
val cons : 'a -> 'a list -> 'a list

(**
   * [choose_random l] returns a random element from list [l].
   * Throws [Invalid_argument] if [l] empty.
*)
val choose_random : 'a list -> 'a
val get_only_element: 'a list -> 'a
  (**
     If the list contains exactly one element, returns it
     or else raise Invalid_argument
  *)

(**
   Remove duplicates from a sorted list.
   The first occurrence is keeped, and the returned
   list is still sorted.
*)
val uniq : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list

(**
   Remove duplicates from a unsorted list.
   The returned list is not sorted, but does
   contains only the first occurrence of each element.
*)
val uniq_unsorted : ?cmp:('a -> 'a -> int) -> ?conflict:('a -> 'a -> unit) -> 'a list -> 'a list

(**
   [insert p e l] inserts element e at position p in list l
   positions starting from offset [0]
   @raise Empty if the end of the list comes before the insertion position
*)
val insert : int -> 'a -> 'a list -> 'a list

(**
   [insert_sorted x l] assumes [l] is sorted and inserts element [x] in [l]
   such that the result is still sorted.

   optional arguments:
   -[cmp]: the comparison used for sorting;
   -[conflict]: the function used when there is already an element equal to [x] in [l].
   by default, conflict will keep the 2 elements in the list.
*)
val insert_sorted : ?cmp:('a -> 'a -> int )-> ?conflict:('a -> 'a -> 'a list) ->'a -> 'a list -> 'a list

val filter_and_fold :
  ('a -> 'b -> 'a * bool) -> 'a -> 'b list -> 'a * 'b list
val memi : 'a -> 'a list -> int option (* same as pos_opt *)

(** flip is also known as transpose (think matrix) *)
val flip : 'a list list -> 'a list list

(**
   Same than [combine] but returns [None] instead of raising
   [Invalid_argument] if the 2 list do not have the same length.
*)
val combine_opt : 'a list -> 'b list -> ('a * 'b) list option
val assoc_opt : 'a -> ('a * 'b) list -> 'b option
val assq_opt : 'a -> ('a * 'b) list -> 'b option
val find_opt : ('a -> bool) -> 'a list -> 'a option

(**
   [findi p l] returns the index (position) of the first element
   that satisfy the predicate p
*)
val findi : ('a -> bool) -> 'a list -> int option

(**
   [find_i p l] returns a pair of the index of the first element
   that satisfy the predicate p, and this element
*)
val find_i : ('a -> bool) -> 'a list -> (int * 'a) option
val find_map : ('a -> 'b option) -> 'a list -> 'b option
val pos_opt : 'a -> 'a list -> int option (* same as memi *)
val filter_map : ('a -> 'b option) -> 'a list -> 'b list

val filter_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b list

val partition_map : ('a -> 'b option * 'c option) -> 'a list -> 'b list * 'c list

(** return the first some result of functions in a' list or none *)
val get_first_some : ('a -> 'b option) list -> 'a -> 'b option
val get_first_some_ar2 :
  ('a -> 'b -> 'c option) list -> 'a -> 'b -> 'c option
val fold_right_map :
  ('a -> 'b -> 'c * 'b) -> 'a list -> 'b -> 'c list * 'b
  (** foldl f [ 1, 2 , 3 ] acc =       acc |> f 1 |> f 2 |> f 3 *)
val foldl  : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
  (** foldr f [ 1, 2 , 3 ] acc =       acc |> f 3 |> f 2 |> f 1 *)
val foldr  : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc

(**
   {[foldl1 f [ 1, 2 , 3 ] = 1 |> f 2 |> f 3]}
   @raise Invalid_argument if the list is empty
*)
val foldl1 : ('a -> 'a -> 'a) -> 'a list -> 'a

(**
   {[foldr1 f [ 1, 2 , 3 ] = 3 |> f 2 |> f 1]}
   @raise Invalid_argument is the list is empty
*)
val foldr1 : ('a -> 'a -> 'a) -> 'a list -> 'a

(**
   {[fold_left1 f [ 1, 2 , 3 ] = (f(f 1 2) 3)]}
   @raise Invalid_argument if the list is empty
*)
val fold_left1 :  ('a -> 'a -> 'a) -> 'a list -> 'a

val fold_left_snd : ('acc -> 'elt -> 'acc) -> 'acc -> (_ * 'elt) list -> 'acc
  (**
     just like fold_left, except that you are given only the second element of
     the assoc list
     (useful for folding on the values of a record node in an ast for example)
  *)

(**This is like a [map] but returns physically the same list if the maped
   elt is physically the same for each element of the list. Tail recursive.
   Used for optimizing traversal constructions*)
val map_stable : ('a -> 'a) -> 'a list -> 'a list

(** This is a fold_left combined with a map. Tail recursive. *)
val fold_left_map :
  ((*accumulator*)'a -> 'b -> 'a * 'c) -> (*init*)'a -> 'b list -> 'a * 'c list

(** This is a fold_left combined with a rev_map. Tail recursive. *)
val fold_left_rev_map :
  ('acc -> 'input -> 'acc * 'output) -> 'acc -> 'input list -> 'acc * 'output list

(** Same than [fold_left_map] but returns physically the same list if the maped
    elt is physically the same for each element of the list. Tail recursive.
    Used for optimizing traversal constructions *)
val fold_left_map_stable :
  ((*accumulator*)'a -> 'b -> 'a * 'b) -> (*init*)'a -> 'b list -> 'a * 'b list
val fold_right_map_stable :
  ((*accumulator*)'a -> 'b -> 'a * 'b) -> (*init*)'a -> 'b list -> 'a * 'b list

val fold_left_filter_map :
  ('a -> 'b -> 'a * 'c option) -> 'a -> 'b list -> 'a * 'c list
val fold_left_map2 :
  ('a -> 'b -> 'c -> 'a * 'd) -> 'a -> 'b list -> 'c list -> 'a * 'd list

(**
   Like a fold_left_map, but the function returns a list for each element.
   At end, all the list are flattened. Tail rec.
*)
val fold_left_collect :
  ('a -> 'b -> 'a * 'c list) -> 'a -> 'b list -> 'a * 'c list
val fold_left_map_i :
  (int -> 'a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
val for_all2_same_length :
  ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

(**
   Tail rec
*)
val rev_concat_map : ('a -> 'b list) -> 'a list -> 'b list

(**
   Tail rec. (implemented with rev (rev_concat_map)
*)
val concat_map : ('a -> 'b list) -> 'a list -> 'b list (* same as collect *)

val rev_concat_map2 : ('a -> 'b -> 'c list) -> 'a list -> 'b list -> 'c list
val concat_map2 : ('a -> 'b -> 'c list) -> 'a list -> 'b list -> 'c list

val tail_concat : 'a list list -> 'a list
val tail_append_keep_length : 'a list -> 'a list -> int * int * 'a list
val tail_split : ('a * 'b) list -> 'a list * 'b list
val tail_map : ('a -> 'b) -> 'a list -> 'b list
val tail_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list (* raises Invalid_argument *)
val vmap : ('a -> 'b) -> ('c * 'a) list -> ('c * 'b) list
val imap : ('a -> 'b) -> ('a * 'c) list -> ('a * 'b) list
val rmap : ('a -> 'b) -> ('a * 'c) list -> ('b * 'c) list
val tail_vmap : ('a -> 'b) -> ('c * 'a) list -> ('c * 'b) list
val tail_imap : ('a -> 'b) -> ('a * 'c) list -> ('a * 'b) list
val tail_rmap : ('a -> 'b) -> ('a * 'c) list -> ('b * 'c) list
val rectangle_map : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val triangle_map : ('a -> 'a -> 'b) -> 'a list -> 'a list -> 'b list

val make_compare : ('a -> 'b -> int) -> 'a list -> 'b list -> int
  (**
     Lists are compared first by comparing their elements pair-wise
     and then, if one, is exhausted, by comparing their lengths
  *)

val option_like_merge :
  ('a list -> 'a list -> 'a list) -> 'a list -> 'a list -> 'a list

val filterbounds : 'a option * int -> ('b -> 'a) -> 'b list -> 'b list

(* ************************************************************************** *)
(** {b Descr}: Returns the value (second component) associated with the key
    (first component) equal to [x] in a list of pairs.
    Raises [Not_found] if there is no value associated with [x] in the list.
    Equality test is performed with the provided function [eq] instead of the
    general [=] function.                                                     *)
(* ************************************************************************** *)
val assoc_custom_equality: eq:('a -> 'b -> bool) -> 'b -> ('a * 'c) list -> 'c

(* ************************************************************************** *)
(** {b Descr}: Transforms a list of triplets into a triplet of lists.         *)
(* ************************************************************************** *)
val split3: ('a * 'b * 'c) list -> ('a list * 'b list * 'c list)

module MakeAssoc(S:Map.OrderedType) : sig
  type +'a t = (S.t * 'a) list
  val find : S.t -> 'a t -> 'a
  val find_opt : S.t -> 'a t -> 'a option
  val mem : S.t -> 'a t -> bool
  val remove : S.t -> 'a t -> 'a t

  (** input are assumed to be sorted
      ouput list is sorted
      duplicated keys are kept
  *)
  val sorted_merge : 'a t -> 'a t -> 'a t

  (** input are assumed to be sorted and not have duplicates keys
      output list is sorted and does not have duplicated keys
  *)
  val unique_sorted_merge : merge:(S.t * 'a -> S.t * 'a -> S.t * 'a) -> 'a t -> 'a t -> 'a t

  val sort : 'a t -> 'a t
end

module StringAssoc : module type of MakeAssoc(String)

(**
   The same as [map2] except that it stops when one list becomes
   empty, instead of throwing an error when the lists don't have
   the same length
   Tail recursive
*)
val partial_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

(**
   The same as [List.rev (List.partial_map2 f l1 l2)]
*)
val rev_partial_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

val fold_left_partial_map2 : ('acc -> 'a -> 'b -> 'acc * 'c) -> 'acc -> 'a list -> 'b list -> 'acc * 'c list
val rev_fold_left_partial_map2 : ('acc -> 'a -> 'b -> 'acc * 'c) -> 'acc -> 'a list -> 'b list -> 'acc * 'c list

val filter2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list * 'b list
