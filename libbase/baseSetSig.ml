(*
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
*)
module type S =
sig
  type elt
  type t
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val add_list : elt list -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t

  (**
     Ensures that sets are disjoints. Else, fails raising
     [Invalid_argument "Base.Set.safe_union"]
     @raise Invalid_argument if the sets are not disjoints.
  *)
  val safe_union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool

  (**
     [subset s1 s2] tests whether the set s1 is a subset of the set s2 .
  *)
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_rev : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val map : (elt -> elt) -> t -> t
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> elt list
  val from_list : elt list -> t
  val min_elt : t -> elt
  val max_elt : t -> elt

  (**
     Returns an arbitrary element of the set.
     @raise Not_found on the empty set
  *)
  val choose : t -> elt

  (**
     Returns an arbitrary element of the set if it is not empty
  *)
  val choose_opt : t -> elt option

  (**
     If the two set are different, give back an element in the left set not in
     the right one, or in the right one not in the left one, or None
     We cannot use there [elt Base.either option], because this would create
     a circular build. If needed, we can return a variant indicating where
     the element was found.
  *)
  val example_diff : t -> t -> elt option
  val split : elt -> t -> t * bool * t
  val draw : t -> elt * t
  val size : t -> int
  val complete : (elt -> elt -> bool) -> elt -> t -> t

  (** Binding with Format for printing.
      The first argument is a separator which will be
      printed after each elt of the set.
      If you does not want to see the separator after the last element, use an intermediate
      list traduction. *)
  val pp : (unit, Format.formatter, unit) format -> (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit

  (** exporting the Arg.compare *)
  val compare_elt : elt -> elt -> int

  (**
     Optimized initialization for huge maps, to avoid temporary unused balancing.
     The array should be sorted in increasing order, using the same order than the set.
     <!> Unoptimized on IntMap
  *)
  val from_sorted_array : elt array -> t
end
