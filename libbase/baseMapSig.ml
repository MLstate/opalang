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
module type S =
sig
  type key
  type +'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val singleton : key -> 'a -> 'a t

  (**
     Decons un non empty map
  *)
  val decons : 'a t -> 'a t * key * 'a * 'a t

  (**
     Overwrites the old value at that key.
  *)
  val add : key -> 'a -> 'a t -> 'a t

  (**
     Fails raising [Invalid_argument "Base.Map.safe_add"]
     @raise Invalid_argument if the key is already present
  *)
  val safe_add : key -> 'a -> 'a t -> 'a t

  (**
     [replace key maping t]
     Call the maping function from the previous value contained at that key,
     and add the resulting value into the map.
  *)
  val replace : key -> ('a option -> 'a) -> 'a t -> 'a t

  (**
     @raise Not_found if the key is not present
  *)
  val find : key -> 'a t -> 'a

  val findi : key -> 'a t -> key * 'a

  (**
     No exception if the key is not present.
  *)
  val remove : key -> 'a t -> 'a t

  (**
     @raise Not_found if the binding to update is not present
  *)
  val update : key -> ('a -> 'a) -> 'a t -> 'a t

  (**
     [update_default key updater default map]
     Looks for the value of [key] in [map]
     - if it is present then, replace it with [updater value]
     - or else add the binding [key] -> [default] to the map
  *)
  val update_default : key -> ('a -> 'a) -> 'a -> 'a t -> 'a t

  val mem : key -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val rev_iter : (key -> 'a -> unit) -> 'a t -> unit

  (**
     Returns an arbitrary element of the map.
     @raise Not_found on the empty list
  *)
  val choose : 'a t -> (key * 'a)
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold_map : (key -> 'a -> 'c -> 'c * 'b) -> 'a t -> 'c -> 'c * 'b t
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    (** [fold_range_compare f map kmin kmax acc] Like fold range but
        [kmin] and [kmax] are not key type and the range selection is
        computed with given [compare] function.

        Note : in major cases use [fold_range] ([fold_range_compare
        Ord.compare]).

        Beware : forall key. [compare kmin key <= compare kmax
        key]. *)
    val fold_range_compare : ('c -> key -> int) ->
      (key -> 'a -> 'b -> 'b) -> 'a t -> 'c -> 'c -> 'b -> 'b

    (** [fold_range f map kmin kmax acc] Fold with [f] on [map]
        beetween [kmin] and [kmax] includes. [acc |> f kmin vmin |>
        ... |> f kmax vmax]. *)
  val fold_range: (key -> 'a -> 'b -> 'b) -> 'a t -> key -> key -> 'b -> 'b
  val fold_length :
    start:key -> length:int
    -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** If [length] is negative, [start] is the end.
        If [length] is zero the fold is void (accumulator returned).
        Time cost is O(log n + length) and memory cost O(log n),
        both for balanced trees. Constants are the same as in
        DFS restricted to the parts of the tree with the sought keys,
        plus all their ancestors, plus a thin margin. *)

  val fold_rev : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val filter_keys : (key -> bool) -> 'a t -> 'a t
  val filter_val : ('a -> bool) ->'a t -> 'a t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  (**
     Check that both map contains the same keys,
     and that the binded value is the same,
     using the equality function for values
  *)
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  module Iter :
    (IterSig.S with type +'a element = key * 'a and type +'a structure = 'a t)
  module RevIter :
    (IterSig.S with type +'a element = key * 'a and type +'a structure = 'a t)
  val nearest : key ->' a t -> key * 'a

  (**
     Find the naearest smaller entry in a map.
     @raise Not_found if there is no smaller key in the map
  *)
  val find_inf : key -> 'a t -> key * 'a

  (**
     Find the naearest greater entry in a map.
     @raise Not_found if there is no smaller key in the map
  *)
  val find_sup : key -> 'a t -> key * 'a

  val from_list : (key * 'a) list -> 'a t

  (**
     Return an assoc list consitued of all bindings contained
     in the map.
     <!> The order of elements in the returned list is unspecified.
  *)
  val to_list : 'a t -> (key * 'a) list

  (**
     Same than [to_list] but with the guaranty that the assoc list is
     sorted by keys, using the order used in the map.
     The returned list is in increasing order of keys.
  *)
  val ordered_list : 'a t -> (key * 'a) list

  (**
     Same than [List.rev (ordered_list map)] but more efficient.
     The returned list is in decreasing order of keys.
  *)
  val rev_ordered_list : 'a t -> (key * 'a) list

  val keys : 'a t -> key list
  val elts : 'a t -> 'a list
  val random : 'a t -> key * 'a
  val size : 'a t -> int
  val height : 'a t -> int
  val merge_i : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  (**
     Fails raising [Invalid_argument "Base.Map.safe_add"].
     @raise Invalid_argument is case of conflict
  *)
  val safe_merge : 'a t -> 'a t -> 'a t
  val findi_opt : key -> 'a t -> (key * 'a) option
  val find_opt : key -> 'a t -> 'a option
  val min : 'a t -> key * 'a
  val max : 'a t -> key * 'a
  val rename : (key -> key) -> 'a t -> 'a t

  (** Binding with Format for printing.
      The first argument is a separator which will be
      printed after each elt of the set.
      If you does not want to see the separator after the last element, use an intermediate
      list traduction. *)
  val pp : (unit, Format.formatter, unit) format -> (Format.formatter -> key -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  (** exporting compare on key from Arg.compare *)
  val compare_key : key -> key -> int

  (** [diff m1 m2] returns the submap of [m1] containing all the keys that
      are in [m1] but not in [m2]
      ie it computes [m1 \ m2]
  *)
  val diff : 'a t -> _ t -> 'a t

  (**
     [diff2 m1 m2 m3] computes [m1 \ (m2 \ m3)], ie the submap of [m1]
     containing all the keys that are in [m3] or not in [m2]
  *)
  val diff2 : 'a t -> _ t -> _ t -> 'a t

  (**
     Optimized initialization for huge maps, to avoid temporary unused balancing.
     The key array should be sorted in increasing order, using the same order than the set.
     The value array should have the same length than the key array.
     {[
     [| key0 ; key1 ; key2 |] [| val0 ; val1 ; val2 |]
     ]}

     <!> Unoptimized on IntMap
  *)
  val from_sorted_array : key array -> 'a array -> 'a t
end
