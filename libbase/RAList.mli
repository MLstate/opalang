(** result of a comparison *)
type comparison = Lesser | Equal | Greater
(** invalid access of the ith element when only n elements (Invalid_subscript(i,n)) *)
exception Invalid_subscript of (int*int)
(** invalid operation on empty ra_list *)
exception Empty

type 'a ra_list

(** classic operations of list, all O(1) *)
module AsList :
sig
  exception StopFold (*to implement fold until *)
  val is_empty : 'a ra_list -> bool
  val empty : 'a ra_list
  val cons : 'a -> 'a ra_list -> 'a ra_list
  val head : 'a ra_list -> 'a (* raise Empty *)
  val tail : 'a ra_list -> 'a ra_list (* raise Empty *)
  val fold : ('a->'b->'b) -> 'a ra_list -> 'b -> 'b (* catch StopFold to implement fold until *)
  val rev_fold : ('a->'b->'b) -> 'a ra_list -> 'b -> 'b (* catch StopFold to implement fold until *)
end

(** classic operations of functional array, O(ln(N)) *)
module AsArray :
sig
  val size : 'a ra_list -> int
  val get : 'a ra_list -> int -> 'a (* raise Invalid_subscript *)
  val update : 'a ra_list -> int -> 'a -> 'a ra_list (* raise Invalid_subscript *)
end

module AsMonotoniousList :
sig
  (** return the lastest entry that is <= hypothetical asked entry,
      via the comparison function which asked entry has already been passed to (facilitate comparison on projection) *)
  val get_lesser : ('a -> comparison ) -> 'a ra_list -> 'a (* raise Not_found *)
(*  val get  *)
end
