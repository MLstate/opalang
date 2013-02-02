(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)
(**
   Manipulation of mutable list.
*)

(**
   Fee free to add any persistent function from [List] in this module,
   in the imperative version.
*)

type 'a t
val create : unit -> 'a t

(**
   O(n)
*)
val to_list : 'a t -> 'a list

(**
   O(1)
*)
val to_rev_list : 'a t -> 'a list

val clear : 'a t -> unit
val add : 'a t -> 'a -> unit

(**
   O(n)
*)
val mem : 'a -> 'a t -> bool

(**
   tail rec, O(n)
*)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(**
   tail rec, O(n)
*)
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

(**
   tail rec, O(n)
*)
val length : 'a t -> int

(**
   tail rec, O(n)
*)
val iter : ('a -> unit) -> 'a t -> unit

(**
   tail rec, O(n)
*)
val rev_iter : ('a -> unit) -> 'a t -> unit

val is_empty : 'a t -> bool

(**
   appends the reversed list at the end of the mutable list
*)
val rev_append : 'a t -> 'a list -> unit

(**
   appends the list at the end of the mutable list
*)
val append : 'a t -> 'a list -> unit

val from_list : 'a list -> 'a t
val from_rev_list : 'a list -> 'a t
val reset_from_list : 'a t -> 'a list -> unit
val reset_from_rev_list : 'a t -> 'a list -> unit
val update : ('a list -> 'a list) -> 'a t -> unit
