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
(**
   Manipulation of non empty lists
*)

type 'a t = 'a * 'a list

(** unwrap a value of type t *)
val unwrap : 'a t -> 'a list

(** wrap a list
   @raise Invalid_argument if the list is empty
*)
val wrap : 'a list -> 'a t

(**
   Build an new HdList from an element
*)
val singleton : 'a -> 'a t

val length : 'a t -> int
val hd : 'a t -> 'a
val tl : 'a t -> 'a list

val last : 'a t -> 'a

val nth : 'a t -> int -> 'a
val rev : 'a t -> 'a t
val iter : ('a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t

val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc

val reduce_left : ('a -> 'a -> 'a) -> 'a t -> 'a
val reduce_left : ('a -> 'a -> 'a) -> 'a t -> 'a

val fold_left_map : ('acc -> 'a -> 'acc * 'a) -> 'acc -> 'a t -> 'acc * 'a t
