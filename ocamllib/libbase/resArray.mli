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
exception MaxSize
exception UnknownCell

type 'a t = {
  mutable array : 'a array;
  init : 'a;
  mutable length : int;
}
val make : ?size:int -> int -> 'a -> 'a t
val create : ?size:int -> int -> 'a -> 'a t

(** [clear t] remove all elements *)
val clear : 'a t -> unit

val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val length : 'a t -> int
val real_length : 'a t -> int
val append : 'a t -> 'a t -> 'a t
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_left_i : ('a -> 'b -> i:int -> 'a) -> 'a -> 'b t -> 'a
val delete : 'a t -> int -> int -> unit
val insert : 'a t -> int -> 'a t -> unit
