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

type t
val empty : t
val length : t -> int
val get : t -> int -> char
val set : t -> int -> char -> unit
val create : int -> t
val make : int -> char -> t
val copy : t -> t
val sub : t -> int -> int -> t
val unsafe_sub : t -> int -> int -> t
val fill : t -> int -> int -> char -> unit
val blit : t -> int -> t -> int -> int -> unit
val concat : t -> t list -> t
val iter : (char -> unit) -> t -> unit
val escaped : t -> t
val index : t -> char -> int
val rindex : t -> char -> int
val index_from : t -> int -> char -> int
val rindex_from : t -> int -> char -> int
val contains : t -> char -> bool
val contains_from : t -> int -> char -> bool
val rcontains_from : t -> int -> char -> bool
val uppercase : t -> t
val lowercase : t -> t
val capitalize : t -> t
val uncapitalize : t -> t
val compare: t -> t -> int
val unsafe_get : t -> int -> char
val unsafe_set : t -> int -> char -> unit
val unsafe_blit : t -> int -> t -> int -> int -> unit
val unsafe_fill : t -> int -> int -> char -> unit

val of_string : string -> t
val to_string : t -> string
val export : t -> string * int * int
val import : string * int * int -> t
val widen : t -> unit
val normalize : t -> t
val real_size : t -> int
val set_size : t -> int -> t
val rebase : t -> unit
