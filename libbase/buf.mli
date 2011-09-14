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
   * Simple library, intended to have some of the properties of both
   * String and Buffer.  No automatic resize but it can be done manually.
**)

(** This type is concrete **)
type buf = { mutable str : string; mutable i : int; }

(** Common to String and Buffer **)
type t = buf
val length : buf -> int
val create : int -> buf
val sub : buf -> int -> int -> string

(** Compatibility with String *)
val make : int -> char -> buf
val get : buf -> int -> char
val unsafe_get : buf -> int -> char
val set : buf -> int -> char -> unit
val unsafe_set : buf -> int -> char -> unit
val copy : buf -> buf

(** Compatibility with Buffer **)
val nth : buf -> int -> char
val clear : buf -> unit
val reset : buf -> unit
val add_char : buf -> char -> unit
val add_string : buf -> string -> unit
val add_substring : buf -> string -> int -> int -> unit
val contents : buf -> string

(** Specifics **)
val empty : buf
val append : buf -> string -> int -> unit
val add_buf : buf -> buf -> unit
val of_string : string -> buf
val to_string : buf -> string
val resize : buf -> int -> unit
val extend : buf -> int -> unit
val real_length : buf -> int
val spare : buf -> int
