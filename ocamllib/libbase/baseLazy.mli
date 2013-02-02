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
(** {6 Ocaml Stdlib} *)

type 'a t = 'a lazy_t


exception Undefined

external force : 'a t -> 'a = "%lazy_force"
val force_val : 'a t -> 'a

val lazy_from_fun : (unit -> 'a) -> 'a t
val lazy_from_val : 'a -> 'a t

val lazy_is_val : 'a t -> bool

(** {6 Mlstate} *)

(**
   Goes throw all nested values contained in any block,
   and force all lazy values.
*)
val deep_force : Obj.t -> Obj.t
