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
    @author Henri Binsztok,
    @author Gregoire Makridis
*)

type t =
  | IntKey of int
  | StringKey of string
  | ListKey of t array
  | VariableKey of int
      (** Only valid within a transaction; resolved at commit to fresh keys *)

type real = t

val newkey : t
val to_string : t -> string
val unsafe_make : real -> t
val succ : t -> t option
val pred : t -> t option
val max : t -> t -> t
val min : t -> t -> t
val equal : t -> t -> bool
val compare : t -> t -> int
val value : t -> real

exception Unqualified of string
