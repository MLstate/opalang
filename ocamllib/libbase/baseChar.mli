(*
    Copyright © 2011, 2012 MLstate

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
(** {6 From Ocaml Standard Library} *)

external code : char -> int = "%identity"
val chr : int -> char
val escaped : char -> string
val lowercase : char -> char
val uppercase : char -> char
type t = char
val compare : t -> t -> int
external unsafe_chr : int -> char = "%identity"

(** {6 Extra API} *)

val equal_insensitive : char -> char -> bool
val compare_insensitive : char -> char -> int
val width : char -> int
val is_digit : char -> bool
val is_lower : char -> bool
val is_upper : char -> bool
val is_alpha : char -> bool
(* val pred : char -> char *)
(* val succ : char -> char *)

(**
   A space char : {[ ' ' ; '\t' ; '\r' ; '\n' ]}
*)
val is_space : char -> bool
val hexa_value : char -> int
val cache : (char->bool) -> (char->bool)
