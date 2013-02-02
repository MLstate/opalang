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
(**
   The identifier used in the js ast
*)

(**
   The documentation of these type is in JsAst
*)

type native_ident = [ `global of bool | `local ]

type t =
  | ExprIdent of Ident.t
  | Native of native_ident * string

val compare : t -> t -> int
val equal : t -> t -> bool
val to_string : t -> string
val hash : t -> int
val stident : t -> string

(** Returns a detailed representation of the identifier *)
val inspect : t -> string

(** Returns true iff the identifier is global *)
val is_native_global : t -> bool
