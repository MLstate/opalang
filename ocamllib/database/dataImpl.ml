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
(* depends *)

(* shorthands *)

(* debug *)
#<Debugvar:DEBUG_DB>

(* -- *)

type data =
  | Int of int
  | Text of string
  | Binary of string
  | Float of float
  | Unit

type t = data
type io = t

let empty = Unit

let to_string o =
  match o with
    | Int i -> string_of_int i
    | Text s -> "\"" ^ String.escaped (BaseString.limit 60 s) ^"\""
    | Binary s -> "BINARY (size " ^ string_of_int (String.length s) ^ ")"
    | Float f -> string_of_float f
    | Unit -> "()"

let get_string o =
  match o with
    | Int i -> string_of_int i
    | Text s | Binary s -> s
    | Float f -> string_of_float f
    | Unit -> ""

let index_fun = function
  | Text s -> Indexer.utf8_string StringMap.empty s
  | _ -> StringMap.empty
