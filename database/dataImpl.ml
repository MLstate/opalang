(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
