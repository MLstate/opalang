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
(** Just define the json type in Ocaml *)

(** Json type in OCaml *)
type json =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Void
  | Array of json list
  | Record of (string * json) list

(** {6 Passing Json to OPA} *)

(**
   In opa, you can use magic_unserialize to unserialize
   this structure, and build opa values.

   <!> For optimization purpose, for passing such values to opa,
   the fields in [Record] should be in decreasing order.

   This function checks that property
*)
let assert_for_opa_magic_unserialize json =
  let check_record list =
    let rec aux last = function
      | [] -> ()
      | (hd, _) :: tl ->
          if not (String.compare last hd > 0)
          then assert false
          else aux hd tl
    in
    match list with
    | [] | [_] -> ()
    | (hd, _) :: tl -> aux hd tl
  in
  let rec aux = function
    | Array json -> List.iter aux json
    | Record fields ->
        check_record fields ;
        List.iter (fun (_, json) -> aux json) fields
    | _ -> ()
  in
  aux json
