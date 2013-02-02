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
