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
(* CF mli *)

type filename       = string
type line_number    = int

let private_filename = ref ( "<unnamed file>" : filename )
let private_line_number = ref ( (-1) : line_number )

(* The last directive is set by BslRegisterLib,
   so that the parser can make some context check *)
let private_last_directive = ref None
let set_last_directive directive = private_last_directive := Some directive
let get_last_directive () = !private_last_directive

let make_pos () = FilePos.make_pos_from_line !private_filename !private_line_number

let pp_citation fmt () =
  let pos = make_pos () in
  if FilePos.is_empty pos then
    Format.fprintf fmt "File \"%s\", line %d@\n" !private_filename !private_line_number
  else
    FilePos.citation fmt pos

let warning fmt =
  OManager.warning ~wclass:WarningClass.bsl_register ("@\n%a"^^fmt) pp_citation ()

let error fmt =
  OManager.printf "%a" pp_citation () ;
  OManager.error fmt

module TypeVar =
struct
  let tbl = Hashtbl.create 10

  let var name =
    try Hashtbl.find tbl name with
    | Not_found ->
        let typevar = BslTypes.TypeVar.next ~name () in
        Hashtbl.add tbl name typevar;
        typevar

  let fresh () =
    BslTypes.TypeVar.next ()

  let reset () =
    Hashtbl.clear tbl
end

let init_file ~filename =
  private_last_directive := None;
  private_filename := filename

let init_line ~line_number =
  TypeVar.reset ();
  private_line_number := line_number
