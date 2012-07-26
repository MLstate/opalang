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

(*Print short Location TODO : merge it with same code from typer/w_Misc
  and move it to filePos*)


let to_string_range_short print_fname fe r =
  let start = r.FilePos.start in
  let stop = r.FilePos.stop in
  try(
    let (line1, col1) = FilePos.get_pos fe start in
    let (line2, col2) = FilePos.get_pos fe stop in
    if print_fname
     then (Printf.sprintf "%s:(%d:%d-%d:%d)\t" fe line1 col1 line2 col2)
     else (Printf.sprintf "(%d:%d-%d:%d)" line1 col1 line2 col2)
    )
  with
  | Not_found ->
      (Printf.sprintf "%s: (%d-%d)\t" fe start stop)

let pp_filerange_short print_fname filename fmt r =
  let str = to_string_range_short print_fname filename r in
  if print_fname 
   then Format.fprintf fmt "%s" (str ^ String.make (20 - String.length str) ' ')
   else Format.pp_print_string fmt str

let pp_filerange_short print_fname fmt {FilePos.filename=filename; FilePos.ranges=ranges} =
  let ranges = HdList.unwrap ranges in
  BaseFormat.pp_list "@\n" (pp_filerange_short print_fname  filename) fmt ranges

let pp_pos_short fmt (pos, print_fname) = 
  match pos with 
  | FilePos.Builtin pass -> Format.fprintf fmt "<no position available (%s)>" pass
  | FilePos.Files (files, _) ->
      let files = HdList.unwrap files in
      BaseFormat.pp_list "@\n" (pp_filerange_short print_fname) fmt files

let pos_to_short_string p = BaseFormat.to_string pp_pos_short p

type annot = {
  pos : FilePos.pos ;
  notes: int ;
}

let pos annot = annot.pos

type 'a label = 'a * annot
