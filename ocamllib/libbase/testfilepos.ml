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
   Application for testing citations.
   @author Mathieu Barbin
*)

(* depends *)
module Arg = Base.Arg
module String = Base.String

(**
   This is a unit test for FilePos, but it can seen as a tool (installed in MLSTATELIBS/bin)
*)

let def = FilePos.default_options

let truncate_lines = ref (def.FilePos.truncate_lines)
let lines_before = ref (def.FilePos.lines_before)
let lines_after = ref (def.FilePos.lines_after)
let lines_between = ref (def.FilePos.lines_between)
let color = ref (def.FilePos.color)
let max_length_citation = ref (def.FilePos.max_length_citation)

let defi = function None -> "none" | Some i -> string_of_int i

let int2 s =
  try
    let a, b = String.split_char ',' s in
    int_of_string a, int_of_string b
  with
  | Failure "int_of_string" ->
      prerr_endline "Expected format for --range : %d,%d (e.g. 35,67)";
      exit 2

let positions, options =

  let positions = ref (FilePos.nopos "nothing") in
  let current_file = ref "default" in
  let add_file file =
    current_file := file ;
    FilePos.add_file file (File.content file)
  in
  let add_range s =
    let start, stop = int2 s in
    let pos = FilePos.make_pos !current_file start stop in
    positions := FilePos.merge_pos !positions pos
  in
  let add_line line =
    let pos = FilePos.make_pos_from_line !current_file line in
    positions := FilePos.merge_pos !positions pos
  in

  let _ = Arg.parse (Arg.align (Arg.sort
   [

     "--file", Arg.String add_file, "%s Add a file for citation";

     "--range", Arg.String add_range, "%d,%d Add a char range to the current file";

     "--line", Arg.Int add_line, "%d Add a single line of the current file";

     "--truncate-lines", Arg.Int (fun i -> truncate_lines := Some i),
     Printf.sprintf "%%d Truncate lines longer than an int(default is %s)" (defi !truncate_lines);

     "--lines-before", Arg.Set_int lines_before,
     Printf.sprintf "%%d Set the number of lines before the first citation (default is %d)" !lines_before;

     "--lines-after", Arg.Set_int lines_after,
     Printf.sprintf "%%d Set the number of lines after the last citation (default is %d)" !lines_after;

     "--lines-between", Arg.Set_int lines_between,
     Printf.sprintf "%%d Set the number of lines between 2 citations (default is %d)" !lines_between;

     "--color", Arg.Symbol (("none"::Ansi.symbols),
                            (fun s -> color := Ansi.color_of_string s)),
     " Choose a color for the cited ranges";

     "--max-length-citation", Arg.Int (fun i -> max_length_citation := Some i),
     Printf.sprintf "%%d Set a maximal for the length of a citation (default is %s)" (defi !max_length_citation);

   ]))
    (fun _ -> prerr_endline "anonymous argument not allowed\ntry --help"; exit 2)
   (Printf.sprintf "%s: citation extractor\nUsage: %s [options]\nOptions:" Sys.argv.(0) Sys.argv.(0))
  in
  let options = { FilePos.
    truncate_lines = !truncate_lines ;
    lines_before = !lines_before ;
    lines_after = !lines_after ;
    lines_between = !lines_between ;
    color = !color ;
    max_length_citation = !max_length_citation ;
  } in
  !positions, options

let _ =
  FilePos.citation ~options Format.std_formatter positions
