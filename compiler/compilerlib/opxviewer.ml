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
type pos
type package = string * pos
type hash = string
type basename

type consistencycheck_t = {
  sources : hash;
  separated_mode : [ `partial | `full ];
  js_libs : (basename * hash) list;
  dependencies : (package * hash) list;
}

let string_of_hash x = string_of_int (Hashtbl.hash x)
let string_of_package (s,_) = s

let () =
  match Sys.argv with
  | [|_; s|] ->
      let s =
        if Filename.check_suffix s ".opx"
        || Filename.check_suffix s ".opx.broken" then
          s
        else
          s ^ ".opx" in
      let ic = open_in (s ^ "/consistency_check") in
      ignore (input_line ic);
      let v : consistencycheck_t = Marshal.from_channel ic in
      close_in ic;
      Printf.printf "sources: %s\n" (string_of_hash v.sources);
      Printf.printf "deps:\n";
      List.iter (fun (p,h) -> Printf.printf "  %s: %s\n" (string_of_package p) (string_of_hash h)) v.dependencies;
      flush stdout
  | _ ->
      Printf.printf "Give me one package\n%!";
      exit 1
