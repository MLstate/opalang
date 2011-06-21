(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)

(**
   Some functions used by compilation passes.
*)

(* -- *)

let slash2filename f =
  let n = String.length f in
  let buf = Buffer.create n in
    for i = 0 to n-1 do
      if f.[i] = '/' then
        Buffer.add_string buf File.path_sep
      else Buffer.add_char buf f.[i]
    done;
    Buffer.contents buf

 (** new version of StaticsInclude : see file ofile.ml *)
let file_content_and_embedded ?(warn_no_embedded=false) f = match StaticsInclude.get_file f with
| Some c -> c
| None ->  (if warn_no_embedded then prerr_endline (Printf.sprintf "%s is not embedded" f)); File.content (PathTransform.string_to_mysys f)

let output fname s =
  if not (File.output fname s) then
    failwith (Printf.sprintf "Can't write file \"%s\" (current path is \"%s\")" fname (Sys.getcwd ()))
