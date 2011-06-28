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
(** Simplier version of ofile + retourned type is an option (safer) *)
open Base


let extension = "of"
let error s = prerr_endline (Printf.sprintf "[!] ofile : %s" s); exit 1
let do_verbose = ref false
let verbose s =
  if !do_verbose then prerr_endline (Printf.sprintf "ofile : %s" s)

let safe_content file =
  try (String.escaped (File.content file))
  with Unix.Unix_error(e,s0,s1) -> error (Printf.sprintf "cannot load \"%s\" from \"%s\" [%s,%s,%s]" file (Sys.getcwd ()) (Unix.error_message e) s0 s1)

let win2unix f =
  let res = String.copy f in
  let n = String.length res in
    for i = 0 to n-1 do
      if res.[i] = '\\' then res.[i] <- '/'
    done;
    res

let iter_of path file =
  if File.extension file <> extension
  then error (Printf.sprintf "input file \"%s\" must have .%s extension" file extension)
  else
    if Sys.file_exists file && not (Sys.is_directory file)
    then
      let buf = FBuffer.create 1024 in
      let buf = FBuffer.addln buf "let get_file = function" in
      let files_list = List.rev
        (File.lines_fold
           (fun acc line -> let t = String.trim line in if t = "" then acc else t::acc)
           [] file) in
      let portable_path _path file =
        let path_file =(*_path^*)file in
        (Mlstate_platform.platform_dependent
         ~unix:PathTransform.string_to_unix
         ~windows:PathTransform.string_to_windows ()) path_file
      in
      let buf = List.fold_left
        (fun buf t -> FBuffer.addln buf (Printf.sprintf "  | %S -> Some \"%s\"" t (safe_content (portable_path path t) ))) buf files_list in
      let buf = FBuffer.addln buf "  | _ -> None\n" in
      let buf = FBuffer.addln buf "let file_list =" in
      let buf = FBuffer.addln buf (String.concat_map ~left:"[" ~right:"]" "; " (fun f -> Printf.sprintf "%S" f) files_list) in
      let out = (File.chop_extension file)^".ml" in
      if File.output out (FBuffer.contents buf)
      then verbose (Printf.sprintf "write file \"%s\"" out)
      else error (Printf.sprintf "cannot write file \"%s\" from \"%s\"" out (Sys.getcwd ()))

let _ =
  (** please, keep the possibility of this option for other potential users of this generic application *)
  let path = ref "" and empty = ref true in
  Arg.parse
    [ "-v", Arg.Set do_verbose, " verbose";
      "-path", Arg.Set_string path, "<dir> prefix directory for files"
    ]
    (fun t -> iter_of !path t; empty := false)
    (Printf.sprintf "%s : embedded file ocaml preprocessor" Sys.argv.(0));
  if !empty
  then error (Printf.sprintf "no input files (give me some \"%s\" file)" extension)
