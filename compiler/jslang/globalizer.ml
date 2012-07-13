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
   Command line wrapper for JsUtils.export_to_global_namespace.

   This is a temporary hack to use the Opa NodeJs runtime as a
   package. It should be removed when the runtime is fully converted.

   @author Arthur Azevedo de Amorim
*)

module List = BaseList

type path = string

let read_cmd_line_args (args : path list) =
  let rec aux files output args =
    match args with
    | [] when List.is_empty files ->
      `ko "No input files"
    | [] -> (
      match output with
      | Some output -> `ok (files, output)
      | None -> `ko "No output file given"
    )
    | ["-o"] -> `ko "Missing output"
    | "-o" :: arg :: args -> (
      match output with
      | Some _ -> `ko "Multiple outputs"
      | None -> aux files (Some arg) args
    )
    | arg :: args -> aux (arg :: files) output args
  in
  aux [] None args

let process formatter (files : (path * JsAst.code) list) =
  List.iter (fun (filename, content) ->
    Format.fprintf formatter "// FILE: %s\n%a\n" filename
      JsPrint.pp_min#code content
  ) files

let die message =
  Printf.eprintf "error: %s\n" message; exit 1

let _ =
  let args = List.tl (Array.to_list Sys.argv) in
  match read_cmd_line_args args with
  | `ko error -> die error
  | `ok (files, output) ->

    (* Read files and reexport their identifiers *)
    let files = List.map (fun filename ->
      let content = File.content filename in
      let content =
        try
          JsParse.String.code ~throw_exn:true content
        with
          _ -> die "Could not read input"
      in
      (filename, JsUtils.export_to_global_namespace content)
    ) files
    in

    (* Output nodejs package *)
    File.remove_rec output;
    if not (File.check_create_path output) then
      die "Couldn't create output path";
    let main_path = Filename.concat output "main.js" in
    let package_json_path = Filename.concat output "package.json" in
    let package_desc = JsUtils.basic_package_json output "main.js" in
    let output_result = File.pp_output package_json_path
      Format.pp_print_string package_desc
    in
    begin match output_result with
    | None -> ()
    | Some error -> OManager.error "Couldn't create package: %s\n" error
    end;
    match File.pp_output main_path process files with
    | None -> ()
    | Some error -> OManager.error "Couldn't create package: %s\n" error
