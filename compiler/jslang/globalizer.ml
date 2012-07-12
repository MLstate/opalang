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
   Command line wrapper for JsUtils.export_to_global_namespace

   @author Arthur Azevedo de Amorim
*)

type path = string

let read_cmd_line_args (args : path list) =
  let rec aux files output args =
    match args with
    | [] when List.empty files ->
      `ko "No input files"
    | [] -> (
      match output with
      | Some output -> `ok (files, output)
      | None -> `ko "No output file given"
    )
    | ["-o"] -> `ko "Missing output"
    | "-o" :: arg :: args -> (
      match output with
      | Some output -> `ko "Multiple outputs"
      | None -> aux files (Some arg)
    )
    | arg :: args -> aux (arg :: files) output
  in
  aux [] None args

let process formatter (files : path * JsAst.code list) =
  List.iter (fun (filename, content) ->
    formatter "// FILE: %s\n%a\n" filename
      JsPrint.pp_min#code content
  ) files

let die message =
  Printf.eprintf "error: %s\n" message; exit 1

let _ =
  let args = Sys.get_cmd () in
  match read_cmd_line_args args with
  | `ko error -> die error
  | `ok (files, output) ->
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
    match File.pp_output output process files with
    | None -> ()
    | Some error -> die error
