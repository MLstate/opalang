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
(*
 * This file is a standalone parser for the opa syntax
 * It is meant for debugging/profiling the parser
 *)

let show = ref false
let show_pos = ref false
let stop = ref false
let files = ref []
let () =
  Arg.parse
    ["--show", Arg.Set show, "show the parse tree"
    ;"--pos", Arg.Set show_pos, "show the positions when printing"
    ;"--stop", Arg.Set stop, "stop at the first file containing a parse error"]
    (fun s -> files := s :: !files)
    (Printf.sprintf "Usage: %s [options] files" Sys.argv.(0))

let () = OManager.CompilerAsLib.at_exit { OManager.at_exit = fun _ -> raise Exit }

let () =
  let code = ref 0 in
  let printer = if !show_pos then OpaPrint.string_and_pos else OpaPrint.string in
  let ppenv = Pprocess.fill_with_sysenv Pprocess.empty_env in
  let ppopt = Pprocess.default_options ppenv in
  let process = (Pprocess.process Pplang.opa_description ppopt) in
  List.iter
    (fun filename ->
       match File.content_opt filename with
       | None -> Printf.printf "%s is not a file, ignoring it\n%!" filename
       | Some content ->
           try
             let content = process content in
             let ast = OpaParser.code ~cache:false ~filename content in
             if !show then Format.printf "%s:@\n%a@." filename printer#code ast;
             Printf.printf "Parsed %s successfully\n%!" filename
           with
           | Exit ->
               (* the error message has been printed already *)
               incr code;
               if !stop then exit !code
    ) (List.rev !files);
  exit (min 255 !code)
