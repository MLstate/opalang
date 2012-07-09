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
module Format = BaseFormat
module Arg = BaseArg
module String = BaseString

let minify = ref false
let files = ref []
let cleanup = ref false

let () =
  let options =
    Arg.align (
      Arg.sort [
        "--minify", Arg.Set minify, "";
        "--clean-up", Arg.Set cleanup, "";
      ]
    ) in
  let anon s = files := s :: !files in
  let usage = "" in
  Arg.parse options anon usage;
  files := List.rev !files

let () =
  List.iter
    (fun filename ->
       let code = JsParse.File.code filename in
       let code =
         if !cleanup then
           Imp_CleanUp.clean ~use_shortcut_assignment:true code
         else
           code in
       let string = Format.to_string JsPrint.pp#code code in
       let string =
         if !minify then
           JsMinify.minify string
         else
           string in
       print_string string;
       Printf.printf "\n%!";
    ) !files
