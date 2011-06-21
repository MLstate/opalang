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
(** This module provides a function to register source code of the application.

    Calls to this BSL function are inserted by the compiler in the compilation
    pass RegisterAppSrcCode.

    It is subsequently used to present application's source code at the URL:
    _internal_/src_code.


    @author Adam Koprowski
  *)

let (|>) = InfixOperator.(|>)

let src : (string, bool * string) Hashtbl.t = Hashtbl.create 20

let aux_register_src_code special fn content =
  Hashtbl.replace src fn (special, content)

(** Registers a file named [fn] with its source code [content], as part
    of the application sources.
    This call is inserted by the compiler into the application. *)
##register [opacapi] register_src_code : string, string -> void
let register_src_code fn content =
  aux_register_src_code false fn content

(** As [register_src_code], but marks the file as special. *)
##register [opacapi] register_special_src_code : string, string -> void
let register_special_src_code fn content =
  aux_register_src_code true fn content

(** Returns the list of registered files (see [register_src_code]),
    constituting the sources of the application. *)
##register get_file_list : -> opa[list(string)]
let get_file_list () =
  let cmp_srcs src1 src2 =
    match src1, src2 with
    | (false, _), (true, _) -> -1
    | (true, _), (false, _) -> 1
    | (_, s1), (_, s2) -> String.compare s1 s2
  in
  let caml_list =
    Hashtbl.fold (fun x (s, _) xs -> (s, x)::xs) src []
     |> List.sort cmp_srcs
     |> List.map snd
  in
  BslNativeLib.caml_list_to_opa_list Base.identity caml_list

(** Returns the content of the file [fn], or [""] if such files
    was not registered with [register_src_code]. *)
##register get_file_content : string -> string
let get_file_content fn =
  try
    let _, content = Hashtbl.find src fn in
    content
  with
    Not_found -> ""
