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
(* CF mli *)

(* depends *)
module List = Base.List
module String = Base.String
let (|>) = InfixOperator.(|>)

(* alias for Constructors and Fields *)
module O = OpaEnv

(* type alias, for readability *)
type filename = string

(* -- *)

(*
  Filtering all the files from the stdlib.
  WARNING: only used with --no-embedded-stdlib
  In this case, the path starts with "stdlib" (see s3Passes.ml)
*)
let filtering_files path =
  let kept =
    (
      String.is_suffix ".opa" path
    )
  in
  let _ =
    #<If:ADD_STDLIB$islevel 1> if not kept     then OManager.printf "Rejecting : %S@." path #<End> ;
    #<If:ADD_STDLIB$islevel 2> if kept then OManager.printf "Keeping   : %S@." path #<End> ;
    #<If:ADD_STDLIB$minlevel 3>
      if not kept
      then OManager.printf "Rejecting : %S@." path
      else OManager.printf "Keeping   : %S@." path
    #<End>
  in
  kept

(*
  This is a way for helping the reordering to have in the first place in the code
  all the dependencies of the generated code by the compiler.
*)
let sort_stdlib =
  let order_map =
    [
      ("opa_compiler_interface_private.opa" , 0)
    ]
  |> StringMap.from_list
  in
  let proj s =
    let s = Filename.basename s in
    Option.default max_int (StringMap.find_opt (Filename.basename s) order_map)  in
  let cmp f1 f2 = compare (proj f1) (proj f2) in
  fun l -> List.sort cmp l

let folder acc path =
  if filtering_files path then path::acc else acc

let of_basedir basedirs options =
  if not options.O.stdlib then []
  else (
    let folder acc ~name:_ ~path = folder acc path in
    List.fold_left (fun acc basedir -> acc @ (File.fold_dir_rec folder [] basedir)) [] basedirs
  |> sort_stdlib
  )

let of_static_include options =
  if not options.O.stdlib then []
  else (
    List.fold_left folder [] StaticsInclude.file_list
  |> sort_stdlib
  )
