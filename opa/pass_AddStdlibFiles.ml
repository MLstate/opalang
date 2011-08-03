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

let folder acc path =
  if filtering_files path then path::acc else acc

let of_basedir basedirs options =
  if not options.O.stdlib then []
  else (
    let folder acc ~name:_ ~path = folder acc path in
    List.fold_left (fun acc basedir -> acc @ (File.fold_dir_rec folder [] basedir)) [] basedirs
  )

let of_static_include options =
  if not options.O.stdlib then []
  else (
    List.fold_left folder [] StaticsInclude.file_list
  )
