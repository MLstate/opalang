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

module SAP = SurfaceAstPasses
module SAH = SurfaceAstHelper
module P = Passes

let noloc v = v, SurfaceAstCons.Label.builtin ()

let register_code ~special files srcs =
  let add_file map f =
    StringMap.add f.P.inputFile_filename f.P.inputFile_org_content map
  in
  let sources = List.fold_left add_file StringMap.empty srcs  in
  (* Puts:
     _ = (%%BslAppSrcCode.register_src_code%%)(fn, code)
     at the beginning of [code].
  *)
  let register_src_code f_name =
    let f_content =
      match StringMap.find_opt f_name sources with
      | Some content -> content
      | None -> OManager.error "Internal error: cannot find source code for @{<bright>%S@}" f_name
    in
    let bypass = SAH.bypass (
      if special then
        Opacapi.Opabsl.BslAppSrcCode.register_special_src_code
      else
        Opacapi.Opabsl.BslAppSrcCode.register_src_code
    )
    in
    let args_tuple = [ ("f2", noloc (SAH.string f_name))
                     ; ("f3", noloc (SAH.string f_content))
                     ]
    in
    let register = SAH.apply (noloc bypass, noloc args_tuple) in
    let nv = SAH.newval ([noloc SAH.patany, noloc register], false) in
    noloc nv
  in
  let register_file f =
    { f with
        SAP.parsedFile_lcode =
          register_src_code f.SAP.parsedFile_filename :: f.SAP.parsedFile_lcode
    }
  in
  List.map register_file files
