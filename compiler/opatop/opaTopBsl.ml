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
(* CF mli *)

module BSL = BslLib.BSL
module BPI = BslPluginInterface

type bypass_map = BSL.ByPassMap.t
type bypass     = BSL.ByPass.t

let cache_bymap = ref ( None : bypass_map option )

let bypass_map () =
  match !cache_bymap with
  | Some bymap -> bymap
  | _ ->
      (*
        This is the only case where we are allowed to call the plugins 'loaders'
        because in this case, they must contain functions pointers
      *)
      let loaders = BslPluginTable.finalize () in
      let iter loader =
        OManager.verbose "loading primitives from @{<bright>%S@}" loader.BPI.self_module_name ;
        BSL.RegisterInterface.dynload loader.BPI.dynloader
      in
      List.iter iter loaders ;
      let bymap = BSL.RegisterTable.build_bypass_map () in
      cache_bymap := Some bymap ;
      bymap

let find_opt key t = BSL.ByPassMap.find_opt t key

let bypass_typer bypass_map = BSL.ByPassMap.bypass_typer bypass_map

let lang = BslLanguage.mli

let eval bypass =
  let bslty = BSL.ByPass.definition_type bypass in
  let interpreted = BSL.ByPass.interpreted_implementation bypass ~lang in
  match interpreted with
  | None -> None
  | Some implementation -> Some (
      let obj = BSL.Implementation.dynamic_repr implementation in
      OpaTopValue.Proj.t_of_ocaml bslty (Obj.obj obj)
    )
