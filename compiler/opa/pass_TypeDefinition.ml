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
(* depends *)
module List = Base.List

(* shorthands *)
module Q = QmlAst


module S =
struct
  type t = QmlTypes.gamma
  let pass = "pass_TypeDefinition"
  let pp f _ = Format.pp_print_string f "<dummy>"
end

module R = ObjectFiles.Make(S)

let process_code register typerEnv code =
  let new_gamma = QmlTypes.Env.empty in
  let gamma = typerEnv.QmlTypes.gamma in
  let (gamma, stdlib) =
    (* during pre_linking, the whole gamma is loaded
       because dbGen loads the whole database schema *)
    let options_packages = ObjectFiles.compilation_mode() = `init in
    R.fold_with_name
      ~packages: options_packages
      ~deep:true (* need to go deep because you can depend directly on a unit
                  * that says type t = u
                  * when u is defined in another package saying type u = v
                  * when v is defined in another package etc.
                  *)
      (fun package (acc_gamma, acc_stdlib) gamma ->
         let gamma = QmlRefresh.refresh_gamma package gamma in
         let stdlib =
           if ObjectFiles.compiler_package package then
             QmlTypes.Env.append acc_stdlib gamma
           else acc_stdlib
         in (QmlTypes.Env.append acc_gamma gamma, stdlib))
      (gamma, QmlTypes.Env.empty) in
  let typerEnv = { typerEnv with QmlTypes.gamma = gamma } in
  (* Rgeister fields declared on [ty] *)
  let rec register_type ty =
    let reg_fields = List.iter (fun (x,_) -> register x) in
    QmlAstWalk.Type.iter
      (function
         | Q.TypeRecord (Q.TyRow (fields, _)) -> reg_fields fields
         | Q.TypeSum (Q.TyCol (fields, _)) -> List.iter reg_fields fields
         | _ -> ()
      )
      ty
  in
  let ((local_typedefs, new_gamma, typerEnv), code) =
    let aux ((local_typedefs, new_gamma, typerEnv) as acc) = function
      | Q.NewType (_, typedefs) as code_elt ->
          (* BEWARE: types such as #private_Date.date are not considered as being locals *)
          let local_typedefs =
            List.fold_left
              (fun acc ty_def ->
	             register_type ty_def.Q.ty_def_body;
                 Q.TypeIdentSet.add ty_def.QmlAst.ty_def_name acc)
              local_typedefs
              typedefs in
          let (new_gamma, typerEnv) =
            QmlTyper.OfficialTyper.type_newtype_for_separation
              ~more_gamma: new_gamma typerEnv code_elt typedefs in
          (local_typedefs, new_gamma, typerEnv), None
      | code_elt -> (acc, (Some code_elt)) in
    List.fold_left_filter_map
      aux (Q.TypeIdentSet.empty,new_gamma,typerEnv) code in
  R.save new_gamma ;
  (local_typedefs, typerEnv, code, stdlib)
