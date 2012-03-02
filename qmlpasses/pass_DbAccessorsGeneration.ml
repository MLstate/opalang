(*
    Copyright © 2011, 2012 MLstate

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

module Q = QmlAst

module BSLDbGen = QmlDbGen.DbGenByPass.BSLDbGenAlphaOpa
module DbGen = QmlDbGen.DbGen ( BSLDbGen )

(* note:
   in the code, NewDbValue elements can appear before Database elements
   maybe it should not happen after reordering ?
*)

(* split the code at first Database or NewDbValue element *)
let split_at_first_dbdecl code =
  let rec split acc code =
  match code with
    | [] -> List.rev acc, code
    | Q.Database _ :: code_tl -> List.rev acc, code_tl
    | Q.NewDbValue _ :: code_tl -> List.rev acc, code_tl
    | code_elt :: code_tl -> split (code_elt :: acc) code_tl
  in
  match split [] code with
    | _, [] -> [], code
    | s -> s

(* split the code at last NewDbValue element and remove Database elements *)
let split_at_last_NewDbValue code =
  let rec split (tmp_init, tmp_end) (acc_init) code =
    match code with
    | [] -> List.rev tmp_init, tmp_end
    | Q.Database _ :: code_tl ->
        split (tmp_init, tmp_end) acc_init code_tl
    | Q.NewDbValue _ :: code_tl ->
        split (acc_init, code_tl) acc_init code_tl
    | code_elt :: code_tl ->
        split (tmp_init, tmp_end) (code_elt :: acc_init) code_tl
  in
  split ([],code) [] code

let split_code code =
  let code_before_dbdecl, code_after_dbdecl =
    split_at_first_dbdecl code
  in
  let code_before_newdbvalue, code_after_newdbvalue =
    split_at_last_NewDbValue code_after_dbdecl
  in
  code_before_dbdecl, code_before_newdbvalue, code_after_newdbvalue

let process_code gamma annotmap schema code alpha_opt =
  (* 1°: split the code to insert DbGen at the right place *)
  let code_before_dbdecl, code_before_newdbvalue, code_after_newdbvalue =
    split_code code
  in
  let sorted_code = QmlAstSort.add QmlAstSort.empty code_after_newdbvalue in
  let code_after_newdbvalue = QmlAstSort.Get.new_val sorted_code in

  (* 2°: generate database accessors from the schema *)
  let dbinfo, gamma, annotmap_opt, dbgen_init_code, dbgen_accessors_code =
    DbGen.initialize
      ~annotmap:(Some annotmap)
      ~valinitial_env:alpha_opt
      gamma
      schema
  in

  (* 3°: adding annotmap of dbGen code *)
  let annotmap =
    Option.default_map annotmap
      (fun am -> QmlAnnotMap.merge (QmlTypes.process_typenames_annotmap ~gamma:gamma am)
         annotmap) annotmap_opt
  in

  (* 4°: inserting dbgen code *)
  let code =
    code_before_dbdecl @ dbgen_init_code
    @ code_before_newdbvalue @ dbgen_accessors_code
    @ code_after_newdbvalue
  in

(*   (\* 6°: merging dbinfo with the ones of previous packages *\) *)
(*   let _ = Base.jlog(Base.sprintf "saving a dbinfo of size %d" (StringListMap.size dbinfo)) in *)
(*   let _ = R.save dbinfo in *)
(*   let dbinfo = R.fold (StringListMap.merge QmlDbGen.merge_dbinfo) dbinfo in *)
  List.iter (function | Q.Database _ -> assert false | _ -> ()) code;

  dbinfo, gamma, annotmap, code
