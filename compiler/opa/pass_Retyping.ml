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
(* shorthands *)
module P = Passes
module Q = QmlAst

let same o1 o2 =
  match (o1, o2) with
  | (None, None)
  | ((Some _), (Some _)) -> true
  | (None, (Some _))
  | ((Some _), None) -> false

let compare_annotmap original_annotmap correct_annotmap code =
  let f label =
    let annot = Annot.annot label in
    let pos = Annot.pos label in
    let tsc_gen_opt1 = QmlAnnotMap.find_tsc_opt annot original_annotmap in
    let tsc_gen_opt2 = QmlAnnotMap.find_tsc_opt annot correct_annotmap in
    let tsc_inst_opt1 = QmlAnnotMap.find_tsc_inst_opt annot original_annotmap in
    let tsc_inst_opt2 = QmlAnnotMap.find_tsc_inst_opt annot correct_annotmap in
    let _ty_opt1 = QmlAnnotMap.find_ty_opt annot original_annotmap in
    let _ty_opt2 = QmlAnnotMap.find_ty_opt annot correct_annotmap in
    let annot_int = Annot.to_int annot in
    if not (same tsc_gen_opt1 tsc_gen_opt2) then
      if tsc_gen_opt1 = None then
        OManager.printf
          "%a-%d@\n  No tsc_gen in the computed annotmap@."
          FilePos.pp_pos pos annot_int
      else
        OManager.printf
          "%a-%d@\n  Tsc_gen in the computed annotmap@."
          FilePos.pp_pos pos annot_int;
    if not (same tsc_inst_opt1 tsc_inst_opt2) then
      if tsc_inst_opt1 = None then
        OManager.printf
          "%a-%d@\n  No tsc_inst in the computed annotmap@."
          FilePos.pp_pos pos annot_int
      else
        OManager.printf
          "%a-%d@\n  Tsc_inst in the computed annotmap@."
          FilePos.pp_pos pos annot_int in
  QmlAstWalk.CodeExpr.iter
    (QmlAstWalk.ExprPatt.iter
       (fun expr -> f (QmlAst.Label.expr expr))
       (fun pat -> f (QmlAst.Label.pat pat)))
    code

let process_code env =
  let typed_env = Pass_Typing.process_code ~save:false env in
  let original_annotmap = env.P.typerEnv.QmlTypes.annotmap in
  let correct_annotmap = typed_env.P.typerEnv.QmlTypes.annotmap in
  compare_annotmap original_annotmap correct_annotmap env.P.qmlAst;
  env
