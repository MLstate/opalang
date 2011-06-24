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

module Q = QmlAst
module List = BaseList

let rec is_a_bypass = function
  | Q.Coerce (_,e,_)
  | Q.Directive (_,(#Q.type_directive | `restricted_bypass _ | `may_cps),[e],_) -> is_a_bypass e
  | Q.Bypass (_,key) -> Some key
  | _ -> None

(* can't find some utils that eta expands typed code !! *)
let eta_expand gamma annotmap e =
  let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
  let tsc_gen_opt = QmlAnnotMap.find_tsc_opt (Q.QAnnot.expr e) annotmap in
  let annotmap = QmlAnnotMap.remove_tsc (Q.QAnnot.expr e) annotmap in
  match QmlTypesUtils.Inspect.get_arrow_params gamma ty with
  | None -> annotmap, e
  | Some args ->
      let arity = List.length args in
      let idents = List.init arity (fun i -> Ident.next ("eta"^string_of_int i)) in
      let params = List.combine idents args in
      let annotmap, args = List.fold_left_map2 QmlAstCons.TypedExpr.ident annotmap idents args in
      let annotmap, app = QmlAstCons.TypedExpr.apply gamma annotmap e args in
      let annotmap, fun_ = QmlAstCons.TypedExpr.lambda annotmap params app in
      let annotmap = QmlAnnotMap.add_tsc_opt (Q.QAnnot.expr fun_) tsc_gen_opt annotmap in
      annotmap, fun_

let process_code gamma annotmap code =
  QmlAstWalk.CodeExpr.fold_map
    (QmlAstWalk.Expr.self_traverse_foldmap
       (fun self tra annotmap e ->
          match is_a_bypass e with
          | Some _key -> eta_expand gamma annotmap e
          | None ->
              let input_e = e in
              match e with
              | Q.Apply (label,e,el) ->
                  let annotmap, e', el' =
                    match is_a_bypass e with
                    | Some _ ->
                      let annotmap, el' = List.fold_left_map_stable self annotmap el in
                      annotmap, e, el'
                    | None ->
                      let annotmap, e' = self annotmap e in
                      let annotmap, el' = List.fold_left_map_stable self annotmap el in
                      annotmap, e', el' in
                  if e == e' && el == el' then
                    annotmap, input_e
                  else
                    annotmap, Q.Apply (label,e',el')
              | _ ->
                  tra annotmap e
       )
    ) annotmap code
