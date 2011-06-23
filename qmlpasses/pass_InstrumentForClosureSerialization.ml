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
module Cons = QmlAstCons.TypedExpr
module List = BaseList

type env = Ident.t * (Q.ty,unit) QmlGenericScheme.tsc option IdentMap.t

let empty = IdentMap.empty

let extract_env_type env_size gamma ty =
  match QmlTypesUtils.Inspect.get_arrow_through_alias_and_private gamma ty with
  | Some (l1,ret) ->
      assert (List.length l1 >= env_size);
      let l1, l2 = List.split_at env_size l1 in
      l1, Q.TypeArrow (l2, ret), l2, ret
  | None -> assert false

let generate_typeofer gamma annotmap env (i,e) =
  match e with
  | Q.Directive (_, `lifted_lambda (env_size, function_of_origin), [_], _) ->
      let new_i = Ident.refreshf ~map:"%s_ser" i in
      let tsc_gen_opt = QmlAnnotMap.find_tsc_opt (Q.QAnnot.expr e) annotmap in
      let ty_i = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
      let ty_i =
        (* refreshing or else ei will stupidly propagate type vars to the original def *)
        let tsc = QmlTypes.Scheme.quantify ty_i in
        let tsc = QmlTypes.Scheme.refresh tsc in
        let _quant, ty_i, () = QmlGenericScheme.export_unsafe tsc in
        ty_i in
      let ty_env, ty_remaining, ty_args, _ty_ret = extract_env_type env_size gamma ty_i in
      let annotmap, g = Cons.ident annotmap i (*ty_i*) (Q.TypeArrow (ty_env, ty_remaining)) in
      let annotmap = QmlAnnotMap.add_tsc_inst_opt (Q.QAnnot.expr g) tsc_gen_opt annotmap in
      let new_tsc_gen_opt, gamma =
        let ty = Q.TypeArrow (ty_env,ty_remaining) in
        let tsc = QmlTypes.Scheme.quantify ty in
        let gamma = QmlTypes.Env.Ident.add i tsc gamma in
        let tsc_opt =
          if QmlGenericScheme.is_empty tsc then
            None
          else
            Some tsc in
        tsc_opt, gamma in
      let params = List.init env_size (fun i -> Ident.next ("eta_" ^ string_of_int i)) in
      let annotmap, args = List.fold_left_map2 (fun annotmap i ty -> Cons.ident annotmap i ty) annotmap params ty_env in
      let annotmap, apply_g = Cons.apply_partial gamma annotmap g args in
      let partial_apply = `partial_apply (Some (List.length ty_args), true) in
      let annotmap, typeofs =
        List.fold_left_map2
          (fun annotmap i ty ->
             let annotmap, i = Cons.ident annotmap i ty in
             Cons.directive annotmap `typeof [i] []
          ) annotmap params ty_env in
      let annotmap, body =
        let label = Annot.refresh (Q.Label.expr e) in
        let annotmap = QmlAnnotMap.add_ty_label label ty_remaining annotmap in
        annotmap, Q.Directive (label,partial_apply,apply_g::typeofs,[]) in
      let annotmap, fun_ = Cons.lambda annotmap (List.combine params ty_env) body in
      (* the @lifted_lambda is for the slicer, so that it puts the function on the right side
       * (which is the side of function_of_origin)
       * this probably won't work when we have local annotation, because this function should
       * be on the side of the lambda it is created from instead *)
      let annotmap, fun_ = Cons.directive_id annotmap (`lifted_lambda (0, function_of_origin)) fun_ in
      let annotmap =
        QmlAnnotMap.add_tsc_opt (Q.QAnnot.expr fun_) new_tsc_gen_opt annotmap in
      let env = IdentMap.add i (new_i, new_tsc_gen_opt) env in
      Some (gamma, annotmap, env, new_i, fun_)
  | _ ->
      None

let generate_new_binding (gamma, annotmap, env) iel =
  List.fold_left_filter_map
    (fun (gamma, annotmap, env) (i,e) ->
       match generate_typeofer gamma annotmap env (i,e) with
       | None -> (gamma, annotmap, env), None
       | Some (gamma, annotmap, env, i, e) -> (gamma, annotmap, env), Some (i,e)
    ) (gamma, annotmap, env) iel

let rewrite_identifiers env annotmap code =
  QmlAstWalk.CodeExpr.fold_map
    (QmlAstWalk.Expr.foldmap
       (fun annotmap e ->
          match e with
          | Q.Directive (_, `partial_apply (_,false), [Q.Apply (label2, Q.Ident (label1, i), args)], _)
              when IdentMap.mem i env ->
              let new_ident, tsc_opt = IdentMap.find i env in
              let e = Q.Apply (label2, Q.Ident (label1, new_ident), args) in
              let annotmap = QmlAnnotMap.remove_tsc_inst_label label1 annotmap in
              let annotmap = QmlAnnotMap.add_tsc_inst_opt_label label1 tsc_opt annotmap in
              annotmap, e
          | _ ->
              annotmap, e
       )
    ) annotmap code

let process_code gamma annotmap code =
  if ObjectFiles.stdlib_packages (ObjectFiles.get_current_package ()) then
    gamma, annotmap, code
  else
    let (gamma, annotmap, env), code =
      List.fold_left_collect
        (fun acc code_elt ->
           match code_elt with
           | Q.NewVal (label,iel) ->
               let acc, new_iel = generate_new_binding acc iel in
               let code =
                 if new_iel = [] then
                 [code_elt]
                 else
                   [code_elt; Q.NewVal (Annot.refresh label,new_iel)] in
               acc, code
           | Q.NewValRec (label,iel) ->
               let acc, new_iel = generate_new_binding acc iel in
               let code = [Q.NewValRec (label,iel @ new_iel)] in
               acc, code
           | _ ->
               assert false
        ) (gamma, annotmap, empty) code in
    let annotmap, code = rewrite_identifiers env annotmap code in
    gamma, annotmap, code
