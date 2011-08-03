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

module List = Base.List
module Q = QmlAst


type env =  {
  bindings : Q.expr IdentMap.t;
  warn_x_field : unit (* just in case, if we want to add a field to this env *)
}

let depth_max = 10

let empty_env () = {
  bindings = IdentMap.empty ;
  warn_x_field = ()
}
let update_gamma gamma id ty =
  let tsc = QmlTypes.Scheme.generalize gamma ty in
  QmlTypes.Env.Ident.add id tsc gamma

let add_to_bindings env (id,e) = { env with bindings = IdentMap.add id e env.bindings }

let insert_bindings ~typed (gamma,annotmap) bindings e =
  List.fold_right
    (fun (id,e) (gamma,annotmap,expr) ->
      if typed then
        let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
        let gamma = update_gamma gamma id ty in
        let annotmap,expr = QmlAstCons.TypedExpr.letin annotmap [(id,e)] expr in
          gamma, annotmap, expr
      else
        let expr = QmlAstCons.UntypedExpr.letin [(id,e)] expr in
        gamma, annotmap, expr
    )
    bindings
    (gamma, annotmap, e)

let traverse_expr ~typed (gamma,annotmap) e =
  let rec aux tra depth (gamma, annotmap, env) e =
    let depth = succ depth in
    match e with
    | Q.Record _ ->
        let ((gamma, annotmap, env), e) as nothing_to_do = tra depth (gamma, annotmap, env) e in
        let (gamma, annotmap, env),e =
          (match e with
          | Q.Record (label, l) ->
              if depth < depth_max then
                nothing_to_do
              else (* store bindings and make the replacements *)
                let fresh () = Ident.next "r" in
                let l = List.map (fun (f,e) -> f,fresh (),e) l in
                let env = List.fold_left (fun env (_, id, e) -> add_to_bindings env (id,e)) env l in
                let (gamma, annotmap), l =
                  List.fold_left_map
                    (fun (gamma, annotmap) (f, v, e) ->
                      let gamma, annotmap, new_expr =
                        if typed then
                          let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
                          let gamma = update_gamma gamma v ty in
                          let annotmap,expr = QmlAstCons.TypedExpr.ident annotmap v ty in
                          gamma, annotmap, expr
                        else
                          let expr = QmlAstCons.UntypedExpr.ident v in
                          gamma, annotmap, expr
                      in (gamma, annotmap),(f, new_expr)
                    )
                    (gamma, annotmap)
                    l
                in
                (gamma, annotmap,env), Q.Record (label, l)

          | Q.ExtendRecord (label, f, expr, r) ->
              let (_gamma, _annotmap, env_), r = aux tra depth (gamma, annotmap, env) r in
              let (gamma, annotmap, env), expr = aux tra depth (gamma, annotmap, env) expr in
              let b = env_.bindings in
              let env = { env with bindings =
                  IdentMap.fold (fun id e m -> IdentMap.add id e m) b env.bindings } in
              let e = Q.ExtendRecord (label, f, expr, r) in
              (gamma, annotmap, env), e
          | _ -> tra 0 (gamma, annotmap, env) e)
        in
        (gamma, annotmap, env), e
    | Q.ExtendRecord _ ->
        tra depth (gamma, annotmap, env) e
     | _ -> tra 0 (gamma, annotmap, env) e
  in
  let env = empty_env () in
  let rec aux2 (gamma, annotmap) e =
    match e with
    | Q.Record _
    | Q.ExtendRecord _ ->
        let (gamma, annotmap, env),new_e = QmlAstWalk.Expr.traverse_foldmap_context_down aux 0 (gamma, annotmap, env) e in
        let bindings = IdentMap.rev_ordered_list env.bindings in
        let (gamma, annotmap, expr) = insert_bindings ~typed (gamma, annotmap) bindings new_e in
        (gamma, annotmap), expr
    | Q.Lambda (label, l, e) ->
        let (gamma, annotmap), new_e = aux2 (gamma, annotmap) e in
        let expr = Q.Lambda (label, l,new_e) in
        (gamma, annotmap), expr
    | Q.LetIn (_, l, e) ->
        let l,e = QmlAstUtils.LetIn.uncons l e in
        let (gamma, annotmap), new_e = aux2 (gamma, annotmap) e in
        let expr = QmlAstUtils.LetIn.cons l new_e in
        (gamma, annotmap), expr
    | Q.LetRecIn (_, l, e) ->
        let l,e = QmlAstUtils.LetRecIn.uncons l e in
        let (gamma, annotmap), new_e = aux2 (gamma, annotmap) e in
        let expr = QmlAstUtils.LetRecIn.cons l new_e in
        (gamma, annotmap), expr
    | Q.Match (_, e, p) ->
        let if_,pats,exprs = QmlAstUtils.Match.uncons e p in
        let (gamma, annotmap), new_e = aux2 (gamma, annotmap) if_ in
        let (gamma, annotmap), new_exprs = List.fold_left_map aux2 (gamma, annotmap) exprs in
        let expr = QmlAstUtils.Match.cons new_e pats new_exprs in
        (gamma, annotmap), expr
    | _ ->
        let (gamma, annotmap, env), new_e = QmlAstWalk.Expr.traverse_foldmap_context_down aux 0 (gamma, annotmap, env) e in
        let bindings = IdentMap.rev_ordered_list env.bindings in
        let (gamma, annotmap,expr) = insert_bindings ~typed (gamma, annotmap) bindings new_e in
        (gamma, annotmap), expr
  in
  aux2 (gamma, annotmap) e

let process_code ~typed gamma annotmap code =
  let (gamma, annotmap), code = QmlAstWalk.CodeExpr.fold_map (traverse_expr ~typed) (gamma, annotmap) code in
  (gamma,annotmap), code
