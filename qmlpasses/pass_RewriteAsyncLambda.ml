module Q = QmlAst

(* keep in sync with the slicer *)
type ignored_directive = [
| Q.type_directive
| Q.lambda_lifting_directive
| Q.slicer_directive
]

let tyvoid = Q.TypeRecord (Q.TyRow ([], None))
let scheduler_push_ty = Q.TypeArrow ([Q.TypeArrow ([], tyvoid)], tyvoid)

let rewrite_lambda ~gamma ~val_ annotmap e =
  QmlAstWalk.Expr.traverse_foldmap (
    fun tra annotmap expr ->
      match expr with
      | Q.Coerce _
      | Q.Directive (_, #ignored_directive, _, _) -> tra annotmap expr
      | Q.Lambda (label, args, body) ->
          let return_type = QmlAnnotMap.find_ty (Q.QAnnot.expr body) annotmap in
          if QmlMoreTypes.equal_ty ~gamma return_type tyvoid then (
            let annotmap, lambda = QmlAstCons.TypedExpr.lambda annotmap [] body in
            let annotmap, push = QmlAstCons.TypedExpr.ident annotmap (val_ Opacapi.Scheduler.push) scheduler_push_ty in
            let annotmap, app = QmlAstCons.TypedExpr.apply gamma annotmap push [lambda] in
            annotmap, Q.Lambda (label, args, app)
          ) else (
            let context = QmlError.Context.expr expr in
            QmlError.serror context "@@async lambdas must return void@ (and not %a)" QmlPrint.pp#ty return_type;
            annotmap, expr
          )
      | _ ->
          (* not a lambda, leaving the directive for cps *)
          raise Exit
  ) annotmap e

let process_code ~val_ gamma annotmap code =
  QmlAstWalk.CodeExpr.fold_map (
    QmlAstWalk.Expr.traverse_foldmap
      (fun tra annotmap expr ->
         match expr with
         | Q.Directive (label, `async, [e], []) ->
             (* putting the potentiel generalization under the directive so that ei
              *  introduces the lambda below the directive *)
             let tsc_gen_opt = QmlAnnotMap.find_tsc_opt_label label annotmap in
             let annotmap = QmlAnnotMap.remove_tsc_label label annotmap in
             assert (QmlAnnotMap.find_tsc_opt (Q.QAnnot.expr e) annotmap = None);
             let annotmap = QmlAnnotMap.add_tsc_opt (Q.QAnnot.expr e) tsc_gen_opt annotmap in

             (* leaving the directive so that the slicer that see the 'async' *)
             let annotmap, e = try rewrite_lambda ~gamma ~val_ annotmap e with Exit -> annotmap, e in
             let expr = Q.Directive (label, `async, [e], []) in
             tra annotmap expr
         | _ -> tra annotmap expr
      )
  ) annotmap code
