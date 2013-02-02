


let translate env =
  let rec dig_directive (f: ('a, 'b) SurfaceAst.expr -> ('a, 'b)
                          SurfaceAst.expr) ((e,label) as v: ('a, 'b) SurfaceAst.expr) : ('a, 'b) SurfaceAst.expr =
    match e with
    | SurfaceAst.Directive (dir,exps,[]) ->
        SurfaceAst.Directive(dir,List.map (dig_directive f) exps,[]), label
    | _ -> f v
  in
  let rec dig_lambda (f: ('a, 'b) SurfaceAst.expr -> ('a, 'b) SurfaceAst.expr) ((e,label) as v : ('a, 'b) SurfaceAst.expr) =
    match e with
    | SurfaceAst.Lambda(pat,exp) -> SurfaceAst.Lambda(pat,dig_lambda f exp) , label
    | _ -> f v
  in
  let aux ((e,label) as v)  =
    match e with
    | SurfaceAst.Directive (`no_client_calls, [exp], _) ->
        let f exp =
          SurfaceAstCons.with_label label (fun () ->
            let f_ctx = OpaMapToIdent.val_ Opacapi.ThreadContext.no_client_calls in
            let f = SurfaceAstCons.ExprIdentCons.E.ident f_ctx in
            let ctx = SurfaceAstCons.ExprIdentCons.E.applys f [] in
            SurfaceAstCons.ExprIdentCons.D.with_thread_context ctx exp)
        in
        let g exp = dig_lambda f exp in
        dig_directive g exp
    | _ -> v in

  let r = OpaWalk.Code.map_down aux env.Passes.sa_lcode in
  {env with Passes.sa_lcode = r}
