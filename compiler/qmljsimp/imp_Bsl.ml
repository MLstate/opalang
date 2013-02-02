(*
    Copyright © 2011-2013 MLstate

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
module B = BslTypes
module J = JsAst
module BI = BslInterface

module List = Base.List
module Format = Base.Format

(** When projecting, we want to translate between "normal" JS values
    (e.g. null, undefined, objects...) and values that follow the Opa
    API (e.g. {none}, {some: val}...). The correspondence goes as follows:

    {none} <-> null
    {some: a} <-> a
    void <-> undefined

    In order to avoid ambiguity with nested options, we only project
    them at the outermost level. Thus, {some: {some: v}} becomes
    {some: v} in JS. In particular, JS code that uses nested options
    must call explicitely the constructors js_some and js_none for
    nested options.

    We also require that external types not be nullable, otherwise we
    can't use them in native options.
*)

module JS_CTrans =
struct
  type env = {
    options : Qml2jsOptions.t;
  }

  let empty () = {
    options = Qml2jsOptions.Argv.default ()
  }

  let label = Annot.nolabel "imp_Bsl"

  (*
    The bsl projection will be globalized
    Do not use global native there.
  *)
  let call_native name args =
    JsCons.Expr.call ~pure:true
      (JsCons.Expr.ident (JsCons.Ident.native name))
      args

  type private_env = {
    local_vars : JsIdent.t list;
  }

  let param name =
    (JsAst.Native (`local, name))

  let fresh_param name =
    J.ExprIdent (Ident.next name)
  let fresh_var private_env name =
    let ident = fresh_param name in
    let private_env = {local_vars = ident :: private_env.local_vars} in
    private_env, ident

  let declare_local_vars private_env =
    let local_vars = private_env.local_vars in
    let private_env = {local_vars = []} in
    let declarations = List.map (fun i -> JsCons.Statement.var i) local_vars in
    private_env, declarations

  let call_typer ~key typer ?ret id =
    JsCons.Expr.comma
      [JsCons.Expr.call ~pure:false typer [JsCons.Expr.string (BslKey.to_string key); id]]
      (match ret with
       | None -> id
       | Some ret -> ret)

  let function_projection
      ~level (* The recursion level, to avoid parameter shadowing *)
      ?(more=None)
      ?(callback=false)
      ?(cps=`no)
      ?(check=false) ~inputs ~outputs ~bsltags _env ~key private_env
      type_params type_return id =
    let cps = if BslTags.do_projection bsltags "cps" then cps else `no in
    let initial_local_vars = private_env.local_vars in
    (* 1 - Projection of inputs *)
    let private_env = {local_vars = []} in
    let params = List.mapi (fun i _ ->
      param (Printf.sprintf "p%d_%d" level i)
    ) type_params in
    let proj_input (private_env,projected) typ x =
      match inputs private_env typ (JsCons.Expr.ident x) with
      | Some (private_env, ast) -> (private_env, true), ast
      | None -> (private_env, projected), (JsCons.Expr.ident x) in
    let (private_env, projected), (arguments:JsAst.expr list) =
      List.fold_left_map2 proj_input (private_env,false) type_params params
    in
    let params, arguments =
      if cps <> `no then
        let k = param "k" in
        (params @ [k]), (arguments @ [JsCons.Expr.ident k])
      else params, arguments
    in
    (* 2 - Projection of outputs *)
    let js_ret =
      JsCons.Expr.call ~pure:bsltags.BslTags.pure id arguments
    in
    let (private_env, projected), proj_output =
      match outputs private_env type_return js_ret with
      | Some (private_env, ast) -> (private_env, true), ast
      | None -> (private_env, projected), js_ret in
    if projected || cps <> `no || more <> None then
      let check_arity =
        if check
        then
          [JsCons.Statement.expr (
             JsCons.Expr.call ~pure:false
               Imp_Common.ClientLib.type_fun_arity [
                 JsCons.Expr.string (BslKey.to_string key);
                 JsCons.Expr.native "arguments";
                 JsCons.Expr.int (List.length type_params)
               ])]
        else [] in
      let _private_env, declarations = declare_local_vars private_env in
      let private_env = {(*private_env with*) local_vars = initial_local_vars} in
      let return =
        match cps with
        | `to_ -> (* cps return : return_(k, output) *)
            JsCons.Expr.call ~pure:true
              (JsCons.Expr.ident (JsAst.Native (`global true, "return_")))
              [(JsCons.Expr.ident (JsAst.Native (`local, "k")));
               proj_output]
        | _ -> proj_output
      in
      let return = JsCons.Statement.return return in
      let function_ =
        JsCons.Expr.function_ None params (check_arity @ declarations @ [return])
      in
      let function_ =
        match cps with
        | `no -> function_
        | (`to_ | `from) as cps->
            match cps with
            | `to_ -> function_
            | `from ->
                let transform =
                  if callback then "callback_opa2js"
                  else "uncps"
                in
                JsCons.Expr.call ~pure:true
                  (JsCons.Expr.ident (JsAst.Native (`global true, transform)))
                  [(JsCons.Expr.ident (JsAst.Native (`local, "k")));
                   function_;
                   JsCons.Expr.int (List.length type_params);
                   JsCons.Expr.string (BslKey.to_string key)
                  ]
      in
      let function_ = match more with None -> function_ | Some more -> more function_ in
      let function_ =
        if check then
          call_typer ~key Imp_Common.ClientLib.type_fun id ~ret:function_
        else
          function_ in
      Some (private_env, function_)
    else
      None

  let call_option_typer is_record key id private_env proj =
    let typer =
      if is_record then Imp_Common.ClientLib.type_option
      else Imp_Common.ClientLib.type_native_option in
    match proj with
    | Some (private_env, ast) ->
      Some (private_env, call_typer ~key typer id ~ret:ast)
    | None ->
      Some (private_env, call_typer ~key typer id)

  (* Convert from a JS nullable type to an Opa value *)
  let aux_option_qml_of_js
      in_option caller key env private_env typ (id:JsAst.expr) =
    let private_env, x = fresh_var private_env "js" in
    let proj = caller key env private_env typ (JsCons.Expr.ident x) in
    let res = match proj, in_option with
      | None, true -> None
      | None, false ->
        Some (
          private_env,
          (* id === null ? js_none : js_some(id) *)
          JsCons.Expr.cond
            (JsCons.Expr.strict_equality id (JsCons.Expr.null ()))
            Imp_Common.ClientLib.none
            (JsCons.Expr.call ~pure:true Imp_Common.ClientLib.some [id])
        )
      | Some (private_env, ast), true ->
        Some (
          private_env,
          (* (x = udot(id, "some"))!=null ? js_some(ast) : js_none *)
          JsCons.Expr.cond
            (JsCons.Expr.strict_neq
               (JsCons.Expr.assign_ident x
                  (JsCons.Expr.call ~pure:true Imp_Common.ClientLib.udot
                     [id; JsCons.Expr.string "some"]))
               (JsCons.Expr.null ())
            )
            (JsCons.Expr.call ~pure:true Imp_Common.ClientLib.some [ast])
            Imp_Common.ClientLib.none
        )
      | Some (private_env, ast), false ->
        Some (
          private_env,
          (* id === null ? js_none : (x = id, js_some(ast)) *)
          JsCons.Expr.cond
            (JsCons.Expr.strict_equality id (JsCons.Expr.null ()))
            Imp_Common.ClientLib.none
            (JsCons.Expr.comma
               [JsCons.Expr.assign_ident x id]
               (JsCons.Expr.call ~pure:true Imp_Common.ClientLib.some [ast]))
        ) in
    if env.options.Qml2jsOptions.check_bsl_types then
      call_option_typer in_option key id private_env res
    else
      res

  (* The inverse conversion: take an Opa value and produce a JS value,
     where [null] represents [{none}] *)
  let aux_option_js_of_qml
      in_option caller key env private_env typ (id:JsAst.expr) =
    let private_env, x = fresh_var private_env "js" in
    let proj = caller key env private_env typ (JsCons.Expr.ident x) in
    let res = match proj, in_option with
      | None, true -> None
      | None, false ->
        Some (
          private_env,
          (* udot(id, "some") *)
          JsCons.Expr.cond
            (JsCons.Expr.neq
               (JsCons.Expr.assign_ident x
                  (JsCons.Expr.call ~pure:true Imp_Common.ClientLib.udot
                     [id; JsCons.Expr.string "some"]))
               (JsCons.Expr.null ()))
            (JsCons.Expr.ident x)
            (JsCons.Expr.null ())
        )
      | Some (private_env, ast), true ->
        Some (
          private_env,
          (* x = udot(id, "some") ? js_some(ast) : js_none *)
          JsCons.Expr.cond
            (JsCons.Expr.assign_ident x
               (JsCons.Expr.call ~pure:true Imp_Common.ClientLib.udot
                  [id; JsCons.Expr.string "some"]))
            (JsCons.Expr.call ~pure:true Imp_Common.ClientLib.some [ast])
            Imp_Common.ClientLib.none
        )
      | Some (private_env, ast), false ->
        Some (
          private_env,
          (* ( (x = udot(id, "some")) != null ? ast : null *)
          JsCons.Expr.cond
            (JsCons.Expr.neq
               (JsCons.Expr.assign_ident x
                  (JsCons.Expr.call ~pure:true Imp_Common.ClientLib.udot
                     [id; JsCons.Expr.string "some"]))
               (JsCons.Expr.null ()))
            ast
            (JsCons.Expr.null ())
        )
    in
    if env.options.Qml2jsOptions.check_bsl_types then
      call_option_typer in_option key id private_env res
    else
      res

  let aux_external ?(check=false) caller key env private_env p (id:JsAst.expr) =
    List.iter
      (fun ty ->
         (* This is just a check that the inner types don't need a projection.
            Since we don't project inner values in an external type, if the
            inner types do need a projection, then we must throw an error.
            Consider what should happen in the following case:

                /** @externType Wrap.t('a) */

                /** @register {Wrap.t(option(string))} */
                var foo = null;

                /** @register {Wrap.t(opa[option(string)])} */
                var bar = js_none;

                /** @register {Wrap.t('a) -> 'a} */
                function unwrap(x) { return x; }

            In Opa code, if we applied [unwrap] to [foo] and [bar], we'd end
            up with two values that have the same Opa type but different
            representations, which would result in a runtime error. *)

         (* We must deactivate check_bsl_types or else we always have a
            projection :/ *)
         let fake_env = {
           options = {env.options with Qml2jsOptions.check_bsl_types = false}
         } in

         match caller key fake_env private_env ty id with
         | None -> ()
         | Some _ ->
             OManager.error
               ("(projection of %a) don't know how to handle types that need " ^^
                "projections when given as arguments to external types. " ^^
                "Please use the Opa representation instead.")
               BslKey.pp key
      ) p;
    if check then
      Some (private_env, call_typer ~key Imp_Common.ClientLib.type_extern id)
    else
      None

  (* when the relevant option is activated, inserting type checks that the js
   * object received correspond to the type declared in the bsl *)
  let rec aux_qml_of_js ~level ~bsltags ?(in_option=false)
      key env private_env typ (id:JsAst.expr) :
      (private_env * JsAst.expr) option =
    match typ with
    | B.Const (_, c) ->
        if env.options.Qml2jsOptions.check_bsl_types then
          let typer =
            match c with
            | QmlAst.TyFloat -> Imp_Common.ClientLib.type_float
            | QmlAst.TyInt -> Imp_Common.ClientLib.type_int
            | QmlAst.TyNull -> assert false
            | QmlAst.TyString -> Imp_Common.ClientLib.type_string in
          Some (private_env, call_typer ~key typer id)
        else
          None

    | B.TypeVar _ ->
        if env.options.Qml2jsOptions.check_bsl_types then
          Some (private_env, call_typer ~key Imp_Common.ClientLib.type_var id)
        else
          None

    | B.Void _ ->
        if bsltags.BslTags.cps_bypass then None
        else
          let qml_void = Imp_Common.ClientLib.void in
          if env.options.Qml2jsOptions.check_bsl_types then
            Some (private_env, call_typer ~key Imp_Common.ClientLib.type_native_void id ~ret:qml_void)
          else
            Some (private_env, JsCons.Expr.comma [id] qml_void)

    | B.Bool _ ->
        if env.options.Qml2jsOptions.check_bsl_types then
          Some (private_env, call_typer ~key Imp_Common.ClientLib.type_bool id)
        else
          None (* same representation for booleans *)

    | B.Option (_, o) ->
        (* We always project options *)
        aux_option_qml_of_js
          in_option
          (aux_qml_of_js ~level ~bsltags ~in_option:true)
          key env private_env o id

    | B.OpaValue (_, t) ->
        if env.options.Qml2jsOptions.check_bsl_types then
          let typer =
            match t with
            | B.Const (_, c) -> (
                match c with
                | QmlAst.TyFloat -> Imp_Common.ClientLib.type_float
                | QmlAst.TyInt -> Imp_Common.ClientLib.type_int
                | QmlAst.TyNull -> assert false
                | QmlAst.TyString -> Imp_Common.ClientLib.type_string
              )
            | B.TypeVar _ -> Imp_Common.ClientLib.type_var
            | B.Void _ -> Imp_Common.ClientLib.type_void
            | B.Bool _ -> Imp_Common.ClientLib.type_bool
            | B.Option _ -> Imp_Common.ClientLib.type_option
            | B.OpaValue _ -> assert false
            | _ -> Imp_Common.ClientLib.type_opavalue
          in
          Some (private_env, call_typer ~key typer id)
        else
          None

    | B.External (_, _, p) ->
        aux_external ~check:env.options.Qml2jsOptions.check_bsl_types
          (aux_qml_of_js ~level ~bsltags) key env private_env p id

    | B.Callback (_, inputs, output)
    | B.Fun (_, inputs, output) ->
        let callback = match typ with | B.Callback _ -> true | _ -> false in
        let cps =
          match env.options.Qml2jsOptions.cps, bsltags.BslTags.cps_bypass with
          | true, false -> `to_
          | false, true -> `from
          | true, true | false, false -> `no
        in
        function_projection ~level ~cps ~bsltags env ~key ~callback
          ~check:env.options.Qml2jsOptions.check_bsl_types
          ~inputs:(aux_js_of_qml ~level:(level + 1) ~bsltags key env)
          ~outputs:(aux_qml_of_js ~level:(level + 1) ~bsltags key env)
          private_env
          inputs output id

  (* in the projection qml -> js, there is no check since the typer
   * already checks that the input of bypasses are right *)
  and aux_js_of_qml ~level ~bsltags ?(in_option=false)
      key env private_env typ (id:JsAst.expr) =
    match typ with
    | B.Const _ ->
        None

    | B.TypeVar _ ->
        None

    | B.Void _ ->
        (* Nobody cares about the returned value of a javascript function
         * returning nothing *)
        None

    | B.Bool _ ->
        None

    | B.Option (_, o) ->
        aux_option_js_of_qml in_option
          (aux_js_of_qml ~level ~bsltags ~in_option:true)
          key env private_env o id

    | B.OpaValue _ ->
        None

    | B.External (_,_,p) ->
        aux_external (aux_js_of_qml ~level ~bsltags) key env private_env p id

    | B.Callback (_, inputs, output)
    | B.Fun (_, inputs, output) ->
        let callback = match typ with | B.Callback _ -> true | _ -> false in
        let cps, more =
          match env.options.Qml2jsOptions.cps, bsltags.BslTags.cps_bypass with
          | true, false -> `from, None
          | false, true -> `to_, None
          | true, true -> `no, Some (fun fun_ ->
                                       JsCons.Expr.call ~pure:true
                                         Imp_Common.ClientLib.wrap_tc
                                         [fun_])
          | false, false -> `no, None
        in
        function_projection ~level ~more ~cps ~bsltags env ~key ~callback
          ~inputs:(aux_qml_of_js ~level:(level + 1) ~bsltags key env)
          ~outputs:(aux_js_of_qml ~level:(level + 1) ~bsltags key env)
          private_env
          inputs output id

  let wrap_return_of_aux = function
    | None -> None
    | Some (private_env, ast) ->
        Some (private_env.local_vars, ast)

  let initial_private_env = {local_vars = []}

  let qml_of_js ~bslkey:key ~bsltags typ ~env (BI.MetaIdent meta_ident) =
    if bsltags.BslTags.cps_bypass then
      env, None
    else
      let o = aux_qml_of_js ~level:0 key ~bsltags env initial_private_env typ
        (JsCons.Expr.ident (JsCons.Ident.native meta_ident)) in
      let o = wrap_return_of_aux o in
      env, o

  let js_of_qml ~bslkey:key ~bsltags typ ~env (BI.MetaIdent meta_ident) =
    let o = aux_js_of_qml ~level:0 ~bsltags key env initial_private_env typ
      (JsCons.Expr.ident (JsCons.Ident.native meta_ident)) in
    let o = wrap_return_of_aux o in
    env, o

  let conversion_code env =
    (env, (if env.options.Qml2jsOptions.qml_closure then
             Pass_Closure.generate_applys_js ()
           else ["",JsCons.Statement.comment "closure not activated"])
    )

  let addk bsltags env =
    env.options.Qml2jsOptions.cps
    && bsltags.BslTags.second_order
    && BslTags.do_projection bsltags "cps"

  let should_cps_return bsltags env =
    env.options.Qml2jsOptions.cps
    && bsltags.BslTags.second_order
    && not(bsltags.BslTags.cps_bypass)
    && BslTags.do_projection bsltags "cps"

  let more_args _ bsltags env =
    if addk bsltags env then Some ["k"]
    else if should_cps_return bsltags env then
      Some []
    else None

  let map_result _ bsltags env expr =
    if not(should_cps_return bsltags env) then
      expr
    else
      let open JsAst in
      Je_call (label,
               Je_ident (label, (Native (`global true, "return_"))),
               [Je_ident (label, (Native (`local, "k"))); expr],
               false)

end

module JsImpBSL = BslLib.LibBSLForQml2Js (JS_CTrans)

let build_ctrans_env ~options =
  {JS_CTrans.options}
