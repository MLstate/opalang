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
module B = BslTypes
module J = JsAst
module BI = BslInterface

module List = Base.List
module Format = Base.Format

module JS_CTrans =
struct
  type env = {
    options : Qml2jsOptions.t;
  }

  let empty () = {
    options = Qml2jsOptions.Argv.default ()
  }

  (* checks if a value of the type can evaluate to false
   * in a 'if' for instance *)
  let can_be_false = function
    | B.Const _
    | B.Bool _
    | B.TypeVar _
    | B.OpaValue _
    | B.External _ -> true
    | B.Void _
    | B.Option _
    | B.Fun _ -> false

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

  let fresh_param name =
    J.ExprIdent (Ident.next name)
  let fresh_var private_env name =
    let ident = fresh_param name in
    let private_env = {(*private_env with*) local_vars = ident :: private_env.local_vars} in
    private_env, ident

  let declare_local_vars private_env =
    let local_vars = private_env.local_vars in
    let private_env = {(*private_env with*) local_vars = []} in
    let declarations = List.map (fun i -> JsCons.Statement.var i) local_vars in
    private_env, declarations

  let call_typer ~key typer ?ret id =
    JsCons.Expr.comma
      [JsCons.Expr.call ~pure:false typer [JsCons.Expr.string (BslKey.to_string key); JsCons.Expr.ident id]]
      (match ret with
       | None -> JsCons.Expr.ident id
       | Some ret -> ret)

  let function_projection ?(check=false) ~inputs ~outputs ~key private_env type_params type_return id =
    let initial_local_vars = private_env.local_vars in
    let private_env = {local_vars = []} in
    let private_env, js_ret = fresh_var private_env "js_ret" in
    let params = List.map (fun _ -> fresh_param "p") type_params in
    let (private_env, projected), proj_output =
      match outputs private_env type_return js_ret with
      | Some (private_env, ast) -> (private_env, true), ast
      | None -> (private_env, false), JsCons.Expr.ident js_ret in
    let proj_input (private_env,projected) typ x =
      match inputs private_env typ x with
      | Some (private_env, ast) -> (private_env, true), ast
      | None -> (private_env, projected), JsCons.Expr.ident x in
    let (private_env, projected), arguments = List.fold_left_map2 proj_input (private_env,projected) type_params params in
    if projected then
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
      let call =
        JsCons.Statement.assign_ident js_ret (
          JsCons.Expr.call ~pure:true
            (JsCons.Expr.ident id)
            arguments
        ) in
      let return = JsCons.Statement.return proj_output in
      let function_ = JsCons.Expr.function_ None params (check_arity @ declarations @ [call;return]) in
      let function_ =
        if check then
          call_typer ~key Imp_Common.ClientLib.type_fun id ~ret:function_
        else
          function_ in
      Some (private_env, function_)
    else
      None

  let aux_option ?(check=false) caller key env private_env typ id =
    (* no projection for options *)
    let private_env, x = fresh_var private_env "js" in
    match caller key env private_env typ x with
    | None -> None
    | Some (private_env, ast) ->
        let ast =
          if can_be_false typ then
            (* 'some' in id ? (x = id.some, {some = ast}) : id *)
            JsCons.Expr.cond
              (JsCons.Expr.in_ (JsCons.Expr.string "some") (JsCons.Expr.ident id))
              (JsCons.Expr.comma
                 [JsCons.Expr.assign_ident x (JsCons.Expr.dot (JsCons.Expr.ident id) "some")]
                 (JsCons.Expr.obj ["some", ast]))
              (JsCons.Expr.ident id)
          else
            (* (x = id.some) ? {some = ast} : id (* none *) *)
            JsCons.Expr.cond
              (JsCons.Expr.assign_ident x (JsCons.Expr.dot (JsCons.Expr.ident id) "some"))
              (JsCons.Expr.obj ["some", ast])
              (JsCons.Expr.ident id) in
        let ast =
          if check then
            call_typer ~key Imp_Common.ClientLib.type_option id ~ret:ast
          else
            ast in
        Some (private_env, ast)


  let aux_external ?(check=false) caller key env private_env p id =
    List.iter
      (fun ty ->
         (* this is just a check that the inner
          * types don't need a projection *)
         let fake_env = {(*env with*) options = {env.options with Qml2jsOptions.check_bsl_types = false}} in
         (* we must deactive check_bsl_types or else we always have a projection :/ *)
         match caller key fake_env private_env ty id with
         | None -> ()
         | Some _ ->
             Format.printf "Proj of %a@." BslKey.pp key;
             assert false (* TODO: proper error *)
      ) p;
    if check then
      Some (private_env, call_typer ~key Imp_Common.ClientLib.type_extern id)
    else
      None

  (* when the relevant option is activated, inserting type checks that the js
   * object received correspond to the type declared in the bsl *)
  let rec aux_qml_of_js key env private_env typ id : (private_env * JsAst.expr) option =
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
        let qml_void = Imp_Common.ClientLib.void in
        let qml_void =
          if env.options.Qml2jsOptions.check_bsl_types then
            call_typer ~key Imp_Common.ClientLib.type_native_void id ~ret:qml_void
          else
            qml_void
        in
        Some (private_env, qml_void)

    | B.Bool _ ->
        if env.options.Qml2jsOptions.check_bsl_types then
          Some (private_env, call_typer ~key Imp_Common.ClientLib.type_bool id)
        else
          None (* same representation for booleans *)

    | B.Option (_, o) ->
        aux_option ~check:env.options.Qml2jsOptions.check_bsl_types aux_qml_of_js key env private_env o id

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
        aux_external ~check:env.options.Qml2jsOptions.check_bsl_types aux_qml_of_js key env private_env p id

    | B.Fun (_, inputs, output) ->
        assert (not env.options.Qml2jsOptions.cps);
        let initial_conv =
          function_projection ~key
            ~check:env.options.Qml2jsOptions.check_bsl_types
            ~inputs:(aux_js_of_qml key env)
            ~outputs:(aux_qml_of_js key env)
            private_env
            inputs output id in
        initial_conv

  (* in the projection qml -> js, there is no check since the typer
   * already checks that the input of bypasses are right *)
  and aux_js_of_qml key env private_env typ id =
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
        aux_option aux_js_of_qml key env private_env o id

    | B.OpaValue _ ->
        None

    | B.External (_,_,p) ->
        aux_external aux_js_of_qml key env private_env p id

    | B.Fun (_, inputs, output) ->
        assert (not env.options.Qml2jsOptions.cps);
        let p private_env id =
          function_projection ~key
            ~inputs:(aux_qml_of_js key env)
            ~outputs:(aux_js_of_qml key env)
            private_env
            inputs output id in
        p private_env id

  let wrap_return_of_aux = function
    | None -> None
    | Some (private_env, ast) ->
        Some (private_env.local_vars, ast)

  let initial_private_env = {local_vars = []}

  let qml_of_js ~bslkey:key ~bsltags:_ typ ~env (BI.MetaIdent meta_ident) =
    let o = aux_qml_of_js key env initial_private_env typ (JsCons.Ident.native meta_ident) in
    let o = wrap_return_of_aux o in
    env, o

  let js_of_qml ~bslkey:key ~bsltags:_ typ ~env (BI.MetaIdent meta_ident) =
    let o = aux_js_of_qml key env initial_private_env typ (JsCons.Ident.native meta_ident) in
    let o = wrap_return_of_aux o in
    env, o

  let conversion_code env =
    (env, (if env.options.Qml2jsOptions.qml_closure then
             Pass_Closure.generate_applys_js ()
           else ["",JsCons.Statement.comment "closure not activated"])
    )
end

module JsImpBSL = BslLib.LibBSLForQml2Js (JS_CTrans)

let build_ctrans_env ~options =
  {JS_CTrans.options}
