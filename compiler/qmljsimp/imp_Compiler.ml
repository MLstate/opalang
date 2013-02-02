(*
    Copyright © 2011, 2012 MLstate

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
(* alias *)
module PatternAnalysis = Imp_PatternAnalysis
module List = Base.List

(* shorthand *)
module E = Imp_Env

(* -- *)

let warning_set = Imp_Warnings.warning_set

let initial_env ~val_ ~is_distant ~renaming ~bsl_lang options env_typer code =
  let js_ctrans = Imp_Bsl.build_ctrans_env ~options in
  (* Keep only Bypasses used in the code *)
  let filter =
    let used_bypasses =
      let fail = Opacapi.Opabsl.BslPervasives.fail in
      let nctc = Opacapi.Opabsl.BslCps.Notcps_compatibility.thread_context in
      BslKeySet.add nctc (BslKeySet.add fail BslKeySet.empty)
    in
    let used_bypasses =
      QmlAstWalk.CodeExpr.fold
        (fun used_bypasses expr ->
           QmlAstWalk.Expr.fold
             (fun used_bypasses -> function
              | QmlAst.Bypass (_, bkey) -> BslKeySet.add bkey used_bypasses
              | _ -> used_bypasses
             ) used_bypasses expr
        ) used_bypasses code
    in (fun bypass -> BslKeySet.mem (Imp_Bsl.JsImpBSL.ByPass.key bypass) used_bypasses)
  in
  let private_bymap = Imp_Bsl.JsImpBSL.RegisterTable.build_bypass_map ~filter ~js_ctrans () in
  let gamma = env_typer.QmlTypes.gamma in
  let annotmap = env_typer.QmlTypes.annotmap in
  let env = {E.
    options;
    gamma;
    annotmap;
    val_;
    private_bymap;
    bsl_lang;
    is_distant;
    srenaming = renaming;
  } in
  env

let initial_private_env () = {E.
  local_vars = [];
  renaming = IdentMap.empty;
  no_warn_x = ();
}

let _outputer oc js_code =
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a%!" JsPrint.pp#code js_code

(* repeats [f] [n] times *)
let repeat2 n (f : int -> 'a -> 'b -> 'a * 'b) =
  let rec aux i a b =
    if i = n then a, b
    else
      let a, b = f i a b in
      aux (i+1) a b in
  aux 0

let compile
    ?runtime_ast ?(val_=fun ?(side=`client) _ -> ignore side; assert false) ?bsl ?(closure_map=IdentMap.empty)
    ~renaming ~is_distant ~bsl_lang ~exported
    options _env_bsl env_typer code =
  let _chrono = Chrono.make () in
  _chrono.Chrono.start ();
  let env = initial_env ~val_ ~is_distant ~renaming ~bsl_lang options env_typer code in
  let js_init = (if BslLanguage.is_nodejs bsl_lang then Imp_Bsl.JsImpBSL.ByPassMap.node_init
                 else Imp_Bsl.JsImpBSL.ByPassMap.js_init)
    env.E.private_bymap in
  #<If:JS_IMP$contains "time"> Printf.printf "bsl projection: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  let private_env = initial_private_env () in

  (* lambda lifting used to generate some code specially for qmljsimp
   * but now that lambda lifting is done before slicing, we can't do that anymore
   * here we use the directives left by the lambda lifting in the code to generate
   * the code as it was before the early lambda lifting:
   * - no partial application
   * - the code is almost lifted ie one lambda appears at the toplevel
   *   or two successive lambdas appear at the toplevel
   *   (the first lambda taking the environment, and the second the argument)
   *)
  (* Format.eprintf "%a\n%!" QmlPrint.pp#code code; *)
  let code =
    QmlAstWalk.CodeExpr.map
      (QmlAstWalk.Expr.traverse_map
         (fun tra -> function
          | QmlAst.Directive (label, `lifted_lambda (env,_), [QmlAst.Lambda (label2,params,body) as sub],_) ->
              if env = 0 then tra sub else
                let env_params, params = List.split_at env params in
                (* the type is crappy here but the
                 * backend doesn't look at it anyway *)
                tra (QmlAst.Lambda (label, env_params, QmlAst.Lambda (label2, params, body)))
          | QmlAst.Directive (label, `full_apply env, [QmlAst.Apply (label2,fun_,args) as sub], _) ->
              if env = 0 then sub else
              let env_args, args = List.split_at env args in
              let _missing = List.length args - env + 1 in
              (* same here *)
              (* BEWARE duplicating the annotation [label] is bad, but the
               * backend doesn't care about that and then they are lost *)
              tra (QmlAst.Apply (
                     label,
                     QmlAst.Directive
                       (label,
                        `partial_apply (None, false),
                        [QmlAst.Apply (label2, fun_, env_args)],
                        []),
                     args))
          | QmlAst.Directive (label, `full_apply env, [QmlAst.LetIn (inl, lst, ine)], tys) ->
              let lst = List.map (function (i, e) -> i, tra e) lst in
              let ine = QmlAst.Directive (label, `full_apply env, [ine], tys) in
              let ine = tra ine in
              tra (QmlAst.LetIn (inl, lst, ine))
          | QmlAst.Directive (_,(`lifted_lambda _ | `full_apply _),_,_) as expr ->
              OManager.i_error "Unexpected expression %a\n%!" QmlPrint.pp#expr expr
          | e -> tra e
         )
      ) code
  in
  let exported = IdentSet.union (Qmljs_Serializer.JsIdent.get_idents ()) exported in
  let exported =
    IdentSet.fold
      (fun i acc ->
         let i =
           try
             QmlRenamingMap.original_from_new env.E.srenaming i
           with Not_found -> i
         in
         JsIdentSet.add (JsCons.Ident.ident i) acc)
      exported JsIdentSet.empty
  in
  let _private_env, js_code = Imp_Code.compile ?runtime_ast env private_env code in
  #<If:JS_IMP$contains "time"> Printf.printf "qml -> js: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:"js_imp_0_translation" _outputer js_code) #<End>;

  let pass = BslLanguage.to_string bsl_lang in

  let inlining_env, save_inlining_env, js_code =
    if options.Qml2jsOptions.global_inlining then (
      let initial_env =
        match bsl with
        | Some code ->
            let topobj =
              if BslLanguage.is_nodejs bsl_lang then
                function "global" -> true
                | _ -> false
              else function _ -> false
            in
            Imp_Inlining.global_inline_analyse_code
              ~topobj
              (Imp_Inlining.env_of_map closure_map) code
        | None -> Imp_Inlining.env_of_map closure_map in
      let module Imp_Inlining_R = (val Imp_Inlining.make_r pass : Imp_Inlining.R) in
      let loaded_env = Imp_Inlining_R.load initial_env in
      (* we first analyse the code just to be able to inline simple recursive functions
       * the results of this analysis will be overwritten when we do the real analysis in
       * global_inline_stm *)
      let env = Imp_Inlining.global_inline_analyse_code loaded_env js_code in
      let env, js_code =
        repeat2 2 (* we repeat all this twice because well, you need to simplify the code
                   * to be able to inline better but once you inline, some other pass can
                   * simplify your code, etc... :/
                   * It should be better to run the function below twice on each code element
                   * instead of analysing the whole code twice, but it didn't make any difference
                   * in my example
                   *)
          (fun _n ->
             List.fold_left_collect
               (fun env stm ->
                  (* first global inline inside the statement using the
                   * environment of the dependencies (which may not be enough for
                   * recursive cases, but you can't realy inline anyway, except
                   * for cases such as an eta expansion in the middle of a
                   * recursive group) *)
                  match Imp_Inlining.global_inline_rewrite_stm env stm with
                  | None -> env, []
                  | Some stm ->
                      #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:(Printf.sprintf "js_imp_1_%d_global_inline" _n) _outputer [stm]) #<End>;
                      (* then, local inline to remove all useless stuff in the original code,
                       * introduced by patterns compilation and from global inline *)
                  let stm = Imp_Inlining.local_inline_stm stm in
                  #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:(Printf.sprintf "js_imp_2_%d_local_inline" _n) _outputer [stm]) #<End>;
                  (* cleanup, constant folding, etc to make the code as lightweight as possible *)
                  let stms = JsCleanUp.clean_stm ~use_shortcut_assignment:false stm in
                  #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:(Printf.sprintf "js_imp_3_%d_cleanup" _n) _outputer stms) #<End>;
                  (* now that the code is as small as it can get, we analyse it to know if it is inlinable
                   * in particular, if inlining inside its body made the code simpler, then we can
                   * inline now and we couldn't have if we analysed first, and inlined after *)
                  let env = Imp_Inlining.global_inline_analyse_code env stms in
                  env, stms
               )
          ) env js_code in
      #<If:JS_IMP$contains "time"> Printf.printf "global inline: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
      env, (fun env -> Imp_Inlining_R.save ~env ~loaded_env ~initial_env), js_code
    ) else
      Imp_Inlining.empty_env, ignore, js_code in
  (* this local inline doesn't do much but it still removes a few variables *)
  let js_code = if options.Qml2jsOptions.inlining then Imp_Inlining.local_inline js_code else js_code in
  #<If:JS_IMP$contains "time"> Printf.printf "local inline: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:"js_imp_4_local_inline" _outputer js_code) #<End>;

  (* local renaming must be done last, because it generates very tricky code
   * when it squashes variables together *)
  let js_code = if options.Qml2jsOptions.alpha_renaming then JsRenaming.rename js_code else js_code in
  #<If:JS_IMP$contains "time"> Printf.printf "local renaming: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:"js_imp_5_local_renaming" _outputer js_code) #<End>;
  let js_code, sharing_env = Imp_Sharing.process_code ~pass js_code in
  let inlining_env =
    Imp_Inlining.map_expr_in_env (Imp_Sharing.rewrite_expr sharing_env) inlining_env
  in
  let exported =
    JsIdentSet.fold
      (fun i exported ->
         match Imp_Sharing.get_substitute sharing_env i with
         | None -> exported
         | Some s -> JsIdentSet.add s (JsIdentSet.remove i exported)
      ) exported exported in
  save_inlining_env inlining_env;
  let exported =
    Imp_Inlining.fold_env
      (fun i e exported ->
         if JsIdentSet.mem i exported then
           JsWalk.Expr.fold
             (fun exported -> function
              | JsAst.Je_ident (_, i) -> JsIdentSet.add i exported
              | _ -> exported
             ) exported e
         else exported)
      inlining_env exported
  in
  #<If:JS_IMP$contains "time"> Printf.printf "code sharing: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:"js_imp_6_code_sharing" _outputer js_code) #<End>;
  let js_code = if options.Qml2jsOptions.cleanup then JsCleanUp.clean ~use_shortcut_assignment:true js_code else js_code in
  #<If:JS_IMP$contains "time"> Printf.printf "clean up: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:"js_imp_7_cleanup" _outputer js_code) #<End>;
  let js_code =
    let keep = (fun i -> JsIdentSet.mem i exported ||
                  match i with
                  | JsAst.ExprIdent i ->
                      let i =
                        try QmlRenamingMap.new_from_original env.E.srenaming i with Not_found -> i
                      in
                      is_distant i
                  | _ -> false
               )
    in
    Imp_Cleaning.process_code ~keep js_code
  in

  {Qml2jsOptions.
     js_init_contents = [ "bsl_dynamic_code_and_projections.js", `ast js_init;
                        ];
     js_code;
     exported;
  }


let dummy_compile () =
  let module R = (val Imp_Inlining.make_r "js" : Imp_Inlining.R) in
  R.save
    ~env:Imp_Inlining.empty_env
    ~loaded_env:Imp_Inlining.empty_env
    ~initial_env:Imp_Inlining.empty_env;
  ignore (Imp_Sharing.process_code ~pass:"js" [])

module Backend =
struct
  let dynloader plugin =
    Imp_Bsl.JsImpBSL.RegisterInterface.dynload plugin.BslPluginInterface.dynloader
  let name = "qmljsimp"
  let compile = compile
  let dummy_compile = dummy_compile
  let runtime_libs ~cps:_ =
    (*
      If needed: we can have specialized configuration for each file
    *)
    let conf = BslJsConf.Optimized {BslJsConf.
         localrenaming = true;
         cleanup = true;
      } in [
      (* No extension means that the file is a nodejs package and should
         be used by the server. An extension means a standalone js file,
         used in the client. Both of these right now contain the same code,
         but the server code is preprocessed to globalize all identifiers
         a hack to circumvent the nodejs module system *)
      `server ("opa-js-runtime-cps", conf);
      `client ("clientLibLib.js", conf);
      `client ("qmlCpsClientLib.js", conf);
      `client ("qmlJsImpClientLib.js", conf)
    ]
end

let () = Qml2jsOptions.register_backend (module Backend : Qml2jsOptions.JsBackend)
