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
(* alias *)
module PatternAnalysis = Imp_PatternAnalysis
module List = Base.List

(* shorthand *)
module E = Imp_Env

(* -- *)

let warning_set = Imp_Warnings.warning_set

let initial_env ~val_ ~renaming_server ~renaming_client options env_typer code =
  let js_ctrans = Imp_Bsl.build_ctrans_env ~options in
  let private_bymap = Imp_Bsl.JsImpBSL.RegisterTable.build_bypass_map ~js_ctrans () in
  let gamma = env_typer.QmlTypes.gamma in
  let annotmap = env_typer.QmlTypes.annotmap in
  let env = {E.
    options;
    gamma;
    annotmap;
    val_;
    private_bymap;
    renaming_client;
    renaming_server;
  } in
  env, code

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

let compile ?(val_=fun _ -> assert false) ?bsl ?(closure_map=IdentMap.empty) ~renaming_server ~renaming_client options _env_bsl env_typer code =
  let _chrono = Chrono.make () in
  _chrono.Chrono.start ();
  let env, code = initial_env ~val_ ~renaming_server ~renaming_client options env_typer code in
  let js_init = Imp_Bsl.JsImpBSL.ByPassMap.js_init env.E.private_bymap in
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
  let code =
    QmlAstWalk.CodeExpr.map
      (QmlAstWalk.Expr.map
         (function
          | QmlAst.Directive (label, `lifted_lambda (env,_), [QmlAst.Lambda (label2,params,body) as sub],_) ->
              if env = 0 then sub else
              let env_params, params = List.split_at env params in
              (* the type is crappy here but the
               * backend doesn't look at it anyway *)
              QmlAst.Lambda (label, env_params, QmlAst.Lambda (label2, params, body))
          | QmlAst.Directive (label, `full_apply env, [QmlAst.Apply (label2,fun_,args) as sub], _) ->
              if env = 0 then sub else
              let env_args, args = List.split_at env args in
              (* same here *)
              (* BEWARE duplicating the annotation [label] is bad, but the
               * backend doesn't care about that and then they are lost *)
              QmlAst.Apply (
                label,
                QmlAst.Directive
                  (label,
                   `partial_apply (None, false),
                   [QmlAst.Apply (label2, fun_, env_args)],
                   []),
                args)
          | QmlAst.Directive (_,(`lifted_lambda _ | `full_apply _),_,_) -> assert false
          | e -> e)
      ) code in

  let _private_env, js_code = Imp_Code.compile env private_env code in
  #<If:JS_IMP$contains "time"> Printf.printf "qml -> js: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:"js_imp_0_translation" _outputer js_code) #<End>;

  let js_code =
    if options.Qml2jsOptions.global_inlining then (
      let initial_env =
        match bsl with
        | Some code -> Imp_Inlining.global_inline_analyse_code (Imp_Inlining.env_of_map closure_map) code
        | None -> Imp_Inlining.env_of_map closure_map in
      let loaded_env = Imp_Inlining.R.load initial_env in
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
                  (* first global inline inside the statement using the environment of the dependencies
                   * (which may not be enough for recursive cases, but you can't realy inline anyway,
                   * except for cases such as an eta expansion in the middle of a recursive group) *)
                  let stm = Imp_Inlining.global_inline_rewrite_stm env stm in
                  #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:(Printf.sprintf "js_imp_1_%d_global_inline" _n) _outputer [stm]) #<End>;
                  (* then, local inline to remove all useless stuff in the original code,
                   * introduced by patterns compilation and from global inline *)
                  let stm = Imp_Inlining.local_inline_stm stm in
                  #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:(Printf.sprintf "js_imp_2_%d_local_inline" _n) _outputer [stm]) #<End>;
                  (* cleanup, constant folding, etc to make the code as lightweight as possible *)
                  let stms = Imp_CleanUp.clean_stm ~use_shortcut_assignment:false stm in
                  #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:(Printf.sprintf "js_imp_3_%d_cleanup" _n) _outputer stms) #<End>;
                  (* now that the code is as small as it can get, we analyse it to know if it is inlinable
                   * in particular, if inlining inside its body made the code simpler, then we can
                   * inline now and we couldn't have if we analysed first, and inlined after *)
                  let env = Imp_Inlining.global_inline_analyse_code env stms in
                  env, stms
               )
          ) env js_code in
      Imp_Inlining.R.save ~env ~loaded_env ~initial_env;
      #<If:JS_IMP$contains "time"> Printf.printf "global inline: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
      js_code
    ) else
      js_code in

  (* this local inline doesn't do much but it still removes a few variables *)
  let js_code = if options.Qml2jsOptions.inlining then Imp_Inlining.local_inline js_code else js_code in
  #<If:JS_IMP$contains "time"> Printf.printf "local inline: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:"js_imp_4_local_inline" _outputer js_code) #<End>;

  (* local renaming must be done last, because it generates very tricky code
   * when it squashes variables together *)
  let js_code = if options.Qml2jsOptions.alpha_renaming then Imp_Renaming.rename js_code else js_code in
  #<If:JS_IMP$contains "time"> Printf.printf "local renaming: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:"js_imp_5_local_renaming" _outputer js_code) #<End>;

  let js_code = if options.Qml2jsOptions.cleanup then Imp_CleanUp.clean ~use_shortcut_assignment:true js_code else js_code in
  #<If:JS_IMP$contains "time"> Printf.printf "clean up: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
  #<If:JS_IMP$contains "print"> ignore (PassTracker.file ~filename:"js_imp_6_cleanup" _outputer js_code) #<End>;

  {Qml2jsOptions.
     js_init_contents = [ "bsl_dynamic_code_and_projections.js", `ast js_init;
                        ];
     js_code = js_code;
  }


let dummy_compile () =
  Imp_Inlining.R.save
    ~env:Imp_Inlining.empty_env
    ~loaded_env:Imp_Inlining.empty_env
    ~initial_env:Imp_Inlining.empty_env

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
      "clientLibLib.js", conf ;
      "qmlCpsClientLib.js", conf ;
      "qmlJsImpClientLib.js", conf ;
    ]
end

let () = Qml2jsOptions.register_backend (module Backend : Qml2jsOptions.JsBackend)
