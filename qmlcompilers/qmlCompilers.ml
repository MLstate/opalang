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
(**
   Common Passes for Qml Command Line compilers.
*)

(** {6 Design Notes} *)

(**
   Currently, qml is still too far from opa in the framework organisation,
   so we have dependencies probleme to be solved so that the options are
   shared between qml passes and opa passes.

   Waiting (not too long) where we will have time to continue to refactor
   this, we start the passes once the options are already available, and
   we have a unified dynloader function in libbsl. (todo)
*)

(* depends *)
let ( |> ) x f = f x

(* alias *)
module PH = PassHandler

(** {6 Types used} *)

(**
   A dynloader is a function which take a generated loader
   (like [OpabslgenLoader]), and register all bypass which its defines
   somewhere, with a side effect.

   @see "BslLib.BSLINTROSPECTION.ByPassMap.RegisterInterface" to see how
   back-end build a value of this type
*)
type dynloader = BslPluginInterface.plugin -> unit

(**
   The type of options used by qml compilers passes.
   Currently, due to refactoring issues, this type is the type of
   qml2ocaml options, which is bad.
   TODO: use the same options than opa.exe
*)
type options = Qml2ocamlOptions.argv_options

(**
   The type taken by the back-end at end of passes
*)
type env_final = {
  env_bsl : BslLib.env_bsl ;
  env_blender : QmlBlender.qml_milkshake
}


module QmlPasses :
sig
  (** {6 Front passes} *)

  (**
     The type returned by the parser
  *)
  type env_parsed = {
    init_code : QmlAst.code option ;
    user_code : QmlAst.code ;
  }

  (**
     Load plugins and register all bypasses
  *)
  val pass_BslLoading :
    (options, options, dynloader, BslLib.env_bsl) PassHandler.pass

  (**
     Get the code (stdlib of qml, and user code).
     Waiting for killing the qml-syntax, there will be a transitionnal time
     during which the 2 syntax will be supported as input of qmlcompilers.
  *)
  val pass_Parse :
    (options, options, BslLib.env_bsl, BslLib.env_bsl * env_parsed)
    PassHandler.pass

  (**
     Blend the code : DbGen + Typing + AlphaConv
  *)
  val pass_Blend :
    (options, options, BslLib.env_bsl * env_parsed, env_final) PassHandler.pass

  (** {6 Middle passes} *)
  (** From there, the final passes have the same type *)
  type final_pass = (options, options, env_final, env_final) PassHandler.pass

  (** main: combine passes using PassHandler, and return the final env for the
      backends *)
  val main : ?dynloader:dynloader -> side:[`client|`server] -> lang:BslLanguage.t -> options -> env_final
end =
struct
  module List = Base.List

  type env_parsed = {
    init_code : QmlAst.code option ;
    user_code : QmlAst.code ;
  }
  type final_pass = (options, options, env_final, env_final) PassHandler.pass

  (* FIXME: sharing options with more *)
  open Qml2ocamlOptions

  let pass_BslLoading =
    let transform pass_env =
      let options = pass_env.PassHandler.options in
      let dynloader = pass_env.PassHandler.env in
      (* FIXME : share with opa/passes.ml : pass_BSLLoading will call this function with the minimum argv_options accessed here *)
      let search_path = "" :: options.extra_path in
      List.iter
        (fun bypass_plugin ->
           let file = bypass_plugin in
           let found_files =
             List.filter_map
               (fun p ->
                  let fullname = Filename.concat p file in
                  if File.is_regular fullname then Some fullname else None)
               search_path in
           let file =
             match found_files with
             | [] -> file
             | [fullname] -> fullname
             | fullname::_ ->
                 OManager.warning ~wclass:WarningClass.bsl_loading (
                   "Bypass-plugin @{<bright>%s@} found in several places.@\n"^^
                     "I will use @{<bright>%s@}" ) file fullname;
                 fullname in
           OManager.verbose "load file @{<bright>%S@} (plugin)" file;
           BslDynlink.loadfile_private (BslDynlink.MarshalPlugin bypass_plugin)
        )
        options.bypass_plugin;
      (* building the loader table -- resolving the dependencies (else,
         fatal error containing a message) *)
      let loaders = BslPluginTable.finalize () in
      (* Call always BslLib.BSL + with the dynloader in arg *)
      List.iter
        (fun loader ->
           BslLib.BSL.RegisterInterface.dynload
             loader.BslPluginInterface.dynloader ;
           dynloader loader
        )
        loaders ;
      (* building a first version the map (not compiler specific) *)
      let bymap = BslLib.BSL.RegisterTable.build_bypass_map () in
      let env = { BslLib. bymap = bymap ; plugins = loaders } in
      let empty _ = [] in
      { pass_env with PassHandler.
          env = env ;
          printers = empty;
          trackers = empty } in
    let precond =
      [
        (* TODO: add pre condition *)
      ] in
    let postcond =
      [
        (* TODO: add post condition *)
      ] in
    PassHandler.make_pass ~precond ~postcond transform


  (* ======================================================================= *)

  let env_Parse_extract_code (_,env) = (Option.default [] env.init_code) @ env.user_code
  let env_Parse_extract_ac env = (QmlAnnotMap.empty, QmlTypes.Env.empty, env_Parse_extract_code env)

  let pass_Parse =
    let transform pass_env =
      let vcp = ConsoleParser.create () in
      let options = pass_env.PassHandler.options in
      let bsl = pass_env.PassHandler.env in

      let input_string location parse_me =
        try
          OpaToQml.Parser.of_string ~filename: location parse_me
        with
        | OpaToQml.Parser.Exception e ->
            OManager.error "%s -- parsing error@\n%s" location e in

      let lines_foldi filename line int =
        match ConsoleParser.accumulate vcp line with
        | None ->
            []
        | Some input -> (
            match input with
            | ConsoleParser.Directive _ ->
                (* This qmltop directive is ignored by this compiler. *)
                []
            | ConsoleParser.Code parse_me ->
                let location =
                  Printf.sprintf
                    "file \"%s\"%s"
                    filename
                    (if int > 0 then Printf.sprintf " -- line %d" int else "") in
                input_string location parse_me
          ) in

      let input_file input_file =
        let filename =
          match input_file with
          | Qml2ocamlOptions.QmlFile filename ->
              OManager.error
                "filename %S has suffix .qml@\nThe syntax qml is deprecated, please use opa syntax@\n" filename
          | Qml2ocamlOptions.OpaFile filename -> filename in

        OManager.verbose "parsing @{<bright>%S@}" filename;
        ConsoleParser.reset vcp ;
        let lcode = File.lines_rev_mapi (lines_foldi filename) filename in
        let lcode = List.rev ((lines_foldi filename ";;" (-1))::lcode) in
        List.concat lcode
      in

      let input_string (filename, contents) =
        OManager.verbose "parsing @{<bright>%S@}" filename;
        input_string filename contents in

      let init_code = if options.no_stdlib then None else
        Some (List.concat_map
                (fun loader -> List.concat_map input_string
                   loader.BslPluginInterface.opa_code)
                bsl.BslLib.plugins) in

      let user_code =
        List.concat_map input_file options.input_files in

      let env = bsl,
        {
          init_code = init_code ;
          user_code = user_code
        } in

      let printers = QmlTracker.printers env_Parse_extract_ac in
      let trackers = QmlTracker.trackers env_Parse_extract_code in
      { pass_env with PassHandler.
          env ;
          printers ;
          trackers ;
      }
    in
    let precond =
      [
        (* TODO: add pre condition *)
      ] in
    let postcond =
      [
        (* TODO: add post condition *)
      ] in
    PassHandler.make_pass ~precond ~postcond transform


  (* ============================================================================== *)
  (* FIXME: it is not yet sure what we should do with exceptions / vs OManager *)
  (* Some user may be keen on keeping exception instead of errors (qmltop, ide?) *)
  let handle_exception_error fct =
    try
      fct ()
    with
    | QmlBlender.Exception s -> OManager.error "Blender : %s" s
    | (QmlTypes.Exception _ | QmlTyperException.Exception _) as exn ->
        (* At this point, we do not have any environment nor annotations map. *)
        OManager.error "Typer : %a"
          (QmlTyperErrHandling.pp_report_from_typer_exception
             QmlTypes.Env.empty QmlAnnotMap.empty)
          exn ;
    | QmlCpsRewriter.Exception e ->
        OManager.error "QmlCps : %s" (QmlCpsRewriter.error_message e)

  let pass_Blend =
    let extract_ac env = env.env_blender.QmlBlender.env.QmlTypes.annotmap, env.env_blender.QmlBlender.code in
    let extract_agc env = env.env_blender.QmlBlender.env.QmlTypes.annotmap, env.env_blender.QmlBlender.env.QmlTypes.gamma, env.env_blender.QmlBlender.code in
    let extract_code env = env.env_blender.QmlBlender.code in
    let transform pass_env =
      let options = pass_env.PassHandler.options in
      let bsl, qml = pass_env.PassHandler.env in
      (** construction of bypass_typer *)
      let bypass_typer = BslLib.BSL.ByPassMap.bypass_typer bsl.BslLib.bymap in
      let blender_options =
        let blender_options = QmlBlender.DyDbGenBlender.default_options () in
        { blender_options with
            initial_env = { blender_options.QmlBlender.initial_env with QmlTypes.bypass_typer = bypass_typer } ;
        } in
      let qml_milkshake =
        let fct () =
          QmlBlender.DyDbGenBlender.full_blend ~options:blender_options (Option.default [] qml.init_code @ qml.user_code)
        in
        handle_exception_error fct
      in

      OManager.flush_errors () ;

      let env =
        {
          env_bsl = bsl ;
          env_blender = qml_milkshake
        }
      in
      let printers = QmlTracker.printers extract_agc in
      let trackers = QmlTracker.trackers extract_code in
      { pass_env with PassHandler.
          env ;
          printers ;
          trackers ;
      }
    in
    let precond =
      [
        (* TODO: add pre condition *)
      ] in
    let postcond =
      [
        QmlAlphaConv.Check.alpha extract_ac
      ] in
    PassHandler.make_pass ~precond ~postcond transform

  (* ============================================================================== *)

  (* FIXME: passing options around : if the type options is defined somewhere higher *)
  (* Or even if a pass gives directly command arg tuple ?*)
  (* need to pass some options to the pass from argv_options *)

  let make_final_pass precond postcond process_code =
    let transform pass_env =
      let blender = pass_env.PassHandler.env.env_blender in
      let blender =
        let fct () = QmlBlender.Sugar.process_code ~process_code blender in
        handle_exception_error fct
      in
      { pass_env with PassHandler.
          env = { pass_env.PassHandler.env with
                    env_blender = blender }
      }
    in
    PassHandler.make_pass ~precond ~postcond transform

  let make_final_pass_options precond postcond process_code =
    let transform pass_env =
      let options = pass_env.PassHandler.options in
      let blender = pass_env.PassHandler.env.env_blender in
      let process_code = process_code options in
      let blender =
        let fct () = QmlBlender.Sugar.process_code ~process_code blender in
        handle_exception_error fct
      in
      { pass_env with PassHandler.
          env = { pass_env.PassHandler.env with
                    env_blender = blender }
      }
    in
    PassHandler.make_pass ~precond ~postcond transform


  let make_final_pass_options_blender precond postcond process_code =
    let transform pass_env =
      let options = pass_env.PassHandler.options in
      let blender = pass_env.PassHandler.env.env_blender in
      let blender =
        let fct () = QmlBlender.Sugar.process_code ~process_code:(process_code options blender) blender in
        handle_exception_error fct
      in
      { pass_env with PassHandler.
          env = { pass_env.PassHandler.env with
                    env_blender = blender }
      }
    in
    PassHandler.make_pass ~precond ~postcond transform

  let make_final_pass_bypass_typer precond postcond process_code =
    let transform pass_env =
      let blender = pass_env.PassHandler.env.env_blender in
      let bypass_typer = blender.QmlBlender.env.QmlTypes.bypass_typer in
      let blender =
        let fct () = QmlBlender.Sugar.process_code ~process_code:(process_code bypass_typer) blender in
        handle_exception_error fct
      in
      { pass_env with PassHandler.
          env = { pass_env.PassHandler.env with
                    env_blender = blender }
      }
    in
    PassHandler.make_pass ~precond ~postcond transform

  let make_final_pass_bymap precond postcond process_code =
    let transform pass_env =
      let blender = pass_env.PassHandler.env.env_blender in
      let bymap = pass_env.PassHandler.env.env_bsl.BslLib.bymap in
      let blender =
        let fct () = QmlBlender.Sugar.process_code ~process_code:(process_code ~bymap) blender in
        handle_exception_error fct
      in
      { pass_env with PassHandler.
          env = {pass_env.PassHandler.env with env_blender = blender} } in
    PassHandler.make_pass ~precond ~postcond transform

  let extract_final_ac env = env.env_blender.QmlBlender.env.QmlTypes.annotmap, env.env_blender.QmlBlender.code
  let extract_final_code env = env.env_blender.QmlBlender.code
  let extract_final_bypass_typer env = env.env_blender.QmlBlender.env.QmlTypes.bypass_typer
  let extract pass_env = (extract_final_bypass_typer pass_env, extract_final_code pass_env)

  let pass_Assertion =
    let process_code options gamma annotmap code =
      let no_assert = options.no_assert in
      let annotmap, code = Pass_Assertion.process_code ~no_assert gamma annotmap code in
      (gamma, annotmap), code
    in
    make_final_pass_options
      []
      []
      process_code

  let pass_LambdaLifting side =
    let precond =
      [
        QmlAlphaConv.Check.unicity extract_final_ac ;
      ] in
    let postcond =
      [
        QmlAlphaConv.Check.alpha extract_final_ac ;
      ] in
    make_final_pass_options_blender precond postcond
      (fun options _blender -> Pass_LambdaLifting.process_code ~early:false ~typed:(not options.cps) ~side)

  let pass_BypassHoisting =
    let precond =
      [
        (* TODO: add pre condition *)
      ] in
    let postcond =
      [
        QmlAlphaConv.Check.alpha extract_final_ac ;
      ] in
    make_final_pass precond postcond (
      fun gamma annotmap code ->
        let annotmap, code = Pass_BypassApply.process_code gamma annotmap code in
        (gamma, annotmap), code
    )

  let pass_DiscardRemoteBypasses ~lang =
    let precond = [] and postcond = [] in
    make_final_pass_bymap precond postcond (QmlFakeSlicer.discard_remote_bypasses ~lang)

  let pass_Uncurry side =
    let precond =
      [
        (* TODO: add pre condition *)
      ] in
    let postcond =
      [
        QmlAlphaConv.Check.alpha extract_final_ac ;
      ] in
    make_final_pass_options_blender precond postcond
      (fun options _ gamma annotmap code ->
         let gamma_annotmap, _closure_map, code =
           Pass_Uncurry.process_code
             ~typed:(not options.cps)
             ~side
             gamma
             annotmap
             code in
         gamma_annotmap, code
      )

  (* need bypass typer *)
  let pass_Closure ~side =
    let precond =
      [
        (* TODO: add pre condition *)
      ] in
    let postcond =
      [
        QmlAlphaConv.Check.alpha extract_final_ac ;
      ] in
    make_final_pass_options_blender precond postcond
      (fun options blender ->
         Pass_Closure.process_code
           ~typed:(not options.cps)
           ~side
           ~renaming_server:QmlRenamingMap.empty
           ~renaming_client:QmlRenamingMap.empty
           blender.QmlBlender.env.QmlTypes.bypass_typer)

  let pass_ConstantSharing =
    let transform pass_env =
      let options = pass_env.PassHandler.options in
      (*
        Work arround : if we activate typed mode of ConstantSharing after cps,
        some annot are not found because currently cps refresh annots and does
        not report types.
        TODO: once cps will be fixed, remove this hack, and let the pass be in typed mode.
      *)
      let typed = not options.cps in
      let process_code = Pass_ConstantSharing.process_code ~side:`client ~typed in
      let blender = pass_env.PassHandler.env.env_blender in
      let blender =
        QmlBlender.Sugar.process_code ~process_code blender
      in
      { pass_env with PassHandler.
          env = { pass_env.PassHandler.env with
                    env_blender = blender }
      }
    in
    let precond =
      [
        (* TODO: add pre condition *)
      ] in
    let postcond =
      [
        QmlAlphaConv.Check.alpha extract_final_ac ;
      ] in
    PassHandler.make_pass ~precond ~postcond transform
  (* ============================================================================== *)

  let pass_CpsRewriter ~lang =
    let transform pass_env =
      let options = pass_env.PassHandler.options in
      let bsl = pass_env.PassHandler.env.env_bsl in
      let blender = pass_env.PassHandler.env.env_blender in
      let cps_options =
        { QmlCpsRewriter.default_options with QmlCpsRewriter.
            no_assert = options.no_assert ;
            qml_closure = options.qml_closure ;
            toplevel_concurrency = options.cps_toplevel_concurrency ;
            server_side = options.server_side
        } in
      let bsl_bypass_typer key =
        match BslLib.BSL.ByPassMap.bsl_bypass_typer bsl.BslLib.bymap key with
        | None ->
            (* TODO: this must be a precondition, violation by the type checking *)
            OManager.error (
            "%%%% @{<bright>%s@} %%%% : unknown external primitives are not allowed@\n"^^
            "Please use a plugin"
            )
              (BslKey.to_string key)
        | Some t -> t in
      let bsl_bypass_tags key =
        match BslLib.BSL.ByPassMap.bsl_bypass_tags
          ~lang
          bsl.BslLib.bymap key
        with
        | None ->
            OManager.error (
            "%%%% @{<bright>%s@} %%%% : unknown external primitives are not allowed@\n"^^
            "Please use a plugin"
            )
              (BslKey.to_string key)
        | Some t -> t in
      let bsl_bypass_cps =
        BslLib.BSL.ByPassMap.bsl_bypass_cps ~lang  bsl.BslLib.bymap in
      let env_cps =
        QmlCpsRewriter.env_initial
          ~options:cps_options
          ~bsl_bypass_typer
          ~bsl_bypass_tags
          ~bsl_bypass_cps
          ~typing:blender.QmlBlender.env
          ()
      in
      let code =
        let fct () = (if options.cps then QmlCpsRewriter.cps_pass ~side:`server else QmlCpsRewriter.no_cps_pass) env_cps blender.QmlBlender.code in
        handle_exception_error fct in
      let blender = { blender with QmlBlender.code = code } in

      OManager.flush_errors () ;

      { pass_env with PassHandler.
          env = { pass_env.PassHandler.env with
                    env_blender = blender }
      }
    in
   let precond =
      [
      ] in
    let postcond =
      [
      (*
        This post condition is currently broken because of the second_order bypasses.
        In bsl_ocaml_init.ml they take 1 extra argument, but the bypass_typer does not know
        about it. This condition is desactivated until we solve this probleme.
        QmlCheck.Bypass.applied extract;
      *)
        QmlAlphaConv.Check.alpha extract_final_ac ;
      ] in
    PassHandler.make_pass ~precond ~postcond transform

  let pass_RemoveTyperCrap =
    let transform env =
      let blender = env.PassHandler.env.env_blender in
      let code =
        QmlAstWalk.CodeExpr.map
          (QmlAstWalk.Expr.map_up
             (function
              | QmlAst.Coerce (_, e, _) -> e
              | QmlAst.Directive (_, #QmlAst.type_directive, l, _) ->
                  (match l with
                   | [e] -> e
                   | _ -> assert false)
              | e -> e)
          ) blender.QmlBlender.code in
      {env with PassHandler.env = {env.PassHandler.env with env_blender = {blender with QmlBlender.code}}} in
    PassHandler.make_pass transform

  (* lambda lifting can be switche on without closure, for testing *)
  let if_LambdaLifting ~options _ = options.lambda_lifting || options.qml_closure
  let if_Closure ~options _ = options.qml_closure
  let if_ClosureServer side ~options _ = options.qml_closure && side = `server
  let if_CpsRewriter ~options _ = options.cps

  let if_ConstantSharing ~options _ = options.constant_sharing

  let main ?(dynloader=ignore) ~side ~lang options =
    ObjectFiles.no_init := true;
    PassHandler.make_env options dynloader
    |> PassHandler.handler "BslLoading" pass_BslLoading
    |> PassHandler.handler "Parse" pass_Parse
    |> PassHandler.handler "Blend" pass_Blend

    |> PassHandler.handler "Assertion" pass_Assertion

    (* needed by closures *)
    |> PassHandler.handler "BypassHoisting" pass_BypassHoisting

    |> PassHandler.handler "DiscardRemoteBypasses" (pass_DiscardRemoteBypasses ~lang)

    (* This one is for testing, maybe we'll use it, and update Cps so that it does its
       own lambda lifting. (wip) *)
    (* |> PassHandler.if_handler ~if_:if_LambdaLifting "PreCpsLambdaLifting" pass_LambdaLifting *)

    |> PassHandler.handler "CpsRewriter" (pass_CpsRewriter ~lang)

    |> PassHandler.if_handler ~if_:if_LambdaLifting "LambdaLifting" (pass_LambdaLifting side)
    |> PassHandler.if_handler ~if_:(if_ClosureServer side) "Uncurry" (pass_Uncurry side)
    |> PassHandler.if_handler ~if_:(if_ClosureServer side) "Closure" (pass_Closure ~side)
    |> PassHandler.if_handler ~if_:if_ConstantSharing "ConstantSharing" pass_ConstantSharing
    |> PassHandler.handler "RemoveTyperCrap" pass_RemoveTyperCrap
    |> PassHandler.return

  (* See : where to apply constant sharing ? *)
  (* PassHandler.handler ~if_:if_ConstantSharing "ConstantSharing" pass_ConstantSharing *)
end

(** {6 Back-end passes} *)

module Qml2jsSugar :
sig
  val console : unit -> int
end
=
struct
  let console () =
    let options = Qml2jsOptions.Argv.parse () in
    let module B = (val (Qml2jsOptions.find_backend options.Qml2jsOptions.backend) : Qml2jsOptions.JsBackend) in
    if options.Qml2jsOptions.input_files = [] then 0
    else
      let env_final =
        let dynloader = B.dynloader in
        let options = Qml2jsOptions.Argv.qml2ocaml_sharing options in
        QmlPasses.main ~lang:BslLanguage.js ~side:`client ~dynloader options
      in
      let bsl = env_final.env_bsl in
      let blender = env_final.env_blender in
      let env = PassHandler.make_env options (bsl, blender) in
      env
      |> PassHandler.handler "JavascriptBslfilesLoading" (PassHandler.make_pass (
           fun env ->
             let options = env.PH.options in
             let env_js_input = env.PH.env in
             let generated_files, generated_ast = Qml2js.JsTreat.js_bslfilesloading options bsl in
             PassHandler.make_env options (generated_files,generated_ast,env_js_input)
         ))
      |> PassHandler.handler "JavascriptCompilation" (PassHandler.make_pass (
           fun env ->
             let options = env.PH.options in
             let generated_files, generated_ast, (bsl, blender) = env.PH.env in
             let renaming_client = QmlRenamingMap.empty in
             let renaming_server = QmlRenamingMap.empty in
             let env_js_input = B.compile options ~renaming_server ~renaming_client ~bsl:generated_ast bsl blender in
             PassHandler.make_env options (generated_files, env_js_input)
         ))
      |> PassHandler.handler "JavascriptGeneration" (PassHandler.make_pass (
           fun env ->
             let options = env.PH.options in
             let generated_files, env_js_input = env.PH.env in
             let env_js_output = Qml2js.JsTreat.js_generation options generated_files env_js_input in
             PassHandler.make_env options env_js_output
         ))
      |> PassHandler.handler "JavascriptTreat" (PassHandler.make_pass (
           fun env ->
             let options = env.PH.options in
             let env_js_output = env.PH.env in
             let returned = Qml2js.JsTreat.js_treat options env_js_output in
             PassHandler.make_env options returned
         ))
      |> PassHandler.return
end
