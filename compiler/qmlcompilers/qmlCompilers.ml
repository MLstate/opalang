(*
    Copyright © 2011 MLstate

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
  env_typer : QmlTyper.env ;
  code : QmlAst.code ;
}

let extract_final_ac env = env.env_typer.QmlTypes.annotmap, env.code
let extract_final_agc env = env.env_typer.QmlTypes.annotmap, (env.env_typer.QmlTypes.gamma, env.env_typer.QmlTypes.gamma) , env.code
let extract_final_code env = env.code
let extract_final_bypass_typer env = env.env_typer.QmlTypes.bypass_typer
let extract_final_bypass_typer_code env = extract_final_bypass_typer env, extract_final_code env

(**
   Get a function working on env_final from a process_code working on gamma, annotmap and code
*)
let final_sugar ~process_code =
  let pass env_final =
    let env_typer = env_final.env_typer in
    let gamma = env_typer.QmlTypes.gamma in
    let annotmap = env_typer.QmlTypes.annotmap in
    let code = env_final.code in
    let (gamma, annotmap), code = process_code gamma annotmap code in
    { env_final with
        env_typer = { env_typer with QmlTypes.
                        gamma ;
                        annotmap ;
                    } ;
        code ;
    }
  in
  ( pass : env_final -> env_final )

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
     AlphaConv + Typing
  *)
  val pass_Typing :
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
  let env_Parse_extract_ac env = (QmlAnnotMap.empty, (QmlTypes.Env.empty, QmlTypes.Env.empty), env_Parse_extract_code env)

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
    | (QmlTypes.Exception _ | QmlTyperException.Exception _) as exn ->
        (* At this point, we do not have any environment nor annotations map. *)
        OManager.error "Typer : %a"
          (QmlTyperErrHandling.pp_report_from_typer_exception QmlAnnotMap.empty)
          exn ;
    | QmlCpsRewriter.Exception e ->
        OManager.error "QmlCps : %s" (QmlCpsRewriter.error_message e)

  module HighTyper = QmlTyper.DynamicallyChangeableTyper.HighTyper

  let pass_Typing =
    let transform pass_env =
      let env_bsl, qml = pass_env.PassHandler.env in
      (** construction of bypass_typer *)
      let bypass_typer =
        BslLib.BSL.ByPassMap.bypass_typer env_bsl.BslLib.bymap in
      let env_typer =
        HighTyper.initial
          ~bypass_typer ~explicit_instantiation: false
          ~value_restriction: `disabled ~exported_values_idents: IdentSet.empty
          () in
      let env_typer, code =
        let code = Option.default [] qml.init_code @ qml.user_code in
        let fct () =
          (* 1°: sorting things out *)
          let code_defs, code_dbfiles, code_dbdefs, code =
            (* verbose "I-1) Sorting top-level nodes"; *)
            let sort_user = QmlAstSort.add QmlAstSort.empty code in
            let code_defs = QmlAstSort.Get.new_type sort_user
            and code_dbfiles = QmlAstSort.Get.database sort_user
            and code_dbdefs = QmlAstSort.Get.new_db_value sort_user
            and user_code = QmlAstSort.Get.new_val sort_user
            in code_defs, code_dbfiles, code_dbdefs, user_code
          in

          assert (code_dbfiles = []);
          assert (code_dbdefs = []);

          (* pre-2: dependency analysis on type definitions *)
          QmlTypes.check_no_duplicate_type_defs code_defs;

          (* 2°: getting type definitions into Gamma *)
          let env_typer = HighTyper.fold env_typer code_defs in

          (* 5°: alpha-conversion *)
          let code =
            let alpha = QmlAlphaConv.next () in
            (* verbose "I-5) alpha-converting the code"; *)
            let _, code = QmlAlphaConv.code alpha code in
            code
          in

          (* 6°: typing *)
          let env_typer = HighTyper.fold env_typer code in
          env_typer, code
        in
        handle_exception_error fct
      in

      OManager.flush_errors () ;

      let env =
        {
          env_bsl ;
          env_typer ;
          code ;
        }
      in
      let printers = QmlTracker.printers extract_final_agc in
      let trackers = QmlTracker.trackers extract_final_code in
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
        QmlAlphaConv.Check.alpha extract_final_ac
      ] in
    PassHandler.make_pass ~precond ~postcond transform

  (* ============================================================================== *)

  (* FIXME: passing options around : if the type options is defined somewhere higher *)
  (* Or even if a pass gives directly command arg tuple ?*)
  (* need to pass some options to the pass from argv_options *)

  let make_final_pass precond postcond process_code =
    let transform pass_env =
      let env_final = pass_env.PassHandler.env in
      let env_final =
        let fct () = final_sugar ~process_code env_final in
        handle_exception_error fct
      in
      { pass_env with PassHandler.
          env = env_final ;
      }
    in
    PassHandler.make_pass ~precond ~postcond transform

  let make_final_pass_options precond postcond process_code =
    let transform pass_env =
      let options = pass_env.PassHandler.options in
      let env_final = pass_env.PassHandler.env in
      let process_code = process_code options in
      let env_final =
        let fct () = final_sugar ~process_code env_final in
        handle_exception_error fct
      in
      { pass_env with PassHandler.
          env = env_final ;
      }
    in
    PassHandler.make_pass ~precond ~postcond transform


  let make_final_pass_options_env_typer precond postcond process_code =
    let transform pass_env =
      let options = pass_env.PassHandler.options in
      let env_final = pass_env.PassHandler.env in
      let env_typer = env_final.env_typer in
      let env_final =
        let fct () = final_sugar ~process_code:(process_code options env_typer) env_final in
        handle_exception_error fct
      in
      { pass_env with PassHandler.
          env = env_final ;
      }
    in
    PassHandler.make_pass ~precond ~postcond transform

  let make_final_pass_bypass_typer precond postcond process_code =
    let transform pass_env =
      let env_final = pass_env.PassHandler.env in
      let bypass_typer = env_final.env_typer.QmlTypes.bypass_typer in
      let env_final =
        let fct () = final_sugar ~process_code:(process_code bypass_typer) env_final in
        handle_exception_error fct
      in
      { pass_env with PassHandler.
          env = env_final ;
      }
    in
    PassHandler.make_pass ~precond ~postcond transform

  let make_final_pass_bymap precond postcond process_code =
    let transform pass_env =
      let env_final = pass_env.PassHandler.env in
      let bymap = env_final.env_bsl.BslLib.bymap in
      let env_final =
        let fct () = final_sugar ~process_code:(process_code ~bymap) env_final in
        handle_exception_error fct
      in
      { pass_env with PassHandler.
          env = env_final ;
      }
    in
    PassHandler.make_pass ~precond ~postcond transform

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
    make_final_pass_options precond postcond
      (fun options -> Pass_LambdaLifting.process_code ~early:false ~typed:(not options.cps) ~side)

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
    make_final_pass_options precond postcond
      (fun options gamma annotmap code ->
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
    make_final_pass_options_env_typer precond postcond
      (fun options env_typer ->
         Pass_Closure.process_code
           ~typed:(not options.cps)
           ~side
           ~renaming_server:QmlRenamingMap.empty
           ~renaming_client:QmlRenamingMap.empty
           env_typer.QmlTypes.bypass_typer)

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
      let env_final = pass_env.PassHandler.env in
      let env_final =
        final_sugar ~process_code env_final
      in
      { pass_env with PassHandler.
          env = env_final ;
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
      let env_final = pass_env.PassHandler.env in
      let code = env_final.code in
      let env_bsl = env_final.env_bsl in
      let env_typer = env_final.env_typer in
      let cps_options =
        { QmlCpsRewriter.default_options with QmlCpsRewriter.
            no_assert = options.no_assert ;
            qml_closure = options.qml_closure ;
            toplevel_concurrency = options.cps_toplevel_concurrency ;
            server_side = options.server_side
        } in
      let bsl_bypass_typer key =
        match BslLib.BSL.ByPassMap.bsl_bypass_typer env_bsl.BslLib.bymap key with
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
          env_bsl.BslLib.bymap key
        with
        | None ->
            OManager.error (
            "%%%% @{<bright>%s@} %%%% : unknown external primitives are not allowed@\n"^^
            "Please use a plugin"
            )
              (BslKey.to_string key)
        | Some t -> t in
      let bsl_bypass_cps =
        BslLib.BSL.ByPassMap.bsl_bypass_cps ~lang env_bsl.BslLib.bymap in
      let env_cps =
        QmlCpsRewriter.env_initial
          ~options:cps_options
          ~bsl_bypass_typer
          ~bsl_bypass_tags
          ~bsl_bypass_cps
          ~typing:env_typer
          ()
      in
      let code =
        let fct () =
          (if options.cps then QmlCpsRewriter.cps_pass ~side:`server else QmlCpsRewriter.no_cps_pass)
            env_cps code
        in
        handle_exception_error fct in
      let env_final = { env_final with code ; } in

      OManager.flush_errors () ;

      { pass_env with PassHandler.
          env = env_final ;
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
      let env_final = env.PassHandler.env in
      let code = env_final.code in
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
          ) code in
      let env_final = { env_final with code } in
      {env with PassHandler.env = env_final }
    in
    PassHandler.make_pass transform

  (* lambda lifting can be switche on without closure, for testing *)
  let if_LambdaLifting ~options _ = options.lambda_lifting || options.qml_closure
  let if_Closure ~options _ = options.qml_closure
  let if_ClosureServer side ~options _ = options.qml_closure && side = `server
  let if_CpsRewriter ~options _ = options.cps

  let if_ConstantSharing ~options _ = options.constant_sharing

  let main ?(dynloader=ignore) ~side ~lang options =
    let open PassHandler in
    ObjectFiles.no_init := true;
    PassHandler.make_env options dynloader
    |+> ("BslLoading", pass_BslLoading)
    |+> ("Parse", pass_Parse)
    |+> ("Typing", pass_Typing)

    |+> ("Assertion", pass_Assertion)

    (* needed by closures *)
    |+> ("BypassHoisting", pass_BypassHoisting)

    |+> ("DiscardRemoteBypasses", pass_DiscardRemoteBypasses ~lang)

    |?> (if_CpsRewriter, "CpsRewriter", pass_CpsRewriter ~lang)

    |?> (if_LambdaLifting, "LambdaLifting", pass_LambdaLifting side)
    |?> (if_ClosureServer side, "Uncurry", pass_Uncurry side)
    |?> (if_ClosureServer side, "Closure", pass_Closure ~side)
    |?> (if_ConstantSharing, "ConstantSharing", pass_ConstantSharing)
    |+> ("RemoveTyperCrap", pass_RemoveTyperCrap)
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
      let env = PassHandler.make_env options env_final in
      env
      |> PassHandler.handler "JavascriptBslfilesLoading" (PassHandler.make_pass (
           fun env ->
             let options = env.PH.options in
             let env_final = env.PH.env in
             let env_bsl = env_final.env_bsl in
             let generated_files, generated_ast = Qml2js.JsTreat.js_bslfilesloading options env_bsl in
             PassHandler.make_env options (generated_files, generated_ast, env_final)
         ))
      |> PassHandler.handler "JavascriptCompilation" (PassHandler.make_pass (
           fun env ->
             let options = env.PH.options in
             let generated_files, generated_ast, env_final = env.PH.env in
             let { env_bsl ; env_typer ; code } = env_final in
             let renaming_client = QmlRenamingMap.empty in
             let renaming_server = QmlRenamingMap.empty in
             let env_js_input = B.compile options ~renaming_server ~renaming_client ~bsl:generated_ast env_bsl env_typer code ~bsl_lang:BslLanguage.js in
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
