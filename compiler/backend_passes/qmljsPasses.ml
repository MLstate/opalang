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

module P = Passes
module PH = PassHandler

module List = BaseList

type env_JsCompilation = {
  env_js_input : Qml2jsOptions.env_js_input;
  jsoptions : Qml2jsOptions.t;
  env_bsl : BslLib.env_bsl;
  loaded_bsl : Qml2js.loaded_bsl;
  is_distant : JsIdent.t -> bool;
}

let pass_ServerJavascriptCompilation =
  PassHandler.make_pass
    (fun e ->
       let options = e.PH.options in
       let env = e.PH.env in
       let module JsBackend = (val (options.OpaEnv.js_back_end) : Qml2jsOptions.JsBackend) in
       let compilation_directory =
         match ObjectFiles.compilation_mode () with
         | `compilation -> Option.get (ObjectFiles.get_compilation_directory ())
         | `init | `linking -> "_build"
         | `prelude -> assert false
       in
       let jsoptions =
         let argv_options = Qml2jsOptions.Argv.default () in
         { argv_options with Qml2jsOptions.
             cps = options.OpaEnv.cps;
             cps_toplevel_concurrency = options.OpaEnv.cps_toplevel_concurrency ;
             qml_closure = options.OpaEnv.closure;
             extra_lib = options.OpaEnv.extrajs;
             alpha_renaming = options.OpaEnv.js_local_renaming;
             check_bsl_types = options.OpaEnv.js_check_bsl_types;
             cleanup = options.OpaEnv.js_cleanup;
             inlining = options.OpaEnv.js_local_inlining;
             global_inlining = options.OpaEnv.js_global_inlining;
             no_assert = options.OpaEnv.no_assert;
             target = options.OpaEnv.target;
             compilation_directory;
             static_link = options.OpaEnv.static_link;
             package_version = options.OpaEnv.package_version;
             modular_plugins = options.OpaEnv.modular_plugins;
             lang = `node;
         } in
       let jsoptions =
         match options.OpaEnv.run_server_options with
         | None -> jsoptions
         | Some exe_argv ->
             { jsoptions with Qml2jsOptions. exe_argv; exe_run = true }
       in
       let env_bsl = env.Passes.newFinalCompile_bsl in
       let loaded_bsl =
         Qml2js.JsTreat.js_bslfilesloading jsoptions env_bsl in
       let is_distant, renaming =
         let other = env.P.newFinalCompile_renaming_client in
         let here  = env.P.newFinalCompile_renaming_server in
         S3Passes.EnvUtils.jsutils_from_renamings ~here ~other
       in
       let exported = env.Passes.newFinalCompile_exported in
       let env_js_input = JsBackend.compile
         ~runtime_ast:false
         ~bsl:loaded_bsl.Qml2js.generated_ast
         ~val_:OpaMapToIdent.val_
         ~closure_map:env.Passes.newFinalCompile_closure_map
         ~is_distant
         ~renaming
         ~bsl_lang:BslLanguage.nodejs
         ~exported
         jsoptions
         env_bsl
         env.Passes.newFinalCompile_qml_milkshake.QmlBlender.env
         env.Passes.newFinalCompile_qml_milkshake.QmlBlender.code
       in
       let is_distant ident =
         match ident with
         | JsIdent.ExprIdent i ->
             (try
                ignore
                  (QmlRenamingMap.new_from_original
                     env.P.newFinalCompile_renaming_client
                i);
                true
              with Not_found -> false)
         | _ -> false
       in
       PH.make_env options {
         env_js_input;
         jsoptions;
         env_bsl;
         loaded_bsl;
         is_distant;
       }
    )

let pass_ServerJavascriptOptimization =
  PassHandler.make_pass
    (fun e ->
       let env = e.PH.env in
       let exported = env.env_js_input.Qml2jsOptions.exported in
       let is_exported i = JsIdentSet.mem i exported || env.is_distant i in
       let js_code =
         Pass_ServerJavascriptOptimization.process_code
           is_exported
           env.env_js_input.Qml2jsOptions.js_code
       in
       let js_init_contents =
         List.map
           (fun (x, c) -> x,
              match c with
              | `string _ -> assert false
              | `ast proj -> `ast
                  (List.map
                     (fun (i, e) ->
                        (i, Pass_ServerJavascriptOptimization.process_code_elt
                           is_exported e))
                     proj)
           ) env.env_js_input.Qml2jsOptions.js_init_contents
       in
       PH.make_env e.PH.options
         { env with env_js_input =
             { env.env_js_input with Qml2jsOptions. js_code; js_init_contents }
         }
    )

let pass_ServerJavascriptGeneration =
  PassHandler.make_pass
    (fun e ->
       let env = e.PH.env in
       let jsoptions = env.jsoptions in
       let env_bsl = env.env_bsl in
       let loaded_bsl = env.loaded_bsl in
       let env_js_output =
         Qml2js.JsTreat.js_generation jsoptions env_bsl
           loaded_bsl env.env_js_input
       in
       let code = Qml2js.JsTreat.js_treat jsoptions env_js_output in
       PH.make_env e.PH.options code
    )
