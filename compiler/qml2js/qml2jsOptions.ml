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

(**
   Command-line options for the Qml to JS compiler.

   wip, should be merged with OpaEnv, Qml2ocamlOptions

   @author Mathieu Barbin
   @author Maxime Audouin
*)

(* depends *)
module Arg = Base.Arg
module String = Base.String

module StaticParameters =
struct
  (** options for js exec *)
  let options_js = []

  (** directory to find libs to use with the generated code *)
  let js_include_dir = []

  (** directory to find libs to use with the generated code *)
  let js_include_mlstate_dir = [
    InstallDir.lib_opa ;
  ]
end

type extra_lib = [ `client of (string * BslJsConf.conf)
                 | `server of (string * BslJsConf.conf) ]

type t =
    {
      bypass_plugin : string list ;
      (**
         Beware, unlike opa, this is the list of .bypass files
      *)

      command_line : bool ; (** [true] if the generated js is meant to be run by command
                                line tool in that case, some bsl files are dropped *)
      compilation_directory : string ;
      cps : bool ;
      cps_toplevel_concurrency : bool ;
      exe_run : bool ;
      exe_argv : string list ;
      extra_lib : extra_lib list ;
      extra_path : string list ;
      input_files : Qml2ocamlOptions.input_file list ;
      backend : string;
      jsopt : string list ;
      alpha_renaming : bool;
      cleanup : bool;
      inlining : bool;
      global_inlining : bool;
      lambda_lifting : bool;
      check_bsl_types : bool;
      mlstatelibs : string ;
      no_assert: bool;
      no_stdlib : bool ;
      qml_closure : bool;
      split : bool ;
      split_js_value : int option ;
      target : string ;
      lang : [`js | `node];
      package_version : string;
    }
type env_js_input =
    {
      js_init_contents : (string * [
                          | `ast of (BslInterface.unicity_index * JsAst.code_elt) list
                          | `string of string
                          ]) list ;
      exported : JsIdentSet.t ;
      js_code : JsAst.code ; (** will be split according to argv_options *)
    }


module type JsBackend = sig
  val dynloader : BslPluginInterface.plugin -> unit
  val compile :
    ?runtime_ast:bool ->
    ?val_:(?side:[`client|`server] -> string -> QmlAst.ident) ->
    ?bsl:JsAst.code ->
    ?closure_map:Ident.t IdentMap.t ->
    renaming:QmlRenamingMap.t   ->
    is_distant:(Ident.t -> bool) ->
    bsl_lang:BslLanguage.t ->
    exported:IdentSet.t ->
    t -> BslLib.env_bsl -> QmlTyper.env -> QmlAst.code -> env_js_input
  val name : string
  val runtime_libs : cps:bool -> extra_lib list
  val dummy_compile : unit -> unit (* if the back end is not called because the input code is empty
                                    * then this fake compilation function will be called instead
                                    * The backend should use it to save dummy object files
                                    * for all the separated passes that it contains *)
end
let backends = ref ([] : (module JsBackend) list)
let find_backend name =
  List.find
    (fun backend ->
       let module M = (val backend : JsBackend) in
       M.name = name
    ) !backends
let register_backend b =
  backends := b :: !backends
let backend_names () =
  List.map
    (fun backend ->
       let module M = (val backend : JsBackend) in
       M.name
    ) !backends

module Argv :
sig
  val default : unit -> t
  val parse : unit -> t
  val parse_argv : string array -> t (** see Arg.parse_argv (raise the same exceptions) *)

  (** to be able to share some passes done in qml2ocaml, we need a traduction to qml2ocaml options *)
  (** this traduction does not need to be complete, just should provide options which are used in qml2ocaml passes *)
  val qml2ocaml_sharing : t -> Qml2ocamlOptions.argv_options
end
=
struct
  (** tools *)
  let extra_split g = List.map Base.String.trim (Base.String.slice_chars "{} ,;" g)
  let mutable_list_factory () =
    let s = MutableList.create () in
    ((fun () -> MutableList.to_list s), (fun p -> List.iter (MutableList.add s) (extra_split p)), (fun () -> MutableList.clear s))

  (** Mutable env to set options
      please preserve alphabetic order in independant vars and options *)
  (** !! Default value must no be setted here, but in function reset !! *)
  let bypass_plugins = MutableList.create ()
  let bypass_plugin_add files =
    List.iter (fun file ->
                 assert (String.is_suffix ".bypass" file);
                 MutableList.add bypass_plugins file)
      (extra_split files)
  let compilation_directory = ref None
  let cps = ref false
  let cps_toplevel_concurrency = ref false
  let exe_run = ref false
  let exe_argv_get, exe_argv_add, exe_argv_reset = mutable_list_factory ()
  let extra_path_get, extra_path_add, extra_path_reset = mutable_list_factory ()
  let input_files = MutableList.create ()
  let input_files_get () = MutableList.to_list input_files
  let input_files_reset () = MutableList.clear input_files
  let add_input_qml_file qml = List.iter (fun qml -> MutableList.add input_files (Qml2ocamlOptions.QmlFile qml)) (extra_split qml)
  let add_input_opa_file opa = List.iter (fun opa -> MutableList.add input_files (Qml2ocamlOptions.OpaFile opa)) (extra_split opa)
  let backend = ref ""
  let jsopt_get, jsopt_add, jsopt_reset = mutable_list_factory ()
  let lambda_lifting = ref false
  let check_bsl_types = ref false
  let mlstatelibs = ref ""
  let mlstatepath_get, mlstatepath_add, mlstatepath_reset = mutable_list_factory ()
  let no_assert=ref false
  let no_stdlib = ref false
  let alpha_renaming = ref true
  let cleanup = ref true
  let inlining = ref true
  let global_inlining = ref true
  let qml_closure = ref false
  let split = ref false
  let split_js_value = ref None
  let target = ref None
  let package_version = ref "0.1.0"

  let plugin_inclusion file =
    let cwd = Sys.getcwd () in
    let inclusion = BslConvention.inclusion ~cwd file in
    extra_path_add inclusion.BslConvention.extrapath ;
    bypass_plugin_add inclusion.BslConvention.plugin ;
    ()

  (** DEFAULT VALUE and reset (if 2 time parsed (differents argv) *)
  let reset () =
    MutableList.clear bypass_plugins ;
    compilation_directory := None ;
    cps := false ;
    cps_toplevel_concurrency := false ;
    exe_run := false ;
    exe_argv_reset () ;
    let _ = extra_path_reset () ;
      List.iter extra_path_add StaticParameters.js_include_dir in
    input_files_reset () ;
    backend := "qmljsimp";
    let _ =
      jsopt_reset () ;
      List.iter (fun s -> jsopt_add s) StaticParameters.options_js ;
    in
    lambda_lifting := false;
    check_bsl_types := false;
    mlstatelibs := Lazy.force InstallDir.getenv ;
    let _ =
      mlstatepath_reset () ;
      List.iter mlstatepath_add StaticParameters.js_include_mlstate_dir in
    no_assert := false;
    no_stdlib := false ;
    qml_closure := false;
    split := false ;
    split_js_value := None ;
    target := None ;
    package_version := "0.1.0";
    ()

  let speclist_aux () =
    [
      ("*.qml", Arg.Unit (fun () -> ()), " Give a qml file to the compiler");
      ("*.opa", Arg.Unit (fun () -> ()), " Give a opa file to the compiler");
      ("--qml", Arg.String add_input_qml_file, "<file> load a file as a qml source");
      ("--opa", Arg.String add_input_opa_file, "<file> load a file as a opa source");
      ("--", Arg.Tuple [Arg.Set exe_run ; Arg.Rest exe_argv_add], " -- Take every remaining options, and run directly the exe with them after compilation");
      ("--back-end", Arg.Symbol (backend_names (), (fun s -> backend := s)), " choose the js backend");
      ("--build-dir", Arg.String (fun s -> compilation_directory := Some s), "<dir> -- Specify a directory for js production (def is _build/target/)");
      ("--check-bsl-types", Arg.Set check_bsl_types, " Enables runtime type checking of the types of bypasses");
      ("--closure", Arg.Tuple [ Arg.Set lambda_lifting ; Arg.Set qml_closure ], " -- Use Qml closures (enforce --lambda-lifting)");
      ("--cps", Arg.Set cps, " -- Activate cps transformation mode");
      ("--cps-toplevel-concurrency", Arg.Tuple [ Arg.Set cps ; Arg.Set cps_toplevel_concurrency ],
       " -- During cps transformation, toplevel not functionnal values are compiled as future (enforce --cps)");
      ("--extra-path", Arg.String extra_path_add, "<dir> -- Add an include directory for searching libs");
      ("--jsopt", Arg.String jsopt_add, "<opt> -- Pass option <opt> to js exec only");
      ("--lambda-lifting", Arg.Set lambda_lifting, " -- Use Qml lambda lifting");
      ("--mlstate-I", Arg.String mlstatepath_add, "<dir> -- Add an include directory from mlstatelibs");
      ("--no-assert", Arg.Set no_assert, " -- Remove all assert directives. Faster but less safe.");
      ("--no-alpha-renaming", Arg.Clear alpha_renaming, " -- disable alpha-renaming for local names");
      ("--no-cleanup", Arg.Clear cleanup, " -- disable clean up of the produced js");
      ("--no-inlining", Arg.Clear inlining, " -- disable inlining of the produced js");
      ("--no-global-inlining", Arg.Clear global_inlining, " -- disable toplevel inlining of the produced js");
      ("--no-stdlib", Arg.Set no_stdlib, " -- Do not use qml-initial");
      ("--split", Arg.Set split, " -- Do not merge all js-files in one uniq target");
      ("--split-js-value", Arg.Int (fun i -> split_js_value := Some i), " -- UNDOCUMENTED");
      ("-I", Arg.String extra_path_add, "<dir> -- Shorthand for --extra-path");
      ("-mI", Arg.String mlstatepath_add, "<dir> -- Shorthand for --mlstate-I");
      ("-o", Arg.String (fun s -> target := Some s), "<exe> -- Specify a name for the target");
    ]
    @ OManager.Arg.options
    @ PassHandler.Arg.options
    @ WarningClass.Arg.options
    @ Qml2jsBackendOptions.Arg.options
    @ OpaSyntax.Args.options

  let speclist () =
    Arg.sort
      (Arg.align
         (Arg.add_bash_completion
            ~names:["qmljs";"qmljs.native";"qmljs.byte";"qmljs.exe"]
            ~default:(Arg.File "@(opa|qml|cmxs|js|bypass)")
            (speclist_aux ())
         )
      )

  let anon_fun arg =
    let ext = File.extension arg in
    let fct =
      match ext with
      | "qml" -> add_input_qml_file
      | "opa" -> add_input_opa_file
      | "bypass" -> bypass_plugin_add
      | "opp" -> plugin_inclusion
      | _ -> (fun s -> OManager.error "I don't know what to do with anonymous argument %S@\n" s) in
    fct arg

  (** building the compiler input from current mutable env.
      @parameter [backend] either "qmlflat" or "qmlfake" *)
  let build_argv_options () =
    let input_files = input_files_get () in
    let last_qml = match List.rev input_files with t::_ -> Qml2ocamlOptions.input_filename t | _ -> "a.qml" in
    let target_dir = File.from_pattern "%" last_qml in
    let target = Option.default (File.from_pattern "%b" last_qml) !target in
    {
      bypass_plugin = MutableList.to_list bypass_plugins ;
      command_line = true; (* no need to have an arg parser for this option
                            * if you use the command line qmljs, then you will
                            * execute the js in command line too *)
      compilation_directory = Option.default (Filename.concat "_build" target_dir) !compilation_directory ;
      cps = !cps ;
      cps_toplevel_concurrency = !cps_toplevel_concurrency ;
      exe_run = !exe_run ;
      exe_argv = exe_argv_get () ;
      extra_lib = [] ;
      extra_path = (List.map (fun s -> Filename.concat !mlstatelibs s) (mlstatepath_get ())) @ (extra_path_get ()) ;
      input_files = input_files ;
      backend = !backend ;
      jsopt = jsopt_get () ;
      alpha_renaming = !alpha_renaming;
      cleanup = !cleanup;
      inlining = !inlining;
      global_inlining = !global_inlining;
      lambda_lifting = !lambda_lifting;
      check_bsl_types = !check_bsl_types;
      mlstatelibs = !mlstatelibs ;
      no_assert= !no_assert ;
      no_stdlib = !no_stdlib ;
      qml_closure = !qml_closure;
      split = !split ;
      split_js_value = !split_js_value ;
      target = target;
      lang = `js;
      package_version = !package_version;
    }

  let usage_msg = Printf.sprintf "%s: command-line options for the Qml-to-JS compiler\nUsage: %s [options]\n" Sys.argv.(0) Sys.argv.(0)

  (** parse *)
  let parse () =
    reset () ;
    Arg.parse (speclist ()) anon_fun (usage_msg ^ "Options:");
    let o = build_argv_options () in
    let module M = (val (find_backend o.backend) : JsBackend) in
    {o with extra_lib = M.runtime_libs ~cps:o.cps }
  let parse_argv argv =
    reset () ;
    let current = ref 0 in
    Arg.parse_argv ~current argv (speclist ()) anon_fun "<qml2jslib>" ;
    let o = build_argv_options () in
    let module M = (val (find_backend o.backend) : JsBackend) in
    {o with extra_lib = M.runtime_libs ~cps:o.cps }
  let default () =
    reset () ;
    build_argv_options ()



  (** this traduction does not need to be complete, just should provide options which are used in qml2ocaml passes shared by qml2js.ml *)
  let qml2ocaml_sharing t =
    let qml2ocaml = Qml2ocamlOptions.ArgvOptions.default "qmlflat" in (* hack because of dirty string arg in qml2ocaml *)
    { qml2ocaml with Qml2ocamlOptions.
        bypass_plugin = t.bypass_plugin ;
        cps = t.cps ;
        cps_toplevel_concurrency = t.cps_toplevel_concurrency ;
        exe_run = t.exe_run ;
        exe_argv = t.exe_argv ;
        extra_path = t.extra_path ;
        input_files = t.input_files ;
        lambda_lifting = t.lambda_lifting;
        no_assert = t.no_assert ;
        no_stdlib = t.no_stdlib ;
        qml_closure = t.qml_closure;
        server_side = false;
        target = t.target ;
    }
end
