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
   Command line options and Static Parameters for opa.exe.

   wip, some clean-up is needed (deprecated options, sharing with qml2ocaml, etc...)

   @author Cédric Soulas
   @author Mathieu Barbin
   @author Mehdi Bouaziz
*)

(* depends *)
module Arg = Base.Arg
module Format = BaseFormat
module String = BaseString

(* - *)

module Parameters =
struct

  let bsl_client_language = BslLanguage.js
  let bsl_server_language = BslLanguage.ml

  (** ==============================================*)
  (** COMPILATION OF SERVERS WITH OCAML *)

  (** the following values are modifiable with the opa options *)
  let ocamlopt = "ocamlopt.opt"
  let ocamlc = "ocamlc.opt"

  (** the other values are not mutable -- cf opa options -I -x to add some more libraries *)
  (** these parameters are DEFINED and DOCUMENTED in qml2llvm/qml2ocaml/qml2ocamlOptions.ml *)
  include Qml2ocamlOptions.StaticParameters

(** ==============================================*)
end

module I18n = struct
  let template_message ext = Printf.sprintf " Generate %s source code template as translation package, use --i18n-pkg to specify the output file name and corresponding opa package name" ext

  module Type = struct
    type options = {
      template_opa : bool;
      template_po : bool;
      pkg : string list;
      dir : string list;
    }
  end

  include Type

  let default_options = {
    template_opa = false;
    template_po = false;
    pkg = [] ;
    dir = [] ;
  }

  let r = ref default_options

  let options =   [
    "--i18n-template-opa",
    Arg.Unit (fun () -> r := {!r with template_opa=true}),
    template_message "opa";

    "--i18n-template-po",
    Arg.Unit (fun () -> r := {!r with template_po=true}) ,
    template_message "po";

    "--i18n-pkg",
    Arg.String (fun str -> r := {!r with pkg = str :: (!r.pkg)}),
    " Use the explicitely give package name for i18n";

    "--i18n-dir",
    Arg.String (fun str -> r := {!r with dir = str :: (!r.dir)}),
    " Specify the directory containing translations";

  ]

end

let cwd = Sys.getcwd ()

let available_js_back_end_list = Qml2jsOptions.backend_names ()
let available_js_back_end_of_string = Qml2jsOptions.find_backend

type available_back_end = [ `qmlflat | `qmljs ]
let available_back_end_list = [ "qmlflat" ; "qmljs" ; "native" ; "node" ; "js" ; "nodejs"; "node.js" ]
let available_back_end_of_string : string -> available_back_end option = function
  | "native"
  | "qmlflat" -> Some `qmlflat
  | "node"
  | "js"
  | "nodejs"
  | "node.js"
  | "qmljs" -> Some `qmljs
  | _ -> None
let string_of_available_back_end : available_back_end -> string = function
  | `qmlflat -> "qmlflat"
  | `qmljs -> "qmljs"

type opa_options = {

  ocamlc : string ;
  ocamlopt : string ;

  ccopt : string list ;
  cclib : string list ;

  mlcopt : string list ;
  mllopt : string list ;

  makefile_rule : Qml2ocamlOptions.makefile_rule ;
  back_end : available_back_end ;
  js_back_end : (module Qml2jsOptions.JsBackend) ;
  hacker_mode : bool ;

  filenames : string list;
  server_plugin_files : string list;
  client_plugin_files : string list;
  show_types : bool ;

  (* n *)

  no_assert : bool ;

  no_server : bool option ; (* None means "look if there is a server declaration"
                             * Some true means "no server"
                             * Some false means "with a server" *)
  stdlib : bool ;
  embedded_opa : bool ;

  show_compilation : bool ;
  no_cache_parse : bool ;
  no_discard_of_unused_stdlib : bool ;
  cps : bool ;
  cps_client : bool ;
  cps_toplevel_concurrency : bool ;
  closure : bool ;
  extralibs : string list ;
  extrajs : Qml2jsOptions.extra_lib list ;
  extrapath : string list ;
  resname : string ;
  target : string ;
  explicit_instantiation : bool ;
  value_restriction : [`disabled|`normal|`strict] ;
  dump_dbgen_schema : string option ;
  bypass_plugin : string list ;
  compile_release:bool;
  run_server_options : string list option;
  build_dir : string;
  slicer_test : bool;
  slicer_dump : bool;
  rpc_options : int * int * int;

  profile : bool;
  mime_database : string option;
  project_root : string;
  root_inclusions : string option;
  undot : bool;
  generate_interface : bool;
  generate_interface_and_compile : bool;
  js_serialize : [`adhoc|`ast];
  constant_sharing : bool;
  constant_sharing_client : bool;
  js_check_bsl_types : bool;
  js_cleanup : bool;
  js_local_inlining : bool;
  js_global_inlining : bool;
  js_local_renaming : bool;
  publish_src_code : bool; (** if true, application source code will be published at [_internal_/src_code] *)

  i18n : I18n.options ;

  parser_ : OpaSyntax.Args.options;

  static_link : bool; (* Whether or not to link object files statically *)
  package_version: string; (* The version to be used when outputting
                              the package.json file *)
  modular_plugins: bool;
}

let i18n_template option = option.i18n.I18n.template_opa || option.i18n.I18n.template_po

module Options :
sig
  val parse_options : unit -> unit
  val get_options : unit -> opa_options
  val echo_help : unit -> unit

  (** Fill a pprocess environment from opa options. *)
  val to_ppenv : opa_options -> Pprocess.env -> Pprocess.env

  val write_manpage : out_channel -> unit
end
=
struct

  module ArgParser =
  struct
    module Env =
    struct
      let ocamlopt = ref (lazy Parameters.ocamlopt)
      let ocamlc = ref (lazy Parameters.ocamlc)

      let envvar =
        [
          "OCAMLOPT", "Path of the ocaml native compiler, e.g. ocamlopt, ocamlopt.opt, /usr/local/bin/ocamlopt ...", ocamlopt;
        ]

      let set =
        List.iter (
          fun (v, _, ref_)->
            try
              let res = Sys.getenv v in
              ref_ := lazy res
            with Not_found -> ()
        )
          envvar
    end
    let hacker_mode = ref false
    let run_server_options = ref None
    let print_help = ref false
    let explicit_instantiation = ref true
    let value_restriction = ref `normal
    let target_only_qml = ref "only_qml.qml"
    let target = ref ""
    let show_types = ref false

    (* n *)

    let no_assert = ref false
    let no_server = ref None

    let stdlib = ref true
    let show_compilation = ref false
    let no_cache_parse = ref false
    let no_discard_of_unused_stdlib = ref false
    let cps = ref true
    let cps_client = ref false
    let cps_toplevel_concurrency = ref false
    let closure = ref true
    let embedded_opa = ref true
    let build_dir = ref ""
    let makefile_rule = ref Qml2ocamlOptions.Native
    let opa_walker = ref None
    let profile = ref false
    let slicer_test = ref false
    let slicer_dump = ref false
    let rpc_options = ref (1,0,0)
    let mime_database = ref None
    let project_root = ref cwd
    let root_inclusions = ref None
    let undot = ref true
    let generate_interface = ref false
    let generate_interface_and_compile = ref false
    let js_serialize = ref `ast

    let constant_sharing = ref true
    let constant_sharing_client = ref false

    let js_check_bsl_types = ref false
    let js_cleanup = ref true
    let js_local_inlining = ref true
    let js_global_inlining = ref true
    let js_local_renaming = ref true
    (* in release, force publishing source code ; otherwise, don't
       publish unless --publish-src-code). *)
    let publish_src_code = ref false

    let static_link = ref false

    let package_version = ref "0.1.0"
    let set_package_version version =
      package_version := version

    let modular_plugins = ref false

    let back_end_wanted = ref ( `qmljs : available_back_end )
    let back_end s =
      let back_end =
        match available_back_end_of_string s with
        | None -> assert false (* use symbol in Arg.parse *)
        | Some back_end -> back_end in
      back_end_wanted := back_end;
      match back_end with
      | `qmlflat -> js_serialize := `adhoc
      | _ -> ()
    let js_back_end_wanted_name = "qmljsimp"
    let js_back_end_wanted = ref (available_js_back_end_of_string js_back_end_wanted_name)
    let js_back_end s =
      js_back_end_wanted := available_js_back_end_of_string s

    let dump_dbgen_schema = ref false
    let target_qmli = ref ""
    let target_dbgen_schema = ref ""

    let compile_release  = ref false
    let target_opt = ref None
    let gen_change_target target fct s =
(*       let s = (File.chop_extension s)^".exe" in *)
      target := fct s
    let change_target = gen_change_target target_opt (fun t -> Some t)
    let last_target_from_file = ref "a"
    let change_last_file s =
      gen_change_target last_target_from_file (fun t -> t) (File.chop_extension s)

    let filenames = ref []
    let mutable_filenames = MutableList.create ()
    let add_any_file f =
      if Sys.file_exists f && not (Sys.is_directory f) then (change_last_file f; MutableList.add mutable_filenames f)
      else OManager.error "I/O error: @{<bright>%S@} -> No such file or directory" f
    let add_opa_file f =
      if File.extension f = "opa" then add_any_file f
      else OManager.error (
        "I don't know what to do with file @{<bright>%S@}@\n"^^
        "@[<2>@{<bright>Hint@}:@\n"^^
        "Use option @{<bright>-impl@} if that is really an opa source@]" )
        f

    let client_plugin_files = MutableList.create ()
    let server_plugin_files = MutableList.create ()

    let extra_split g =
      List.map String.trim (String.slice_chars "{} ,;'" g)

    (** lib & js for ocaml compilation *)
    let ccopt = MutableList.create ()
    let cclib = MutableList.create ()
    let mlcopt = MutableList.create ()
    let mllopt = MutableList.create ()

    let add_ccopt f = List.iter (MutableList.add ccopt) (extra_split f)
    let add_cclib f = List.iter (MutableList.add cclib) (extra_split f)
    let add_mlcopt f = List.iter (MutableList.add mlcopt) (extra_split f)
    let add_mllopt f = List.iter (MutableList.add mllopt) (extra_split f)

    let extralibs = ref []
    let mutable_extralibs = MutableList.create ()
    let extrajs = ref []
    let add_extra_lib f =
      let ext = File.extension f in
      let expected = [""; "cmxa"; "cma"; "cmo"; "cmx"] in
      if List.mem ext expected then
        MutableList.add mutable_extralibs f
      else
        OManager.error (
          "I don't know what to do with arg @{<bright>%S@}@\n"^^
          "@[<2>@{<bright>Hint@}:@\n"^^
          "expected extensions for @{<bright>--extra-lib@} are {%a}@]"
        )
          f
          (Format.pp_list ", " (fun fmt -> Format.fprintf fmt "%S")) expected

    let set_project_root dir =
        try
            if Sys.is_directory dir then
              begin
                if Filename.is_relative dir then
                  project_root := Filename.concat (Unix.getcwd ()) dir
                else
                  project_root := dir
                ;
                OManager.verbose "Setting project root to %s" !project_root;
              end
            else
                OManager.error "I/O error: @{<bright>%S@} -> No such directory" dir
        with Sys_error _ ->
                OManager.error "I/O error: @{<bright>%S@} -> No such directory" dir

    let set_root_inclusions dir = root_inclusions := Some dir

    let set_mime_database f =
        if Sys.file_exists f && not (Sys.is_directory f) then
            if File.extension f =  "xml" then
                mime_database := Some f
            else
                OManager.error "%S : Bad extension. Given file must be a .xml file" f
        else
          OManager.error "I/O error: @{<bright>%S@} -> No such file or directory" f


    let add_full_extra_lib s = List.iter add_extra_lib (extra_split s)

    (** plugin for bypass : needed and dynlinked by opa, not by the ocaml compilation *)
    let bypass_plugin = ref []
    let mutable_bypass_plugin = MutableList.create ()
    let plugin_inclusion file = MutableList.add mutable_bypass_plugin file

    (** path for ocaml compilation *)
    let extrapath = ref []
    let mutable_extrapath = MutableList.create ()
    let existing_dir d = Sys.file_exists d && Sys.is_directory d
    let add_extra_path p =
      if p.[0] <> '+' && not (existing_dir p)
      then OManager.error "Option --extra-path %S\nNo such file or directory" p
      else MutableList.add mutable_extrapath p

    let add_full_extra_path s = List.iter add_extra_path (extra_split s)

    let str_version =
      Printf.sprintf (
        "Opa compiler (c) MLstate -- version %s -- build %d"
      )
        BuildInfos.opa_version_name
        BuildInfos.opalang_git_version

    let print_version () = prerr_endline str_version

    let rpc_doc =
      "(i,p,c) Different level for RPC optimization (i : server values inserted on client code, p : published functions, c : rpc call)"

    let set_rpc_options str =
      try
        Scanf.sscanf str
          "(%d,%d,%d)"
          (fun i p c -> rpc_options := (i,p,c))
      with Failure _ | End_of_file | Scanf.Scan_failure _ ->
        OManager.error "An error occured while parsing rpc options@\n%s" rpc_doc

    (* ===== *)
    (** Options which refers to (and so depends on, ...) options *)
    let full_help = ref (fun () -> ())

    let command_name = "opa" (* TODO: use buildInfos to know if we are on windows and should add .exe *)

    let synopsis = command_name ^ " [options] source1.opa [source2.opa ...]"

    let help_menu speclist () =
      let head =
	Printf.sprintf "Usage: %s\nwhere options are :\n" synopsis
      in
      Arg.usage speclist head;
      if not BuildInfos.is_release
      then (
        prerr_endline "\nYou can set the following environment variables if needed (VAR MEANING DEFAULT):\n" ;
        List.iter (fun (var, mess, _)-> prerr_endline (Printf.sprintf "  %s \t-- %s" (String.escaped var) mess)) Env.envvar ;
      ) ;
      prerr_endline "\n-----------------------------\n"

    let do_print_help () = !full_help ()
    let opack_file_function =
      ref (fun s -> OManager.error "--opack %s : you cannot use option @{<bright>--opack@} in a opack file !" s)
    (* use a ref because of recursive dependencies
       (the function is updated just after the definition of the options list) *)
      (* ===== *)

    let speclist =
      let standard = (* Please preverse the alphabetical order for lisibility *)
        OManager.Arg.options @
        WarningClass.Arg.options @
        ObjectFiles.Arg.public_options @
        I18n.options @
        OpaSyntax.Args.options @
        BslArgs.options @
        QmlDbGen.Args.options @
        QmlSimpleSlicer.Options.list @
        [
          (* a *)
          "--api",
          Arg.Set generate_interface_and_compile,
          " Generate interfaces (json and text) and continue compilation"
          ;

          "--api-only",
          Arg.Set generate_interface,
          " Generate interfaces (json and text) and exit"
          ;

          ("--back-end", Arg.Symbol (available_back_end_list, back_end),
           (Printf.sprintf "Select a backend between (default is %s)"
              (string_of_available_back_end !back_end_wanted)));

          (* b *)
          "--build-dir",
          Arg.String (fun s -> build_dir := s),
          " set the build directory : default is _build. You must set an absolute path."
          ;

          (* c *)
          "--ccopt",
          Arg.String add_ccopt,
          "<opt>  Pass option <opt> to the C compiler and linker"
          ;

          "--cclib",
          Arg.String add_cclib,
          "<opt>  Pass option <opt> to the C linker"
          ;

          ("--compile-release",   Arg.Set compile_release, " use this option to make a release : errors should be ignored, activate static file embedding.");
          ("--constant-sharing", Arg.Set constant_sharing, " Activate the constant sharing pass");
          ("--constant-sharing-client", Arg.Set constant_sharing, " Activate the constant sharing pass on javascript code");
          ("--no-constant-sharing", Arg.Clear constant_sharing, " Deactivate the constant sharing pass");
          ("--no-constant-sharing-client", Arg.Clear constant_sharing, " Deactivate the constant sharing pass on javascript code");


          ("--conf-opa-files",
           Arg.Unit (fun () -> List.iter add_any_file (ObjectFiles.conf_opa_files ())),
           "Use conf content to determine opa files"
          );


          ("--dump-dbgen-schema", Arg.Set dump_dbgen_schema, " Dump the inferred dbgen schema (to files %.dot and %.png)");
          ("--extra-lib",         Arg.String add_full_extra_lib, "\"*.cm*,*.js,...\" Add lib(s) to link the generated server");
          ("--extra-path",        Arg.String add_full_extra_path, "\"dir,...\" Add path(s) to link the generated server");
          ("-impl",               Arg.String add_any_file,   "<file> Take <file> as a .opa file");
          ("--js-check-bsl-types", Arg.Set js_check_bsl_types, " Enables runtime type checking of the types of bypasses");

          (* m *)

          "--minimal-version",
          Arg.String (fun s ->
                        match BuildInfos.assert_minimal_version s with
                        | None ->
                            OManager.error (
                              "option --minimal-version: @{<bright>%s@} not recognized@\n"^^
                              "@[<2>@{<bright>Hint@}:@\n"^^
                              "try e.g. S3.%%d, v%%d, or %%d@]"
                            )
                              s
                        | Some false ->
                            OManager.error (
                              "@[<2>This application needs a more recent version of Opa@\n"^^
                              "Required version: %s or later@\n"^^
                              "Current version:  %s/%d@]"
                            )
                              s
                              BuildInfos.opa_version_name
                              BuildInfos.opalang_git_version
                        | Some true -> ()
                     ),
          "<version> Ensure that the compiler is newer than the given version"
          ;

          "--mlcopt",
          Arg.String add_mlcopt,
          "<opt> Give option to ocaml compilation"
          ;

          "--mllopt",
          Arg.String add_mllopt,
          "<opt> Give option to ocaml linking"
          ;

          "--modular-plugins",
          Arg.Set modular_plugins,
          " Use plugins as node modules instead of exporting globally. (qmljs)"
          ;
          (* n *)

          "--no-assert",
          Arg.Set no_assert,
          " Ignore @assert directives. (e.g. for a release)"
          ;

          ("--no-server",         Arg.Unit (fun () -> no_server := Some true), " Executable will not start a server service");
          ("--force-server",      Arg.Unit (fun () -> no_server := Some false), " Force the compile to execute passes that are meaningless without server");
          ("--no-stdlib",         Arg.Tuple [Arg.Clear stdlib], " Do not use standard libraries");

          ("-o",                  Arg.String change_target,  "<file> Set output file name to <file>");
          ("--opack",             Arg.String (fun s -> (!opack_file_function) s), "<opack-file> Use an options-packaging file");
          ("--package-version", Arg.String set_package_version, " Version to be included in the package.json file (default 0.1.0, qmljs only)");
          ("--project-root", Arg.String set_project_root, " Specify the root directory of the project");
          ("--publish-src-code", Arg.Set publish_src_code, " Publish application src code at [_internal_/src_code]");
          ("--root-inclusions", Arg.String set_root_inclusions, "<root> Specify the root directory of static inclusions");
          ("--set-mime-database", Arg.String set_mime_database, " Consider given mime database for detecting mimetypes. Used with the directive @static_include_directory");
          ("--show-types",        Arg.Set show_types,        " Show types of declarations (L0 elements)");
          ("--slicer-dump",       Arg.Set slicer_dump       ," Dumps a file containing the side of identifiers and the remote calls they make");
          ("--version",           Arg.Unit (fun () -> print_version (); exit 0), " Print version and exit");
          ("--",     Arg.Tuple [Arg.Unit (fun ()-> run_server_options:= Some []);
                                Arg.Rest (fun s ->
                                  run_server_options:= (Some (s::(Option.default [] (!run_server_options)))))], " run the compiled server with all remaining options");
          (* Shorthand and synonym (sorted alphabetically) *)
          ("-I",                  Arg.String add_full_extra_path, " Shorthand for \"--extra-path {dir,...}\"");
          ("-v",                  Arg.Unit (fun () -> OManager.set_verbose true), " Shorthand for \"--verbose\"");
          ("-x",                  Arg.String add_full_extra_lib, " Shorthand for \"--extra-lib {*.cm*,*.js,...}\"");

          ("-h",                  Arg.Set print_help,        " Shorthand for --help");
          ("-help",               Arg.Set print_help,        " Like --help");
          ("--help",              Arg.Set print_help,        " Print this help");

          ("--static-link", Arg.Set static_link, " Link everything in a single object file");
          ("--no-static-link", Arg.Clear static_link, " Load libraries dynamically (qmljs only)");
        ]
      in
      let non_release = (* Please preserve the alphabetical order *)
        Qml2jsBackendOptions.Arg.options @
        PassHandler.Arg.options @
        Pass_DbSchemaGeneration.Arg.options @
        Flat_Compiler.Arg.options @
        ObjectFiles.Arg.private_options @
        [
          (* Undocumented options *)
          ("--bytecode", Arg.Unit (fun () -> makefile_rule := Qml2ocamlOptions.Bytecode), " Compile ml in bytecode (default is native)");
          ("--bytecode-or-native", Arg.Unit (fun () -> makefile_rule := Qml2ocamlOptions.Bytecode_or_native), " Compile ml in bytecode or in native");
          ("--bytecode-and-native", Arg.Unit (fun () -> makefile_rule := Qml2ocamlOptions.Bytecode_and_native), " Compile ml in bytecode and in native");

          ("--closure", Arg.Set closure, " Activate opa closures");
          ("--no-closure", Arg.Clear closure, "");

          ("--cps", Arg.Set cps, " Activate cps transformation mode");
          ("--cps-client", Arg.Set cps_client, " Activate cps transformation mode on client");
          ("--cps-toplevel-concurrency", Arg.Tuple [ Arg.Set cps ; Arg.Set cps_toplevel_concurrency ],
           " During cps transformation, toplevel non-functional values are compiled as future (enforce --cps)");
          ("--no-cps", Arg.Clear cps, "");
          ("--show-compilation", Arg.Set show_compilation, " show the caml compilation");
          ("--explicit-instantiation",    Arg.Set explicit_instantiation, " Annotate instantiations of polymorphic functions and use the type information at runtime");
          ("--no-explicit-instantiation", Arg.Clear explicit_instantiation, "");
          ("--ei",    Arg.Set explicit_instantiation, " A shorthand for --explicit-instantiation");
          ("--no-ei", Arg.Clear explicit_instantiation, "");
          ("--generate-interface",Arg.Set generate_interface," DEPRECATED (use --api-only instead)");
          ("--generate-interface-and-compile",Arg.Set generate_interface_and_compile," DEPRECATED (use --api instead)");

          "--hacker-mode",
          Arg.Set hacker_mode,
          " Perform some useful tricks when compiling the ocaml code for debugging purpose"
          ;

          "--js-back-end",
          Arg.Symbol (available_js_back_end_list, js_back_end),
          Printf.sprintf " Select a JS backend between %s (default is %s)"
            (String.concat ", " available_js_back_end_list) js_back_end_wanted_name
          ;

          ("--js-as", Arg.spec_of_assoc js_serialize ["adhoc", `adhoc; "ast", `ast], " Compile the client into a json string, instead of the runtime ast directly");
          ("--js-no-cleanup", Arg.Clear js_cleanup, "");
          ("--js-no-local-inlining", Arg.Clear js_local_inlining, "");
          ("--js-no-global-inlining", Arg.Clear js_global_inlining, "");
          ("--js-no-local-renaming", Arg.Clear js_local_renaming, "");

          ("--no-cache-parse",    Arg.Set no_cache_parse,    " UNDOCUMENTED");
          ("--no-discard-of-unused-stdlib", Arg.Set no_discard_of_unused_stdlib, " UNDOCUMENTED");
          ("--no-embedded-stdlib",Arg.Clear embedded_opa, " Disable embedded stdlib usage");
          ("--no-undot",          Arg.Unit (fun () -> undot := false), " Deactivate the optimization on module field calls");

          ("--opa-walker",        Arg.Unit (fun () -> opa_walker := Some true), " UNDOCUMENTED");
          ("--no-opa-walker",     Arg.Unit (fun () -> opa_walker := Some false), " UNDOCUMENTED");
          ("--optimize-rpc",      Arg.String set_rpc_options, "(i,p,c) Different level of RPC optimization (i : server values inserted on client code, p : published functions, c : rpc call)");
          ("--profile",           Arg.Set profile, " UNDOCUMENTED");
          ("--set-opa-walker",    Arg.Symbol (OpaWalker.Options.available_walkers, OpaWalker.Options.set_opa_walker), " UNDOCUMENTED");
          ("--slicer-test",       Arg.Set slicer_test, " Make the slicer output specific information (and exit after it)");
          ("--value-restriction", Arg.spec_of_assoc value_restriction ["disabled", `disabled;
                                                                       "normal", `normal;
                                                                       "strict", `strict],
                                              " Restrict definition of polymorphic values");
        ] in
        Arg.sort (
          Arg.align (
            Arg.add_bash_completion
	      ~name:command_name
              ~default:(Arg.File "@(opa|cm@(o|a|x|xa|xs)|js|bypass|opack)")
              (standard @ (if BuildInfos.is_release then [] else non_release))
          )
        )

  let parse () =
    let anon_fun arg =
        let ext = File.extension arg in
        match ext with
        | "opa" -> add_opa_file arg
        | "cmx" | "cmxa" | "cmo" | "cma" -> add_full_extra_lib arg
        | "cmxs" ->
            let plugin = BslDynlink.SharedObject arg in
            BslDynlink.loadfile_private plugin
        | "opp" -> plugin_inclusion arg
        | "opack" -> !opack_file_function arg
        | "conf" -> ObjectFiles.load_conf arg
        | "opx" -> ObjectFiles.Arg.add_link_package (File.chop_extension arg)
        | "js" -> MutableList.add client_plugin_files arg
        | "nodejs" -> MutableList.add server_plugin_files arg
        | _ -> OManager.error "I don't know what to do with anonymous argument %S" arg in

      (** feature : opack files (used e.g. in spec.git) *)
      (** this feature is a macro pass for options, so it requires to be done here not in any pass (too late) *)
      let opack_file_rule file =
        let _ =
          if not (File.is_regular file)
          then OManager.error "cannot find opack file @{<bright>%S@}" file in
        (** beware, in case of any non option arg (a file) we add the prefix before it (the files are pointing from the opack file) *)
        let prefix = Filename.dirname file in
        let env s =
          if not (File.is_relative s) then s else
            File.simplify_path (Filename.concat prefix s)
        in
        let preprocess_word acc word =
          (** assert : called only with at leat 1 char length string without blank *)
          (** solving the env *)
          let find s =
            try Sys.getenv s with Not_found -> s in
          let word =
            try
              let b = Buffer.create 256 in
              let _ = Buffer.add_substitute b find word in
              Buffer.contents b
            with
            | Not_found -> word in

          match word.[0] with
          | '-' -> word :: acc (** this is an option : to be passed 'as is' *)
          | _ ->
              ( if List.mem (File.extension word) [ "opa" ; "conf" ; "opack" ]
                then ( env word )
                else word ) :: acc
                (** this is an opa file : we must prefix it
                    because in opack file, path to opa files are relative to the opack-file.*)
                (** for js files, we use the extra-path feature *)
        in
        let preprocess acc line =
          let line = String.trim line in
          let len = String.length line in
          if len = 0 then acc
          else
            match line.[0] with
            | '#' -> acc
            | _ ->
                (** split, and call preprocess_word *)
                let split = String.slice_chars " \t\n" in
                List.fold_left preprocess_word acc (split line)
        in
        let opack_options = File.lines_fold preprocess [] file in
        let opack_options = Sys.argv.(0) :: (List.rev opack_options) in
        let opack_options = Array.of_list opack_options in
        try
          Arg.parse_argv ~current:(ref 0) opack_options speclist anon_fun ("")
        with
        | Arg.Bad message ->
            OManager.error "error while reading opack file @{<bright>%S@} :@\n%s@" file message
        | Arg.Help _ ->
            help_menu speclist () ;
            OManager.error "error, the opack file @{<bright>%S@} contains the option --help" file
      in
      (** updating options depending on options *)
      let _ =
        opack_file_function := opack_file_rule ;
        full_help := help_menu speclist
      in
      (** Default opack file *)
      let default_opack = File.concat (Lazy.force File.mlstate_dir) "default.opack" in
      let _ = if File.is_regular default_opack then opack_file_rule default_opack in
      (** Main Command line *)
      Arg.parse speclist anon_fun "";
      (** Print_help **)
      if !print_help then begin
        do_print_help ();
        exit 0
      end;
      (** extra settings at the end *)
      extralibs := MutableList.to_list mutable_extralibs;
      extrajs :=
        (let module B = (val !js_back_end_wanted : Qml2jsOptions.JsBackend) in
         B.runtime_libs ~cps:!cps_client);
      extrapath := MutableList.to_list mutable_extrapath;
      bypass_plugin := MutableList.to_list mutable_bypass_plugin;
      filenames := MutableList.to_list mutable_filenames;
      target := (
        let ext = match !back_end_wanted with
          | `qmljs -> ".js"
          | `qmlflat -> ".exe"
        in
        Option.default (!last_target_from_file ^ ext) !target_opt);
      target_only_qml := Option.default (!last_target_from_file ^ ".qml") !target_opt;
      target_qmli := Option.default (!last_target_from_file ^ ".qmli") !target_opt;
      target_dbgen_schema := Option.default (!last_target_from_file ^ ".dot") !target_opt

  end

  (* Parse and get options, work with a side effect on module ArgParser *)
  let parse_options () =
    ArgParser.parse ();
    begin
      OpaWalker.Options.disp := match !ArgParser.opa_walker with
      | Some true -> OpaWalker.Options.True
      | Some false -> OpaWalker.Options.ForceExit
      | None ->
          if Unix.isatty (Unix.descr_of_out_channel OpaWalker.Options.och) then
            OpaWalker.Options.True
          else
            OpaWalker.Options.ForceExit
    end


  let check_options opt =
    (* 1) add other check if you need *)
    (*
      let _ =
        your check
      in
    *)
    opt

  (* Should not be called somewhere else than in pass_ArgParse *)
  let get_options () = check_options {
    build_dir = !ArgParser.build_dir ;
    run_server_options = Option.map List.rev (!ArgParser.run_server_options) ;
    ocamlc = Lazy.force !ArgParser.Env.ocamlc ;
    ocamlopt = Lazy.force !ArgParser.Env.ocamlopt ;

    cclib = MutableList.to_list ArgParser.cclib ;
    ccopt = MutableList.to_list ArgParser.ccopt ;

    mlcopt = MutableList.to_list ArgParser.mlcopt ;
    mllopt = MutableList.to_list ArgParser.mllopt ;

    back_end = !ArgParser.back_end_wanted ;
    js_back_end = !ArgParser.js_back_end_wanted ;
    hacker_mode = !ArgParser.hacker_mode ;
    makefile_rule = !ArgParser.makefile_rule ;
    filenames = !ArgParser.filenames;
    client_plugin_files = MutableList.to_list ArgParser.client_plugin_files;
    server_plugin_files = MutableList.to_list ArgParser.server_plugin_files;
    show_types = !ArgParser.show_types ;

    (* n *)

    no_assert = !ArgParser.no_assert ;
    no_server =
      (match !ArgParser.no_server with
       | None when (!OpaSyntax.Args.r).OpaSyntax.Args.parser = OpaSyntax.Js -> Some false
       | x -> x)
    ;

    stdlib = !ArgParser.stdlib ;
    embedded_opa = !ArgParser.embedded_opa ;
    show_compilation = !ArgParser.show_compilation ;
    no_cache_parse = !ArgParser.no_cache_parse ;
    no_discard_of_unused_stdlib = !ArgParser.no_discard_of_unused_stdlib ;
    cps = !ArgParser.cps ;
    cps_client = !ArgParser.cps_client ;
    cps_toplevel_concurrency = !ArgParser.cps_toplevel_concurrency ;
    closure = !ArgParser.closure ;
    extralibs = !ArgParser.extralibs ;
    extrajs = !ArgParser.extrajs ;
    extrapath = !ArgParser.extrapath ;
    resname = File.chop_extension !ArgParser.target ;
    target = !ArgParser.target ;
    explicit_instantiation = !ArgParser.explicit_instantiation ;
    value_restriction = !ArgParser.value_restriction ;

    dump_dbgen_schema = if !ArgParser.dump_dbgen_schema then Some !ArgParser.target_dbgen_schema else None ;
    bypass_plugin = !ArgParser.bypass_plugin ;
    compile_release = !ArgParser.compile_release;
    slicer_test = !ArgParser.slicer_test;
    slicer_dump = !ArgParser.slicer_dump;
    rpc_options = !ArgParser.rpc_options;
    profile = !ArgParser.profile;
    mime_database = !ArgParser.mime_database;
    project_root = !ArgParser.project_root;
    root_inclusions = !ArgParser.root_inclusions;
    undot = !ArgParser.undot;
    js_serialize = !ArgParser.js_serialize;
    generate_interface = !ArgParser.generate_interface;
    generate_interface_and_compile = !ArgParser.generate_interface_and_compile;

    constant_sharing = !ArgParser.constant_sharing;
    constant_sharing_client = !ArgParser.constant_sharing_client;

    js_check_bsl_types = !ArgParser.js_check_bsl_types;
    js_cleanup = !ArgParser.js_cleanup;
    js_local_inlining = !ArgParser.js_local_inlining;
    js_global_inlining = !ArgParser.js_global_inlining;
    js_local_renaming = !ArgParser.js_local_renaming;
    publish_src_code = !ArgParser.publish_src_code;

    i18n = !I18n.r;
    parser_ = !OpaSyntax.Args.r;

    static_link = !ArgParser.static_link;
    package_version = !ArgParser.package_version;
    modular_plugins = !ArgParser.modular_plugins;

  }

  let echo_help () = ArgParser.do_print_help ()

  (** Fill a pprocess environment from opa options. *)
  let to_ppenv options env =
    let env =
      let (i, _, _) = options.rpc_options in
      Pprocess.add_env "OPA_OPTIMIZE_RPC_I" (string_of_int i) env
    in let env =
      if options.closure then Pprocess.add_env "OPA_CLOSURE" "" env
      else env
    in let env =
      if options.cps then Pprocess.add_env "OPA_CPS" "" env
      else env
    in let env =
      if options.cps_client then Pprocess.add_env "OPA_CPS_CLIENT" "" env
      else env
    in let env =
      Pprocess.add_env "OPA_BADOP" "1" env
    in let env =
      let module JsCC = (val options.js_back_end : Qml2jsOptions.JsBackend) in
      Pprocess.add_env "OPA_JS_COMPILER" JsCC.name env
    in let env =
      if options.back_end = `qmljs then
        let env = Pprocess.add_env "OPA_BACKEND_QMLJS" "1" env in
        let env = Pprocess.add_env "OPA_CHANNEL" "1" env in
        Pprocess.add_env "OPA_FULL_DISPATCHER" "1" env
      else env
    in env

  let write_manpage file =
    Arg.write_simple_manpage
      ~cmdname:ArgParser.command_name
      ~summary:"The Opa compiler"
      ~section:1
      ~centerheader:"Opa Manual"
      ~synopsis:ArgParser.synopsis
      ~description:"The Opa compiler allows you to compile Opa projects into executable files. Please refer to the online manual on http://doc.opalang.org for a detailed description of the language and its tools.\n"
      ~options:ArgParser.speclist
      ~other:[("VERSION", ArgParser.str_version)]
      file

end
