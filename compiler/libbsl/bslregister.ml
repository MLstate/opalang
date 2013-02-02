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
   This application is the register generator, used as a preprocessor
   on all files including to build the Bypass Standard Library.

   @author Mathieu Barbin
   @author Mehdi Bouaziz
*)

(* TODO: refactoring of libbase *)
(* open Base is BAD, hoisting modules *)
module Arg            = Base.Arg
module Format         = Base.Format
module String         = Base.String

(* shorthand *)
module BI = BslInterface
module BR = BslRegisterLib
module BG = BslGeneration

let (|>) x f = f x

(* Options for BslGenerator *)
let static = ref false
let no_opp = ref false
let modular_plugins = ref false
let build_dir = ref ""
let bsl_pref = ref "default"
let auto_build = ref true
let check_style = ref false
let clean = ref false
let clean_would_only = ref false
let default_iformats = ref true
let extrapaths = MutableList.create ()
let unsafe_js = ref false
let unsafe_opa = ref false
let bypass_plugins = MutableList.create ()
let files = MutableList.create ()
let package_version = ref "0.1.0"
let spec_process = Hashtbl.create 5
let ml_flags = MutableList.create ()
let mlopt_flags = MutableList.create ()
let js_validator = ref (Some "js")
let js_validator_files = MutableList.create ()
let js_validator_options = MutableList.create ()
let pprocess = ref None
let js_bypass_syntax : [`classic | `jsdoc] ref = ref `jsdoc

let cwd = Sys.getcwd ()
let is_default_lib = ref true

let customize_lib_name name =
  let name =
    String.remove_suffix_if_possible ("."^BslConvention.Extension.plugin) name
  in
  if
    not (String.is_universal_ident name)
    || name.[0] = '_' (* an universal ident is not empty *)
  then
    OManager.error (
      "@{<bright>%S@} is not a valid plugin name@\n"^^
      "Plugin name should be alphanumeric and start with a letter"
    )
      name
  ;
  let name = BslConvention.plugin_name name in

  is_default_lib       := false ;
  bsl_pref             := name ;
  ()

let spliter g =
  List.map String.trim
    (String.slice_chars "{} ,;" g)

(* b *)

let bypass_plugins_add_file files =
  List.iter (
    fun file ->
      if String.is_suffix ("." ^ BslConvention.Extension.bypass) file
      then MutableList.add bypass_plugins (BslDynlink.MarshalPlugin file)
      else MutableList.add bypass_plugins (BslDynlink.SharedObject file)
  )
    (spliter files)

(* e *)

let extrapaths_add d =
  let existing_dir d = Sys.file_exists d && Sys.is_directory d in
  let iter d =
    if d.[0] <> '+' && not (existing_dir d)
    then OManager.error "Option -I %S\nNo such file or directory" d
    else (
      let d =
        if File.is_relative_include_path d
        then
          Filename.concat cwd d
        else d
      in
      MutableList.add extrapaths d
    )
  in
  List.iter iter (Arg.split d)

(* j *)

let js_files = MutableList.create ()
let nodejs_files = MutableList.create ()

let js_validator_files_set = ref StringSet.empty
let js_validator_add_file s =
  List.iter (fun f ->
               if not (File.is_regular f)
               then OManager.error "cannot find file %S (js-validation)" f
               else MutableList.add js_validator_files f)
    (spliter s)

let js_validator_add_option o =
  MutableList.add js_validator_options o

let available_js_bypass_syntax_list = ["classic"; "jsdoc"; "new"]
let js_bypass_syntax_of_string = function
  | "classic" -> Some `classic
  | "jsdoc"
  | "new" -> Some `jsdoc
  | _ -> None
let set_js_bypass_syntax s =
  js_bypass_syntax := Option.get (js_bypass_syntax_of_string s)

(* m *)


(* p *)


let set_package_version version =
  package_version := version

let plugin_inclusion file =
  let inclusion = BslConvention.inclusion ~cwd file in
  MutableList.add extrapaths inclusion.BslConvention.extrapath ;
  bypass_plugins_add_file inclusion.BslConvention.plugin ;
  ()

(* s *)

(* u *)

(* following guidelines for command line tools *)

let (!>) = Format.sprintf

let spec = [

  (* b *)


  "--build-dir",
  Arg.Set_string build_dir,
  !>
    " Change the build directory. Default is $PWD" ;


  (* c *)


  "--check-style",
  Arg.Set check_style,
  !>
    " Make some more check about some guidelines used in the files" ;


  "--clean",
  Arg.Symbol (["-n" ; "-f"], (
    function
    | "-f" ->
        clean := true
    | _ ->
        clean := true;
        clean_would_only := true
  )),
  !>
    " With -n, it only says the files which would be cleaned, with '-f' the files are removed" ;


  (* i *)


  "-I",
  Arg.String extrapaths_add,
  !>
    " Add path to external librairies for the compilation" ;

  (* j *)

  "--js-bypass-syntax",
  Arg.Symbol (available_js_bypass_syntax_list, set_js_bypass_syntax),
  !>
    " Choose a bsl directive syntax for JS files (default: \"classic\")" ;

  "--js-validator",
  Arg.String (fun s -> js_validator := Some s),
  !>
    " Specify a js-validator (default is %a)" (Option.pp Format.pp_print_string) !js_validator ;


  "--js-validator-file",
  Arg.String js_validator_add_file,
  !>
    "<file> Add an js init file for the js-validation only" ;


  "--js-validator-off",
  Arg.Unit (fun () -> js_validator := None),
  !>
    " Disable the js validation (sad)" ;


  "--js-validator-opt",
  Arg.String js_validator_add_option,
  !>
    "<opt> Add an shell option for the the js-validator" ;


  (* l *)


  (* m *)


  "--ml",
  Arg.String (fun s -> List.iter (MutableList.add ml_flags) (Arg.split s)),
  !>
    "<flags> Add options for ocaml compilation (both byte and native)" ;


  "--mlopt",
  Arg.String (fun s -> List.iter (MutableList.add mlopt_flags) (Arg.split s)),
  !>
    "<flags> Add options for ocaml native compilation" ;

  "--modular-plugins",
  Arg.Set modular_plugins,
  " Export module identifiers following common js conventions instead of globally" ;

  (* n *)

  "--no-build",
  Arg.Clear auto_build,
  !>
    " Do not build the plugin, just generate opp-files" ;


  "--no-default-iformats",
  Arg.Clear default_iformats,
  !>
    " Do not load default format for ##include" ;


  "--no-opp",
  Arg.Set no_opp,
  !>
    " Produces files in the build_dir directly, do not produce any opp directoy" ;

  (* o *)

  "-o",
  Arg.String customize_lib_name,
  !>
    "<name> Specify the name of the plugin, default is %s"
    !bsl_pref ;

  (* p *)

  "--package-version",
  Arg.String set_package_version,
  !>
    "version to be added to the package.json file (default 0.1.0)" ;

  "--plugin",
  Arg.String plugin_inclusion,
  !>
    "<opp> Take the following argument as an opa plugin (opp)" ;

  "--pp",
  Arg.String (fun s -> pprocess := Some s),
  !>
    "<command> Pipe sources through preprocessor <command>";

  "--pp-file",
  Arg.String (fun s ->
                match BaseString.split_char ':' s with
                | (_, "") -> raise (Arg.Help "--pp-file")
                | (file, pprocess) -> Hashtbl.add spec_process file pprocess
             ),
  !>
    "<file>:<command> Pipe file through preprocessor <command>";

  (* u *)

  "--unsafe-js",
  Arg.Set unsafe_js,
  !>
    " Activate unsafe-js mode (ignore js errors)" ;

  "--unsafe-opa",
  Arg.Set unsafe_opa,
  !>
    " Activate unsafe-opa mode (ignore opa errors)" ;

  (* s *)

  "--static",
  Arg.Set static,
  !>
    "produces files for static linking with opa.exe (not for standard distrib)" ;

]

let anon_fun file =
  match File.extension file with

  | opp when opp = BslConvention.Extension.plugin ->
      plugin_inclusion file

  | ("js" | "nodejs") as ext ->
      (*
        The js files are indexed by their basename.
      *)
      let key = file in
      if StringSet.mem key (!js_validator_files_set)
      then
        OManager.error (
          "Found several js files with the same basename : @{<bright>%s@}@\n"^^
            "@[<2>{@<bright>Hint@}:@\n"^^
            "Perhaps the same file is passed several time in the command line@\n"^^
            "or maybe you could rename one of the clashing javascript files@]@\n"
        )
          file
      ;
      js_validator_files_set := StringSet.add key (!js_validator_files_set);
      (if ext = "js" then MutableList.add js_files file
       else MutableList.add nodejs_files file);
      MutableList.add files file
  | _ -> MutableList.add files file


let usage_msg =
  !> "@{<bright>%s@}: Opa External Libraries Register\nUsage: %s [options] files\n"
    (Filename.basename Sys.argv.(0)) (Filename.basename Sys.argv.(0))

let parse () =
  let spec = (
    WarningClass.Arg.options @
    (OManager.Arg.version "bslregister" :: OManager.Arg.options) @
    BslLib.Arg.options @
    spec
  )

  |> Arg.add_bash_completion
  |> Arg.sort
  |> Arg.align

  in
  Arg.parse spec anon_fun (usage_msg^"Options:");
  {
    BG.
    static = !static;
    no_opp = !no_opp;
    modular_plugins = !modular_plugins;
    build_dir = !build_dir;
    bsl_pref = !bsl_pref;
    auto_build = !auto_build;
    check_style = !check_style;
    clean = !clean;
    clean_would_only = !clean_would_only;
    default_iformats = !default_iformats;
    extrapaths = MutableList.to_list extrapaths;
    unsafe_js = !unsafe_js;
    unsafe_opa = !unsafe_opa;
    bypass_plugins = MutableList.to_list bypass_plugins;
    files = MutableList.to_list files;
    package_version = !package_version;
    spec_process =
      Hashtbl.fold StringMap.add spec_process StringMap.empty;
    ml_flags = MutableList.to_list ml_flags;
    mlopt_flags = MutableList.to_list mlopt_flags;
    js_validator = !js_validator;
    js_validator_files = MutableList.to_list js_validator_files;
    js_validator_options = MutableList.to_list js_validator_options;
    pprocess = !pprocess;
    js_classic_bypass_syntax = !js_bypass_syntax = `classic;
  }

(* Checking options *)
let check_options options =
  (* if needed, add some checks *)
  ignore options ;

  (* default lib name *)
  if !is_default_lib then
    OManager.warning ~wclass:WarningClass.bsl_register (
      "@[<2>You did not precise a name for your lib.@\n"^^
      "By default the lib name will be @{<bright>%S@}.@]@\n"^^
      "@[<2>@{<bright>Hint@}:@\n"^^
      "use option @{<bright>-o@} <libname>@]" )
      !bsl_pref ;
  ()

(* === *)

(* Main *)
let _ =
  try
    WarningClass.load_set BR.warning_set;
    BR.set_signal_sigint ();
    let options = parse () in
    check_options options;
    OManager.this_is_tool "bslregister";
    if MutableList.length files = 0 then (
      OManager.unquiet "no input files";
      exit 0
    );
    BG.process options
  with
  | BR.SigInt ->
    OManager.error
      "building process @{<bright>not accomplished@} due to an @{<bright>user interruption@}"
  | e ->
    let backtrace = Printexc.get_backtrace () in
    OManager.apologies ();
    OManager.printf "@[<2>@{<bright>Hint@}:@\n%s@]@\n@{<bright>Backtrace@}:@\n%s@\n"
      (Printexc.to_string e) backtrace;
    exit 2
