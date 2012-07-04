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

(*
  <!> The layout of this file can seems weird,
  it is a prototype for optimizing maintenace as well as git conflicts
  and commit diffing. WIP, RFC
*)

(* TODO: refactoring of libbase *)
(* open Base is BAD, hoisting modules *)
module Arg            = Base.Arg
module Format         = Base.Format
module String         = Base.String

(* shorthand *)
module BI = BslInterface
module BR = BslRegisterLib

let (|>) x f = f x

let opp_extension = BslConvention.Extension.plugin
let cwd = Sys.getcwd ()
let build_dir = ref ""
let opp_dir = ref ""
let is_default_lib     = ref true
let bsl_pref           = ref "default"

(**
  A boolean to say if we want to generate the Plugin and the Loader file.
  these files are used to link statically a plugin with an executable,
  so that there is no need to load dynamically a .bypass file for beeing
  able to load bypasses of a given plugin.

  Currently, the static mode is used only for the opabsl, and the resulting
  plugin is linked with opa.exe

  The loader file can be used for interpreter in ml only (opatop)
  to add new primitives.
*)
let static = ref false

(**
  A boolean to say that generated files should be put in the build_dir directly,
  without storing them in a opp directory.
*)
let no_opp             = ref false

(**
   These are the inclusion to give for compiling plugins.
   This text will be produced in the Makefile, which will interprate shell expension like $(MY_VAR)
*)
let static_extrapaths () =
  Printf.sprintf "-I $(%s)/%s" InstallDir.name InstallDir.lib_opa

(**
  outputs in build_dir/libname.opp/libnameSuffix.ext
  path to file may be absolute, in this case, produces
  all the absolute path inside the build dir
*)
let finalize_opp_dir () =
  opp_dir := (
    if !no_opp then !build_dir
    else
      let opp = !bsl_pref ^ "." ^ opp_extension in
      Filename.concat !build_dir opp
  )


let output_prefix ?(mkdir=true) prefix filename =
  (*
    info about the file
  *)
  let dirname  =
    let d = Filename.dirname filename in
    if d = "." then "" else d
  in
  let basename = Filename.basename filename in
  let filename = prefix ^ basename in

  (*
    output directory
  *)
  let directory = !opp_dir in
  let directory = Filename.concat directory dirname in

  (*
    eventually, check or create the directory
  *)
  if mkdir then (
    if not (File.check_create_path directory) then
      OManager.error "cannot create directory %s" directory
  );

  let filename = Filename.concat directory filename in
  filename


module Name =
struct
  let map f () =
    output_prefix !bsl_pref f

  let jskeys           = map (BslConvention.Suffix.jskeys ^ ".js")
  let loader           = map (BslConvention.Suffix.loader ^ ".ml")
  let marshalplugin    = map (BslConvention.Suffix.marshalplugin ^ "." ^ BslConvention.Extension.bypass)
  let mlruntime        = map (BslConvention.Suffix.mlruntime ^ ".ml")
  let mlruntime_mli    = map (BslConvention.Suffix.mlruntime ^ ".mli")
  let plugin           = map (BslConvention.Suffix.plugin ^ ".ml")
end


(*
  This function is used for itering opa and js generated files
*)
let prefix filename =
  output_prefix (!bsl_pref ^ "_") filename


let js_code = prefix
let opa_code = prefix
let opa_interface f = (prefix f)^"i"


let jskeys             = ref ""
let loader             = ref ""
let marshalplugin      = ref ""
let mlruntime          = ref ""
let mlruntime_mli      = ref ""
let plugin             = ref ""

let makefile           = ref "Makefile"
let stamp              = ref "stamp"
let stamp_date         = ref ""

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


let finalize_options () =
  finalize_opp_dir () ;

  jskeys               := Name.jskeys () ;
  loader               := Name.loader () ;
  marshalplugin        := Name.marshalplugin () ;
  mlruntime            := Name.mlruntime () ;
  mlruntime_mli        := Name.mlruntime_mli () ;
  plugin               := Name.plugin () ;

  makefile := output_prefix "" !makefile ;
  stamp := output_prefix "" !stamp ;

  ()


let spliter g =
  List.map String.trim
    (String.slice_chars "{} ,;" g)


let itersplit f sss = List.iter f (spliter sss)


let rename_factory src ml filename =
  let s = if String.is_suffix ("."^ml) filename then filename else
    (File.chop_extension filename)^"."^ml in
  src := s


let rename_pair (ml, mli) src head filename =
  let s =
    if String.is_suffix ("."^ml) filename
    then Filename.chop_extension filename
    else filename in
  src := s^"."^ml ;
  head := s^"."^mli ;
  ()


(* a *)


let auto_build = ref true


(* b *)


let bypass_plugins = MutableList.create ()
let bypass_plugins_add_file files =
  List.iter (
    fun file ->
      if String.is_suffix ("." ^ BslConvention.Extension.bypass) file
      then MutableList.add bypass_plugins (BslDynlink.MarshalPlugin file)
      else MutableList.add bypass_plugins (BslDynlink.SharedObject file)
  )
    (spliter files)


(* c *)


let check_style = ref false


let clean = ref false
let clean_would_only = ref false


(* d *)


let default_iformats = ref true


(* e *)


let extrapaths = MutableList.create ()
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


(* f *)


let files = MutableList.create ()
let files_generated = ref 0


(* j *)


let js_files = MutableList.create ()
let nodejs_files = MutableList.create ()

let js_validator       = ref (Some "js")
let js_validator_files = MutableList.create ()
let js_validator_files_set = ref StringSet.empty
let js_validator_add_file s =
  List.iter (fun f ->
               if not (File.is_regular f)
               then OManager.error "cannot find file %S (js-validation)" f
               else MutableList.add js_validator_files f)
    (spliter s)

let js_validator_options = MutableList.create ()
let js_validator_add_option o =
  MutableList.add js_validator_options o


(* m *)


let ml_flags = MutableList.create ()
let mlopt_flags = MutableList.create ()

(* p *)

let pprocess = ref None
let spec_process = Hashtbl.create 5


(* r *)


let rename_mlruntime   = rename_pair ( "ml"  ,  "mli" ) mlruntime  mlruntime_mli


(* u *)


let unsafe_js = ref false
let unsafe_opa = ref false


let plugin_inclusion file =
  let inclusion = BslConvention.inclusion ~cwd file in
  MutableList.add extrapaths inclusion.BslConvention.extrapath ;
  bypass_plugins_add_file inclusion.BslConvention.plugin ;
  ()


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
    Sys.argv.(0) Sys.argv.(0)


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
  Arg.parse spec anon_fun (usage_msg^"Options:") ;
  finalize_options ()


(* ======================================================================= *)
(** {6 Makefile Generation} *)
(* ======================================================================= *)
module Makefile :
sig
  (**
     Return a buffer containing the generated Makefile.
  *)
  val generate : unit -> Buffer.t
end =
struct
  (**
     For lisibility of this generation there, we use the add_substitute function
     of the [Buffer] module. This means that we use a list of binding for inserting
     dynamic parts into this generated makefile.
     As we generating a Makefile, we need to generate the $ character, in this case,
     it is echaped '\$'.
  *)

  let bindings () =
    let extrapaths =
      static_extrapaths () ^ (
        String.concat_map ~left:" " " " (Printf.sprintf "-I %s")
          (MutableList.to_list extrapaths)
      ) in
    let suffix = String.concat " " (
      BslConvention.Suffix.mlruntime ::
        if !static then [
          BslConvention.Suffix.loader ;
          BslConvention.Suffix.plugin ;
        ] else []
    )
    in
    let date =
      let d = DebugTracer.now () in
      stamp_date := d ;
      d
    in

    [

      (* c *)

      "command", String.concat " " (Array.to_list Sys.argv) ;

      (* d *)

      "date", date ;

      (* g *)

      "generator", Sys.argv.(0) ;

      (* i *)

      "include", extrapaths ;

      (* m *)

      "ml_flags", String.concat " " (MutableList.to_list ml_flags) ;
      "mlopt_flags", String.concat " " (MutableList.to_list mlopt_flags) ;

      (* p *)

      "plugin", !bsl_pref ;

      (* s *)

      "suffix", suffix ;

      (* v *)

      "version", BuildInfos.version_id ;
    ]

  let makefile_pattern =
"# ============================== #
# ==== BSL-CUSTOMLIB-MAKER ===== #
# ========== MLstate =========== #

# Generated Makefile by $(generator) version $(version) : $(date)
# from command : $(command)

OPP=$(plugin)

SUFFIX=$(suffix)
INCLUDE=$(include)

OCAML_FLAGS=$(ml_flags)
OCAMLOPT_FLAGS=$(mlopt_flags)
"

  let static_part = "
TARGETS_CMI=$(patsubst %, $(OPP)%.cmi, $(SUFFIX))
TARGETS_CMX=$(patsubst %, $(OPP)%.cmx, $(SUFFIX))
TARGETS_CMXA=$(patsubst %, $(OPP)%.cmxa, $(SUFFIX))

all: $(TARGETS_CMI) $(TARGETS_CMX) $(TARGETS_CMXA)

OCAMLOPT ?= ocamlopt.opt
TRX ?= $(MLSTATELIBS)/bin/trx

%.ml : %.trx
\t$(TRX) $^ > $@

%.cmx : %.ml %.cmi
\t$(OCAMLOPT) $(OCAML_FLAGS) $(OCAMLOPT_FLAGS) $(INCLUDE) -c $<

%.cmxa : %.cmx
\t$(OCAMLOPT) $(OCAML_FLAGS) $(OCAMLOPT_FLAGS) $(INCLUDE) -a $< -o $@

%.cmi : %.mli
\t$(OCAMLOPT) $(OCAML_FLAGS) $(OCAMLOPT_FLAGS) $(INCLUDE) -c $<

%.cmi : %.ml
\t$(OCAMLOPT) $(OCAML_FLAGS) $(OCAMLOPT_FLAGS) $(INCLUDE) -c $<

clean :
\trm -f *.cmx *.cmo *.o *.cmi

wclean :
\t@echo \"Would remove *.cmx *.cmo *.o *.cmi\"
"

  let generate () =
    let bindings = bindings () in
    let map = StringMap.from_list bindings in
    let subst var =
      try StringMap.find var map with
      | Not_found ->
          OManager.apologies ();
          OManager.printf (
            "@[<2>@{<bright>Hint@}:@\nvar %S is not found during Makefile generation.@]@\n"
          )
            var
          ;
          assert false
    in
    let buf = Buffer.create 1024 in
    let () =
      try
        Buffer.add_substitute buf subst makefile_pattern
      with
      | Not_found ->
          OManager.apologies ();
          OManager.printf (
            "@[<2>@{<bright>Hint@}:@\nthe closing character of a parenthesized variable@\n"^^
            "cannot be found during Makefile generation.@]@\n"
          ) ;
          assert false
    in
    Buffer.add_string buf static_part ;
    buf

end
(* ======================================================================= *)


let iter_generated_files fct =

  MutableList.iter (
    fun f ->
      match File.extension f with
      | "opa" ->
          fct (opa_code f) ;
          fct (opa_interface f)  ;
          ()
      | "js" ->
          fct (js_code f) ;
          ()
      | "nodejs" ->
          fct (js_code f) ;
          ()
      | _ -> ()
  ) files ;

  fct !jskeys ;
  fct !marshalplugin ;
  fct !mlruntime ;
  fct !mlruntime_mli ;

  if !static then (
    fct !loader ;
    fct !plugin ;
  ) ;

  fct !makefile ;
  fct !stamp ;

  ()


let check_safety_overwrite () =
  let fct n =
    if MutableList.mem n files then (
      OManager.error (
        "@[<2><!> bslregister refuse to do that !@\n"^^
        "The file @{<bright>%S@} would be @{<bright>overwritten@} during the process@]"
      ) n
    ) in
  iter_generated_files fct


let remove_file f =
  try Unix.unlink f with Unix.Unix_error (e,_,_) ->
    OManager.verbose "@[<2><!> Cannot clean @{<bright>%s@}:@\n%s@]" f (Unix.error_message e)


(* preprocessing format, for ##include *)
let may_add_format () =
  if !default_iformats then (
    BslLib.HLParser.add_iformat BslLib.HLParser.default_opa_iformats ;
    ()
  )


(* build BR.options from the current state of parameters *)
let bslregister_options ()=

  let basename = !bsl_pref in

  let bypass_plugins        = MutableList.to_list bypass_plugins in

  let check_style           = !check_style in

  let js_files = MutableList.to_list js_files in

  let nodejs_files = MutableList.to_list nodejs_files in

  let js_validator =
    Option.map (
      fun js -> (js, MutableList.to_list js_validator_files), MutableList.to_list js_validator_options
    ) (!js_validator)
  in

  let ml_plugin_filename   = !plugin in
  let ml_runtime_filename  = !mlruntime in

  let unsafe_js            = !unsafe_js in
  let unsafe_opa           = !unsafe_opa in


  let options = {
    BI.

    basename ;
    bypass_plugins ;

    check_style ;

    js_files ;
    nodejs_files ;
    js_validator ;

    ml_plugin_filename ;
    ml_runtime_filename ;

    unsafe_js ;
    unsafe_opa ;

  } in
  options


(* clean, or just say what would be cleaned *)
let may_clean () =
  if !clean then (
    let rm f =
      if !clean_would_only
      then OManager.printf "Would remove @{<bright>%s@}@\n" f
      else (
        OManager.unquiet "rm -f %s" f ;
        remove_file f ;
        ()
      )
    in iter_generated_files rm;
    exit 0
  )

(* open_out_bin : read ocaml doc,
   Marshal should be used with binary channel
   for a win OS compatilibity *)
let handle_open_out file =
  try open_out_bin file
  with
  | Sys_error s ->
      OManager.error "@[<2>@{<bright>bslregister@}: cannot open_out @{<bright>%s@}:@\n%s@]" file s


let handle_close_out file oc =
  try close_out oc
  with
  | Sys_error s ->
      OManager.error "@[<2>@{<bright>bslregister@}: cannot close_out @{<bright>%s@}:@\n%s@]" file s


let output filename pp a =
  OManager.verbose "writing file @{<bright>%S@}..." filename ;
  let oc = handle_open_out filename in
  pp oc a ;
  handle_close_out filename oc ;
  incr(files_generated) ;
  ()


let make_iterator rename =
  let output filename =
    let filename = rename filename in
    output filename
  in
  { BR.output = output }


(* after finalization of register session, actually produce files *)
let files_generation ( finalized_t : BR.finalized_t ) =
  let iterator_js_code        = make_iterator js_code         in
  let iterator_opa_code       = make_iterator opa_code        in
  let iterator_opa_interface  = make_iterator opa_interface   in

  BR.out_js_code              iterator_js_code                finalized_t ;
  BR.out_nodejs_code          iterator_js_code                finalized_t ;
  BR.out_opa_code             iterator_opa_code               finalized_t ;
  BR.out_opa_interface        iterator_opa_interface          finalized_t ;

  output !jskeys              BR.out_js_keys                  finalized_t ;

  output !marshalplugin       BR.out_ml_marshal_plugin        finalized_t ;

  if !static then (
    output !loader              BR.out_ml_loader                finalized_t ;
    output !plugin              BR.out_ml_plugin                finalized_t ;
  ) ;

  if BR.need_makefile finalized_t then (
    output !mlruntime           BR.out_ml_runtime               finalized_t ;
    output !mlruntime_mli       BR.out_ml_runtime_mli           finalized_t ;
    output !makefile            Buffer.output_buffer            (Makefile.generate ()) ;
  );

  output !stamp               Pervasives.output_string        !stamp_date ;

  ()


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
    parse ();
    check_safety_overwrite ();
    may_clean ();
    OManager.this_is_tool "bslregister";
    may_add_format ();
    if MutableList.length files = 0 then (
      OManager.unquiet "no input files";
      exit 0
    );
    let session =
      let options = bslregister_options () in
      check_options options ;
      BR.create ~options
    in
    let session =
      let pprocess filename content =
        match
          try
            Some (Hashtbl.find spec_process filename)
          with Not_found -> !pprocess
        with None -> content
        | Some command ->
            try
	      let command = Printf.sprintf "%s \"%s\"" command filename in
              let ic = Unix.open_process_in command in
              let rec aux lines =
                try
                  let line = input_line ic in
                  aux (line::lines)
                with
                | End_of_file ->
                    match Unix.close_process_in ic with
                    | Unix.WEXITED 0 -> String.rev_concat_map "\n" (fun s -> s) lines
                    | _ -> OManager.error "Error while preprocessing file '%s', command '%s'"
                        filename command
              in
              aux []
            with
            | Unix.Unix_error (error, a, b) ->
                OManager.error "Unix_error (%S, %S, %S)" (Unix.error_message error) a b
      in
      BR.set_pprocess ~pprocess session
    in
    let session = MutableList.fold_left (
      fun session file ->
        OManager.verbose "registering file @{<bright>%S@}" file ;
        BR.preprocess_file session file
    ) session files
    in
    OManager.verbose "generating files ...";
    let finalized_t = BR.finalize session in
    files_generation finalized_t ;

    BR.js_validator finalized_t;

    OManager.verbose "successfull generation of plugin files : @{<bright>%d@} files" !files_generated ;

    if !auto_build && BR.need_makefile finalized_t then (
      OManager.verbose "building plugin...";
      let ret = Sys.command (Printf.sprintf "%s -C %s -f %s" Config.makebinary !opp_dir (Filename.basename !makefile)) in
      if ret <> 0
      then
        OManager.error "building failure due to error(s) in source files@\n"
      else
        OManager.verbose "successfull compilation of plugin @{<bright>%s@}" !bsl_pref
    );

    (* if success : remove unused logs from previous error *)
    if not (!unsafe_opa || !unsafe_js) then ignore (Sys.command "rm -f bsl_log_*");

    ()
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
