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
   Wrapper around BslRegisterLib to make it easier to call from outside code.

   Most of this file used to be part of bslregister.ml. This was
   refactor to make it easier to call the plugin compiler from the
   rest of the code.
*)

module String = Base.String

module BI = BslInterface
module BR = BslRegisterLib
module List = BaseList

let ( / ) a b = a ^ "/" ^ b (* linux or cygwin compliant, ok for build see for user's space use *)

let files_generated = ref 0

type options = {
  (** A boolean to say if we want to generate the Plugin and the
      Loader file.  these files are used to link statically a plugin
      with an executable, so that there is no need to load dynamically
      a .bypass file for being able to load bypasses of a given plugin.

      Currently, the static mode is used only for the opabsl, and the
      resulting plugin is linked with opa.exe

      The loader file can be used for interpreter in ml only (opatop)
      to add new primitives.  *)
  static: bool;

  (** A boolean to say that generated files should be put in the
      build_dir directly, without storing them in a opp directory. *)
  no_opp: bool;

  (** Flag that says whether identifiers in the nodejs plugin should
      be exported into the global namespace or if they should be used
      as regular module exports (i.e. "exports.foo = foo;") *)
  modular_plugins: bool;

  build_dir: string;
  bsl_pref: string;
  auto_build: bool;
  check_style: bool;
  clean: bool;
  clean_would_only: bool;
  default_iformats: bool;
  extrapaths: string list;
  unsafe_js: bool;
  unsafe_opa: bool;
  bypass_plugins: BslDynlink.bypass_plugin_file list;
  files: string list;
  package_version: string;
  spec_process: string StringMap.t;
  ml_flags: string list;
  mlopt_flags: string list;
  js_validator: string option;
  js_validator_files: string list;
  js_validator_options: string list;
  pprocess: string option;
  js_classic_bypass_syntax: bool;
}

let default_opts = {
  static = false;
  no_opp = false;
  modular_plugins = false;
  build_dir = "";
  bsl_pref = "default";
  auto_build = false;
  check_style = false;
  clean = false;
  clean_would_only = false;
  default_iformats = false;
  extrapaths = [];
  unsafe_js = false;
  unsafe_opa = false;
  bypass_plugins = [];
  files = [];
  package_version = "0.1.0";
  spec_process = StringMap.empty;
  ml_flags = [];
  mlopt_flags = [];
  js_validator = None;
  js_validator_files = [];
  js_validator_options = [];
  pprocess = None;
  js_classic_bypass_syntax = true;
}

type files = {
  opp_dir: string;
  js_files: string list;
  nodejs_files: string list;
  nodejspackage: string;
  package_json: string;
  jskeys: string;
  loader: string;
  marshalplugin: string;
  mlruntime: string;
  mlruntime_mli: string;
  plugin: string;
  makefile: string;
  stamp: string;
  stamp_date: string;
}

(** These are the inclusion to give for compiling plugins.  This text
    will be produced in the Makefile, which will interpret shell
    expansion like $(MY_VAR) *)
let static_extrapaths () =
  Printf.sprintf "-I $(%s)/%s" InstallDir.name InstallDir.lib_opa

(**
   outputs in build_dir/libname.opp/libnameSuffix.ext
   path to file may be absolute, in this case, produces
   all the absolute path inside the build dir
*)
let opp_dir opt =
  if opt.no_opp then opt.build_dir
  else
    let opp = opt.bsl_pref ^ "." ^ BslConvention.Extension.plugin in
    opt.build_dir / opp

let output_prefix ?(mkdir=true) opt prefix filename =
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
  let directory = opp_dir opt in
  let directory = directory/dirname in

  let filename = directory/filename in
  (*
    eventually, check or create the directory
  *)
  if mkdir then (
    if not (File.check_create_path filename) then
      OManager.error "cannot create directory %s" directory
  );

  filename

let files_of_opt opt =
  let map f = output_prefix opt opt.bsl_pref f in
  let module S = BslConvention.Suffix in
  let module E = BslConvention.Extension in
  let has_extension ext f = File.extension f = ext in
  let js_files = List.filter (has_extension "js") opt.files in
  let nodejs_files = List.filter (has_extension "nodejs") opt.files in
  {
    opp_dir          = opp_dir opt;
    js_files;
    nodejs_files;
    nodejspackage    = map (S.nodejspackage ^ ".js");
    package_json     = output_prefix opt "" "package.json";
    jskeys           = map (S.jskeys ^ ".js");
    loader           = map (S.loader ^ ".ml");
    marshalplugin    = map (S.marshalplugin ^ "." ^ E.bypass);
    mlruntime        = map (S.mlruntime ^ ".ml");
    mlruntime_mli    = map (S.mlruntime ^ ".mli");
    plugin           = map (S.plugin ^ ".ml");
    makefile         = output_prefix opt "" "Makefile";
    stamp            = output_prefix opt "" "stamp";
    stamp_date       = DebugTracer.now ();
  }

(*
  This function is used for itering opa and js generated files
*)
let prefix opt filename =
  output_prefix opt (opt.bsl_pref ^ "_") filename

let js_code opt = prefix opt
let opa_code opt = prefix opt
let opa_interface opt f = (prefix opt f)^"i"

(* ======================================================================= *)
(** {6 Makefile Generation} *)
(* ======================================================================= *)
module Makefile :
sig
  (**
     Return a buffer containing the generated Makefile.
  *)
  val generate : options -> files -> Buffer.t
end =
struct
  (** For lisibility of this generation there, we use the
      add_substitute function of the [Buffer] module. This means that
      we use a list of binding for inserting dynamic parts into this
      generated makefile.  As we generating a Makefile, we need to
      generate the $ character, in this case, it is escaped '\$'. *)

  let bindings opt fs =
    let extrapaths =
      static_extrapaths () ^ (
        String.concat_map ~left:" " " " (Printf.sprintf "-I %s")
          opt.extrapaths
      ) in
    let suffix = String.concat " " (
      BslConvention.Suffix.mlruntime ::
        if opt.static then [
          BslConvention.Suffix.loader ;
          BslConvention.Suffix.plugin ;
        ] else []
    )
    in
    let date = fs.stamp_date in

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

      "ml_flags", String.concat " " opt.ml_flags;
      "mlopt_flags", String.concat " " opt.mlopt_flags;

      (* p *)

      "plugin", opt.bsl_pref ;

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

  let generate opt fs =
    let bindings = bindings opt fs in
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

let bslregisterlib_options opt fs =
  let js_validator =
    Option.map (
      fun js ->
        opt.build_dir, (js, opt.js_validator_files), opt.js_validator_options
    ) (opt.js_validator)
  in

  let options = {
    BI.

    basename = Some opt.bsl_pref;
    bypass_plugins = opt.bypass_plugins;
    check_style = opt.check_style;
    js_files = fs.js_files;
    nodejs_files = fs.nodejs_files;
    js_validator ;

    ml_plugin_filename = fs.plugin;
    ml_runtime_filename = fs.mlruntime;

    modular_plugins = opt.modular_plugins;

    unsafe_js = opt.unsafe_js;
    unsafe_opa = opt.unsafe_opa;

    js_classic_bypass_syntax = opt.js_classic_bypass_syntax;

  } in
  options

let iter_generated_files opt fs fct =

  List.iter (
    fun f ->
      match File.extension f with
      | "opa" ->
          fct (opa_code opt f) ;
          fct (opa_interface opt f)  ;
          ()
      | "js" ->
          fct (js_code opt f) ;
          ()
      | "nodejs" ->
          fct (js_code opt f) ;
          ()
      | _ -> ()
  ) opt.files ;

  fct fs.nodejspackage ;
  fct fs.jskeys ;
  fct fs.marshalplugin ;
  fct fs.mlruntime ;
  fct fs.mlruntime_mli ;

  if opt.static then (
    fct fs.loader ;
    fct fs.plugin ;
  ) ;

  fct fs.makefile ;
  fct fs.stamp ;

  ()

let check_safety_overwrite opt fs =
  let fct n =
    if List.mem n opt.files then (
      OManager.error (
        "@[<2><!> bslregister refuses to do that !@\n"^^
        "The file @{<bright>%S@} would be @{<bright>overwritten@} during the process@]"
      ) n
    ) in
  iter_generated_files opt fs fct

let remove_file f =
  try Unix.unlink f with Unix.Unix_error (e,_,_) ->
    OManager.verbose
      "@[<2><!> Cannot clean @{<bright>%s@}:@\n%s@]" f (Unix.error_message e)

(* clean, or just say what would be cleaned *)
let may_clean opt fs =
  if opt.clean then (
    let rm f =
      if opt.clean_would_only
      then OManager.printf "Would remove @{<bright>%s@}@\n" f
      else (
        OManager.unquiet "rm -f %s" f ;
        remove_file f ;
        ()
      )
    in iter_generated_files opt fs rm;
    exit 0
  )

(* preprocessing format, for ##include *)
let may_add_format opt =
  if opt.default_iformats then (
    BslLib.HLParser.add_iformat BslLib.HLParser.default_opa_iformats ;
    ()
  )

(* open_out_bin : read ocaml doc,
   Marshal should be used with binary channel
   for a win OS compatilibity *)
let handle_open_out file =
  try open_out_bin (PathTransform.string_to_mysys file)
  with
  | Sys_error s ->
      OManager.error
        "@[<2>@{<bright>bslregister@}: cannot open_out opa file @{<bright>%s@}:@\n%s@]"
        file s

let handle_close_out file oc =
  try close_out oc
  with
  | Sys_error s ->
      OManager.error
        "@[<2>@{<bright>bslregister@}: cannot close_out @{<bright>%s@}:@\n%s@]"
        file s

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
let files_generation (opt : options) (fs : files) (fin : BR.finalized_t) =
  let iterator_opa_code       = make_iterator (opa_code opt)      in
  let iterator_opa_interface  = make_iterator (opa_interface opt) in

  BR.out_opa_code             iterator_opa_code               fin;
  BR.out_opa_interface        iterator_opa_interface          fin;

  BR.write_nodejs_pack        fs.nodejspackage                fin;

  output fs.jskeys            BR.out_js_keys                  fin;

  output fs.marshalplugin     BR.out_ml_marshal_plugin        fin;

  if opt.static then (
    output fs.loader          BR.out_ml_loader                fin;
    output fs.plugin          BR.out_ml_plugin                fin;
  ) ;

  if BR.need_makefile fin then (
    let makefile = Makefile.generate opt fs in
    output fs.mlruntime       BR.out_ml_runtime               fin;
    output fs.mlruntime_mli   BR.out_ml_runtime_mli           fin;
    output fs.makefile        Buffer.output_buffer            makefile;
  );

  output fs.stamp             Pervasives.output_string        fs.stamp_date;

  ()

let process opt =
  let fs = files_of_opt opt in
  check_safety_overwrite opt fs;
  may_clean opt fs;
  may_add_format opt;
  let session =
    let options = bslregisterlib_options opt fs in
    BR.create ~options
  in
  let session =
    let pprocess filename content =
      match
        try
          Some (StringMap.find filename opt.spec_process)
        with Not_found -> opt.pprocess
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
  let session = List.fold_left (
    fun session file ->
      OManager.verbose "registering file @{<bright>%S@}" file ;
      BR.preprocess_file session file
  ) session opt.files
  in
  OManager.verbose "generating files ...";
  let finalized_t = BR.finalize session in
  files_generation opt fs finalized_t ;

  BR.js_validator finalized_t;

  OManager.verbose "successful generation of plugin files : @{<bright>%d@} files"
    !files_generated ;

  if opt.auto_build && BR.need_makefile finalized_t then (
    OManager.verbose "building plugin...";
    let cmd = Printf.sprintf "%s -C %s -f %s"
      Config.makebinary fs.opp_dir
      (Filename.basename fs.makefile)
    in
    let log = Printf.sprintf "%s/compilation.log" fs.opp_dir in
    let ret =
      let system_call cmd =
        let ic, oc = Unix.open_process cmd in
        let buf = Buffer.create 16 in
        (try
           while true do
             Buffer.add_channel buf ic 1
           done
         with End_of_file -> ());
        let ret = Unix.close_process (ic, oc) in
        Buffer.contents buf, ret
      in
      let std, ret =
        system_call cmd
      in
      ignore (File.output log std);
      match ret with
      | Unix.WSIGNALED _
      | Unix.WSTOPPED _ ->  1
      | Unix.WEXITED x -> x
    in
    if ret <> 0
    then
      OManager.error
        ("Building failure due to error(s) in source files@\n"^^
           "The command was @{<bright>%s@}, log is in @{<bright>%s@}")
        cmd log
    else
      OManager.verbose "successful compilation of plugin @{<bright>%s@}"
        opt.bsl_pref
  );

  (* if success : remove unused logs from previous error *)
  if not (opt.unsafe_opa || opt.unsafe_js) then ignore (Sys.command "rm -f bsl_log_*");
