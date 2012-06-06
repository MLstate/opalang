(*
    Copyright © 2011, 2012 MLstate

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
(* ------------------------------------------------------------ *)
(* Tags, flags, directory contexts and custom stuff             *)
(* ------------------------------------------------------------ *)

(* -- Directory contexts -- *)

shared_namespace_dir "libqmlcompil";
shared_namespace_dir "database";
shared_namespace_dir "opa";
include_subdirs "opalib";
shared_namespace_dir "opalang";
shared_namespace_dir "appruntime";
shared_namespace_dir "libnet";
include_subdirs "qmlflat";

(* -- Stubs -- *)

def_stubs ~dir:"libbase" "stubs";
def_stubs ~dir:"libsecurity" "ssl_ext";
def_stubs ~dir:"appruntime" "io";

let root_ocamldir = Config.ocamllib in

let is_release () = Config.is_release in

let link_cmd = if windows_mode then S[Sh"cp";A"-r"] else S[Sh"ln";A"-s";A"-f"] in

let extralib_opt = function
  | Some (lib,ldir,idir) ->
      flag ["link"; "use_"^lib] (S [A "-ccopt"; P ("-L" ^ ldir); A"-cclib"; A ("-l" ^ lib)]);
      flag ["compile"; "c"; "use_"^lib] (S [A"-I"; P idir])
  | None -> ()
in
extralib_opt Config.libnatpmp;
extralib_opt Config.miniupnpc;

let linux_system_libs = ["iconv"]
and mac_system_libs = []
and windows_system_libs = []
in

let filter_system_libs l =
  let system_libs =
    match Config.os with
    | Config.Linux | Config.FreeBSD -> linux_system_libs
    | Config.Mac -> mac_system_libs
    | Config.Win32 | Config.Cygwin -> windows_system_libs
  in List.filter (fun x -> not (List.mem x system_libs)) l
in

begin
  match Config.camlidl with
  | None -> ()
  | Some camlidl ->
      flag ["link"; "use_camlidl"] (S [A "-cclib"; P "-lcamlidl" ]);

      rule "camlidl processing: .idl -> .ml .mli _stubs.c lib_stubs.clib"
        ~deps:[ "%(dir)stubs%(name).idl" ]
        ~prods:[ "%(dir:<**/>)stubs%(name:<*> and not <*.*> and not <>).mli";
                 "%(dir:<**/>)stubs%(name:<*> and not <*.*> and not <>).ml" ;
                 "%(dir:<**/>)stubs%(name:<*> and not <*.*> and not <>)_stubs.c" ]
        ~insert:`top
        (fun env _build ->
           let dir = env "%(dir)" in
           let name = env "%(name)" in

           def_stubs ~dir (name ^ "_idl");

           Cmd(S[Sh"cd"; P dir; Sh"&&";
                 P camlidl; A"-prepro"; P"/usr/bin/cpp"; A"-no-include"; P ("stubs" ^ name -.- "idl") ])
        )
end;

(* -- Ocamldoc plugin -- *)

flag_and_dep ["ocaml"; "doc"] (S[A"-g";P"utils/ocamldoc_plugin.cmxs"]);

(* ------------------------------------------------------------ *)
(* Hacks                                                        *)
(* ------------------------------------------------------------ *)

(* opatop needs to link its serverLib *then* opabsl *then* the rest. We
   cheat by copying the opabsl generated files so that they can be seen as
   local by opatop. *)
let magic_import_from_opabsl dest f =
  rule ("import from opabsl: "^f) ~deps:["opabsl/"^f] ~prod:(dest / f)
    (fun env build ->
       Seq[Cmd(S[Sh"mkdir";A"-p";P dest]);
           if Pathname.exists "opabsl"
           then (build_list build ["opabsl" / f]; cp ("opabsl"/ f) dest)
           else cp (mlstatelibs/"opabsl"/ f) dest
          ])
in
List.iter
  (fun m -> magic_import_from_opabsl "opatop" (m ^ ".ml"))
  [ "opabslgenMLRuntime"; "opabslgenLoader" ];


(* ------------------------------------------------------------ *)
(* Additional rules: internal                                   *)
(* ------------------------------------------------------------ *)

(* -- static ppdebug -- *)
(* what happens here is:
 * - generate the file _build/environment containing the environment vars used by ppdebug
 * - make files tagged with with_static_proprecessing depend on _build/environment
 * - tag any ml file containing some static ppdebug with the tag with_static_proprecessing
 *)

let static_ppdebug_vars = [
  "CPS_WITH_ML_CLOSURE"; (* disable the use of qml closures in cps continuations
                          * (for performance reasons) *)
  "CPS_STACK_SIZE" ; (* observe the size of the stack @ cps return *)
] in
let environment = "environment" in
let build_env_strings =
    (List.map
       (fun s ->
          Printf.sprintf "let %s = %s\n"
            s (try Printf.sprintf "Some %S" (Sys.getenv s)
               with Not_found -> "None")
       ) static_ppdebug_vars) in
dep ["with_static_preprocessing"] [environment];

rule "environment generation"
  ~prods:[environment;"i_dont_exist"](* fake prod to make sure this rule is
                                      * always called *)
  (fun env build ->
     Echo (build_env_strings, environment));

let generate_ocamldep_rule ml_or_mli =
  rule ("ocaml dependencies " ^ ml_or_mli ^ " reloaded (deps of static ppdebug)")
    ~prod:("%." ^ ml_or_mli ^ ".depends")
    ~dep:("%." ^ ml_or_mli)
    ~insert:`top (* need to be inserted before the real ocamldep rule *)
    (fun env build ->
       let ml = env "%." ^ ml_or_mli in
       if Tags.mem "with_mlstate_debug" (tags_of_pathname ml) &&
         Sys.command ("grep -q '#<Ifstatic:' " ^ Pathname.to_string ml) = 0
       then
         tag_file ml ["with_static_preprocessing"];
       fail_rule build (* failing to that ocamlbuild calls the real ocamldep rule *)
    ) in
generate_ocamldep_rule "ml";
generate_ocamldep_rule "mli";

(* -- Macro-rules generating mlstate_platform.h files -- *)
(* TODO BUG 2 : droits du script invalides *)
(* TODO BUG 3 : path du fichier généré foireux *)
rule "mlstate_platform: () -> libbase/mlstate_platform.h"
  ~deps:(tool_deps "mlstate_platform")
  ~prods:["libbase/mlstate_platform.h"]
  (fun env build ->
     Seq[
       Cmd(S[Sh"chmod +x"; P "libbase/gen_platform"]);
       Cmd(S[get_tool "mlstate_platform"; A (if windows_mode then "WIN" else "")]);
       Cmd(S[Sh"mv mlstate_platform.h libbase/mlstate_platform.h"])
     ]
  );

(* -- Opa stdlib -- *)

(*
  <!> Mathieu Fri Oct 22 10:54:50 CEST 2010
  !! IF YOU NEED TO PATCH THE FOLLOWING RULE !!
  ---> come to me, we have to talk first.
*)
let stdlib_files =
  (* keep in sync with s3passes@pass_AddStdlibFiles*)
  let core_dir = "stdlib/core" in
  let tests_dir = "stdlib/tests" in
  let dirs = core_dir :: tests_dir :: rec_subdirs [ core_dir ] in
  let files = List.fold_right (fun dir acc -> dir_ext_files "js" dir @ dir_ext_files "opa" dir @ acc) dirs [] in
  files
in
rule "stdlib embedded: stdlib_files -> opalib/staticsInclude.of"
  ~deps:stdlib_files
  ~prod:"opalib/staticsInclude.of"
  (fun env build -> Echo (List.map (fun f -> f^"\n") stdlib_files, "opalib/staticsInclude.of"));

let opa_opacapi_files =
  let dirs = rec_subdirs ["stdlib"] in
  let files = List.fold_right (fun dir acc -> dir_ext_files "opa" dir @ acc) dirs [] in
  files
in

(* used in mkinstall *)
let opacapi_validation = "opacapi.validation" in
rule "Opa Compiler Interface Validation (opacapi)"
  ~deps:("opa/checkopacapi.native" :: opa_opacapi_files)
  ~prod:opacapi_validation
  (fun env build ->
     Cmd(S ([
              P "./opa/checkopacapi.native" ;
              A "-o" ;
              P opacapi_validation ;
            ] @ (List.rev_map (fun file -> P file) opa_opacapi_files)));
  );

(* -- Build infos and runtime version handling -- *)

(* TODO: probably same bugs than mlstate_platform *)
let generate_buildinfos = "buildinfos/generate_buildinfos.sh" in
let version_buildinfos = "buildinfos/version_major.txt" in
let pre_buildinfos = "buildinfos/buildInfos.ml.pre" in
let post_buildinfos = "buildinfos/buildInfos.ml.post" in
let buildinfos = "buildinfos/buildInfos.ml" in
rule "buildinfos: buildinfos/* -> buildinfos/buildInfos.ml"
  ~deps:[version_buildinfos ; pre_buildinfos ; generate_buildinfos ; post_buildinfos]
  ~prods:(buildinfos :: (if is_release() then ["always_rebuild"] else []))
  (fun env build ->
     let version = env version_buildinfos in
     let pre_prod = env pre_buildinfos in
     let prod = env buildinfos in
     let post_prod = env post_buildinfos in
     Seq[
       Cmd(S[Sh "cat" ; P pre_prod ; Sh ">" ; P prod]);
       Cmd(S[P "bash"; A "-e"; P generate_buildinfos; P Pathname.pwd;
             if is_release () then A "--release" else N;
             A "--version" ; P version ;
             Sh ">>" ; P prod]);
       Cmd(S[Sh "cat" ; P post_prod ; Sh ">>" ; P prod]);
     ]
  );

let parser_files =
  let dir = ["opalang/classic_syntax";"opalang/js_syntax"] in
  let files = List.fold_right (fun dir acc -> dir_ext_files "trx" dir @ dir_ext_files "ml" dir) dir ["general/surfaceAst.ml"] in
  files
in
let opaParserVersion = "opalang"/"classic_syntax"/"opaParserVersion.ml"
in
rule "opa parser version: opalang/*_syntax/* stdlib -> opalang/classic_syntax/opaParserVersion.ml"
  ~deps:parser_files
  ~prod:opaParserVersion
  (fun build env ->
    let files = List.map (fun s-> P s) parser_files in
    Seq[
      Cmd(S ( [Sh"echo let hash = \\\" > "; P (opaParserVersion)]));
      Cmd(S ( [Sh"cat"] @ files @ [Sh"|"; md5; Sh">>"; P opaParserVersion]));
      Cmd(S ( [Sh"echo \\\" >>"; P opaParserVersion ] ))
    ]
  );

(* -- Internal use of built tools -- *)

rule " ofile"
  ~deps:("%.of" :: tool_deps "ofile")
  ~prod:"%.ml"
  (fun env build ->
     let dir = Pathname.dirname (env "%.of") in
     build_list build (string_list_of_file (env "%.of"));
     Cmd(S[get_tool "ofile"; A"-path"; P(Pathname.pwd / !Options.build_dir); P(env "%.of")]));

(* -- Proto rules -- *)

rule "proto_deps: proto -> proto.depends"
  ~dep: "%.proto"
  ~prod: "%.proto.depends"
  (proto_deps "%.proto" "%.proto.depends");

rule "proto: proto & proto_depends -> ml & mli & trx_parse"
  ~deps:("%.proto" :: "%.proto.depends" :: tool_deps "genproto")
  ~prods: [ "%.ml" ; "%.mli" ; "%_parse.trx" ]
  (fun env build ->
    let dir = Pathname.dirname (env "%.proto") in
    let proto_deps = (string_list_of_file (env "%.proto.depends")) in
    if proto_deps <> [] then build_list build proto_deps;
    Cmd(S[get_tool "genproto"
         ; P (env "%.proto")
         ; P dir
         ; P (Pathname.basename (env "%"))
         ; P (Pathname.basename (env "%") ^ "_parse")
         ]));

(* -- Wsdl2ml rules -- *)

rule "wsdl2ml: wsdl -> types.ml"
  ~deps:("%.wsdl" :: tool_deps "wsdl2ml")
  ~prods: [ "%types.ml" ]
  (fun env build ->
    let dir = Pathname.dirname (env "%.wsdl") in
    Cmd(S[get_tool "wsdl2ml"
         ; P (env "%.wsdl")
         ]));

(* -- Mlidl rules -- *)

rule "mlidl: mlidl -> types.ml & types.mli"
  ~deps:("%.mlidl" :: tool_deps "mlidl")
  ~prods: [ "%types.ml"; "%types.mli" ]
  (fun env build ->
    let dir = Pathname.dirname (env "%.mlidl") in
    Cmd(S[get_tool "mlidl"
         ; P (env "%.mlidl")
         ]));

(* -- js validation  -- *)
(*
  TODO: enable all of them as soon as possible.
*)
let google_closure_compiler_options =
  A"--warning_level"  :: A"VERBOSE"::
    (*
      Turn on every available warning as errors.
      Keep synchronized with the checker.
    *)
    (
      List.fold_left (fun acc s -> A"--jscomp_error" :: A s :: acc)
        [] [
          "accessControls" ;
          "checkRegExp" ;
          (* "checkTypes" ; *)
          "checkVars" ;
          "deprecated" ;
          "fileoverviewTags" ;
          "invalidCasts" ;
          "missingProperties" ;
          (* "nonStandardJsDocs" ; *)
          "strictModuleDepCheck" ;
          "undefinedVars" ;
          "unknownDefines" ;
          "visibility" ;
        ]
    )
in

let js_checker =
  let local = windows_mode in
  A"java" :: A"-jar"  :: (get_tool ~local "jschecker.jar") ::
(*    A"--externs"        :: (get_tool ~local "jschecker_externals.js") ::
    A"--externs"        :: (get_tool ~local "jschecker_jquery.js") ::
    A"--externs"        :: (get_tool ~local "jschecker_clientliblib.js") ::
    A"--js_output_file" :: A output_file :: *)
    google_closure_compiler_options (*@
    A"--js"             :: A clientlib ::
    A"--js"             :: A "opabsl/jsbsl/jquery_ext_bslanchor.extern.js" ::
    A"--js"             :: A "opabsl/jsbsl/jquery_ext_jQueryExtends.extern.js" ::
    A"--js"             :: A "opabsl/jsbsl/selection_ext_bsldom.extern.js" ::
    A"--js"             :: A "opabsl/jsbsl/jquery_extra.externs.js" ::
    A"--js"             :: A"qmlcps/qmlCpsClientLib.js" ::
    []*)
in

(* -- opa plugin -- *)
(* -- plugin that fails to validate -- *)
let accept_js_validation_failure = []
in
(* -- file that are only needed for validation process -- *)
let jschecker_file s =
  let prefix = "jschecker_" in
  try String.sub (Pathname.basename s) 0 (String.length prefix) = prefix with _ -> false
in
let gen_tag prefix s =
  let pfx = prefix ^ "_" in
  let lenp = String.length pfx in
  let t = try String.sub s 0 lenp = pfx with _ -> false in
  if t then Some(String.sub s lenp ((String.length s) -lenp))
  else None
in
let use_tag s = gen_tag "use" s in
let clib_tag s = gen_tag "clib" s in
let opa_plugin_builder_name = "opa-plugin-builder-bin" in
let opa_plugin_builder = get_tool opa_plugin_builder_name in
let opp_build opa_plugin opp oppf env build =
  let dir = Pathname.dirname (env opa_plugin) in
  let path = let p = Pathname.to_string dir in if p = "." then "" else p^"/" in
  let files = string_list_of_file (env "%.opa_plugin") in
  let caml_use_lib = Tags.fold (fun tag list ->
    match use_tag tag with
    | None -> list
    | Some dep -> dep::list
  ) (tags_of_pathname (env "%.opa_plugin")) []
  in
  let c_libs = Tags.fold (fun tag list ->
    match clib_tag tag with
    | None -> list
    | Some dep -> dep::list
  ) (tags_of_pathname (env "%.opa_plugin")) []
    |> filter_system_libs
  in
  let lib_dir s = [A"--ml";A"-I";A"--ml";P (if Pathname.exists s then ".." / s else ("+"^s))] in
  let include_dirs = List.flatten (List.map lib_dir caml_use_lib) in
  let clib s = [A"--ml";A"-cclib";A"--ml";A("-l"^s)] in
  let include_libs = List.flatten (List.map clib c_libs) in
  let files = List.map ((^) path) files in
  build_list build files;
  let files_validation, files_lib = List.partition jschecker_file files in
  let files_lib = List.map (fun s -> P s) files_lib in
  let plugin_name = Pathname.basename (env "%") in
  let files_js = List.filter (fun f-> Pathname.check_extension f "js") files in
  let js_validation = if files_js=[]
    then [A"--js-validator-off"]
    else (
      let files_validation = List.flatten (List.map (fun s -> [A"--js-validator-file";P s]) files_validation) in
      let unsafe_js = if List.mem plugin_name accept_js_validation_failure then [A"--unsafe-js"] else [] in
      let js_checker = List.tl (List.flatten (List.map (fun opt -> [A"--js-validator-opt";opt]) js_checker)) in
      unsafe_js @ [A"--js-validator"] @ js_checker @ files_validation
    )
  in
  let options = [A"--static" ; A"-o" ; P((Pathname.basename (env opp)))] @ include_dirs @ include_libs @js_validation @ files_lib in
  Seq[Cmd(S(opa_plugin_builder::options));
      Cmd(S[A"touch"; P(env oppf) ] )]
in
rule "opa_plugin_dir: opa_plugin -> oppf"
  ~deps:("%.opa_plugin" :: (tool_deps opa_plugin_builder_name))
  ~prod:"%.oppf" (* use a dummy target because ocamlbuild don't want directory target *)
  (opp_build "%.opa_plugin" "%" "%oppf")
;



(* -- BSL compilation (using bslregister) -- *)
let ml_sources_bsl = dir_sources_bsl "opabsl/mlbsl" in
let js_sources_bsl = dir_sources_bsl "opabsl/jsbsl" in
let js_dest_bsl    = dir_sources_bsl ~prefix:"opabslgen_" "opabsl/jsbsl" in
let mlstate_init_bsl = dir_sources_bsl "opabsl/mlstatebsl" in
let js_conf = "opabsl/jsbsl/bsl-sources.jsconf" in

(*
let rec ponctuate s f = function
  | [] -> ""
  | [ty] -> f ty
  | ty::q -> ((f ty) ^ s) ^ ponctuate s f q
in

let _ = Printf.eprintf "js_sources_bsl: %s\n" (ponctuate ", " (fun x -> x)  (List.map Ocamlbuild_plugin.Pathname.to_string js_sources_bsl)) in
let _ = Printf.eprintf "js_dest_bsl: %s\n" (ponctuate ", " (fun x -> x)  (List.map Ocamlbuild_plugin.Pathname.to_string js_dest_bsl)) in
*)

(* TODO: this is not the way to LLVM, the C code is not usable as it is now *)
(* let c_sources_bsl = dir_sources_bsl "opabsl/cbsl" in *)

(* used for js-validation-only *)
rule "opabsl_sources"
  ~deps: (js_conf :: ml_sources_bsl
          @ js_sources_bsl
          @ mlstate_init_bsl
          @ tool_deps "opa-plugin-builder-bin")
  ~prods: ((List.map (fun s -> "opabsl"/s)
             ["opabslgenLoader.ml";"opabslgenPlugin.ml";
              "opabslgen.bypass" ;
              "opabslgenMLRuntime_x.ml";"opabslgenMLRuntime_x.mli";
              "opabslgenJSkeys.js";
             ])@js_dest_bsl)
  begin fun env build ->
    Seq[Cmd(S([Sh"cd opabsl && ";
               get_tool "opa-plugin-builder-bin";
               A"-o";P"opabslgen";
               A"--no-opp";
               A"--no-build";
               A"--static";
              ]
              @special_bsl_options@
                List.map (fun s -> P (".."/s)) (
                  js_conf ::
                  ml_sources_bsl
                  @js_sources_bsl
                  @mlstate_init_bsl
                )));
        mv "opabsl/opabslgenMLRuntime.ml" "opabsl/opabslgenMLRuntime_x.ml";
        mv "opabsl/opabslgenMLRuntime.mli" "opabsl/opabslgenMLRuntime_x.mli";
      ]
  end;

rule "opa-bslgenMLRuntime interface validation"
  ~deps: [
    "opabsl/opabslgenMLRuntime_x.cmo";
    "opabsl/js_validation/imp_client_lib.js";
    "opabsl/js_validation/bsl.js";
  ]
  ~prod: "opabsl/opabslgenMLRuntime.ml"
  (fun env build ->
    Seq[
    cp "opabsl/opabslgenMLRuntime_x.ml" "opabsl/opabslgenMLRuntime.ml";
  ]
  );

let js_pp_bsl    = dir_sources_bsl ~prefix:"opabslgen_" ~suffix:".pp" "opabsl/jsbsl" in

rule "preprocess JS files for validation"
 ~deps:((tool_deps "ppjs")@js_dest_bsl)
 ~prods:js_pp_bsl
 (fun env build ->
    let ppjs = get_tool "ppjs" in
    Cmd(S (ppjs::A"--output-suffix"::A ".pp"::List.map (fun x -> A x) js_dest_bsl))
 );

rule "Client lib JS validation"
  ~deps: (
         "qmljsimp/qmlJsImpClientLib.js" ::
         "qmlcps/qmlCpsClientLib.js"     ::
         "opabsl/jsbsl/jquery_ext_bslanchor.extern.js" ::
         "opabsl/jsbsl/jquery_ext_jQueryExtends.extern.js" ::
         "opabsl/jsbsl/selection_ext_bsldom.extern.js" ::
         "opabsl/jsbsl/jquery_extra.externs.js" ::
         (tool_deps "jschecker.jar") @
         (tool_deps "jschecker_externals.js") @
         (tool_deps "jschecker_clientliblib.js") @
         (tool_deps "jschecker_jquery.js") )
  ~prods:[
    "opabsl/js_validation/imp_client_lib.js";
  ]
 (fun env build ->
    let run_check clientlib output_file =
      let local = windows_mode in
      [
        Cmd(S [Sh"mkdir"; A"-p";P "opabsl/js_validation"]);
        Cmd(S(
          js_checker @
            A"--externs"        :: (get_tool ~local "jschecker_externals.js") ::
            A"--externs"        :: (get_tool ~local "jschecker_jquery.js") ::
            A"--externs"        :: (get_tool ~local "jschecker_clientliblib.js") ::
            A"--js_output_file" :: A output_file ::
            A"--js"             :: A clientlib ::
            A"--js"             :: A "opabsl/jsbsl/jquery_ext_bslanchor.extern.js" ::
            A"--js"             :: A "opabsl/jsbsl/jquery_ext_jQueryExtends.extern.js" ::
            A"--js"             :: A "opabsl/jsbsl/selection_ext_bsldom.extern.js" ::
            A"--js"             :: A "opabsl/jsbsl/jquery_extra.externs.js" ::
            A"--js"             :: A"qmlcps/qmlCpsClientLib.js" ::
            []
        ))
      ]
   in
    Seq (
      run_check "qmljsimp/qmlJsImpClientLib.js" "opabsl/js_validation/imp_client_lib.js"
    )
 );

rule "opa-bslgenMLRuntime JS validation"
  ~deps: (
    (tool_deps "jschecker.jar")
    @ (tool_deps "jschecker_externals.js")
    @ (tool_deps "jschecker_jquery.js")
    @ (tool_deps "jschecker_clientlib.js")
    @ (tool_deps "jschecker_clientliblib.js")
    @ (tool_deps "jschecker_cpsclientlib.js")
    @ js_pp_bsl
    @ [ "opabsl/jsbsl/jquery_ext_bslanchor.extern.js" ;
        "opabsl/jsbsl/jquery_ext_jQueryExtends.extern.js" ;
        "opabsl/jsbsl/selection_ext_bsldom.extern.js" ;
        "opabsl/jsbsl/jquery_extra.externs.js" ;
        "opabsl/opabslgenJSkeys.js" ]
  )
  ~prods: ["opabsl/js_validation/bsl.js"]
  (fun env build ->
     let arg_of_file file acc = match file with (*A very dumb filter to get rid of files that we just can't fix in the first place*)
       | "opabsl/jsbsl/opabslgen_jquery-1.7.2.min.js.pp" -> acc
       | "opabsl/jsbsl/opabslgen_json2.js.pp"        -> acc
       | _                 -> A "--js" :: A file :: acc
     in
     let get_tool = get_tool ~local:windows_mode in
     Seq[
       Cmd(S [Sh"mkdir";A"-p";P "opabsl/js_validation"]);
       Cmd(S(
             A"java" :: A"-jar"  :: (get_tool "jschecker.jar") ::
               A"--externs"        :: (get_tool "jschecker_externals.js") ::
               A"--externs"        :: (get_tool "jschecker_clientliblib.js") ::
               A"--externs"        :: (get_tool "jschecker_clientlib.js") ::
               A"--externs"        :: (get_tool "jschecker_jquery.js") ::
               A"--externs"        :: (get_tool "jschecker_cpsclientlib.js") ::
               A"--externs"        :: A "opabsl/jsbsl/jquery_ext_bslanchor.extern.js" ::
               A"--externs"        :: A "opabsl/jsbsl/jquery_ext_jQueryExtends.extern.js" ::
               A"--externs"        :: A "opabsl/jsbsl/selection_ext_bsldom.extern.js" ::
               A"--externs"        :: A "opabsl/jsbsl/jquery_extra.externs.js" ::
               A"--js_output_file" :: A "opabsl/js_validation/bsl.js" ::
               google_closure_compiler_options @
               (List.fold_right (fun s acc -> arg_of_file s acc) js_pp_bsl [])
               @ [ A"--js" ; A "opabsl/opabslgenJSkeys.js" ]
           ))]
  );

(*
  -The documentation generator does not work if files are not suffixed with '.js'
  -But, we do not need to preprocess the opabslgen_ files with ppjs,
  as for JS validation (files js_pp_bsl)
  -We simply use the files opabslgen_ for generating the doc. It is obtained from
  the origin js file, and with a resolution of bsl directives (+ generation of additionnal
  type directive for the js validation

  Configuration stuff for jsdoc generator.
  Needs to access lots of files. Cf jsdocdir/README.txt
*)
let jsdocdir =
  let opageneral = Pathname.to_string Pathname.pwd in
  opageneral ^ "/tools/jsdoc-toolkit"
in

let jsdoc_target = "doc.jsbsl" in

rule "opa-bslgenMLRuntime JS documentation"
  ~deps: (
    js_dest_bsl
  )
  ~prods: (
    [ jsdoc_target ^ "/index.html" ]
  )
  (fun env build ->
     Cmd(S(
           A"java" ::
           A("-Djsdoc.dir="^jsdocdir) ::
           A("-Djsdoc.template.dir="^jsdocdir^"/templates") ::
           A"-jar" ::
           A(jsdocdir^"/jsrun.jar") :: A(jsdocdir^"/app/run.js") ::
           A("-t="^(jsdocdir^"/templates/jsdoc")) ::
           A("--allfunctions") ::
           (* Set the target directory *)
           A("-d="^jsdoc_target) ::
           (List.map (fun js -> A js) js_dest_bsl)
         ))
  );


(* ------------------------------------------------------------------ *)
(* Additional rules: final compilation (compiling using our backends) *)
(* ------------------------------------------------------------------ *)

(* -- OPA compiler rules -- *)

let stdlib_packages_dir = "stdlib" in

let opaopt = try Sh(Sys.getenv "OPAOPT") with Not_found -> N in

let opacomp_deps_js = string_list_of_file "opa-run-js-libs.itarget" in
let opacomp_deps_native = string_list_of_file "opa-run-libs.itarget" in
let opacomp_deps_byte = List.map (fun l -> Pathname.update_extension "cma" l) opacomp_deps_native in

let opacomp_deps_native = opacomp_deps_native @ opacomp_deps_js in
let opacomp_deps_byte = opacomp_deps_byte @ opacomp_deps_js in

let opa_prefix = Pathname.pwd / !Options.build_dir in

let opa_libs_dir = "lib" / "opa" / "static" in

let opa_share_dir = "share" / "opa" in

let copy_lib_to_runtime lib =
  let modules = string_list_of_file (lib -.- "mllib") in
  let files =
    List.fold_left
      (fun acc f ->
         let dir, modl = Pathname.dirname f, Pathname.basename f -.- "cmi" in
         List.filter (fun m -> Pathname.exists (opa_prefix / m))
           [dir / String.uncapitalize modl; dir / String.capitalize modl] @ acc)
      [] modules
  in
  let stubs =
    List.map (Pathname.update_extension !Options.ext_lib) (dir_ext_files "clib" (mlstate_lib_dir lib))
  in
  let files = stubs @ files in
  Cmd(S(link_cmd :: List.map (fun f -> P (opa_prefix / f)) files @ [ P (opa_prefix / opa_libs_dir) ]))
in

rule "opa run-time libraries"
  ~deps:("libbase"/"mimetype_database.xml" :: opacomp_deps_native)
  ~stamp:"runtime-libs.stamp"
  (fun _env _build ->
     let mllibs = List.filter (fun f -> Pathname.check_extension f "cmxa") opacomp_deps_native in
     let mllibs = List.map Pathname.remove_extension mllibs in
     let copylibs = List.map copy_lib_to_runtime mllibs in
     Seq[
       Cmd(S[Sh"rm"; A"-rf"; P (opa_prefix / opa_libs_dir)]);
       Cmd(S[Sh"mkdir"; A"-p"; P (opa_prefix / opa_libs_dir)]);
       Cmd(S[Sh"rm"; A"-rf"; P (opa_prefix / opa_share_dir)]);
       Cmd(S[Sh"mkdir"; A"-p"; P (opa_prefix / opa_share_dir)]);
       Cmd(S(link_cmd :: List.map (fun f -> P (opa_prefix / f)) opacomp_deps_native
             @ [ P (opa_prefix / opa_libs_dir) ]));
       Cmd(S(link_cmd :: List.map (fun f -> P (opa_prefix / f -.- !Options.ext_lib)) mllibs
             @ [ P (opa_prefix / opa_libs_dir) ]));
       Cmd(S(link_cmd :: P (opa_prefix / "libbase" / "mimetype_database.xml") :: [ P (opa_prefix / opa_share_dir / "mimetype_database.xml") ]));
       Seq copylibs
     ]
  );

let opacomp build src dst_ext opt =
  build_list build
    (List.map ((/) (Pathname.dirname src)) (string_list_of_file (src-.-"depends")));
  let dst = Pathname.update_extension dst_ext src in
  Cmd(S[
        Sh("MLSTATELIBS=\""^ opa_prefix ^"\"");
        get_tool "opa-bin"; opt;
        A"--verbose";(* A"--no-opa-walker"; *)
        opaopt;
        A"-o"; P dst; P src
      ])
in

rule "opa and server dependencies"
  ~deps:("runtime-libs.stamp" :: tool_deps "opa-bin")
  ~stamp:"opacomp.stamp"
  (fun env build -> Nop);

rule "opackdep: .opack -> .opack.depends"
  ~dep:"%.opack"
  ~prod:"%.opack.depends"
  (fun env build ->
     Cmd(S[P"grep"; A"-v"; A"^\\w*-"; P(env "%.opack"); Sh">"; P(env "%.opack.depends"); Sh"|| true"]));

(* A rule to build applications using the stdlib (e.g. opadoc) *)
rule "opacomp: .opack -> .native"
  ~deps: ("%.opack"::"%.opack.depends"::"opa-packages.stamp"::"opacomp.stamp"::[])
  ~prod: "%.native"
  (fun env build ->
     let dir = Pathname.dirname (env "%") in
     let mano_depends = Pathname.pwd / (env "%.depends") in
     if Pathname.exists mano_depends then (
       build_list build (List.map ((/) dir) (string_list_of_file mano_depends))
     );
     build_list build (List.map ((/) dir) (string_list_of_file (env "%.opack.depends")));
     opacomp build (env "%.opack") "native"
       (S[ A"-I" ; P stdlib_packages_dir ; A"--project-root" ; P dir; A"--parser"; A"classic";]));

rule "opacomp: .opack -> .byte"
  ~deps: ("%.opack"::"%.opack.depends"::"opa-packages.stamp"::"opacomp-byte.stamp"::[])
  ~prod: "%.byte"
  (fun env build ->
     let dir = Pathname.dirname (env "%") in
     build_list build (List.map ((/) dir) (string_list_of_file (env "%.opack.depends")));
     opacomp build (env "%.opack") "byte" (S[A"-I";P stdlib_packages_dir]));
(*
  (A"--bytecode"));
  Used to give this option to opa-bin, but since the opa package are build by ocamlbuild,
  we do not generated bytecode version of opa-packages, making the bytecode compilation
  of server not available anymore.
*)

(* temporary and unreliable *)
rule "opadep: .opa -> .opa.depends"
  ~dep: "%.opa"
  ~prod: "%.opa.depends"
  (fun env build ->
     let dep_opx_regex = "^ *import  \\*\\(.\\+\\) *$" in
     let dep_opp_regex = "^ *import-plugin  \\*\\(.\\+\\) *$" in
     let sed_dep dep_regex redir dest = S[sed; A"-n"; A("s%"^dep_regex^"%\\1.opx%p"); P(env "%.opa"); Sh redir; P dest] in
     Seq[
       Cmd(sed_dep dep_opp_regex ">" (env "%.opa.depends"));
       Cmd(sed_dep dep_opx_regex ">>" (env "%.opa.depends"))
     ]
  )
;

rule "opacomp: .opa -> .native"
  ~deps: ("%.opa"::"%.opa.depends"::"opacomp.stamp"::[])
  ~prod: "%.native"
  (fun env build -> opacomp build (env "%.opa") "native" N);

rule "opacomp: .opa -> .byte"
  ~deps: ("%.opa"::"%.opa.depends"::"opacomp-byte.stamp"::[])
  ~prod: "%.byte"
  (fun env build -> opacomp build (env "%.opa") "byte" (A"--bytecode"));

rule "opa bash_completion: opa-bin -> bash_completion"
  ~deps: (tool_deps "opa-bin")
  ~prod: "bash_completion"
  (fun env build ->
     Seq[Cmd(S[get_tool "opa-bin"; A"--bash-completion"])]);


(* -- OPA packages -- *)

let package_to_dir s0 =
  let s = String.copy s0 in
  for i = 0 to String.length s - 1 do
    if s.[i] = '.' then s.[i] <- '/'
  done;
  s
in
let dir_to_package s0 =
  let s = String.copy s0 in
  let len_std = String.length stdlib_packages_dir in
  let pfx,s = (* remove optional stdlib_packages_dir prefix *)
    try
      let pfx = String.sub s 0 len_std in
      if pfx = stdlib_packages_dir
      then pfx, String.sub s (len_std + 1) (String.length s - len_std - 1)
      else "", s
    with Invalid_argument _ -> "", s
  in
  for i = 0 to String.length s - 1 do
    if s.[i] = '/' then s.[i] <- '.'
  done;
  pfx, s
in

let module RuleFailure = struct exception E end in
let files_of_package pkg =
  let pkdir = package_to_dir pkg in
  if not (Pathname.is_directory (Pathname.pwd / pkdir)) then
    let () = Printf.eprintf "Error: can not find sources for package %s (directory %s does not exist)\n" pkg pkdir in
    raise RuleFailure.E
  else
    let opack = dir_ext_files "opack" (Pathname.pwd / pkdir) in
    let files = dir_ext_files "opa" (Pathname.pwd / pkdir) in
    let files = files @ opack in
    (*
      return relative filenames
    *)
    let files =
      let len = String.length Pathname.pwd + 1 in (* the additinal '/' *)
      let relative_part s = String.sub s len (String.length s - len) in
      List.map relative_part files
    in
    (*
      When you compare 2 branches, if the order of opa sources is not deterministic, you can become crazy
    *)
    let files = List.sort String.compare files in
    files in

let make_all_packages = stdlib_packages_dir/"all_packages.sh" in
let all_packages_file = stdlib_packages_dir/"all.packages" in

rule "all.packages"
  ~dep: make_all_packages
  ~prod: all_packages_file
  (fun env build ->
     Cmd(S[
           Sh"cd"; P (Pathname.pwd / stdlib_packages_dir); Sh"&&";
           P"./all_packages.sh"; Sh">"; P (Pathname.pwd / !Options.build_dir / all_packages_file);
         ])
  );

let make_all_plugins = stdlib_packages_dir/"all_plugins.sh" in
let all_plugins_file = stdlib_packages_dir/"all.plugins" in

rule "all.plugins"
  ~dep: make_all_plugins
  ~prod: all_plugins_file
  (fun env build ->
     Cmd(S[
           Sh"cd"; P (Pathname.pwd / stdlib_packages_dir); Sh"&&";
           P"./all_plugins.sh"; Sh">"; P (Pathname.pwd / !Options.build_dir / all_plugins_file);
         ])
  );

let opa_create_prefix = "tools/opa-create/src/opa-create" in 
let opa_create_src = opa_create_prefix ^ ".opa" in 
let opa_create_dst = opa_create_prefix ^ ".exe" in

let dir_all_files dir =
  List.filter (fun p -> (not (Pathname.is_directory p))) (dirlist dir)
in

let dir_rec_all_files dir =
  let dirs =  rec_subdirs [ dir ] in
  List.fold_right (fun dir acc -> dir_all_files dir @ acc) dirs []
in

rule "opa application creator"
  ~deps:((dir_rec_all_files "tools/opa-create"))
  ~prods: [opa_create_dst]
  (fun env build -> 
      Cmd(S[
        Sh("MLSTATELIBS=\""^ opa_prefix ^"\"");
        get_tool "opa-bin";
        A"-o"; P opa_create_dst; P opa_create_src
      ]));

let package_building ~name ~stamp ~stdlib_only ~rebuild =
  rule name
    ~deps:[opacapi_validation;all_plugins_file;all_packages_file;"opacomp.stamp"]
    ~stamp
    ~prod:"i_dont_exist" (* forces ocamlbuild to always run the command *)
  (fun env build ->
     try
       let plugins = string_list_of_file all_plugins_file in
       let plugins = List.map (fun f -> "plugins" /  f / f -.- "oppf") plugins in
       build_list build plugins;
       let packages = string_list_of_file all_packages_file in
       let packages =
         if stdlib_only
         then
           let stdlib = "stdlib.core" in
           List.filter (fun package ->
                          String.length package > String.length stdlib &&
                            stdlib = String.sub package 0 (String.length stdlib)) packages
         else packages in
       let list_package_files = List.map
         (fun pkg ->
            let files = files_of_package pkg in
            (*
              Copy in _build the opa files of the packages.
              In this way, files given to the compiler are relative,
              and api files are generated in the _build
              This makes also that the packages does not contain absolute filename,
              which is not valid wrt the deb checker.
            *)
            build_list build files;
            (pkg, files)) packages in
       let conf =
         List.concat (
           List.map
             (fun (pkg, files) ->
                (pkg ^ ":\n") ::
                  (
                    (* auto import *)
                    let prefix = "stdlib.core" in
                    let len = String.length prefix in
                    if String.length pkg >= String.length prefix && String.sub pkg 0 len = prefix
                    then (
                      if pkg = prefix
                      then ""
                      else
                        (* subdirectory of stdlib.core import stdlib.core *)
                         "  import stdlib.core\n"
                    )
                    else "  import stdlib.core\n  import stdlib.core.*\n"
                  ) ::
                  List.map (fun file -> "  " ^ file ^ "\n") files)
             list_package_files
         ) in
       let all_files =
         (*List.concat (List.map (fun (_,files) -> List.map (fun f -> P f) files) list_package_files)*)
         [A"--conf-opa-files"]
       in
       let rebuild_opt = if rebuild then [A"--rebuild"] else [] in
       Seq[
         Echo(conf, "conf");
         Cmd(S([Sh("MLSTATELIBS=\""^ opa_prefix ^"\"");
                get_tool "opa-bin";
                A"--autocompile";
                (* A"--verbose-build"; *)
                A"--conf";P "conf";
                A"--slicer-check"; A "low";
                A"--warn-error"; A"root";
                A"--project-root"; P Pathname.pwd; (* because the @static_resource in the stdlib expect this *)
                A"--no-stdlib";
                A"--parser"; A"classic";
                opaopt;
                S all_files;
               ] @ rebuild_opt));
       ]
     with RuleFailure.E ->
       fail_rule build
  ) in

package_building
  ~name:"opa-packages: meta-rule to build all .opx"
  ~stamp:"opa-packages.stamp"
  ~stdlib_only:false
  ~rebuild:false;

package_building
  ~name:"opa-stdlib-packages: meta-rule to build all the stdlib .opx"
  ~stamp:"opa-stdlib-packages.stamp"
  ~stdlib_only:true
  ~rebuild:false;

() (* This file should be an expr of type unit *)
