(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

let pp = Format.fprintf in

let fmt_sprintf fmt =
  Format.kfprintf (fun _ -> Format.flush_str_formatter ()) Format.str_formatter fmt
in

let pp_list sep ?singleton p f l =
  match l with
    | [] -> ()
    | h :: t ->
        (match singleton, t with Some p, [] -> p f h | _ -> p f h);
        let rec aux = function
          | [] -> ()
          | h :: t -> pp f sep; p f h; aux t in
        aux t
in

(* ------------------------------------------------------------ *)
(* Tags, flags, directory contexts and custom stuff             *)
(* ------------------------------------------------------------ *)


(* -- Directory contexts -- *)

shared_namespace_dir (prefix_me "compiler/libqmlcompil");
shared_namespace_dir (prefix_me "compiler/opa");
shared_namespace_dir (prefix_me "compiler/opalang");
include_subdirs (prefix_me "compiler/opalib");

(* -- Stubs -- *)

def_stubs ~dir:(prefix_me "ocamllib/libbase") "stubs";

(* PATHS *)

let prefixed_plugins_dir = prefix_me "lib" / "plugins" in
let libbase_dir = prefix_me "ocamllib" / "libbase" in

let extralib_opt = function
  | Some (lib,ldir,idir) ->
      flag ["link"; "use_"^lib] (S [A "-ccopt"; P ("-L" ^ ldir); A"-cclib"; A ("-l" ^ lib)]);
      flag ["compile"; "c"; "use_"^lib] (S [A"-I"; P idir])
  | None -> ()
in

(* Pre-installed system libs *)
let linux_system_libs = ["iconv"]
and mac_system_libs = []
and windows_system_libs = []
in

let system_libs =
  if is_linux || is_fbsd then linux_system_libs
  else if is_mac then mac_system_libs
  else if is_win then windows_system_libs
  else []
in

let is_system_libs lib = List.mem lib system_libs in

let filter_system_libs l =
  List.filter (fun x -> not (is_system_libs x)) l
in

(* -- Ocamldoc plugin -- *)

flag_and_dep ["ocaml"; "doc"] (S[A"-g";P(prefix_me "tools/utils/ocamldoc_plugin.cmxs")]);


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
  ~prods:[environment;"always_rebuild"] (* fake prod to make sure this rule is
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

(* -- Macro-rule generating mlstate_platform.h file -- *)
rule "mlstate_platform: () -> mlstate_platform.h"
  ~deps:(tool_deps mlstate_platform)
  ~prods:["mlstate_platform.h"]
  (fun env build ->
     Seq[
       (* TODO : we should not need to change mode manually *)
       chmod (A"+x") (libbase_dir/"gen_platform");
       Cmd(S[get_tool mlstate_platform; A (if is_win32 then "WIN" else "")]);
     ]
  );

(* -- Opa stdlib -- *)

let stdlib_files =
  (* keep in sync with s3passes@pass_AddStdlibFiles*)
  let core_dir = prefix_me "lib/stdlib/core" in
  let tests_dir = prefix_me "lib/stdlib/tests" in
  let dirs = core_dir :: tests_dir :: rec_subdirs [ core_dir ] in
  let files = List.fold_right (fun dir acc -> dir_ext_files "js" dir @ dir_ext_files "opa" dir @ acc) dirs [] in
  files
in
rule "stdlib embedded: stdlib_files -> opalib/staticsInclude.of"
  ~deps:stdlib_files
  ~prod:(prefix_me "compiler/opalib/staticsInclude.of")
  (fun env build -> Echo (List.map (fun f -> f^"\n") stdlib_files, (prefix_me "compiler/opalib/staticsInclude.of")));

(* begin opacapi *)

let opa_opacapi_files =
  let dirs = rec_subdirs [prefix_me "lib/stdlib/core"] @ rec_subdirs [prefix_me "lib/stdlib/database"] in
  let files = List.fold_right (fun dir acc -> dir_ext_files "opa" dir @ acc) ((prefix_me "lib/stdlib/core")::dirs) [] in
  files
in

let stdlib_packages_dir = prefix_me "lib"/"stdlib" in
let stdlib_parser_options =
  let files = string_list_of_file (stdlib_packages_dir/"new_syntax") in
  let files = List.map prefix_me files in
  [A "--parser" ; A (fmt_sprintf "js-like:%a" (pp_list "," Format.pp_print_string) files)
  ;A "--parser" ; A "classic"
  ]
in

let opacapi_validation_rule ?(opts=[]) name prod =
  rule name
    ~deps:(tool_deps "checkopacapi" @ opa_opacapi_files)
    ~prod
    (fun env build ->
       Cmd(S ([ get_tool "checkopacapi" ;
		A "-o" ;
		P prod ;
              ]
	      @ (List.rev_map (fun file -> P file) opa_opacapi_files)
	      @ stdlib_parser_options
	      @ opts
	   )
        )
    );
in

let opacapi_validation = "opacapi.validation" in
let () = opacapi_validation_rule "Opa Compiler Interface Validation (opacapi)" opacapi_validation in

(* end opacapi *)


(* -- Build infos and runtime version handling -- *)

(* TODO: probably same bugs than mlstate_platform *)
let generate_buildinfos = prefix_me "compiler/buildinfos/generate_buildinfos.sh" in
let version_buildinfos = prefix_me "compiler/buildinfos/version_major.txt" in
let pre_buildinfos = prefix_me "compiler/buildinfos/buildInfos.ml.pre" in
let post_buildinfos = prefix_me "compiler/buildinfos/buildInfos.ml.post" in
let buildinfos = prefix_me "compiler/buildinfos/buildInfos.ml" in
rule "buildinfos: compiler/buildinfos/* -> compiler/buildinfos/buildInfos.ml"
  ~deps:[version_buildinfos; pre_buildinfos; generate_buildinfos; post_buildinfos]
  ~prods:(buildinfos :: (if Config.is_release then ["always_rebuild"] else []))
  (fun env build ->
     let version = env version_buildinfos in
     let pre_prod = env pre_buildinfos in
     let prod = env buildinfos in
     let post_prod = env post_buildinfos in
     Seq[
       Cmd(S[Sh "cat" ; P pre_prod ; Sh ">" ; P prod]);
       Cmd(S([P "bash"; A "-e"; P generate_buildinfos; P source_dir;
             if Config.is_release then A "--release" else N;
             A "--version" ; P version ] @ (
	     try
	       let suffix = Sys.getenv "VERSION_SUFFIX" in
	       [ A "--version-suffix"; A suffix ]
	     with Not_found -> []
	   ) @ [Sh ">>" ; P prod]));
       Cmd(S[Sh "cat" ; P post_prod ; Sh ">>" ; P prod]);
     ]
  );

(* Warning: make sure to call this after producing the file *)
let get_version_buildinfos () =
  List.hd (string_list_of_file (build_dir/version_buildinfos)) in

let parser_files =
  let dir = [
    prefix_me "compiler/opalang/classic_syntax";
    prefix_me "compiler/opalang/js_syntax"
  ] in
  let files = List.fold_right (fun dir acc -> dir_ext_files "trx" dir @ dir_ext_files "ml" dir) dir ["general/surfaceAst.ml"] in
  files
in
let opaParserVersion = prefix_me "compiler"/"opalang"/"classic_syntax"/"opaParserVersion.ml" in
rule "opa parser version: compiler/opalang/*_syntax/* stdlib -> compiler/opalang/classic_syntax/opaParserVersion.ml"
  ~deps:parser_files
  ~prod:opaParserVersion
  (fun build env ->
    let files = List.map (fun s-> P s) parser_files in
    Seq[
      Cmd(S ( [Sh"echo let hash = \\\" > "; P (opaParserVersion)]));
      Cmd(S ( [Sh"cat"] @ files @ [Sh"|"; md5; Sh">>"; P opaParserVersion]));
      Cmd(S ( [Sh"echo \\\" >>"; P opaParserVersion ] ));
    ]
  );

let dependencies_path = prefix_me "tools"/"dependencies" in
let launch_helper_script = dependencies_path/"launch_helper.sh" in
let launch_helper_js = dependencies_path/"launch_helper.js" in
let qml2js_path = prefix_me "compiler"/"qml2js" in
let qml2js_file = qml2js_path/"qml2js.ml" in
let launchHelper = qml2js_path/"launchHelper.ml" in

let escape_external_content = [
  Sh"|"; Sh"sed -e 's/\\\\/\\\\\\\\/g'";
  Sh"|"; Sh"sed -e 's/\\\"/\\\\\\\"/g'";
] in

let escape_sh_comments = [
  Sh"|"; Sh"sed -e '\\%^#\\(.*\\)%d'";
] in

let escape_js_comments = [
  Sh"|"; Sh"sed -e '\\%^//\\(.*\\)%d'";
] in

rule "copy launch files"
  ~deps:[]
  ~prods:[launch_helper_script; launch_helper_js; "always_rebuild"]
  (fun env build ->
     Seq[
       Cmd(S[Sh"mkdir"; A"-p"; P dependencies_path]);
       cp (source_dir/launch_helper_script) launch_helper_script;
       cp (source_dir/launch_helper_js) launch_helper_js;
     ]);

rule "launchHelper: tools/dependencies/launch_helper.sh -> compiler/qml2js/launchHelper.ml"
  ~deps:[launch_helper_script; launch_helper_js]
  ~prods:[launchHelper]
  (fun env build ->
     Seq[
       Cmd(S[Sh"mkdir"; A"-p"; P qml2js_path]);
       Cmd(S([Sh"echo let script = \\\" > "; P launchHelper]));
       Cmd(S([Sh"cat"; P launch_helper_script]
             @ escape_external_content
             @ escape_sh_comments
             @ [Sh">>"; P launchHelper]
            ));
       Cmd(S([Sh"echo \\\" >>"; P launchHelper]));
       Cmd(S([Sh"echo let js = \\\" >> "; P launchHelper]));
       Cmd(S([Sh"cat"; P launch_helper_js]
             @ escape_external_content
             @ escape_js_comments
             @ [Sh">>"; P launchHelper]
            ));
       Cmd(S([Sh"echo \\\" >>"; P launchHelper]));
     ]
  );


(* -- Internal use of built tools -- *)

rule "ofile"
  ~deps:("%.of" :: tool_deps "ofile")
  ~prod:"%.ml"
  (fun env build ->
     let dir = Pathname.dirname (env "%.of") in
     build_list build (string_list_of_file (env "%.of"));
     Cmd(S[get_tool "ofile"; A"-path"; P(build_dir); P(env "%.of")]));

(* -- begin js validation -- *)
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
          (* "missingProperties" ; *)
          (* "nonStandardJsDocs" ; *)
          "strictModuleDepCheck" ;
          "undefinedVars" ;
          "unknownDefines" ;
          "visibility" ;
        ]
    )
in

let js_checker =
  let local = is_win32 in
  A"java" :: A"-jar"  :: (get_tool ~local "jschecker.jar") ::
    google_closure_compiler_options
in

(* -- end js validation -- *)


(* -- opa plugin -- *)
(* -- plugin that fails to validate -- *)
(* TODO - Remove this and add preprocessing on plugins files before js validation *)
let accept_js_validation_failure = ["server"]
in
(* -- file that are only needed for validation process -- *)
let is_jschecker_file s =
  let suffix = ".externs.js" in
  let suflen = String.length suffix in
  let s = Pathname.basename s in
  let start = (String.length s) - suflen in
  s = "externs.js" ||
      try String.sub s start suflen = suffix with _ -> false
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

let client_lib_validation_externs = [
  prefix_me "lib"/"plugins"/"opabsl"/"jsbsl"/"jquery_ext_bslanchor.externs.js";
  prefix_me "lib"/"plugins"/"opabsl"/"jsbsl"/"jquery_ext_jQueryExtends.externs.js";
  prefix_me "lib"/"plugins"/"opabsl"/"jsbsl"/"selection_ext_bsldom.externs.js";
  prefix_me "lib"/"plugins"/"opabsl"/"jsbsl"/"jquery_extra.externs.js"
] in

let client_lib_files = [
  prefix_me "compiler"/"qmljsimp"/"qmlJsImpClientLib.js";
  prefix_me "compiler"/"qmlcps"/"qmlCpsClientLib.js";
  prefix_me "compiler"/"qml2js"/"clientLibLib.js"
] in

let client_lib_validation_output =
  prefix_me "lib"/"plugins"/"opabsl"/"js_validation"/"imp_client_lib.js" in

rule "Client lib JS validation"
  ~deps:(tool_deps "jschecker.jar" @
           tool_deps "jschecker_externals.js" @
           tool_deps "jschecker_jquery.js" @
           client_lib_validation_externs @
           client_lib_files)
  ~prod:client_lib_validation_output
 (fun env build ->
   let concat_map f l = List.concat (List.map f l) in
   Seq[
     Cmd(S [Sh"mkdir"; A"-p";P (prefix_me "lib/plugins/opabsl/js_validation")]);
     Cmd(S(
       js_checker @
         [A"--externs"; get_tool ~local:is_win32 "jschecker_externals.js";
          A"--externs"; get_tool ~local:is_win32 "jschecker_jquery.js"] @
         (concat_map (fun ext -> [A"--externs"; A ext])
            client_lib_validation_externs) @
         (concat_map (fun inp -> [A"--js"; P inp])
            client_lib_files) @
         [A"--js_output_file"; A client_lib_validation_output]
     ))
   ]
 );

(*
  -The documentation generator does not work if files are not suffixed with '.js'
  -But, we do not need to preprocess the opabsl_ files with ppjs,
  as for JS validation (files js_pp_bsl)
  -We simply use the files opabsl_ for generating the doc. It is obtained from
  the origin js file, and with a resolution of bsl directives (+ generation of additionnal
  type directive for the js validation

  Configuration stuff for jsdoc generator.
  Needs to access lots of files. Cf jsdocdir/README.txt
*)
let jsdocdir =
  source_dir/"tools"/"jsdoc-toolkit"
in

let jsdoc_target = "doc.jsbsl" in

let js_doc_input =
  (prefixed_plugins_dir/"opabsl.opp"/"opabslNodeJsPackage.js") :: client_lib_files in

rule "opa-bslgenMLRuntime JS documentation"
  ~deps:(client_lib_validation_output :: js_doc_input)
  ~prod:(jsdoc_target ^ "/index.html")
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
           (List.map (fun js -> P js) js_doc_input)
         ))
  );

(* ------------------------------------------------------------------ *)
(* Additional rules: final compilation (compiling using our backends) *)
(* ------------------------------------------------------------------ *)

(* -- begin compiler -- *)

let build_tools_dir = prefix_me "tools"/"build" in

let opaopt = try Sh(Sys.getenv "OPAOPT") with Not_found -> N in

let prefixed_string_list_of_file f =
  let l = string_list_of_file f in
  List.map prefix_me l
in

let opacomp_deps_js = prefixed_string_list_of_file (build_tools_dir/"opa-run-js-libs.itarget") in
let opacomp_deps_js_cps = prefixed_string_list_of_file (build_tools_dir/"opa-run-js-cps-libs.itarget") in
let opacomp_deps_js_no_cps = prefixed_string_list_of_file (build_tools_dir/"opa-run-js-no-cps-libs.itarget") in

let opa_libs_dir = "lib" / "opa" / "static" in

let opa_share_dir = "share" / "opa" in

let copy_lib_to_runtime lib =
  let modules = string_list_of_file (lib -.- "mllib") in
  let files =
    List.fold_left
      (fun acc f ->
	 let unprefixed =
           let dir, modl = Pathname.dirname f, Pathname.basename f -.- "cmi" in
	   List.filter (fun m -> Pathname.exists (build_dir / m))
             [dir / String.uncapitalize modl; dir / String.capitalize modl] in
	 let prefixed  =
	   let f = prefix_me f in
	   let dir, modl = Pathname.dirname f, Pathname.basename f -.- "cmi" in
	   List.filter (fun m -> Pathname.exists (build_dir / m))
             [dir / String.uncapitalize modl; dir / String.capitalize modl] in
	 unprefixed @ prefixed @ acc)
      [] modules
  in
  let stubs =
    List.map (Pathname.update_extension !Options.ext_lib) (dir_ext_files "clib" (mlstate_lib_dir lib))
  in
  let files = stubs @ files in
  Cmd(S(link_cmd :: List.map (fun f -> P (build_dir / f)) files
	@ [ P (build_dir / opa_libs_dir) ]))
in

let globalizer_prods dest = [dest/"package.json"; dest/"main.js"] in
let opa_js_runtime_cps = "opa-js-runtime-cps" in
let opa_js_runtime_no_cps = "opa-js-runtime-no-cps" in

(* Convert the JS runtime to a global-prefixed nodejs package *)
let js_runtime_rule (files, dest) =
  rule ("opa js runtime " ^ dest)
    ~deps:(version_buildinfos :: tool_deps "globalizer" @ files)
    ~prods:(globalizer_prods dest)
    (fun env build ->
      let version = get_version_buildinfos () in
      Cmd(S(get_tool "globalizer" :: A"-o" :: A dest ::
            A "--package-version" :: A version ::
            List.map (fun file -> P file) files))
    )
in
List.iter js_runtime_rule [opacomp_deps_js_cps, opa_js_runtime_cps;
                           opacomp_deps_js_no_cps, opa_js_runtime_no_cps];


rule "opa js run-time libraries"
  ~deps:(libbase_dir/"mimetype_database.xml" ::
            globalizer_prods opa_js_runtime_cps @
            globalizer_prods opa_js_runtime_no_cps @
	    opacomp_deps_js
  )
  ~stamp:"js-runtime-libs.stamp"
  (fun _env _build ->
     let mllibs = List.filter (fun f -> Pathname.check_extension f "cmxa") opacomp_deps_js in
     let mllibs = List.map Pathname.remove_extension mllibs in
     let js_deps =
       List.map (fun f -> P (build_dir / f)) opacomp_deps_js in
     let mllibs_local =
       List.map (fun f -> P (build_dir / f -.- !Options.ext_lib)) mllibs in
     let copylibs = List.map copy_lib_to_runtime mllibs in
     let may_link_cmd l p = if l = [] then Cmd(A"true") else Cmd(S(link_cmd :: l @ p)) in
     Seq[
       Cmd(S[Sh"mkdir"; A"-p"; P (build_dir / opa_libs_dir)]);
       Cmd(S[Sh"mkdir"; A"-p"; P (build_dir / opa_share_dir)]);
       Cmd(S(link_cmd :: js_deps @ [ P (build_dir / opa_libs_dir) ]));
       may_link_cmd mllibs_local [P (build_dir / opa_libs_dir)];
       Cmd(S(link_cmd ::
             P (build_dir / libbase_dir / "mimetype_database.xml") ::
             [ P (build_dir / opa_share_dir / "mimetype_database.xml") ]));
       Cmd(S[link_cmd;
             P (build_dir / opa_js_runtime_cps);
             P (build_dir / opa_libs_dir)]);
       Cmd(S[link_cmd;
             P (build_dir / opa_js_runtime_no_cps);
             P (build_dir / opa_libs_dir)]);
       Seq copylibs
     ]
  );

let opacomp build src dst_ext opt =
  build_list build
    (List.map ((/) (Pathname.dirname src)) (string_list_of_file (src-.-"depends")));
  let dst = Pathname.update_extension dst_ext src in
  set_mlstatelibs ();
  Seq [
    Cmd(S[
          get_tool "opa-bin"; opt;
          opaopt;
          A"-o"; P dst; P src
	]);
    unset_mlstatelibs
  ]
in

rule "js opa and server dependencies"
  ~deps:("js-runtime-libs.stamp" :: tool_deps "opa-bin")
  ~stamp:"opacomp.stamp"
  (fun env build -> Nop);

rule "opackdep: .opack -> .opack.depends"
  ~dep:"%.opack"
  ~prod:"%.opack.depends"
  (fun env build ->
     Cmd(S[P"grep"; A"-v"; A"^\\w*-"; P(env "%.opack"); Sh">"; P(env "%.opack.depends"); Sh"|| true"]));

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
  );

(* -- end compiler -- *)


(* -- begin plugins -- *)

(** Generates a rule to build the [name] Opa plugin *)
let plugin_building
    ?(additional=(fun _ _ -> ([], Tags.empty)))
    ?rule_name
    plugins_dir name =

  (* Plugins paths *)

  let p_path        = plugins_dir/name in
  let p_opp         = p_path -.- "opp" in
  let p_oppf        = p_path/name -.- "oppf" in
  let p_opa_plugin  = p_path/name -.- "opa_plugin" in
  let p_tags = tags_of_pathname p_opa_plugin in
  let p_files =
    if Pathname.exists p_opa_plugin then string_list_of_file p_opa_plugin
    else [] in

  let prefixed_files = List.map (fun file -> p_path/file) p_files in

  let (additional_files, additional_tags) = additional plugins_dir name in
  let files = prefixed_files @ additional_files in
  let tags = Tags.union p_tags additional_tags in

  (* Plugins options *)
  let options = A "-o" :: A name :: A "--build-dir" :: A prefixed_plugins_dir :: [] in

  let options (* Ocaml libs *) = Tags.fold
    (fun tag options -> match use_tag tag with
     | None -> options
     | Some d ->
         let dir = match mlstate_lib_dir d with
           | "." -> "+"^d
           | dir ->
	       let dir =
		 (* HACK: always prefix opabsl.opp *)
		 if dir = "lib/plugins/opabsl.opp" then prefix_me dir
		 else dir in
	       build_dir/dir
         in
         A "--ml" :: A "-I" :: A "--ml" :: P dir :: options
    ) tags options
  in

  let options (* C libs *) = Tags.fold
    (fun tag options ->
       match clib_tag tag with
       | None -> options
       | Some dep ->
           if not (is_system_libs dep) then
             A "--ml" :: A "-cclib" :: A "--ml" :: A ("-l"^dep) :: options
           else options
    ) tags options
  in

  let has_node, options (* JavaScript files *) =
    let jsfiles =
      List.filter
        (fun f -> Pathname.check_extension f "js" || Pathname.check_extension f "nodejs")
        files in

    let options = List.fold_left
      (fun options jsfile ->
           match is_jschecker_file jsfile with
           | true -> A "--js-validator-file" :: P jsfile :: options
           | false ->
               P jsfile ::
               match Tags.mem "with_mlstate_debug" (tags_of_pathname jsfile) with
               | true -> A "--pp-file" ::
                   P (Printf.sprintf "%s:%s" jsfile
                        (string_of_command_spec (get_tool"ppjs"))) :: options
               | false -> options
      ) options (List.rev jsfiles)
    in
    (* Specify the JavaScript validator *)
    (List.exists (fun f -> Pathname.check_extension f "nodejs") jsfiles),
    if jsfiles = [] then options
    else match js_checker with
    | [] -> []
    | t::q ->
        (* TODO : Remove unsafe-js *)
        A "--unsafe-js" :: A "--js-validator" :: t
        :: List.fold_right (fun opt acc -> A"--js-validator-opt"::opt::acc) q options
  in

  let options =
    let conf_files =
      List.filter (fun f ->
        Pathname.check_extension f "jsconf" ||
          Pathname.check_extension f "nodejsconf"
      ) files in
    List.map (fun file -> P file) conf_files @ options in

  let has_ml, options (* OCaml files *) =
    let mlfiles = List.filter (fun f -> Pathname.check_extension f "ml") files in
    (mlfiles <> [],
     List.fold_left
       (fun options mlfile ->
          P mlfile ::
            match Tags.mem "with_mlstate_debug" (tags_of_pathname mlfile) with
            | true ->
                A"--pp-file"
                :: P (Printf.sprintf "%s:%s" mlfile (source_dir/(prefix_me "tools"/"utils"/"ppdebug.pl")))
                :: options
            | false -> options
       ) options (List.rev mlfiles)
    )
  in

  let options (* Opa files *) =
    List.map (fun p -> P p) (List.filter (fun f -> Pathname.check_extension f "opa") files)
    @ options
  in

  let is_static = Tags.mem "static" (tags_of_pathname p_path) in
  let options =
    if is_static then
      A "--static" :: A "--no-build" :: options
    else options
  in

  let options = A "--js-bypass-syntax" :: A "new" :: options in

  (* Hack for opabsl *)
  let postlude = [] in
  let prods = [] in
  let prods, files, postlude =
    if name = "opabsl" then
      ("serverLib.mli"::prods, p_path/"serverLib.mli"::files,
       (ln_f (p_path/"serverLib.mli") (p_opp/"serverLib.mli")) :: postlude
      )
    else (prods, files, postlude)
  in

  (* Plugins productions *)
  let prod_suffix suffix =
    Printf.sprintf "%s%s" name suffix
  in
  let prods =
    if has_ml then
      prod_suffix "MLRuntime.ml" :: prod_suffix "MLRuntime.mli" :: prods
    else prods
  in
  let prods =
    if has_node then
      prod_suffix "NodeJsPackage.js" :: prods
    else prods
  in
  let prods =
    if is_static then (
      let prods =
        prod_suffix "Plugin.ml" :: prod_suffix "Loader.ml" :: prods
      in
      (* Tags all produced files (in %.opp) as file in (%) *)
      List.iter
        (fun file ->
           let tags = Tags.elements (tags_of_pathname (p_path/file)) in
           tag_file (p_opp/file) tags;
        ) prods;
      prods
    )
    else prods
  in
  let prods = List.map (fun file -> p_opp/file) prods in
  let prods = prods in

  (* Plugins deps *)

  let deps =
    files @ (tool_deps "jschecker.jar") @ (tool_deps "ppdebug") @
      (tool_deps "ppjs") @ (tool_deps opa_plugin_builder_name)
  in

  let deps (* Ocaml libs*) =
    let parent s =
      String.sub s 0 (String.rindex s '/')
    in
    Tags.fold
    (fun tag deps -> match use_tag tag with
     | None -> deps
     | Some lib ->
         match mlstate_lib_dir lib with
         | "." -> deps
         | dir -> ((parent dir)/lib^".cmxa") :: deps
    ) tags deps
  in

  let stamp = p_oppf in

  let name =
    match rule_name with
    | None -> name
    | Some n -> n in
  rule (Printf.sprintf "Opa plugin: %s" name)
    ~deps
    ~prods
    ~stamp
    (fun env build ->
       let opp_build =
         Cmd (S (opa_plugin_builder::options))
       in
       set_mlstatelibs ();
       (Seq (opp_build :: unset_mlstatelibs :: postlude))
    )
in

(** Script which generates the a file which contains the list of all Opa plugins
    name ([all_plugins_file}) *)
let make_all_plugins = stdlib_packages_dir/"all_plugins.sh" in

(** The all plugins file, list of all Opa plugins in stdlib *)
let all_plugins_file = stdlib_packages_dir/"all.node.plugins" in

(** This rule generates rules for all plugins *)
let node_plugins =
  let all_plugins_file = build_dir / all_plugins_file in
  Command.execute ~quiet:true ~pretend:false
    (Cmd (S[
	    Sh"mkdir"; A"-p"; P (build_dir / stdlib_packages_dir); Sh"&&";
	    P (source_dir/stdlib_packages_dir/"all_plugins.sh");
	    P (source_dir/stdlib_packages_dir);
	    Sh">"; P all_plugins_file;
	  ])) ;
  string_list_of_file all_plugins_file
in

let plugins_deps =
  List.map (
    fun f -> prefixed_plugins_dir/f/f -.- "opa_plugin"
  ) node_plugins in

let plugins =
  List.map (
    fun f -> prefixed_plugins_dir/f/f -.- "oppf"
  ) node_plugins in

(* -- end plugins -- *)


(* -- begin packages -- *)

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
  let aux_files pkdir =
    let opack = dir_ext_files "opack" (source_dir / pkdir) in
    let files = dir_ext_files "opa" (source_dir / pkdir) in
    let files = files @ opack in
    (* return relative filenames *)
    let files =
      let len = String.length source_dir + 1 in (* the additional '/' *)
      let relative_part s = String.sub s len (String.length s - len) in
      List.map relative_part files
    in
    (*
      When you compare 2 branches, if the order of opa sources is not deterministic, you can become crazy
    *)
    List.sort String.compare files
  in
  let pkdir = prefix_me ("lib" / package_to_dir pkg) in
  if not (Pathname.is_directory (source_dir / pkdir)) then
    let pkdir = "lib" / package_to_dir pkg in
    if not (Pathname.is_directory (source_dir / pkdir)) then
      let () = Printf.eprintf "Error: can not find sources for package %s (directory %s does not exist)\n" pkg pkdir in
      raise RuleFailure.E
    else aux_files pkdir
  else aux_files pkdir
in

let packages_exclude_node = "node.exclude" in
let make_all_packages = [
  stdlib_packages_dir/"all_packages.sh";
  stdlib_packages_dir/packages_exclude_node
] in
let all_packages_file = stdlib_packages_dir/"all.node.packages" in
(* FIXME: is not regenerated when a new package is added *)
let all_packages_building =
  let prod = all_packages_file in
  rule "all.node.packages"
    ~deps:make_all_packages
    ~prod
    (fun env build ->
         Cmd(S[
               P (source_dir/stdlib_packages_dir/"all_packages.sh");
	       P (source_dir/stdlib_packages_dir/packages_exclude_node);
	       P (source_dir/stdlib_packages_dir);
	       Sh">"; P (build_dir/prod);
             ])
    )
in

let packages_building ~name ~stamp ~core_only ~rebuild
    ~plugins
    ?(opacomp_stamp="opacomp.stamp")
    ?(all_packages_file=all_packages_file)
    ?(opx_dir="stdlib.qmljs")
    ?(backend_opt=[])
    ?(opacapi_validation=opacapi_validation)
    () =

  rule name
    ~deps:(plugins @ [
	     opacapi_validation;
	     version_buildinfos;
	     all_packages_file;
	     opacomp_stamp;
    ])
    ~stamp
  (fun env build ->
     try
       let packages = string_list_of_file all_packages_file in
       let packages =
         if core_only then
           let stdlib = "stdlib.core" in
           List.filter (
	     fun package ->
	       let stdlib_len = String.length stdlib in
               String.length package >= stdlib_len &&
                 stdlib = String.sub package 0 stdlib_len
	   ) packages
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
       let version = get_version_buildinfos () in
       let extra_opt = if rebuild then [A"--rebuild"] else [] in
       let extra_opt =
         [A"--opx-dir";A opx_dir;
          A"--package-version"; A version;
          A"--warn-error";A"root";
          A"--no-warn-error";A"coding.deprecated";
          A"--no-warn-error";A"load-opx";
          A"--no-warn-error";A"load-import";
          A"--no-warn-error";A"bsl.loading";
          A"--no-warn-error";A"bsl.projection";
          A"--warn"; A"jscompiler";
         ] @ backend_opt @ extra_opt
       in
       set_mlstatelibs();
       Seq[
         Echo(conf, "conf");
         Cmd(S([Sh"mkdir";A"-p";P opx_dir;]));
         Cmd(S([Sh"rm -rf";P (opx_dir/"*.opp")]));
         Cmd(S([get_tool "opa-bin";
                A"--autocompile";
                (* A"--verbose-build"; *)
                A"--conf";P "conf";
                A"--slicer-check"; A "low";
                A"--project-root"; P (source_dir/opalang_prefix); (* because the @static_resource in the stdlib expect this *)
                A"--no-stdlib";
                A"-I"; A prefixed_plugins_dir;
                opaopt;
                S all_files;
               ] @ extra_opt @ stdlib_parser_options));
         unset_mlstatelibs
       ]
     with RuleFailure.E ->
       fail_rule build
  ) in

rule "register qmljs opabsl plugin rule"
  ~deps:[prefixed_plugins_dir/"opabsl"/"opabsl" -.- "opa_plugin"]
  ~stamp:"opabsl.qmljs.stamp"
  (fun env build ->
     plugin_building prefixed_plugins_dir "opabsl";
     Nop
  );

rule "register qmljs other plugins rule"
  ~deps:plugins_deps
  ~stamp:"plugins.qmljs.stamp"
  (fun env build ->
     List.iter (plugin_building prefixed_plugins_dir) node_plugins;
     Nop
  );

packages_building
  ~name:"opa-node-packages: meta-rule to build the node stdlib and .opx"
  ~stamp:"opa-node-packages.stamp"
  ~core_only:false
  ~rebuild:false
  ~plugins
  ();

(* -- end packages -- *)


(* -- begin opa-create -- *)

let opa_create_prefix = prefix_me "tools/opa-create/src/opa-create" in
let opa_create_src = opa_create_prefix ^ ".opa" in
let opa_create_dst = opa_create_prefix ^ ".exe" in

let dir_all_files dir =
  List.filter (fun p -> (not (Pathname.is_directory p))) (dirlist dir)
in

let dir_rec_all_files dir =
  let dirs =  rec_subdirs [ dir ] in
  List.fold_right (fun dir acc -> dir_all_files dir @ acc) dirs []
in

let opa_create name stamp deps ?(more_app_opts=[]) app_opx_dir =
rule name
  ~deps:(deps @ tool_deps "opa-bin" @ dir_rec_all_files (prefix_me "tools/opa-create"))
  ~stamp
  ~prods: [opa_create_dst]
  (fun env build ->
     if no_tools then Nop
     else (
    set_mlstatelibs ();
    Seq [
      Cmd(S([
	      get_tool "opa-bin";
              A"-o"; P opa_create_dst; P opa_create_src;
              A"--opx-dir"; A app_opx_dir;
              A"--no-server";
              A"--project-root"; P (source_dir/opalang_prefix); (* because the @static_resource in the stdlib expect this *)
              A"-I"; A prefixed_plugins_dir
	    ] @ more_app_opts));
      unset_mlstatelibs
    ]
    )
  )

in
opa_create "opa node application creator" "qmljs.opa.create" ["opabsl.qmljs.stamp"; "plugins.qmljs.stamp"] "stdlib.qmljs";

(* -- end opa-create -- *)

(* -- begin misc -- *)

rule "opa bash_completion: opa-bin -> bash_completion"
  ~deps: (tool_deps "opa-bin")
  ~prod: "bash_completion"
  (fun env build ->
     Seq[Cmd(S[get_tool "opa-bin"; A"--bash-completion"])]);

(* -- end misc -- *)

(* -- begin opx2js -- *)

rule "node-packages"
  ~deps:["opa-node-packages.stamp"]
  ~stamp:"node-packages.stamp"
  (fun env build ->
     let packages = string_list_of_file all_packages_file in
     set_mlstatelibs();
     Seq [
     Cmd(S([get_tool "opx2js-bin";
            A"--build-dir";A"test"
           ] @ List.map (fun p -> A p) packages));
     unset_mlstatelibs
     ];
  );

(* -- end opx2js -- *)

() (* This file should be an expr of type unit *)
