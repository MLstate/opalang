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
open Ocamlbuild_plugin

open Command


(**
   {6 Portability utility functions }
*)

let mlstate_platform = "mlstate_platform"
let is_mac = Config.os = Config.Mac
let is_fbsd = Config.os = Config.FreeBSD

let sed = if is_mac  then P"gsed" else P"sed"
let md5 = if is_fbsd then P"md5"  else P"md5sum"

(**
 tools for which we call the windows version (and that need some call translation)
*)

let windows_mode = (os = Win32)

let c_wall,c_werror =
  if windows_mode (*&& compiler=microsoft*) then "/Wall","/Wall"
  else if is_mac then "-Wall","-Wall"
  else "-Wall","-Werror"

let winocamldir   = Pathname.pwd /  "ms_windows"

let wingate s     = winocamldir / "windows_gate" ^ " " ^ s
let windows_opa   = winocamldir / "windows_opa"
let winocamlc     = winocamldir / "windows_ocamlc"
let winocamlfind  = winocamldir / "windows_ocamlfind"
let winocamlmklib = winocamldir / "windows_ocamlmklib"
let winocamlmktop = winocamldir / "windows_ocamlmktop"
let winocamldoc   = winocamldir / "windows_ocamldoc"

let as_wintools = [ ("trx", "") ; ("bslregister", "") ; ("opa.exe", windows_opa) ]

let _ = if windows_mode then Printf.printf "MYOCAMLBUILD WINDOWS MODE\n" else ()

let _ =
  Options.ext_lib := Config.ext_lib;
  Options.ext_obj := Config.ext_obj;
  Options.ext_dll := Config.ext_shared

let _ = if windows_mode then (
  Options.ocamlmklib := P winocamlmklib;
  Options.ocamlmktop := P winocamlmktop;
  Options.ocamldoc := P winocamldoc
) else ()

let run_and_read s =
  let output = run_and_read s in
  String.sub output 0 (String.length output - 1) (* Remove trailing \n *)

let debug_getenv_toggle var = try Sys.getenv var = "1" with Not_found -> false

(**
   {6 Configure TREX}
*)

let rec trx_deps dep prod env build =
  Cmd(S[A"grep"; A"^\\s*\\(read\\|include\\)"; P(env dep);
        Sh("| perl -p -e 's/^ *(read|include) +([^ ]+).*/\\2/'");
        Sh("> "^(env prod))])

let proto_deps dep prod env build =
  Cmd(S[A"grep"; A"^-include"; P(env dep);
        Sh("| perl -p -e 's/^ *([-]include) +\"([^ ]+)\".*/\\2/'");
        Sh("> "^(env prod))])

let build_list build targets = List.iter Outcome.ignore_good (build (List.map (fun f -> [f]) targets))

let mlstatelibs = try Sys.getenv "MLSTATELIBS" with
    Not_found -> Pathname.pwd / !Options.build_dir

type tool_type = Internal of Pathname.t | External of Pathname.t
let tools_table = (Hashtbl.create 17: (string, tool_type) Hashtbl.t)

let set_tool ~internal ?(env="") name f =
  let tool =
    if internal then Internal f
    else External (if env = "" then mlstatelibs/f else try Sys.getenv env with Not_found -> mlstatelibs/f)
  in Hashtbl.add tools_table name tool

let tool_deps name =
  try
    (match Hashtbl.find tools_table name with
       | Internal f -> [f]
       | External f -> [])
  with Not_found -> failwith ("Build tool not found (tool_deps) : "^name)

let get_tool ?local:(local=false) name =
  try
    (match Hashtbl.find tools_table name with
      | Internal f ->
        let f = if local then f else Pathname.pwd / !Options.build_dir / f in
        let wintools_b, winf = try
          let c = List.assoc name as_wintools in
          true, (if c <> "" then c else (wingate f))
          with Not_found -> false, ""
        in
        if windows_mode && wintools_b then (Sh winf) else (P f)
      | External f -> P f)
  with Not_found -> failwith ("Build tool not found: "^name)

(* Installs the tool forcibly in _build *)
let force_copy_tool t =
  List.iter
    (fun x ->
       let dir = Pathname.pwd / !Options.build_dir / Pathname.dirname x in
       Command.execute
         (Seq [ Cmd(S[P"mkdir"; A"-p"; P dir]);
                cp x dir;
                chmod (A "a+x") (dir / Pathname.basename x) ]))
    (tool_deps t)

let trx_build_aux ~just_binary src dst ext ops = fun env build ->
  let trx_tool = "trx" in
  let transitive_deps build f =
    let rec aux = function
      | [] -> ()
      | (_, []) :: todo -> aux todo
      | (path, f :: rest) :: todo ->
          if List.mem f path then
            failwith (Printf.sprintf "Circular build detected (%s already seen in [%s])"
                        f (String.concat "; " path))
          else
            let prefix = let p = Pathname.to_string (Pathname.dirname f) in if p = "." then "" else p^"/" in
            let deps = List.map ((^) prefix) (string_list_of_file (f -.- "depends")) in
            let dep_files = List.map (fun d -> d -.- "depends") deps in
            build_list build dep_files;
            build_list build deps;
            aux (((f :: path), deps) :: (path, rest) :: todo) in
    aux [[], [f]]
  in
  let trx_cache_tag = Tags.does_match (tags_of_pathname (env src)) (Tags.of_list ["trx_cache"]) in
  let do_build () =
    transitive_deps build (env src);
    let trxexec = get_tool trx_tool in
    let command =
      if just_binary then
        [ trxexec; ops;
          P(Pathname.basename (env src));
          T(tags_of_pathname (env src));
          Sh("-binary "^(Pathname.basename (env dst)))
        ]
      else
        let fn_src = Pathname.basename (env src) in
        let fn_dst = Pathname.basename (env dst) in
        [ trxexec; ops;
          P(fn_src);
          S[A"--output-basename"; P(Filename.chop_extension fn_dst)];
          T(tags_of_pathname (env src))
        ]
    in
    Seq((match ext with
         | "ml" ->
             [Cmd(S([Sh("cd " ^ Pathname.dirname (env src) ^ " &&")]
                    @ command ))]
         | "mli" -> [] (* the compilation of the ml is supposed to have generated the .mli already *)
         | _ -> assert false)
        @ (if trx_cache_tag
           then [Cmd(S[Sh"cp ";P(env dst);P(env dst -.- "cache")]);
                 Cmd(S[Sh"cp ";P(env dst);P(Pathname.pwd / env dst -.- "cache")])]
           else []))
  in
  let get_tool_trx_file () =
    match Hashtbl.find tools_table trx_tool with Internal f -> f | _ -> assert false
  in
  match Outcome.wrap build [tool_deps trx_tool] with
    | Outcome.Good _ -> do_build ()
    | Outcome.Bad _ ->
        let cache = Pathname.pwd / env dst -.- "cache" in
        if Sys.file_exists (get_tool_trx_file ())
        then do_build ()
        else if trx_cache_tag && Sys.file_exists cache
        then (
          Ocamlbuild_pack.Log.dprintf 1 "Using cached file (%s) to bootstrap trx" (env dst -.- "cache");
          cp cache (env dst)
        )
        else
          failwith (Printf.sprintf "No cache, no trx exe: sorry, cannot build %s from %s.\nYou may want to re-run with TRX_OVERRIDE set" (env dst) (env src))

let trx_build ?(just_binary = false) pattern cmd =
  let src = "%.trx" in
  let exts = ["ml"; "mli"] in
  fun env build ->
    let combine acc ext = Seq [acc; trx_build_aux ~just_binary src (pattern ^ "." ^ ext) ext cmd env build] in
    List.fold_left combine Nop exts

let dirlist dir =
  let searchdir = if Pathname.is_relative dir then Pathname.pwd / dir else dir in
  List.map (fun d -> dir/d) (Array.to_list (Pathname.readdir searchdir))

let dir_ext_files ext dir =
  List.filter (fun p ->
    let b = Filename.basename p in
    (* emacs tmp files: in case you are editing files during a mkinstall *)
    (not (String.length b >= 2 && b.[0] = '.' && b.[1] = '#')) &&
      Pathname.check_extension b ext) (dirlist dir)

let path_to_list s =
  let l = String.length s in
  let rec aux r count pos =
    if pos < 0 then
      if count = 0 then r
      else String.sub s 0 count :: r
    else
      if String.unsafe_get s pos = '/' then
        let r = if count = 0 then r else
          String.sub s (succ pos) count :: r in
        aux r 0 (pred pos)
      else
        aux r (succ count) (pred pos)
  in
  aux [] 0 (pred l)

(* Removes /../ and /./ from the path if possible *)
let flatten_path s =
   let p = path_to_list s in
   let rec aux = function
     | "."::r -> aux r
     | a::".."::r -> aux r
     | a::r -> a :: aux r
     | [] -> []
   in
   String.concat "/" (aux p)

(* A hack to tell ocamlbuild that the current rule fails and to try the next
   one *)
let fail_rule build =
  ignore (List.map Outcome.good (build [["__failing_rule__"]]));
  Nop

let _ = dispatch begin function
  | After_rules ->

      (* ------------------------------------------------------------ *)
      (* Library definitions                                          *)
      (* ------------------------------------------------------------ *)

      (* Workaround missing stuff in ocamlbuild *)

      (* ocamlbuild doesn't offer the option to customise what is called for camlp4o *)
      flag ["ocaml"; "pp"; "camlp4o_fixed"] (P Config.camlp4o);
      flag ["ocaml"; "pp"; "camlp4orf_fixed"] (P Config.camlp4orf);

      let ocaml_lib ?(extern=false) ?dir ?tag_name lib =
        let tag_name = match tag_name with None -> "use_"^lib | Some n -> n in
        ocaml_lib ~extern ?dir ~tag_name lib;
        match dir with Some dir -> flag ["use_"^lib; "doc"] (S[A"-I"; P dir])
        | None -> ()
      in

      (* OCaml libs *)
      ocaml_lib ~extern:true ?dir:Config.Libdir.camlzip ~tag_name:"use_zip" Config.Libdir.camlzip_name;
      ocaml_lib ~extern:true ?dir:Config.Libdir.graph "graph";
      ocaml_lib ~extern:true ?dir:Config.Libdir.ssl "ssl";
      ocaml_lib ~extern:true ?dir:Config.Libdir.cryptokit "cryptokit";
      ocaml_lib ~extern:true ?dir:Config.Libdir.ulex ~tag_name:"use_ulex" "ulexing";
      ocaml_lib ~extern:true ~dir:"+mascot" "mascot";
      if Config.has_dbm then
        ocaml_lib ~extern:true ~dir:"+dbm" ~tag_name:"opt_use_dbm" "dbm";

      (* MLstate libs *)
      let mlstate_lib, internal_lib, mlstate_lib_deps, mlstate_lib_dir =
        let mlstate_libs_table = Hashtbl.create 17 in
        let mlstate_lib ?dir lib =
          let dir = match dir with None -> lib | Some dir -> dir in
          Hashtbl.add mlstate_libs_table lib ([],dir);
          ocaml_lib ~extern:true ~dir:(mlstatelibs / dir) lib
        in
        let internal_lib ?dir lib =
          let dir = match dir with None -> lib | Some dir -> dir in
          Hashtbl.add mlstate_libs_table lib ([lib],dir);
          ocaml_lib ~dir lib;
          dep ["use_"^lib; "byte"] [lib^".cma"];
          dep ["use_"^lib; "native"] [lib^".cmxa"]
          (* How this works: the ~dir is only the top-level of the lib, hence
             the .cmi from sub-directories are not seen. That's good, use them
             for modules internal to your library *)
        in
        let mlstate_lib_deps l =
          try fst (Hashtbl.find mlstate_libs_table l) (* returned without extension *)
          with Not_found -> prerr_endline ("WARNING: lib "^l^" not found");
            [l]
        in
        let mlstate_lib_dir l =
          try snd (Hashtbl.find mlstate_libs_table l) (* returned without extension *)
          with Not_found -> prerr_endline ("WARNING: lib "^l^" not found"); "."
        in
        mlstate_lib, internal_lib, mlstate_lib_deps, mlstate_lib_dir
      in


      (* ------------------------------------------------------------ *)
      (* Additional rules                                             *)
      (* ------------------------------------------------------------ *)

      (* Preprocessed .mllib *)
      rule "Preprocessed mllib: mllibp -> mllib"
        ~dep:"%.mllibp"
        ~prod:"%.mllib"
        (fun env _build ->
           let tags = String.uppercase (String.concat "|" Config.available) in
           let sedexpr =
             Printf.sprintf "s/^\\?HAS_(%s)://; /HAS_.*:/d" tags
           in
           Cmd(S[sed; A"-r"; A sedexpr; P(env "%.mllibp"); Sh">"; P(env "%.mllib")]));

      (* Windows specific : redefinition of an existing rule in Ocaml_specific.ml,
         Louis please have a look to avoid the two copies at the end
         we cheat using prods from non windows rule and making copies to generate
         the (ocamlbuild) wanted targets
      *)
      if windows_mode then
      begin
      (* since we are in cygwin in the end, we need to select extensions by hand *)
      let ext_lib = "a" in
      let ext_dll = "so" in
      rule "WIN ocaml C stubs: clib & (o|obj)* -> (a|lib) & (so|dll)"
        ~prods:["%(path:<**/>)lib%(libname:<*> and not <*.*>)"-.-ext_lib
                ;"%(path:<**/>)dll%(libname:<*> and not <*.*>)"-.-ext_dll]
       ~dep:"%(path)lib%(libname).clib"
        ~insert:`top
        (
         (* A verbatim from Ocaml_specific.ml private part *)
         fun env build ->
         let module C_tools = struct
           open Outcome
           type ('a,'b) t = ('a,'b) Outcome.t
           let link_C_library clib a libname env build =
             let clib = env clib and a = env a and libname = env libname in
             let objs = string_list_of_file clib in
             let include_dirs = Pathname.include_dirs_of (Pathname.dirname a) in
             let obj_of_o x =
             if Filename.check_suffix x ".o" && !Options.ext_obj <> "o" then
                Pathname.update_extension !Options.ext_obj x
             else x in
             let resluts = build (List.map (fun o -> List.map (fun dir -> dir / obj_of_o o) include_dirs) objs) in
             let objs = List.map begin function
             | Good o -> o
             | Bad exn -> raise exn
                end resluts
           in
           Printf.printf "I AM A STUPID RULE THAT SHOULD GENERATE %s and %s from %s\n" ((env "%(path:<**/>)lib%(libname:<*> and not <*.*>)")-.-ext_lib)((env "%(path:<**/>)lib%(libname:<*> and not <*.*>)")-.-ext_dll) (env "%(path)lib%(libname).clib");
           Seq ([
             Cmd(S[!Options.ocamlmklib; A"-oc"; Px libname; T(tags_of_pathname a++"c"++"ocamlmklib"); atomize objs]);
             cp (env "%(path)lib%(libname).lib") (env "%(path)lib%(libname).a");
           ] @ (if Sys.file_exists (env "%(path)dll%(libname).dll") then [cp (env "%(path)dll%(libname).dll") (env "%(path)dll%(libname).so")] else []))
           end
        in
        C_tools.link_C_library "%(path)lib%(libname).clib" ("%(path)lib%(libname)"-.-ext_lib) "%(path)%(libname)" env build
        );
      end else ();

      rule "Library doc: mllib -> odocl"
        ~dep: "%.mllib"
        ~prod: "%.odocl"
        (fun env build -> cp (env "%.mllib") (env "%.odocl"));

      (* -- C rules -- *)

      (*
       take care of #include  "..."
       it is needed for generating mlstate_platform.h and futur headers
      *)
      rule "c_deps: c -> c.depends"
        ~dep: "%.c"
        ~prod: "%.c.depends"
        (fun env build ->
           Cmd(S[P"perl"; A"-n"; A"-e";
                 A("if (/^ *(#include) +\"(\\.\\.\\/)*([^\"]+)\".*/) { print \"$3\\n\"; }");
                 P(env "%.c"); Sh">"; P(env "%.c.depends")]));

      rule "cc: c & c.depends -> o"
        ~deps: ["%.c";"%.c.depends"]
        ~prod: ("%"-.- !Options.ext_obj)
        ~insert:`top
        (fun env build ->
             let c = env "%.c" in
             let o = env ("%"-.- !Options.ext_obj) in
             let hs = string_list_of_file (env "%.c.depends") in
             (* try to compile the deps either at the root of the repo, or in the same dir as src *)
             List.iter Outcome.ignore_good (build (List.map (fun f -> [f; Pathname.dirname (env "%.c")/f]) hs));
             let cc = Cmd(S[ !Options.ocamlc; T(tags_of_pathname c++"c"++"compile"); A"-c"; Px c]) in
             if Pathname.dirname o = Pathname.current_dir_name
             then cc
             else Seq[cc; mv (Pathname.basename o) o]
         );


      (* -- TRX rules -- *)

      rule "trx_deps: trx -> trx.depends"
        ~dep: "%.trx"
        ~prod: "%.trx.depends"
        (trx_deps "%.trx" "%.trx.depends");

      rule "trx_functor: trx & trx.depends -> _functor.ml"
        ~deps:["%.trx"; "%.trx.depends"]
        ~prods:["%_functor.ml"; "%_functor.mli"]
        (trx_build "%_functor" (S[A"--functorize"]));

      rule "trx_auto: trx & trx.depends -> _auto.ml"
        ~deps:["%.trx"; "%.trx.depends"]
        ~prods:["%_auto.ml"; "%_auto.mli"]
        (trx_build "%_auto" (S[A"--auto-ast"; A"--functorize"]));

      rule "trx: trx & trx.depends -> ml"
        ~deps:["%.trx"; "%.trx.depends"]
        ~prods:["%.ml"; "%.mli"]
        (trx_build "%" N);


      (* -- Macro-rules generating ML files -- *)

      (* aspell custom dictionnary *)
      rule "aspell individual word list"
        ~dep:"%.aspell"
        ~prod:"%.aspell.dict"
        (fun env build ->
           (* aspell --lang=en create master ./opa < dico *)
           Cmd (S [
                  Sh "aspell" ; A"--lang=en" ; A"create" ; A"master" ; A("./"^(env "%.aspell.dict")) ;
                  Sh"<"; P (env "%.aspell")
                ])
        );

      (* -- Specific BSL rules -- *)

      let dir_sources_bsl ?(prefix="") ?(suffix="") d =
        let env_set s = try Sys.getenv ("MLSTATE_"^s) <> "0" with Not_found-> false in
        let append_vars = [] in
        (* Optionally loaded bsl-sources triggered by env vars: you can add a
           bsl-sources.MY_VAR / bsl-sources.NO.MY_VAR file alongside bsl-sources *)
        List.map (fun s -> d/(prefix^s^suffix))
          (List.fold_left
             (fun acc var ->
                if env_set var then
                  if Sys.file_exists (d/"bsl-sources."^var)
                  then acc @ string_list_of_file (d/"bsl-sources."^var)
                  else acc
                else
                  if Sys.file_exists (d/"bsl-sources.NO."^var)
                  then acc @ string_list_of_file (d/"bsl-sources.NO."^var)
                  else  acc)
             (string_list_of_file (d/"bsl-sources"))
             append_vars)
      in
      let special_bsl_options =
        let fold acc (s, a) =
          let cond =
            (List.mem s !Options.tags) ||
              (try
                 let _ = Sys.getenv (String.uppercase s) in true
               with
                 | Not_found -> false
              ) in
          (if cond then (A a)::acc else acc) in
        List.fold_left fold [A"--js-validator-off"] (* until bsl is a standard plugin *)
          [
            ("bsl_debug", "-debug");
            ("bsl_unsafe", "-unsafe") ;
            ("bsl_no_js_validator", "--js-validator-off");
          ]
      in

      (* ------------------------------------------------------------ *)
      (* Tags, flags, directory contexts and custom stuff             *)
      (* ------------------------------------------------------------ *)

      (* fixes for buggy and missing rules in ocamlbuild *)
      flag ["ocaml"; "extension:top"; "thread"] (S[A"-thread";P"threads.cma";A"-cclib";A"-lthreads"]);
      flag ["ocaml"; "pack"; "rectypes"] (A"-rectypes");
      flag ["ocaml"; "doc"; "rectypes"] (A"-rectypes");
      flag ["ocaml"; "compile"; "noassert"] (A"-noassert");
      flag ["extension:c"; "compile"; "c_wall"] (S[A"-ccopt";A c_wall;A"-ccopt";A c_werror]);


      (* PB WITH libcrypto.obj MISSING ??? *)
      if windows_mode then (
        (* openssl *)
          let flags = S[A"-I";A "/windows_libs/openssl/include";
                      A"-I";A "/windows_libs/openssl/lib"] in
          flag ["use_ssl"; "compile"] flags;
          flag ["use_ssl"; "link"] flags;

        (* windows primitives *)
          flag ["ocaml"; "link"] (S [A "Gdi32.lib"; A "User32.lib"]);

        (* zlib *)
          let flags = S[A"-I";A "/windows_libs/zlib/";
                        A"-ccopt";A "-L/windows_libs/zlib/"  ] in
          flag ["use_zip"; "compile"] flags;
          flag ["use_zip"; "link"] flags;

        (* dns *)
          let flags = S[A"-ccopt";A"DnsAPI.Lib"] in
          flag ["use_io";"link"] flags
      );

      if is_mac then (
	flag ["ocaml"; "link"; "iconv"] (S[A"-cclib";A"-liconv"]);
      );

      (* for C #includes *)
      flag ["extension:c"] (S[A"-I";P (Pathname.pwd / !Options.build_dir)]);

      (* -- Directory contexts -- *)

      (* This enables us to tell ocamlbuild that in the supplied directories,
         all subdirs can see each other *)
      let subdirs dir = List.filter Pathname.is_directory (dirlist dir) in
      let rec rec_subdirs dlist =
        List.fold_left
          (fun acc d -> let sub = subdirs d in sub @ rec_subdirs sub @ acc) [] dlist in
      let set_common_context dirlist =
        List.iter (fun dir -> Pathname.define_context dir dirlist) dirlist in
      let shared_namespace_dir dir = set_common_context (dir::rec_subdirs [dir]) in
      let include_subdirs dir = Pathname.define_context dir (dir::subdirs dir) in

      (* -- Stubs -- *)

      let def_stubs ~dir name =
        let tag = "use_"^name in
        let file = dir / "lib" ^ name -.- !Options.ext_lib in
        dep ["ocaml"; tag] [file];
        flag ["ocaml"; "byte"; "link"; tag] (S[A"-ccopt";A("-L"^dir);A"-cclib";A("-l"^name);A"-custom"]);
        flag ["ocaml"; "native"; "link"; tag] (S[A"-ccopt";A("-L"^dir);A"-cclib";A("-l"^name)]);
      in

      if is_fbsd then (
        (* In the memory.c in FreeBSD part that uses kvm_getprocs() required
           link with -lkvm. *)
        flag ["use_stubs"; "link"] (S[A "-cclib";A "-lkvm"]);
        (* Build with converters/libiconv port, which it installs in the
           /usr/local by default *)
        flag ["iconv"; "compile"] (S[A"-I";A "/usr/local/include"]);
        flag ["iconv"; "link"] (S[A"-ccopt";A "-L/usr/local/lib";A "-cclib";A "-liconv"]);
      );

(* -- Don't forget that the rest of the "mlstate build stdlib" is in --
   -- myocamlbuild_suffix.ml. The rest comes from the build_rules*.ml in each repo -- *)
