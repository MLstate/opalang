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

(**
   Common library for Qml to OCaml compiler.
   @author Mathieu Barbin
*)

(* depends *)
module Char = Base.Char
module Hashtbl = Base.Hashtbl
module List = Base.List
module String = Base.String

(* shorthands *)
module O = Qml2ocamlOptions

(* -- *)

(** {6 Exported Types} *)
type dynloader = BslPluginInterface.plugin -> unit

type env_qml_code =
    {
      init_code : QmlAst.code option ;
      user_code : QmlAst.code ;
    }

type env_blender = QmlBlender.qml_milkshake

type 'a env_ocaml = {
  ocaml_init_contents : (string * string) list ; (** wanted name + direct ocaml generated as string,deps *)
  ocaml_code : 'a ;
}

type env_ocaml_input = Ocaml.code env_ocaml

(**
   + [split]
   The name of the file and the code.
   For each splitten file.

   + [depends]
   The complete dependencies of files in the current packages.
   From basename to basename.
*)
type env_ocaml_split = {
  split : (string * Ocaml.code) list env_ocaml ;
  depends : (string * string list) list ;
}

type env_ocaml_output = {
  generated_files : string list ; (** path/name without build directory *)
  caller_wd : string; (** absolute path of the caller working directory *)
}


(** New : back end specific traduction from QmlAst.ast to Ocaml.code
    This is a record not a module (simplification for dynamically choices)
    This can be used
*)
type ('bymap, 'env, 'code) back_end_factory =
    {
      build_bymap : ?filter:(BslKey.t -> bool) -> Qml2ocamlOptions.argv_options -> BslLib.BSL.ByPassMap.t -> 'bymap ; (** will build its own specific map (later uses export in libBSL) *)
      ocaml_init : 'bymap -> string; (** contents *)
      env_initial : Qml2ocamlOptions.argv_options -> 'bymap -> QmlTyper.env -> 'env ;
      empty_code : 'code ;
      compile : ('env * 'code) -> QmlAst.code -> 'env * 'code ;
      get_code : 'code -> Ocaml.code ;
    }

type qml_to_ocaml = Qml2ocamlOptions.argv_options -> BslLib.env_bsl -> env_blender -> env_ocaml_input

(** We use this type for the sugar, until we use export function in BslLib *)
type back_end =
    {
      dynloader : dynloader ;
      qml_to_ocaml : qml_to_ocaml
    }

let bsl_init_basename () =
  match ObjectFiles.compilation_mode () with
  | `init -> "bsl_init"
  | `compilation | `linking | `prelude -> "bsl_init_" ^ ObjectFiles.get_current_package_name_as_a_valid_ident ~will_be_prefixed:true ()

let bsl_init_module () =
  OcamlUtils.Module.of_filename (bsl_init_basename ())

(**
   PASSES of qml2ocaml :
   ---------------------
   // Command line passes
   returns a : env_bsl, env_blender

     << qml_to_ocaml : argv_options -> env_bsl -> env_blender -> env_ocaml_input >>

   // End passes
   val ocaml_generation : argv_options -> env_ocaml_input -> env_ocaml_output
   val ocaml_compilation : argv_options -> env_ocaml_output -> int

   NEEDED from any instance of a qml-compiler :
   val qml_to_ocaml : qml_to_ocaml

   Since the 2ocaml compilers are similar, we propose a factory function
   see qml_to_ocaml_factory : ('a, 'b, 'c) back_end_factory -> qml_to_ocaml
*)

(* ============================================================================== *)

let command options s =
  if options.O.show_compilation then prerr_endline s ;
  Sys.command s

let command_raise options s =
  let result = command options s in
  if result <> 0 then OManager.error "external error -- the command was :@\n@\n%s" s

(* ============================================================================== *)

let qml_to_ocaml_factory : ('a, 'b, 'c) back_end_factory -> qml_to_ocaml =
  fun compiler options bsl blender ->
    OManager.verbose "bsl: projection of bypass code";
    let qml_code = blender.QmlBlender.code in
    (* projecting only the necessary bypasses *)
    let used_bypasses =
      let bypass_of_directive = function
        | `assert_ -> Some "bslpervasives.assertion"
        | `fail | `throw | `catch -> Some "bslpervasives.fail"
        | `callcc -> Some "bslcps.notcps_compatibility.callcc_directive"
        | `thread_context -> Some "bslcps.notcps_compatibility.thread_context"
        | `with_thread_context -> Some "bslcps.notcps_compatibility.with_thread_context"
        | _ -> None in
      QmlAstWalk.CodeExpr.fold
        (QmlAstWalk.Expr.fold
           (fun set -> function
            | QmlAst.Bypass (_, k) -> BslKeySet.add k set
            | QmlAst.Directive (_, v, _, _) ->
                (match bypass_of_directive v with
                 | Some s -> BslKeySet.add (BslKey.normalize s) set
                 | None -> set)
            | _ -> set)) BslKeySet.empty qml_code in
    let bymap = compiler.build_bymap ~filter:(fun k -> BslKeySet.mem k used_bypasses) options bsl.BslLib.bymap in
    let ocaml_init = compiler.ocaml_init bymap in
    (** Ocaml Init (projection of lib init) *)
    let file = (bsl_init_basename ()) ^ ".ml" in
    let ocaml_env, ocaml_code = compiler.env_initial options bymap (blender.QmlBlender.env : QmlTyper.env), compiler.empty_code in
    OManager.verbose "ocaml traduction";
    let _, ocaml_code = compiler.compile (ocaml_env, ocaml_code) qml_code in
    let ocaml_code = compiler.get_code ocaml_code in
    {
      ocaml_init_contents = [ file, ocaml_init ] ;
      ocaml_code = ocaml_code
    }

(* ============================================================================== *)

module OcamlCompilation :
sig

  (**
     Splitting the code into smaller ocaml modules.
     Each identifier is prefixed with the absolute module path of its definition.
     This pass is compositionnal, it loads and saves corresponding environments.
  *)
  val ocaml_split_code : Qml2ocamlOptions.argv_options -> env_ocaml_input -> env_ocaml_split

  (**
     Outputs on the disk the ml files corresponding to ocaml modules.
     Outputs also a previously computed .depends file.
  *)
  val ocaml_generation : Qml2ocamlOptions.argv_options -> env_ocaml_split -> env_ocaml_output

  val ocaml_compilation : Qml2ocamlOptions.argv_options -> env_ocaml_output -> int
end =
struct

  let tuple_pat_name = function
    |Ocaml.Pat(Ocaml.PatVar s),_ -> Some s
    | _ -> None

  let get_decl_name = function
    |Ocaml.Let    (l)
    |Ocaml.Letrec (l)  -> List.filter_map tuple_pat_name l
    | _ -> []

  (* ident -> module name of definition *)
  type ocaml_split_code = string IdentTable.t
  module OcamlSplitCode = ObjectFiles.Make(
    struct
      type t = ocaml_split_code
      let pass = "OcamlSplitCode"
      let pp f _ = Format.pp_print_string f "<dummy>"
    end)

  let ocaml_split_code options env_ocaml_input =
    let ocaml_code = env_ocaml_input.ocaml_code in

    (* Generation of module names *)
    let next_filename setf index =
      let target =
        (* the name of the ml files from different packages and for the linking
         * should never be able conflict *)
        match ObjectFiles.compilation_mode () with
        | `compilation ->
            let target = ObjectFiles.get_current_package_name_as_a_valid_ident ~will_be_prefixed:true () in
            Printf.sprintf "opa_%s_%.3d" target index
        | `init ->
            let target = File.from_pattern "%b" options.O.target in
            Printf.sprintf "init_%s_%.3d" target index
        | `linking | `prelude ->
            let target = File.from_pattern "%b" options.O.target in
            Printf.sprintf "link_%s_%.3d" target index
      in
      (* modification of the name, because Ocaml modules names are strict *)
      let target = (String.map (fun c -> if Char.is_alpha c || Char.is_digit c then c else '_') target ) in
      if StringSet.mem target setf then Printf.sprintf "collide_%d_%s" index target else target
    in
    (*
      For a stable split in case of modification.
    *)
    (*let split_number = options.O.split_ocaml_value in
    let magic_synchro s = Hashtbl.hash s mod split_number == 0 in*)
    let magic_synchro =
      let r = ref 0 in
      fun _ -> if !r = 300 then (r := 0; true) else (incr r; false) in
    let split_randomly_with_sync code =
      let rec aux acc l =
        match l with
        | x:: rl ->
            if magic_synchro (String.concat_map "" Ident.stident (get_decl_name x))
            then (List.rev (x::acc), rl)
            else aux (x::acc) rl
        | [] -> (List.rev acc, [])
      in
      aux [] code
    in

    (* compositionality *)
    let fileapi_all = IdentTable.create 1024 in
    let () =
      let iter t =
        let add id m = IdentTable.add fileapi_all id m in
        IdentTable.iter add t in
      OcamlSplitCode.iter iter
    in
    let fileapi_this = IdentTable.create 1024 in
    (* -- *)

    (* depends *)
    let static_depends = ListHashtbl.create 1024 in
    (* -- *)

    let split_files code =
      let rec aux index setf rev_files code =
        match code with
        | [] -> List.rev rev_files
        | _ ->
            let code, rest = split_randomly_with_sync code in

            let filename = next_filename setf index in
            let module_ = String.capitalize filename in
            let index = succ index in
            let setf = StringSet.add filename setf in

            (*
              dependencies to the bsl init file cannot be determinated easily,
              and we do not care actually. Each file can depends on bsl_init.
            *)
            let add_depends =
              if options.O.compile_via_makefile
              then (
                ListHashtbl.add static_depends filename (bsl_init_basename()) ;
                (fun module_ ->
                   let depends = String.uncapitalize module_ in
                   if StringSet.mem depends setf
                   then
                     ListHashtbl.add static_depends filename depends
                )
              )
              else ignore
            in

            (*
              Resolve dependencies:
              1) add complete path, instead of producing open
              2) collect dependencies for the build system.
                 only the depends in the same package are keeped.
            *)
            let map_ident ids =
              match ids with
              | [id] -> (
                  match IdentTable.find_opt fileapi_all id with
                  | Some module_ ->
                      add_depends module_ ;
                      [ Ident.source module_ ; id ]
                  | _ -> ids
                )
              | _ -> ids
            in
            let map_pe pe =
              match pe with
              | Ocaml.Pated (ident, b) ->
                  let fident = map_ident ident in
                  if fident == ident
                  then pe
                  else Ocaml.Pated (fident, b)
              | _ -> pe
            in
            let map_expr = OcamlWalk.Expr.map (
              fun e ->
                match e with
                | Ocaml.Var pe ->
                    let fpe = map_pe pe in
                    if pe == fpe
                    then e
                    else
                      Ocaml.Var fpe
                | _ -> e
            )
            in
            let code = List.map_stable map_expr code in

            let () =
              (* Collect definitions, and store it for a future compilation *)
              let iter_bindings = function
                | Ocaml.Pat (Ocaml.PatVar id), _ -> (
                    IdentTable.add fileapi_all id module_ ;
                    IdentTable.add fileapi_this id module_ ;
                  )
                | _ -> () in
              let iter = function
                | Ocaml.Let l
                | Ocaml.Letrec l -> List.iter iter_bindings l
                | _ -> () in
              List.iter iter code
            in
            let filename = filename^".ml" in
            let file = filename, code in
            let rev_files = file :: rev_files in
            aux index setf rev_files rest
      in
      aux 1 StringSet.empty [] code
    in

    let ocaml_code = split_files ocaml_code in

    (* compositionality *)
    OcamlSplitCode.save fileapi_this ;
    (* -- *)

    let split =
      { env_ocaml_input with
          ocaml_code = ocaml_code
      } in
    let depends =
      if options.O.compile_via_makefile
      then
        ListHashtbl.to_list static_depends
      else
        []
    in
    {
      split = split ;
      depends = depends ;
    }

  let open_out filename =
    let oc =
      try open_out filename
      with
      | Sys_error s -> OManager.error "cannot write file @{<bright>%S@} : %s" filename s
    in
    oc

  let close_out filename oc =
    let () =
      try close_out oc
      with
      | Sys_error s -> OManager.error "cannot close file @{<bright>%S@} : %s@\n" filename s
    in
    ()

  (* ml and cmx files are stored in blabla.opx/_build in compilation mode
   * and in the _build/"path from the -o options " for linking *)
  let build_dir_of_opx opx = Filename.concat opx "_build"
  let get_build_dir ?package_dir:package_dir_opt caller_wd argv_options =
    let package_dir_opt =
      match package_dir_opt with
      | None -> ObjectFiles.get_compilation_directory ()
      | Some _ -> package_dir_opt in
    let d =
      Option.default_map
        argv_options.O.compilation_directory
        build_dir_of_opx
        package_dir_opt in
    if Filename.is_relative d then
      Filename.concat caller_wd d
    else
      d

  let ocaml_generation argv_options env_ocaml_split =

    let split = env_ocaml_split.split in

    (*
      PRODUCTION OF FILES
      The iteration is done is the corresponding folder, with a previous Sys.chdir
    *)

    let output_file_contents acc (filename, contents) =
      if not (File.output filename contents) then OManager.error "cannot write file @{<bright>%S@}" filename ;
      filename :: acc
    in

    let output_file_code acc (filename, code) =
      let () =
        let oc = open_out filename in
        OcamlPrint.Output.code oc code ;
        flush oc ;
        close_out filename oc ;
        ()
      in
      filename :: acc
    in

    (* retrieve current path (caller working directory path) *)
    let caller_wd = Sys.getcwd () in
    let build_dir = get_build_dir caller_wd argv_options in
    let success = File.check_create_path build_dir in
    let _ = if not success then OManager.error "cannot create or enter in directory @{<bright>%S@}" build_dir in

    (* going to building directory *)
    OManager.verbose "goto build directory@ %s" build_dir;
    Sys.chdir build_dir;
    OManager.verbose "outputing ml files";
    let rev_generated_files = [] in
    let rev_generated_files = List.fold_left output_file_contents rev_generated_files split.ocaml_init_contents in
    let rev_generated_files = List.fold_left output_file_code rev_generated_files split.ocaml_code in

    let rev_generated_files =
      if argv_options.O.check_lib
      then
        let filename = "check_lib_15343.ml" in
        let content = "
let _ = prerr_endline (String.concat \" \" (Array.to_list Sys.argv)) ;
prerr_endline \"CHECKUP - LIBS - OK\"
"
        in
        output_file_contents rev_generated_files (filename, content)
      else
        rev_generated_files
    in

    (* generation of the .depends file *)
    let () =
      if argv_options.O.compile_via_makefile then
      let filename = ".depends" in
      let oc = open_out filename in
      let () =
        let iter (basename, depends) =
          let one_line ext =
            Printf.fprintf oc "%s.%s:" basename ext ;
            List.iter (
              fun dep ->
                output_string oc " " ;
                output_string oc dep ;
                output_string oc "." ;
                output_string oc ext
            ) depends ;
            output_string oc "\n" ;
          in
          one_line "cmx" ;
          one_line "cmo" ;
        in
        List.iter iter env_ocaml_split.depends
      in
      flush oc ;
      close_out filename oc ;
    in

    (*
      for the track system, a pass is not allowed to finish somewhere else then in the
      starting directory
    *)
    Sys.chdir caller_wd;

    {
      generated_files = List.rev rev_generated_files ;
      caller_wd = caller_wd ;
    }

  let ocaml_compilation options env_ocaml_output =
    let caller_wd = env_ocaml_output.caller_wd in
    let build_dir = get_build_dir caller_wd options in

    (** COMPILATION AND LINKING *)
    let link_lib = "link" in
    let libs =
      match ObjectFiles.compilation_mode () with
      | `compilation | `linking | `prelude -> []
      | `init ->
          if ObjectFiles.Arg.is_fully_separated () then
            List.rev (
              link_lib ::
                ObjectFiles.fold_name ~deep:true ~packages:true
                (fun acc package ->
                   ObjectFiles.get_package_as_a_valid_ident package :: acc
                ) []
            )
          else
            [link_lib] in

    (* do not change the current dir before calling ObjectFiles.fold* *)
    Sys.chdir build_dir ;
    let chop_extension_files = List.map (fun f ->
                                           try
                                             Filename.chop_extension f
                                           with Invalid_argument s -> raise (Invalid_argument ( s ^ ":Cannot remove extension of "^f ))
                                        ) env_ocaml_output.generated_files in
    let add_extension_gen l ext = List.map (fun v -> Printf.sprintf "%s.%s " v ext) l in
    let add_extension_source = add_extension_gen chop_extension_files in
    let add_extension_lib = add_extension_gen libs in
    let ml_list = add_extension_source "ml" in

    let objects_extension_native = "cmx"
    and objects_extension_bytecode = "cmo"
    and lib_extension_native = "cmxa"
    and lib_extension_bytecode = "cma" in

    let libname =
      match ObjectFiles.compilation_mode () with
      | `compilation | `init -> ObjectFiles.get_current_package_name_as_a_valid_ident ()
      | `linking | `prelude -> link_lib in

    let lib_native = libname ^ "." ^ lib_extension_native in
    let lib_bytecode = libname ^ "." ^ lib_extension_bytecode in

    let dep_lib_native = String.concat " " (add_extension_lib lib_extension_native) in
    let dep_lib_bytecode = String.concat " " (add_extension_lib lib_extension_bytecode) in
    let objects_list_native = String.concat " " (add_extension_source objects_extension_native) in
    let objects_list_bytecode = String.concat " " (add_extension_source objects_extension_bytecode) in

    let objects_list = match options.O.makefile_rule with
    | O.Bytecode -> "all_cmo"
    | O.Native -> "all_cmx"
    | _ -> "all_cmo all_cmx"
    in
    let exe = Filename.basename options.O.target in
    let target_exe = options.O.target in
    let is_relative_caml_lib dir = dir.[0] = '+' in
    let extra_path =
      let relative_position = PathTransform.of_string caller_wd in
      let deep, packages =
        match ObjectFiles.compilation_mode () with
        | `compilation -> false, false
        | `init | `linking | `prelude -> true, true in
      String.concat_map " "
      (fun dir ->
        let dir = if is_relative_caml_lib dir then dir else PathTransform.string_to_mysys ~relative_position dir in
        if is_relative_caml_lib dir || File.exists dir then Printf.sprintf "-I %S" dir else "")
        (ObjectFiles.fold_dir ~deep ~packages (fun acc opx -> build_dir_of_opx opx :: acc) options.O.extra_path)
    in
    let substitute_lib_extension_native = File.subst ["", "cmxa" ; "cmo", "cmx" ; "cma", "cmxa"] in
    let substitute_lib_extension_bytecode = File.subst ["", "cma" ; "cmx", "cmo" ; "cmxa", "cma"] in

    let extra_lib_native = String.concat_map " " substitute_lib_extension_native options.O.extra_lib in
    let extra_lib_bytecode = String.concat_map " " substitute_lib_extension_bytecode options.O.extra_lib in

    (* options for compiler & linker *)
    let options_compiler = String.concat " " ((if options.O.hacker_mode then "-annot" else "")::(if options.O.profile then "-p" else "")::options.O.mlcopt) in
    let options_linker = String.concat " " ((if options.O.profile then "-p" else "")::options.O.mllopt) in

    let extra_bytecode_options = String.concat " " Qml2ocamlOptions.StaticParameters.extra_bytecode_options in
    let extra_native_options = String.concat " " Qml2ocamlOptions.StaticParameters.extra_native_options in

    let compiler_native = options.O.ocamlopt in
    let compiler_bytecode = options.O.ocamlc in

    let do_command name compilation =
      Some (compilation,
            (fun () ->
               OManager.verbose "%s" name;
               OManager.verbose "%s" compilation;
               command options compilation)) in

    let compilation_native ml_file =
      (** compilation : ml to cmx *)
      Printf.sprintf "%s -c %s %s %s %s"
        compiler_native
        extra_native_options
        options_compiler
        extra_path
        ml_file
    in

    let compilation_bytecode ml_file =
      (** compilation : ml to cmx *)
      Printf.sprintf "%s -c %s %s %s %s"
        compiler_bytecode
        extra_bytecode_options
        options_compiler
        extra_path
        ml_file
    in

    let target_makefile =
      (* avoiding that the Makefile of the init overwrites the Makefile
       * of the linking (since the compilation directory is the same)*)
      match ObjectFiles.compilation_mode () with
      | `init | `compilation -> "Makefile"
      | `linking | `prelude -> "MakefileLinking" in
    let compilation = match options.O.makefile_rule with
    | O.Bytecode -> compilation_bytecode
    | O.Native -> compilation_native
    | _ -> (fun s -> Printf.sprintf "%s\t%s" (compilation_bytecode s) (compilation_native s))
    in
    let linking_native_gen more_opt exe =
      (** linking : cmx + all lib to .exe *)
      Printf.sprintf "%s %s %s %s %s %s runtimeMain.cmx -o %s"
        compiler_native
        extra_native_options
        options_linker
        extra_path
        extra_lib_native
        more_opt
        exe
    in
    let linking_native_with_lib exe =
      match ObjectFiles.compilation_mode () with
      | `init ->
          (* the init package needs to be linked first *)
          linking_native_gen (lib_native ^ " " ^ dep_lib_native) exe
      | `compilation | `linking | `prelude ->
          linking_native_gen (dep_lib_native ^ " " ^ lib_native) exe
    in
    let linking_native_with_obj exe =
      linking_native_gen objects_list_native exe
    in

    let linking_bytecode_gen more_opt exe =
      (** linking : cmx + all lib to .exe *)
      Printf.sprintf "%s %s %s %s %s %s runtimeMain.cmo -o %s"
        compiler_bytecode
        extra_bytecode_options
        options_linker
        extra_path
        extra_lib_bytecode
        more_opt
        exe
    in
    let linking_bytecode_with_lib exe =
      match ObjectFiles.compilation_mode () with
      | `init ->
          (* the init package needs to be linked first *)
          linking_bytecode_gen (lib_bytecode ^ " " ^ dep_lib_bytecode) exe
      | `compilation | `linking | `prelude ->
          linking_bytecode_gen (dep_lib_bytecode ^ " " ^ lib_bytecode) exe
    in
    let linking_bytecode_with_obj exe =
      linking_bytecode_gen objects_list_bytecode exe
    in

    let linking_with_obj, _linking_with_lib = match options.O.makefile_rule
    with
    | O.Bytecode -> linking_bytecode_with_obj exe, linking_bytecode_with_lib exe
    | O.Native -> linking_native_with_obj exe, linking_native_with_lib exe
    | _ -> "", ""
    in

    let do_compilation =
      if options.O.compile_via_makefile
      then []
      else
        List.map
          (fun ml_file -> do_command "compiling" (compilation ml_file)) ml_list
    in

    let do_linking =
      if options.O.compile_via_makefile
      then None
      else
        Some (linking_with_obj,
              (fun () ->
                 OManager.verbose "linking";
                 OManager.verbose "%s" linking_with_obj;
                 let r = command options linking_with_obj in
                 if r = 0 then OManager.verbose "exe is \t@{<bright>%s@}" exe ; r))
    in

    let compilation_via_makefile =
      let make = Printf.sprintf "%s --makefile=%s -W %s -j %d %s" Config.makebinary target_makefile target_makefile options.O.makefile_max_jobs
        (match options.O.makefile_rule with
        | O.Bytecode ->
            (match ObjectFiles.compilation_mode () with
             | `compilation | `linking | `prelude -> "cma"
             | `init -> "byte")
        | O.Native ->
            (match ObjectFiles.compilation_mode () with
             | `compilation | `linking | `prelude -> "cmxa"
             | `init -> "native")
        | O.Bytecode_or_native -> "tryall"
        | O.Bytecode_and_native -> "all")
      in
      if options.O.show_compilation then make else make^" -s"
    in

    let do_compilation_via_makefile =
      if options.O.compile_via_makefile
      then do_command "compiling" compilation_via_makefile
      else None
    in

    let do_one_camlp4o_makefile ml = Printf.sprintf "\t$(CAMLP4O) %s -o %s" ml ml in
    let do_one_camlp4o ml = Printf.sprintf "\tcamlp4o.opt %s -o %s" ml ml in
    let do_camlp4o = if options.O.camlp4o || options.O.hacker_mode then Some (
      "camlp4o preprocess",
      (fun () ->
         List.fold_left (fun a f ->
                           OManager.verbose "applying camlp4o.opt to file %S" f;
                           let c = command options (do_one_camlp4o f) in
                           if a = 0 then c else a)
           0 env_ocaml_output.generated_files))
    else None in

    let mrule ~native = Printf.sprintf "\t%s $<" (if native then "$(NATIVE_RULE)" else "$(BYTECODE_RULE)" ) in
    (* let all_ml = String.concat " " env_ocaml_output.generated_files in *)


    let compilation_log_file = "compilation.log" in
    (* let make_filter = "2> >(grep -v \"ld: warning: directory not found for option*\" 1>&2)" in *)
    let make_filter = Printf.sprintf "2> %s" compilation_log_file in (* FIXME: we ignore all caml compilation error... *)

    let makefile : unit -> string = fun () ->
      let ( |> ) = FBuffer.addln in
      let ( |< ) x f = f x in
      let sprintf = Printf.sprintf in
      FBuffer.create 1024
        (* THIS IS THE GENERATED MAKEFILE *)
    |> sprintf "# Makefile for %s" exe

    |> sprintf "\nCAMLP4O=camlp4o.opt"
    |> sprintf "\n.SUFFIXES: .cmo .cmx .cmi .ml"
    |> sprintf "\n.ml.cmo:"
    |> sprintf "\t%s" (mrule ~native:false)
    |> sprintf "\n.ml.cmi:"
    |> sprintf "\t%s" (mrule ~native:false)
    |> sprintf "\n.ml.cmx:"
    |> sprintf "\t%s" (mrule ~native:true)

    |> sprintf "\nNATIVE_RULE=%s" (compilation_native "")
    |> sprintf "\nBYTECODE_RULE=%s" (compilation_bytecode "")

    |> sprintf "\nNATIVE_LINKING=%s" (linking_native_with_lib "")
    |> sprintf "\nBYTECODE_LINKING=%s" (linking_bytecode_with_lib "")
    |> sprintf "\nOCAMLDEP=%s " "ocamldep"

    |> sprintf "all :\n" ^
      (if options.O.hacker_mode then "\t$(MAKE) clean_old_source\n" else "")
    |> sprintf "\t$(MAKE) native"
    |> sprintf "\t$(MAKE) byte"
    |> sprintf "\t$(MAKE) exe"
    |> sprintf "exe : %s" exe

    |> sprintf "tryall :"
    |> sprintf "\t$(MAKE) byte || $(MAKE) native || echo \"compilation failed in native and in byte\""
    |> sprintf "all_cmx : %s" objects_list_native
    |> sprintf "all_cmo : %s" objects_list_bytecode
    |> sprintf "cmxa: all_cmx"
    |> sprintf "\t%s -linkall -a %s -o %s" compiler_native objects_list_native lib_native
    |> sprintf "cma: all_cmo"
    |> sprintf "\t%s -linkall -a %s -o %s" compiler_bytecode objects_list_bytecode lib_bytecode
    |>         "\n# Default compilation"
    |> sprintf "%s : %s" exe objects_list
    |> sprintf "\t%s $@ %s" (match options.O.makefile_rule
       with
       | O.Bytecode -> "$(BYTECODE_LINKING)"
       | O.Native -> "$(NATIVE_LINKING)"
       | O.Bytecode_and_native -> "$(MAKE) all"
       | O.Bytecode_or_native -> "$(MAKE) tryall"
       ) make_filter

    |>         "\n# Native compilation"
    |> sprintf "native : cmxa"
    |> sprintf "\t$(NATIVE_LINKING) %s %s" exe make_filter
    |> sprintf "%s.native : cmxa" exe
    |> sprintf "\t$(NATIVE_LINKING) $@ %s" make_filter

    |>         "\n# Bytecode compilation"
    |> sprintf "byte : cma"
    |> sprintf "\t$(BYTECODE_LINKING) %s %s" exe make_filter
    |> sprintf "%s.byte : cma" exe
    |> sprintf "\t$(BYTECODE_LINKING) $@ %s" make_filter


    |> "\n# COMPILATION RULES"
    (*|< compute_linear_dependencies ~native:true
    |< compute_linear_dependencies ~native:false*)
    |> sprintf "\n-include .depends"
    |> "\ncamlp4 :"
    |> String.concat_map "\n" do_one_camlp4o_makefile env_ocaml_output.generated_files

    |> "\nclean :"
    |> "\trm -f *.cmx *.cmi *.cmo *.o"

    |> sprintf "\nshow_old_source :\n\tls | grep -v -E \"%s\""
        (String.concat "|" env_ocaml_output.generated_files)


    |> sprintf "\nclean_old_source :\n\tls | grep -v -E \"%s|*.qml|*.exe\" | xargs rm -f"
        (String.concat "|" (target_makefile::env_ocaml_output.generated_files))

    |< FBuffer.contents

    in

    let do_makefile =
      (*       if options.O.compile_via_makefile *)
      (*       then *)
      Some (
        Printf.sprintf "writing \"%s\"" target_makefile,
        (fun () ->
           OManager.verbose "writing debug makefile : %S" target_makefile;
           let b = File.output target_makefile (makefile ()) in
           if not b then OManager.printf "cannot write %S@\n" target_makefile ;
           0))
        (*       else None  *)
    in

    let abs_path_exe = Filename.concat build_dir exe in
    let rel_path_exe =
      if Filename.is_relative target_exe then
        Filename.concat caller_wd target_exe
      else target_exe
    in

    let exe_move =
      match ObjectFiles.compilation_mode (), options.O.no_move, !ObjectFiles.no_init with
      | (`compilation | `linking | `prelude), _, false
      | _, true, false -> None
      | _, _, true
      | `init, false, false ->
        Some (
          let c = Printf.sprintf "copying %s to %s"  abs_path_exe rel_path_exe in
          c,
          (fun () ->
             try
               if abs_path_exe <> rel_path_exe then
                 File.copy ~force:true abs_path_exe rel_path_exe
               else 0
             with Sys_error s -> OManager.printf "System error: %s" s ; 1
          ))
    in
    let exe_run =
      match ObjectFiles.compilation_mode (), options.O.exe_run, !ObjectFiles.no_init with
      | (`compilation | `linking | `prelude), _, false
      | _, false, false -> None
      | _, _, true
      | `init, true, false ->
        let path_exe =
          if options.O.no_move
          then abs_path_exe
          else rel_path_exe
        in
        Some
          (let argv = path_exe :: options.O.exe_argv in
           let c = String.concat " " argv in
           c,
           (fun () ->
              OManager.verbose "building finished, will run the exe@\n@{<bright>%s@}" c;
              let argv = Array.of_list ( path_exe :: options.O.exe_argv ) in
              Sys.chdir caller_wd ;
              let do_it () = Unix.execv path_exe argv in
              Unix.handle_unix_error do_it ()
           )) in

    (*     let command_line s = s, (fun () -> command options s) in *)
    let full_compilation =
      [
        do_camlp4o ;
        do_makefile ]@
        do_compilation@
        [	do_linking ;
                do_compilation_via_makefile ;
                exe_move ;
                exe_run
        ]
    in

    let filter_compilation = List.filter_map (fun s -> s) full_compilation in
    let rec do_compilation = function
      | [] -> 0
      | (c, cont) :: q ->
          let o = cont () in
          if o <> 0
          then (
            OManager.printf "error during ocaml compilation -- the command was :@\n%s@\nyou can see compilation logs in %s/%s\n" c build_dir compilation_log_file;
            o
          )
          else do_compilation q in
    let return = do_compilation filter_compilation in
    (* OManager.printf "Compilation log file at %s/%s\n" build_dir compilation_log_file; *)
    Sys.chdir caller_wd ;
    return
end

(* ============================================================================== *)

module Sugar :
sig
  (** sugar of interface to be used in opa/passes.
      Beware, this function do not call any Front pass.
      It starts at qml_to_ocaml and goes still end passes.
      This function does not have the same behavior than console which includes Front passes.*)
  val for_opa : qml_to_ocaml -> Qml2ocamlOptions.argv_options -> BslLib.env_bsl -> QmlBlender.qml_milkshake -> int
end
  =
struct
  let for_opa qml_to_ocaml options bsl blender =
    let env_ocaml_input = qml_to_ocaml options bsl blender in
    let env_ocaml_split = OcamlCompilation.ocaml_split_code options env_ocaml_input in
    let env_ocaml_output = OcamlCompilation.ocaml_generation options env_ocaml_split in
    let returned = OcamlCompilation.ocaml_compilation options env_ocaml_output in
      returned
end
