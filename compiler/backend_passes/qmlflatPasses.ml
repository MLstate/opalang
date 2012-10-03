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

module O = OpaEnv
module P = Passes
module PH = PassHandler

type env_QmlCompilation = {
  qmlCompilation_options : Qml2ocamlOptions.argv_options ;
  qmlCompilation_env_ocaml_input : Qml2ocaml.env_ocaml_input ;
}

type env_OcamlSplitCode = {
  ocamlSplitCode_options : Qml2ocamlOptions.argv_options ;
  ocamlSplitCode_env_ocaml_split : Qml2ocaml.env_ocaml_split ;
}

type env_OcamlGeneration = {
  ocamlGeneration_options : Qml2ocamlOptions.argv_options ;
  ocamlGeneration_env_ocaml_output : Qml2ocaml.env_ocaml_output ;
}

type env_OcamlCompilation = {
  ocamlCompilation_returned_code : int ;
}

let pass_OpaOptionsToQmlOptions ~(options:Passes.opa_options) qml_milkshake =
  (** PASSING OPTIONS TO QML2OCAML : we must absolutly here use the syntax \{ with \} because
      the interface of qmlfake options is unfrozen (and need to stay so) (don't need to update this code) *)
  let argv_options = Qml2ocamlOptions.ArgvOptions.default (OpaEnv.string_of_available_back_end options.OpaEnv.back_end) in
  let argv_options =
    let compilation_directory =
      if options.OpaEnv.build_dir = ""
      then Filename.concat "_build" (File.chop_extension options.OpaEnv.target)
      else options.OpaEnv.build_dir
    in
    let split_ocaml_value =
      let mindeclfile = 20 (* desire *) in
      let maxfiles = 500 (* approximate constraint *) in
      let nb_decl = List.length qml_milkshake.QmlBlender.code in
      let split_for_maxfiles =  int_of_float (ceil ((float nb_decl ) /. (float maxfiles))) in
        (* min 2 *) (max mindeclfile split_for_maxfiles)
    in
    let cclib = List.map (Printf.sprintf "-cclib %s") options.OpaEnv.cclib in
    let ccopt = List.map (Printf.sprintf "-ccopt %s") options.OpaEnv.ccopt in
    { argv_options with Qml2ocamlOptions.
        bypass_plugin = options.OpaEnv.bypass_plugin ;
        camlp4o = false ; (* options.OpaEnv.camlp4o *)
        compilation_directory = compilation_directory ; (* options.OpaEnv.build_dir *)
        compile_via_makefile = true; (* not Base.is_windows; *) (** use _build/Makefile except on Windows *)
        cps = options.OpaEnv.cps ;
        cps_toplevel_concurrency = options.OpaEnv.cps_toplevel_concurrency ;
        server_side = true;
        display_schema = options.OpaEnv.dump_dbgen_schema ;
        exe_argv = Option.default [] options.OpaEnv.run_server_options ;
        extra_lib = argv_options.Qml2ocamlOptions.extra_lib @ options.OpaEnv.extralibs ;
        extra_path = OpaEnv.Parameters.server_include_dir @
        (List.map (Filename.concat (Lazy.force InstallDir.getenv)) OpaEnv.Parameters.server_include_mlstate_dir) @
        options.OpaEnv.extrapath ;
        exe_run = Option.is_some options.OpaEnv.run_server_options ;
        hacker_mode = options.OpaEnv.hacker_mode ;
        makefile_rule = options.OpaEnv.makefile_rule ;
        mlcopt = argv_options.Qml2ocamlOptions.mlcopt @ options.OpaEnv.mlcopt @ ccopt ;
        mllopt = argv_options.Qml2ocamlOptions.mllopt @ options.OpaEnv.mllopt @ ccopt @ cclib ;
        no_assert = options.OpaEnv.no_assert ;
        ocamlc = options.OpaEnv.ocamlc ;
        ocamlopt = options.OpaEnv.ocamlopt ;
        profile = options.OpaEnv.profile ;
        qml_closure = options.OpaEnv.closure;
        show_compilation = options.OpaEnv.show_compilation ;
        split_ocaml_value =  split_ocaml_value ;
        target = options.OpaEnv.target ;
        (* hack for "cannot generalize"; in rare cases of Opa code helps, and can be complementary with eta_expand *)
        top_magic = true ;
    } in
  argv_options

let pass_QmlCompiler ~(options:Passes.opa_options) (env:Passes.env_NewFinalCompile) : Passes.env_BinaryGeneration =
  let qml_milkshake = env.Passes.newFinalCompile_qml_milkshake in
  let env_bsl = env.Passes.newFinalCompile_bsl in
  let argv_options = pass_OpaOptionsToQmlOptions ~options qml_milkshake in
  (** Choice of back-end *)
  let qml_to_ocaml = Flat_Compiler.qml_to_ocaml in
  (* This pass is splitten in 3 in opas3 *)
  let return = Qml2ocaml.Sugar.for_opa qml_to_ocaml argv_options env_bsl qml_milkshake in
  let out = {
    Passes.binaryGeneration_success = return = 0
  }
  in let () = if return <> 0 then prerr_endline "OCAML COMPILER FAIL"
  in out

let pass_QmlCompilation =
  let transform pass_env =
    let options = pass_env.PH.options in
    let env = pass_env.PH.env in
    (* get env entities *)
    let qml2ocaml_env_bsl = env.Passes.newFinalCompile_bsl in
    let qml2ocaml_qml_milkshake = env.Passes.newFinalCompile_qml_milkshake in
    (* renaming is not used *)
    (* 1) transform options *)
    let qmlCompilation_options = pass_OpaOptionsToQmlOptions ~options qml2ocaml_qml_milkshake in
    (* 2) selection of the back-end *)
    let qml_to_ocaml = Flat_Compiler.qml_to_ocaml in
    (* proceed *)
    let qmlCompilation_env_ocaml_input = qml_to_ocaml qmlCompilation_options qml2ocaml_env_bsl qml2ocaml_qml_milkshake in
    (* build env *)
    let qmlCompilation_env =
      {
        qmlCompilation_options = qmlCompilation_options ;
        qmlCompilation_env_ocaml_input = qmlCompilation_env_ocaml_input
      }
    in
    let empty _ = [] in
    {
      pass_env with PassHandler.
        env = qmlCompilation_env ;
        printers = OcamlTrack.printers (fun env -> env.qmlCompilation_env_ocaml_input.Qml2ocaml.ocaml_code) ;
        trackers = empty
    }
  in
  let invariant =
    [
      (* TODO: add invariants *)
    ] in
  let precond =
    [
      (* TODO: add pre conditions *)
    ] in
  let postcond =
    [
      (* TODO: add pre conditions *)
    ] in
  PassHandler.make_pass ~invariant ~precond ~postcond transform

let pass_OcamlSplitCode =
  let transform pass_env =
    let env = pass_env.PassHandler.env in
    (* get env entities *)
    let ocamlSplitCode_options = env.qmlCompilation_options in
    let qmlCompilation_env_ocaml_input = env.qmlCompilation_env_ocaml_input in
    (* proceed *)
    let ocamlSplitCode_env_ocaml_split =
      Qml2ocaml.OcamlCompilation.ocaml_split_code ocamlSplitCode_options qmlCompilation_env_ocaml_input in
    (* build env *)
    let env = {
      ocamlSplitCode_options = ocamlSplitCode_options ;
      ocamlSplitCode_env_ocaml_split = ocamlSplitCode_env_ocaml_split ;
    }
    in
    let empty _ = [] in
    {
      pass_env with PassHandler.
        env = env ;
        printers = empty ;
        trackers = empty
    }
  in
  let invariant =
    [
      (* TODO: add invariants *)
    ] in
  let precond =
    [
      (* TODO: add pre conditions *)
    ] in
  let postcond =
    [
      (* TODO: add pre conditions *)
    ] in
  PassHandler.make_pass ~invariant ~precond ~postcond transform

let pass_OcamlGeneration =
  let transform pass_env =
    let env = pass_env.PassHandler.env in
    (* get env entities *)
    let ocamlGeneration_options = env.ocamlSplitCode_options in
    let ocamlSplitCode_env_ocaml_split = env.ocamlSplitCode_env_ocaml_split in
    (* proceed *)
    let ocamlGeneration_env_ocaml_output =
      Qml2ocaml.OcamlCompilation.ocaml_generation ocamlGeneration_options ocamlSplitCode_env_ocaml_split in
    (* build env *)
    let ocamlGeneration_env =
      {
        ocamlGeneration_options = ocamlGeneration_options ;
        ocamlGeneration_env_ocaml_output = ocamlGeneration_env_ocaml_output
      }
    in
    let empty _ = [] in
    {
      pass_env with PassHandler.
        env = ocamlGeneration_env ;
        printers = empty ;
        trackers = empty
    }
  in
  let invariant =
    [
      (* TODO: add invariants *)
    ] in
  let precond =
    [
      (* TODO: add pre conditions *)
    ] in
  let postcond =
    [
      (* TODO: add pre conditions *)
    ] in
  PassHandler.make_pass ~invariant ~precond ~postcond transform

let pass_OcamlCompilation =
  let transform pass_env =
    let env = pass_env.PassHandler.env in
    (* get env entities *)
    let ocamlGeneration_options = env.ocamlGeneration_options in
    let ocamlGeneration_env_ocaml_output = env.ocamlGeneration_env_ocaml_output in
    (* proceed *)
    let ocamlCompilation_returned_code =
      Qml2ocaml.OcamlCompilation.ocaml_compilation ocamlGeneration_options ocamlGeneration_env_ocaml_output in
    let empty _ = [] in
    {
      pass_env with PassHandler.
        env = ocamlCompilation_returned_code ;
        printers = empty ;
        trackers = empty ;
    }
  in
  let invariant =
    [
      (* TODO: add invariants *)
    ] in
  let precond =
    [
      (* TODO: add pre conditions *)
    ] in
  let postcond =
    [
      (* TODO: add pre conditions *)
    ] in
  PassHandler.make_pass ~invariant ~precond ~postcond transform
