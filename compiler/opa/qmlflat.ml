(* extractors *)
let extract_final_ac env =
  env.P.newFinalCompile_qml_milkshake.QmlBlender.env.QmlTypes.annotmap,
  env.P.newFinalCompile_qml_milkshake.QmlBlender.code
let extract_final_gamma env =
  env.P.newFinalCompile_qml_milkshake.QmlBlender.env.QmlTypes.gamma
let extract_final_code env = env.P.newFinalCompile_qml_milkshake.QmlBlender.code
let extract_final_bypass_typer env = env.P.newFinalCompile_qml_milkshake.QmlBlender.env.QmlTypes.bypass_typer
let extract pass_env = (extract_final_bypass_typer pass_env, extract_final_code pass_env)

let pass_QmlLiftDeepRecords =
  make_process_code_pass
    (fun _ -> Pass_LiftDeepRecords.process_code ~typed:true)
    P.extract_env_NewFinalCompile
    P.rebuild_env_NewFinalCompile
    ()

let pass_InitializeBslValues =
  PH.make_pass
    (fun e ->
       let env =  e.PH.env in
       let annotmap, code = extract_final_ac env in
       let gamma = extract_final_gamma env in
       let stdlib_gamma = env.P.newFinalCompile_stdlib_gamma in
       let gamma, annotmap, code =
         Pass_InitializeBslValues.process_code ~stdlib_gamma gamma annotmap code
       in
       let milkshake = { QmlBlender.
                          code = code;
                          env = {env.P.newFinalCompile_qml_milkshake.QmlBlender.env with QmlTypes.
                                   annotmap = annotmap;
                                   gamma = gamma;
                                }
                       } in
       let env = {env with P.newFinalCompile_qml_milkshake = milkshake} in
       {e with PH.env = env}
    )

let pass_ServerCpsRewriter =
  let transform pass_env =
    let options = pass_env.PassHandler.options in
    let env = pass_env.PassHandler.env in
    { pass_env with PassHandler.
        env = Passes.pass_QmlCpsRewriter false ~options env
    }
  in
  (* invariants, and pre/post conds *)
  let invariant =
    [
      (* TODO: add invariants *)
    ] in
  let precond =
    [
    ] in
  let postcond =
    [
      (*
        This post condition is currently broken because of the second_order bypasses.
        In bsl_ocaml_init.ml they take 1 extra argument, but the bypass_typer does not know
        about it. This condition is desactivated until we solve this probleme.
        QmlCheck.Bypass.applied extract
      *)
    ] in
  PassHandler.make_pass ~invariant ~precond ~postcond transform

let pass_ServerQmlClosure =
  PassHandler.make_pass
    (fun e ->
       { e with PH.env = P.pass_QmlClosure2 ~typed:false ~side:`server ~options:e.PH.options e.PH.env })

let pass_QmlConstantSharing_gen side =
  let transform pass_env =
    let typerEnv, code = P.extract_env_NewFinalCompile pass_env.PH.env in
    let gamma = typerEnv.QmlTypes.gamma
    and annotmap = typerEnv.QmlTypes.annotmap in
    let (gamma,annotmap), code =
      Pass_ConstantSharing.process_code ~side ~typed:false gamma annotmap code in
    let typerEnv =
      { typerEnv with
        QmlTypes.gamma = gamma;
        QmlTypes.annotmap = annotmap }
    in
    { pass_env with PH.env = P.rebuild_env_NewFinalCompile pass_env.PH.env typerEnv code
    }
  in
  let precond =
    [
      (* TODO: add pre condition *)
    ] in
  let postcond =
    [
      QmlAlphaConv.Check.alpha extract_final_ac ;
    ] in
  PassHandler.make_pass ~precond ~postcond transform

let pass_QmlConstantSharing =
  pass_QmlConstantSharing_gen `server
let pass_ClientQmlConstantSharing =
  let pass_QmlConstantSharing = pass_QmlConstantSharing_gen `client in
  Adapter.adapt_new_sliced_on_client pass_QmlConstantSharing

let pass_QmlCompilation =
  let transform pass_env =
    let options = pass_env.PH.options in
    let env = pass_env.PH.env in
    (* get env entities *)
    let qml2ocaml_env_bsl = env.Passes.newFinalCompile_bsl in
    let qml2ocaml_qml_milkshake = env.Passes.newFinalCompile_qml_milkshake in
    (* renaming is not used *)
    (* 1) transform options *)
    let qmlCompilation_options = Passes.pass_OpaOptionsToQmlOptions ~options qml2ocaml_qml_milkshake in
    (* 2) selection of the back-end *)
    assert (options.O.back_end = OpaEnv.Backend "qmlflat");
    let qml_to_ocaml =Flat_Compiler.qml_to_ocaml in
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
