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
