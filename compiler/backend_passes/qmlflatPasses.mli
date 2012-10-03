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

(* FINAL QMLFLAT COMPILATION *********************)

(**
   Environment returned by the QmlCompilation.
*)
type env_QmlCompilation = {
  qmlCompilation_options : Qml2ocamlOptions.argv_options ;
  qmlCompilation_env_ocaml_input : Qml2ocaml.env_ocaml_input ;
}

(**
   Environment returned after splitting ocaml code into smaller files
*)
type env_OcamlSplitCode = {
  ocamlSplitCode_options : Qml2ocamlOptions.argv_options ;
  ocamlSplitCode_env_ocaml_split : Qml2ocaml.env_ocaml_split ;
}

(**
   Environment returned by the OcamlGeneration.
*)
type env_OcamlGeneration = {
  ocamlGeneration_options : Qml2ocamlOptions.argv_options ;
  ocamlGeneration_env_ocaml_output : Qml2ocaml.env_ocaml_output ;
}

(**
   Environment returned by the OcamlCompilation.
*)
type env_OcamlCompilation = {
  ocamlCompilation_returned_code : int ;
}

val pass_QmlCompilation :
  (Passes.env_NewFinalCompile, env_QmlCompilation) S3Passes.opa_pass

val pass_OcamlSplitCode :
  (env_QmlCompilation, env_OcamlSplitCode) S3Passes.opa_pass

val pass_OcamlGeneration :
  (env_OcamlSplitCode, env_OcamlGeneration) S3Passes.opa_pass

val pass_OcamlCompilation :
  (env_OcamlGeneration, int) S3Passes.opa_pass

(* ***********************************************)
