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

(* FINAL QMLJS COMPILATION ***********************)

(** Environment needed by the final JavasScript compilation. *)
type env_JsCompilation

val pass_ServerJavascriptCompilation :
  (Passes.env_NewFinalCompile, env_JsCompilation) S3Passes.opa_pass

val pass_ServerJavascriptOptimization :
  (env_JsCompilation, env_JsCompilation) S3Passes.opa_pass

val pass_ServerJavascriptGeneration :
  (env_JsCompilation, int) S3Passes.opa_pass

(* ***********************************************)
