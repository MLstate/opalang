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

(** The main program for the OPA compiler. S3 version. *)

(* Opening the generic pass system. *)
module PH = PassHandler

(* FIXME: define a module InfixOperators in PassHandler *)
(* this could by the only case an 'open' is allowed *)
let (|+>) = PH.(|+>)
let (|>) = PH.(|>)
let (<?>) = PH.(<?>)
let (&) = PH.(&)
let (|?>) = PH.(|?>)
let (|?|) = PH.(|?|)
let (or) = PH.(or)

(* Shorthands for accessing options of compilation *)
module If = Main_utils.If
module Switch = Main_utils.Switch

(* The deprecated passes *)
(* FIXME: adapt to the new PassHandler *)
module S2 = Passes

(* S3 implementations. *)
module S3 = S3Passes

let flat_backend = Compiler.make_backend "qmlflat"
  ~aliases:["native"] (
    PassHandler.make_pass (
      fun e -> e

      |> PH.old_if_handler ~if_:If.closure
	  "ServerQmlLambdaLifting" (S2.pass_LambdaLifting2 ~typed:false ~side:`server)

      |?> (If.constant_sharing,
	   "QmlConstantSharing", S3.pass_QmlConstantSharing)

      |>  PH.old_if_handler ~if_:If.closure
	  "ServerQmlUncurry" (S2.pass_QmlUncurry2 ~typed:false ~side:`server)

      |?> (If.closure,
	   "ServerQmlClosure", S3.pass_ServerQmlClosure)

      |+> ("QmlCompilation", QmlflatPasses.pass_QmlCompilation)

      |+> ("OcamlSplitCode", QmlflatPasses.pass_OcamlSplitCode)

      |+> ("OcamlGeneration", QmlflatPasses.pass_OcamlGeneration)

      |+> ("OcamlCompilation", QmlflatPasses.pass_OcamlCompilation)

    )
  ) (Some Flat_Compiler.dynloader) BslLanguage.ml Flat_Compiler.register_field_name

let js_backend = Compiler.make_backend "qmljs"
  ~aliases:["node";"js";"nodejs";"node.js"] (
    PassHandler.make_pass (
      fun e -> e

      |> PH.old_handler
	  "ServerQmlLambdaLifting" (S2.pass_LambdaLifting2 ~typed:false ~side:`server)

      |+> ("ServerJavascriptCompilation", QmljsPasses.pass_ServerJavascriptCompilation)

      |+> ("ServerJavascriptOptimization", QmljsPasses.pass_ServerJavascriptOptimization)

      |+> ("ServerJavascriptGeneration", QmljsPasses.pass_ServerJavascriptGeneration)

    )
  ) None BslLanguage.nodejs ignore

let () = Compiler.compile([js_backend; flat_backend])
