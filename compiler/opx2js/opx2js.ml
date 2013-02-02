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

(* Set title of generic pass system. *)
let _ = PH.set_title "Opa2JS"

module O2J = Opx2jsPasses

(* Run all passes *)
let code =
  (**********************************************)
  (* INITIALIZATION *****************************)
  PH.init

  |+> ("Welcome", O2J.pass_Welcome)

  |+> ("CheckOptions", O2J.pass_CheckOptions)

  |+> ("LoadEnvironment", O2J.pass_LoadEnvironment (fun e -> e

    |+> ("NodeJsPluginCompilation", O2J.pass_NodeJsPluginCompilation)

    |+> ("NodeJsPluginGeneration", O2J.pass_NodeJsPluginGeneration)

    |> PH.return
  ))

  |> PH.return


let () = OManager.exit code

