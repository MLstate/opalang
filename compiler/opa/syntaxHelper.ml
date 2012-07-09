(*
    Copyright © 2011 MLstate

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

(** The main program for the Opa compiler. S3 version. *)

(* Opening the generic pass system. *)
module PH = PassHandler

(* FIXME: define a module InfixOperators in PassHandler *)
(* this could by the only case an 'open' is allowed *)
let (|+>) = PH.(|+>)
let (|>) = PH.(|>)
let (<?>) = PH.(<?>)
let (&) = PH.(&)
let (|?>) = PH.(|?>)
let (or) = PH.(or)

(* Shorthands for accessing options of compilation *)
module If = Main_utils.If

(* The deprecated passes *)
(* FIXME: adapt to the new PassHandler *)
module S2 = Passes

(* S3 implementations. *)
module S3 = S3Passes

(* Set title of generic pass system. *)
let _ = PH.set_title "SyntaxConverter"

(* Load warnings of opa s3 applications *)
let _ = WarningClass.load_set S3Warnings.warning_set

(* Run all passes *)
let _ =
  PH.init

  |+> ("Welcome", S3.pass_Welcome)

  |+> ("CheckOptions", S3.pass_CheckOptions)

  |+> ("AddStdlibFiles", S3.pass_AddStdlibFiles)

  |> PH.old_handler
      "OpenFiles" S2.pass_OpenFiles

  |+> ("PreProcess", S3.pass_PreProcess)

  |+> ("Parse", S3.pass_ParseSugar)

  |+> ("Print", S3.pass_Print)

  |> PH.return

let () = OManager.exit 0
