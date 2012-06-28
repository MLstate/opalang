(*
    Copyright © 2011 MLstate

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
   Flat Compiler : Bypass Projection
   @author Mathieu Barbin
   @author David Rajchenbach-Teller
   @author Valentin Gatien-Baron
*)

(** {6 Compositionality} *)

(**
   The module should be restarted before each compilation unit
*)
val reset : unit -> unit

(** {6 CTrans} *)

module ML_CTrans : BslInterface.ML_CTRANS

val build_ctrans_env :
  ?typesmap:BslInterface.typesmap ->
  Qml2ocamlOptions.argv_options ->
  ML_CTrans.env

(**
   In the ctrans_env there is a map of seg-fault bypass.t
   collected during the projection (not give the error directly,
   and fail at bypass resolution only if this bypass is used)

   Currently the string is a message of error, None if no error.

   TODO: clean-up, use QmlError, etc...
*)
val is_segfault : BslKey.t -> ML_CTrans.env -> string option

(** {6 BSL} *)

module FlatBSL : BslInterface.BSLINTROSPECTION with type ml_ctrans_env = ML_CTrans.env
