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

(**
   Flat Compiler : Compilation of Expression
   @author Mathieu Barbin
   @author David Rajchenbach-Teller
   @author Louis Gesbert
*)

(** {6 Field Registration} *)

type label = string

(**
   Register a shared variable for fields.
   All definition of shared fiels will be produced at toplevel,
   in the beginning of the code.
*)
val register_field_name : label -> unit

(**
   Probably needed: register all vtables, by folding type definitions.
*)

(** {6 Processing code} *)

(**
   Where to load primitives from plugins.
   Used by the pass BslLoading.
*)
val dynloader : Qml2ocaml.dynloader

(**
   This is the function considered as the pass.
   It contains the loading of previous environment,
   the compilation of the current package,
   the saving of the current environment, and the reset
   of the private imperative tables of the flat.
*)
val qml_to_ocaml : Qml2ocaml.qml_to_ocaml

(**
   For the consol application.
*)
val back_end : Qml2ocaml.back_end

(** {6 Options} *)

module Arg:
sig
  val options : (Base.Arg.key * Base.Arg.spec * Base.Arg.doc) list
end

(** {6 Warnings} *)

val warning_set : WarningClass.Set.t
