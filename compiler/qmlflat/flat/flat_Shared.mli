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
   Flat Compiler : Compile time sharing, and runtime sharing generation
   @author Mathieu Barbin
*)

(**
   Those function returns the identifier which will be generated in the generated code,
   corresponding to the sharing version of the correspondant value.

   During the compilation, at any moment, we can call these function for accessing
   the identifier.

   At the end, we reintroduce all the binding at the beginning of the generated code.
*)

(** {6 Shared identifiers} *)

type label = Flat_Common.label

(**
   FIXME !
   Ocaml Ast should distinguish between top level binding and expressions.
*)
type ocaml_val = Ocaml.expr

val field : label -> Ocaml.expr
val vtable : label list -> Ocaml.expr
val simple : label -> Ocaml.expr

(** {6 Caches} *)

val cache : unit -> Ocaml.expr

(** {6 Introduction of declaration in the generated code} *)

module Let :
sig
  (**
     For outputting the code, you can introduce them in any order
     because the sharing is done at runtime (separated compilation)

     [all] give all the declaration.

     To avoid multiple declaration, the internal state is modified when
     you get ocaml_val definitions.
  *)

  val fields : unit -> ocaml_val list
  val vtables : unit -> ocaml_val list
  val simples : unit -> ocaml_val list

  val caches : unit -> ocaml_val list

  val all : unit -> ocaml_val list

  (**
     Insert the definition before an existing list.
     More efficient than a list concatenation.
  *)
  val insert : ocaml_val list -> ocaml_val list
end

(**
   The module should be restarted before each compilation unit.
*)
val reset : unit -> unit
