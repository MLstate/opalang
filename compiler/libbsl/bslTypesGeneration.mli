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
   Module for macro generating bsl types coercions and definitions.

   @author Mathieu Barbin
*)

(**
   This module implements the traduction of bsl-types into the back-end languages.
   It has also a module dedicated to Opa syntax for bsl-preprocessing (cf [iformats]).

   TODO:
   Currently, we output code only as concrete syntax (Ocaml, Js).
   It is really usefull to have dedicated printers for BslTypes,
   because we can make some specific control for external primitives
   printing and error messages,
   but for code generation, it would be better to pass through an
   AST generation.
   Some (sunny) day we will add an implemention for AST production.

   + when OcamlAst will be more stable.
   + when JsAst will be more stable.
   + when we will decide what we do with LLVM, and have a AST for extern C bindings.

*)

(** *)
type 'a pprinter = 'a LangPrint.pprinter
type ('env, 'a, 'ast) generator = 'env -> 'a -> 'env * 'ast

module Opa :
sig
  (**
     Printer for [t] with the opa syntax
  *)
  val pp : BslTypes.t pprinter

  (**
     Used for declarating a new external type
     {[
     type toto('a, 'b) = external
     ]}
     Assert: This function is called with an [External] type, with [TypeVar] for all parameters
     @error if [t] is not a candidate for definition (outside of this case)
  *)
  val pp_definition : BslTypes.t pprinter
end

module Ocaml :
sig
  (** TODO: add link for ocaml path manager *)

  (**
     Printer for [t] with the ocaml syntax.

     This function is the kernel of the type checking done
     on the [MLRuntime], because it produces types coercions
     from the types read in the register directives.

     cf: BslServerLib.S

     The scope of type variables is fresh for every call to pp.
  *)
  val pp : BslTypes.t pprinter

  (**
     For External types, the implementation is hidden, it is a ocaml row code.
     {[
     ##extern-type ('a, 'b) toto = Ka of 'a | Kb of 'b
     ]}
     will produce
     + in the ml:
     {[
     type ('a, 'b) toto = Ka of 'a | Kb of 'b
     ]}
     + in the mli:
     {[
     type ('a, 'b) toto
     ]}
     + in opa:
     {[
     type toto('a, 'b) = external
     ]}

     Assert: This function is called with an exteranl types, with only [TypeVars] for parameters

     @error if [t] is not a candidate for definition (outside of this case)
  *)
  val pp_definition : BslTypes.t pprinter
end

module C :
sig
  (**
     At some we did some tests for targetting llvm.
     This is currently unused, and unmaintain, and this is
     probably no more corresponding to anything we will keep.

     This is partially not implemented and not supported by
     any back-end.

     @deprecated Wait for LLVM backend.
  *)

  (**
     Printer for [t] with the C syntax.

     <!> It outputs type name and macro which are define in
     the pervasives of the C (["opabsl/cbsl"]). You cannot use
     it without the C init library.
  *)
  val pp : BslTypes.t pprinter

end
