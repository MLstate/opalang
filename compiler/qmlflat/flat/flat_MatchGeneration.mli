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
   Flat Compiler : Pattern Matching generation
   @author Mathieu Barbin
*)

(**
   With the new compositional qmlflat, there are some optimization possible for compiling,
   pattern matching, based in particular on array matching with OCaml, and vtable sharing.
*)

(**
   {[
   match record with
   | {a} -> e_a
   | {b} -> e_b
   | x -> e_x
   ]}
   {[
   match record with
   | v when v == shared_record_a -> e_a
   | v when v == shared_record_b -> e_b
   | x -> e_x
   ]}
*)

module PatternAnalysis :
sig
  (** {6 Types of Match} *)
  (**
     This is the cases handled by the generation.
     We use this private type for ensuring statically that the correct function
     of generation is applied to the correct kind of matching.
     We also use this type for making possible an internal traduction of patterns into
     a private algebra, hidden by the abstraction, and handled by the generation modules.
  *)

  type trivial
  type closed
  type mixed

  type t =
    | Trivial of trivial
    | Closed of closed
    | Mixed of mixed

  (**
     Given the expression we are matched, and the pattern list with right-side
     production, return the analysed pattern matching, ready for generation.

     TODO: for optimization, add [gamma] and [ty] as argument
  *)
  val analysis : Ocaml.expr -> (QmlAst.pat * Ocaml.expr) list -> t
end

module TrivialGeneration :
sig
  val compile : PatternAnalysis.trivial -> Ocaml.expr
end

module ClosedGeneration :
sig
  val compile : PatternAnalysis.closed -> Ocaml.expr
end

module MixedGeneration :
sig
  val compile : PatternAnalysis.mixed -> Ocaml.expr
end

(** Sugar for outside *)
val compile :  Ocaml.expr -> (QmlAst.pat * Ocaml.expr) list -> Ocaml.expr
