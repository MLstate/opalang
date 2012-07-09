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
   Pattern normalisation.
   @author Rudy Sicard
   @author Mathieu Barbin (interfaces, documentation)
*)

(**
   Onion takes a Onionlang:ONIONLANG parameter which define a simplified interface for the langage
   Onion define a generic internal representation for pattern

   All you have to do is to write conversion functions from the external langage to the internal representation and vice-versa.
   See QmlLang (implements ONIONLANG)
   You can then use Onion(MyOnionLang).normalize to normalize the internal represention and convert back.

   This is already done for Qml. So you can directly use Transform.optimize which do the normalisation process


   Current implementation :
   Take an arbitrary pattern disjunction and transform it to recursive matching (i.e. a decision tree)
   over disjunction of surfaces patterns, such that backtracking is never necessary.

   A surface patterns can only check immediate property like record shape or constant value.


   General scheme

   The functor operates on onion pattern, a internal representation containing :
     - conjunction (like opa record) of patterns,
     - disjonction (like or pattern and match) of patterns,
     - simple patterns (const and any),
     - and corresponding production (an expression) or an explicit recusion on a binded pattern variable




   To garantee that backtracking is not necessary, all intersections of branches in the disjunction must be empty.

   The algorithm can be summarized as
   1) make everything complete, instantiating _  and ... whenever possible
      this separates patterns as complete, unstrict and any with the order complete, unstrict and any
      i.e. no any pattern or unstrict must be checked before a complete pattern ...

      currently this can duplicate pattern and production,
      it is planned that production duplication will have no effect in the LLVM (sharing, need low level primitives)

   2) separate patterns as complete, unstrict and any

   3) regroup each pattern depending on the surface pattern
      a) reprocess each group as a subpattern
      b) when the group contains conjunction of pattern, nest the processing field by field (current implementation)
         or do the intersection of each field valid branch set (futur implemention).
         The nesting can exponentially duplicate pattern and production,
            but it garantees that no test is done twice (compared to backtracking, the ocaml approach, not planned),
            production will be shared in LLVM.
         The intersection approach garantees that no test is done twice and that no duplication occurs


   During the process missing case are detected (in this case a failure branch is introduced) and useless branch are removed.

   TODO : Useless pattern (i.e. hidden patterns) can be detected when a branch has been erased
   Currently a pass is doing this in opa, so it is not urgent
   It temporaly only work on typed pattern. With untyped pattern completion verification cannot be done.
*)

(** {6 Env} *)

(**
   For using the pass, the typer env is stored internally.
   At the end, you should reset env, for the GC.
*)
module Env :
sig
  val reset : unit -> unit
end

(** {6 Conversion} *)

(**
   A abtract type for representing an complete analysed pattern matching.
   Contains the analysed version of all patterns, and productions
*)
type pattern_matching

(**
   Given the ty of the matched expression, and the pattern matching,
   represented by a sorted list associating patterns and production,
   do the conversion, and produces an internal represention of the
   pattern matching

   The analyse may fail using [QmlError].
*)
val conversion :
  gamma:QmlTypes.gamma ->
  annotmap:QmlAst.annotmap ->
  label:Annot.label ->
  matched_expr:QmlAst.expr ->
  patterns:(QmlAst.pat * QmlAst.expr) list -> pattern_matching

(** {6 Normalization} *)

(**
   A private type for representing a normalized pattern matching.
*)
type normalized_pattern_matching

(**
   Pattern normalization.
   The normalization may fail using [QmlError].
*)
val normalize : pattern_matching -> normalized_pattern_matching

(** {6 Get back to qml} *)

(**
   Currently, the normalized code is not used.
   We use this function only for checking and debuging the pass.
   The normalized code may be used according to some heuristic.
   (e.g. if the size of the code does not explose)
*)
val generation :
  normalized_pattern_matching ->
  QmlAst.annotmap * QmlAst.expr

(** {6 Errors} *)

val has_public_exceptions : unit -> bool
val flush_exceptions_fmt : Format.formatter -> unit -> unit
