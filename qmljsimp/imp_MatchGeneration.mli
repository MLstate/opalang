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
   Generation of pattern matching
   @author Mathieu Barbin
   @author Valentin Gatien-Baron
*)

(**
   Note for hacker:

   Since this module is private to the jsimp compiler, the interface of this module
   is volontary too big, and export too much functions.
   The goal of this is to have an internal documentation of implementation on the
   form of a standard mli file.
*)

(** {6 type alias} *)

type env = Imp_Env.env
type penv = Imp_Env.private_env
type condition = Imp_SumCase.SumCondition.t
type decision = Imp_SumCase.SumCondition.decision
type check = Imp_SumCase.SumCondition.check
type path = Imp_SumCase.path
type rev_path = Imp_SumCase.rev_path
type guard = JsAst.expr
type rev_guards = guard list
type binding = JsAst.ident * JsAst.expr
type bindings = binding list
type pat = Imp_PatternAnalysis.pat
type 'right_hand rev_cases = (guard option * bindings * 'right_hand) list

(**
   The type of an global analysis of a match.
   Should be built with all cases of the match.
   This is a local environment, used at the level of
   the match, for compiling each pattern.
*)
type menv

(** {6 Guards} *)

type special_bool_hack = [
  | `maybebool of bool
      (**
         means that the dot should be perform with a safety runtime function
         working also on native bool.
      *)

  | `onlybool of bool
      (**
         means that the test can be performed on bool directly
      *)

  | `partialbool of bool
      (**
         the test should be strict
      *)
]

(**
   In javascript, an assignment expression is evaluated as the assigned expression.
   e.g., [x=5] is evaluated to [5]. In the generation of pattern matching, we take
   advantage of this, for computing both at the same time pattern-checking,
   and pattern-binding.

   Example, for matching a list, we will generate from:
   {[
   match e with
   | [ hd = v0 | tl = v1 ]
   | []
   ]}
   a code like:
   {[
   if ( v1 = e.tl, v1 != undefined ) {
     v0 = e.hd ;
     <right-side>;
   } else {
     <right-side> ;
   }
   ]}

   In an other hand, some js value can never be evaluated to false.
   We take also benefits of this for having shorter guards.
   Example, for list matching, we know that a list is an js object,
   and a js object is equivalent to [true] in a conditional,
   so, we will finally replace the guard
   {[
   if ( v1 = e.tl, v1 != undefined )
   ]}
   by
   {[
   if ( v1 = e.tl )
   ]}

   Details about params [assign] and [ident]
   If the ident is Some, is should necessary be the ident of the assignement.
   {[
   assign: v0 = a.x
   ident: Some v0
   ]}
   will build one of the following forms:
   {[
   (v0 = a.x, v0 != undefined)
   ]}
   {[
   (v0 = a.x, v0 == undefined)
   ]}
   {[
   v0 = a.x
   ]}
   {[
   !(v0 = a.x)
   ]}
   If the expr should not be kept in a comma, we use the assign as the ident.
   In this case, use [None] for the ident:
   {[
   assign: v0
   ident: None
   ]}
   will build one of the following forms:
   {[
   v0 != undefined
   ]}
   {[
   v0 == undefined
   ]}
   {[
   v0
   ]}
   {[
   !v0
   ]}
*)
val aux_build_guard :
  env:env ->
  menv:menv ->
  rev_path:rev_path ->
  assign:JsAst.expr ->
  ident:JsAst.ident option ->
  check:check ->
  special_bool_hack:special_bool_hack option ->
  guard

(** {6 Nested Guard} *)

(**
   add in the current guards the necessary guard to assign and check that
   the matched expression verify the nested guard.
   Examples.
   From:
   {[
   present(m.a.b.c.d)
   ]}
   To:
   {[
   v0 = m.a &&
   v1 = v0.b &&
   v2 = v1.c &&
   v3 = v2.d
   ]}
   We use the optimization about maybe_js_false if possible, but in cases where it is not,
   it will generated something like;
   {[
   v0 = m.a && v0 != undefined &&
   v1 = v0.b && v1 != undefined &&
   v2 = v1.c && v2 != undefined &&
   v3 = v2.d && v3 != undefined.
   ]}
   We give the path concerned by the guard, and the kind of check we are talking about:
   -present
   -absent
   -const

   Important remarks:
   This function does not look into the SumEnv for previous assigment,
   this is done by the function [aux_build_decision].
   We assume that all dot asked have not yet be assigned.
   For optimizing next lookup, we should enrich the [SumEnv.t].
   <!> condition of insertion in sum env are growing up during the recursives call,
   as condition are added to the guards.
*)
val build_guard :
  env:env ->
  penv:penv ->
  menv:menv ->
  minimal_condition:condition ->
  rev_guards:rev_guards ->
  matched:JsAst.expr ->
  check:check ->
  special_bool_hack:special_bool_hack option ->
  rev_path:rev_path ->
  path:path ->
  penv * menv * condition * rev_guards

(** {6 Decisions} *)

(**
   Essentially like [build_nested_dot], but sharing already assigned variables.
   This function add to the guards the necessary checks for validating the given path.

   It is used as an auxilary function of [build_decision].
   This work directly on the  [rev_path] contained in the decision node.
*)
val aux_build_decision :
  env:env ->
  penv:penv ->
  menv:menv ->
  minimal_condition:condition ->
  rev_guards:rev_guards ->
  matched:JsAst.expr ->
  check:check ->
  special_bool_hack:special_bool_hack option ->
  rev_path:rev_path ->
  penv * menv * condition * rev_guards

(**
   Tranform a decision into a guard list.
   <!> Append to the guard list in reverse order.

   Arguments are needed for:
   -sum: for optimizing non_js_false cases.
   -sum_env: for looking for previous dots, even partially on the path
   -the current growing condition, for knowing if we are allowed to use the previous affectations,
   and for adding new ident in the SumEnv.
   This current growing condition can start from [True], because each previous case has been executed.
   So we have 2 forms of condition. The one beeing used for storing in SumEnv (starting from 0)
   and the one aggregating everything, for finding in SumEnv
   -the current field_path
*)
val build_decision :
  env:env ->
  penv:penv ->
  menv:menv ->
  minimal_condition:condition ->
  rev_guards:rev_guards ->
  matched:JsAst.expr ->
  decision:decision ->
  penv * menv * condition * rev_guards

(**
   Build a dot for binding.
   We assume for building this dot that the check assuring that this dot can be done without
   any check. All checks have already be done in guards.
   Use previous shared assignement.
   Some example:
   -if the rev_path is empty, this return directly the matched expression
   -if there is not any previous shared dot, it returns an nested dot:
   {[
   build_dot ~rev_path:["c"; "b"; "a"] ~matched:x
   ]}
   returns
   {[
   x.a.b.c
   ]}
*)
val build_dot :
  menv:menv ->
  matched:JsAst.expr ->
  rev_path:rev_path ->
  maybebool:bool ->
  JsAst.expr

(**
   Return elements of javascript for compiling an analysed pattern.

   The returned [cond] and [bindings] should be used in this way
   {[
   if ( cond ) {
     bindings;
     <right-side>
   } else
   if ( cond ) {
     bindings;
     <rigth-side>
   } else {
     match_failure()
   }
   ]}

   Note about minimal_condition:
   The minimal condition restart for each new pattern, but the maximal condition
   takes advantage and informations from previous matched patterns, and grows up
   from a pattern to an other. The maximal condition is in the menv

   When we are processing to the negation of condition,
   if it leads to an inconsistency, that means that the
   pattern is exhaustive.

   Static invalidation:
   if the pattern is invalidate statically, the function
   will return None, and the pattern is skipped of the code.
*)
val aux_pattern :
  env:env ->
  penv:penv ->
  menv:menv ->
  matched:JsAst.expr ->
  pat:pat ->
  (penv * menv * guard option * binding list) option

(**
   [penv, (cond, bindings) list = ClosedGeneration.compile_pat env penv matched matched_ty patterns]
   If we have an option at some point, we do not complete with a match failure message,
   and we ignore the rest of the match, and should warn something !
*)
val aux_compile :
  env:env ->
  penv:penv ->
  matched:JsAst.expr ->
  ty:QmlAst.ty ->
  patterns:(pat * 'right_hand) list ->
  penv * 'right_hand rev_cases

(**
   Call aux_compile, and build the global expression.
   The [matched] and the right productions in patterns are simply the traduction of qml expression,
   using the projection [compile_expr_to_expr]
*)
val compile :
  env:env ->
  penv:penv ->
  pos:FilePos.pos ->
  (**
     The pos of the full pattern matching, for producing a match failure message
  *)

  matched:JsAst.expr ->
  (**
     The matched expression.
     <!> If this expression is not an identifier, this will generate
     code duplication, because the matched expression needs to appear
     several time in the compilation of patterns (dot, etc.)
  *)

  ty:QmlAst.ty ->
  patterns:(pat * JsAst.expr) list ->
  penv * JsAst.expr

(**
   Trivial patterns
*)
module T :
sig
  (**
     Trivial match are pattern matching on constants.
     The compilation of such patterns is simply compiled as sequential comparison.
     TODO:We could envisage to compile them using BDD, if needed.
  *)

  type pat = Imp_PatternAnalysis.T.pat

  val aux_compile :
    env:env ->
    penv:penv ->
    matched:JsAst.expr ->
    ty:QmlAst.ty ->
    patterns:(pat * 'right_hand) list ->
    penv * 'right_hand rev_cases

  val compile :
    env:env ->
    penv:penv ->
    pos:FilePos.pos ->
    matched:JsAst.expr ->
    ty:QmlAst.ty ->
    patterns:(pat * JsAst.expr) list ->
    penv * JsAst.expr

end

(**
   Special cases (ad hoc optimizations)
*)
module AdHoc :
sig
  val compile :
    env:env ->
    penv:penv ->
    matched:JsAst.expr ->
    ty:QmlAst.ty ->
    patterns:(QmlAst.pat * JsAst.expr) list ->
    (penv * JsAst.expr) option
end
