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
   Finding field for sum case detection
   @author Mathieu Barbin
   @author Valentin Gatien-Baron
   @author Francois Pessaux
*)

(**
   TODO(maybe): this module may be used by lot's of code.
   1) clever warning about pattern matching
   2) javascript optimized pattern compilation
   3) better server compilation

   Make it a separated lib, no only for JS.
*)

(**
   In the compilation of closed pattern matching in javascript,
   we have sequentiel [if] in which some affectations are done under some conditions.
   If an other pattern needs again to bind or to check for the same value,
   the question is if we can reuse the same identifier, or if we cannot
   because the conditions necessary for this identifier to be defined are
   not true in this case.

   Example:
   {[
   if ( c1 ) {
     a = m.toto
   }
   if ( c1 && c2 ) {
     b = a.tutu // there we can use a, because c1 && c2 => c1
   }
   c = m.toto.tata
   ]}

   The general problem is complex, but there, we need just a very small subset
   of utilisation, in which the structure of conditions are simple :
   The structure representing conditions contains just 2 terminal nodes,
   present, absent, a totologic node [True], and a conjonction node (flatened)
*)

(**
   The type of record field
*)
type field = string

(**
   The indexation supporting nested dot.
   Example:
   The [path] of [a.tl.tl.tl.hd] is [["tl"; "tl"; "tl" ; "hd"]]
*)
type path = field list

(**
   The indexation supporting nested dot, but in reverse order.
   Example:
   The field_path of [a.tl.tl.tl.hd] is [["hd"; "tl"; "tl"; "tl"]]

   <!> It is in reverse order for optimization purpose.
*)
type rev_path = field list

(**
   In some places, we manipulate set of field with all the same prefix.
   Instead of having a redondant parts in [rev_path], we give e.g. a tuple
   with a [rev_prefix_path] and a field set
*)
type rev_prefix_path = field list

(**
   For the heuristic for choosing the fields used for detecting the sum case,
   we may want to use a field rather than an other, depending on all the other
   tests needed on the same field, for sharing tests between guards.
   The priority is increasing, [0] is the lowest priority.
*)
type to_dot = int StringMap.t

(** {6 Conditions} *)

module SumCondition :
sig

  (**
     The abstract type of conditions.
  *)
  type t

  val pp : Format.formatter -> t -> unit

  (**
     The type of check on a field we can check in a pattern matching.
  *)
  type check =
    | Field of bool
        (**
           [true] means present, [false] means absent
        *)

    | Const of QmlAst.const_expr
        (**
           this check will test if a js value is equal to a qml constant
        *)

    | Size of int
        (**
           this check will test if the size of a js object (record) is
           of the given size.
        *)

  (**
     The item composing conditions.
     [Check] is followed by a bool indicating the presence of the path [true], or
     its absence [false]
  *)
  type decision =
    | True
    | Check of check * rev_path
    | And of decision list

  val pp_decision : Format.formatter -> decision -> unit

  (**
     The abstract type returned by the negation of a sum case
  *)
  type negation

  type implication = t * negation

  val pp_implication : Format.formatter -> implication -> unit

  exception Inconsistency

  (** {6 decision constructors} *)

  (**
     test the presence of a rev_path
  *)
  val present : rev_path -> decision

  (**
     test the absence of a rev_path
  *)
  val absent : rev_path -> decision

  (**
     test the presence or the absence of a rev_path,
     according to the bool
  *)
  val check_field : bool -> rev_path -> decision

  (**
     test a constant
  *)
  val const : rev_path -> QmlAst.const_expr -> decision

  (**
     test the size
  *)
  val size : rev_path -> int -> decision

  (**
     tell if a condition is a conjonction
  *)
  val is_conjonction : decision -> bool

  (**
     Regroup and flaten conjonctives decisions.
     In particular, proceed to simplifications such as :
     {[
     flattern And nodes
     [] => True
     And [] => True
     ]}
  *)
  val conjonction : decision list -> decision

  (**
     The empty condition. Equivalent to [true]
  *)
  val empty : t

  (**
     Tell if a condition is empty.
     Note that if the internal condition is empty,
     but contains some implications,
     this test returns false.
     Implementation in 0(1)
  *)
  val is_empty : t -> bool

  (**
     Add a guard to a pre-existing condition.
     The resulting condition may not be equivalent to [false],
     or in this case, it will raise [Inconsistency]

     Note that with the restricted condition we are considering,
     the logical and is the only operation for composing conditions.
  *)
  val add : decision -> t -> t

  (**
     Build a condition from a decision
  *)
  val decision : decision -> t

  (**
     At some point, you know that if a certain condition is true, then it
     implies a decision.
     Exemple:
     If you know [C], and you know [A => d], you may use:
     [let C' = add_implication A d C].
     If at some point you will add a decision in [C'] implying [A], then [d] will
     be infered as part of the implied decisions by [C']
  *)
  val add_implication : implication -> t -> t

  (**
     [implies a b] returns true if and only if [a => b]
     <!> Beware, this function will assert failure if [b] contains
     some implications.
  *)
  val implies : t -> t -> bool

  (**
     Filter the decision already implied by the condition.
     Return [True] in case the condition implies the decision
  *)
  val filter : t -> decision -> decision

  (**
     This is equivalent than [filter cond decision = True],
     and the implementation of [implies] can use the implementation
     of [filter]
  *)
  val implies_decision : t -> decision -> bool
end

(** {6 Env} *)

module SumEnv :
sig

  (**
     The environment of variable definitions, parametrized by the ident stored inside.
  *)
  type 'ident t

  (**
     The empty environment
  *)
  val empty : 'ident t

  (**
     Tell the implementation that this nested dot has been assigned to a new identifier,
     under a certain condition.
     Example:
     An assign has been generated, under the condition [Ci]
     {[
     v0 = x.a.b.c
     ]}
     So, we will call
     {[
     SumEnv.add ~rev_path:["c";"b";"a"] Ci v0 sum_env
     ]}
  *)
  val add_dot : rev_path:rev_path -> SumCondition.t -> 'ident -> 'ident t -> 'ident t

  (**
     Find the ident already assigned to this [rev_path] under a certain condition.
     If no such assignment has been done, or if some was, but under a stronger
     condition which is not implied by the current condition,
     the function will returns [None]
  *)
  val find_dot : rev_path:rev_path -> SumCondition.t -> 'ident t -> 'ident option

end

(**
   This module implements 2 functions related to sum case detection,
   and condition about presence or absence of fields in a sum case.
*)

(**
   The function [decide], with notation {[D()[]]} returns the list of
   possible minimal (suffisant) tests for deciding the case of the sum.

   Some examples:
   {[
   D(hd,tl|nil)[hd,tl] = present(hd) / present(tl) / absent(nil)
   D(hd,tl|nil)[nil] = present(nil) / absent(hd) / absent(tl)

   D(a|a,b)[a] = absent(b)
   D(a|a,b)[a,b] = present(b)

   D(a|b|a,b)[a] = absent(b)
   D(a|b|a,b)[b] = absent(a)
   D(a|b|a,b)[a,b] = present(a) AND present(b)
   ]}

   It is used for deciding with a number minimal of test in which
   case of a sum type we are.

   Some invariants about the returned list:

   1) if we note [#(c)] the number of terminal nodes of a condition,
   we have : it exists an int [i], such as for all [c] in [list], #(c) = i

   2) for all [c1] != [c2] in [list], not (c1 => c2)
   if we considere 1) verified, this means that order matters
*)

(**
   The function [condition] with notation {[C()[]]} returns the complete
   condition corresponding to the sum case.

   Some examples:
   {[
   C(hd,tl|nil)[hd,tl] = present(hd) AND present(tl) AND absent(nil)
   C(hd,tl|nil)[nil] = present(nil) AND absent(hd) AND absent(tl)

   C(a|a,b)[a] = present(a) AND absent(b)
   C(a|a,b)[a,b] = present(a) AND present(b)

   C(a|b|a,b)[a] = present(a)
   C(a|b|a,b)[b] = present(b)
   C(a|b|a,b)[a,b] = present(a) AND present(b)
   ]}

   It is used for building condition when we add definition of dots
   in the compilation of closed pattern matching.
*)

module SumAnalysis :
sig
  (**
     The abstract type of an analysed sum case.
     An analysed sum case has a notion of [rev_prefix_path], where sum structure
     are working as they were all at toplevel.
  *)
  type t

  (**
     Pretty printer for value of type [t]
     For debug only.
  *)
  val pp : Format.formatter -> t -> unit

  (**
     The type of a field list.
     assert: should be in lexicographic order
  *)
  type sum_case = StringSet.t
  type sum (* equivalent to field list list, with extra cached infos (types) *)

  (**
     Pretty printer for value of type [sum]
     For debug only.
  *)
  val pp_sum : Format.formatter -> sum -> unit

  (**
     Compute the fields cases corresponding to a sum or a record type.
     give the absolute [rev_path] where this sum appear in the pattern

     <!> Currently in closure mode, some matched expression are not annoted.
     The returned [sum] is then empty, and not any optimization are performed.
  *)
  val from_ty : rev_path:rev_path -> QmlTypes.gamma -> QmlAst.ty -> sum

  (**
     Built an analysed sum case. If the colvar is not the same than the colvar infered by typing
     in [from_ty], this will cause an internal error.
     In case we have both rowvar and colvar open, this means that this is just a syntactic rowvar.
  *)
  val make :
    sum ->
    rowvar:Imp_PatternAnalysis.rowvar ->
    colvar:Imp_PatternAnalysis.colvar ->
    rev_prefix_path:rev_prefix_path ->
    sum_case ->
    t

  (**
     Export the cases of the sum. If the sum is unknown, this will return [None]
  *)
  val cases : sum -> StringSet.t array option

  (**
     The condition implied by the sum.
     is [True] most of the time, but sometime, there are some possible informations,
     like {[(a|a,b) ==> present(a)]}

     If the sum is open in column, if the sum is undefined, we cannot imply anything. [True]
  *)
  val implies : sum -> SumCondition.decision

  (**
     Important remark:
     Here this is strongly dependant to the property that same field
     inside a sum type should necessary be on the same type.
     The path is for nested fields.
     Example, if {[ty = list(int)]}, then
     {[
     SumAnalysis.ty sum ["hd" ; "tl"; "tl" ; "tl"]
     ]}
     will return {[int]},
     because the type of {[a.tl.tl.tl.hd]} is {[int]}

     It is used combined with [Imp_Common.maybe_js_false]
     Assume the rev_path is not empty.

     Implementation request:
     make so that there is a cache in [sum] indexed
     by rev_path. We assume that the gamma used with
     a given sum object is the same during all the
     live of the sum object.

     If the sum is unknown, this will return a fresh [typevar]
  *)
  val ty : QmlTypes.gamma -> sum -> rev_path:rev_path -> QmlAst.ty

  (**
     This is an optimized shorthand for [from_ty (ty sum)] with a cache
     indexed by the rev_path
  *)
  val from_sum : QmlTypes.gamma -> sum -> rev_path:rev_path -> sum

  (**
     Returns a list of all possible decisions for being sure that this
     sum case is the correct one.
     We will use some other heuristic for choosing between this list
     of equivalent choices.

     Optimization:
     This function takes a [SumCondition.t] in argument,
     the current maximal condition, for beeing smarter about the
     decision to return, if some cases have already been invalidated.

     If the current condition implies the anlalysed case,
     This function would return [None], indicating than there is nothing to check.

     <!> The function may raises [Inconsistency]
  *)
  val decisions : SumCondition.t -> t -> SumCondition.decision list option

  (**
     Enrich the condition with all property enforced by this sum case.
     If the analysed case is unknown, this return the same cond
  *)
  val add : t -> SumCondition.t -> SumCondition.t

  (**
     Returns the negation of the sum case.
     Used for enriching the accumulator condition, for
     skiping some tests at last patterns.

     If the negation cannot be expressed into the simple
     decision algebra, the function will return [None].

     In particular, the negation of an open sum case, or an unknown sum
     is [None].
  *)
  val negation : SumCondition.t -> t -> SumCondition.negation option

  module Filter :
  sig

    (** {6 Choosing between decisions} *)
    (**
       For choosing between several decision, we will essentially use 2 heuristic, by priority :
       + sharing with dot binding variables in this pattern implying sub-checks and other patterns
       + using not maybe_js_false types for minimizing the size of the test
       + using presence rather than absence checks

       This fitering respect the following property:
       if the filter apply make so that the list become empty, the filter is ignored,
       and return the list received as input.
    *)

    (**
       First heuristic.
       The [to_dot] parameter contains priorities for sorting decisions,
       [0] is the lowest priority.
    *)
    val patvar :
      rev_prefix_path:rev_prefix_path ->
      to_dot:to_dot ->
      SumCondition.decision list -> SumCondition.decision list

    (**
       Second heuristic
    *)
    val non_js_false :
      gamma:QmlTypes.gamma ->
      sum:sum ->
      SumCondition.decision list -> SumCondition.decision list

    (**
       Last heuristic
    *)
    val present : SumCondition.decision list -> SumCondition.decision list

    (**
       Combine heuristic, and choose the first remaining decision.
    *)
    val final_choice :
      rev_prefix_path:rev_prefix_path ->
      gamma:QmlTypes.gamma ->
      sum:sum ->
      to_dot:to_dot ->
      SumCondition.decision list -> SumCondition.decision

  end
end
