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
   This module is for various utility functions on QML AST.
   See also QmlAst, QmlAstWalk and QmlAstCons for more basic operations.

   @author Louis Gesbert
   @author Rudy Sicard
   @author Esther Baruk
   @author Mathieu Barbin
   @author Valentin Gatien-Baron
   @author Quentin Bourgerie
*)

(** {6 Design Note (TODO)} *)

(**
   TODO:introduce in the ast definition a notion of structural ignored
   directives for common utils. By default, an util must traverse all
   these directives.

   {[
   type structural_ignored_directives = [ `tracker | `coerce, etc..]
   let util ... =
     let rec aux ... = function
      | Directive (#structural_ignored_directive, e, ...) -> aux e
      | ...
   ]}

   TODO:refactor with Lang design.
   Currently a lot of utils are considering some assemptions about the
   expr that are applied to, and so have [assert false] or [invalid_arg]
   in their implementation.
   Instead of defining them to the type [expr], they should take arguments
   of the corresponding constructor.

   TODO:the utils should define a type 'a utils working an arguments of the
   constructor.

   {[
   (* in Lang.Ast *)
   type expr =
     | A of int
     | B of expr * expr
     | Directive of (variant, ....)

   module QmlUtils =
   struct
     module B =
     struct
       let utils_1 e e' = <impl>
       instead of
       let old_utils_1 e =
          match e with
          | B (e, e') -> <impl>
          | _ -> assert false
     end
   end
   ]}
   Typically, user of utils are doing things like :
   {[
   match e with
   | ....
   | B (a, b) ->
      (* oups, I need an utils on B *)
      B.utils a b
      (* instead of *)
      B.old_utils e
   ]}
*)

val map_exprident : QmlAst.code -> ( Ident.t -> Ident.t ) ->  QmlAst.code

(**
   take the deeper expression, go through all letin, lambda ...,
   except paramater can be used to stop on a particular expression
*)
val get_deeper_expr : ?except:(QmlAst.expr-> bool) -> QmlAst.expr -> QmlAst.expr
(**
   substitute old_expr new_expr global_expr
   =>replace in global_expr every occurrence of old_expr (based on the annotation number) with new_expr
*)
val substitute : QmlAst.expr -> QmlAst.expr -> QmlAst.expr -> QmlAst.expr
(**
   collect sub_expr global_expr
   =>collect all occurrence of sub_expr (based on the annotation number)
*)
val collect : QmlAst.expr -> QmlAst.expr -> QmlAst.expr list
(**
   collect_annot sub_expr_annot global_expr
   =>same as collect, but you give only the annotation number
*)
val collect_annot : Annot.t -> QmlAst.expr -> QmlAst.expr list

(**
   checks whether a Qml expression is expensive or not
*)
val is_expansive : QmlAst.expr -> bool
val is_expansive_strict : QmlAst.expr -> bool
val is_expansive_with_options : [`disabled|`normal|`strict] -> (QmlAst.expr -> bool)

module App : sig
  (**
     The type of utils for an [Apply] Node
  *)
  type 'a util = QmlAst.expr -> QmlAst.expr list -> 'a

  (**
     Gives the number of arguments with which an expression is applied
     Example:
     {[
     (((f x) y) z)
     ]}
     The [nary_args_number] is [1].

     {[
     ((f x) y z)
     ]}
     The [nary_args_number] is [2].

     Not Implemented because QmlAst is not ready yet for nary applications.
     Currently the implementation is [assert false]

     {[
     | Apply (f, args) -> nary_args_number f args
     ]}
     The argument [f] is not used, but we follow the interface of App.
  *)
  val nary_args_number : int util

  (**
     Gives the number of arguments with which an expression is applied
     Example:
     {[
     (((f x) y) z t)
     ]}
     The [curryfied_args_number] is [4].
     @see "nary_args_number" for nary support
  *)
  val curryfied_args_number : int util

 (** {6 Old Util: TODO use util type} *)

  val to_list : ?strict:bool -> QmlAst.expr -> QmlAst.expr list
  (** transform an apply() to a list of function :: args
      @param strict if [true], means there must be at list one apply node
                    for this function to succeed (so the output list has at least length 2)
                    if [false], this function never fails
                    Default is [true]
  *)

  val from_list : QmlAst.expr list -> QmlAst.expr
  (**
     inverse of to_list, regardless of the [strict] flag that was used
     @raise Invalid_argument if the list is empty
  *)

end

module ExprIdent :
sig
  (**
     get the uniq ident string from an ident expression
  *)
  val string : QmlAst.expr -> string

  (**
     change the content of an ident keeping the same annotation
  *)
  val change_ident : QmlAst.ident -> QmlAst.expr -> QmlAst.expr

  (**
     substitute all occurrences of an ident by another expression
     dont care about annotmap and annot unicity, you are warned
     can embbed side effet in the ident substitution map,
     to count substitution for instance
  *)
  val substitute : (unit -> QmlAst.expr) IdentMap.t -> QmlAst.expr -> QmlAst.expr
end

module Lambda :
sig
  (**
     The type of utils for a [Lambda] Node

     The functions must take the two arguments of the constructor :
        the ident and the expression

     Example :
     {[val toto e = match e with
       | Lambda (params, expr) -> QmlAstUtils.curryfied_arity params expr
       | _ -> 0]}

     gives the curryfied_arity of the expression [e], assuming that it is a lambda
     or 0 instead.
  *)
  type 'a util = QmlAst.ident list -> QmlAst.expr -> 'a

  (**
     Returns the number of arguments of [lambda] taking in consideration the nary informations.
     Examples :
     {[
     fun x -> fun y, z -> x + y
     ]}
     The [nary_arity] is [1], where the [curryfied_arity] is [3]
  *)
  val nary_arity : int util

  (**
     Returns the number of arguments of a lambda without distinction between a function
     which returns a function and its curryfied version.

     {[
     fun x -> fun y, z -> x + y
     ]}
     The [curryfied_arity] is [3], where the [nary_arity] is [1]
  *)
  val curryfied_arity : int util

  (** {6 Old Utils: TODO use lambda_utils type} *)

  (**
     The function that count successive lambda node, traversing coercion node only
     Examples :
     count {[
     fun x -> fun y, z -> x + y
     ]}

     return 3

     @deprecated use [curryfied_arity] instead
  *)
  val count : QmlAst.expr -> int

  (** eta-expands an expression by int argument *)
  val eta_expand_ast :  int -> QmlAst.expr -> QmlAst.expr

end

module Const : sig
  (**
     Compare at compile time 2 constants.
     Assume that the two constant are of the same type,
     assert false otherwise.
  *)
  val compare : QmlAst.const_expr -> QmlAst.const_expr -> int

  (**
     Checks if compare returns 0
  *)
  val equal : QmlAst.const_expr -> QmlAst.const_expr -> bool
end

module Coerce : sig
  (** remove all nested coerces at the root of the expression, and keep information to recoerce
      as a list of annotation and type *)
  val uncoerce : QmlAst.expr -> QmlAst.expr * (Annot.label * QmlAst.ty) list

  (** inverse of uncoerce
      warning: the annotations are restored as they were (no consistency with an annotmap in case of type change) *)
  val recoerce : QmlAst.expr -> (Annot.label * QmlAst.ty) list -> QmlAst.expr

  (** non reversible coerce removing *)
  val rm_coerces : QmlAst.expr -> QmlAst.expr
end

(** Returns an IdentSet.t of the free vars in an expression *)
module FreeVars :
sig
  val pat  : QmlAst.pat -> IdentSet.t
  val expr : QmlAst.expr -> IdentSet.t

  val pat_fold  : ('a -> Annot.t -> QmlAst.ident -> 'a) -> QmlAst.pat -> 'a -> 'a
  val expr_fold : ('a -> Annot.t -> QmlAst.ident -> 'a) -> QmlAst.expr -> 'a -> 'a
end

(**
   Utils on Record node.
*)
module Record :
sig
  type 'a util = (string * QmlAst.expr) list -> 'a

  (**
     uncons a tuple.
     If the record is a standard tuple, ["f1", "f2", .. "fn"], will return
     an option of the list of data of length [n]. if not, returns [None]
  *)
  val uncons_tuple : QmlAst.expr list option util

  (**
     special case for deprecated qml couple.
     @deprecated Opa tuple are now the standard tuples.
  *)
  val uncons_qml_tuple : QmlAst.expr list option util

  (**
      Uncons a record returning the list of its fields and the list of its expressions
  *)
  val uncons : (string list * QmlAst.expr list) util

  (**
     Construct a record given the list of its fields and the list of expressions corresponding to the fields
  *)
  val cons : string list -> QmlAst.expr list -> QmlAst.expr

end

(**
   Utils on tuples (Decons).

   In the opa compiler, a tuple is a standard record where fields are nammed ["f1", "f2", "f3", etc...]

   In qml side, it used to have only couple ["fst", "snd"].
   Qml couple are deprecated, but still used in existing code.
   Please, do not use them in new code, use only standard tuples.

   Some utils are related to Types. QmlAstUtils and QmlTypesUtils will be merged
   into QmlUtils, taking part of qmllang.

   For constructing expression or type, cf module [QmlAstCons]
*)
module Tuple :
sig
  (**
     Will call internally [Record.uncons_tuple].
     If the expression is not a record, will return [None]
  *)
  val uncons : QmlAst.expr -> QmlAst.expr list option

  (**
     Inspect a typeident and see if it is a tuple type. If the type is a tuple, returns its arity.
     if not, returns [None]
  *)
  val uncons_typeident : QmlAst.TypeIdent.t -> int option

  (**
     Will call internally [Record.uncons_qml_couple].
     If the expression is not a record, will return [None].
     @deprecated Opa tuple are now the standard tuples.
  *)
  val uncons_qml_tuple : QmlAst.expr -> QmlAst.expr list option
end

(**
   Utils on patterns
*)
module Pat :
sig
  type 'a util = QmlAst.pat -> 'a

  (** Tell if the pat is [true] or [false], traversing patcoercion of [TypeName "bool"] or structural patcoerce *)
  val is_bool : bool option util
end

(**
   Utils on Match node
*)
module Match :
sig
  type 'a util = QmlAst.expr -> (QmlAst.pat * QmlAst.expr) list -> 'a

  (** Uncons a match which was built with QmlAstCons.ifthenelse *)
  val uncons_ifthenelse : (QmlAst.expr * QmlAst.expr * QmlAst.expr) option util

  (**
      Uncons a match, returning a triplet of
      - the expression matched,
      - the list of patterns,
      - the list of resulting expressions
      (elements in the last two lists have corresponding orders)
  *)
  val uncons : (QmlAst.expr * QmlAst.pat list * QmlAst.expr list) util

  (**
      Construct a match given
      - its expression matched,
      - its list of patterns,
      - its list of resulting expressions
      (elements in the last two lists have corresponding orders)
  *)
  val cons : QmlAst.expr -> QmlAst.pat list -> QmlAst.expr list -> QmlAst.expr
end

(**
    Utils on LetIn node
*)
module LetIn :
sig
  type 'a util = (QmlAst.ident * QmlAst.expr) list -> QmlAst.expr -> 'a

  (** Uncons a LetIn node, returning the pair of the list of declarations list and the last expression *)
  val uncons : ((QmlAst.ident * QmlAst.expr) list list * QmlAst.expr) util

  (** Construct a LetIn node given the list of declaration list and the last expression *)
  val cons : (QmlAst.ident * QmlAst.expr) list list -> QmlAst.expr -> QmlAst.expr
end

(**
    Utils on LetRecIn node
*)
module LetRecIn :
sig
  type 'a util = (QmlAst.ident * QmlAst.expr) list -> QmlAst.expr -> 'a

  (** Uncons a LetRecIn node, returning the pair of the list of declarations list and the last expression *)
  val uncons : ((QmlAst.ident * QmlAst.expr) list list * QmlAst.expr) util

  (** Construct a LetRecIn node given the list of declaration list and the last expression *)
  val cons : (QmlAst.ident * QmlAst.expr) list list -> QmlAst.expr -> QmlAst.expr
end

(**
   Utils on full code
*)
module Code :
sig

  (**
     Insertion of a portion of code with dependencies.
     The code is inserted just after the first dependencies starting from the end of the code.
     Example:
     {[
     insert (["a"; "b"; "c"], "val h = a+b+c", ...)

     g = 7
     a = 6
     b = 7
     c = 7
     h = a+b+c
     ...
     ]}
  *)
  val insert :
    deps:IdentSet.t ->
    insert:QmlAst.code ->
    QmlAst.code ->
    QmlAst.code
end
