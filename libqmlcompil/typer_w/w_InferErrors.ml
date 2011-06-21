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
type unification_conflict_context =
  | UCC_pattern_coerce of
      (QmlAst.pat *
       W_Algebra.simple_type *  (** Pattern type. *)
       W_Algebra.simple_type)   (** Coercing type. *)
  | UCC_apply of
      (QmlAst.expr *
       W_Algebra.simple_type *  (** Functional part. *)
       W_Algebra.simple_type)   (** Arguments types -> 'a. *)
  | UCC_match_left_part_ty_previous_vs_ty_current of
      (QmlAst.expr *
       W_Algebra.simple_type *
       W_Algebra.simple_type)
  | UCC_match_ty_right_parts_vs_ty_branch of
      (QmlAst.expr *
       W_Algebra.simple_type *
       W_Algebra.simple_type)
  | UCC_dot of
      (QmlAst.expr *
       W_Algebra.simple_type *  (** Record expression type. *)
       W_Algebra.simple_type *  (** Tmp record with only the accessed field. *)
       string)                  (** Label used for access. *)
  | UCC_record_extend of
      (QmlAst.expr *
       W_Algebra.simple_type *  (** Extended expression type. *)
       W_Algebra.simple_type)   (** Extension type. *)
  | UCC_coerce of
      (QmlAst.expr *
       W_Algebra.simple_type *  (** Expression type. *)
       W_Algebra.simple_type)   (** Coercing type. *)
  | UCC_let_rec_body of
      (Ident.t * QmlAst.expr *
       W_Algebra.simple_type *  (** Body type. *)
       W_Algebra.simple_type)   (** Tmp expected type. *)
  | UCC_unknown_directive of
      (QmlAst.expr *
       W_Algebra.simple_type *  (** Directive expected type. *)
       W_Algebra.simple_type)   (** Inferred type. *)
  | UCC_catch of
      (QmlAst.expr *
       W_Algebra.simple_type *  (** Handler expected type. *)
       W_Algebra.simple_type)   (** Handler effective type. *)
  | UCC_throw of
      (QmlAst.expr *            (** Thrown expression. *)
       W_Algebra.simple_type *  (** Current type of exceptions. *)
       W_Algebra.simple_type)   (** Effective type of thrown expression. *)



(* ************************************************************************** *)
(** {b Descr}: Exception raised when an unification error is detected during
    inference at a point where it is possible to give more information to the
    user than simply the 2 incriminated types shown by the unification routine
    proper exception.
    This exception embeds a detailed position of the construct where the
    unification failed and the 2 types finally incriminated in the unification
    routine proper exception.
    The detailed position of the construct embeds the expression or pattern
    where the error occurred and also the original types initially to unify
    when the unification routine was called.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
exception Infer_detailled_unification_conflict of
  (unification_conflict_context *
   W_Algebra.simple_type * (* First type pointed by the unification routine. *)
   W_Algebra.simple_type *  (* Second type pointed by the unification
                               routine. *)
   W_Unify.unification_conflict_detail)



exception Infer_private_type_not_opaque of
  (Ident.t *    (** The name of the definition that could not be
                               generalized. *)
   QmlAst.expr *  (** The body expression of the definition that could not be
                      generalized. *)
   W_Algebra.simple_type *  (** The type of the definition that could not be
                                generalized. *)
   W_Algebra.simple_type)   (** The precise private type (part of the above
                                type obvioously) causing the definition to be
                                not generalizabled. *)
