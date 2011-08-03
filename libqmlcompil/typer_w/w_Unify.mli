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
(*
   @author François Pessaux
*)

type detail_kind =
  | DK_none
  | DK_fun_type_arity of (int * int)
  | DK_named_type_arity of (W_Algebra.type_name_ident * int * int)
  | DK_binding_level_mismatch
  | DK_forall_type_quantification_arity of (int * int)

type unification_conflict_detail = {
  ucd_kind : detail_kind ;
  ucd_through_field : string option (** If the unification conflict appeared
     through the unification of 2 record fields of same name, this is the name
     of the 2 fields that caused the error. *)
}

exception Unification_simple_type_conflict of
  (W_Algebra.simple_type * W_Algebra.simple_type * unification_conflict_detail)

exception Unification_column_conflict of
  (W_Algebra.column_type * W_Algebra.column_type)

(** @raise Unification_simple_type_conflict *)
val unify_simple_type:
  W_TypingEnv.t -> W_Algebra.simple_type -> W_Algebra.simple_type -> unit

(** @raise Unification_simple_type_conflict if rows had a same field having
    2 different types or return [false] in case of failure not due to fields
    having different types but because then rows can't unify due to the fact
    they have different fields and are not opened to each "absorb" the fields
    coming from the other row. *)
val unify_row_type:
  W_TypingEnv.t ->W_Algebra.row_type -> W_Algebra.row_type -> bool

(** @raise Unification_simple_type_conflict
    @raise Unification_column_conflict *)
val unify_column_type:
  W_TypingEnv.t ->W_Algebra.column_type -> W_Algebra.column_type -> unit
