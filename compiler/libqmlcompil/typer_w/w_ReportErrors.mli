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
(*
   @author Fran�ois Pessaux
*)

val set_annotmap_for_error_report: QmlAst.annotmap option -> unit
val get_annotmap_for_error_report: unit -> QmlAst.annotmap

val pp_unification_conflict_detail:
  Format.formatter -> W_Unify.unification_conflict_detail -> unit
val report_unification_conflict_with_context:
  W_TypingEnv.t ->
    (W_InferErrors.unification_conflict_context * W_Algebra.simple_type *
     W_Algebra.simple_type * W_Unify.unification_conflict_detail) -> 'a
val report_cyclic_or_ill_formed_type_in_expr:
  W_AnnotMap.annotmap -> QmlAst.expr -> 'a
