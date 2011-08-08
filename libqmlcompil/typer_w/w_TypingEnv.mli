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

(* ************************************************************************** *)
(** {b Descr} This module provides typing environments manipulation.          *)
(* ************************************************************************** *)



type t = {
  ty_env_local : (QmlAst.ident * W_Algebra.types_scheme) list ;
  ty_def_env_local : (QmlAst.TypeIdent.t * (W_Algebra.types_scheme * int)) list;
  ty_env_qml_global : QmlTypes.Env.t
}

(* ************************************************************************** *)
(** {b Descr}: Empty the memoization table for values type schemes. This
    function *MUST* be called before each package compilation otherwise we
    accumulates schemes fom previous packages that have been renamed with
    different refreshed (for separate compilation stuff).
    Not calling it has the disastrous consequence that when compiling a package
    P1, the table is filled with schemes renamed for P1. Then when compiling
    a package P2 (opa.exe is still running without having ended) then schemes
    of values already used by P1 and now used by P2 are already in the table,
    but with the refreshed variables for P1, not those for P2. As a result,
    we typecheck P2 with type variables belonging to P1 and then explicit
    instantiation fails because it uses, itself, variables renamed for P2.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
val reset_toplevel_tydefs_schemes_env_memo: unit -> unit

(* ************************************************************************** *)
(** {b Descr}: Empty the memoization table for type definitions type schemes.
    See documentation of the above function
    [reset_toplevel_tydefs_schemes_env_memo] for the reason of such a mechanism
    is needed.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
val reset_toplevel_valdefs_schemes_env_memo: unit -> unit

val extend_toplevel_valdefs_env_memo:
  Ident.t -> W_Algebra.types_scheme -> unit

val empty_typing_env: t
val from_qml_typing_env: QmlTypes.Env.t -> t

val new_empty_variables_mapping: unit -> unit
val new_inheriting_variables_mapping: unit -> unit
val release_variables_mapping: unit -> unit
val reset_empty_variables_mapping_on_error: unit -> unit

val automatically_add_type_construtor_arguments_if_omitted:
  W_Algebra.simple_type list -> int -> W_Algebra.simple_type list

exception Importing_qml_abstract_ty

val qml_type_to_simple_type:
  t -> QmlAst.ty -> is_type_annotation: bool -> W_Algebra.simple_type

val get_ident_type_and_annotmap_scheme:
  QmlAst.ident -> t ->
  (W_Algebra.simple_type *
   (W_Algebra.simple_type, unit) QmlGenericScheme.tsc)

val find_type: QmlAst.TypeIdent.t -> t -> W_Algebra.types_scheme

val forward_ref__unify_simple_type:
  (t -> W_Algebra.simple_type -> W_Algebra.simple_type -> unit) ref
val forward_ref__unify_row_type:
  (t -> W_Algebra.row_type -> W_Algebra.row_type -> bool) ref
val forward_ref__unify_column_type:
  (t -> W_Algebra.column_type -> W_Algebra.column_type -> unit) ref
