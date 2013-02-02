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



val generic_binding_level: int
val current_binding_level: int ref

val begin_definition: unit -> unit
val end_definition: unit -> unit
val restore_binding_level_from_backup: int -> unit
val reset_toplevel_binding_level_on_error: unit -> unit

val new_annotations_generalizable_level: unit -> unit
val release_annotations_generalizable_level : unit -> unit

val __row_variable: unit -> W_Algebra.row_variable
val __column_variable: unit -> W_Algebra.column_variable

val __generic_row_variable: unit -> W_Algebra.row_variable
val __generic_column_variable: unit -> W_Algebra.column_variable
val __generic_type_variable: unit -> W_Algebra.simple_type

val row_ending_variable: unit -> W_Algebra.row_type_ending
val column_ending_variable: unit -> W_Algebra.column_type_ending

val __row_variable_with_public_identity:
  QmlTypeVars.RowVar.t -> W_Algebra.row_variable
val __column_variable_with_public_identity:
  QmlTypeVars.ColVar.t -> W_Algebra.column_variable
val __type_variable_with_public_identity:
  QmlTypeVars.TypeVar.t -> W_Algebra.simple_type
val __annotation_row_variable_with_public_identity:
  QmlTypeVars.RowVar.t -> W_Algebra.row_variable
val __annotation_column_variable_with_public_identity:
  QmlTypeVars.ColVar.t -> W_Algebra.column_variable
val __annotation_type_variable_with_public_identity:
  QmlTypeVars.TypeVar.t -> W_Algebra.simple_type

val __generic_row_variable_with_public_identity:
  QmlTypeVars.RowVar.t -> W_Algebra.row_variable
val __generic_column_variable_with_public_identity:
  QmlTypeVars.ColVar.t -> W_Algebra.column_variable
val __generic_type_variable_with_public_identity:
  QmlTypeVars.TypeVar.t -> W_Algebra.simple_type

val type_variable: unit -> W_Algebra.simple_type
val type_named:
  W_Algebra.type_name_ident -> int -> W_Algebra.simple_type list ->
  W_Algebra.simple_type option -> W_Algebra.simple_type
val type_int: unit -> W_Algebra.simple_type
val type_bool: unit -> W_Algebra.simple_type
val type_float: unit -> W_Algebra.simple_type
val type_string: unit -> W_Algebra.simple_type
val type_arrow:
  W_Algebra.simple_type list -> W_Algebra.simple_type -> W_Algebra.simple_type
val type_closed_record:
  (string * W_Algebra.simple_type) list -> W_Algebra.simple_type
val type_module_record:
  (string * W_Algebra.simple_type) list -> W_Algebra.simple_type
val type_opened_record:
  (string * W_Algebra.simple_type) list -> W_Algebra.simple_type
val type_of_record_pattern:
  row_is_opened: bool -> (string * W_Algebra.simple_type) list ->
      W_Algebra.simple_type
val record_extention_min_expected_ty_and_result_ty:
  string -> W_Algebra.simple_type ->
  (W_Algebra.simple_type * W_Algebra.simple_type)

val change_ty_var_level: W_Algebra.type_variable -> int -> unit
val change_row_var_level: W_Algebra.row_variable -> int -> unit
val change_column_var_level: W_Algebra.column_variable -> int -> unit
val change_ty_link:
  receiver: W_Algebra.simple_type -> link_val: W_Algebra. simple_type option ->
  unit
val change_row_var_link:
  receiver: W_Algebra.row_variable ->
  link_val: W_Algebra.row_variable_value -> unit
val change_column_var_link:
  receiver: W_Algebra.column_variable ->
  link_val: W_Algebra.column_variable_value -> unit

type unification_changes_checkpoint
val reset_unification_changes_trace: unit -> unit
val get_current_changes_checkpoint: unit -> unification_changes_checkpoint
val rewind_unification_changes:
  performed_after: unification_changes_checkpoint -> unit


val simple_type_repr: W_Algebra.simple_type -> W_Algebra.simple_type
val row_type_repr: W_Algebra.row_type -> W_Algebra.row_type
val column_type_repr: W_Algebra.column_type -> W_Algebra.column_type

val cleanup_simple_type: W_Algebra.simple_type -> unit
val cleanup_row_type: W_Algebra.row_type -> unit
val cleanup_column_type: W_Algebra.column_type -> unit

val named_type_expr_height: W_Algebra.named_simple_type -> int
