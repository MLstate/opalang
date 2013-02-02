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
   @author François Pessaux
*)

(* ************************************************************************** *)
(** {b Descr}: This module implements the typechecker's type algebra
    data-structure. This module should export opaque data-types in order to
    prevent savage manipulation breaking invariants (mostly sharing and
    canonical representation not being equal to structural representation), but
    in order to avoid having one huge implementation module containing all the
    functions that require manipulating the algebra structure, we preferred to
    export the algebra.
    Hence, "people" seeing it must be aware of invariants and must avoid as far
    as possible to manipulate these data-structure manually, without using the
    helper functions. "Dangerous functions", i.e. function that may break
    invariants if called without respecting protocol described in their
    documentation will be marked as is in their header comment.               *)
(* ************************************************************************** *)



type type_name_ident = QmlAst.TypeIdent.t



(* Initially set to [MARK_general_not_seen].
   Modified during various processing, then cleared back to
   [MARK_general_not_seen].
*)


type 'variable_info var_marker =
  | VM_not_seen
  | VM_print_prep_seen of int ref (** During the phase preparing a
         pretty-print the variable was already seen. This doesn't means that the
         type embedding this variable is or is not recursive by essence. The
         fact that it is recursive is determined by the fact that the "seen"
         counter is > 1 after having explored a type's sub-terms although we
         marked this type as seen 1 time. This marker is only set a
         pretty-print preparation time. *)
  | VM_print_prep_seen_not_rec (** During the phase preparing a pretty-print
         the variable was already seen more then one times but it appears that
         it was not seen in a recursive way. This may arise with sharing between
         parts of type that are not sub-term of each other. In this case, we
         won't want any abbreviation to be introduced to name this variable. *)
  | VM_print_final_abbrevd of string (** This is also used to give a name to a
         variable when in final pass of pretty-print. *)
  | VM_print_sequence_abbrevd of string  (** During the final pass of
         pretty-print, this variable was previously printed as assigned an
         abbreviated name. However because this variable is printed in sequence
         with other ones, we don't want to directly use this abbreviation the
         first time the variable is printed in a sequence step. The first time,
         we want to re-print the variable as if it was the first time we see it
         and next occurrences of it (inside its structure) will then use again
         the already generated abbreviation.
         This marker in fact appears after a variable is printed in the case we
         are printing in sequence. This marker then replaces all the
         [VM_print_final_abbrevd]. When the variable is printed in a new step of
         the sequence, this marker gets back turned in to
         [VM_print_final_abbrevd]. *)
  | VM_copy_copied_as of 'variable_info  (** During a copy (specialization)
         the row/column variable was seen and copied as a new variable that is
         recorded in the argument of this constructor.
         This means that this variable, already seen, must be replaced in the
         copy everywhere it appears by the type argument of the constructor.
         In case the variable we see for the first time is not generalizable,
         then it is marked aliased to itself. Hence, when going on descending
         on the structure of the type embedding this variable, if this type is
         cyclic then we may encounter it again and we will stop descending,
         replacing it by itself in the copy. *)



type type_marker =
  | TM_not_seen
  | TM_print_prep_seen of int ref (** During the phase preparing a
         pretty-print the type was already seen. This doesn't means that the
         type is or is not recursive by essence. The fact that it is recursive
         is determined by the fact that the "seen" counter is > 1 after having
         explored a type's sub-terms although we marked this type as seen 1
         time. This marker is only set a pretty-print preparation time. *)
  | TM_print_prep_seen_not_rec (** During the phase preparing a pretty-print
         the type was already seen more then one times but it appears that it
         was not seen in a recursive way. This may arise with sharing between
         parts of type that are not sub-term of each other. In this case, we
         won't want any abbreviation to be introduced to name this type. *)
  | TM_print_final_abbrevd of string (** During the final pass of
         pretty-print this type was previously printed as assigned an
         abbreviated name that must now be used instead of the effective type's
         structure. This is also used to give a name to a variable when in
         final pass of pretty-print. *)
  | TM_print_sequence_abbrevd of string  (** During the final pass of
         pretty-print, this type was previously printed as assigned an
         abbreviated name. However because this type is printed in sequence
         with other ones, we don't want to directly use this abbreviation the
         first time the type is printed in a sequence step. The first time, we
         want to re-print the type as if it was the first time we see it and
         next occurrences of it (inside its structure) will then use again the
         already generated abbreviation.
         This marker in fact appears after a type is printed in the case we
         are printing in sequence. This marker then replaces all the
         [TM_print_final_abbrevd]. When the type is printed in a new step of
         the sequence, this marker gets back turned in to
         [TM_print_final_abbrevd]. *)
  | TM_copy_copied_as of simple_type  (** During a copy (specialization) the
         type was seen and copied as a type that is recorded in the argument of
         this constructor.
         This means that this type, already seen, must be replaced in the copy
         everywhere it appears by the type argument of the constructor.
         In case the type we see for the first time is not generalizable, then
         it is marked aliased to itself. Hence, when going on descending its
         structure, if this type is cyclic then we may encounter it again and
         we will stop descending, replacing it by itself in the copy. *)
  | TM_export_seen_not_rec
  | TM_export_seen of int ref  (* [TODO] argument seems useless. *)
  | TM_export_cyclic of type_name_ident
  | TM_generalize_seen (** During generalization, this type was already
         seen. *)
  | TM_col_close_seen  (** During a column closing this type was already
         seen. Hence to prevent looping, we must not explore it again. *)
  | TM_col_open_seen of simple_type  (** During a column opening this
         type was already seen. Hence to prevent looping, we must not explore it
         again and used instead the opened version of it recorded in this
         constructor. *)
  | TM_lowerise_seen (** During binding level "lowerization" of variables,
         this type was previously seen. Hence to prevent looping, we must not
         explore it again. *)
  | TM_row_open_seen (** During a row opening this type was already seen.
         Hence to prevent looping, we must not explore it again. *)
  | TM_col_var_collect_seen  (** During collecting of column variables in types
         assigned to catchall patterns, the type was already visited.
         Hence to prevent looping, we must not explore it again. *)



(* ************************************************************************** *)
(** {b Descr}: Simple type, AKA \tau in the type algebra.
    {b Attention}: Since types representation uses union-find and path
    compression, one must **never** discriminate on a type's structure without
    requested its canonical representation by the [simple_type_repr] function.
    Calling this function ensures that in a canonical representation of a
    type's structure, [SType_var]s are really variables, i.e. they are not
    instantiated by another type. Not following this procedure may cause
    considering as a variable some [SType_var]s that in fact are  already known
    as being "equal to another type", hence making the type looking more general
    than it is really.                                                        *)
(* ************************************************************************** *)
and simple_type = {
  sty_desc : simple_type_desc ;
  mutable sty_link : simple_type option ;
  mutable sty_mark : type_marker
}



(* ************************************************************************** *)
(** {b Descr}: Description of a type variable. As well as types are shared and
    respond to physical equality, type variables descriptions also are shared.
    Moreover, a type variable description is hosted in only one [simple_type].
    This means that it is impossible to have 2 different [simple_type]s of kind
    "variable" being physically different but hosting the (physically) same
    [type_variable] type variable description.                                *)
(* ************************************************************************** *)
and type_variable = {
  mutable tv_level : int ;  (** Binding level of the type variable. At
      generalization-time, any variable with a binding level > to the current
      one will be generalized (i.e. will have this field physically set to
      [W_CoreTypes.generic_binding_level]). *)
  tv_qml : QmlTypeVars.TypeVar.t ;  (** QML type variable related to this
      type variable. This allows to keep identity, to reflect sharing between
      the internal view of the type variables by the typechecker and the QML
      view of this type variable. *)
  tv_debug_count : int   (** For debug only, must disappear. Serves only to have
      a unique int for each variable in order to visually differentiate them
      when printed in separate types or type schemes. *)
}



(* ************************************************************************** *)
(** {b Descr}: Named simple type, i.e. a type constructor that may have
    arguments and that is either an abbreviation of another type or a purely
    abstract type.                                                            *)
(* ************************************************************************** *)
and named_simple_type = {
  nst_name : type_name_ident ;  (** Name of the type constructor. For
                                    instance "list". *)
  nst_abbrev_height : QmlTypes.abbrev_height ;
  nst_args : simple_type list ;  (** Arguments of the type constructor. *)
  mutable nst_unwinded : simple_type option  (** In case the named type is an
      abbreviation for another type, this field represents this other type. By
      construction, this type is inductively the fully unwounded view of the
      global type, i.e. the final representation of the type once all the
      cascading abbreviations involved in it are unwounded.
          For example in:
            type t('a, 'b) = ('b, 'a)
            type u = t(int, bool)
            type v = u
            type w('c) = ('c, v)
            type x = string
      the unwounded view of w(x) is tuple2(string, (boot, int)) *)
}



and simple_type_desc =
 | SType_var of type_variable
 | SType_arrow of ((simple_type list) * simple_type)
 | SType_named of named_simple_type
 | SType_sum_of_records of column_type
 | SType_forall of types_scheme



(* ************************************************************************** *)
(** {b Descr}: Row type, i.e. sequence of labels with types, ended either by a
    row variable (i.e. opened) or closed. This kind of type is used to represent
    the types of fields of a record type, or of sum types cases.
    To prevent loss of principal type existence, it is forbidden to have a row
    ended by a row variable and hosted in a sum type (column type) ended by a
    column variable.
    {b Attention}: Since rows representation uses union-find and path
    compression, one must **never** discriminate on a row's structure without
    requested its canonical representation by the [row_type_repr] function.
    Calling this function ensures that in a canonical representation of a
    row's structure, [row_variable]s are really variables, i.e. they are not
    instantiated by another row. Not following this procedure may cause
    considering as a variable some [row_variable]s that in fact are  already
    known as being "equal to another row", hence making the type looking more
    general than it is really.                                                *)
(* ************************************************************************** *)
and row_type = {
  (* First component is the list of already known fields of the record with
     their type.
     Second component is the ending part of the row, i.e. either closed,
     opened by a non instantiated row-variable, or another row that must be
     appended to the already known fields of the current row. *)
  mutable rt_value : ((string * simple_type) list * row_type_ending)
}



(* ************************************************************************** *)
(** {b Descr}: End of a row. A row can either ends by a "final point" (it is
    then closed) or by a non-instantiated row variable (it is then opened). Row
    variables provide both row-polymorphism and fields catenation. If a row
    variable is instantiated by another row, then the whole type represents the
    row having the fields of both rows and ended by the end of the row by which
    the row variable is instantiated.
    {b Attention}: Since columns representation uses union-find and path
    compression, one must **never** discriminate on a column's structure without
    requested its canonical representation by the [column_type_repr] function.
    Calling this function ensures that in a canonical representation of a
    column's structure, [column_variable]s are really variables, i.e. they are
    not instantiated by another column. Not following this procedure may cause
    considering as a variable some [column_variable]s that are in fact already
    known as being "equal to another column", hence making the type looking
    more general than it is really.                                           *)
(* ************************************************************************** *)
and row_type_ending =
  | Closed_row
  | Var_row of row_variable



and row_variable = {
  mutable rv_level: int ;
  mutable rv_value: row_variable_value ;
  (* Representation of this variable in the public type algebra, i.e. in the
     QML side. *)
  rv_public_identity: QmlTypeVars.RowVar.t ;
  mutable rv_mark : row_variable var_marker
}



and row_variable_value =
  | Row_unknown             (* Row is closed. *)
  | Row_known of row_type   (* Row is either opened or continues with the fields
                               of the row by which the variable is
                               instantiated. *)



(* ************************************************************************** *)
(** {b Descr}: column type, i.e. sequence of record types, ended either by a
    column variable (i.e. opened) or closed. The rows enumerated in the type
    represent the different cases of the sum.
    To prevent loss of principal type existence, it is forbidden to have a
    column type ended by a column variable and having rows ended by row
    variables.                                                                *)
(* ************************************************************************** *)
and column_type = {
  (* First component is the list of already known cases of the sum.
     Second component is the ending part of the column, i.e. either closed,
     opened by a non instantiated column-variable, or another column (cases of
     sum) that must be appended to the already known cases of the current column
     (sum). *)
  mutable ct_value : (row_type list * column_type_ending)
}



(* ************************************************************************** *)
(** {b Descr}: End of a column. A column can either ends by a "final point"
    (it is then closed) or by a non-instantiated column variable (it is then
    opened). Column variables provide both column-polymorphism and sum cases
    catenation. If a column variable is instantiated by another column, then
    the whole type represents the column having the cases of both column and
    ended by the end of the column by which the column variable is
    instantiated.                                                             *)
(* ************************************************************************** *)
and column_type_ending =
  | Closed_column                 (* Column is closed. *)
  | Var_column of column_variable (* Column is either opened or continues with
                                     the cases of the column by which the
                                     variable is instantiated. *)



and column_variable = {
  mutable cv_level: int ;
  mutable cv_value: column_variable_value ;
  (* Representation of this variable in the public type algebra, i.e. in the
     QML side. *)
  cv_public_identity: QmlTypeVars.ColVar.t ;
  mutable cv_mark : column_variable var_marker
}



and column_variable_value =
  | Col_unknown
  | Col_known of column_type



(* ************************************************************************** *)
(** {b Descr}: Type scheme. They encode polymorphism of type by providing a
    "pattern" of types. A type scheme exhibits variables that considered
    polymorphic and must be replaced by fresh ones each type an instance of
    the scheme's body is requested.                                           *)
(* ************************************************************************** *)
and types_scheme = {
  (* Assumed to be variables by construction. *)
  ty_parameters : simple_type list ;
  row_parameters : row_variable list ;
  column_parameters :  column_variable list ;
  body : simple_type
}



let tmp_debug_var_marker = function
  | VM_not_seen -> "VM_not_seen"
  | VM_print_prep_seen i -> "VM_print_prep_seen (" ^ (string_of_int !i) ^")"
  | VM_print_prep_seen_not_rec -> "VM_print_prep_seen_not_rec"
  | VM_print_final_abbrevd n -> "VM_print_final_abbrevd (" ^ n ^ ")"
  | VM_print_sequence_abbrevd _ -> "VM_print_sequence_abbrevd"
  | VM_copy_copied_as _ -> "VM_copy_copied_as (_)"



let tmp_debug_type_marker = function
  | TM_not_seen -> "TM_not_seen"
  | TM_print_prep_seen i -> "TM_print_prep_seen (" ^ (string_of_int !i) ^")"
  | TM_print_prep_seen_not_rec -> "TM_print_prep_seen_not_rec"
  | TM_print_final_abbrevd n -> "TM_print_final_abbrevd (" ^ n ^ ")"
  | TM_print_sequence_abbrevd _ -> "TM_print_sequence_abbrevd"
  | TM_copy_copied_as _ -> "TM_copy_copied_as (_)"
  | TM_export_seen_not_rec -> "TM_export_seen_not_rec"
  | TM_export_seen i -> "TM_export_seen (" ^ (string_of_int !i) ^")"
  | TM_export_cyclic _i -> "TM_export_cyclic"
  | TM_generalize_seen -> "TM_generalize_seen"
  | TM_col_close_seen -> "TM_col_close_seen"
  | TM_col_open_seen _ -> "TM_col_open_seen"
  | TM_lowerise_seen -> "TM_lowerise_seen"
  | TM_row_open_seen -> "TM_row_open_seen"
  | TM_col_var_collect_seen -> "TM_col_var_collect_seen"
