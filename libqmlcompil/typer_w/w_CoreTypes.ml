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
(** {b Descr}: This module implements the typechecker's type algebra and its
    manipulation. This module is especially big, but since it is the only one
    to have access to the internal representation of types, we must put inside
    any function that requires this internal representation.
    This is a design choice, to avoid exporting hence giving to much visibility
    on the types internal structure, since this structure contains strong
    invariants (mostly sharing and canonical representation not being equal to
    structural representation) that must really not be broken.
    So, this module is split into several sections, each being dedicated to a
    particular kind of operation on the types.                                *)
(* ************************************************************************** *)



(* ************************************************************************** *)
(** {b Descr}: Returns the current binding level for generalization, i.e. the
    level of variables than can be generalized if they are of a level strictly
    greater than the current binding level.
    {b Visibility} Exported outside this module, BUT must never be assigned
    manually. This value is visible in READ-ONLY mode. The 4 only functions
    allowed to modify this value are [begin_definition], [end_definition]
    [reset_toplevel_binding_level_on_error] and
    [restore_binding_level_from_backup].                                      *)
(* ************************************************************************** *)
let current_binding_level = ref 0



(* ************************************************************************** *)
(** {b Descr}: Binding level at which type variables introduced by type
    annotations are created. In effect since the scope of such variables is
    either the whole toplevel let-definition or the whole module field, we must
    prevent them from being generalized when leaving definitions where they
    appear because some coming type constraint can still instantiate them after
    the local let-definition or field module.
    Do to so, before typechecking a toplevel let-definition, we artificially
    increase the current binding level of 1, remind it as the level for
    variables introduced by annotation, then we go on with regular
    let-definition typechecking. We do idem before typechecking each field of
    a module.
    And at the end of the toplevel let-definition or the module field, before
    generalization, we decrease the current binding level since we increased
    it before.
    Then, at this level, variables created by annotation finally become
    generalizable.
    So, this ensure that these variables can be generalized only once returned
    at toplevel or once exited of the module field, not before, not "too soon".
    {b Visibility}: Not exported outside this module. BUT must never be
    assigned manually. This value is visible in READ-ONLY mode. The 3 only
    functions allowed to modify this value are
    [new_annotations_generalizable_level],
    [release_annotations_generalizable_level] and
    [reset_toplevel_binding_level_on_error]                                   *)
(* ************************************************************************** *)
let current_annotations_generalizable_level = ref 0



(* ************************************************************************** *)
(** {b Descr}: Value of binding level indicating that variables having this
    level are generalized (polymorphic). This is a bit hacky to arbitrarily
    decide of this value, but we need a level greater than any level that can
    be obtained during type inference. Of course, theoretically it is possible
    to nest let-definition until having a binding level equal to this value,
    but... it would require tons of nested definitions and there is really an
    insignificant chance a developer does this ! So, we assume living with this
    assumption ^^
   {b Visibility}: Exported outside this module.                              *)
(* ************************************************************************** *)
let generic_binding_level = 100000000



(* ************************************************************************** *)
(** {b Descr}: Must be called AFTER every potentially generalizable
    definition. It decreases the binding level to prevent generalization of
    higher binding level type variables.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let begin_definition () = incr current_binding_level



(* ************************************************************************** *)
(** {b Descr}: Must be called AFTER every potentially generalizable
    definition. It decreases the binding level to prevent generalization of
    higher binding level type variables.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let end_definition () = decr current_binding_level




(* ************************************************************************** *)
(** {b Descr}: Reset the current binding level to 0, i.e. as when the
    typechecker is at toplevel, in no let-definition.
    This function must only be called in case of typechecking error in order to
    allow future toplevel sentences to be typechecked.
    In effect, opatop allows to continue accepting code even if the previous
    phrase was ill-typed. In case the wrong type left the typechecker in the
    middle of definitions or type constraints, it is possible to have the
    current binding level not lowered back to 0 because the exception that
    pinpointed the error prevented from executing correctly the [end_definition]
    calls corresponding to the executed [begin_definition] calls.
    Then, to reset the typechecker, this function must be called in case of
    error. This will prevent next phrases to be (wrongly) typechecked starting
    with a binding level greater that 0.
    We could even say that this function can always be called when
    [Typer_w.type_of_expr] is launched, but this should not be necessary since
    if there is no bug around, the binding level should always goes back at the
    end of typechecking a toplevel phrase, i.e. at the end of
    [Typer_w.type_of_expr]. Always calling the present function at the beginning
    of [Typer_w.type_of_expr] would mask bugs of coupling [begin_definition] and
    [end_definition] calls in the typechecker.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let reset_toplevel_binding_level_on_error () =
  current_binding_level := 0 ;
  current_annotations_generalizable_level := 0



(* ************************************************************************** *)
(** {b Descr}: Argggg! Very hairy function that sets the current binding level
    to a certain value. In a ideal world, we should not need this. But in fact,
    during QML type -> [simple_type] conversion, the case of abstract QML types
    [QmlAst.TypeAbstract] raised an exception to delay the creation of the
    [simple_type]. The problem is that raising this exception can interrupt the
    conversion in the middle of nowhere, especially after having started the
    definition of of type in the environment. This operation involves scheme
    creations and may temporarily change the current binding level. If the
    search is killed by the exception before completion, then the binding level
    will remain modified, breaking the balance between [begin_definition] and
    [end_definition] calls.
    To circumvent this issue, [W_TypingEnv.rec_import_qml_type] makes a backup
    of the current binding level before processing the case of a named type
    and restores it in case of exception [W_TypingEnv.Importing_Qml_Abstract]
    (this exception meaning that a QML type "abstract" was encountered).
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let restore_binding_level_from_backup backup =
  current_binding_level := backup



(* ************************************************************************** *)
(** {b Descr}: Record the new binding level strictly above which type variables
    introduced by explicit type annotations will be generalizable. The effect
    is to artificially increase the current binding level and reminding this
    level. From this point, type variables created by annotations will have this
    reminded level.
    This function must only be called in the case of toplevel let-definitions
    or modules.
    In the first case, it is called before typechecking the bindings of the
    let-definition and [release_annotations_generalizable_level] is called just
    after typechecking the bindings of the let-definition.
    In the second case, it is called before typechecking *each* field of the
    module and [release_annotations_generalizable_level] is called just
    after typechecking *this* field of the module.
    Hence, except variables introduced by explicit annotations, no other
    variables can be created with this recorded level.
    Until we descend back to a binding level strictly lower to this recorded
    level, variables created with this recorded level won't be generalizable.
    And, especially, to descend back to a binding level where these variables
    become generalizable, we call [release_annotations_generalizable_level]
    whose job is to lower the binding level we artificially increased here.
    So [release_annotations_generalizable_level] must be called if we were
    called and in this case, just before generalizing the type of a toplevel
    let-definition or of a module field (via forallization in this last case).
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let new_annotations_generalizable_level () =
  begin_definition () ;
  current_annotations_generalizable_level := !current_binding_level




(* ************************************************************************** *)
(** {b Descr}: Revert back to the previously known level strictly above which
    type variables introduced by explicit type annotations must be created.
    The effect is to decrease the current binding level originally increased by
    [new_annotations_generalizable_level] and to record it as the new level
    for variables introduced by annotations.
    Because we always balance [begin_definition] and [end_definition], by
    calling the present function, once we decreased the level we increased by
    [new_annotations_generalizable_level], by construction the current binding
    level re-becomes equal to the previous annotations generalizable level.
    Note: read also documentation of [new_annotations_generalizable_level].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let release_annotations_generalizable_level () =
  end_definition () ;
  current_annotations_generalizable_level := !current_binding_level



(* {b Visibility}: Exported outside this module. *)
let __row_variable () =
  { W_Algebra.rv_level = !current_binding_level ;
    W_Algebra.rv_value = W_Algebra.Row_unknown ;
    W_Algebra.rv_public_identity = QmlTypeVars.RowVar.next () ;
    W_Algebra.rv_mark = W_Algebra.VM_not_seen
  }

(* {b Visibility}: Exported outside this module. *)
let __row_variable_with_public_identity qml_var =
  { W_Algebra.rv_level = !current_binding_level ;
    W_Algebra.rv_value = W_Algebra.Row_unknown ;
    W_Algebra.rv_public_identity = qml_var ;
    W_Algebra.rv_mark = W_Algebra.VM_not_seen
  }



(* ************************************************************************** *)
(** {b Descr}: Creates a row variable whose QML counterpart is known. This
    function must be used when the variable to create comes from the context of
    an explicit type annotation (i.e. a constraining type expression). In
    effect, such variables introduced by annotations can only be generalized at
    toplevel and not when getting outside the let-body where they were created.
    To achieve this, such variables get a binding level equal to 0, and the
    [W_SchemeGenAndInst.generalize] function makes a special case when it
    encounters a variable of level 0 and the current binding level is 0: it
    generalize it.
    This is to achive the same scoping policy of type variables than OCaml does,
    and make so type variables inserted by type annotations have a scope being
    the whole current toplevel definition.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let __annotation_row_variable_with_public_identity qml_var =
  { W_Algebra.rv_level = !current_annotations_generalizable_level ;
    W_Algebra.rv_value = W_Algebra.Row_unknown ;
    W_Algebra.rv_public_identity = qml_var ;
    W_Algebra.rv_mark = W_Algebra.VM_not_seen
  }



(* {b Visibility}: Exported outside this module. *)
let __generic_row_variable () =
  { W_Algebra.rv_level = generic_binding_level ;
    W_Algebra.rv_value = W_Algebra.Row_unknown ;
    W_Algebra.rv_public_identity = QmlTypeVars.RowVar.next () ;
    W_Algebra.rv_mark = W_Algebra.VM_not_seen
  }

(* {b Visibility}: Exported outside this module. *)
let __generic_row_variable_with_public_identity qml_var =
  { W_Algebra.rv_level = generic_binding_level ;
    W_Algebra.rv_value = W_Algebra.Row_unknown ;
    W_Algebra. rv_public_identity = qml_var ;
    W_Algebra.rv_mark = W_Algebra.VM_not_seen
  }

(* {b Visibility}: Exported outside this module. *)
let row_ending_variable () = W_Algebra.Var_row (__row_variable ())

(* {b Visibility}: Exported outside this module. *)
let __column_variable () =
  { W_Algebra.cv_level = !current_binding_level ;
    W_Algebra.cv_value = W_Algebra.Col_unknown ;
    W_Algebra.cv_public_identity = QmlTypeVars.ColVar.next () ;
    W_Algebra.cv_mark = W_Algebra.VM_not_seen
  }

(* {b Visibility}: Exported outside this module. *)
let __column_variable_with_public_identity qml_var =
  { W_Algebra.cv_level = !current_binding_level ;
    W_Algebra.cv_value = W_Algebra.Col_unknown ;
    W_Algebra.cv_public_identity = qml_var ;
    W_Algebra.cv_mark = W_Algebra.VM_not_seen
  }



(* ************************************************************************** *)
(** {b Descr}: Creates a column variable whose QML counterpart is known. This
    function must be used when the variable to create comes from the context of
    an explicit type annotation (i.e. a constraining type expression). In
    effect, such variables introduced by annotations can only be generalized at
    toplevel and not when getting outside the let-body where they were created.
    To achieve this, such variables get a binding level equal to 0, and the
    [W_SchemeGenAndInst.generalize] function makes a special case when it
    encounters a variable of level 0 and the current binding level is 0: it
    generalize it.
    This is to achive the same scoping policy of type variables than OCaml does,
    and make so type variables inserted by type annotations have a scope being
    the whole current toplevel definition.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let __annotation_column_variable_with_public_identity qml_var =
  { W_Algebra.cv_level = !current_annotations_generalizable_level ;
    W_Algebra.cv_value = W_Algebra.Col_unknown ;
    W_Algebra.cv_public_identity = qml_var ;
    W_Algebra.cv_mark = W_Algebra.VM_not_seen
  }



(* {b Visibility}: Exported outside this module. *)
let __generic_column_variable () =
  { W_Algebra.cv_level = generic_binding_level ;
    W_Algebra.cv_value = W_Algebra.Col_unknown ;
    W_Algebra.cv_public_identity = QmlTypeVars.ColVar.next () ;
    W_Algebra.cv_mark = W_Algebra.VM_not_seen
  }

(* {b Visibility}: Exported outside this module. *)
let __generic_column_variable_with_public_identity qml_var =
  { W_Algebra.cv_level = generic_binding_level ;
    W_Algebra.cv_value = W_Algebra.Col_unknown ;
    W_Algebra.cv_public_identity = qml_var ;
    W_Algebra.cv_mark = W_Algebra.VM_not_seen
  }

(* {b Visibility}: Exported outside this module. *)
let column_ending_variable () =
  W_Algebra.Var_column (__column_variable ())



let foo_counter = ref 0 ;;

(* {b Visibility}: Exported outside this module. *)
let type_variable () =
incr foo_counter ;  (* FOR DEBUG PURPOSE ONLY. *)
  { W_Algebra.sty_desc =
      W_Algebra.SType_var {
        W_Algebra.tv_level = !current_binding_level ;
        W_Algebra.tv_qml = (QmlTypeVars.TypeVar.next ()) ;
        W_Algebra.tv_debug_count = !foo_counter (* FOR DEBUG PURPOSE ONLY. *)} ;
    W_Algebra.sty_link = None ;
    W_Algebra.sty_mark = W_Algebra.TM_not_seen }

(* {b Visibility}: Exported outside this module. *)
let __type_variable_with_public_identity qml_var =
incr foo_counter ;  (* FOR DEBUG PURPOSE ONLY. *)
  { W_Algebra.sty_desc =
      W_Algebra.SType_var {
        W_Algebra.tv_level = !current_binding_level ;
        W_Algebra.tv_qml = qml_var ;
        W_Algebra.tv_debug_count = !foo_counter (* FOR DEBUG PURPOSE ONLY. *)} ;
    W_Algebra.sty_link = None ;
    W_Algebra.sty_mark = W_Algebra.TM_not_seen }



(* ************************************************************************** *)
(** {b Descr}: Creates a type variable whose QML counterpart is known. This
    function must be used when the variable to create comes from the context of
    an explicit type annotation (i.e. a constraining type expression). In
    effect, such variables introduced by annotations can only be generalized at
    toplevel and not when getting outside the let-body where they were created.
    To achieve this, such variables get a binding level equal to 0, and the
    [W_SchemeGenAndInst.generalize] function makes a special case when it
    encounters a variable of level 0 and the current binding level is 0: it
    generalize it.
    This is to achive the same scoping policy of type variables than OCaml does,
    and make so type variables inserted by type annotations have a scope being
    the whole current toplevel definition.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let __annotation_type_variable_with_public_identity qml_var =
incr foo_counter ;  (* FOR DEBUG PURPOSE ONLY. *)
  { W_Algebra.sty_desc =
      W_Algebra.SType_var {
        (* This variable will be generalizable only when the current binding
           level will be > to this level and this level is the binding level
           recorded before *)
        W_Algebra.tv_level = !current_annotations_generalizable_level ;
        W_Algebra.tv_qml = qml_var ;
        W_Algebra.tv_debug_count = !foo_counter (* FOR DEBUG PURPOSE ONLY. *)} ;
    W_Algebra.sty_link = None ;
    W_Algebra.sty_mark = W_Algebra.TM_not_seen }



(* ************************************************************************** *)
(** {b Descr}: Creates a type variable already generalized. This function is
    only used by [unify_simple_type] during unification to handle types
    forall and must **absolutely neither** be used somewhere else **nor
    exported** !
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let __generic_type_variable () =
incr foo_counter ;  (* FOR DEBUG PURPOSE ONLY. *)
  { W_Algebra.sty_desc =
      W_Algebra.SType_var {
        W_Algebra.tv_level = generic_binding_level ;
        W_Algebra.tv_qml = QmlTypeVars.TypeVar.next () ;
        W_Algebra.tv_debug_count = !foo_counter (* FOR DEBUG PURPOSE ONLY. *)} ;
    W_Algebra.sty_link = None ;
    W_Algebra.sty_mark = W_Algebra.TM_not_seen }

(* {b Visibility}: Not exported outside this module. *)
let __generic_type_variable_with_public_identity qml_var =
incr foo_counter ;  (* FOR DEBUG PURPOSE ONLY. *)
  { W_Algebra.sty_desc =
      W_Algebra.SType_var {
        W_Algebra.tv_level = generic_binding_level ;
        W_Algebra.tv_qml = qml_var ;
        W_Algebra.tv_debug_count = !foo_counter (* FOR DEBUG PURPOSE ONLY. *)} ;
    W_Algebra.sty_link = None ;
    W_Algebra.sty_mark = W_Algebra.TM_not_seen }



let type_named ty_name ty_abb_height ty_args manifest =
  { W_Algebra.sty_desc =
      W_Algebra.SType_named {
        W_Algebra.nst_name = ty_name ;
        W_Algebra.nst_abbrev_height = ty_abb_height ;
        W_Algebra.nst_args = ty_args ;
        W_Algebra.nst_unwinded = manifest } ;
    W_Algebra.sty_link = None ;
    W_Algebra.sty_mark = W_Algebra.TM_not_seen }

let type_int () =
  type_named (QmlAst.TypeIdent.of_string Opacapi.Types.int) 0 [] None

let type_bool () =
  type_named (QmlAst.TypeIdent.of_string Opacapi.Types.bool) 0 [] None

let type_float () =
  type_named (QmlAst.TypeIdent.of_string Opacapi.Types.float) 0 [] None

let type_string () =
  type_named (QmlAst.TypeIdent.of_string Opacapi.Types.string) 0 [] None


(* ************************************************************************** *)
(** {b Descr}: Creates a type of function whose arguments have types [tys_args]
    and return type is [ty_res].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let type_arrow tys_args ty_res =
  { W_Algebra.sty_desc = W_Algebra.SType_arrow (tys_args, ty_res) ;
    W_Algebra.sty_link = None ;
    W_Algebra.sty_mark = W_Algebra.TM_not_seen }



(* ************************************************************************** *)
(** {b Descr}: Creates a type of record with no row-variable, and plugged in
    an opened column. This represents the type of a simple record expression,
    **not of a record pattern** !
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let type_closed_record fields_tys =
  let closed_row =
    { W_Algebra.rt_value = (fields_tys, W_Algebra.Closed_row) } in
  let opened_column =
    { W_Algebra.ct_value = ([closed_row], (column_ending_variable ())) } in
  { W_Algebra.sty_desc = W_Algebra.SType_sum_of_records opened_column ;
    W_Algebra.sty_link = None ;
    W_Algebra.sty_mark = W_Algebra.TM_not_seen }



(** Closed record in closed column. *)
let type_module_record fields_tys =
  let closed_row =
    { W_Algebra.rt_value = (fields_tys, W_Algebra.Closed_row) } in
  let closed_column =
    { W_Algebra.ct_value = ([closed_row], W_Algebra.Closed_column) } in
  { W_Algebra.sty_desc = W_Algebra.SType_sum_of_records closed_column ;
    W_Algebra.sty_link = None ;
    W_Algebra.sty_mark = W_Algebra.TM_not_seen }



(* ************************************************************************** *)
(** {b Descr}: Creates a type of record with a row-variable, and plugged in
    a closed column. This represents the type of an expression (**not a record
    expression**) in which a dot access can be done, or the type of an opened
    (i.e. ended explicitly by "...") record pattern.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let type_opened_record fields_tys =
  let opened_row =
    { W_Algebra.rt_value = (fields_tys, (row_ending_variable ())) } in
  let closed_column =
    { W_Algebra.ct_value = ([opened_row], W_Algebra.Closed_column) } in
  { W_Algebra.sty_desc = W_Algebra.SType_sum_of_records closed_column ;
    W_Algebra.sty_link = None ;
    W_Algebra.sty_mark = W_Algebra.TM_not_seen }



(* ************************************************************************** *)
(** {b Descr}: Creates a type record with or without row variable, depending on
    the [row_is_opened] parameter, plugged into an opened column. This is used
    to create the type of a record *pattern*. The row is opened if the pattern
    ends by an elipsis (...), otherwise it is closed.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let type_of_record_pattern ~row_is_opened fields_tys =
  let row =
    if row_is_opened then
      { W_Algebra.rt_value = (fields_tys, (row_ending_variable ())) }
    else { W_Algebra.rt_value = (fields_tys, W_Algebra.Closed_row) } in
  let opened_column =
    { W_Algebra.ct_value = ([row], (column_ending_variable ())) } in
 { W_Algebra.sty_desc = W_Algebra.SType_sum_of_records opened_column ;
   W_Algebra.sty_link = None ;
   W_Algebra.sty_mark = W_Algebra.TM_not_seen }



(* ************************************************************************** *)
(** {b Descr}: From a field name and its type, which are expected to come from
    a record extension expression and be the field to update with its new type,
    this function synthesizes 2 types:
      - the minimal record type expected for the expression to extend
      - the type to use as result of the whole result after extension.
    These 2 types share the same ending row variable to ensure that fields
    "other" than the updated one are shared between the 2 types.
    This allows to give to the extension primitive of the field "label" a
    type like:
      { label : 'a ; rho } -> 'b -> { label : 'b ; rho }
    This is more visible on f(x) = { x with lbl = 5 }
    where we expect the application of [f] on a record having extra fields [A]
    and [B] to return a record extension with these 2 fields present.
      f({ lbl = ""; A; 5 = 5 }) must have type { lbl = int; A; 5 = 5 }
    This means that the row variable shared between the argument's type of [f]
    and its result type propagate the fields "other than" [lbl] in the result
    of the function.
    So, the type returned as minimal expected type will have to be unified
    during inferrence with the actual type inferred for the expression to extend
    in order to ensure propagation of constraints in the result type via this
    shared row variable.
    Because we want to avoid creation of type with both row and column
    variables, we make the choice that the result record type is plugged in
    a *closed*  column.
    The column of the minimal type is closed since we need it to be
    instantiated only by 1 unique row, the one having at least the stated field
    (the field to update by the extension).
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let record_extention_min_expected_ty_and_result_ty field field_ty =
  (* Create the row variable that will be shared between the minimal type to
     expected for the expression to extend and the type of the result after
     extension. Sharing this variable will ensure propagation of "other"
     fields between the 2 types. *)
  let shared_row_ending_var = row_ending_variable () in
  (* Minimal row expected has just the field with no special type constraint
     (i.e. with a variable) and any remaining row (i.e. a row variable, the one
     we will share with the row of the result). *)
  let min_expected_row =
    { W_Algebra.rt_value =
        ([(field, (type_variable ()))], shared_row_ending_var) } in
  let extension_result_row =
    { W_Algebra.rt_value = ([(field, field_ty)], shared_row_ending_var) } in
  (* The column hosting the row of the minimal record type we expect for the
     expression to extend. This column is closed since we expect only 1 row to
     instantiate it, i.e. we expect it to be unified with the unique row
     representing a record. *)
  let min_expected_column =
    { W_Algebra.ct_value = ([min_expected_row], W_Algebra.Closed_column) } in
  let extension_result_column =
    { W_Algebra.ct_value =
        ([extension_result_row], W_Algebra.Closed_column) } in
  let min_expected_record_ty = {
    W_Algebra.sty_desc = W_Algebra.SType_sum_of_records min_expected_column ;
    W_Algebra.sty_link = None ;
    W_Algebra.sty_mark = W_Algebra.TM_not_seen } in
  let extension_result_record_ty = {
    W_Algebra.sty_desc =
      W_Algebra.SType_sum_of_records extension_result_column ;
    W_Algebra.sty_link = None ;
    W_Algebra.sty_mark = W_Algebra.TM_not_seen } in
  (min_expected_record_ty, extension_result_record_ty)



(* ************************************************************************** *)
(* ****  Tracing and performing changes induced by unification in place  **** *)
(* ****  stuff                                                           **** *)
(* ************************************************************************** *)



(* ************************************************************************** *)
(** {b Descr}: Represents the various operations that unification may apply on
    types to change their structure. This information will be recorded in a
    stack to be able to revert the changes if needed.
    Changes need to be reverted in 2 cases:
     - Unification error in the inference: we want to display types in the
       error message as they were before unification and not once they are
       partially instantiated and hacked by the unification. This is more clean
       for the user.
     - Unification failure between 2 rows when unifying 2 column types. In
       effect, in this case unification must by tried in cross. This is similar
       to what happens during unification of 2 rows, the only thing is that
       for 2 rows we just have to compare the 2 head constructors to know if we
       must unify in cross or not. In the case of column, "comparing the 2 head"
       constructors means "see if the 2 head rows unify". Hence, to see if they
       unify... we must unify them, performing the side effects. And if they
       finally don't unify, we must restore these 2 rows in there state before
       we tried to unify them in order to be clean when trying to do the column
       unification in cross.
    ATTENTION:
    When restoring the types from information stored in changes,
    [simple_type]s recorded in the change must **never** be "repr-ed". In
    effect, they represent the physical structures modified by the changes, so
    reverting these changes must be done on these physical structures, not on
    the canonical representation of them ! Especially, we want to rewind the
    modifications that changed the canonical representation of these type !
    So again, **never** "repr" them during restoration !
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
type unification_change =
  | UC_ty_level of (W_Algebra.type_variable * int)  (** The level of the type
                      variable was of the recorded integer value before the
                      change occurred during the unification. In other words,
                      the recorded integer is the backup of the level of the
                      type variable before the unification modified it. *)
  | UC_row_level of (W_Algebra.row_variable * int)
  | UC_column_level of (W_Algebra.column_variable * int)
  | UC_ty_link of (W_Algebra.simple_type * W_Algebra.simple_type option) (** The
                      recorded first [simple_type] had the recorded
                      [simple_type option] as [sty_link] field before the change
                      occurred during the unification. In other words, the
                      recorded [simple_type option] is the backup of the
                      type's [sty_link] field before the unification modified
                      it. *)
  | UC_row_link of (W_Algebra.row_type *
                    ((string * W_Algebra.simple_type) list *
                     W_Algebra.row_type_ending))  (** Same thing than
                      [UC_ty_link] but for rows. *)
  | UC_column_link of (W_Algebra.column_type *
                       (W_Algebra.row_type list *
                        W_Algebra.column_type_ending))  (** Same thing than
                      [UC_ty_link] but for columns. *)
  | UC_row_var_link of
      (W_Algebra.row_variable * W_Algebra.row_variable_value)  (** Same thing
                      than [UC_ty_link] but for row variables. *)
  | UC_column_var_link of 
      (W_Algebra.column_variable * W_Algebra.column_variable_value)  (** Same
                      thing than [UC_ty_link] but for column variables. *)



(* ************************************************************************** *)
(** {b Descr}: Basic cell of linked-list of unification changes. Such cells
    are linked to represent a list (trace) of changes, each change being
    plugged into a record type in order to get a physical equality test.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
type unification_changes_trace_item =
  | UCT_no_change
  | UCT_change of (unification_change * unification_changes_checkpoint)



(* ************************************************************************** *)
(** {b Descr}: Represents a checkpoint in a linked list of changes, i.e. a
    "location" in the linked list of changes that can be identified by physical
    equality. The need of physical equality is the reason for having here a
    record. We need physical equality to be able to compare changes and
    determine that while rewinding them, we reached the point where to stop
    rewinding.
    {b Visibility}: Exported as abstract outside this module.                 *)
(* ************************************************************************** *)
and unification_changes_checkpoint = {
  uct_item : unification_changes_trace_item
}



(* ************************************************************************** *)
(** {b Descr}: Global, but really private, head of the current linked-list of
    changes performed by unification at some point. This head gets updated as
    long as unification is performed and according to reset and rewind requests.
    The only guys allowed to manipulate it are:
    [reset_unification_changes_trace], [get_current_changes_checkpoint],
    [rewind_unification_changes], [change_..._level] and [change_..._link].
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let __performed_unification_changes = ref { uct_item = UCT_no_change }



(* ************************************************************************** *)
(** {b Descr}: Resets the list of changes performed by unification, i.e. clear
    the linked-list, i.e. forget all the changes in the current trace.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let reset_unification_changes_trace () =
  (* Initialize the head of the linked-list of changes with a fresh record
     containing "no change". *)
  __performed_unification_changes := { uct_item = UCT_no_change }



(* ************************************************************************** *)
(** {b Descr}: Returns the current state of the linked-list of changes done by
    the unification, in order to get a "mark" of what is already changed, to be
    able later to rewind changes until getting back into this state.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let get_current_changes_checkpoint () = !__performed_unification_changes



let rewind_unification_changes ~performed_after =
  let rec iter_on_history head =
    if head == performed_after then (
      (* Finished: we arrived at the change that marks the point were rewind
         must stop. *)
      __performed_unification_changes := performed_after
    )
    else (
      (* Not yet arrived at the change marked as the first one not to undo. *)
      match head.uct_item with
      | UCT_no_change -> assert false
      | UCT_change (ch, next) -> (
          match ch with
          | UC_ty_level (var, lev) -> var.W_Algebra.tv_level <- lev
          | UC_row_level (row_var, lev) -> row_var.W_Algebra.rv_level <- lev
          | UC_column_level (col_var, lev) -> col_var.W_Algebra.cv_level <- lev
          | UC_ty_link (ty, link_val) -> ty.W_Algebra.sty_link <- link_val
          | UC_row_link (row_type, link_val) ->
              row_type.W_Algebra.rt_value <- link_val
          | UC_column_link (col_type, link_val) ->
              col_type.W_Algebra.ct_value <- link_val
          | UC_row_var_link (var, link_val) ->
              var.W_Algebra.rv_value <- link_val ;
          | UC_column_var_link (var, link_val) ->
              var.W_Algebra.cv_value <- link_val) ;
          (* Continue going back into the past. *)
          iter_on_history next
    ) in
  iter_on_history !__performed_unification_changes



(* ************************************************************************** *)
(** {b Descr}: Changes the binding level of a type variable. **Only** used by
    the unification routine [W_Unify.__unify_simple_type] and
    [W_Unify.lowerize_level_in_simple_type].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let change_ty_var_level var level =
  (* Create a "change" representing a [tv_level] modification, backing-up the
     type variable's level before its modification. *)
  let ch = UC_ty_level (var, var.W_Algebra.tv_level) in
  (* Add the change in head of the linked list of changes. *)
  __performed_unification_changes :=
    { uct_item = UCT_change (ch, !__performed_unification_changes) } ;
  (* Now really change the type's level. *)
  var.W_Algebra.tv_level <- level



(* ************************************************************************** *)
(** {b Descr}: Changes the binding level of a row variable. **Only** used by the
    unification routines [W_Unify.__unify_row_type] and
    [W_Unify.lowerize_level_in_row].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let change_row_var_level row_var level =
  (* Create a "change" representing a [rv_level] modification, backing-up the
     row variable's level before its modification. *)
  let ch = UC_row_level (row_var, row_var.W_Algebra.rv_level) in
  (* Add the change in head of the linked list of changes. *)
  __performed_unification_changes :=
    { uct_item = UCT_change (ch, !__performed_unification_changes) } ;
  (* Now really change the row variable's level. *)
  row_var.W_Algebra.rv_level <- level



(* ************************************************************************** *)
(** {b Descr}: Changes the binding level of a column variable. **Only** used
    by the unification routines [W_Unify.__unify_column_type] and
    [W_Unify.lowerize_level_in_column].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let change_column_var_level col_var level =
  (* Create a "change" representing a [cv_level] modification, backing-up the
     column variable's level before its modification. *)
  let ch = UC_column_level (col_var, col_var.W_Algebra.cv_level) in
  (* Add the change in head of the linked list of changes. *)
  __performed_unification_changes :=
    { uct_item = UCT_change (ch, !__performed_unification_changes) } ;
  (* Now really change the column variable's level. *)
  col_var.W_Algebra.cv_level <- level



(* ************************************************************************** *)
(** {b Descr}: Changes the link toward what a type is in fact equal to.
    **Only** used by the unification routine [W_Unify.__unify_simple_type] and
    the "repr" routine [simple_type_repr].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let change_ty_link ~receiver ~link_val =
  (* Create a "change" representing a [sty_link] modification, backing-up the
     type's link before its modification. *)
  let ch = UC_ty_link (receiver, receiver.W_Algebra.sty_link) in
  (* Add the change in head of the linked list of changes. *)
  __performed_unification_changes :=
    { uct_item = UCT_change (ch, !__performed_unification_changes) } ;
  (* Now really change the type's link. *)
  receiver.W_Algebra.sty_link <- link_val



(* ************************************************************************** *)
(*
val change_row_link:
  receiver: W_Algebra.row_type ->
  link_val:
    ((string * W_Algebra.simple_type) list * W_Algebra.row_type_ending) ->
  unit
*)
(** {b Descr}: Changes the link toward what a row type is in fact equal to.
    **Only** used by the "repr" routine [row_type_repr].
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let change_row_link ~receiver ~link_val =
  (* Create a "change" representing a [rt_link] modification, backing-up the
     row type's link before its modification. *)
  let ch = UC_row_link (receiver, receiver.W_Algebra.rt_value) in
  (* Add the change in head of the linked list of changes. *)
  __performed_unification_changes :=
    { uct_item = UCT_change (ch, !__performed_unification_changes) } ;
  (* Now really change the row type's link. *)
  receiver.W_Algebra.rt_value <- link_val



(* ************************************************************************** *)
(*
val change_column_link:
  receiver: W_Algebra.column_type ->
  link_val: (W_Algebra.row_type list * W_Algebra.column_type_ending) ->
  unit
*)
(** {b Descr}: Changes the link toward what a column type is in fact equal to.
    **Only** used by the "repr" routine [column_type_repr].
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let change_column_link ~receiver ~link_val =
  (* Create a "change" representing a [ct_link] modification, backing-up the
     column type's link before its modification. *)
  let ch = UC_column_link (receiver, receiver.W_Algebra.ct_value) in
  (* Add the change in head of the linked list of changes. *)
  __performed_unification_changes :=
   { uct_item = UCT_change (ch, !__performed_unification_changes) } ;
  (* Now really change the column type's link. *)
  receiver.W_Algebra.ct_value <- link_val



let change_row_var_link ~receiver ~link_val =
  (* Create a "change" representing a [rv_value] modification, backing-up the
     row variable's link before its modification. *)
  let ch = UC_row_var_link (receiver, receiver.W_Algebra.rv_value) in
  (* Add the change in head of the linked list of changes. *)
  __performed_unification_changes :=
    { uct_item = UCT_change (ch, !__performed_unification_changes) } ;
  (* Now really change the row type's link. *)
  receiver.W_Algebra.rv_value <- link_val



let change_column_var_link ~receiver ~link_val =
  (* Create a "change" representing a [cv_value] modification, backing-up the
     column variable's link before its modification. *)
  let ch = UC_column_var_link (receiver, receiver.W_Algebra.cv_value) in
  (* Add the change in head of the linked list of changes. *)
  __performed_unification_changes :=
    { uct_item = UCT_change (ch, !__performed_unification_changes) } ;
  (* Now really change the row type's link. *)
  receiver.W_Algebra.cv_value <- link_val



(* *************************************************** *)
(* ****  Canonical representation of types stuff. **** *)
(* *************************************************** *)



(* ************************************************************************** *)
(** {b Descr}: Computes the canonical representation of a simple (i.e. ML-like)
    type.
    Before a type's structure can be structurally examined, this function must
    be called to ensure that the type representation is the one of its canonical
    form.
    This function performs the union-find and path-compression to incrementally
    follow instantiated types (variables or cycles) appearing in a type.
    Instantiated variables as well as type cycles are "uncompressed" only at
    the present level, i.e. at one level each time.
    If we need one day to known more about the type instantiating a variable,
    this will "uncompressed" later, just when we will really need it.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let rec simple_type_repr ty =
  match ty with
  | { W_Algebra.sty_link = Some eq_to_ty } ->
      (* The type is "already known to be equal to another type". So, we
         recursively ask for the canonical representation of this "another
         type", and compress the instantiation path by telling in the
         type's structure that we now it is equal to the canonical
         representation of this "another" type. And then, we return this
         "another type" since the type is equal to it. Hence, the original type
         "disappears" from our result type. *)
      let eq_to_ty' = simple_type_repr eq_to_ty in
      change_ty_link ~receiver: ty ~link_val: (Some eq_to_ty') ;
      eq_to_ty'
 | otherwise ->
     (* The type is not a an alias, it is something with a "hard" constructor.
        Hence, we already have its "effective" toplevel structure. We don't
        descent inside the structure, this will be done later if someone need
        to know more about the sub-terms of type this type contains. *)
     otherwise



(* ************************************************************************** *)
(** {b Descr}: Computes the canonical representation of a row type. The idea is
    the same than for simple types. This is just a bit more tricky because rows
    have a mutable "value field" and can contain row variables that, them, have
    a mutable "row variable value" field. Hence path compression is performed
    at the 2 levels. We compress row-variables when they are instantiated.
    And we also compress rows when their "row ending" is instantiated. With this
    last compression, we can see the canonical representation of a row in one
    shot, instead of having each time to take its fields part and dig in its
    "row ending" part to see if there is some more known fields to add to the
    former part.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let rec row_type_repr row =
  let (row_fields, row_ending) = row.W_Algebra.rt_value in
  match row_ending with
  | W_Algebra.Closed_row
  | W_Algebra.Var_row { W_Algebra.rv_value = W_Algebra.Row_unknown } ->
      row
  | W_Algebra.Var_row { W_Algebra.rv_value = W_Algebra.Row_known row' } ->
      (
        let row'_repr = row_type_repr row' in
        let (row'_fields, row'_ending) = row'_repr.W_Algebra.rt_value in
        (* We assume lists are already individually sorted. *)
        let all_fields =
          Sort.merge (fun (f1, _) (f2, _) -> f1 < f2) row_fields row'_fields in
        change_row_link ~receiver: row ~link_val: (all_fields, row'_ending) ;
        row
      )



(* ************************************************************************** *)
(** {b Descr}: Computes the canonical representation of a column type. Works
    the same way than [row_type_repr] does.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let rec column_type_repr column =
  let (col_rows, col_ending) = column.W_Algebra.ct_value in
  match col_ending with
  | W_Algebra.Closed_column
  | W_Algebra.Var_column { W_Algebra.cv_value = W_Algebra.Col_unknown } ->
      column
  | W_Algebra.Var_column { W_Algebra.cv_value = W_Algebra.Col_known col' } ->
      (
        let col'_repr = column_type_repr col' in
        let (col'_rows, col'_ending) = col'_repr.W_Algebra.ct_value in
        (* We don't sort the rows representing the records of the sum. So,
           we simply catenate all of them. *)
        let all_rows = col_rows @ col'_rows in
        change_column_link
          ~receiver: column ~link_val: (all_rows, col'_ending) ;
        column
      )



(* ************************************************************************** *)
(** {b Descr}: Remove all the markers from a type (and inductively its embedded
    row types and column types), resetting them to
    [Algebra.VMARK_general_not_seen]. This function must be called after any
    function that modify the types' markers. In effect, all these functions
    expect to have "non-marked" types when they are called. Hence, guys who
    previously marked types must cleanup to leave the type clean for the future
    guys.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let rec cleanup_simple_type ty =
  (* First, get the canonical representation of the type. *)
  let ty = simple_type_repr ty in
  match ty.W_Algebra.sty_mark with
  | W_Algebra.TM_not_seen -> ()
  | _ -> (
      ty.W_Algebra.sty_mark <- W_Algebra.TM_not_seen ;
      match ty.W_Algebra.sty_desc with
      | W_Algebra.SType_var _ -> ()
      | W_Algebra.SType_arrow (args_tys, res_ty) ->
          List.iter cleanup_simple_type args_tys ;
          cleanup_simple_type res_ty
      | W_Algebra.SType_named named ->
          (match named.W_Algebra.nst_unwinded with
           | None -> ()
           | Some t -> cleanup_simple_type t) ;
          List.iter cleanup_simple_type named.W_Algebra.nst_args
      | W_Algebra.SType_sum_of_records sumcases_column ->
          cleanup_column_type sumcases_column
      | W_Algebra.SType_forall scheme ->
          cleanup_simple_type scheme.W_Algebra.body
    )



(* ************************************************************************** *)
(** {b Descr}: Remove all the markers from a row type (and inductively its
    embedded types and column types), resetting them to
    [Algebra.VMARK_general_not_seen]. This function must be called after any
    function that modify the types' markers. In effect, all these functions
    expect to have "non-marked" types when they are called. Hence, guys who
    previously marked types must cleanup to leave the type clean for the future
    guys.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
and cleanup_row_type row =
  (* First, get the canonical representation of the row type. *)
  let row = row_type_repr row in
  let (row_fields, row_ending) = row.W_Algebra.rt_value in
  (* Propagate cleanup inside the types of the fields. *)
  List.iter (fun (_, ty) -> cleanup_simple_type ty) row_fields ;
  (* Now, if the row ends by a row-variable, then propagate cleanup in
     this row-variable. *)
  match row_ending with
  | W_Algebra.Var_row row_variable -> (
      match row_variable.W_Algebra.rv_mark with
      | W_Algebra.VM_not_seen -> ()
      | _ -> row_variable.W_Algebra.rv_mark <- W_Algebra.VM_not_seen
    )
  | W_Algebra.Closed_row -> ()



(* ************************************************************************** *)
(** {b Descr}: Remove all the markers from a column type (and inductively its
    embedded types and row types), resetting them to
    [Algebra.VMARK_general_not_seen]. This function must be called after any
    function that modify the types' markers. In effect, all these functions
    expect to have "non-marked" types when they are called. Hence, guys who
    previously marked types must cleanup to leave the type clean for the future
    guys.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
and cleanup_column_type column =
  (* First, get the canonical representation of the column type. *)
  let column = column_type_repr column in
  let (col_rows, col_ending) = column.W_Algebra.ct_value in
  (* Propagate cleanup inside the cases of the sum. *)
  List.iter cleanup_row_type col_rows ;
  (* Now, if the column ends by a column-variable, then propagate cleanup in
     this column-variable. *)
  match col_ending with
  | W_Algebra.Var_column col_variable -> (
      match col_variable.W_Algebra.cv_mark with
      | W_Algebra.VM_not_seen -> ()
      | _ -> col_variable.W_Algebra.cv_mark <- W_Algebra.VM_not_seen
    )
  | W_Algebra.Closed_column -> ()



let named_type_expr_height nty =
  if nty.W_Algebra.nst_abbrev_height < 0 then (
    let interest_arg =
      List.nth
        nty.W_Algebra.nst_args (- (nty.W_Algebra.nst_abbrev_height + 1)) in
    let interest_arg_height =
      (match interest_arg.W_Algebra.sty_desc with
      | W_Algebra.SType_var _ -> -1
      | W_Algebra.SType_named nst -> nst.W_Algebra.nst_abbrev_height
      | _ -> 0) in
    (* Again special case if we get -1 which means that's a type variable.
       In this case we are unable to compute the real height since the variable
       is not instantiated. Hence we have no other choice than expanding the
       type. This is signaled by returning -1 ... so to make shorter, to
       return [interest_arg_height]. So, in any case, we return
       [interest_arg_height]. *)
    interest_arg_height
   )
  else nty.W_Algebra.nst_abbrev_height
