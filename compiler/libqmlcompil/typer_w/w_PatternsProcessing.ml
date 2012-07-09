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
(** {b Descr}: Recursively closes column types appearing in a column type,
    except for sums ended by a variable belonging to the exclusion list and
    sums present in the rows representing the cases of the sum having its
    variable in the exclusion set.
    Closing operation it done by side effect, physically modifying the types.
    This function is used after patterns types merge, to prevent them from
    being opened sums. However, since some of these sum types can be bound
    to pattern parts having a catchall case, these ones must not get closed.
    For this reason, we are passed the list of column variables known to
    end columns that must not be closed because their a assigned to a pattern
    with a catchall (note that inductively, rows preceding such a variable
    need to be also not closed). Hence, when walking along the type structure,
    if we find a column ended by such a variable, we don't close it, otherwise
    we close it.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let rec recursively_close_columns_in_column_type except_for column =
  (* First, get the canonical representation of the column. *)
  let (col_records, col_ending) =
    (W_CoreTypes.column_type_repr column).W_Algebra.ct_value in
  (* If the current column has a variable belonging to the exclusion set,
     then we must not close it. Furthermore, since this means that this
     column has a related catchall, all sums present in the rows of this
     sum fall in this catchall and hence must not be closed.
     First, have a look at the current column ending to close it if needed. *)
  let requires_close_records =
    (match col_ending with
     | W_Algebra.Closed_column -> true
     | W_Algebra.Var_column v ->
         (* Attention, variables marked as generalized, i.e. bound by the
            non-instantiated scheme of a type-forall must not be touched,
            otherwise closing them would break the scheme possibly making
            variables disappearing. *)
         if not (List.memq v except_for) &&
           v.W_Algebra.cv_level <> W_CoreTypes.generic_binding_level then (
             v.W_Algebra.cv_value <-
               W_Algebra.Col_known {
                 W_Algebra.ct_value = ([], W_Algebra.Closed_column) } ;
             true
           )
         else false) in
  (* Recurse the closing in each record forming the cases of the sum only
     if we didn't close the current column because its variable was in the
     exclusion set. *)
  if requires_close_records then
    List.iter (recursively_close_columns_in_row_type except_for) col_records



(* ************************************************************************** *)
(** {b Descr}: Recursively close column types appearing in a row type, except
    for sums ended by a variable belonging to the exclusion list.
    Closing operation is done by side effect on the types.
    For more description, consult header of
    [recursively_close_columns_in_column_type].
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and recursively_close_columns_in_row_type except_for row =
  (* First, get the canonical representation of the row. *)
  let (row_fields, _) =
    (W_CoreTypes.row_type_repr row).W_Algebra.rt_value in
  (* Recurse the closing in each type of field of the record. *)
  List.iter
    (fun (_, field_ty) ->
       recursively_close_columns_in_simple_type except_for field_ty)
    row_fields



(* ************************************************************************** *)
(** {b Descr}: Recursively close column types appearing in a simple type,
    except for sums ended by a variable belonging to the exclusion list.
    Closing operation is done by side effect on the types.
    For more description, consult header of
    [recursively_close_columns_in_column_type].
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and recursively_close_columns_in_simple_type except_for ty =
  (* First, get the canonical representation of the type. *)
  let ty = W_CoreTypes.simple_type_repr ty in
  match ty.W_Algebra.sty_mark with
  | W_Algebra.TM_col_close_seen -> ()
  | W_Algebra.TM_not_seen -> (
      (* Mark the type as already seen before going on to prevent looping. *)
      ty.W_Algebra.sty_mark <- W_Algebra.TM_col_close_seen ;
      match ty.W_Algebra.sty_desc with
      | W_Algebra.SType_var _ | W_Algebra.SType_arrow (_, _) -> ()
      | W_Algebra.SType_named nty ->
          (* We need to recurse in type structure ! In effect if we inferred
             a type like
                option ({ cell: { on_message: 'a; _~a } / _`b; . } / _`a)
             then it is right a named type, "option" but its manifest view
             really contains variables that may need to be closed. Not
             descending on the manifest representation and arguments may
             forget to close some columns. *)
          (match nty.W_Algebra.nst_unwinded with
           | None -> ()
           | Some t ->
               recursively_close_columns_in_simple_type except_for t) ;
          List.iter
            (recursively_close_columns_in_simple_type except_for)
            nty.W_Algebra.nst_args
      | W_Algebra.SType_sum_of_records sumcases_column ->
          recursively_close_columns_in_column_type except_for sumcases_column
      | W_Algebra.SType_forall scheme ->
          recursively_close_columns_in_simple_type
            except_for scheme.W_Algebra.body
    )
  | _ (* Other markers. *) -> assert false



(* ************************************************************************** *)
(** {b Descr}: Find column variables in the type recursively. This function is
    intended to be used on types assigned to catchall patterns. It harvest
    column variables in type begin instantiated to unions. Because, since
    catchall can have been instantiated by named types indeed being sums, that's
    the reason why we must descend in the named types to check for this and if
    a named type assigned to a catchall is a sum, we will have harvest its
    nested column variables inductively.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let collect_column_variables _env initial_ty =
  let col_vars_not_to_close = ref [] in

  let rec rec_collect_in_simple_type ~dont_expand ty =
    let ty = W_CoreTypes.simple_type_repr ty in
    match ty.W_Algebra.sty_mark with
    | W_Algebra.TM_col_var_collect_seen -> ()
    | W_Algebra.TM_not_seen -> (
        (* Mark the type as already seen before going on to prevent loops. *)
        ty.W_Algebra.sty_mark <- W_Algebra.TM_col_var_collect_seen ;
        match ty.W_Algebra.sty_desc with
        | W_Algebra.SType_var _
            (* Not very clear for the 3 cases below... *)
        | W_Algebra.SType_forall _ | W_Algebra.SType_arrow (_, _) ->
            ()
        | W_Algebra.SType_named nty -> (
            (* [NOTE] Think about... This was the old code when abbrevs were
               not fully incremental and unflatted all if on unflat was
               asked. Since this time, I wonder of simply descending in the
               arguments is not simply sufficient... Is there cases where the
               structure hidded under the type name has some column variables
               that need to remain open and that are not reachable from the
               type name arguments ?
               For the moment, simply descendign on the type arguments seems
               to typecheck existing code.
               But, keep this question in mind... *)
            (*
               (* The catchall pattern was in fact assigned a named type. May be
                  this one is a sum and possibly recursively contains sums. In
                  this case, we must take the row variables it hosts into
                  account. *)
               match nty.W_Algebra.nst_unwinded with
               | Some ty' -> rec_collect_in_simple_type ~dont_expand ty'
               | None ->
                   (* If we are no more allowed to expand the type, that's
                      because it has already be expanded and since we seen it
                      has not manifest representation after this expansion, this
                      means that the type is fully abstract. In this case, we
                      stop searching for column variables.
                      Otherwise, we fully expand it once, and retry again
                      searching, noting that it was now fully expanded once and
                      must not be again. *)
                   if not dont_expand then (
                     W_TypeAbbrevs.fully_expand_abbrev env ty ;
                     ty.W_Algebra.sty_mark <- W_Algebra.TM_not_seen ;
                     rec_collect_in_simple_type ~dont_expand: true ty
                   )
            *)
            List.iter
              (fun g -> rec_collect_in_simple_type ~dont_expand g)
              nty.W_Algebra.nst_args
          )
        | W_Algebra.SType_sum_of_records col_ty ->
            rec_collect_in_column_type col_ty
      )
    | _ (* Other markers. *) -> assert false

    (* ********************************************************************** *)
    (** {b Descr}: Local function to search for column variables, recursing on
        column types.
        {b Visibility}: Local to the function [collect_column_variables].     *)
    (* ********************************************************************** *)
    and rec_collect_in_column_type column =
      let (col_rows, col_ending) =
        (W_CoreTypes.column_type_repr column).W_Algebra.ct_value in
      List.iter rec_collect_in_row_type col_rows ;
      match col_ending with
      | W_Algebra.Var_column col_variable ->
          (* Only add this column variable in the list if it is not
             already recorded inside. *)
          if not (List.memq col_variable !col_vars_not_to_close) then
            col_vars_not_to_close :=
              col_variable :: !col_vars_not_to_close
      | _ -> ()



    (* ********************************************************************** *)
    (** {b Descr}: Local function to search for column variables, recursing on
        row types.
        {b Visibility}: Local to the function [collect_column_variables].     *)
    (* ********************************************************************** *)
    and rec_collect_in_row_type row =
      let (row_fields, _) =
        (W_CoreTypes.row_type_repr row).W_Algebra.rt_value in
      List.iter
        (fun (_, ty) ->
           (* Initially, expansion is allowed since we are processing
              individual types of each field. *)
           rec_collect_in_simple_type ~dont_expand: false ty)
        row_fields in


    (* Effective body of [collect_column_variables]. Initially, we are allowed
       to expand the type if it is a named type. So, pass [true] to
       [~dont_expand]. *)
    rec_collect_in_simple_type ~dont_expand: false initial_ty ;
    !col_vars_not_to_close



(* ************************************************************************** *)
(** {b Descr}: Merge incrementally all the types found for the patterns of a
    matching. Merge is done by unifying each pattern type with the accumulator
    type obtained by the merge of previous patterns.
    Once all the unifications are performed, since we are in the case of
    left-sides for a pattern matching, we must close all the columns (sums) that
    have been inferred, to avoid telling that in fact our pattern can take all
    the enumerated cases plus something else. In effect, each pattern has an
    opened column to be unifiable with other cases. But once all the cases
    have been seen, the matching can't take any other case. So its sum type must
    turned closed.
    This is true except for sums synthesized for patterns (and sub-patterns)
    that are catchall. For this reason, the closing is done except on columns
    that have been identified as related to a catchall pattern.
    Finally, this function return the type considered globally suitable for
    all the patterns.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let merge_patterns_types env found_catchalls ~pat_match_expr ~matched_expr_ty
    ~patterns_types =

  (* Local function that merges 2 types, assuming the first one to be the
     accumulation of previous merges.
     REMARK: This function got so simplified that now it became a simple
     unification of the matched expression type and patterns types. So don't
     be impressed, we will cleanup all this ! Because unification is done in
     place, this function will become of type void. *)
  let one_step_merge accu_type ty =
    (* First, get the canonical representation of both types. *)
    let ty = W_CoreTypes.simple_type_repr ty in
    let accu_type = W_CoreTypes.simple_type_repr accu_type in
    match (accu_type.W_Algebra.sty_desc, ty.W_Algebra.sty_desc) with
    | ((W_Algebra.SType_forall _), _) | (_, (W_Algebra.SType_forall _)) ->
        (* [TODO]. *)
        OManager.printf "TODO merge_patterns_types forall/forall@." ;
        failwith "merge_patterns_types forall/forall"
    | ((W_Algebra.SType_var _), _) | (_, (W_Algebra.SType_var _))
    | ((W_Algebra.SType_arrow _), _) | (_, (W_Algebra.SType_arrow _))
    | ((W_Algebra.SType_named _), _) | (_, (W_Algebra.SType_named _))
    | ((W_Algebra.SType_sum_of_records _),
       (W_Algebra.SType_sum_of_records _)) ->
        (* In fact, perform a standard unification... *)
        (try W_Unify.unify_simple_type env accu_type ty with
         | W_Unify.Unification_simple_type_conflict (err_t1, err_t2, detail) ->
             raise
               (W_InferErrors.Infer_detailled_unification_conflict
                  (W_InferErrors.UCC_match_left_part_ty_previous_vs_ty_current
                     (pat_match_expr, accu_type, ty),
                   err_t1, err_t2, detail))) ;
        accu_type in

  (* This is just a [List.fold_left], but made explicit. Since unification is
     performed in place, a simple List.iter is indeed sufficient ! ^^ *)
  let rec fold accu = function
    | [] -> accu
    | h :: q ->
        let accu' = one_step_merge accu h in
        fold accu' q in

  (* Effective body of the function [merge_patterns_types]. *)
  let found_ty = fold matched_expr_ty patterns_types in
  (* Recover from the type assigned to catchall patterns, the ones instantiated
     to unions, and for them, simply remind their column ending variable if
     some. Attention, since catchall can have been instantiated by named type
     being sums, we must descend in the named types to check for this and if
     a named type assigned to a catchall is a sum, we will have to open all
     the sums it contains inductively. *)
  let col_vars_not_to_close = ref [] in
  List.iter
    (fun catchall_ty ->
       col_vars_not_to_close :=
         (collect_column_variables env catchall_ty) @ !col_vars_not_to_close ;
       (* Cleanup the markers set during column variables collecting. *)
       W_CoreTypes.cleanup_simple_type catchall_ty)
    found_catchalls ;
  (* Now, recursively close each sum, except those ended by a column-variable
     belonging to the list above, i.e. those belonging to a sum type inferred
     for a part of the pattern with a catchall. *)
  recursively_close_columns_in_simple_type !col_vars_not_to_close found_ty ;
  (* Cleanup the markers set by the closing routine. We only need to cleanup the
     type we wanted to close and not the closed one because when creating the
     closed one, if we share part from the first one, then because we clean it
     there is not problem, and if we have fresh parts then we never set markers
     inside. *)
  W_CoreTypes.cleanup_simple_type found_ty ;
  found_ty
