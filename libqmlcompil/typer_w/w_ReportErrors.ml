(*
    Copyright Â© 2011 MLstate

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


(* depends *)
module List = Base.List



(* ************************************************************************** *)
(** {b Descr}: Kind of fields difference between 2 sum types reported as
    incompatible during unification. This embedds the case where one of the
    type is missing cases of the other and the case where both types have the
    same number of cases, but some of these cases are different.
    {b Visibility}: Not visible outside this module.                          *)
(* ************************************************************************** *)
type missing_or_different_cases_kind =
  | MODCK_missing of string list list
  | MODCK_different of (string list list * string list list)



(* ************************************************************************** *)
(** {b Descr}: Collects the cases missing between the 2 sums passed as
    argument. If both sums have the same number of cases, then no missing cases
    are reported since we can't really say in this case that one is most
    "complete" than the other. In this case, instead, we report the cases
    missing in each type compared against the other.
    Missing cases are returned as a list of lists of fields names.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let get_missing_or_different_cases col_ty1 col_ty2 =
  let col_ty1 = W_CoreTypes.column_type_repr col_ty1 in
  let col_ty2 = W_CoreTypes.column_type_repr col_ty2 in
  let cases1 = fst col_ty1.W_Algebra.ct_value in
  let cases2 = fst col_ty2.W_Algebra.ct_value in
  let nb_cases1 = List.length cases1 in
  let nb_cases2 = List.length cases2 in
  (* To save computation, we directly transform each sum in a list of lists of
     fields names. For this, just define a local flattening function and apply
     it on both sums. *)
  let flatten_cases cases =
    List.map
      (fun row ->
         let row = W_CoreTypes.row_type_repr row in
         List.map fst (fst row.W_Algebra.rt_value))
      cases in
  if nb_cases1 = nb_cases2 then (
    (* Both sums have the same number of cases, so we can't really say that
       one of them is missing case(s) of the other. So, instead, we will try
       to find the cases that are different (may be  a spelling error). *)
    (* First, flatten the lists. *)
    let flat_cases1 = flatten_cases cases1 in
    let flat_cases2 = flatten_cases cases2 in
    (* By construction, the remaining cases of cases2 we get at the end are
       the cases of 2 missing in the cases of 1. *)
    let (miss1_in2, miss2_in1) =
      List.fold_left
        (fun (accu_miss1_in2, rem_cases2) case1 ->
           try
             (* If removal succeeds, then [field1] was really found in the list
                [rem_cases2], and removed from it in the result. *)
             let rem_cases2' =
               List.remove_first_or_fail_eq
                 ~eq:
                   (fun c_1 c_2 ->
                      ((List.length c_1) = (List.length c_2)) &&
                      (List.for_all (fun field -> List.mem field c_2) c_1))
                 case1 rem_cases2 in
             (accu_miss1_in2, rem_cases2')
           with Not_found ->
             (* [field1] was not present in [rem_cases2], hence it is
                missing. *)
             ((case1 :: accu_miss1_in2), rem_cases2))
        ([], flat_cases2)
        flat_cases1 in
    MODCK_different (miss1_in2, miss2_in1)
  )
  else (
    (* Ok, sums have a different number of cases. So determine the least and
       most complete ones before trying to make the difference. *)
    let (least_complete_sum_cases, most_complete_sum_cases) =
      if nb_cases1 < nb_cases2 then (cases1, cases2) else (cases2, cases1) in
    (* Really flatten the 2 sums. *)
    let least_complete = flatten_cases least_complete_sum_cases in
    let most_complete = flatten_cases most_complete_sum_cases in
    let missing_cases =
      List.fold_left
        (fun accu mcomplete_fields ->
           (* If there is one case of [least_complete] containing
              [mcomplete_fields], then drop [fields], otherwise keep it. *)
           let drop =
             List.exists
               (fun lcomplete_fields ->
                  List.for_all
                    (fun mcomplete_name ->
                       List.exists
                         (fun lcomplete_name -> mcomplete_name = lcomplete_name)
                         lcomplete_fields)
                    mcomplete_fields)
               least_complete in
           if drop then accu else mcomplete_fields :: accu)
        []
        most_complete in
    MODCK_missing missing_cases
  )



(* ************************************************************************** *)
(** {b Descr}: Tries to give hints, clues about why 2 types reported by an
    unification error are considered not compatible. This function dig the
    types, trying to find some particular cases of errors we can better
    explain. This function is a collection of heuristics and is allowed to be
    a bit heavy since it is called in an error case, i.e. before the
    compilation fails and ends.
    {b Args}:
     - [accur_ty1] : First type involved in the incompatibility error.
     - [accur_ty2] : Second type involved in the incompatibility error.
    {b Visibility} : Not exported outside this module.                        *)
(* ************************************************************************** *)
let try_explain_ty_incompatibility ppf accur_ty1 accur_ty2 =
  let accur_ty1 = W_CoreTypes.simple_type_repr accur_ty1 in
  let accur_ty2 = W_CoreTypes.simple_type_repr accur_ty2 in
  match (accur_ty1.W_Algebra.sty_desc, accur_ty2.W_Algebra.sty_desc) with
  | ((W_Algebra.SType_sum_of_records col_ty1),
     (W_Algebra.SType_sum_of_records col_ty2)) -> (
      let col_ty1 = W_CoreTypes.column_type_repr col_ty1 in
      let col_ty2= W_CoreTypes.column_type_repr col_ty2 in
      match (col_ty1.W_Algebra.ct_value, col_ty2.W_Algebra.ct_value) with
      | (([uniq_case1], _), ([uniq_case2], _)) ->
          (* Case where the 2 types are in fact 2 records, i.e. each is a
             sum type with only one case. We will try to identify which
             fields are different in these record to report them to the
             user. *)
          let uniq_case1 = W_CoreTypes.row_type_repr uniq_case1 in
          let uniq_case2 = W_CoreTypes.row_type_repr uniq_case2 in
          let compare_fields (n1, _) (n2, _) = compare n1 n2 in
          let fields1 =
            List.sort compare_fields (fst uniq_case1.W_Algebra.rt_value) in
          let fields2 =
            List.sort compare_fields (fst uniq_case2.W_Algebra.rt_value) in
          let eq_fields (n1, _) (n2, _) = n1 = n2 in
          (* Get the fields missing in each type compared to the other type. *)
          let fields_of_1_not_in_2 =
            List.substract_eq ~eq: eq_fields fields1 fields2 in
          let fields_of_2_not_in_1 =
            List.substract_eq ~eq: eq_fields fields2 fields1 in
          if fields_of_1_not_in_2 <> [] then (
            Format.fprintf ppf
              ("@\n@[<2>@{<bright>Hint@}:@\nField(s)@ ") ;
            List.iter
              (fun (n, _) -> Format.fprintf ppf "@{<red>%s@}@ " n)
              fields_of_1_not_in_2 ;
            Format.fprintf ppf
              "only@ appear(s)@ in@ the@ first@ type.@]@\n"
          ) ;
          if fields_of_2_not_in_1 <> [] then (
            Format.fprintf ppf
              ("@\n@[<2>@{<bright>Hint@}:@\nField(s)@ ") ;
            List.iter
              (fun (n, _) -> Format.fprintf ppf "@{<red>%s@}@ " n)
              fields_of_2_not_in_1 ;
            Format.fprintf ppf
              "only@ appear(s)@ in@ the@ second@ type.@]@\n"
          ) ;
      | (_, _) -> (
          (* Other cases of 2 column types. In this case, not both sums have
             one unique case. In other words, at least one of the sums has
             no or several cases. We will try to find if one of the sums is
             missing cases from the other. *)
          let miss_diff_cases =
            get_missing_or_different_cases col_ty1 col_ty2 in
          (* Local function to print a list of cases. *)
          let print_cases cases =
            List.iter
              (fun row_fields_names ->
                 Format.fprintf ppf "@\n@[<2>{ " ;
                 List.iter
                   (fun name -> Format.fprintf ppf "%s@ " name)
                   row_fields_names ;
                 Format.fprintf ppf "}@]")
              cases in
          match miss_diff_cases with
          | MODCK_missing missing_cases ->
              if missing_cases <> [] then (
                Format.fprintf ppf
                  ("@\n@[<2>@{<bright>Hint@}:@\nOne@ of@ the@ sum@ types@ " ^^
                   "may@ be@ missing@ the@ following@ cases@ of@ the@ other:") ;
                print_cases missing_cases ;
                Format.fprintf ppf ".@]@\n"
              )
          | MODCK_different (miss1_in2, miss2_in1) ->
              if miss2_in1 <> [] then (
                Format.fprintf ppf
                  ("@\n@[<2>@{<bright>Hint@}:@\nFirst@ type@ is@ missing@ " ^^
                   "the@ following@ cases@ from@ second@ type:") ;
                print_cases miss2_in1 ;
                Format.fprintf ppf ".@]@\n"
              ) ;
              if miss1_in2 <> [] then (
                Format.fprintf ppf
                  ("@\n@[<2>@{<bright>Hint@}:@\nSecond@ type@ is@ missing@ " ^^
                   "the@ following@ cases@ from@ first@ type:") ;
                print_cases miss1_in2 ;
                Format.fprintf ppf ".@]@\n"
              ) ;
        )
    )
  | (_, _) ->
      (* Other cases of types. We do not try to explain more for the
         moment. *)
      ()



let (set_annotmap_for_error_report, get_annotmap_for_error_report) =
  let recorded_annotmap_opt = ref None in
  (
    (fun annotmap_opt -> recorded_annotmap_opt := annotmap_opt),
    (fun () ->
       match !recorded_annotmap_opt with
       | None -> QmlAnnotMap.empty
       | Some a -> a)
  )



(** Prints leading and trailing \n if a message is printed. *)
let pp_unification_conflict_detail ppf detail =
  let something_printed =
    (match detail.W_Unify.ucd_kind with
     | W_Unify.DK_none -> false
     | W_Unify.DK_fun_type_arity (n1, n2) ->
         Format.fprintf ppf
           ("@\n@[<2>@{<bright>Hint@}:@\nFunction@ types@ have@ different@ " ^^
              "arguments@ arity@ (%d@ versus@ %d).@]@\n")
           n1 n2 ;
         true
     | W_Unify.DK_named_type_arity (ty_name, n1, n2) ->
         Format.fprintf ppf
           ("@\n@[<2>@{<bright>Hint@}:@\nNamed@ type@ @{<red>%s@}@ is@ " ^^
            "used@ with@ different@ arguments@ arity@ (%d@ versus@ %d).@]@\n")
           (QmlAst.TypeIdent.to_string ty_name) n1 n2 ;
         true
     | W_Unify.DK_binding_level_mismatch ->
         Format.fprintf ppf
           ("@\n@[<2>@{<bright>Hint@}:@\nTrying@ to@ unify@ a@ generalized@ " ^^
              "type@ variable@ and@ a@ non-generalized@ type.@]@\n") ;
         true
     | W_Unify.DK_forall_type_quantification_arity (n1, n2) ->
         Format.fprintf ppf
           ("@\n@[<2>@{<bright>Hint@}:@\nDifferent@ numbers@ of@ "^^
            "generalized@ variables@ between@ forall@ types@ " ^^
            "(%d@ versus@ %d).@]@\n")
           n1 n2 ;
         true) in
  (match detail.W_Unify.ucd_through_field with
   | None -> ()
   | Some field_name ->
       (* If something was already printed, then we do not need to print a
          leading \n. If not, then wee need. *)
       if not something_printed then Format.fprintf ppf "@\n" ;
       (* Now print the name of the faulty field. *)
       Format.fprintf ppf
         ("@[<2>@{<bright>Hint@}:@\nError@ occurred@ through@ field@ " ^^
          "@{<red>%s@}.@]@\n")
         field_name)



(* ************************************************************************** *)
(** {b Descr}: Prints the message "[accur_ty1] and [accur_ty2] are
    incompatible" if these 2 types are smaller than [glob_ty1] and [glob_ty2],
    i.e. more precise, i.e. are sub-term of [glob_ty1] (resp. [glob_ty2]).
    This is used to print the 2 types that finally, in the deep of unification
    caused this unification to fail. In effect, globally saying that
    int -> char and int -> bool are incompatible is cool. But, the real deep
    reason is that char and bool are incompatible. Since char and bool are
    smaller, more precise than the 2 surrounding type, we want to print the
    message. If [glob_ty1] and [accur_ty1] (resp. [glob_ty2] and [accur_ty2])
    are the same, it is useless to print this message since error reporting
    routine that uses the present function will have already reported these
    types.
    This way, we do not have doubles and we don't lose accuracy in error
    reporting.
    Prints a leading \n but no trailing \n.

    {b Args}:
     - [glob_ty1] : First global (entire) type causing unification to fail.
     - [glob_ty2] : Second global (entire) type causing unification to fail.
     - [accur_ty1] : First deepest type causing unification to fail. For the
       message to be consistent, it is expected that this type is a sub-term
       of [glob_ty1].
     - [accur_ty2] : Second deepest type causing unification to fail. For the
       message to be consistent, it is expected that this type is a sub-term
       of [glob_ty2].

    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let pp_incompatible_types_if_more_precise
    ppf (glob_ty1, glob_ty2, accur_ty1, accur_ty2) =
  if glob_ty1 != accur_ty1 || glob_ty2 != accur_ty2 then (
    Format.fprintf ppf
      "@\n@[Types@ @{<red>%a@}@ and@ @{<red>%a@}@ are@ not@ compatible@]"
      W_PrintTypes.pp_simple_type_continue_sequence accur_ty1
      W_PrintTypes.pp_simple_type_end_sequence accur_ty2
  )
  else (
    (* Nothing else to print, byt since we are currently in a started printing
       sequence, we must close it to cleanup all the markers set on the types
       already printed. Otherwise, we'll get assert failures. *)
    W_PrintTypes.pp_nothing_end_sequence ()
  ) ;
  try_explain_ty_incompatibility ppf accur_ty1 accur_ty2



(* ************************************************************************** *)
(** {b Descr}: Creates a fake record type, closed row in a closed column
    from a list of fields and a list of labels, making this record type having
    the fields listed in the list of labels.
    This function is intended to be used when reporting type error in
    dot-access to create a "smaller" record type that the one in which the
    dot-access was done, hence avoiding if this type is huge to flood the user
    with tons of fields.
    So, we just rebuild a smaller type from the original type, selecting only
    its fields that are listed in the specified list of fields (supposed to
    be a list of "interesting" fields to describe the type in the context of
    the error).

    {b Args}:
     - [original_fields] : The list of fields labels with their type of the
       original record type involved in the typechecking error.
     - [interresting_fields_names] : The list of labels considered as
       "interesting"  and that must be taken from [original_fields] to rebuild
       the fake record type. Obviously, this list is expected to contain
       fields names also present in [original_fields], otherwise we will get
       not match and the resulting fake record type will have to field !

    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let create_fake_shorten_record_ty ~original_fields ~interresting_fields_names =
  (* Recover only the labels and types from the original type that are present
     in the list of possibly interesting fields. *)
  let fields =
    List.filter
      (fun (l, _) -> List.mem l interresting_fields_names)
      original_fields in
  (* For readabiolity, sort fields names. *)
  let sorted_fields = List.sort (fun (l1, _) (l2, _) -> compare l1 l2) fields in
  (* Now, build a closed record type plugged in a closed column (we use for
     this [CoreTypes.type_module_record] that makes a type closed in both
     directions) from these fields. *)
  W_CoreTypes.type_module_record sorted_fields



(** [err_ty1] : First deeper type causing the unification error.
    [err_ty2] : Second deeper type causing the unification error.

    {b Visibility}: Exported outside this module.                     *)
let report_unification_conflict_with_context
    env (context, err_ty1, err_ty2, detail) =
  (* Recover by side effect the annotation map that really contains source
     locations. *)
  let public_annotmap_with_locs = get_annotmap_for_error_report () in
  (* Issue a dedicated error message and get the error context used to pinpoint
     the location of the error in the source code for the coming general failure
     notification. *)
  match context with
  | W_InferErrors.UCC_pattern_coerce (pat, pat_ty, coercing_ty) ->
      let err_ctxt =
        QmlError.Context.annoted_pat public_annotmap_with_locs pat in
      QmlError.error err_ctxt
        ("@[Pattern@ has@ type@ @{<red>%a@}@ but@ is@ coerced@ into@ " ^^
         "@{<red>%a@}.@]%a%a@.")
        W_PrintTypes.pp_simple_type_start_sequence pat_ty
        W_PrintTypes.pp_simple_type_continue_sequence coercing_ty
        pp_incompatible_types_if_more_precise
        (pat_ty, coercing_ty, err_ty1, err_ty2)
        pp_unification_conflict_detail detail
  | W_InferErrors.UCC_apply (expr, fun_pat_ty, tmp_fun_ty) ->
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      QmlError.error err_ctxt
        ("@[Function@ was@ found@ of@ type@ @{<red>%a@}@ but@ " ^^
         "application@ expects@ it@ to@ be@ of@ type@ @{<red>%a@}.@]%a%a@.")
        W_PrintTypes.pp_simple_type_start_sequence fun_pat_ty
        W_PrintTypes.pp_simple_type_continue_sequence tmp_fun_ty
        pp_incompatible_types_if_more_precise
        (fun_pat_ty, tmp_fun_ty, err_ty1, err_ty2)
        pp_unification_conflict_detail detail
  | W_InferErrors.UCC_match_left_part_ty_previous_vs_ty_current
      (expr, previous_left_ty, current_left_ty) ->
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      QmlError.error err_ctxt
        ("@[Matched@ expression@ or@ previous@ patterns@ have@ type@ " ^^
         "@{<red>%a@}@ but@ new@ pattern@ is@ found@ of@ type@ " ^^
         "@{<red>%a@}.@]%a%a@.")
        W_PrintTypes.pp_simple_type_start_sequence previous_left_ty
        W_PrintTypes.pp_simple_type_continue_sequence current_left_ty
        pp_incompatible_types_if_more_precise
        (previous_left_ty, current_left_ty, err_ty1, err_ty2)
        pp_unification_conflict_detail detail
  | W_InferErrors.UCC_match_ty_right_parts_vs_ty_branch
        (expr, ty_right_parts, ty_branch) ->
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      QmlError.error err_ctxt
        ("@[Previous@ right-side@ parts@ of@ the@ pattern@ matching@ " ^^
           "return@ type@ @{<red>%a@}@ but@ current@ one@ returns@ " ^^
           "type@ @{<red>%a@}.@]%a%a@.")
        W_PrintTypes.pp_simple_type_start_sequence ty_right_parts
        W_PrintTypes.pp_simple_type_continue_sequence ty_branch
        pp_incompatible_types_if_more_precise
        (ty_right_parts, ty_branch, err_ty1, err_ty2)
        pp_unification_conflict_detail detail
  | W_InferErrors.UCC_dot
      (expr, rec_expr_ty, accessed_field_rec_ty, accessed_label) -> (
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      (* First, expand possible sequence of type abbrevs in order to try
         to discover the real structure of the type. *)
      let rec_expr_ty_unwinded =
        W_TypeAbbrevs.fully_expand_abbrev
          env W_TypeAbbrevs.empty_memory rec_expr_ty in
      (* Try to see, if the expression in which we dot it a record, which
         field is the closest from the one we tried to use. *)
      match
        (W_CoreTypes.simple_type_repr rec_expr_ty_unwinded).W_Algebra.sty_desc
      with
      | W_Algebra.SType_sum_of_records column -> (
          (* Ok, the type is a sum. *)
          let (cases, _) =
            (W_CoreTypes.column_type_repr column).W_Algebra.ct_value in
          (* If there are several cases, then we tried to "dot" in a sum
             and not in a record. Otherwise, we really lookup for the
             possible misspelling of the field. *)
          match cases with
          | [] ->
              QmlError.error err_ctxt
                ("@[Record@ has@ type@ @{<red>%a@}@ but@ field@ access@ " ^^
                 "expected@ it@ to@ have@ type@ @{<red>%a@}@]@." ^^
                 "@[<2>@{<bright>Hint@}:@\nYou@ tried@ to@ access@ an@ " ^^
                 "empty@ sum@ type@ as@ a@ record.@]@\n%a@.")
                W_PrintTypes.pp_simple_type_start_sequence rec_expr_ty_unwinded
                W_PrintTypes.pp_simple_type_continue_sequence
                accessed_field_rec_ty
                pp_unification_conflict_detail detail
          | [unique_case] -> (
              let (fields, _) =
                (W_CoreTypes.row_type_repr unique_case).
                  W_Algebra.rt_value in
              let labels = List.map fst fields in
              (* Get the list of labels in the dotted type sorted that are
                 close to the label used to make the access. Closest labels are
                 in head of the list. *)
              let close_labels =
                HintUtils.get_closest_names labels accessed_label in
              (* Only retains a few found close labels. Say that ... 7 is
                 sufficient. Why 7 ? Because it's a prime number and really
                 lower than 42 ^^. *)
              let few_close_labels = List.take 7 close_labels in
              if few_close_labels <> [] then (
                (* We found some fields in the record type that are close to
                   the one used to perform the dot-access.
                   So, Let's create a fake and shorter record type in which we
                   will only show the labels close to the one used to make the
                   access with their type.
                   This will allow to avoid printing huge records and print
                   instead a sub-record containing only possibly interesting
                   fields.
                   Of course, this record type won't be the real type of the
                   accessed expression, but this abbreviated information may be
                   easier for the user to dig into, instead of reading the ton
                   of irrelevant fields to find the unique interesting one ! *)
                let shortened_record_ty =
                  create_fake_shorten_record_ty
                    ~original_fields: fields
                    ~interresting_fields_names: few_close_labels in
                QmlError.error err_ctxt
                  ("@[Expression@ has@ a@ record@ type@ incompatible@ for@ " ^^
                     "access@ to@ field@ @{<red>%s@}. Its@ type,@ limited@ " ^^
                     "to@ the@ most@ probable@ fields@ you@ want@ to@ " ^^
                     "access,@ is@ @{<red>%a@}.@]@\n%a%a@.")
                  accessed_label
                  W_PrintTypes.pp_simple_type shortened_record_ty
                  (HintUtils.pp_suggestion labels) accessed_label
                  pp_unification_conflict_detail detail
              )
              else (
                (* The search of suggestion for fields close to the one used
                   to make the access gave nothing. So, in this case, fall-back
                   on printing directly the 2 guilty types. *)
                QmlError.error err_ctxt
                  ("@[Record@ expression@ has@ type@ @{<red>%a@}@ but@ " ^^
                   "field@ access@ expected@ it@ to@ have@ type@ " ^^
                   "@{<red>%a@}.@]@\n%a%a@.")
                  W_PrintTypes.pp_simple_type_start_sequence
                    rec_expr_ty_unwinded
                  W_PrintTypes.pp_simple_type_continue_sequence
                  accessed_field_rec_ty
                  (HintUtils.pp_suggestion labels) accessed_label
                  pp_unification_conflict_detail detail
              )
            )
          | _ ->
              QmlError.error err_ctxt
                ("@[Record@ has@ type@ @{<red>%a@}@ but@ field@ access@ " ^^
                 "expected@ it@ to@ have@ type@ @{<red>%a@}@]@." ^^
                 "@[<2>@{<bright>Hint@}:@\nYou@ tried@ to@ access@ a@ " ^^
                 "sum@ type@ with@ several@ cases@ as@ a@ " ^^
                 "record.@]@\n%a")
                W_PrintTypes.pp_simple_type_start_sequence rec_expr_ty_unwinded
                W_PrintTypes.pp_simple_type_continue_sequence
                accessed_field_rec_ty
                pp_unification_conflict_detail detail
        )
      | _ ->
          (* Other cases than a type sum. *)
          QmlError.error err_ctxt
            ("@[Record@ expression@ has@ type@ @{<red>%a@}@ but@ field@ " ^^
             "access@ expected@ it@ to@ have@ type@ @{<red>%a@}.@]%a@.")
            W_PrintTypes.pp_simple_type_start_sequence
            rec_expr_ty  (* Since unwinding didn't give a sum, use the
                            non-unwinded type for the error message. *)
            W_PrintTypes.pp_simple_type_continue_sequence
            accessed_field_rec_ty
            pp_unification_conflict_detail detail
      )
  | W_InferErrors.UCC_record_extend (expr, extended_expr_ty, extension_ty) ->
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      QmlError.error err_ctxt
        ("@[Record@ to@ update@ has@ type@ @{<red>%a@}@ but@ extension@ " ^^
         "requires@ it@ to@ have@ type@ @{<red>%a@}.@]%a%a@.")
        W_PrintTypes.pp_simple_type_start_sequence extended_expr_ty
        W_PrintTypes.pp_simple_type_continue_sequence extension_ty
        pp_incompatible_types_if_more_precise
        (extended_expr_ty, extension_ty, err_ty1, err_ty2)
        pp_unification_conflict_detail detail
  | W_InferErrors.UCC_coerce (expr, expr_ty, coercing_ty) ->
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      QmlError.error err_ctxt
        ("@[Expression@ has@ type@ @{<red>%a@}@ but@ is@ coerced@ " ^^
         "into@ @{<red>%a@}.@]%a%a@.")
        W_PrintTypes.pp_simple_type_start_sequence expr_ty
        W_PrintTypes.pp_simple_type_continue_sequence coercing_ty
        pp_incompatible_types_if_more_precise
        (expr_ty, coercing_ty, err_ty1, err_ty2)
        pp_unification_conflict_detail detail
  | W_InferErrors.UCC_let_rec_body (binding_name, expr, body_ty,
                                    expected_ty) ->
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      QmlError.error err_ctxt
        ("@[Recursive@ identifier@ @{<red>%s@}@ was@ found@ of@ type@ " ^^
         "@{<red>%a@}@ but@ expected@ to@ be@ of@ type@ @{<red>%a@}.@]%a%a@.")
        (Ident.original_name binding_name)
        W_PrintTypes.pp_simple_type_start_sequence body_ty
        W_PrintTypes.pp_simple_type_continue_sequence expected_ty
        pp_incompatible_types_if_more_precise
        (body_ty, expected_ty, err_ty1, err_ty2)
        pp_unification_conflict_detail detail
  | W_InferErrors.UCC_unknown_directive (expr, expected_ty, inferred_ty) ->
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      QmlError.error err_ctxt
        ("@[Directive@ was@ expected@ to@ be@ of@ type@ @{<red>%a@}@ " ^^
         "but@ was@ found@ of@ type@ @{<red>%a@}.@]%a%a@.")
        W_PrintTypes.pp_simple_type_start_sequence expected_ty
        W_PrintTypes.pp_simple_type_continue_sequence inferred_ty
        pp_incompatible_types_if_more_precise
        (expected_ty, inferred_ty, err_ty1, err_ty2)
        pp_unification_conflict_detail detail
  | W_InferErrors.UCC_catch (expr, expected_handler_ty, handler_ty) ->
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      QmlError.error err_ctxt
        ("@[Exception@ catching@ expected@ handler@ to@ be@ of@ type@ " ^^
         "@{<red>%a@}@ but@ was@ found@ of@ type@ @{<red>%a@}.@]%a%a@.")
        W_PrintTypes.pp_simple_type_start_sequence expected_handler_ty
        W_PrintTypes.pp_simple_type_continue_sequence handler_ty
        pp_incompatible_types_if_more_precise
        (expected_handler_ty, handler_ty, err_ty1, err_ty2)
        pp_unification_conflict_detail detail
  | W_InferErrors.UCC_throw (expr, curr_exn_ty, thrown_ty) ->
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      QmlError.error err_ctxt
        ("@[Exception@ raising@ expected@ a@ type@ compatible@ with@ the@ " ^^
         "current@ exceptions@ type's@ structure @{<red>%a@}@ but@ found@ " ^^
         "type@ @{<red>%a@}.@]%a%a@.")
        W_PrintTypes.pp_simple_type_start_sequence curr_exn_ty
        W_PrintTypes.pp_simple_type_continue_sequence thrown_ty
        pp_incompatible_types_if_more_precise
        (curr_exn_ty, thrown_ty, err_ty1, err_ty2)
        pp_unification_conflict_detail detail



(* ************************************************************************** *)
(** {b Descr}: This function implements detection of cyclic or ill-formed
    types as a post-mortem report. When a cyclic type is detected, we afford
    descending on the whole typechecked expression to see at each node if
    there is a cyclic or ill-formed type. This is a bit costly but that's not a
    big deal since it is used only in case of error. This brings a more precise
    error report than just raising an exception with the type.
    We are forced to make this descent since cyclic or ill-formed types are
    detected after typechecking, during the whole expression or the annotmap's
    types exportation. In the case of the whole expression, we can accurately
    report ... the expression, but when the error arises during annotmap
    exportation, since we "map" on the annotmap, we don't know where the guilty
    type was detected. So, inspecting the expression's types, we can get a
    better, more accurate, location of the error. This saves time for the user
    finding where the guilty type was detected.
    We make 3 local functions for the descent, this way the main, and only
    exported function [report_cyclic_or_ill_formed_type_in_expr] can remind the
    global hosting expression. That's sometimes useful to locate the smallest
    one (i.e. get context around) when we have no line number location like
    with opatop.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let report_cyclic_or_ill_formed_type_in_expr annotmap original_expr =

  let rec __report_cyclic_or_ill_formed_type_in_pattern pat =
    (* Make a deep search first to detect the deepest pattern where the problem
       exists. This will give the smallest location enclosing the apparition of
       the guilty type. *)
    (match pat with
    | QmlAst.PatAny _
    | QmlAst.PatVar _
    | QmlAst.PatConst _ -> ()
    | QmlAst.PatRecord (_, fields, _) ->
        List.iter
          (fun (_, p) -> __report_cyclic_or_ill_formed_type_in_pattern p)
          fields
    | QmlAst.PatCoerce (_, sub_pat, _) ->
        __report_cyclic_or_ill_formed_type_in_pattern sub_pat
    | QmlAst.PatAs (_, sub_pat, _) ->
        __report_cyclic_or_ill_formed_type_in_pattern sub_pat
    ) ;
    (* If we arrive here, then no error was detected in our sub-terms. So
       inspect ourselves to see if we would not be the first pattern presenting
       the problem. *)
    let ty = QmlAnnotMap.find_ty (QmlAst.QAnnot.pat pat) annotmap in
    try ignore (W_PublicExport.simple_type_to_qml_type ty)
    with
    | W_PublicExport.Cyclic_type cyclic_ty ->
        OManager.printf
          ("@[Cyclic@ type@ @{<red>%a@}@]@.")
          W_PrintTypes.pp_simple_type cyclic_ty ;
        let public_annotmap_with_locs = get_annotmap_for_error_report () in
        let err_ctxt =
          QmlError.Context.annoted_pat public_annotmap_with_locs pat in
        QmlError.error err_ctxt "Exportation failure"
    | W_PublicExport.Ill_formed_column_type (ill_formed_ty, err_kind) ->
        (match err_kind with
         | W_PublicExport.IFCTR_row_and_column_vars ->
             OManager.printf
               "@[Sum@ type@ with@ row@ and@ column@ variables@ "
         | W_PublicExport.IFCTR_sum_case_with_row_variable ->
             OManager.printf
               "@[Closed@ sum@ type@ with@ row@ variable(s)@ ") ;
        OManager.printf "@{<red>%a@}@]@."
          W_PrintTypes.pp_simple_type ill_formed_ty ;
        let public_annotmap_with_locs = get_annotmap_for_error_report () in
        let err_ctxt =
          QmlError.Context.annoted_pat public_annotmap_with_locs pat in
        QmlError.error err_ctxt "Exportation failure" in



  let rec __report_cyclic_or_ill_formed_type_in_expr expr =
    (* Make a deep search first to detect the deepest pattern where the problem
       exists. This will give the smallest location enclosing the apparition of
       the guilty type. *)
    (match expr with
    | QmlAst.Const _ | QmlAst.Ident _ | QmlAst.Bypass _ -> ()
    | QmlAst.LetIn (_, def_bindings, in_expr)
    | QmlAst.LetRecIn (_, def_bindings, in_expr) ->
        List.iter
          (fun (_, binding_expr) ->
            __report_cyclic_or_ill_formed_type_in_expr binding_expr)
          def_bindings ;
        __report_cyclic_or_ill_formed_type_in_expr in_expr
    | QmlAst.Lambda (_, _, body) ->
        __report_cyclic_or_ill_formed_type_in_expr body
    | QmlAst.Apply (_, fun_part_expr, args_exprs) ->
        __report_cyclic_or_ill_formed_type_in_expr fun_part_expr ;
        List.iter __report_cyclic_or_ill_formed_type_in_expr args_exprs
    | QmlAst.Match (_, matched_e, cases_list) ->
        __report_cyclic_or_ill_formed_type_in_expr matched_e ;
        List.iter
          (fun (pat, right_part_expr) ->
            __report_cyclic_or_ill_formed_type_in_pattern pat ;
            __report_cyclic_or_ill_formed_type_in_expr right_part_expr)
          cases_list
    | QmlAst.Record (_, fields_exps) ->
        List.iter
          (fun (_, field_expr) ->
            __report_cyclic_or_ill_formed_type_in_expr field_expr)
          fields_exps
    | QmlAst.Dot (_, record_expr, _) ->
        __report_cyclic_or_ill_formed_type_in_expr record_expr
    | QmlAst.ExtendRecord (_, _, field_expr, record_expr) ->
        __report_cyclic_or_ill_formed_type_in_expr field_expr ;
        __report_cyclic_or_ill_formed_type_in_expr record_expr
    | QmlAst.Coerce (_, expr, _) ->
        __report_cyclic_or_ill_formed_type_in_expr expr
    | QmlAst.Directive (_, _, dir_exprs, _) ->
        (* Blindly descend in the expressions embedded in the directive. Don't
           try to see if directive is well-formed, i.e. if for each kind of
           known directive, it really contains the expected kind of embedded
           expressions. We are just there to report an error, so make short ! *)
        List.iter __report_cyclic_or_ill_formed_type_in_expr dir_exprs
    | _ ->
        OManager.printf
          "Non handled expression in cyclic types report: %a@."
          QmlPrint.pp#expr expr ;
        failwith
          "TODO: W_ReportCycles.__report_cyclic_or_ill_formed_type_in_expr"
    ) ;
    (* If we arrive here, then no error was detected in our sub-terms. So
       inspect ourselves to see if we would not be the first expression
       presenting the problem. *)
    let ty = QmlAnnotMap.find_ty (QmlAst.QAnnot.expr expr) annotmap in
    try ignore (W_PublicExport.simple_type_to_qml_type ty)
    with
    | W_PublicExport.Cyclic_type cyclic_ty ->
        OManager.printf
          ("@[Cyclic@ type@ @{<red>%a@}@]@.")
          W_PrintTypes.pp_simple_type cyclic_ty ;
        let public_annotmap_with_locs = get_annotmap_for_error_report () in
        let err_ctxt =
          QmlError.Context.annoted_expr public_annotmap_with_locs expr in
        QmlError.error err_ctxt "Exportation failure"
    | W_PublicExport.Ill_formed_column_type  (ill_formed_ty, err_kind) ->
        (match err_kind with
         | W_PublicExport.IFCTR_row_and_column_vars ->
             OManager.printf
               "@[Sum@ type@ with@ row@ and@ column@ variables@ "
         | W_PublicExport.IFCTR_sum_case_with_row_variable ->
             OManager.printf
               "@[Closed@ sum@ type@ with@ row@ variable(s)@ ") ;
        OManager.printf "@{<red>%a@}@]@."
          W_PrintTypes.pp_simple_type ill_formed_ty ;
        let public_annotmap_with_locs = get_annotmap_for_error_report () in
        let err_ctxt =
          QmlError.Context.annoted_expr public_annotmap_with_locs expr in
        QmlError.error err_ctxt "Exportation failure" in



  (* ************************************************************************ *)
  (** {b Descr}: Effective body of the function
      [report_cyclic_or_ill_formed_type_in_expr].
      {b Visibility}: Exported outside this module.                           *)
  (* ************************************************************************ *)
  __report_cyclic_or_ill_formed_type_in_expr original_expr ;
  (* Since the error reporting function must raise an exception once it
     found the error and since we only call it in case we are sure there is
     an error, we should never access the line of code below. *)
  assert false
