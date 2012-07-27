(*
    Copyright Â© 2011, 2012 MLstate

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
  (* To save computation, we directly transform each sum in a list of lists of
     fields names. For this, just define a local flattening function and apply
     it on both sums. *)
  let flatten_cases cases =
    List.map
      (fun row ->
        let row = W_CoreTypes.row_type_repr row in
        List.map fst (fst row.W_Algebra.rt_value))
      cases in
  (* We can't really say that
     one of them is missing case(s) of the other. So, we will try
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

let plurial n = if n>1 then "s" else ""
let are n = if n=1 then "is" else "are"
let counting_ending n =
  if n < 1 then  ""
  else if n = 1 then "st"
  else if n = 2 then "nd"
  else if n = 3 then "rd"
  else "th"

let rec __hint_compare_fun_arguments ppf (real_args, tmp_args, n) =
  match (real_args, tmp_args) with 
   | ([], []) -> ()
   | (real_ty::real_tys, applied_ty::applied_tys) ->
      let real_ty = W_CoreTypes.simple_type_repr real_ty in
      let applied_ty = W_CoreTypes.simple_type_repr applied_ty in
      if (real_ty = applied_ty)
         then __hint_compare_fun_arguments ppf (real_tys, applied_tys, n+1)
         else Format.fprintf ppf 
              ("@\n@[<2>@{<bright>Hint@}:@\nFunction expects a %d%s-argument " ^^
               "of type @\n@{<bright>%a@}@\ninstead of @\n@{<bright>%a@}@]")
               n (counting_ending n) W_PrintTypes.pp_simple_type real_ty
               W_PrintTypes.pp_simple_type applied_ty
    | (real_ty::_, []) ->
      let real_ty = W_CoreTypes.simple_type_repr real_ty in
      Format.fprintf ppf 
        ("@\n@[<2>@{<bright>Hint@}:@\nFunction expects a %d%s-argument of type" ^^
         "@\n@{<bright>%a@}.@]")
           n (counting_ending n) W_PrintTypes.pp_simple_type real_ty
    | _ -> ()

let hint_compare_fun_arguments ppf (real_args, tmp_args) =
   __hint_compare_fun_arguments ppf (real_args, tmp_args, 1) 
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
let try_explain_ty_incompatibility ppf (accur_ty1, accur_ty2) =
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
            let is_one_elem =  (List.length fields_of_1_not_in_2) == 1 in
            Format.fprintf ppf
              ("@\n@[<2>@{<bright>Hint@}:@\nField%s@ ")
                (if is_one_elem then "" else "s");
            List.iter
              (fun (n, _) -> Format.fprintf ppf "@{<red>%s@}@ " n)
              fields_of_1_not_in_2 ;
            Format.fprintf ppf
              "only@ appear%s@ in@ the@ first@ type.@]@\n"
                (if is_one_elem then "s" else "")
          ) ;
          if fields_of_2_not_in_1 <> [] then (
            let is_one_elem =  (List.length fields_of_2_not_in_1) == 1 in
            Format.fprintf ppf
              ("@\n@[<2>@{<bright>Hint@}:@\nField%s@ ")
                (if is_one_elem then "" else "s");
            List.iter
              (fun (n, _) -> Format.fprintf ppf "@{<red>%s@}@ " n)
              fields_of_2_not_in_1 ;
            Format.fprintf ppf
              "only@ appear%s@ in@ the@ second@ type.@]@\n"
                (if is_one_elem then "s" else "")
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
                  ("@\n@[<2>@{<bright>Hint@}:@\nFirst@ type@ does@ not@ include@ " ^^
                   "the@ following@ cases@ from@ second@ type:") ;
                print_cases miss2_in1 ;
                Format.fprintf ppf ".@]@\n"
              ) ;
              if miss1_in2 <> [] then (
                Format.fprintf ppf
                  ("@\n@[<2>@{<bright>Hint@}:@\nSecond@ type@ does@ not@ include@ " ^^
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

let pp_location_hints ppf
  ( err_ty1, _, err_ty2, _
  , ty1 , _, ty2, _) =
  match ( W_SubTerms.locate_subterms err_ty1.W_Algebra.sty_desc ty1.W_Algebra.sty_desc
        , W_SubTerms.locate_subterms err_ty2.W_Algebra.sty_desc ty1.W_Algebra.sty_desc) with
   |(Some (_, s1), Some (_, s2)) -> (
       Format.fprintf ppf
         ("@\n@[<2>@{<bright>Hint@}:@\nIn type @{<red>%a@} " ^^
          "the %s and the %s should be the same.@]@]")
         W_PrintTypes.pp_simple_type ty1 s1 s2
   )
   | _ ->(
    match ( W_SubTerms.locate_subterms err_ty1.W_Algebra.sty_desc ty2.W_Algebra.sty_desc
          , W_SubTerms.locate_subterms err_ty2.W_Algebra.sty_desc ty2.W_Algebra.sty_desc) with
      |(Some (_, s1), Some (_, s2)) -> (
       Format.fprintf ppf
         ("@\n@[<2>@{<bright>Hint@}:@\nIn@ type@ @{<red>%a@} " ^^
          "%s and@ %s@ should@ be@ the@ same.@]@]")
         W_PrintTypes.pp_simple_type ty2 s1 s2
     )
     | _ -> ()
  )

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
     | W_Unify.DK_fun_type_arity _ -> false
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

(** TEST FUNCTION **)
let test_fun_error err_ty1 err_ty2 =
  let err_ty1 = err_ty1.W_Algebra.sty_desc in
  let err_ty2 = err_ty2.W_Algebra.sty_desc in
  let aux t1 t2 =
     match (t1, t2) with
       | (W_Algebra.SType_arrow (_, _), W_Algebra.SType_arrow(_, _)) -> false
       | (W_Algebra.SType_arrow (_, _), _) -> true
       | (_, W_Algebra.SType_arrow(_, _)) -> true
       | _ -> false
  in aux err_ty1 err_ty2

(**  NEW PRINING **)

let pp_info ppf = function
  | W_TypeInfo.NoInfo   s -> (*turn this to blank before sub*)
     Format.fprintf ppf "%s" (s ^ String.make (20 - String.length s) ' ')
  | W_TypeInfo.Location l ->
      Format.fprintf ppf "%a" W_Misc.pp_pos_short (Annot.pos l)
  | W_TypeInfo.FromEnv (_, l) ->
(*     let package_name = Ident.get_package_name id in
     let of_package_name =
       if package_name = "" then "" else (" of " ^ package_name) in
     let name = Ident.original_name id ^ of_package_name in
     let length = String.length name in
     let tab  = if length < 20 then String.make (20 - length) ' ' else "" in
     Format.fprintf ppf "%s"  (name ^ tab)
*)
      Format.fprintf ppf "%a" W_Misc.pp_pos_short (Annot.pos l)
  | W_TypeInfo.Directive (_, l) ->
     Format.fprintf ppf "%a" W_Misc.pp_pos_short (Annot.pos l)
  | W_TypeInfo.Link  _    ->
     Format.fprintf ppf "(link)              "
  | W_TypeInfo.Exception    ->
     Format.fprintf ppf "(exception)         "

(*  compare  conflicting types  *)

let compare_types t1 t2 =
  match (t1.W_Algebra.sty_desc, t2.W_Algebra.sty_desc) with
   | (W_Algebra.SType_arrow(args1, _), W_Algebra.SType_arrow(args2, _)) -> (
      if (List.length args1 = List.length args2)
       then W_PrintTypes.print_subtype_of_function ()
       else W_PrintTypes.print_function_with_n_args ()
   )
   | (_, W_Algebra.SType_arrow(_, _)) | (W_Algebra.SType_arrow(_, _), _)
       -> W_PrintTypes.print_only_function ()
   | (_, _) -> W_PrintTypes.print_subtype_of_function ()

let pp_precise_error ppf (t1, i1, t2, i2) =
  let cmp = W_TypeInfo.cmp_info i1 i2 in
  W_PrintTypes.set_error_type1 t1;
  W_PrintTypes.set_error_type2 t2;
  let (t1, i1, t2, i2) =
    if cmp <= 0 then (t1, i1, t2, i2) else (t2, i2, t1, i1) in
  compare_types t1 t2;
  Format.fprintf ppf
    "@\n  @[@[%a%a@]@\n@[%a%a@]@\n"
        pp_info i1
        W_PrintTypes.pp_simple_type_start_sequence t1
        pp_info i2
        W_PrintTypes.pp_simple_type_continue_sequence t2

(*At least one of the error_types is function type*)
let report_fun_conflict
    _ (context, err_ty1, err_ty2, _) =
  let err_loc1 = W_TypeInfo.retrieve err_ty1.W_Algebra.sty_desc in
  let err_loc2 = W_TypeInfo.retrieve err_ty2.W_Algebra.sty_desc in
  let (fun_error_ty, err_ty, args_number, err_loc) =
    match (err_ty1.W_Algebra.sty_desc, err_ty2.W_Algebra.sty_desc) with
      | (W_Algebra.SType_arrow (args, _), _) ->
          (err_ty1, err_ty2, List.length args, err_loc1)
      | (_, W_Algebra.SType_arrow (args, _)) ->
          (err_ty2, err_ty1, List.length args, err_loc2)
      | (_, _) -> (err_ty1, err_ty2, -1, err_loc1) in
  let reason = "Missing Application" in
  let hint ppf _ =
   if args_number = 0
    then
     Format.fprintf ppf "Unit argument is missing at %a" pp_info err_loc
    else if args_number = 1
    then
     Format.fprintf ppf "1 argument is missing at %a" pp_info err_loc
    else
     Format.fprintf ppf "%d arguments are missing at %a"
       args_number pp_info err_loc in
  let public_annotmap_with_locs = get_annotmap_for_error_report () in
  let default_message err_ctxt =    
     QmlError.error ~msg:reason err_ctxt
       ("%a@\n@[<2>Can not match function type with %a.@]" ^^
        "@\n@[<2>@{<bright>Hint@}:@\n%a@]@.")
           pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
           W_PrintTypes.pp_simple_type err_ty
           hint () in
  match context with
  | W_InferErrors.UCC_apply (expr, fun_pat_ty, _ (*tmp_fun_ty*)) ->(
     W_Misc.set_error_position (Annot.pos (Annot.Magic.label expr));
     let fun_name =
       match expr with
         | QmlAst.Apply(_, (QmlAst.Ident(_, id)), _) ->
           let package_name = Ident.get_package_name id in
           let of_package_name =
             if package_name = "" then " " else (" of " ^ package_name ^ " ") in
           (Ident.original_name id ^ of_package_name)
         | _ -> "" in
      W_Misc.set_error_position (Annot.pos (Annot.Magic.label expr));
    match fun_pat_ty.W_Algebra.sty_desc with
     | W_Algebra.SType_arrow (fun_args_ty, _)
        when List.exists
             (fun t -> t.W_Algebra.sty_desc == fun_error_ty.W_Algebra.sty_desc)
             fun_args_ty -> (
        let err_ctxt =
          QmlError.Context.annoted_expr public_annotmap_with_locs expr in
        QmlError.error ~msg:reason err_ctxt
           ("%a@\n@[<2>Can not match function type with %a.@]" ^^
           "@\n@[<2>@{<bright>Hint@}:The expression should have" ^^
           " function type@\n%a@]@.")
            pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
            W_PrintTypes.pp_simple_type err_ty
     )
     | W_Algebra.SType_arrow (_, _) -> (
        let err_ctxt =
          QmlError.Context.annoted_expr public_annotmap_with_locs expr in
        QmlError.error ~msg:reason err_ctxt
           ("%a@\n@[<2>Can not match function type with %a.@]" ^^
           "@\n@[<2>@{<bright>Hint@}:@\n%a@]@.")
            pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
            W_PrintTypes.pp_simple_type err_ty
            hint ()
     )
     | _ -> (
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      QmlError.error ~msg:reason  err_ctxt
        "%a@\n@[<2>Expression @{<red>%s@}is not a function, it can not be applied.@."
        pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
        fun_name
     )
   )
   | W_InferErrors.UCC_pattern_coerce(pat, _, _) -> (
      W_Misc.set_error_position (Annot.pos (Annot.Magic.label pat));
      let err_ctxt =
        QmlError.Context.annoted_pat public_annotmap_with_locs pat in
      default_message err_ctxt 
   )
   | W_InferErrors.UCC_match_left_part_ty_previous_vs_ty_current(expr, _, _)
   | W_InferErrors.UCC_match_ty_right_parts_vs_ty_branch(expr, _, _)
   | W_InferErrors.UCC_dot (expr, _, _, _)
   | W_InferErrors.UCC_record_extend (expr, _, _)
   | W_InferErrors.UCC_coerce (expr, _, _)
   | W_InferErrors.UCC_let_rec_body (_, expr, _, _)
   | W_InferErrors.UCC_unknown_directive (expr, _, _)
   | W_InferErrors.UCC_catch (expr, _, _)
   | W_InferErrors.UCC_throw (expr, _, _) -> ( (*default error msg*)
     W_Misc.set_error_position (Annot.pos (Annot.Magic.label expr));
     let err_ctxt =
		   QmlError.Context.annoted_expr public_annotmap_with_locs expr in
     default_message err_ctxt 
   )

(** [err_ty1] : First deeper type causing the unification error.
    [err_ty2] : Second deeper type causing the unification error.

    {b Visibility}: Exported outside this module.                     *)
let report_unification_conflict_with_context
    env (context, err_ty1, err_ty2, detail) =
  let err_loc1 = W_TypeInfo.retrieve err_ty1.W_Algebra.sty_desc in
  let err_loc2 = W_TypeInfo.retrieve err_ty2.W_Algebra.sty_desc in
  let reason = "Type Conflict" in
  (* Recover by side effect the annotation map that really contains source
     locations. *)
  let public_annotmap_with_locs = get_annotmap_for_error_report () in

  (
 (* Issue a dedicated error message and get the error context used to pinpoint
     the location of the error in the source code for the coming general failure
     notification. *)
  match context with
  | W_InferErrors.UCC_pattern_coerce (pat, pat_ty, coercing_ty) ->
      W_Misc.set_error_position (Annot.pos (Annot.Magic.label pat));
      let err_ctxt =
        QmlError.Context.annoted_pat public_annotmap_with_locs pat in
      let ty_loc1 = W_TypeInfo.retrieve pat_ty.W_Algebra.sty_desc in
      let ty_loc2 = W_TypeInfo.retrieve coercing_ty.W_Algebra.sty_desc in
      W_PrintTypes.pp_simple_type_prepare_sequence [pat_ty; coercing_ty; err_ty1; err_ty2];
      QmlError.error ~msg:reason err_ctxt
        ("%a@\n@[<2>Pattern has type@\n@{<red>%a@}@\nbut is coerced into " ^^
         "@\n@{<red>%a@}@\n@]%a%a%a@.")
        pp_precise_error (err_ty1, err_loc1, err_ty2, err_loc2)
        W_PrintTypes.pp_simple_type_start_sequence pat_ty
        W_PrintTypes.pp_simple_type_continue_sequence coercing_ty
        try_explain_ty_incompatibility (err_ty1, err_ty2)
        pp_unification_conflict_detail detail
        pp_location_hints ( err_ty1, err_loc1
                          , err_ty2, err_loc2
                          , pat_ty , ty_loc1
                          , coercing_ty, ty_loc2)
  | W_InferErrors.UCC_apply (expr, fun_pat_ty, tmp_fun_ty) ->(
     let fun_name =
       match expr with
         | QmlAst.Apply(_, (QmlAst.Ident(_, id)), _) ->
           let package_name = Ident.get_package_name id in
           let of_package_name =
             if package_name = "" then " " else (" of " ^ package_name ^ " ") in
           (Ident.original_name id ^ of_package_name)
         | _ -> "" in
      W_Misc.set_error_position (Annot.pos (Annot.Magic.label expr));
      let err_ctxt =
           QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      let replace_message str = 
            QmlError.error ~msg:reason err_ctxt
              ("%a@[@\nThe %s of function @{<red>%s@}should be of type@\n\t@[@{<red>%a@}@]" ^^
               "@\ninstead of @\n\t@[@{<red>%a@}@]@]@.")
                  pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
                  str
                  fun_name
                  W_PrintTypes.pp_simple_type_start_sequence err_ty1
                  W_PrintTypes.pp_simple_type_continue_sequence err_ty2 in
      let ty_loc1 = W_TypeInfo.retrieve fun_pat_ty.W_Algebra.sty_desc in
      let ty_loc2 = W_TypeInfo.retrieve tmp_fun_ty.W_Algebra.sty_desc in
      let default_message _ = 
            QmlError.error ~msg:reason err_ctxt
             ("%a@[@\nFunction @{<red>%s@}has type@\n\t@[@{<red>%a@}@]" ^^
              "@\nbut is applied as a@\n\t@[@{<red>%a@}@]@]%a%a@.")
               pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
              fun_name
              W_PrintTypes.pp_simple_type_start_sequence fun_pat_ty
              W_PrintTypes.pp_simple_type_continue_sequence tmp_fun_ty
              pp_unification_conflict_detail detail
              pp_location_hints ( err_ty1, err_loc1
                                , err_ty2, err_loc2
                                , fun_pat_ty , ty_loc1
                                , tmp_fun_ty, ty_loc2) in
    match fun_pat_ty.W_Algebra.sty_desc with
     | W_Algebra.SType_arrow (args1, _) -> (
       match tmp_fun_ty.W_Algebra.sty_desc with
        | W_Algebra.SType_arrow (args2, _) -> (
           let arg_number1 = List.length args1 in
           let arg_number2 = List.length args2 in
           if arg_number1 != arg_number2 then (
            QmlError.error ~msg:"Different number of Arguments" err_ctxt
              ("%a@\n@[<2>Function @{<red>%s@}takes %d argument%s, but %d %s given.@]%a@.")
                pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
                fun_name arg_number1 (plurial arg_number1)
                arg_number2 (are arg_number2)
                hint_compare_fun_arguments (args1,args2)
           ) else
              let err_ty1_in_tmp =
                    W_SubTerms.locate_subterms err_ty1.W_Algebra.sty_desc
                    tmp_fun_ty.W_Algebra.sty_desc in
              let err_ty2_in_tmp =
                    W_SubTerms.locate_subterms err_ty2.W_Algebra.sty_desc
                    tmp_fun_ty.W_Algebra.sty_desc in
              match err_ty1_in_tmp with
               | Some (n, str1)
                when W_SubTerms.check_arrow_subterm n
                     err_ty2.W_Algebra.sty_desc fun_pat_ty.W_Algebra.sty_desc->(
                 replace_message str1
               )
               | _ -> (
                 match err_ty2_in_tmp with
                  | Some (n2, str2)
                   when W_SubTerms.check_arrow_subterm n2
                        err_ty1.W_Algebra.sty_desc
                        fun_pat_ty.W_Algebra.sty_desc-> (
                   replace_message str2 
                  )
                  | _ -> (
                   match (err_ty1_in_tmp, err_ty2_in_tmp) with
                    | (Some (_, str1), Some (_, str2)) -> (
                    QmlError.error ~msg:reason err_ctxt
                     ("%a@\n@[<2>The types of the@ @{<red>%s@}@ and the@ @{<red>%s@}@ " ^^
                      "of function @{<red>%s@}should be the same@]@.")
                        pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
                        str1 str2
                        fun_name
                    )
                    | _ -> (
                    W_PrintTypes.pp_simple_type_prepare_sequence
                      [fun_pat_ty; tmp_fun_ty; err_ty1; err_ty2];
                    default_message err_ctxt
                  ))
               )
         )
         | _ -> ( (* THIS CASE CAN NOT BE TRIGGERED*)
           W_PrintTypes.pp_simple_type_prepare_sequence
             [fun_pat_ty; tmp_fun_ty; err_ty1; err_ty2];
           default_message err_ctxt
        )
     )
     | _ -> (
      QmlError.error ~msg:reason err_ctxt
        "%a@\n@[<2>Expression %sis not a function, it can not be applied.@."
        pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
        fun_name
     )
   )
  | W_InferErrors.UCC_match_left_part_ty_previous_vs_ty_current
      (expr, previous_left_ty, current_left_ty) ->
      W_Misc.set_error_position (Annot.pos (Annot.Magic.label expr));
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      let ty_loc1 = W_TypeInfo.retrieve previous_left_ty.W_Algebra.sty_desc in
      let ty_loc2 = W_TypeInfo.retrieve current_left_ty.W_Algebra.sty_desc in
      W_PrintTypes.pp_simple_type_prepare_sequence [previous_left_ty; current_left_ty; err_ty1; err_ty2];
      QmlError.error ~msg:reason err_ctxt
        ("%a@\n@[<2>Matched expression or patterns have type" ^^
         "@\n@{<red>%a@}@\nbut a new pattern is found of type@\n" ^^
         "@{<red>%a@}.@]%a%a%a@.")
        pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
        W_PrintTypes.pp_simple_type_start_sequence previous_left_ty
        W_PrintTypes.pp_simple_type_continue_sequence current_left_ty
        try_explain_ty_incompatibility (err_ty1, err_ty2)
        pp_unification_conflict_detail detail
        pp_location_hints ( err_ty1, err_loc1
                          , err_ty2, err_loc2
                          , previous_left_ty , ty_loc1
                          , current_left_ty, ty_loc2)

  | W_InferErrors.UCC_match_ty_right_parts_vs_ty_branch
        (expr, ty_right_parts, ty_branch) ->
      W_Misc.set_error_position (Annot.pos (Annot.Magic.label expr));
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      let ty_loc1 = W_TypeInfo.retrieve
          (W_CoreTypes.simple_type_repr ty_right_parts).W_Algebra.sty_desc in
      let ty_loc2 = W_TypeInfo.retrieve ty_branch.W_Algebra.sty_desc in
      W_PrintTypes.pp_simple_type_prepare_sequence [ty_right_parts; ty_branch; err_ty1; err_ty2];
      QmlError.error ~msg:reason err_ctxt
        ("%a@\n@[<2>Right-side parts of the pattern matching " ^^
           "have type@\n@{<red>%a@}@\nbut this right-side expression has " ^^
           "type@\n@{<red>%a@}@]%a%a@.")
        pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
        W_PrintTypes.pp_simple_type_start_sequence ty_right_parts
        W_PrintTypes.pp_simple_type_continue_sequence ty_branch
        pp_unification_conflict_detail detail
        pp_location_hints ( err_ty1, err_loc1
                          , err_ty2, err_loc2
                          , ty_right_parts , ty_loc1
                          , ty_branch, ty_loc2)
 | W_InferErrors.UCC_dot
      (expr, rec_expr_ty, accessed_field_rec_ty, accessed_label) -> (
      let (record_name, record_or_module) = match expr with
         | QmlAst.Dot(_, (QmlAst.Ident(_, id)), _) ->
           let package_name = Ident.get_package_name id in
           let of_package_name =
             if package_name = "" then " " else (" of " ^ package_name ^ " ") in
           let name = Ident.original_name id in
           let record_module = 
             try (
               let init = name.[0] in
               if (init >= 'A' && init <= 'Z') 
               then "Module" else "Record"
            ) with Invalid_argument _-> "Record" in
           ((name ^ of_package_name), record_module)
        | _ -> ("", "Record") in
      W_Misc.set_error_position (Annot.pos (Annot.Magic.label expr));
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      (* First, expand possible sequence of type abbrevs in order to try
         to discover the real structure of the type. *)
      let ty_loc1 = W_TypeInfo.retrieve rec_expr_ty.W_Algebra.sty_desc in
      let ty_loc2 = W_TypeInfo.retrieve
                     accessed_field_rec_ty.W_Algebra.sty_desc in
      let rec_expr_ty_unwinded =
        W_TypeAbbrevs.fully_expand_abbrev
          env W_TypeAbbrevs.empty_memory rec_expr_ty in
      (* Try to see, if the expression in which we dot is a record, which
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
              QmlError.error ~msg:reason err_ctxt
                ("%a@\n@[<2>%s @{<red>%s@}has type@\n@{<red>%a}@\nbut field " ^^
                 "access expects it to have type@\n@{<red>%a@}@]@." ^^
                 "@[<2>@{<bright>Hint@}:@\nYou@ tried to access an " ^^
                 "empty sum type as a record.@]@\n%a%a@.")
                pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2) 
                record_or_module record_name
                W_PrintTypes.pp_simple_type_start_sequence rec_expr_ty_unwinded
                W_PrintTypes.pp_simple_type_continue_sequence
                  accessed_field_rec_ty
                pp_unification_conflict_detail detail
                pp_location_hints ( err_ty1, err_loc1
                                  , err_ty2, err_loc2
                                  , rec_expr_ty , ty_loc1
                                  , accessed_field_rec_ty, ty_loc2)
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
              let pp_string ppf s = Format.fprintf ppf "%s" s in
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
(*                let shortened_record_ty =
                  create_fake_shorten_record_ty
                    ~original_fields: fields
                    ~interresting_fields_names: few_close_labels in
*)                QmlError.error ~msg:reason err_ctxt
                  ("%a@\n@[<2>%s @{<red>%s@}does not have field @{<red>%s@}.@ " ^^
                   "Here is a summary of fields you may want to access:" ^^
                     "@\n@{<red>%a@}.@]@\n%a%a@.")
                  pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
                  record_or_module record_name accessed_label
                  (BaseFormat.pp_list ",@ " pp_string) labels
                  (HintUtils.pp_suggestion labels) accessed_label
                  pp_unification_conflict_detail detail
             )
              else (
                (* The search of suggestion for fields close to the one used
                   to make the access gave nothing. So, in this case, fall-back
                   on printing directly the 2 guilty types. *)
                QmlError.error ~msg:reason err_ctxt
                  ("%a@\n@[<2>%s @{<red>%s@}does not have field @{<red>%s@}." ^^
                   "@ Here are the fields you may want to access" ^^
                   "@\n@{<red>%a@}@]@\n%a%a%a@.")
                pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
                  record_or_module record_name accessed_label
                  W_PrintTypes.pp_fake_simple_type rec_expr_ty_unwinded
                  (HintUtils.pp_suggestion labels) accessed_label
                  pp_unification_conflict_detail detail
                 pp_location_hints ( err_ty1, err_loc1
                                  , err_ty2, err_loc2
                                  , rec_expr_ty , ty_loc1
                                  , accessed_field_rec_ty, ty_loc2)
             )
            )
          | _ ->
              QmlError.error ~msg:reason err_ctxt
                ("%a@\n@[<2>%s @{<red>%s@}has type@\n@{<red>%a@}@\nbut field " ^^
                 "access expected it to have type @\n@{<red>%a@}@\n@]@." ^^
                 "@[<2>@{<bright>Hint@}:@\nYou tried to access a " ^^
                 "sum type with several cases as a " ^^
                 "record.@]@\n%a%a")
                pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
                record_or_module record_name
                W_PrintTypes.pp_simple_type_start_sequence rec_expr_ty_unwinded
                W_PrintTypes.pp_simple_type_continue_sequence
                  accessed_field_rec_ty
                pp_unification_conflict_detail detail
                pp_location_hints ( err_ty1, err_loc1
                                  , err_ty2, err_loc2
                                  , rec_expr_ty , ty_loc1
                                  , accessed_field_rec_ty, ty_loc2)
      )
      | _ ->(*NOT RECORD*)
          (* Other cases than a type sum. *)
          QmlError.error ~msg:reason err_ctxt
            ("%a@\n@[<2>This expression is not a record.@ " ^^
             "You can not access its fields.")
            pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
            W_PrintTypes.pp_simple_type_start_sequence
            rec_expr_ty  (* Since unwinding didn't give a sum, use the
                            non-unwinded type for the error message. *)
            pp_info ty_loc1
            W_PrintTypes.pp_simple_type_continue_sequence
            accessed_field_rec_ty
            pp_info ty_loc2
            pp_unification_conflict_detail detail
                 pp_location_hints ( err_ty1, err_loc1
                                  , err_ty2, err_loc2
                                  , rec_expr_ty , ty_loc1
                                  , accessed_field_rec_ty, ty_loc2)
     )
  | W_InferErrors.UCC_record_extend (expr, extended_expr_ty, extension_ty) -> (
      W_Misc.set_error_position (Annot.pos (Annot.Magic.label expr));
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      let ty_loc1 = W_TypeInfo.retrieve extended_expr_ty.W_Algebra.sty_desc in
      let ty_loc2 = W_TypeInfo.retrieve extension_ty.W_Algebra.sty_desc in
      match extended_expr_ty.W_Algebra.sty_desc with
       | W_Algebra.SType_sum_of_records _ -> (
      QmlError.error ~msg:reason err_ctxt
        ("%a@\n@[<2>Record to update has type@\n@{<red>%a@}@\nbut " ^^
         "extension requires it to have type@\n@{<red>%a@}@\n@]%a%a%a@.")
        pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
        W_PrintTypes.pp_simple_type_start_sequence extended_expr_ty
        W_PrintTypes.pp_simple_type_continue_sequence extension_ty
        try_explain_ty_incompatibility (err_ty1, err_ty2)
        pp_unification_conflict_detail detail
                 pp_location_hints ( err_ty1, err_loc1
                                  , err_ty2, err_loc2
                                  , extended_expr_ty , ty_loc1
                                  , extension_ty, ty_loc2)
      )
      | _ -> (
      QmlError.error ~msg:reason err_ctxt
        ("%a@\n@[<2>The expression is not a record, " ^^
         "it can not be extended.@\n@]@.")
        pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
      )
     )
 | W_InferErrors.UCC_coerce (expr, expr_ty, coercing_ty) -> (
      W_Misc.set_error_position (Annot.pos (Annot.Magic.label expr));
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      let ty_loc1 = W_TypeInfo.retrieve expr_ty.W_Algebra.sty_desc in
      let ty_loc2 = W_TypeInfo.retrieve coercing_ty.W_Algebra.sty_desc in
      let err_ty1_in_coercing_ty =
        W_SubTerms.locate_subterms err_ty1.W_Algebra.sty_desc
                            coercing_ty.W_Algebra.sty_desc in
      let err_ty2_in_coercing_ty =
        W_SubTerms.locate_subterms err_ty2.W_Algebra.sty_desc
                            coercing_ty.W_Algebra.sty_desc in
      match (err_ty1_in_coercing_ty, err_ty2_in_coercing_ty) with
       | (Some(_, str1), Some(_, str2)) -> (
       QmlError.error ~msg:reason err_ctxt
        ("%a@\n@[<2>The@ @{<red>%s@}@ and the@ @{<red>%s@}@ " ^^
         "of the coercing type should be the same.@]@.")
          pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
          str1 str2
       )
       | (Some(n, str1), _) when W_SubTerms.check_arrow_subterm n
           err_ty2.W_Algebra.sty_desc expr_ty.W_Algebra.sty_desc-> (
        QmlError.error ~msg:reason err_ctxt
        ("%a@\n@[<2>Expression's @{<red>%s@} is@\n@{<red>%a@}@\n" ^^
         "but it is coerced into@\n@{<red>%a@}@]@.")
          pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
          str1
          W_PrintTypes.pp_simple_type_start_sequence err_ty1
          W_PrintTypes.pp_simple_type_continue_sequence err_ty2
        )
       | (_, Some(n, str2)) when W_SubTerms.check_arrow_subterm n
           err_ty1.W_Algebra.sty_desc expr_ty.W_Algebra.sty_desc-> (
        QmlError.error ~msg:reason err_ctxt
        ("%a@\n@[<2>Expression's @{<red>%s@} is@\n@{<red>%a@}@\n" ^^
         "but it is coerced into@\n@{<red>%a@}@]@.")
          pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
          str2
          W_PrintTypes.pp_simple_type_start_sequence err_ty2
          W_PrintTypes.pp_simple_type_continue_sequence err_ty1
        )
        | _ ->
       QmlError.error ~msg:reason err_ctxt
        ("%a@\n@[<2>Expression has type@\n@{<red>%a@}@\nbut is coerced " ^^
         "into@\n@{<red>%a@}@\n@]%a%a@.")
        pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
        W_PrintTypes.pp_simple_type_start_sequence expr_ty
        W_PrintTypes.pp_simple_type_continue_sequence coercing_ty
        pp_unification_conflict_detail detail
        pp_location_hints ( err_ty1, err_loc1
                          , err_ty2, err_loc2
                          , expr_ty , ty_loc1
                          , coercing_ty, ty_loc2)
  )
  | W_InferErrors.UCC_let_rec_body (binding_name, expr, body_ty,
                                    expected_ty) ->
      W_Misc.set_error_position (Annot.pos (Annot.Magic.label expr));
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      (* assuming recursive value are function *)
      QmlError.error ~msg:reason err_ctxt
        ("%a@[@\nFunction @{<red>%s@} is defined as a@\n\t@[@{<red>%a@}@]"^^
         "@\nbut is applied as a@\n\t@[@{<red>%a@}@]@]%a%a@.")
        pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
        (Ident.original_name binding_name)
        W_PrintTypes.pp_simple_type_start_sequence body_ty
        W_PrintTypes.pp_simple_type_continue_sequence expected_ty
        try_explain_ty_incompatibility (err_ty1, err_ty2)
        pp_unification_conflict_detail detail
  | W_InferErrors.UCC_unknown_directive (expr, expected_ty, inferred_ty) -> (
      let directive_name =
        match expr with
         | QmlAst.Directive(_, i, _, _) -> QmlDirectives.to_string i
         | _ -> "" in
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      let ty_loc1 = W_TypeInfo.retrieve expected_ty.W_Algebra.sty_desc in
      let ty_loc2 = W_TypeInfo.retrieve inferred_ty.W_Algebra.sty_desc in
      W_Misc.set_error_position (Annot.pos (Annot.Magic.label expr));
      match ( expected_ty.W_Algebra.sty_desc
            , inferred_ty.W_Algebra.sty_desc) with
       | (W_Algebra.SType_arrow (args1, _), W_Algebra.SType_arrow (args2, _))->(
         let args_num1 = List.length args1 in
         let args_num2 = List.length args2 in
         if (args_num1 != args_num2) then (
          QmlError.error ~msg:reason err_ctxt
            ("%a@\n@[<2>Directive %s is expecting %d argument%s, " ^^
            "but it is given %d.@]@.")
            pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
            directive_name
            args_num1 (if args_num1 = 1 then "" else "s") args_num2
         )
         else (
       QmlError.error ~msg:reason err_ctxt
        ("%a@\n@[<2>Directive %s was expected to be of type@\n@{<red>%a@}@\n" ^^
         "but was found of type@\n@{<red>%a@}@\n@]%a%a%a@.")
        pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
        directive_name
        W_PrintTypes.pp_simple_type_start_sequence expected_ty
        W_PrintTypes.pp_simple_type_continue_sequence inferred_ty
        try_explain_ty_incompatibility (err_ty1, err_ty2)
        pp_unification_conflict_detail detail
         pp_location_hints ( err_ty1, err_loc1
                          , err_ty2, err_loc2
                          , expected_ty , ty_loc1
                          , inferred_ty, ty_loc2)
        )
       )
       | (W_Algebra.SType_arrow (args1, _), _)-> (
         let args_num1 = List.length args1 in
          QmlError.error ~msg:reason err_ctxt
            ("%a@\n@[<2>Directive %s is expecting %d argument%s, " ^^
             "but none is given.@]@.")
            pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
            directive_name
            args_num1 (if args_num1 = 1 then "" else "s")
      )
      | _ -> (
       QmlError.error ~msg:reason err_ctxt
        ("%a@\n@[<2>Directive %s was expected to be of type@\n@{<red>%a@}@\n" ^^
         "but was found of type@\n@{<red>%a@}@\n@]%a%a%a@.")
        pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
        directive_name
        W_PrintTypes.pp_simple_type_start_sequence expected_ty
        W_PrintTypes.pp_simple_type_continue_sequence inferred_ty
        try_explain_ty_incompatibility (err_ty1, err_ty2)
        pp_unification_conflict_detail detail
         pp_location_hints ( err_ty1, err_loc1
                          , err_ty2, err_loc2
                          , expected_ty , ty_loc1
                          , inferred_ty, ty_loc2)
        )
      )
 | W_InferErrors.UCC_catch (expr, expected_handler_ty, handler_ty) ->
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      W_Misc.set_error_position (Annot.pos (Annot.Magic.label expr));
      let ty_loc1 =
        W_TypeInfo.retrieve expected_handler_ty.W_Algebra.sty_desc in
      let ty_loc2 = W_TypeInfo.retrieve handler_ty.W_Algebra.sty_desc in
      QmlError.error ~msg:"Invalid Exception Handler" err_ctxt
        ("%a@\n@[<2>@{<red>%a@}@\n is not a valid exception handler.@\n" ^^
         "Exception handlers should have type @\n@{<red>%a@}@\n@]%a%a%a@.")
        pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
        W_PrintTypes.pp_simple_type_start_sequence handler_ty
        W_PrintTypes.pp_simple_type_continue_sequence expected_handler_ty
        try_explain_ty_incompatibility (err_ty1, err_ty2)
        pp_unification_conflict_detail detail
        pp_location_hints ( err_ty1, err_loc1
                          , err_ty2, err_loc2
                          , expected_handler_ty , ty_loc1
                          , handler_ty, ty_loc2)
 | W_InferErrors.UCC_throw (expr, curr_exn_ty, thrown_ty) ->
      W_Misc.set_error_position (Annot.pos (Annot.Magic.label expr));
      let err_ctxt =
        QmlError.Context.annoted_expr public_annotmap_with_locs expr in
      let ty_loc1 = W_TypeInfo.retrieve curr_exn_ty.W_Algebra.sty_desc in
      let ty_loc2 = W_TypeInfo.retrieve thrown_ty.W_Algebra.sty_desc in
      QmlError.error ~msg:"Invalid Exception" err_ctxt
        ("%a@\n@[<2>@{<red>%a@}@\n is not an exception, you cannot throw it." ^^
         "Exceptions should have the form: @\n@{<red>%a@}@\n@]%a%a%a@.")
        pp_precise_error(err_ty1, err_loc1, err_ty2, err_loc2)
        W_PrintTypes.pp_simple_type_start_sequence thrown_ty
        W_PrintTypes.pp_simple_type_continue_sequence curr_exn_ty
        try_explain_ty_incompatibility (err_ty1, err_ty2)
        pp_unification_conflict_detail detail
        pp_location_hints ( err_ty1, err_loc1
                          , err_ty2, err_loc2
                          , curr_exn_ty, ty_loc1
                          , thrown_ty, ty_loc2)
)

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
