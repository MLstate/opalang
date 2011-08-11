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


(* depends *)
module List = BaseList




type detail_kind =
  | DK_none   (** No additionnal and more precise information. This is the
                   general failure case where types are simply incompatible. *)
  | DK_fun_type_arity of (int * int)  (** Arity mismatch between 2 function
                                           types. *)
  | DK_named_type_arity of (W_Algebra.type_name_ident * int * int)  (** Arity
         mismatch between the arguments of 2 instances of a same named type. *)
  | DK_binding_level_mismatch      (** Case of unification between 2 type
         variables with one being generalized and the other not, or between a
         generalized type variable and another type (not being a type
         variable). *)
  | DK_forall_type_quantification_arity of (int * int)   (** Case where the 2
         schemes of types forall do not have the same number of generalized
         variables. *)



(* ************************************************************************** *)
(** {b Descr}: Information precising a unification conflict when the unification
    failed in a case where we can give more accurate description of the issue
    to the user.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
type unification_conflict_detail = {
  ucd_kind : detail_kind ;
  ucd_through_field : string option (** If the unification conflict appeared
     through the unification of 2 record fields of same name, this is the name
     of the 2 fields that caused the error. *)
}



(* ************************************************************************** *)
(** {b Descr}: Exception raised in case of unification failure on 2 simple
    types.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
exception Unification_simple_type_conflict of
  (W_Algebra.simple_type * W_Algebra.simple_type * unification_conflict_detail)



(* ************************************************************************** *)
(** {b Descr}: Exception raised in case of unification failure on 2 sum
    types (i.e. on 2 columns). This exception must not escape when unification
    id triggered by a call to the unification between 2 [simple_type]s and
    must caught by it ([unify_simple_type]) to issue an explicit error message.
    Because we finally needed to export [unify_column_type], this exception
    can only escape outside this module if unification is triggered by a call
    to unification between 2 column types.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
exception Unification_column_conflict of
  (W_Algebra.column_type * W_Algebra.column_type)



(* ************************************************************************** *)
(** {b Descr}: Lowerize types level inside a [simple_type].
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let rec lowerize_level_in_simple_type max_level ty =
  assert (max_level < W_CoreTypes.generic_binding_level) ;
  (* First, get the canonical representation of the type. *)
  let ty = W_CoreTypes.simple_type_repr ty in
  match ty.W_Algebra.sty_mark with
  | W_Algebra.TM_lowerise_seen -> ()
  | W_Algebra.TM_not_seen -> (
      (* First, mark the type as as seen to prevent looping. *)
      ty.W_Algebra.sty_mark <- W_Algebra.TM_lowerise_seen ;
      match ty.W_Algebra.sty_desc with
      | W_Algebra.SType_var ty_var_desc -> (
          if ty_var_desc.W_Algebra.tv_level > max_level then (
            (* Types (variables) marked as generalized are those left marked
               under a forall-type, hence remains bound by a scheme, hence must
               not be lowerized. So if a variable has the generic binding level,
               we don't touch its level. *)
            if ty_var_desc.W_Algebra.tv_level <>
              W_CoreTypes.generic_binding_level then (
                (* Set the level inside the type. *)
                W_CoreTypes.change_ty_var_level ty_var_desc max_level
              )
          )
        )
      | W_Algebra.SType_arrow (args_tys, res_ty) ->
          List.iter (lowerize_level_in_simple_type max_level) args_tys ;
          lowerize_level_in_simple_type max_level res_ty
      | W_Algebra.SType_named named ->
          (match named.W_Algebra.nst_unwinded with
           | None -> ()
           | Some t' -> lowerize_level_in_simple_type max_level t') ;
          List.iter
            (lowerize_level_in_simple_type max_level) named.W_Algebra.nst_args
      | W_Algebra.SType_sum_of_records sumcases_column ->
          lowerize_level_in_column max_level sumcases_column
      | W_Algebra.SType_forall scheme ->
          lowerize_level_in_simple_type max_level scheme.W_Algebra.body
    )
  | _ (* Other markers. *) -> assert false



(* ************************************************************************** *)
(** {b Descr}: Lowerize variables level inside a row type.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and lowerize_level_in_row max_level row =
  assert (max_level < W_CoreTypes.generic_binding_level) ;
  (* First, get the canonical representation of the row type. *)
  let (row_fields, row_ending) =
    (W_CoreTypes.row_type_repr row).W_Algebra.rt_value in
  List.iter
    (fun (_, ty) -> lowerize_level_in_simple_type max_level ty) row_fields ;
  (match row_ending with
   | W_Algebra.Var_row row_variable ->
       (* Variables marked as generalized are those left marked under a
          forall-type, hence remains bound by a scheme, hence must not be
          lowerized. So if a variable has the generic binding level, we
          don't touch its level. *)
       if row_variable.W_Algebra.rv_level > max_level &&
         row_variable.W_Algebra.rv_level <>
         W_CoreTypes.generic_binding_level then
           W_CoreTypes.change_row_var_level row_variable max_level
   | W_Algebra.Closed_row -> ())



(* ************************************************************************** *)
(** {b Descr}: Lowerize variables level inside a column type.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and lowerize_level_in_column max_level column =
  assert (max_level <W_CoreTypes. generic_binding_level) ;
  (* First, get the canonical representation of the column type. *)
  let (col_rows, col_ending) =
    (W_CoreTypes.column_type_repr column).W_Algebra.ct_value in
  List.iter (lowerize_level_in_row max_level) col_rows ;
  (match col_ending with
   | W_Algebra.Var_column col_variable ->
       (* Variables marked as generalized are those left marked under a
          forall-type, hence remains bound by a scheme, hence must not be
          lowerized. So if a variable has the generic binding level, we
          don't touch its level. *)
       if col_variable.W_Algebra.cv_level > max_level  &&
         col_variable.W_Algebra.cv_level <>
         W_CoreTypes.generic_binding_level then
           W_CoreTypes.change_column_var_level col_variable max_level
     | W_Algebra.Closed_column -> ())



(* ************************************************************************** *)
(** {b Descr}: Computes the min binding level of 2 row endings. This is used
    when creating a new row variable for unification, in order to give it
    directly a binding level being the smallest one of the 2 endings of the
    rows we want to unify.
    If none of the endings have a variable, then the returned minimal level is
    the current binding level, i.e. the one with which anyway the fresh variable
    created before calling this function was initialized.
    {b ATTENTION}: This function assumes it is called on row endings that are
    canonical representation, i.e. obtained after a call to [row_type_repr].
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let min_level_of_row_endings end1 end2 =
  match (end1, end2) with
  | (W_Algebra.Closed_row, W_Algebra.Closed_row) ->
      (* None of the endings is a variable, hence there is no smaller level
         than the current one. *)
      !W_CoreTypes.current_binding_level
  | ((W_Algebra.Var_row rv), W_Algebra.Closed_row)
  | (W_Algebra.Closed_row, (W_Algebra.Var_row rv)) ->
      (* There is only one ending with a variable. Then the minimal level is
         the one of this only variable. *)
      rv.W_Algebra.rv_level
  | ((W_Algebra.Var_row rv1), (W_Algebra.Var_row rv2)) ->
      min rv1.W_Algebra.rv_level rv2.W_Algebra.rv_level



(* ************************************************************************** *)
(** {b Descr}: Computes the min binding level of 2 column endings. This
    function works like its counterpart for row, i.e.
    [min_level_of_row_endings].
    {b ATTENTION}: This function assumes it is called on column endings that are
    canonical representation, i.e. obtained after a call to [column_type_repr].
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let min_level_of_column_endings end1 end2 =
  match (end1, end2) with
  | (W_Algebra.Closed_column, W_Algebra.Closed_column) ->
      (* None of the endings is a variable, hence there is no smaller level
         than the current one. *)
      !W_CoreTypes.current_binding_level
  | ((W_Algebra.Var_column cv), W_Algebra.Closed_column)
  | (W_Algebra.Closed_column, (W_Algebra.Var_column cv)) ->
      (* There is only one ending with a variable. Then the minimal level is
         the one of this only variable. *)
      cv.W_Algebra.cv_level
  | ((W_Algebra.Var_column cv1), (W_Algebra.Var_column cv2)) ->
      min cv1.W_Algebra.cv_level cv2.W_Algebra.cv_level



(* ************************************************************************* *)
(* W_Algebra.simple_type list -> W_Algebra.simple_type list -> bool          *)
(** {b Descr}: Function checking if 2 type variables lists are in bijection.
    In other words, we have 2 lists of types [v_vars] and [w_vars], assumed
    all to be *type variables* and we want to ensure that all the variables
    from the first list are equal (i.e. were unified with) one variable of
    the second list and, in the other way, that all the variables of the
    second list are equal (were unified with) to one variable of the first
    list.
    This obviously means that both lists have the same length.
    This function is used to ensure that during unification of 2 types forall,
    the generalized variables of both schemes we mapped one-to-one, in other
    words that both schemes are as general.
    Using this mechanism of bijection allows to be independant on the order
    of variables in the quantifications of the schemes. In effect, we take an
    instance of each scheme and unify the bodies of these instances. During
    this unification, generalized type variables are mapped to their
    counterpart in the other type. At the end, we just need to verify that
    they were all mapped once to a counterpart variable.
    Note by the was that because this applies on qualtified variables, since
    unification allows to unify a generalized type variable only with a also
    generalized other type variable, we are sure that at the end of unification,
    then when we call the present function, all the variables manipulated will
    be generalized ones.
    {b Visibility}: Not exported outside this module.                        *)
(* ************************************************************************* *)
let check_type_variables_are_in_bijection v_vars w_vars =
  (* Local function testing if 2 types are physically equal variables. *)
  let test_vars_eq t1 t2 =
    match (t1.W_Algebra.sty_desc, t2.W_Algebra.sty_desc) with
    | ((W_Algebra.SType_var v1), (W_Algebra.SType_var v2)) ->
        v1 == v2
    | (_, _) -> false in
  (* Local function that takes one variable and tries to remove it from the
     list of other variables. *)
  let check_one_v v ws =
    let v = W_CoreTypes.simple_type_repr v in
    (* If the variable can't be removed because it is not in the list, then
       [Not_found] will be raised. *)
    List.remove_first_or_fail_eq ~eq: test_vars_eq v ws in
  (* Now, iterate the above local function on all the variables "v". At each
     iteration, we get a reduced list of variables "w" in which to find the
     next variable "v". At the end, we expect the remaining list of variables
     "w" to be empty, i.e. meaning that all the variables "v" were found
     equal to one unique variable "w" and reciprocally. *)
  try
    let remaining_ws =
      List.fold_left
        (fun accu_ws v -> check_one_v v accu_ws)
        w_vars v_vars in
    assert (remaining_ws = []) ;
    true
  with Not_found -> false



(* ************************************************************************* *)
(* W_Algebra.row_variable list -> W_Algebra.row_variable list -> bool        *)
(** {b Descr}: Performs the same job than
    [check_type_variables_are_in_bijection] above, but on row variables.
    See header of [check_type_variables_are_in_bijection] for more details.
    {b Visibility}: Not exported outside this module.                        *)
(* ************************************************************************* *)
let check_row_variables_are_in_bijection v_vars w_vars =
  let check_one_v v ws =
    (* We simulate a call to "repr" on a row variable. To do this, we manually
       inspect the [W_Algebra.rv_value] fields. If it is [W_Algebra.Row_unknown]
       then the variable was not instantiated and we take it as it is.
       If it is [W_Algebra.Row_known, then we call [W_CoreTypes.row_type_repr]
       to get the canonical representation of the variable's value then we
       check that we got a row type with not fields and if so, we take its
       ending variable. *)
    let v =
      match v.W_Algebra.rv_value with
      | W_Algebra.Row_unknown -> v
      | W_Algebra.Row_known row_type -> (
          match (W_CoreTypes.row_type_repr row_type).W_Algebra.rt_value with
          | ([], W_Algebra.Var_row v') -> v'
          | _ ->
              (* Not bijective because instantiated by something else than a
                 variable. *)
              raise Not_found
        ) in
    (* If the variable can't be removed because it is not in the list, then
       [Not_found] will be raised. *)
    List.remove_first_or_fail_eq ~eq: (==) v ws in
  (* Same explanation than in [check_type_variables_are_in_bijection] above. *)
  try
    let remaining_ws =
      List.fold_left
        (fun accu_ws v -> check_one_v v accu_ws)
        w_vars v_vars in
    assert (remaining_ws = []) ;
    true
  with Not_found -> false



(* ************************************************************************* *)
(* W_Algebra.column_variable list -> W_Algebra.column_variable list -> bool  *)
(** {b Descr}: Performs the same job than
    [check_type_variables_are_in_bijection] above, but on column variables.
    See header of [check_type_variables_are_in_bijection] for more details.
    {b Visibility}: Not exported outside this module.                        *)
(* ************************************************************************* *)
let check_column_variables_are_in_bijection v_vars w_vars =
  let check_one_v v ws =
    (* Same explanation than in [check_row_variables_are_in_bijection] above. *)
    let v =
      match v.W_Algebra.cv_value with
      | W_Algebra.Col_unknown -> v
      | W_Algebra.Col_known col_type -> (
          match (W_CoreTypes.column_type_repr col_type).W_Algebra.ct_value with
          | ([], W_Algebra.Var_column v') -> v'
          | _ -> raise Not_found
        ) in
    List.remove_first_or_fail_eq ~eq: (==) v ws in
  (* Same explanation than in [check_type_variables_are_in_bijection] above. *)
  try
    let remaining_ws =
      List.fold_left
        (fun accu_ws v -> check_one_v v accu_ws)
        w_vars v_vars in
    assert (remaining_ws = []) ;
    true
  with Not_found -> false



(* ************************************************************************** *)
(** {b Descr}: Internal function performing unification of 2 simple types.
    In any case (error or success), this function keeps the trace of the
    modifications done by the unification. This will be the job of the initial
    caller ([unify_simple_type], [unify_row_type] or [unify_column_type]) to
    deal with what to do of this trace.
    @raise Unification_simple_type_conflict
    @raise Unification_binding_level_conflict
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let rec __unify_simple_type env seen_expansions ty1 ty2 =
  #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
  OManager.printf "__unify_simple_type: %a VERSUS %a@."
    W_PrintTypes.pp_simple_type_start_sequence ty1
    W_PrintTypes.pp_simple_type_end_sequence ty2 ;
  #<End> ;    (* <---------- END DEBUG *)
  (* Special case optimization just in case the 2 types are already the same. *)
  if ty1 == ty2 then () else
  (* First, get the canonical representation of the 2 types to unify. *)
  let ty1 = W_CoreTypes.simple_type_repr ty1 in
  let ty2 = W_CoreTypes.simple_type_repr ty2 in
  if ty1 == ty2 then ()
  else
    match (ty1.W_Algebra.sty_desc, ty2.W_Algebra.sty_desc) with
    | ((W_Algebra.SType_var var1), (W_Algebra.SType_var var2)) -> (
        (* Unification between variables must prevent unification of a
           generalized variable with something else than another generalized
           variable. Otherwise, this would mean that a generalized variable
           gets no more generalized, hence one of the 2 unified types would
           lose its generality. *)
        if (var1.W_Algebra.tv_level <> W_CoreTypes.generic_binding_level &&
            var2.W_Algebra.tv_level <> W_CoreTypes.generic_binding_level)
           ||
           (var1.W_Algebra.tv_level = W_CoreTypes.generic_binding_level &&
            var2.W_Algebra.tv_level = W_CoreTypes.generic_binding_level) then (
          (* To try to preserve as far as we can the names given by the user to
             variables, if we are unifying 2 variables with on having no name
             and the other having a name, then we make the no-name equal to the
             with-name. Note that in case both variables have a name, this
             heuristic won't work and there is no principal better choice.
             I don't know why, I thought that keeping the most recent name was
             better but in fact, it seems that keeping the oldest seems to
             give the best results. This is really hackish. *)
          let v1_name = QmlTypeVars.TypeVar.name var1.W_Algebra.tv_qml in
          let v2_name = QmlTypeVars.TypeVar.name var2.W_Algebra.tv_qml in
          (* A bit casual: since there are only 2 ways to assign variables, we
             encode the assignment way by a boolean. [true] means that [var1] is
             assigned the identity of [var2]. *)
          let way =
            (match (v1_name, v2_name) with
              | ("", "") -> true   (* Arbitrary: v1 <- v2. *)
              | ("", _) -> true    (* v1 <- v2. *)
              | (_, "") -> false   (* v2 <- v1. *)
              | (_, _) ->
                 (* The most recent variable <- the oldest variable. We use
                    stamps for this. *)
                 (QmlTypeVars.TypeVar.to_int var1.W_Algebra.tv_qml) >
                 (QmlTypeVars.TypeVar.to_int var2.W_Algebra.tv_qml)) in
          if way then (
             (* [var1] has no name or both have names and [var1] is the most
                recent variable. Hence it will be assigned the identity of
                [var2]. *)
             (* Behave like the general case var U ty. Since we are in a case
                of 2 variables. We must not forget to adjust the level of the
                first variable. *)
             if var2.W_Algebra.tv_level > var1.W_Algebra.tv_level then
               W_CoreTypes.change_ty_var_level var2 var1.W_Algebra.tv_level ;
             W_CoreTypes.change_ty_link ~receiver: ty1 ~link_val: (Some ty2)
             )
         else (
            (* [var1] has a name or both have names and [var2] is the most
                recent variable. So [var2] will be assigned the identity of
               [var1] and the process is symetrical than above. *)
            if var1.W_Algebra.tv_level > var2.W_Algebra.tv_level then
               W_CoreTypes.change_ty_var_level var1 var2.W_Algebra.tv_level ;
             W_CoreTypes.change_ty_link ~receiver: ty2 ~link_val: (Some ty1)
            )
          )
        else (
          (* This is a failure case where one of the variable is generalized
             and not the other. Hence accepting the unification would lead
             into one of the type (the more general) to lose its generality
             to become equal to the other. And in this case, unification
             would not make the 2 types equal. *)
          raise
            (Unification_simple_type_conflict
               (ty1, ty2,
                { ucd_kind = DK_binding_level_mismatch ;
                  ucd_through_field = None })) ;
        )
      )
    | ((W_Algebra.SType_var var1), _) -> (
        (* Prevents creating cycles through unification of ['a] and ['a t] when
          ['a t] is defined as [type 'a t = 'a u = 'a v ... = 'a]. *)
        let are_same =
          W_TypeAbbrevs.deep_exact_occur_expand_abbrev
            env seen_expansions ~ty_var: ty1 ~in_ty: ty2 in
        if are_same then ()
        else (
          (* If the variable is generalized, then we must not unify since this
             would means a loss of generality. In effect, generalized variable
             are not instantiable ! *)
          if var1.W_Algebra.tv_level = W_CoreTypes.generic_binding_level then
            raise
              (Unification_simple_type_conflict
                 (ty1, ty2,
                  { ucd_kind = DK_binding_level_mismatch ;
                    ucd_through_field = None })) ;
          lowerize_level_in_simple_type var1.W_Algebra.tv_level ty2 ;
          (* Don't forget to cleanup the lowerized type. *)
          W_CoreTypes.cleanup_simple_type ty2 ;
          W_CoreTypes.change_ty_link ~receiver: ty1 ~link_val: (Some ty2)
         )
       )
    | (_, (W_Algebra.SType_var var2)) -> (
        (* Case symmetric than the previous one. *)
        let are_same =
          W_TypeAbbrevs.deep_exact_occur_expand_abbrev
            env seen_expansions ~ty_var: ty2 ~in_ty: ty1 in
        if are_same then ()
        else (
          if var2.W_Algebra.tv_level = W_CoreTypes.generic_binding_level then
            raise
              (Unification_simple_type_conflict
                 (ty1, ty2,
                  { ucd_kind = DK_binding_level_mismatch ;
                    ucd_through_field = None })) ;
          lowerize_level_in_simple_type var2.W_Algebra.tv_level ty1 ;
          (* Don't forget to cleanup the lowerized type. *)
          W_CoreTypes.cleanup_simple_type ty1 ;
          W_CoreTypes.change_ty_link ~receiver: ty2 ~link_val: (Some ty1)
         )
      )
    | (W_Algebra.SType_arrow (args_tys1, res_ty1),
       W_Algebra.SType_arrow (args_tys2, res_ty2)) -> (
        (* Ensure we are not in the case of an error of arity between the
           two function types. Note that this also prevent the [List.iter2]
           exception from being raised in case of arity mismatch.
           Making the test before unifying allows to issue a smarter error
           message about arity before a possible type mismatch is detected and
           issued first when unifying 2-by-2 the non-matching expected and
           provided types. *)
        let n1 = List.length args_tys1 in
        let n2 = List.length args_tys2 in
        if n1 <> n2 then
          raise
            (Unification_simple_type_conflict
               (ty1, ty2,
                { ucd_kind = DK_fun_type_arity (n1, n2) ;
                  ucd_through_field = None })) ;
        W_CoreTypes.change_ty_link ~receiver: ty1 ~link_val: (Some ty2) ;
        (* Unify types of arguments 2 by 2. *)
        List.iter2
          (__unify_simple_type env seen_expansions) args_tys1 args_tys2 ;
        (* Unify the types of results. *)
        __unify_simple_type env seen_expansions res_ty1 res_ty2
       )
    | (W_Algebra.SType_named { W_Algebra.nst_name = name ;
                               W_Algebra.nst_args = args ;
                               W_Algebra.nst_unwinded = _manifest },
       W_Algebra.SType_named { W_Algebra.nst_name = name' ;
                               W_Algebra.nst_args = args' ;
                               W_Algebra.nst_unwinded = _manifest' })
        when QmlAst.TypeIdent.equal name name' -> (
          try
            W_CoreTypes.change_ty_link ~receiver: ty1 ~link_val: (Some ty2) ;
            List.iter2 (__unify_simple_type env seen_expansions) args args' ;
            (* Two instances of a same named type, as far as they have unifiable
               arguments must have unifiable manifest representations. No need
               to descend inside in order to save efficiency unless we want
               to test assertions.
               Moreover, with incremental abbreviations unwinding, this may not
               hold anymore... *)
            (*
            (match (manifest, manifest') with
             | (None, None) -> ()
             | ((Some m), (Some m')) ->
                 __unify_simple_type env m m'
             | (_, _) ->
                 (* Two instances of a same named type should have either no
                    manifest representation or both one. *)
                 assert false) ;
            *)
          with Invalid_argument "List.iter2" ->
            (* In fact, we are in the case of an error of arity between the
               two type constructors. *)
            let n1 = List.length args in
            let n2 = List.length args' in
            raise
              (Unification_simple_type_conflict
                 (ty1, ty2,
                  { ucd_kind = DK_named_type_arity (name, n1, n2) ;
                    ucd_through_field = None }))
        )
    | (W_Algebra.SType_named nty1,
       W_Algebra.SType_named nty2)
        (* name <> name' *) -> (
          __unify_different_named_types env seen_expansions ty1 ty2 nty1 nty2
      )
    | ((W_Algebra.SType_named { W_Algebra.nst_unwinded = Some manifest }), _) ->
          (* Same remark about trivially cyclic types than in the previous
             match case. *)
        #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
        OManager.printf "Case SType_named manifest / other@." ;
        #<End> ;                (* <---------- END DEBUG *)
        if ty1 == (W_CoreTypes.simple_type_repr manifest) then
          raise
            (Unification_simple_type_conflict
               (ty1, ty2,
                { ucd_kind = DK_none ;
                  ucd_through_field = None })) ;
        __unify_simple_type env seen_expansions ty2 manifest
    | (_, (W_Algebra.SType_named { W_Algebra.nst_unwinded = Some manifest })) ->
        #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
        OManager.printf "Case other / SType_named manifest@." ;
        #<End> ;                (* <---------- END DEBUG *)
        (* Same remark about trivially cyclic types than in the previous
           match case. *)
        if ty2 == (W_CoreTypes.simple_type_repr manifest) then
          raise
            (Unification_simple_type_conflict
               (ty1, ty2, { ucd_kind = DK_none ; ucd_through_field = None })) ;
        __unify_simple_type env seen_expansions ty1 manifest
    | ((W_Algebra.SType_named { W_Algebra.nst_unwinded = None }), _) -> (
        #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
        OManager.printf "Case SType_named non-manifest / other@." ;
        #<End> ;                (* <---------- END DEBUG *)
        let (ty1', seen_expansions') =
          W_TypeAbbrevs.incrementally_expand_abbrev env seen_expansions ty1 in
        #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
        OManager.printf "SType_named non-manifest expanded: %a@."
          W_PrintTypes.pp_simple_type ty1' ;
        #<End> ;                (* <---------- END DEBUG *)
        if ty1 == ty1' then
          raise
            (Unification_simple_type_conflict
               (ty1, ty2, { ucd_kind = DK_none ; ucd_through_field = None })) ;
        __unify_simple_type env seen_expansions' ty2 ty1'
       )
    | (_, (W_Algebra.SType_named { W_Algebra.nst_unwinded = None })) -> (
        #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
        OManager.printf "Case other / SType_named non-manifest@." ;
        #<End> ;                (* <---------- END DEBUG *)
        let (ty2', seen_expansions') =
          W_TypeAbbrevs.incrementally_expand_abbrev env seen_expansions ty2 in
        if ty2 == ty2' then
          raise
            (Unification_simple_type_conflict
               (ty1, ty2, { ucd_kind = DK_none ; ucd_through_field = None })) ;
          __unify_simple_type env seen_expansions' ty1 ty2'
       )
    | ((W_Algebra.SType_sum_of_records col1),
       (W_Algebra.SType_sum_of_records col2)) -> (
        try
          W_CoreTypes.change_ty_link ~receiver: ty1 ~link_val: (Some ty2) ;
          __unify_column_type env seen_expansions col1 col2
        with Unification_column_conflict (_, _) ->
          (* At this point, recover the unification on columns failure.
             [TODO] There should be a clear error message here, but for the
             moment it's rather crude. ^^ *)
          raise
            (Unification_simple_type_conflict
               (ty1, ty2, { ucd_kind = DK_none ; ucd_through_field = None }))
      )
    | ((W_Algebra.SType_forall scheme1), (W_Algebra.SType_forall scheme2)) -> (
        W_CoreTypes.change_ty_link ~receiver: ty1 ~link_val: (Some ty2) ;
        (*
          Naive unification, just instantiating both schemes then unifying
          the obtained bodies is wrong !
          For example, on the following program we would get:
            if true then {{ id = fun x -> x }} else {{ id = fun x -> 5 }} ;;
            - : { id: for all 'Ta . ('Ta -> 'Ta); . } / 'Ca
          which is clearly wrong since in the second branch of the
          conditional, the type of the function id is not as general as
          'a -> 'a !
        *)
        (* We must take an instance of each scheme body, but make so that
           generalized variables keep a generic level. This means that we
           must create a copy of the bodies, with fresh variables in place
           of the generalized ones, but these fresh variables being still
           at generic level. Making a copy prevents from dirtying our
           schemes. Forcing copies of generalized variables to be generic
           will allow unification not to instantiate them by something else
           than another generic variable. *)

        (* We count by side effect the number of variables in each scheme
           to save a call to List.length. A bit casual, but more efficient. *)
        let count_ty_vars1 = ref 0 in
        let count_ty_vars2 = ref 0 in
        (* Create the mappings of generalized variables of the scheme onto
           fresh *also generalized* type variables. *)
        let ty_vars_mapping1 =
          List.map
            (fun sch ->
               incr count_ty_vars1 ;
               (sch, (W_CoreTypes.__generic_type_variable ())))
            scheme1.W_Algebra.ty_parameters in
        let ty_vars_mapping2 =
          List.map
            (fun sch ->
               incr count_ty_vars2 ;
               (sch, (W_CoreTypes.__generic_type_variable ())))
            scheme2.W_Algebra.ty_parameters in
        (* Do the same thing for row variables... *)
        let count_row_vars1 = ref 0 in
        let count_row_vars2 = ref 0 in
        let row_vars_mapping1 =
          List.map
            (fun sch_var ->
               incr count_row_vars1 ;
               (sch_var, (W_CoreTypes.__generic_row_variable ())))
            scheme1.W_Algebra.row_parameters in
        let row_vars_mapping2 =
          List.map
            (fun sch_var ->
               incr count_row_vars2 ;
               (sch_var, (W_CoreTypes.__generic_row_variable ())))
            scheme2.W_Algebra.row_parameters in
        (* Do the same thing for column variables... *)
        let count_column_vars1 = ref 0 in
        let count_column_vars2 = ref 0 in
        let column_vars_mapping1 =
          List.map
            (fun sch_var ->
               incr count_column_vars1 ;
               (sch_var, (W_CoreTypes.__generic_column_variable ())))
            scheme1.W_Algebra.column_parameters in
        let column_vars_mapping2 =
          List.map
            (fun sch_var ->
               incr count_column_vars2 ;
               (sch_var, (W_CoreTypes.__generic_column_variable ())))
            scheme2.W_Algebra.column_parameters in
        (* Ensure that both schemes have the same number of variables. *)
        if !count_ty_vars1 <> ! count_ty_vars2 ||
           !count_row_vars1 <> !count_row_vars2 ||
           !count_column_vars1 <> !count_column_vars2 then (
          (* In fact, we are in the case where the 2 schemes do not have the
             same number of generalized variables. *)
          let n1 = !count_ty_vars1 + !count_row_vars1 + !count_column_vars1 in
          let n2 = !count_ty_vars2 + !count_row_vars2 + !count_column_vars2 in
          raise
            (Unification_simple_type_conflict
               (ty1, ty2,
                { ucd_kind = DK_forall_type_quantification_arity (n1, n2) ;
                  ucd_through_field = None }))

        ) ;
        (* Now, create the instances of the type schemes. *)
        let scheme1_instance =
          W_SchemeGenAndInst.specialize_with_given_variables_mapping
            ~deep: false
            ty_vars_mapping1 row_vars_mapping1 column_vars_mapping1 scheme1 in
        let scheme2_instance =
          W_SchemeGenAndInst.specialize_with_given_variables_mapping
            ~deep: false
            ty_vars_mapping2 row_vars_mapping2 column_vars_mapping2 scheme2 in
        (* And now, perform the unification. Because variables that were
           generalized in the schemes have been copied, unification will
           operate on these copies, hence preventing from dirtying the
           schemes original generalized variables. *)
        __unify_simple_type
          env seen_expansions scheme1_instance scheme2_instance ;
        (* Now, ensure that the qualtified variables of both schemes were
           mapped one-to-one, i.e. no collapse occurred, i.e. both scheme
           were as general. Compute the checks one at time to avoid useless
           computation in case one fails. This way, when unifying sums for
           which we can backtrack on type error, we avoid checking bijections
           if a previous bijection test failed. May be we gain a bit of time
           even if lists of variables are generally not very long. *)
        let fresh_gen_ty_vars1 = List.map snd ty_vars_mapping1 in
        let fresh_gen_ty_vars2 = List.map snd ty_vars_mapping2 in
        let bij1 =
          check_type_variables_are_in_bijection
            fresh_gen_ty_vars1 fresh_gen_ty_vars2 in
        if not bij1 then
          raise
            (Unification_simple_type_conflict
               (ty1, ty2, { ucd_kind = DK_none ; ucd_through_field = None })) ;
        let fresh_gen_row_vars1 = List.map snd row_vars_mapping1 in
        let fresh_gen_row_vars2 = List.map snd row_vars_mapping2 in
        let bij2 =
          check_row_variables_are_in_bijection
            fresh_gen_row_vars1 fresh_gen_row_vars2 in
        if not bij2 then
          raise
            (Unification_simple_type_conflict
               (ty1, ty2, { ucd_kind = DK_none ; ucd_through_field = None })) ;
        let fresh_gen_column_vars1 = List.map snd column_vars_mapping1 in
        let fresh_gen_column_vars2 = List.map snd column_vars_mapping2 in
        let bij3 =
          check_column_variables_are_in_bijection
            fresh_gen_column_vars1 fresh_gen_column_vars2 in
        if not bij3 then
          raise
            (Unification_simple_type_conflict
               (ty1, ty2, { ucd_kind = DK_none ; ucd_through_field = None }))
      )
    | (_, _) ->
        raise
          (Unification_simple_type_conflict
             (ty1, ty2, { ucd_kind = DK_none ; ucd_through_field = None }))



(* [TODO-REFACTOR] DOCUMENTATION. *)
and __unify_different_named_types env seen_expansions ty1 ty2 nty1 nty2 =
  #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
  OManager.printf "__unify_different_named_types@." ;
  #<End> ;                (* <---------- END DEBUG *)
  let h_nty1 = W_CoreTypes.named_type_expr_height nty1 in
  let h_nty2 = W_CoreTypes.named_type_expr_height nty2 in
  if (h_nty1 < 0) && (h_nty2 < 0) then (
    #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
    OManager.printf "Both negative: expand L & R@." ;
    #<End> ;                (* <---------- END DEBUG *)
    (* Expand both once then unify the 2 resulting types. *)
    let (ty1', seen_expansions') =
      W_TypeAbbrevs.incrementally_expand_abbrev env seen_expansions ty1 in
    let (ty2', seen_expansions'') =
      W_TypeAbbrevs.incrementally_expand_abbrev env seen_expansions' ty2 in
    if (ty1 == ty1') && (ty2 == ty2') then
      raise
        (Unification_simple_type_conflict
           (ty1, ty2, { ucd_kind = DK_none ; ucd_through_field = None })) ;
    __unify_simple_type env seen_expansions'' ty1' ty2'
  )
  else (  (* Else 0. *)
    (* Not ((h_nty1 < 0) && (h_nty2 < 0)). *)
    if h_nty1 < 0 then (
      #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
      OManager.printf "L negative: expand L@." ;
      #<End> ;                (* <---------- END DEBUG *)
      (* Expand left once then unify expanded left and right. *)
      let (ty1', seen_expansions') =
        W_TypeAbbrevs.incrementally_expand_abbrev env seen_expansions ty1 in
      #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
      OManager.printf "L after expand: %a@."
        W_PrintTypes.pp_simple_type ty1' ;
      #<End> ;                (* <---------- END DEBUG *)
      if ty1 == ty1' then
        raise
          (Unification_simple_type_conflict
             (ty1, ty2, { ucd_kind = DK_none ; ucd_through_field = None })) ;
      __unify_simple_type env seen_expansions' ty2 ty1'
    )
    else (  (* Else 1. *)
      (* (h_nty1 >= 0). *)
      if h_nty2 < 0 then (
        #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
        OManager.printf "R negative: expand R@." ;
        #<End> ;                (* <---------- END DEBUG *)
        (* Expand right once then unify left and expanded right. *)
        let (ty2', seen_expansions') =
          W_TypeAbbrevs.incrementally_expand_abbrev env seen_expansions ty2 in
        #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
        OManager.printf "R after expand: %a@."
          W_PrintTypes.pp_simple_type ty2' ;
        #<End> ;                (* <---------- END DEBUG *)
        if ty2 == ty2' then
          raise
            (Unification_simple_type_conflict
               (ty1, ty2, { ucd_kind = DK_none ; ucd_through_field = None })) ;
        __unify_simple_type env seen_expansions' ty1 ty2'
      )
      else (   (* Else 2. *)
        (* None of heights are negative. *)
        if h_nty1 = h_nty2  then (
          #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
          OManager.printf "Both positive and equal (%d): expand both@." h_nty1 ;
          #<End> ;                (* <---------- END DEBUG *)
          (* None of heights are negative and they are equal. *)
          let (ty1', seen_expansions') =
            W_TypeAbbrevs.incrementally_expand_abbrev env seen_expansions ty1 in
          let (ty2', seen_expansions'') =
            W_TypeAbbrevs.incrementally_expand_abbrev
              env seen_expansions' ty2 in
          #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
          OManager.printf "L after expand: %a@."
            W_PrintTypes.pp_simple_type ty1' ;
          OManager.printf "R after expand: %a@."
            W_PrintTypes.pp_simple_type ty2' ;
          #<End> ;                (* <---------- END DEBUG *)
          if (ty1 == ty1') && (ty2 == ty2') then
            raise
              (Unification_simple_type_conflict
                 (ty1, ty2,
                  { ucd_kind = DK_none ; ucd_through_field = None })) ;
          __unify_simple_type env seen_expansions'' ty1' ty2'
        )
        else (   (* Else 3. *)
          (* None of heights are negative and they are not equal.
             Expand highest to match lowest's level then unify left (possibly
             expanded) and right (possibly expanded).
             If both are at the same level: unwind them once. *)
          if h_nty1 < h_nty2 then (
            #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
            OManager.printf "L (%d) < R (%d): expand R@." h_nty1 h_nty2 ;
            #<End> ;                (* <---------- END DEBUG *)
            (* None of heights are negative and left is lower than right.
               Expand right. *)
            let nb_expansion = h_nty2 - h_nty1 in
            let (ty2', seen_expansions') =
              W_TypeAbbrevs.expand_abbrev_n_times
                nb_expansion env seen_expansions ty2 in
            #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
            OManager.printf "R after expand: %a@."
              W_PrintTypes.pp_simple_type ty2' ;
            #<End> ;                (* <---------- END DEBUG *)
            if ty2 == ty2' then
              raise
                (Unification_simple_type_conflict
                   (ty1, ty2,
                    { ucd_kind = DK_none ; ucd_through_field = None })) ;
            __unify_simple_type env seen_expansions' ty1 ty2'
          )
          else (   (* Else 4. *)
            #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
            OManager.printf "R (%d) < L (%d): expand L@." h_nty2 h_nty1 ;
            #<End> ;                (* <---------- END DEBUG *)
            (* None of heights are negative and left is greater than right.
               Expand left. *)
            let nb_expansion = h_nty1 - h_nty2 in
            let (ty1', seen_expansions') =
              W_TypeAbbrevs.expand_abbrev_n_times
                nb_expansion env seen_expansions ty1 in
            #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
            OManager.printf "L after expand: %a@."
              W_PrintTypes.pp_simple_type ty1' ;
            #<End> ;                (* <---------- END DEBUG *)
            if ty1 == ty1' then
              raise
                (Unification_simple_type_conflict
                   (ty1, ty2,
                    { ucd_kind = DK_none ; ucd_through_field = None })) ;
            __unify_simple_type env seen_expansions' ty2 ty1'
          )     (* End of else 4. *)
        )       (* End of else 3. *)
      )         (* End of else 2. *)
    )           (* End of else 1. *)
  )             (* End of else 0. *)



(* ************************************************************************** *)
(** {b Descr}: Internal routine performing the unification of 2 row types.
    Unification is performed "in place", i.e. by physically modifying the
    variables on which a substitution must be applied in order to make the 2
    types equal.
    Differently from other unification routines, this one returns a boolean
    telling if the unification was a success of if it failed due to endings
    compatibility.
    More in detail, there are 2 cases where unification of rows can fail.
    First one, and hardest one, there exist 2 fields in common in the 2 record
    types that have incompatible types. In such a case, the unification really
    and definitively fails.
    The second case is when common fields have compatible types but the 2 rows
    can't unify because they have incompatible endings. Intuitively, this
    happens when one is closed and the other is open and the list of fields of
    the records are not the same. Or, this happens when both are closed and the
    list of fields of the records are not the same also. In these cases,
    unification is not yet definitively a failure since column unification may
    take the precedence and, if it is a success, then will lead to a sum type
    instead of a record type, this sum type having cases being the union of the
    2 records to initially unify.
    In any case (error or success), this function keeps the trace of the
    modifications done by the unification. This will be the job of the initial
    caller ([unify_simple_type], [unify_row_type] or [unify_column_type]) to
    deal with what to do of this trace.
    @raise Unification_simple_type_conflict
    @raise Unification_binding_level_conflict
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and __unify_row_type env seen_expansions row_ty1 row_ty2 =
  (* First, get the canonical representation of the 2 row types to unify. *)
  let row_ty1 = W_CoreTypes.row_type_repr row_ty1 in
  let row_ty2 = W_CoreTypes.row_type_repr row_ty2 in
  if row_ty1 == row_ty2 then
    (* Unification is a full success since rows are physically the same. *)
    true
  else
    match (row_ty1.W_Algebra.rt_value, row_ty2.W_Algebra.rt_value) with
    | (([], W_Algebra.Closed_row), ([], W_Algebra.Closed_row)) ->
        (* This corresponds to the case:
             o U o
           This is also a full success. *)
        true
    | (([], (W_Algebra.Var_row rv1)),
       ([], (W_Algebra.Var_row rv2))) when rv1 == rv2 ->
        (* This corresponds to the case:
             same-rho-var U same-rho-var
           This is also a full success. *)
        true
    | (([], (W_Algebra.Var_row rv1)), (_, _)) ->
        (* This corresponds to the case:
           rho-var U any-row
           This is also a full success. *)
        lowerize_level_in_row rv1.W_Algebra.rv_level row_ty2 ;
        (* Don't forget to cleanup the lowerized type. *)
        W_CoreTypes.cleanup_row_type row_ty2 ;
        (* In this case, [rv1] must be set equal to the other row. *)
        W_CoreTypes.change_row_var_link
          ~receiver: rv1 ~link_val: (W_Algebra.Row_known row_ty2) ;
        true
    | ((_, _), ([], (W_Algebra.Var_row rv2))) ->
        (* This corresponds to the case:
           any-row U rho-var
           This is also a full success. *)
        lowerize_level_in_row rv2.W_Algebra.rv_level row_ty1 ;
        (* Don't forget to cleanup the lowerized type. *)
        W_CoreTypes.cleanup_row_type row_ty1 ;
        (* In this case, [rv2] must be set equal to the other row. *)
        W_CoreTypes.change_row_var_link
          ~receiver: rv2 ~link_val: (W_Algebra.Row_known row_ty1) ;
        true
    | (([], W_Algebra.Closed_row), (_, _))
    | ((_, _), ([], W_Algebra.Closed_row)) ->
        (* Error trying to unify a closed empty row with something that contains
           fields. The unification is not yet definitively a failure, however
           it is not a success. May be trying to unify columns will lead to
           a correct sum type. We return [false] to tell that the unification
           failed, and this will be checked by [__unify_column_type] to try
           unifying with its next case. To, beware this is **not** a real
           failure ! *)
        false
    | ((row1_fields, row1_ending), (row2_fields, row2_ending)) ->
        (* This corresponds to the general case:
           any-row-with-fields U any-row-with-fields *)
        let field0 = List.hd row1_fields in
        let phi0 = (List.tl row1_fields, row1_ending) in
        let field1 = List.hd row2_fields in
        let phi1 = (List.tl row2_fields, row2_ending) in
        if __unify_field env seen_expansions field0 field1 then
          (* If head field labels are equal, then we must unify their types by
             side effect and go on with the remaining fields. *)
          __unify_row_type
            env seen_expansions
            { W_Algebra.rt_value = phi0 } { W_Algebra.rt_value = phi1 }
        else
          (
            let fresh_rv = W_CoreTypes.__row_variable () in
            (* Create the fresh variable directly with a level being the minimum
               of the 2 endings if they were variables. Otherwise, with the
               current binding level. *)
            W_CoreTypes.change_row_var_level
              fresh_rv (min_level_of_row_endings row1_ending row2_ending) ;
            (* Now, unify in cross. Do not unify lazily because we need to
               anyway ensure that the common fields have compatible types
               even if one of the sub-unification tells us it is not a
               success. *)
            let success1 =
              __unify_row_type
                env seen_expansions
                { W_Algebra.rt_value = phi0 }
                { W_Algebra.rt_value =
                    ([field1], (W_Algebra.Var_row fresh_rv)) } in
            let success2 =
              __unify_row_type
                env seen_expansions
                { W_Algebra.rt_value = phi1 }
                { W_Algebra.rt_value =
                    ([field0], (W_Algebra.Var_row fresh_rv)) } in
            success1 && success2
          )



(* ************************************************************************** *)
(** {b Descr}: Unification of 2 column types. Since row and columns are really
    symmetrical, this unification function works exactly like its counterpart
    for row types.
    In any case (error or success), this function keeps the trace of the
    modifications done by the unification. This will be the job of the initial
    caller ([unify_simple_type], [unify_row_type] or [unify_column_type]) to
    deal with what to do of this trace.
    @raise Unification_simple_type_conflict
    @raise Unification_binding_level_conflict
    @raise Unification_column_conflict
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and __unify_column_type env seen_expansions col_ty1 col_ty2 =
  (* First, get the canonical representation of the 2 column types to unify. *)
  let col_ty1 = W_CoreTypes.column_type_repr col_ty1 in
  let col_ty2 = W_CoreTypes.column_type_repr col_ty2 in
  if col_ty1 == col_ty2 then ()
  else
    match (col_ty1.W_Algebra.ct_value, col_ty2.W_Algebra.ct_value) with
    | (([], W_Algebra.Closed_column), ([], W_Algebra.Closed_column)) ->
        (* This corresponds to the case:
             o U o *)
        ()
    | (([], (W_Algebra.Var_column cv1)), ([], (W_Algebra.Var_column cv2)))
          when cv1 == cv2 ->
        (* This corresponds to the case:
             same-column-var U same-column-var *)
        ()
    | (([], (W_Algebra.Var_column cv1)), (_, _)) ->
        (* This corresponds to the case:
           column-var U any-column *)
        lowerize_level_in_column cv1.W_Algebra.cv_level col_ty2 ;
        (* Don't forget to cleanup the lowerized type. *)
        W_CoreTypes.cleanup_column_type col_ty2 ;
        (* In this case, cv1 must be set equal to the other column. *)
        W_CoreTypes.change_column_var_link
          ~receiver: cv1 ~link_val: (W_Algebra.Col_known col_ty2)
    | ((_, _), ([], (W_Algebra.Var_column cv2))) ->
        (* This corresponds to the case:
           any-column U column-var *)
        lowerize_level_in_column cv2.W_Algebra.cv_level col_ty1 ;
        (* Don't forget to cleanup the lowerized type. *)
        W_CoreTypes.cleanup_column_type col_ty1 ;
        (* In this case, cv2 must be set equal to the other column. *)
        W_CoreTypes.change_column_var_link
          ~receiver: cv2 ~link_val: (W_Algebra.Col_known col_ty1)
    | (([], W_Algebra.Closed_column), (_, _))
    | ((_, _), ([], W_Algebra.Closed_column)) ->
        (* Error trying to unify a closure with something that contains rows
           (sum cases). *)
        raise (Unification_column_conflict (col_ty1, col_ty2)) ;
    | ((col1_row, col1_ending), (col2_row, col2_ending)) ->
        (* This corresponds to the general case:
           any-column-with-row U any-column-with-row *)
        let record0 = List.hd col1_row in
        let phi0 = (List.tl col1_row, col1_ending) in
        let record1 = List.hd col2_row in
        let phi1 = (List.tl col2_row, col2_ending) in
        (* Record the unification changes state to revert back to this point
           in case the unification on the row would fail. *)
        let checkpoint = W_CoreTypes.get_current_changes_checkpoint () in
        if __unify_row_type env seen_expansions record0 record1 then (
          (* If head records are unifiable, then we must go on with the
             remaining records. *)
          __unify_column_type
            env seen_expansions
            { W_Algebra.ct_value = phi0 } { W_Algebra.ct_value = phi1 }
        )
        else (
          (* Revert the unification changes back to the point before trying
             to unify the 2 rows. We do not catch hard type errors reported
             via [Unification_simple_type_conflict] since these ones
             represent real type error. Only failure on row (endings) are
             legal to consider as allowing to unify with another row along
             the column. *)
          W_CoreTypes.rewind_unification_changes ~performed_after: checkpoint ;
          (* Head records can't be unified because one or both are closed
             and this closure prevent row "appending". However, their
             common fields are type-compatible. *)
          let fresh_cv = W_CoreTypes.__column_variable () in
          (* Create the fresh variable directly with a level being the minimum
             of the 2 endings if they were variables. Otherwise, with the
             current binding level. *)
          W_CoreTypes.change_column_var_level
            fresh_cv (min_level_of_column_endings col1_ending col2_ending) ;
          (* Now, unify in cross. *)
          __unify_column_type
            env seen_expansions
            { W_Algebra.ct_value = phi0 }
            { W_Algebra.ct_value =
                ([record1], (W_Algebra.Var_column fresh_cv)) } ;
          __unify_column_type
            env seen_expansions
            { W_Algebra.ct_value = phi1 }
            { W_Algebra.ct_value =
                ([record0], (W_Algebra.Var_column fresh_cv)) }
        )



(* ************************************************************************** *)
(** {b Descr}: Unification of 2 record fields. Fields are unifiable if they
    have the same name and if their types are unifiable. This function performs
    unification in place if possible and returns a boolean telling if the
    unification succeeded.
    There are 2 cases where unification can fail: either the fields names are
    different and then the function stops, returning false, or the fields names
    are the same but their types are not unifiable and in this case, an error
    is directly raised (no need to even return any boolean).
    @raise Unification_simple_type_conflict
    @raise Unification_binding_level_conflict
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and __unify_field env seen_expansions  (field_name1, field_ty1) (field_name2, field_ty2) =
  if field_name1 <> field_name2 then false
  else (
    try
      __unify_simple_type env seen_expansions field_ty1 field_ty2 ;
      (* If the above unification succeeded, then we arrive here then return a
         boolean telling that everything succeeded. Otherwise, the above
         unification will have raised an exception, and then we never arrive
         here. *)
      true
    with
    | Unification_simple_type_conflict
        (t1, t2, { ucd_kind = k ; ucd_through_field = None }) ->
        (* Embedd the name of the fields in each type that caused the error.
           Since both fields have the same name, we expected them to be
           unifiable. Reminding the name the these fields my give better error
           messages. We only embedd the name if none was already recorded, this
           way, we keep the deepest guilty field of the type structure. *)
        raise
          (Unification_simple_type_conflict
             (t1, t2, { ucd_kind = k ; ucd_through_field = Some field_name1 }))
  )



(* ************************************************************************** *)
(** {b Descr}: Unification of 2 simple types. The unification is performed by
    side effect, physically modifying type variables to make them equal to what
    they must be instantiated by the MGU substitution.
    If unification is a success, this function forgets the trace of the
    modifications done by the unification, otherwise it reverts them, hence
    restoring the types as they were before starting to unify them.
    @raise Unification_simple_type_conflict
    @raise Unification_binding_level_conflict
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let unify_simple_type env ty1 ty2 =
  #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
  OManager.printf "unify_simple_type@." ;
  #<End> ;     (* <---------- END DEBUG *)
  let checkpoint = W_CoreTypes.get_current_changes_checkpoint () in
  try
    __unify_simple_type env W_TypeAbbrevs.empty_memory ty1 ty2 ;
    #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
    OManager.printf "Ended unify_simple_type@." ;
    #<End> ;    (* <---------- END DEBUG *)
    W_CoreTypes.reset_unification_changes_trace ()
  with any ->
    W_CoreTypes.rewind_unification_changes ~performed_after: checkpoint ;
    #<If:TYPER $minlevel 11> (* <---------- DEBUG *)
    OManager.printf "Ended unify_simple_type@." ;
    #<End> ;    (* <---------- END DEBUG *)
    raise any



(* ************************************************************************** *)
(** {b Descr}: Unification of 2 row types. Unification is performed "in place",
    i.e. by physically modifying the variables on which a substitution must be
    applied in order to make the 2 types equal.
    If unification is a success, this function forgets the trace of the
    modifications done by the unification, otherwise it reverts them, hence
    restoring the types as they were before starting to unify them.
    @raise Unification_simple_type_conflict
    @raise Unification_binding_level_conflict
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let unify_row_type env row_ty1 row_ty2 =
  let checkpoint = W_CoreTypes.get_current_changes_checkpoint () in
  try
    if __unify_row_type env W_TypeAbbrevs.empty_memory row_ty1 row_ty2 then (
      (* Forget the unification trace since unification succeeded. No need to
         remind them since we won't revert this unification. *)
      W_CoreTypes.reset_unification_changes_trace () ;
      true
    )
    else (
      (* In fact, unification failed, so revert the changes it did. *)
      W_CoreTypes.rewind_unification_changes ~performed_after: checkpoint ;
      false
    )
  with any ->
    (* Other case of unification that failed. Do the same thing than above,
       i.e. revert the changes it did. *)
    W_CoreTypes.rewind_unification_changes ~performed_after: checkpoint ;
    raise any



(* ************************************************************************** *)
(** {b Descr}: Unification of 2 column types. If unification is a success, this
    function forgets the trace of the modifications done by the unification,
    otherwise it reverts them, hence restoring the types as they were before
    starting to unify them.
    @raise Unification_simple_type_conflict
    @raise Unification_binding_level_conflict
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let unify_column_type env column_ty1 column_ty2 =
  let checkpoint = W_CoreTypes.get_current_changes_checkpoint () in
  try
    __unify_column_type env W_TypeAbbrevs.empty_memory column_ty1 column_ty2 ;
    (* Forget the unification trace since unification succeeded. No need to
       remind them since we won't revert this unification. *)
    W_CoreTypes.reset_unification_changes_trace ()
  with any ->
    (* Unification failed, so revert the changes it did. *)
    W_CoreTypes.rewind_unification_changes ~performed_after: checkpoint ;
    raise any



(* [TODO-REFACTOR] DOCUMENTATION. *)
let _ = W_TypingEnv.forward_ref__unify_simple_type :=
  (fun env t1 t2 ->
     __unify_simple_type env W_TypeAbbrevs.empty_memory t1 t2)
let _ = W_TypingEnv.forward_ref__unify_row_type :=
  (fun env row_ty1 row_ty2 ->
     __unify_row_type env W_TypeAbbrevs.empty_memory row_ty1 row_ty2)
let _ = W_TypingEnv.forward_ref__unify_column_type :=
  (fun env column_ty1 column_ty2 ->
     __unify_column_type env W_TypeAbbrevs.empty_memory column_ty1 column_ty2)
