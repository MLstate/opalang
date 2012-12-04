(*
    Copyright © 2011, 2012 MLstate

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
(** {b Descr}: Exception raised during generalization if a private type appears
    to have its opaque name not set. See comment in [find_parameters_in_ty] for
    the reasons of raising an error in this case.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
exception Private_type_not_opaque of W_Algebra.simple_type



(* ************************************************************************** *)
(** {b Descr}: See .mli file for documentation.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
exception Non_generalizable_type_var of
  (W_Algebra.simple_type * (** The global type hosting a non-generalizable type
       variable. *)
   W_Algebra.simple_type)  (** The non-generalizable type variable. *)



(* ************************************************************************** *)
(** {b Descr}: See .mli file for documentation.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
exception Non_generalizable_row_var of
  (W_Algebra.simple_type * (** The global type hosting a non-generalizable row
       variable. *)
   W_Algebra.simple_type)  (** The non-generalizable row variable embedded
       in a type. *)



(* ************************************************************************** *)
(** {b Descr}: See .mli file for documentation.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
exception Non_generalizable_column_var of
  (W_Algebra.simple_type * (** The global type hosting a non-generalizable
       column variable. *)
   W_Algebra.simple_type)  (** The non-generalizable column variable embedded
       in a type. *)



(* ************************************************************************** *)
(** {b Descr}:
    {b Args}:
      - [~forbid_non_gen_vars]: raise an error if a variable is found
        non-generalizable. ATTENTION: only set to [true] in case of
        generalization of a toplevel let-def (not even on modules definitions).
        We can't make this test when exporting to QML because wrapping of
        toplevel expressions into tuples causes the instantiation of let-defined
        idents, hence causes taking an instance of their type schemes which are
        taken at the toplevel binding level, causing variables to have this
        binding level (i.e. 0) instead of the
        [W_CoreTypes.generic_binding_level]
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let __generalize ~forbid_non_gen_vars ~extra_variables initial_ty =
  let found_ty_parameters = ref ([] : W_Algebra.simple_type list) in
  let found_row_parameters = ref ([] : W_Algebra.row_variable list) in
  let found_column_parameters = ref ([] : W_Algebra.column_variable list) in


  (* ************************************************************************ *)
  (** {Descr}: Local function that walks along a simple type, harvesting its
      generalized type variables.
      We need to have an explicit list of the scheme's parameters. So we need
      not only to mark variables as generalized, we also need to record them in
      lists. Because schemes can share some variables (e.g. in the case of
      mutually recursive functions), we can't say that when we encounter a
      generalized variable then if it already marked as generalized then that's
      because it was already seen and hence may be already in the list of the
      scheme's parameter. That can also be because:
       1) This variable is shared with another function whose type was already
          generalized. In this case the variable will not yet be in effect in
          the list of the scheme's parameters we are bulding.
       2) This variable is the one of a type forall present in the type we are
          generalizing. In this case, this variable *must* not be inserted in
          the list the the parameters of the scheme we are bulding since it
          already belongs to the type forall's scheme.
      So, to know when to add a variable whose level is generic in the effective
      list of the scheme's parameter, this function uses 2 kinds of list:
       - [found_ty_parameters], [found_row_parameters] and
         [found_column_parameters] to store the scheme's effective future
         parameters. A variable is inserted if it really has to be.
       - [ignored_tys], [ignored_row_vars] and [ignored_col_vars] to remind
         the variables (type, row and column) that must not be considered as
         parameters of the scheme even if there level is generic.             *)
  (* ************************************************************************ *)
  let rec find_parameters_in_ty
      ignored_tys ignored_row_vars ignored_col_vars ty =
    (* First, get the canonical representation of the type. *)
    let ty = W_CoreTypes.simple_type_repr ty in
    (* Now, check if we already saw it to prevent looping. *)
    match ty.W_Algebra.sty_mark with
    | W_Algebra.TM_generalize_seen ->
        (* Already seen, so, nothing new to explore. *)
        ()
    | W_Algebra.TM_not_seen -> (
        (* First, mark the type as seen to prevent looping. *)
        ty.W_Algebra.sty_mark <- W_Algebra.TM_generalize_seen ;
        (* Now, descend in the type's structure to really hunt for variables
           in order to record them in the list of generalized variables. *)
        match ty.W_Algebra.sty_desc with
        | W_Algebra.SType_var var ->
            (* Only mark (add) the variable as generalizable if it has a
               binding level strictly greater than the current one and if
               this variable is not already marked as generalizable (i.e. is
               bound by an already existing scheme or is already marked as to
               be generalized in the future type scheme we are currently
               trying to create). *)
            if var.W_Algebra.tv_level >
               !W_CoreTypes.current_binding_level then (
              if (List.memq ty ignored_tys)
                 ||
                 (List.memq ty !found_ty_parameters) then
                ()  (* Do nothing since either this variable is always
                       generalized for the current future scheme, hence is
                       already in the list of generalized variables, or this
                       variable is already marked as generalized because it
                       is under a type-forall, and then it must not appear as
                       a parameter of the current future type scheme. *)
              else (
                (* The variable has a binding level > to the current one, is
                   not already recorded as a parameter and is not recorded as
                   to be ignored. Hence, it must be marked as generalized for
                   the current future type scheme. So, mark now the variable
                   as generalized. *)
                var.W_Algebra.tv_level <- W_CoreTypes.generic_binding_level ;
                (* Normally, since this variable was not yet generalized, it
                   should not always be in the list. *)
                assert (not (List.memq ty !found_ty_parameters)) ;
                found_ty_parameters := ty :: !found_ty_parameters
               )
             )
            else (
              (* The variable has a level <= to the current binding level, hence
                 can't be generalized. If we are requested to forbid the presence
                 of non-generalizable type variables, then raise an error. *)
              if forbid_non_gen_vars then
                raise (Non_generalizable_type_var (initial_ty, ty))
             )
        | W_Algebra.SType_arrow (args_tys, res_ty) ->
            List.iter
              (find_parameters_in_ty
                 ignored_tys ignored_row_vars ignored_col_vars)
              args_tys ;
            find_parameters_in_ty
              ignored_tys ignored_row_vars ignored_col_vars res_ty
        | W_Algebra.SType_named named ->
            (match named.W_Algebra.nst_unwinded with
            | None -> ()
            | Some t ->
                find_parameters_in_ty
                  ignored_tys ignored_row_vars ignored_col_vars t) ;
            List.iter
              (find_parameters_in_ty
                 ignored_tys ignored_row_vars ignored_col_vars)
              named.W_Algebra.nst_args
        | W_Algebra.SType_sum_of_records sumcases_column ->
            find_parameters_in_column
              ignored_tys ignored_row_vars ignored_col_vars sumcases_column
        | W_Algebra.SType_forall scheme ->
            (* Extend our lists of type variables that must be ignored, i.e.
               not considered as parameter of our scheme we will build
               because they are already parameter of a type-forall present in
               the current type we are generalizing. *)
            let ignored_tys' =
              scheme.W_Algebra.ty_parameters @ ignored_tys in
            let ignored_row_vars' =
              scheme.W_Algebra.row_parameters @ ignored_row_vars in
            let ignored_col_vars' =
              scheme.W_Algebra.column_parameters @ ignored_col_vars in
            (* Recurse on the scheme's body with our extended sets of
               variables to ignore. *)
            find_parameters_in_ty
              ignored_tys' ignored_row_vars' ignored_col_vars'
              scheme.W_Algebra.body
       )
    | _ (* Other markers. *) -> assert false



  (* ************************************************************************ *)
  (** {Descr}: Local function that walks along a row type, harvesting its
      generalized row variables and recursing in inner types.                 *)
  (* ************************************************************************ *)
  and find_parameters_in_row ignored_tys ignored_row_vars ignored_col_vars row =
    (* Get the canonical representation of the row first. *)
    let (row_fields, row_ending) =
      (W_CoreTypes.row_type_repr row).W_Algebra.rt_value in
    (* Recurse search in the record fields. *)
    List.iter
      (fun (_, ty) ->
         find_parameters_in_ty ignored_tys ignored_row_vars ignored_col_vars ty)
      row_fields ;
    (* Search for a row-variable in the ending. *)
    match row_ending with
    | W_Algebra.Closed_row -> ()
    | W_Algebra.Var_row var ->
        (* Only mark (add) the variable as generalizable if it has a binding
           level strictly greater than the current one and if this variable
           is not already marked as generalizable (i.e. is bound by an
           already existing scheme or is already marked as to be generalized
           in the future type scheme we are currently trying to create). *)
        if var.W_Algebra.rv_level > !W_CoreTypes.current_binding_level then (
          if (List.memq var ignored_row_vars)
             ||
             (List.memq var !found_row_parameters) then
            ()  (* Do nothing since either this variable is always generalized
                   for the current future scheme, hence is already in the
                   list of generalized variables, or this variable is already
                   marked as generalized because it is under a type-forall,
                   and then it must not appear as a parameter of the current
                   future type scheme. *)
          else (
            (* The variable has a binding level > to the current one, is not
               already recorded as a parameter and is not recorded as to be
               ignored. Hence, it must be marked as generalized for the current
               future type scheme. So, mark now the variable as generalized. *)
            var.W_Algebra.rv_level <- W_CoreTypes.generic_binding_level ;
            (* Normally, since this variable was not yet generalized, it
               should not always be in the list. *)
            assert (not (List.memq var !found_row_parameters)) ;
            found_row_parameters := var :: !found_row_parameters
          )
        )
        else (
          (* The variable has a level <= to the current binding level, hence
             can't be generalized. If we are requested to forbid the presence
             of non-generalizable type variables, then raise an error. *)
          if forbid_non_gen_vars then (
            (* Create a dummy [simple_type] to embedd the row variable. *)
            let row = { W_Algebra.rt_value = ([], row_ending) } in
            let col =
              { W_Algebra.ct_value = ([row], W_Algebra.Closed_column) } in
            let ty = {
              W_Algebra.sty_desc = W_Algebra.SType_sum_of_records col ;
              W_Algebra.sty_link = None ;
              W_Algebra.sty_mark = W_Algebra.TM_not_seen } in
            raise (Non_generalizable_row_var (initial_ty, ty))
          )
        )



  (* ************************************************************************ *)
  (** {b Descr}: Local function that walks along a column type, harvesting its
      generalized column variables and recursing in inner types.              *)
  (* ************************************************************************ *)
  and find_parameters_in_column
      ignored_tys ignored_row_vars ignored_col_vars column =
    (* Get the canonical representation of the column first. *)
    let (col_records, col_ending) =
      (W_CoreTypes.column_type_repr column).W_Algebra.ct_value in
     (* Recurse search in the records forming the cases of the sum. *)
    List.iter
      (find_parameters_in_row ignored_tys ignored_row_vars ignored_col_vars)
      col_records ;
    (* Search for a column-variable in the ending. *)
    match col_ending with
    | W_Algebra.Closed_column -> ()
    | W_Algebra.Var_column var ->
        (* Only mark (add) the variable as generalizable if it has a binding
           level strictly greater than the current one and if this variable
           is not already marked as generalizable (i.e. is bound by an
           already existing scheme or is already marked as to be generalized
           in the future type scheme we are currently trying to create). *)
        if var.W_Algebra.cv_level > !W_CoreTypes.current_binding_level then (
          if (List.memq var ignored_col_vars)
             ||
             (List.memq var !found_column_parameters) then
            ()  (* Do nothing since either this variable is always generalized
                   for the current future scheme, hence is already in the
                   list of generalized variables, or this variable is already
                   marked as generalized because it is under a type-forall,
                   and then it must not appear as a parameter of the current
                   future type scheme. *)
          else (
            (* The variable has a binding level > to the current one, is not
               already recorded as a parameter and is not recorded as to be
               ignored. Hence, it must be marked as generalized for the current
               future type scheme. So, mark now the variable as generalized. *)
            var.W_Algebra.cv_level <- W_CoreTypes.generic_binding_level ;
            (* Normally, since this variable was not yet generalized, it
               should always not be in the list. *)
            assert (not (List.memq var !found_column_parameters)) ;
            found_column_parameters := var :: !found_column_parameters
          )
        )
        else (
          (* The variable has a level <= to the current binding level, hence
             can't be generalized. If we are requested to forbid the presence
             of non-generalizable type variables, then raise an error. *)
          if forbid_non_gen_vars then (
            (* Create a dummy [simple_type] to embedd the row variable. *)
            let col =
              { W_Algebra.ct_value = ([], col_ending) } in
            let ty = {
              W_Algebra.sty_desc = W_Algebra.SType_sum_of_records col ;
              W_Algebra.sty_link = None ;
              W_Algebra.sty_mark = W_Algebra.TM_not_seen } in
            raise (Non_generalizable_column_var (initial_ty, ty))
          )
        ) in



  (* ************************************************************************ *)
  (* {b Descr}: Effective body of the generalization function [b __generalize].
     It performs the hunt for generalizable types on the types passed as initial
     arguments.
     {b Visibility}: Not exported outside this module.                        *)
  (* ************************************************************************ *)
  let (extra_tys, extra_rows, extra_cols) = extra_variables in
  (* Attention: start finding first in the parameters we may have been given.
     This ensures that is there are some parameters corresponding to a
     pre-existing scheme (in case of generalization via [generalize2]), then
     they will be looked-up first, in there order of apparition, hence this
     will preserve the order of these parameters in the scheme we generate
     here. *)
  List.iter (find_parameters_in_ty [] [] []) extra_tys ;
  List.iter (find_parameters_in_row [] [] []) extra_rows ;
  List.iter (find_parameters_in_column [] [] []) extra_cols ;
  find_parameters_in_ty [] [] [] initial_ty ;
  (* Don't forget to reset the markers in the type we generalized. *)
  W_CoreTypes.cleanup_simple_type initial_ty ;
  List.iter W_CoreTypes.cleanup_simple_type extra_tys ;
  List.iter W_CoreTypes.cleanup_row_type extra_rows ;
  List.iter W_CoreTypes.cleanup_column_type extra_cols ;
  (* Reverse the lists of seen variables so that the first ones encountered
     appear first in the list.
     This is twofold:
     1) It makes the correspondence more direct between the order in the list
        and the order the variables are seen, hence ease debug inspection.
     2) Most important, it guaranties that when generalization is invoked from
        [__generalize2], the variables given in the [extra_tys] appears in the
        order of this list, which is very important to ensure that the obtained
        scheme has its variables in the same order than the original QML scheme
        manipulated during this generalization. In effect, [__generalize2] is
        used to create a [type_scheme] from a QML scheme. This latter already
        exists and appears in the program, with its own already decided order
        of generalized type variables. If, when we create our [type_scheme] we
        do not respect this order, when later making the variables of these 2
        view of the scheme, we will confuse parameters of the scheme, hence
        lead to incompatible type instances (parameters instantiated with
        arguments in a wrong order). An example:
          type t ('a, 'b) = ('a, 'b) in QML
          type ('b, 'a) t = ('a, 'b) in the typechecker where the scheme of the
            QML body has right be converted correctly, but seen generalized
            variables being registered in reverse order, the prenex list shows
            them in reverse order, hence leading to an incorrect scheme in which
            the parameters are reversed ! *)
  { W_Algebra.ty_parameters = List.rev !found_ty_parameters ;
    W_Algebra.row_parameters = List.rev !found_row_parameters ;
    W_Algebra.column_parameters = List.rev !found_column_parameters ;
    W_Algebra.body = initial_ty }



(* ************************************************************************** *)
(** {b Visibility}: Exported outside this module. *)
(* ************************************************************************** *)
let generalize ~forbid_non_gen_vars ty =
  try
    let sch =
      __generalize ~forbid_non_gen_vars ~extra_variables: ([], [], []) ty in
    W_CoreTypes.cleanup_simple_type ty ;
    sch
  with killed_by ->
    (* In any case, even if an error occurred, don't forget to cleanuo the type
       otherwise printing routines will encounter unexpected remaining
       markers. *)
    W_CoreTypes.cleanup_simple_type ty ;
    raise killed_by



(* ************************************************************************** *)
(** {b Visibility}: Exported outside this module. *)
(* ************************************************************************** *)
let generalize2 ~extra_variables ty =
  __generalize ~forbid_non_gen_vars: false ~extra_variables ty




(* ************************************************************************** *)
(** {b Descr}: Creates a type scheme whose body is the type passed as
    argument in which we do not perform any generalization. Hence, the type
    scheme is trivially non-polymorphic.
    This is very useful to insert the scheme of a recursive function in the
    typing environment used to type-check the body of this function.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let trivial_scheme ty =
  { W_Algebra.ty_parameters = [] ;
    W_Algebra.row_parameters = [] ;
    W_Algebra.column_parameters = [] ;
    W_Algebra.body = ty
  }



(* [TODO-REFACTOR] EST-CE QUE deep DOIT RESSORTIR VISIBLE AU NIVEAU DES
   ROUTINES DE SPÉCIALISATION OU EST-CE QUE L'ON DEVRAIT PLUTÔT AVOIR DES
   ROUTINES DE "SUBSTITUTIONS" QUI SERAIENT DÉDIÉE AU COMPORTEMENT DE QUAND
   deep = true ? *)
let (__specialize, specialize_with_given_variables_mapping) =

  (* Internal recursive copy of a type scheme replacing its generalized
     variables by their associated new fresh type variables found in the above
     mapping. *)
  let rec copy_simple_type ~deep ty =
    (* First, get the canonical representation of the type. *)
    let ty = W_CoreTypes.simple_type_repr ty in
    match ty.W_Algebra.sty_mark with
    | W_Algebra.TM_copy_copied_as aliased_as ->
        (* Type was already seen. Its copy is recorded here. *)
        aliased_as
    | W_Algebra.TM_export_cyclic _
    | W_Algebra.TM_not_seen -> (
        (* We must say we already saw ourself before recursing on the type
           structure, otherwise we can loop. Because we don't still know
           our real value, we will refer ourself with a type variable.
           Then when copy is finished, we'll put a link between this
           variable and the real result found for the copy. *)
        let tmp_ty = W_CoreTypes.type_variable () in
        ty.W_Algebra.sty_mark <- W_Algebra.TM_copy_copied_as tmp_ty ;
        (* And now let's recurse to get the copy of our description. *)
        let copied_ty =
          (match ty.W_Algebra.sty_desc with
           | W_Algebra.SType_var var ->
               (* If the type variable is generalized, then we make a copy
                  of it else, we return the type itself. Variables that
                  are generalized but must not be instantiated because
                  under a type-forall's scheme are pre-marked, so are caught
                  above when inspecting the marker of the hosting type. *)
               if var.W_Algebra.tv_level =
                  W_CoreTypes.generic_binding_level then (
                 (* Create a fresh QML variable but keep its base name. *)
                 let qml_var' =
                   QmlTypeVars.TypeVar.refresh var.W_Algebra.tv_qml in
                 let var' = {
                   W_Algebra.tv_level = !W_CoreTypes.current_binding_level ;
                   W_Algebra.tv_qml = qml_var' ;
                   W_Algebra.tv_debug_count = -1 } in
                 { W_Algebra.sty_desc = W_Algebra.SType_var var' ;
                   W_Algebra.sty_link = None ;
                   W_Algebra.sty_mark = W_Algebra.TM_not_seen }
               )
               else ty
           | W_Algebra.SType_arrow (args_tys, res_ty) ->
               (* Simply perform a structural recursion to inductively
                  copy all the type's sub-terms. *)
               let args_tys' = List.map (copy_simple_type ~deep) args_tys in
               let res_ty' = copy_simple_type ~deep res_ty in
               W_CoreTypes.type_arrow args_tys' res_ty'
           | W_Algebra.SType_named { W_Algebra.nst_name = name ;
                                     W_Algebra.nst_abbrev_height = height ;
                                     W_Algebra.nst_args = args ;
                                     W_Algebra.nst_unwinded = manifest} ->
               (* Unwounded view of the type must also be copied if we are
                  taking an instance with deep copy. This is the case when
                  dealing with type definitions' scheme elaboration and
                  type abbrevs expansion. *)
               let manifest' =
                 if deep then
                 (match manifest with
                  | None -> None
                  | Some t -> Some (copy_simple_type ~deep t))
                 else None in
               (* Finally, build a type description of the named type. *)
               W_CoreTypes.type_named
                 name height (List.map (copy_simple_type ~deep) args) manifest'
           | W_Algebra.SType_sum_of_records sumcases_column ->
               let sumcases_column' = copy_column_type ~deep sumcases_column in
               { W_Algebra.sty_desc =
                   W_Algebra.SType_sum_of_records sumcases_column' ;
                 W_Algebra.sty_link = None ;
                 W_Algebra.sty_mark = W_Algebra.TM_not_seen }
           | W_Algebra.SType_forall scheme ->
               (* Specialization is not deep. This means when taking a copy
                  of a type scheme, we **do not** take a copy of its
                  generalized variables. To prevent these generalized
                  variables from being instantiated by fresh ones, we bypass
                  the regular generalized-stuff-leads-to-fresh by entering
                  them as pre-mapped in the "mapping", pre-aliased directly
                  to themselves. *)
               List.iter
                 (fun sch_ty_var ->
                    let sch_ty_var =
                      W_CoreTypes.simple_type_repr sch_ty_var in
                    sch_ty_var.W_Algebra.sty_mark <-
                      W_Algebra.TM_copy_copied_as sch_ty_var)
                 scheme.W_Algebra.ty_parameters ;
               List.iter
                 (fun row_var ->
                    row_var.W_Algebra.rv_mark <-
                      W_Algebra.VM_copy_copied_as row_var)
                 scheme.W_Algebra.row_parameters ;
               List.iter
                 (fun col_var ->
                    col_var.W_Algebra.cv_mark <-
                      W_Algebra.VM_copy_copied_as col_var)
                 scheme.W_Algebra.column_parameters ;
               let body' = copy_simple_type ~deep scheme.W_Algebra.body in
               { W_Algebra.sty_desc =
                   W_Algebra.SType_forall
                     { scheme with W_Algebra.body = body' } ;
                 W_Algebra.sty_link = None ;
                 W_Algebra.sty_mark = W_Algebra.TM_not_seen }) in
        (* And now add the famous link. *)
        tmp_ty.W_Algebra.sty_link <- Some copied_ty ;
        W_TypeInfo.add_linked_object copied_ty.W_Algebra.sty_desc ty.W_Algebra.sty_desc ;
        (* Return our copy. *)
        copied_ty
      )
    | _ (* Other markers. *) -> assert false



  and copy_row_type ~deep row =
    (* First, get the canonical representation of the row. *)
    let (row_fields, row_ending) =
      (W_CoreTypes.row_type_repr row).W_Algebra.rt_value in
    let row_fields' =
      List.map
        (fun (label, ty) -> (label, copy_simple_type ~deep ty))
        row_fields in
    let row_ending' =
      (match row_ending with
       | W_Algebra.Var_row var -> (
           match var.W_Algebra.rv_mark with
           | W_Algebra.VM_copy_copied_as aliased_as ->
               (* Already seen hence return what it is already known to be. *)
               W_Algebra.Var_row aliased_as
           | W_Algebra.VM_not_seen ->
               (* As for [simple_type]s, if the variable is generalized then we
                  make a copy of it, otherwise we return itself. *)
               let var' =
                 if var.W_Algebra.rv_level =
                   W_CoreTypes.generic_binding_level then
                     W_CoreTypes.__row_variable ()
                 else (
                   (* Variable is not generalized return the variable
                      itself, i.e. the original row ending. *)
                   var
                 ) in
               var.W_Algebra.rv_mark <- W_Algebra.VM_copy_copied_as var' ;
               W_Algebra.Var_row var'
           | _ (* Other markers. *) -> assert false)
       | W_Algebra.Closed_row -> row_ending) in
    (* Finally, rebuild the copy of the row. *)
    { W_Algebra.rt_value = (row_fields', row_ending') }



  and copy_column_type ~deep column =
    (* First, get the canonical representation of the column. *)
    let (col_records, col_ending) =
      (W_CoreTypes.column_type_repr column).W_Algebra.ct_value in
    let col_records' = List.map (copy_row_type ~deep) col_records in
    let col_ending' =
      (match col_ending with
       | W_Algebra.Var_column var -> (
           match var.W_Algebra.cv_mark with
           | W_Algebra.VM_copy_copied_as aliased_as ->
               (* Already seen hence return what it is already known to be. *)
               W_Algebra.Var_column aliased_as
           | W_Algebra.VM_not_seen ->
               (* As for [simple_type]s, if the variable is generalized then we
                  make a copy of it, otherwise we return itself. *)
               let var' =
                 if var.W_Algebra.cv_level =
                   W_CoreTypes.generic_binding_level then
                     W_CoreTypes.__column_variable ()
                 else (
                   (* Variable is not generalized, hence return the
                      variable itself, i.e. the original column ending. *)
                   var
                 ) in
               var.W_Algebra.cv_mark <- W_Algebra.VM_copy_copied_as var' ;
               W_Algebra.Var_column var'
           | _ (* Other markers. *) -> assert false)
       | W_Algebra.Closed_column -> col_ending) in
    (* Finally, rebuild the copy of the column. *)
    { W_Algebra.ct_value = (col_records', col_ending') } in



  (
    (* ********************************************************************** *)
    (** {b Descr}: Effective body of the instantiation function [__specialize].
        This function instantiate a type scheme, i.e. makes a copy of its body
        replacing its generalized variables by fresh ones. Hence, from a type
        scheme, this function generates a fresh type in which polymorphic parts
        of the scheme are fresh.
        By the way, it returns the lists of fresh variables created to
        instantiate the generic ones found in the scheme.
        {b Visibility}: Not exported outside this module.                     *)
    (* ********************************************************************** *)
    (fun ~deep scheme ->
       (* First we need to build the mappings from the generalized variables of
          the scheme to new and fresh variables that will be used in the copy
          (i.e. in the instance) of the type represented by the scheme's
          body.
          These mappings are simulated by directly setting the field [sty_mark]
          of the generalized variables to fresh variables created here. Hence
          when descending on the type to generalize, we will have the impress
          that we already saw generalized variables, hence return what they are
          aliased to, i.e. the fresh ones we will create here. *)
       let fresh_ty_vars =
         List.map
           (fun ty_being_var ->
              match ty_being_var.W_Algebra.sty_desc with
              | W_Algebra.SType_var _ ->
                  (* Since we instantiate a scheme, no need to keep identity of
                     the variables of the scheme since they are only meaningful
                     in the scope of the definition having this scheme.
                     Avoids "inheriting" variable names from other functions. *)
                  let fresh_qml_ty_var = QmlTypeVars.TypeVar.next () in
                  let fresh_ty_var =
                    W_CoreTypes.__type_variable_with_public_identity
                      fresh_qml_ty_var in
                  ty_being_var.W_Algebra.sty_mark <-
                    W_Algebra.TM_copy_copied_as fresh_ty_var ;
                  fresh_ty_var
              | _ ->
                  (* By construction, types representing variables should always
                     be of the form [SType_var], otherwise there's something
                     buggy around. *)
                  assert false)
           scheme.W_Algebra.ty_parameters in
       (* Do the same thing for row variables. *)
       let fresh_row_vars =
         List.map
           (fun row_var ->
              (* Same remark than abovefor type variables about why using
                 [next] instead of [refresh]. *)
              let fresh_qml_row_var = QmlTypeVars.RowVar.next () in
              let fresh_row_var =
                W_CoreTypes.__row_variable_with_public_identity
                  fresh_qml_row_var in
              row_var.W_Algebra.rv_mark <-
                W_Algebra.VM_copy_copied_as fresh_row_var ;
              (* Return the fresh row variable used to instantiate the
                 generalized one directly as a [row_type]. This will be more
                 convenient for further manipulations instead of each time
                 having to embed it in a [row_type]. *)
              { W_Algebra.rt_value = ([], W_Algebra.Var_row fresh_row_var) })
           scheme.W_Algebra.row_parameters in
       (* Do the same thing for column variables. *)
       let fresh_col_vars =
         List.map
           (fun col_var ->
              (* Same remark than abovefor type variables about why using
                 [next] instead of [refresh]. *)
              let fresh_qml_col_var = QmlTypeVars.ColVar.next () in
              let fresh_col_var =
                W_CoreTypes.__column_variable_with_public_identity
                  fresh_qml_col_var in
              col_var.W_Algebra.cv_mark <-
                W_Algebra.VM_copy_copied_as fresh_col_var ;
              (* Return the fresh column variable used to instantiate the
                 generalized one directly as a [column_type]. *)
              { W_Algebra.ct_value = ([], W_Algebra.Var_column fresh_col_var) })
           scheme.W_Algebra.column_parameters in
       (* Now, copy the scheme body to instantiate it. *)
       let instance = copy_simple_type ~deep scheme.W_Algebra.body in
       (* Don't forget to cleanup the body of the scheme. *)
       W_CoreTypes.cleanup_simple_type scheme.W_Algebra.body ;
       (* Also do on its parameters in case they do not appear in the body. *)
       List.iter
         (fun ty_var ->
            ty_var.W_Algebra.sty_mark <- W_Algebra.TM_not_seen)
         scheme.W_Algebra.ty_parameters ;
       List.iter
         (fun row_var -> row_var.W_Algebra.rv_mark <- W_Algebra.VM_not_seen)
         scheme.W_Algebra.row_parameters ;
       List.iter
         (fun col_var -> col_var.W_Algebra.cv_mark <- W_Algebra.VM_not_seen)
         scheme.W_Algebra.column_parameters ;
       ((fresh_ty_vars, fresh_row_vars, fresh_col_vars), instance))
    ,



    (* ********************************************************************** *)
    (** {b Descr}: Effective body of the instantiation function
        [specialize_with_given_variables_mapping].
        This function instantiate a type scheme, i.e. makes a copy of its body
        replacing its generalized variables. Instead of [specialize] that
        replaces generalized variables by fresh ones, this function directly
        replaces them by types provided in the mappings passed as arguments.
        This is especially required when unifying types-forall since we must
        unify their body in which original generalized variables have been
        replaced by custom *also* generalized variables we manually create
        beforehand.                                                           *)
    (* ********************************************************************** *)
    (fun ~deep ty_vars_mapping row_vars_mapping column_vars_mapping scheme ->
       (* Like above in [specialize], we must simulate the mapping of
          generalized variables to what they must be instantiated. Since, we
          receive these mapping from outside, we walk along them setting, for
          each variable to map they contain, the marker as "already copied as"
          the stuff on what to map the variable to. *)
       List.iter
         (fun (ty_var, mapped_to) ->
            ty_var.W_Algebra.sty_mark <- W_Algebra.TM_copy_copied_as mapped_to)
         ty_vars_mapping ;
       List.iter
         (fun (row_var, mapped_to) ->
            row_var.W_Algebra.rv_mark <- W_Algebra.VM_copy_copied_as mapped_to)
         row_vars_mapping ;
       List.iter
         (fun (col_var, mapped_to) ->
            col_var.W_Algebra.cv_mark <- W_Algebra.VM_copy_copied_as mapped_to)
       column_vars_mapping ;
       let instance = copy_simple_type ~deep scheme.W_Algebra.body in
       (* Don't forget to cleanup the body of the scheme. *)
       W_CoreTypes.cleanup_simple_type scheme.W_Algebra.body ;
       (* Also do on its parameters in case they do not appear in the body. *)
       List.iter
         (fun (ty_var, _) -> ty_var.W_Algebra.sty_mark <- W_Algebra.TM_not_seen)
         ty_vars_mapping ;
        List.iter
          (fun (row_var, _) ->
            row_var.W_Algebra.rv_mark <- W_Algebra.VM_not_seen)
          row_vars_mapping ;
       List.iter
         (fun (col_var, _) ->
           col_var.W_Algebra.cv_mark <- W_Algebra.VM_not_seen)
         column_vars_mapping ;
       (* Finally, return the type that is the instantiation of the scheme. *)
       instance)
  )



let specialize scheme = snd (__specialize ~deep: false scheme)



let specialize2 ~deep scheme = __specialize ~deep scheme



(* ************************************************************************** *)
(** {b Descr}: Creates a type-forall surrounding the type received as argument
    if this type has a non trivial type scheme. In other words, this function
    tries to generalize the type received as argument. If the resulting scheme
    contains variables then a type-forall is created otherwise the initially
    passed type is returned as-is.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let type_forall ty =
  let scheme = generalize ~forbid_non_gen_vars: false ty in
  (* Really embed the type inside a forall-type if it contains generalized
     variables. *)
  if scheme.W_Algebra.ty_parameters <> [] ||
     scheme.W_Algebra.row_parameters <> [] ||
     scheme.W_Algebra.column_parameters <> [] then
    { W_Algebra.sty_desc = W_Algebra.SType_forall scheme ;
      W_Algebra.sty_link = None ;
      W_Algebra.sty_mark = W_Algebra.TM_not_seen }
  else ty



let get_type_forall_scheme ty =
  match (W_CoreTypes.simple_type_repr ty).W_Algebra.sty_desc with
  | W_Algebra.SType_forall sch -> Some sch
  | _ -> None



(* ************************************************************************** *)
(** {b Descr}: Performs the implicit instantiation of types forall. If the type
    received as argument is a type forall, this function creates and returns an
    instance of the scheme of this type forall. Otherwise, this function returns
    the initial type untouched (more accurately, returns the canonical
    representation of the initial type, hence the returned type, even if
    semantically identical to the initial one, car by physically different).
    By the way, the instantiated scheme is returned as an option. If the type
    was not a type forall, then there was no scheme to instantiate, then [None]
    is returned.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let automatically_instantiate_if_forall ty =
  let ty = W_CoreTypes.simple_type_repr ty in
  match ty.W_Algebra.sty_desc with
  | W_Algebra.SType_forall scheme -> ((specialize scheme), (Some scheme))
  | _ -> (ty, None)
