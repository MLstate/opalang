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
(** {b Descr}: This module implements typing environments.                    *)
(* ************************************************************************** *)



let toplevel_tydefs_schemes_env_memo = ref QmlAst.TypeIdentMap.empty
let toplevel_valdefs_schemes_env_memo = ref IdentMap.empty



(* ************************************************************************** *)
(** {b Descr}: See documentation in [w_TypingEnv.mli].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let reset_toplevel_tydefs_schemes_env_memo () =
  toplevel_tydefs_schemes_env_memo := QmlAst.TypeIdentMap.empty



(* ************************************************************************** *)
(** {b Descr}: See documentation in [w_TypingEnv.mli].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let reset_toplevel_valdefs_schemes_env_memo () =
  toplevel_valdefs_schemes_env_memo := IdentMap.empty



let extend_toplevel_valdefs_env_memo id_name sch =
  toplevel_valdefs_schemes_env_memo :=
    IdentMap.add id_name sch !toplevel_valdefs_schemes_env_memo



type t = {
  ty_env_local : (QmlAst.ident * W_Algebra.types_scheme) list ;
  ty_def_env_local :
    (QmlAst.TypeIdent.t *      (** Ident representing the name of the type
                                   constructor. *)
     (W_Algebra.types_scheme * (** Type scheme bound to the type constructor. *)
      QmlTypes.abbrev_height)  (** Height of type abbreviations this constructor
                                   has. *)
    ) list ;
  ty_env_qml_global : QmlTypes.Env.t
}



let forward_ref__unify_simple_type :
  (t -> W_Algebra.simple_type -> W_Algebra.simple_type -> unit) ref =
  ref (fun (_env : t) _ty1 _ty2 -> failwith "Unify1")
let forward_ref__unify_row_type :
  (t -> W_Algebra.row_type -> W_Algebra.row_type -> bool) ref =
  ref (fun (_env : t) _ty1 _ty2 -> failwith "Unify2")
let forward_ref__unify_column_type :
  (t -> W_Algebra.column_type -> W_Algebra.column_type -> unit) ref =
  ref (fun (_env : t) _ty1 _ty2 -> failwith "Unify3")



let empty_typing_env = {
  ty_env_local = [] ;
  ty_def_env_local = [] ;
  ty_env_qml_global = QmlTypes.Env.empty
}



let from_qml_typing_env qml_env = {
  ty_env_local = [] ;
  ty_def_env_local = [] ;
  ty_env_qml_global = qml_env
}



(** {Visibility}: Not exported outside this module *)
let automatically_add_type_construtor_arguments_if_omitted args nb_required =
  if (nb_required > 0) && (args = []) then (
    (* Ok, really **no** arguments provided although some are required. Build
       some using fresh type variables. *)
    Base.List.init nb_required (fun _ -> W_CoreTypes.type_variable ())
  )
  else args


(* ************************************************************************** *)
(** {b Descr}: Lists of encountered variables, mapping these QML variables to
    their typechecker's internals counterpart. These lists are initialized as
    empty at the beginning the a type importation, then grow all along the type
    sub-terms processing.
    In the case of processing the body of a type scheme, these lists are
    pre-initialized with the generalized variables of the scheme. This way, when
    we encounter them in the type representing the body, we directly know on
    what to map them (i.e. into which type variable they must be converted).
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
type variables_mapping = {
  mutable vm_type_variables :
    (QmlTypeVars.TypeVar.t * W_Algebra.simple_type) list ;
  mutable vm_row_variables :
    (QmlTypeVars.RowVar.t * W_Algebra.row_variable) list ;
  mutable vm_column_variables :
    (QmlTypeVars.ColVar.t * W_Algebra.column_variable) list
}



(* ************************************************************************** *)
(** {b Descr}: This list stacks the variables mappings currently opened. At
    least, there must be one variables mapping, the one created when the
    main inference routine was called. Normally, at the end of the main
    inference routine, we release this mapping, hence reverting this stack to
    an empty list.
    We need to stack variables mappings because we need to create temporary ones
    for instance to fetch identifiers' types or type's definitions from the
    environment while we are already in a context where some type variables
    have been already seen in type constraints expressions. First, these seen
    variables must not be forgotten when returning from the environment lookup.
    Second, the new variables that may be created while fetching in the
    environment must not pollute (i.e. must not be added to) those we already
    know.
    So, basically, we nee to create a fresh mapping each time we want to
    insulate the current one, then release it when we have finished with it.
    {b Visibility} Not exported outside this module.                          *)
(* ************************************************************************** *)
let variables_mapping_stack = ref ([] : variables_mapping list)



(* ************************************************************************** *)
(** {b Descr}: Creates a new and empty variables mapping, put it in head of the
    variables mappings stack, hence making it the active one. As stated, this
    mapping is empty, i.e. it contains no "seen" variables.
    {b Visibility} Exported outside this module.                              *)
(* ************************************************************************** *)
let new_empty_variables_mapping () =
  let m = {
    vm_type_variables = [] ;
    vm_row_variables = [] ;
    vm_column_variables = []
  } in
  variables_mapping_stack := m :: !variables_mapping_stack



(* ************************************************************************** *)
(** {b Descr}: Creates a new variables mapping that copies the currently active
    one. This way, new coming type variable will be registered in this new
    mapping instead of the currently active one, but variables known in this
    latter are not forgotten in the newly created variables mapping.
    This is required to insulate mappings of definitions that must have their
    own mapping scope but need to remember variables of the surrounding scope.
    This is especially the case of module fields because they are something
    like toplevel let-definitions, with a separate scope for each, but that
    must remind variables introduced by constraints of the outer expression
    (for instance in the case of functors like in
      func(x : 'a) {{ f(x : 'a, y : 'b) }}
    where the outer lambda introducing [x] constrained by ['a] introduces the
    type variable ['a] that must remain known while typechecking the field [f]).
    Note that local let-in definition do not use this. In effect, in them, the
    scope of a type variable continues in the "in"-part. In other words,
      let (x : 'a) = 5 in let (y : 'a) = true in ...
    introduces the variable ['a] as soon as the first let, then the second
    of its apparition, constraining [y] makes reference to the same variable
    than the one constraining [x]. So this example doesn't typecheck
    correctly.
    {b Visibility} Exported outside this module.                              *)
(* ************************************************************************** *)
let new_inheriting_variables_mapping () =
  (* By construction, this function is called while typechecking an expression
     that is more nested than at toplevel. Especially, this function is used to
     typecheck modules since that's them who need to create for each field a
     separate mapping but without forgetting variables introduced by type
     constraints in surrounding expressions hosting the module.
     For this reason, we should always have an existing mapping at this point,
     i.e. the stack of mappings must not be empty. *)
  let current_mapping =
    (match !variables_mapping_stack with
    | [] -> assert false
    | h :: _ -> h) in
  let m = {
   vm_type_variables = current_mapping.vm_type_variables ;
   vm_row_variables = current_mapping.vm_row_variables ;
   vm_column_variables = current_mapping.vm_column_variables } in
   variables_mapping_stack := m :: !variables_mapping_stack



(* ************************************************************************** *)
(** {b Descr}: Removes the current variables mapping, from the variables
    mappings stack, hence making the previous one in the list the active one.
    This function expects there exists an active variables mapping. This should
    be the case otherwise the typechecker is broken somewhere.
    {b Visibility} Exported outside this module.                              *)
(* ************************************************************************** *)
let release_variables_mapping () =
  match !variables_mapping_stack with
  | [] -> assert false
  | _ :: q -> variables_mapping_stack := q



let get_current_variables_mapping () =
  match !variables_mapping_stack with
  | [] -> assert false
  | m :: _ -> m



(** Highly not exported !!!! [TODO] Doc to do !! *)
let restore_variables_mappings_stack_from_backup stack_backup =
  variables_mapping_stack := stack_backup



let reset_empty_variables_mapping_on_error () =
  variables_mapping_stack := []



(* ************************************************************************** *)
(** {b Descr}: Exception raised when the internal an recursive routine that
    transforms a QML type expression into a [simple_type] encounters a
    [QmlAst.TypeAbstract]. In this case, because internally the typechecker
    represents abstract types by named types without manifest representation
    and doesn't have any notion of "abstract" type constructor, we do not
    synthesize any [simple_type] at this point, we raise this exception.
    {b Visibility}: Exported outside this module                              *)
(* ************************************************************************** *)
exception Importing_qml_abstract_ty



let find_type_nb_args_and_abbrev_height id_name typing_env =
  let rec find_in_local_env = function
    | [] -> raise Not_found
    | (n, (sc, h)) :: q -> if QmlAst.TypeIdent.equal n id_name then
        ((List.length sc.W_Algebra.ty_parameters), h)
      else find_in_local_env q in
  try find_in_local_env typing_env.ty_def_env_local
  with Not_found -> (
    let opt_qml_scheme =
      QmlTypes.Env.TypeIdent.find_opt
        ~visibility_applies: true id_name typing_env.ty_env_qml_global in
    match opt_qml_scheme with
    | None -> raise Not_found
    | Some (qml_scheme, height) ->
        let (qml_quantification, _, _) =
          QmlGenericScheme.export_unsafe qml_scheme in
        ((QmlTypeVars.TypeVarSet.cardinal
            qml_quantification.QmlTypeVars.typevar),
         height)
  )



(* ************************************************************************** *)
(** {b Descr} Imports a QML type to transform it into a [simple_type] *without*
    expanding named types definitions. This especially means that a QML named
    type leads to a [SType_named] with no manifest representation even if in
    the QML environment the type name corresponds to a definition having its
    own structure. Remember that abbreviations expansions are done
    incrementally by need !
    @raise QmlTyperException.Exception
    @raise Importing_qml_abstract_ty
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let qml_type_to_simple_type qml_tydef_env initial_qml_ty ~is_type_annotation =

  (* Get the currently active variables mapping. *)
  let variables_mapping = get_current_variables_mapping () in

  (* NOTE: Since QML type algebra doesn't support cyclic types, no need to get
     embarrassed with all the looping prevention stuff. Simply make structural
     descent on types. *)

  (** {b Visibility}: Not exported outside this module. *)
  let rec rec_import_qml_type qml_ty =
    #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
    OManager.printf "@[<1>rec_import_qml_type: %a@]@."
      QmlPrint.pp_base#ty qml_ty ;
    #<End> ; (* <---------- END DEBUG *)
    match qml_ty with
    | QmlAst.TypeConst const_ty -> (
        match const_ty with
        | QmlAst.TyFloat -> W_CoreTypes.type_float ()
        | QmlAst.TyInt -> W_CoreTypes.type_int ()
        | QmlAst.TyString -> W_CoreTypes.type_string ()
        | QmlAst.TyNull ->
            (* According to the comment in qmlAst.ml, nothing should have
               this type. *)
            let err_ctxt = QmlError.Context.ty qml_ty in
            QmlError.error
              err_ctxt "Type 'Null' encountered during QML type importation."
      )
    | QmlAst.TypeVar qml_var -> (
        (* If we already encountered this QML type variable, then to preserve
           sharing between its apparitions, we simply return the variable we
           already created. *)
        #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
        OManager.printf "rec_import_qml_type TypeVar %s@."
          (QmlAst.TypeVar.to_string qml_var) ;
        #<End> ; (* <---------- END DEBUG *)
        try
          let tmp =
            Base.List.assoc_custom_equality
              ~eq: QmlTypeVars.TypeVar.equal qml_var
              variables_mapping.vm_type_variables in
          #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
          OManager.printf "@[<1>Found among known vars and mapped to: %a@]@."
            W_PrintTypes.pp_simple_type tmp ;
          #<End> ; (* <---------- END DEBUG *)
          tmp
        with Not_found ->
          (* Otherwise, we create a fresh type variable and record it in the
             variables mapping. Attention, since QML gives us the public
             identity of the variable, we must remind it here and not create a
             new one. *)
          let var =
            if is_type_annotation then
              W_CoreTypes.__annotation_type_variable_with_public_identity
                qml_var
            else W_CoreTypes.__type_variable_with_public_identity qml_var in
          #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
          OManager.printf
            "@[<1>NOT Found among known vars so mapped to: %a@]@."
            W_PrintTypes.pp_simple_type var ;
          #<End> ; (* <---------- END DEBUG *)
          variables_mapping.vm_type_variables <-
            (qml_var, var) :: variables_mapping.vm_type_variables ;
          (* Since if variable must be generalized, then it is in the mapping,
             we do not need to get worry about whether the created variable must
             really have the current binding level instead a possibly generic
             level.
             Note that because QML doesn't allow non generalized variables in
             type schemes, all the variables we generate during our QML type
             importation should be generalized. So I don't see how we could
             need to create a variable here, hence a variable with the current
             binding level O_o ! *)
          var
      )
    | QmlAst.TypeArrow (qml_args_tys, qml_res_ty) ->
        (* Simple structural re-construction. *)
        let args_tys =
          List.map rec_import_qml_type qml_args_tys in
        let res_ty = rec_import_qml_type qml_res_ty in
        W_CoreTypes.type_arrow args_tys res_ty
    | QmlAst.TypeRecord qml_ty_row ->
        #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
        OManager.printf "rec_import_qml_type TypeRecord@." ;
        #<End> ; (* <---------- END DEBUG *)
        rec_import_qml_row_type qml_ty_row
    | QmlAst.TypeSum qml_ty_col ->
        #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
          OManager.printf "rec_import_qml_type TypeSum@." ;
        #<End> ; (* <---------- END DEBUG *)
        rec_import_qml_column_type qml_ty_col
    | QmlAst.TypeSumSugar _ ->
        (* Don't known what to do with this. *)
        let err_ctxt = QmlError.Context.ty qml_ty in
        QmlError.error
          err_ctxt "Type 'SumSugar' encountered during QML type importation."
    | QmlAst.TypeName (qml_args_ty, qml_name) ->
        (* [TODO-REFACTOR] EXPLICATIONS. *)
        let effective_args = List.map rec_import_qml_type qml_args_ty in
        let (nb_args, height) =
          find_type_nb_args_and_abbrev_height qml_name qml_tydef_env in
        let args =
          automatically_add_type_construtor_arguments_if_omitted
            effective_args nb_args in
        W_CoreTypes.type_named qml_name height args None
    | QmlAst.TypeAbstract ->
        (* QML types encode abstract type expressions this way. Internally, the
           typechecker wants to represent abstract types by a named type with
           no manifest representation. We are in the case where in a QML type
           expression we encounter an "abstract". If there were no way to
           express in the language the type "abstract", there would be no risk
           that when we arrive here we are dissecting a type expression
           literally containing this "abstract".
           But in fact, it is possible to write
             _ = 1 : { x: external }
           and in this case, the exception will escape. So, until it is no more
           possible to write this, the exception must be caught by the typing
           pass.
           In the regular, normal situation, such QML "abstract" appear in type
           expressions bound to type names in the environment. They hence
           represent the fact that a type (which is then abstract) definition
           binds a name to ... an "abstract thing".
           From this analyze, we are sure that the current case
           [QmlAst.TypeAbstract] can only occur "under" a named type expression.
           So, we simply refuse to return a type expression, we raise an
           exception that will be caught by the surrounding [QmlAst.TypeName]
           case, this latter being in charge to really create the named type
           with no manifest representation. *)
         #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
         OManager.printf
           "rec_import_qml_type TypeAbstract. Raising exception to delay@."
         #<End> ; (* <---------- END DEBUG *)
        raise Importing_qml_abstract_ty
    | QmlAst.TypeForall (qml_ty_vars, qml_row_vars, qml_col_vars, qml_body) ->
        #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
        OManager.printf "rec_import_qml_type: TypeForall@." ;
        #<End> ; (* <---------- END DEBUG *)
        (* Since the variables bound in the type forall are expected not to
           appear outside the scope of this type, normally in the mappings we
           do not have any reason to already have recorded them. So, we directly
           and blindly extend the mappings with fresh variables.
           By the way, we recover the list of obtained generalized variables
           for our final scheme, hence we don't have to walk the body of the
           scheme we are creating afterwards.
           Attention: since these variables correspond to generalized ones,
           we must create them with their binding level set to the generic one,
           otherwise their level would remain the current binding level !
           Attention, since QML gives us the public identity of the variable,
           we must remind it here and not create a new one. *)
        let ty_vars =
          List.map
            (fun qml_ty_var ->
               (* Create the variable directly generalized. *)
               let new_ty_var =
                 W_CoreTypes.__generic_type_variable_with_public_identity
                   qml_ty_var in
               variables_mapping.vm_type_variables <-
                 (qml_ty_var, new_ty_var) ::
                 variables_mapping.vm_type_variables ;
               new_ty_var)
            qml_ty_vars in
        let row_vars =
          List.map
            (fun qml_row_var ->
               (* Create the variable directly generalized. *)
               let new_row_var =
                 W_CoreTypes.__generic_row_variable_with_public_identity
                   qml_row_var in
               variables_mapping.vm_row_variables <-
                 (qml_row_var, new_row_var) ::
                 variables_mapping.vm_row_variables ;
               new_row_var)
            qml_row_vars in
        let col_vars =
          List.map
            (fun qml_col_var ->
               (* Create the variable directly generalized. *)
               let new_col_var =
                 W_CoreTypes.__generic_column_variable_with_public_identity
                   qml_col_var in
               variables_mapping.vm_column_variables <-
                 (qml_col_var, new_col_var) ::
                 variables_mapping.vm_column_variables ;
               new_col_var)
            qml_col_vars in
        (* Now, import the body of the scheme, the variables mappings having
           been extended by side effect just above with the scheme generalized
           variables. *)
        let body = rec_import_qml_type qml_body in
        (* Since we handled the generalized variables by hand here, we don't
           need to call the regular [type_forall] function to create the
           type forall. In effect, no need to again generalize the obtained
           body since we already marked its generalized variables. So we
           directly build the structure of our final type manually. *)
        let scheme = {
          W_Algebra.ty_parameters = ty_vars ;
          W_Algebra.row_parameters = row_vars ;
          W_Algebra.column_parameters = col_vars ;
          W_Algebra.body = body } in
        { W_Algebra.sty_desc = W_Algebra.SType_forall scheme ;
          W_Algebra.sty_link = None ;
          W_Algebra.sty_mark = W_Algebra.TM_not_seen }



  (** {b Visibility}: Not exported outside this module. *)
  and rec_import_qml_row_type (QmlAst.TyRow (qml_fields, qml_row_ending)) =
    (* A QML row type corresponds to a QML record type (i.e. to a
       [QmlAst.TypeRecord]). We then transform this into a closed column type
       having only one row.
       First, we transform the fields of the row. *)
    let row_fields =
      List.map
        (fun (field_name, field_qml_ty) ->
           (field_name, (rec_import_qml_type field_qml_ty)))
        qml_fields in
    (* Now, transform the row ending into a variable or a "closed". *)
    let row_ending =
      (match qml_row_ending with
       | None -> W_Algebra.Closed_row       (* Row is closed. *)
       | Some qml_row_var -> (
           (* If we already encountered this QML row variable, then to preserve
              sharing between its apparitions, we simply return the variable we
              already created. *)
           try
             W_Algebra.Var_row
               (Base.List.assoc_custom_equality
                  ~eq: QmlTypeVars.RowVar.equal qml_row_var
                  variables_mapping.vm_row_variables)
           with Not_found ->
             (* Otherwise, we create a fresh row variable and record it in the
                variables mapping.
                Attention, since QML gives us the public identity of the
                variable, we must remind it here and not create a new one. *)
             let var =
               if is_type_annotation then
                W_CoreTypes. __annotation_row_variable_with_public_identity
                   qml_row_var
               else
                 W_CoreTypes.__row_variable_with_public_identity qml_row_var in
             variables_mapping.vm_row_variables <-
               (qml_row_var, var) :: variables_mapping.vm_row_variables ;
             W_Algebra.Var_row var)) in
    (* Finally, build our "record type" by plugging the fields into a row type
       and the row into a closed column type. *)
    let row = { W_Algebra. rt_value = (row_fields, row_ending) } in
    let closed_column =
      { W_Algebra.ct_value = ([row], W_Algebra.Closed_column) } in
    { W_Algebra.sty_desc = W_Algebra.SType_sum_of_records closed_column ;
      W_Algebra.sty_link = None ;
      W_Algebra.sty_mark = W_Algebra.TM_not_seen }



  (** {b Visibility}: Not exported outside this module. *)
  and rec_import_qml_column_type
      (QmlAst.TyCol (qml_column_rows, qml_column_ending)) =
    (* By construction, a QML column type contains several rows but these rows
       are all closed. However, the column itself can be opened or closed.
       First, we transform each row. *)
    let column_rows =
      List.map
        (fun qml_row ->
           (* To transform such a row, we first transform the fields of the
              row. *)
           let row_fields =
             List.map
               (fun (field_name, field_qml_ty) ->
                  (field_name, (rec_import_qml_type field_qml_ty)))
               qml_row in
           (* And since by construction rows are closed here, we plug the
              obtained fields in a closed row type. *)
           { W_Algebra.rt_value = (row_fields, W_Algebra.Closed_row) })
        qml_column_rows in
    (* Now we have transformed all the rows of the sum type, we transform the
       column ending. *)
    let column_ending =
      (match qml_column_ending with
       | None -> W_Algebra.Closed_column       (* Column is closed. *)
       | Some qml_column_var -> (
           (* If we already encountered this QML column variable, then to
              preserve sharing between its apparitions, we simply return the
              variable we already created. *)
           try
             W_Algebra.Var_column
               (Base.List.assoc_custom_equality
                  ~eq: QmlTypeVars.ColVar.equal qml_column_var
                  variables_mapping.vm_column_variables)
           with Not_found ->
             (* Otherwise, we create a fresh column variable and record it in
                the variables mapping.
                Attention, since QML gives us the public identity of the
                variable, we must remind it here and not create a new one. *)
             let var =
               if is_type_annotation then
                 W_CoreTypes.__annotation_column_variable_with_public_identity
                   qml_column_var
               else
                 W_CoreTypes.__column_variable_with_public_identity
                   qml_column_var in
             variables_mapping.vm_column_variables <-
               (qml_column_var, var) :: variables_mapping.vm_column_variables ;
             W_Algebra.Var_column var)) in
    (* Finally, plug all of this in a sum type. *)
    let column = { W_Algebra.ct_value = (column_rows, column_ending) } in
    { W_Algebra.sty_desc = W_Algebra.SType_sum_of_records column ;
      W_Algebra.sty_link = None ;
      W_Algebra.sty_mark = W_Algebra.TM_not_seen } in

  (* Effective body of the function [qml_type_to_simple_type_init]. *)
  #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
  OManager.printf "@[<1>__qml_type_to_simple_type_init on type:@ %a@]@."
    QmlPrint.pp_base#ty initial_qml_ty ;
  #<End> ; (* <---------- END DEBUG *)
  rec_import_qml_type initial_qml_ty



(* ************************************************************************** *)
(** {b Descr}: Imports a QML type scheme to transform it into a [types_scheme].
    The body of the scheme has no expanded abbreviations.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let qml_type_scheme_to_types_scheme qml_tydef_env initial_qml_scheme =
  let (qml_quantification, qml_scheme_body, _) =
   QmlGenericScheme.export_unsafe initial_qml_scheme in
  (* We will transform the body of the QML scheme into a [simple_type], and
     to make sure that variables marked generalized in it will also be
     in the result scheme, we pre-initialize the variables mappings for the
     generalized variables, mapping them onto fresh [type_variable]s we
     create beforehand with their level being generic. This way, during
     the QML type copy-conversion, these variables will directly lead to
     generalized variables in the body of the result [types_scheme].
     At the same time we populate the mappings, we remind the generalized
     variables created as [simple_type]s to later set them in the result
     [types_scheme] when we will finally create it.
     ATTENTION: In QML quantification, variables are stored as sets, not
     as lists in [types_scheme]. However, the order of variables in
     relation with the order they appear in the scheme's body is
     important ! In [types_scheme], this is all explicit since generalized
     variables are recorded in lists, hence with an explicit order.
     In QML scheme, that's not explicit like that. Instead, variables are
     relabeled (stamped ?) in the order they appear in the scheme body.
     Then they are inserted in the sets. So, getting the elements of
     these sets, then sorting them will give back the list of generalized
     variables in the right order. Fortunately, the [Set.elements] function
     that gives the list of elements of a set directly give them sorted in
     increasing order, so we don't have to explicitly sort them. *)
  let variables_mapping = get_current_variables_mapping () in
  let ty_parameters =
    List.map
      (fun qml_ty_var ->
         let ty_var =
          W_CoreTypes.__generic_type_variable_with_public_identity
            qml_ty_var in
        (* Extend the current variables mapping with the generalized type
           variables of the type scheme. *)
         variables_mapping.vm_type_variables <-
           (qml_ty_var, ty_var) :: variables_mapping.vm_type_variables ;
         ty_var)
      (QmlTypeVars.TypeVarSet.elements
         qml_quantification.QmlTypeVars.typevar) in
  #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
    OManager.printf
    "qml_type_scheme_to_types_scheme. Order of simple_type params:@." ;
  List.iter
    (fun ttt ->
       OManager.printf "> %a@." W_PrintTypes.pp_simple_type ttt)
    ty_parameters ;
  #<End> ; (* <---------- END DEBUG *)
  let row_parameters =
    List.map
      (fun qml_row_var ->
         let row_var =
           W_CoreTypes.__generic_row_variable_with_public_identity
             qml_row_var in
        (* Extend the current variables mapping with the generalized row
           variables of the type scheme. *)
         variables_mapping.vm_row_variables <-
           (qml_row_var, row_var) :: variables_mapping.vm_row_variables ;
         row_var)
      (QmlTypeVars.RowVarSet.elements
         qml_quantification.QmlTypeVars.rowvar) in
  let column_parameters =
    List.map
      (fun qml_col_var ->
        let col_var =
          W_CoreTypes.__generic_column_variable_with_public_identity
            qml_col_var in
        (* Extend the current variables mapping with the generalized column
           variables of the type scheme. *)
        variables_mapping.vm_column_variables <-
          (qml_col_var, col_var) :: variables_mapping.vm_column_variables ;
        col_var)
      (QmlTypeVars.ColVarSet.elements qml_quantification.QmlTypeVars.colvar) in
  (* Ok, now mappings are pre-initialized. We can copy and convert the body
     of the QML scheme. Note that because type schemes expressions can't be
     used as annotations (i.e. explicit type constraint) we always perform the
     type importation telling [~is_type_annotation: false].
     If the body of the scheme appears to be abstract, then we will catch the
     point and forward the surrounding type creation delay by a new exception
     raising in which we remind the number of type parameters the scheme we
     obtained but not succeeded to convert from QML had. *)
  let scheme_body =
    qml_type_to_simple_type
      qml_tydef_env qml_scheme_body ~is_type_annotation: false in
  (* Finally, construct a regular [types_scheme]. *)
  { W_Algebra.ty_parameters = ty_parameters ;
    W_Algebra.row_parameters = row_parameters ;
    W_Algebra.column_parameters = column_parameters ;
    W_Algebra.body = scheme_body }



(** {b Descr}: [TODO] Doc.
    Rem: since typechecker really trusts visibility information and is not
    concerned by stuff like getting effective representation of types that
    are considered as abstract or private for sake of code generation,
    serialization or whatever, [find_type] directly handles the visibility,
    returning [Not_found] if the type is found in the environment but not
    visible from the current package.
    @raise Not_found
    @raise Unification_simple_type_conflict
    @raise Unification_binding_level_conflict
    @raise Unification_column_conflict
   {b Visibility}: Not exported outside this module. *)
let find_type id_name typing_env =
  (* Note that local environment doesn't keepthe visibility information. This
     is not needed because it is emptied at each new package compilation, hence
     obviously types present in thsi environment belong to the current compiled
     package and hence are visible from it. *)
  let rec find_in_local_env = function
    | [] -> raise Not_found
    | (n, t) :: q -> if QmlAst.TypeIdent.equal n id_name then t
      else find_in_local_env q in

  #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
  OManager.printf "find_type: %s@." (QmlAst.TypeIdent.to_string id_name) ;
  #<End> ; (* <---------- END DEBUG *)
  try
    (* First, try to recover the scheme bound to the type identifier in the
       most recent, i.e. local part of the environment. See note above in
       [find_in_local_env] about visibility concern. *)
    let (scheme, _) = find_in_local_env typing_env.ty_def_env_local in
    #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
    OManager.printf "@[<1>find_type for %s, Ml scheme:@ %a@]@."
      (QmlAst.TypeIdent.to_string id_name)
      W_PrintTypes.pp_scheme scheme ;
    #<End> ; (* <---------- END DEBUG *)
    scheme
  with Not_found -> (
    (* Have a look in the toplevel environment memoized stuff to avoid
       reconstruction of the scheme from scratch. *)
    try QmlAst.TypeIdentMap.find id_name !toplevel_tydefs_schemes_env_memo
    with Not_found -> (
      (* Finally, nothing found in the local and in the memoized environments,
         so fetch in the real toplevel environment and reconstruct the scheme
         from the QML version. *)
      let opt_qml_scheme =
        QmlTypes.Env.TypeIdent.find_opt
          ~visibility_applies: true id_name typing_env.ty_env_qml_global in
      match opt_qml_scheme with
      | None -> raise Not_found
      | Some (qml_scheme, abbr_height) ->
          #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
          OManager.printf "@[<1>find_type for %s, QML scheme:@ %a@]@."
            (QmlAst.TypeIdent.to_string id_name)
            QmlPrint.pp_base#tsc qml_scheme ;
          #<End> ; (* <---------- END DEBUG *)
          (* We found a binding for this identifier in the global environment.
             Then we must transform the obtained [QmlTypes.typescheme] into a
             scheme in the format manipulated by the typechecker. *)
          let (qml_quantification, _, _) =
            QmlGenericScheme.export_unsafe qml_scheme in
          let ty_parameters =
            List.rev
              (Base.List.init
                 (QmlTypeVars.TypeVarSet.cardinal
                    qml_quantification.QmlTypeVars.typevar)
                 (fun _ -> W_CoreTypes.__generic_type_variable ())) in
          let row_parameters =
            List.rev
              (Base.List.init
                 (QmlTypeVars.RowVarSet.cardinal
                    qml_quantification.QmlTypeVars.rowvar)
                 (fun _ -> W_CoreTypes.__generic_row_variable ())) in
          let column_parameters =
            List.rev
              (Base.List.init
                 (QmlTypeVars.ColVarSet.cardinal
                    qml_quantification.QmlTypeVars.colvar)
                 (fun _ -> W_CoreTypes.__generic_column_variable ())) in

           (* Backup the current binding level in order to restore it in case we
              are killed by an exception. *)
           let backuped_binding_level = !W_CoreTypes.current_binding_level in
           (* Same thing for the variables mapping that may be unbalanced.
              So we make a backup of the current stack of mappings to restore
              it if some new mappings have been created and not popped. *)
           let backuped_variables_mappings_stack = !variables_mapping_stack in
          (* From now, let's protect the code from exceptions because we will
             change both the current binding level and the variable mapping.
             So if an exception kills us, we must restore both of them into
             a consistent state, i.e. avoid unbalanced [begin_definition] and
             [end_definition] and avoid remaining in a variable mapping that
             was created for the current invocation of the present function. *)
          try
            (* Increase binding level to allow generalization of the future
               obtained type scheme. *)
            W_CoreTypes.begin_definition () ;
            let proto_body = W_CoreTypes.type_variable () in
            let proto_scheme = {
              W_Algebra.ty_parameters = ty_parameters ;
              W_Algebra.row_parameters = row_parameters ;
              W_Algebra.column_parameters = column_parameters ;
              W_Algebra.body = proto_body } in
            let typing_env' = {
              typing_env with
                ty_def_env_local =
                (* FPE says: 0 or -1 ? *)
                (id_name, (proto_scheme, abbr_height)) ::
                  typing_env.ty_def_env_local } in
            #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
            OManager.printf "@[<1>find_type for %s: Added proto scheme:@ %a@]@."
              (QmlAst.TypeIdent.to_string id_name)
              W_PrintTypes.pp_scheme proto_scheme ;
            OManager.printf
              "@[<1>find_type for %s: Start converting qml scheme@ %a@]@."
              (QmlAst.TypeIdent.to_string id_name)
              QmlPrint.pp_base#tsc qml_scheme ;
            #<End> ; (* <---------- END DEBUG *)

            (* We are getting the scheme of an identifier in the environment.
               So, there is no reason for having variables in this scheme that
               are already in the variables mapping induced by type constraints
               previously seen. So we safely use a new variables mapping and
               [qml_type_scheme_to_types_scheme] will initialize it with the
               generalized variables of the scheme. *)
            new_empty_variables_mapping () ;
            let got_scheme =
              qml_type_scheme_to_types_scheme
                typing_env' qml_scheme in
            (* Restore the previously active variables mapping, dropping the
               current one. *)
            release_variables_mapping () ;
            #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
            OManager.printf
              "@[<1>find_type for %s: Start spe of proto scheme@ %a@]@."
              (QmlAst.TypeIdent.to_string id_name)
              W_PrintTypes.pp_scheme proto_scheme ;
            #<End> ; (* <---------- END DEBUG *)
            let ((ty_prms1, row_prms1, col_prms1), t1) =
              W_SchemeGenAndInst.specialize2 ~deep: true proto_scheme in
            #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
            OManager.printf
             "@[<1>find_type for %s: End of spe of proto scheme, got type@ %a@]@."
              (QmlAst.TypeIdent.to_string id_name)
              W_PrintTypes.pp_simple_type t1 ;
            OManager.printf
              "@[<1>find_type for %s: Start spe of got scheme@ %a@]@."
              (QmlAst.TypeIdent.to_string id_name)
              W_PrintTypes.pp_scheme got_scheme ;
            #<End> ; (* <---------- END DEBUG *)
            let ((ty_prms2, row_prms2, col_prms2), t2) =
              W_SchemeGenAndInst.specialize2 ~deep: true got_scheme in
            #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
            OManager.printf
              "@[<1>find_type: End of spe of got scheme, got type@ %a@]@."
              W_PrintTypes.pp_simple_type t2 ;
            OManager.printf
              "@[<1>find_type for %s: finally unifying@\n%a@\nand@\n%a@]@."
              (QmlAst.TypeIdent.to_string id_name)
              W_PrintTypes.pp_simple_type t1 W_PrintTypes.pp_simple_type t2 ;
            #<End> ; (* <---------- END DEBUG *)
            !forward_ref__unify_simple_type typing_env' t1 t2 ;
            List.iter2
              (!forward_ref__unify_simple_type typing_env') ty_prms1 ty_prms2 ;
            (* TODO: Big question... What to do if row unification returned
               [false] ? This is the case where the old mechanism used
               [Failure "temporary failure"] we never got raised at this
               point. We really need to understand if this can arise and if
               so what does it mean and in which configuration. *)
            List.iter2
              (fun rt1 rt2 ->
                 ignore (!forward_ref__unify_row_type typing_env' rt1 rt2))
              row_prms1 row_prms2 ;
            List.iter2
              (!forward_ref__unify_column_type typing_env')
              col_prms1 col_prms2 ;
            #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
            OManager.printf "@[<1>find_type for %s: finally t1 =@ %a@]@."
              (QmlAst.TypeIdent.to_string id_name)
              W_PrintTypes.pp_simple_type t1 ;
            OManager.printf "@[<1>find_type for %s: finally t2 =@ %a@]@."
              (QmlAst.TypeIdent.to_string id_name)
              W_PrintTypes.pp_simple_type t2 ;
            #<End> ; (* <---------- END DEBUG *)
            (* Now, restore the previous binding level now we have finished the
               creation of the body of the type scheme. *)
            W_CoreTypes.end_definition () ;
            #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
            OManager.printf
              "Will build the final_scheme for %s Order of the variables:@."
              (QmlAst.TypeIdent.to_string id_name) ;
            List.iter
              (fun t ->
                 OManager.printf "> Var: %a@." W_PrintTypes.pp_simple_type t)
              ty_prms2 ;
            #<End> ; (* <---------- END DEBUG *)
            (* Attention: by induction, the form of a scheme is given from the
               information extracted from the environment, hence by the
               conversion from a QML scheme to a [type_scheme]. Especially, the
               order of the parameters of the scheme, which is significant when
               communicating with the QML side, this order must be preserved
               while creating the [type_scheme]. To enforce this order, we must
               make the generalization by providing the parameters in *this*
               order.
               And because *this* order, as I said, comes from the QML side and
               inductively from conversion from QML to [type_scheme], and *not*
               from the prototype scheme we introduced temporarily in the
               environment, we must call the generalization with the instance
               stuff we obtained from [got_scheme] (i.e. the scheme coming from
               the QML conversion). *)
            let final_scheme =
              W_SchemeGenAndInst.generalize2
                ~extra_variables: (ty_prms2, row_prms2, col_prms2) t2 in
            #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
            OManager.printf "@[<1>Built the final_scheme for %s:@ %a@."
              (QmlAst.TypeIdent.to_string id_name)
              W_PrintTypes.pp_scheme final_scheme ;
            #<End> ; (* <---------- END DEBUG *)
            (* Now we have seen and reconstruct this scheme, let's memoize
               it. *)
            toplevel_tydefs_schemes_env_memo :=
              QmlAst.TypeIdentMap.add
                id_name final_scheme !toplevel_tydefs_schemes_env_memo ;
            final_scheme
          with any_killer ->
            (* We then must restore the binding we had before performing the
               search in the environment and same thing for the variables
               mapping. *)
            W_CoreTypes.restore_binding_level_from_backup
              backuped_binding_level ;
            (* Same thing for the variables mapping that may be
               unbalanced. *)
            restore_variables_mappings_stack_from_backup
              backuped_variables_mappings_stack ;
            raise any_killer
    )
  )



(* ************************************************************************** *)
(** {b Descr}: Function to find the type of an identifier in accordance to
    the environment. The type scheme bound to the identifier is looked-up in
    the environment. If the identifier is bound, then an instance of this type
    scheme is returned.
    In addition, since getting the type of an identifier implies a type scheme
    instantiation and because this instantiation must be recorded in the
    typechecker's private annotation map structure, we also return the type
    scheme from which we obtained the type, under a form suitable to be
    inserted in an annotation map. This way, in one shot we get both the type
    for the inference and the pseudo-scheme for the annotation map.
    @raise Not_found
    @raise QmlTyperException.Exception
    @raise Unification_simple_type_conflict
    @raise Unification_binding_level_conflict
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let get_ident_type_and_annotmap_scheme id_name typing_env =
  let rec find_in_local_or_toplevel_memo_env = function
    | [] ->
        (* Have a look in the toplevel environment memoized stuff to avoid
           reconstruction of the scheme from scratch. *)
        IdentMap.find id_name !toplevel_valdefs_schemes_env_memo
    | (n, t) :: q -> if Ident.equal n id_name then t
      else find_in_local_or_toplevel_memo_env q in

  (* Effective body of the [get_ident_type_and_annotmap_scheme] function.
     We first try to lookup the identifier in the local environment, i.e. the
     one where we record all the identifiers occurring in let-definitions that
     are sub-expressions encountered during the call to the typechecker. *)
  try
    (* First, recover the scheme bound to the identifier. *)
    let sch = find_in_local_or_toplevel_memo_env typing_env.ty_env_local in
    #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
        OManager.printf
          "@[<1>Found ident %s in local env: %a@]@."
          (Ident.to_string id_name)
          W_PrintTypes.pp_scheme sch ;
    #<End> ; (* <---------- END DEBUG *)
    let ty = W_SchemeGenAndInst.specialize sch in
    #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
        OManager.printf
          "@[<1>Type of found ident %s in local env: %a@]@."
          (Ident.to_string id_name)
          W_PrintTypes.pp_simple_type ty ;
    #<End> ; (* <---------- END DEBUG *)
    (* We must now convert the scheme into a form suitable to annotation
       maps. *)
    let annotmap_sch = W_PublicExport.type_scheme_to_annotmap_type_scheme sch in
    (ty, annotmap_sch)
  with Not_found -> (
    (* Get a fresh new variable mapping to make so variables created during
       importation of the type of the identifier from the environment won't
       pollute the one we currently known from explicit type constraints
       expressions. Moreover, since the type we will import belongs to the
       environment, there is no reason for it to contain type variables
       introduced by explicit type constraints expressions that currently "in
       the scope". *)
    new_empty_variables_mapping () ;
    (* Bad, the identifier was not found in the local environment. We then must
       lookup it inside the "external" environment, the one we were provided
       from the QML side when the typechecker was called. In this environment
       there may have some toplevel-defined identifier among those the one we
       are looking for. *)
    let opt_qml_scheme =
      QmlTypes.Env.Ident.find_opt id_name typing_env.ty_env_qml_global in
    match opt_qml_scheme with
    | None ->
        (* The identifier was also not found in the global environment, hence
           it is really unbound. *)
        raise Not_found
    | Some qml_scheme ->
        (* We found a binding for this identifier in the global environment.
           Then we must transform the obtained [QmlTypes.typescheme] into a
           type scheme in the format manipulated by the typechecker. *)
        let sch =
          qml_type_scheme_to_types_scheme
            typing_env qml_scheme in
        #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
        OManager.printf
          "@[<1>Found ident %s in QML env: %a@]@."
          (Ident.to_string id_name)
          W_PrintTypes.pp_scheme sch ;
        #<End> ; (* <---------- END DEBUG *)
        (* Now, take an instance of the identifier's scheme. *)
        let ty = W_SchemeGenAndInst.specialize sch in
        #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
        OManager.printf
          "@[<1>Type of found ident %s in QML env: %a@]@."
          (Ident.to_string id_name)
          W_PrintTypes.pp_simple_type ty ;
        #<End> ; (* <---------- END DEBUG *)
        (* Recover the QML variables of the scheme in order to deal with the
           annotations map just below. *)
        let (qml_quantification, _, _) =
          QmlGenericScheme.export_unsafe qml_scheme in
        (* Now, we must build the representation of the scheme we instantiated
           into a form suitable to annotation maps. Attention, this scheme must
           be exactly the one we instantiated and that was recorded in the
           environment.
           From the QML scheme, we already obtained the quantification.
           For the body, we must *not* use the [simple_type] obtained by
           specialization because its variables are fresh and then its structure
           doesn't correspond to the body of the scheme of the environment.
           However, to help us, we have the suitable body from the importation
           we did to construct [sch] just above. So we simply put the type
           that is the body of [sch] as body of the scheme for the annotation
           map. *)
        let annotmap_sch =
          QmlGenericScheme.import qml_quantification sch.W_Algebra.body () in
        release_variables_mapping () ;
        (* Now we have seen and reconstruct this scheme, let's memoize it. *)
        toplevel_valdefs_schemes_env_memo :=
          IdentMap.add
            id_name sch !toplevel_valdefs_schemes_env_memo ;
        (ty, annotmap_sch)
  )
