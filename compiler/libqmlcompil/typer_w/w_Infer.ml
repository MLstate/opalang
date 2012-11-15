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
   @author Fran�ois Pessaux
*)



(* ************************************************************************** *)
(** {b Descr}: Infers the type of a constant expression and returns this
    type.                                                                     *)
(* ************************************************************************** *)
let infer_constant_type = function
  | QmlAst.Int _ -> W_CoreTypes.type_int ()
  | QmlAst.Float _ -> W_CoreTypes.type_float ()
  | QmlAst.String _ -> W_CoreTypes.type_string ()



(* ************************************************************************** *)
(** {b Descr}: Add an information of type scheme creation (corresponds to a
    generalization point) to the annotation map.
    ATTENTION: Explicit instantiation pass requires to have such information
    **only** if the scheme really has variables, i.e. **not** if it is a
    trivial type  scheme.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let annotmap_add_scheme_creation key scheme annotmap =
  (* Ensure that there exists variables before adding the information in the
     annotations maps. Otherwise, leave the annotations map untouched. *)
  #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
  OManager.printf "annotmap_add_scheme_creation on key %s@]@."
    (Annot.to_string key)
  #<End> ;
  let (quantif, _, _) = QmlGenericScheme.export_unsafe scheme in
  if QmlTypeVars.FreeVars.is_empty quantif then (
    #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
    OManager.printf "annotmap_add_scheme_creation: trivial scheme: no addition.@." ;
    #<End> ;
    annotmap
   )
  else (
    #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
    OManager.printf "annotmap_add_scheme_creation: non-trivial scheme: addition.@." ;
    #<End> ;
    QmlAnnotMap.add_tsc key scheme annotmap
   )


(* ************************************************************************** *)
(** {b Descr}: Add an information of type scheme deconstruction (corresponds to
    an instantiation --specialization-- point) to the annotation map.
    ATTENTION: Explicit instantiation pass requires to have such information
    **only** if the scheme really has variables, i.e. **not** if it is a
    trivial type  scheme.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let annotmap_add_scheme_elimination key scheme annotmap =
  (* Ensure that there exists variables before adding the information in the
     annotations maps. Otherwise, leave the annotations map untouched. *)
  #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
  OManager.printf "annotmap_add_scheme_elimination on key %s@]@."
    (Annot.to_string key)
  #<End> ;
  let (quantif, _, _) = QmlGenericScheme.export_unsafe scheme in
  if  QmlTypeVars.FreeVars.is_empty quantif then (
    #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
    OManager.printf "annotmap_add_scheme_elimination: trivial scheme: no addition.@." ;
    #<End> ;
    annotmap
   )
  else (
    #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
    OManager.printf "annotmap_add_scheme_elimination: non-trivial scheme: addition.@." ;
    #<End> ;
    QmlAnnotMap.add_tsc_inst key scheme annotmap
   )



(* ************************************************************************** *)
(** {b Descr}: Function used to factorize the final processing of inference for
    an expression, processing that is always done. This function has 2 jobs:
    1 - If the type inferred for the expression is a type-forall, it
        automatically instantiates it, hence removing the "forall". Otherwise,
        it leaves the type unchanged.
        If an automatic instantiation has been done, then the annotation map
        is updated by adding the information that a type scheme was instantiated
        at the point of the current expression.
    2 - It always enrich the annotations map by setting inside the inferred type
        for the current expression.

    Finally, it returns the type of the expression, automatically instantiated
    if needed, and the annotations map updated. Such a return value is suitable
    to be the return value of the main function [infer_expr_type].
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let perform_infer_expr_type_postlude expr annotmap infered_ty =
  #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
  OManager.printf "perform_infer_expr_type_postlude start@." ;
  #<End> ;
  (* We still must automatically instantiate the type if it is a type forall. *)
  let (final_ty, was_instantiated) =
    W_SchemeGenAndInst.automatically_instantiate_if_forall infered_ty in
  #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
  OManager.printf
    "perform_infer_expr_type_postlude: forall auto inst: %b@."
    (was_instantiated <> None) ;
  #<End> ;
  (* If the type was a type forall, then it was implicitly instantiated, and in
     this case we must extend the annotation map with a "scheme has been
     instantiated" information. *)
  let annotmap' =
    (match was_instantiated with
     | None -> annotmap
     | Some sch ->
         let annotmap_scheme =
           W_PublicExport.type_scheme_to_annotmap_type_scheme sch in
         annotmap_add_scheme_elimination
           (QmlAst.QAnnot.expr expr) annotmap_scheme annotmap) in
  (* In any case, we must extend the annotation map with a "expression has type"
     information. *)
  let annotmap'' =
    QmlAnnotMap.add_ty (QmlAst.QAnnot.expr expr) final_ty annotmap' in
  #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
  OManager.printf "perform_infer_expr_type_postlude stop@." ;
  OManager.printf "On key %s, registered type: %a@."
    (Annot.to_string (QmlAst.QAnnot.expr expr))
    W_PrintTypes.pp_simple_type final_ty ;
  #<End> ;
  (* Finally, return the possibly automatically instantiated type and the
     updated annotation map. *)
  (final_ty, annotmap'')



(* ************************************************************************** *)
(** {b Descr}: Descends on the expressions of bodies of recursive
    let-definitions in order to update all the annotations in the map that
    correspond to points of instantiation of the scheme of one of the
    recursively bound identifiers.
    In effect, while typechecking the bodies of definitions, we were still
    working with temporary type schemes inserted in the environment to make
    identifiers known before we finally have completely inferred their type.
    So any registered information at this time was dealing with the temporary
    type schemes. Once the schemes of the identifiers are finalized, some
    variables inside may be instantiated, changed and so on, so we must
    correct the schemes previously registered to mention their real form
    instead of their temporary form.
    {b Args}:
     - [initial_annotmap] : The annotation map in which to correct the
       informations of scheme instantiation of the recursively bound
       identifiers (i.e. information registered at recursive calls points).
     - [typing_env] : The typing environment in which recursively bound
       identifiers (i.e. identifiers bound by the recursive let-definition(s))
       must be present with their effective final type scheme. In other words,
       this must be the final environment returned for the typechecking of a
       let-rec.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let update_recursive_calls_annotations typing_env initial_annotmap bindings =
  (* First, record all the recursively bound identifiers. *)
  let rec_bound_idents = List.map fst bindings in

  (* The local walker that looks in for each expression's annotation key if in
     the annotations map there is mention of a scheme instantiation information
     related to a recursive call, and that returns the updated annotations map
     if needed. *)
  let walk annotmap e =
    match e with
    | QmlAst.Ident (_, id) when List.mem id rec_bound_idents ->
        (* First remove old scheme instantiation information in case there was
           some previously inserted. *)
        let annotmap' =
          QmlAnnotMap.remove_tsc_inst (QmlAst.QAnnot.expr e) annotmap in
        (* Lookup should never fail since we are working with the final
           environment in which recursively bound identifiers have been
           finally inserted. *)
        let (_, ident_annotmap_scheme) =
          W_TypingEnv.get_ident_type_and_annotmap_scheme id typing_env in
        (* Now add the correct scheme instantiation information to the
           annotations map not anymore containing the old info. *)
        annotmap_add_scheme_elimination
          (QmlAst.QAnnot.expr e) ident_annotmap_scheme annotmap'
    | _ -> annotmap
       in
  (* Just to have a function ignoring the name bound in the couple representing
     a binding. *)
  let func annotmap (_, e) = QmlAstWalk.Expr.fold walk annotmap e in

  (* And now apply the walker. *)
  List.fold_left func initial_annotmap bindings



(* ************************************************************************** *)
(** {b Descr}: Replaces column variables in a [simple_type] by fresh ones.
    This possibly makes a copy of the type, but in any case preserve physical
    identity of type and row variables and columns variables that are not
    replaced (those bound by types forall).
    This function is used to typecheck [Pat_as] in order to avoid in a
    pattern aliased to have the alias having the type of the unifications of
    all the patterns.
    This function, being used on types built from patterns structures, assumes
    the types it receives are not cyclic. Hence, it doesn't perform any cycle
    check.
    {b Visibility} : Not exported outside this module.                        *)
(* ************************************************************************** *)
let freshen_column_var initial_ty =
  (* Association list recording the row vars already encountered and by which
     fresh variables they are replaced. *)
  let seen_row_vars = ref [] in

  let rec freshen_in_simple_type ty =
    let ty = W_CoreTypes.simple_type_repr ty in
    match ty.W_Algebra.sty_desc with
    | W_Algebra.SType_var _ -> ty
    | W_Algebra.SType_arrow (args_tys, res_ty) ->
        let args_tys' = List.map freshen_in_simple_type args_tys in
        let res_ty' = freshen_in_simple_type res_ty in
        { W_Algebra.sty_desc = W_Algebra.SType_arrow (args_tys', res_ty') ;
          W_Algebra.sty_link = None ;
          W_Algebra.sty_mark = W_Algebra.TM_not_seen }
    | W_Algebra.SType_named nst ->
        let nst_args' =
          List.map freshen_in_simple_type nst.W_Algebra.nst_args in
        let nst_unwinded' =
          (match nst.W_Algebra.nst_unwinded with
           | None -> None
           | Some t -> Some (freshen_in_simple_type t)) in
        let nst' =
          { nst with
              W_Algebra.nst_args = nst_args' ;
              W_Algebra.nst_unwinded = nst_unwinded' } in
        { W_Algebra.sty_desc = W_Algebra.SType_named nst' ;
          W_Algebra.sty_link = None ;
          W_Algebra.sty_mark = W_Algebra.TM_not_seen }
    | W_Algebra.SType_sum_of_records col_type ->
        { W_Algebra.sty_desc =
            W_Algebra.SType_sum_of_records (freshen_in_column_type col_type) ;
          W_Algebra.sty_link = None ;
          W_Algebra.sty_mark = W_Algebra.TM_not_seen }
    | W_Algebra.SType_forall sch ->
        (* To prevent refresh of quantified variables, we pre-map them onto
           themselves. *)
        seen_row_vars :=
          List.fold_left
            (fun accu v -> (v, v) :: accu)
            !seen_row_vars
            sch.W_Algebra.column_parameters ;
        let body' = freshen_in_simple_type sch.W_Algebra.body in
        let sch' = { sch with W_Algebra.body = body' } in
        { W_Algebra.sty_desc = W_Algebra.SType_forall sch' ;
          W_Algebra.sty_link = None ;
          W_Algebra.sty_mark = W_Algebra.TM_not_seen }


  and freshen_in_column_type col_ty =
    let (cases, col_ending) =
      (W_CoreTypes.column_type_repr col_ty).W_Algebra.ct_value in
    let cases' = List.map freshen_in_row_type cases in
    let col_ending' =
      (match col_ending with
       | W_Algebra.Closed_column -> col_ending
       | W_Algebra.Var_column v ->
           let v' =
             (try List.assq v !seen_row_vars
              with Not_found ->
                let new_var = W_CoreTypes.__column_variable () in
                seen_row_vars := (v, new_var) :: !seen_row_vars ;
                new_var) in
           W_Algebra.Var_column v') in
    { W_Algebra.ct_value = (cases', col_ending') }


  and freshen_in_row_type row_type =
    let (fields, row_ending) =
      (W_CoreTypes.row_type_repr row_type).W_Algebra.rt_value in
    let fields' =
      List.map (fun (n, t) -> (n, (freshen_in_simple_type t))) fields in
    { W_Algebra.rt_value = (fields', row_ending) } in

  (* Effective body of the function [freshen_column_var]. *)
  freshen_in_simple_type initial_ty



(* ************************************************************************** *)
(** {b Descr}: Function that typechecks a record pattern.
    In any case, the inferred type is a record plugged in an *opened* column.
    If the pattern is explicitly ended by a "..." then the row of the record
    will be opened, otherwise it will be closed.
    This function returns both the type inferred for the pattern (i.e. the
    record type made of all the fields patterns) and the environment extension
    induced by all the sub-patterns found in the fields specifications of the
    whole pattern.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let rec infer_record_pattern_type typing_env fields row_ending =
  let (found_catchalls, reved_found_fields, env_extens, annotmap) =
     (* Attention, folding left make fields processed in their apparition
        order, however, this builds the list of type fields in reverse order.
        We must reverse it after. *)
     List.fold_left
       (fun (accu_catchalls, accu_fields, accu_bindings, accu_annotmap)
            (field_name, field_pat) ->
         (* Infer the type of the sub-pattern. *)
         let ((field_found_catchalls, field_pat_ty, field_env_extens),
              field_annotmap) =
           infer_pattern_type typing_env field_pat in
         let accu_catchalls' = field_found_catchalls @ accu_catchalls in
         let accu_fields' = (field_name, field_pat_ty) :: accu_fields in
         let accu_bindings' = field_env_extens @ accu_bindings in
         let accu_annotmap' = QmlAnnotMap.merge field_annotmap accu_annotmap in
         (accu_catchalls', accu_fields', accu_bindings', accu_annotmap'))
     ([], [], [], QmlAnnotMap.empty)
     fields in
  let found_fields = List.rev reved_found_fields in
  (* Process the row ending. *)
  let row_is_opened =
     (match row_ending with
     | `closed -> false
     | `open_ -> true) in
  (* The synthesized type is plugged in a **opened** column.
     The only difference is on the opening/closing of the row. *)
  let ty = W_CoreTypes.type_of_record_pattern ~row_is_opened found_fields in
  (* The annotmap modification to store the type of the pattern will be done
     by the caller. At the present point, we don't deal with the pattern itself
     but with its list of fields and its ending. So, we just return the
     annotmap induced by processing the fields patterns. *)
  ((found_catchalls, ty, env_extens), annotmap)



(* ************************************************************************** *)
(** {b Descr}: Infers the type of a pattern, ignoring the right-side expression
    related to this pattern. In particular, record-like patterns are inferred
    as having ... a record type, i.e. possibly having a row-variable if the
    pattern ends by an ellipsis, and plugged in a closed sum.
    This function returns both the type inferred for the pattern and an
    environment extension (to append in order to typecheck later the right-side
    part of the related pattern) in which variables of the pattern are bound to
    their types (that are sub-terms of the inferred type pattern).
    This function returns as first component the list of types bound to
    catchall patterns. This will be useful later, to close sum types except
    those bound to parts of patters hosting a catchall case.
    @raise W_InferErrors.Infer_detailed_unification_conflict
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and infer_pattern_type typing_env pattern =
  let loc = QmlAst.Label.pat pattern in
  match pattern with
  | QmlAst.PatAny _ ->
     (* Pattern is unconstrained, to has a type variable. Since no variable
        is bound, we return an empty environment extension. *)
     let pat_ty = W_CoreTypes.type_variable () in
     let annotmap =
       QmlAnnotMap.add_ty
         (QmlAst.QAnnot.pat pattern) pat_ty QmlAnnotMap.empty in
     (([pat_ty], pat_ty, []), annotmap)
  | QmlAst.PatVar (_, var_name) ->
      (* Pattern is unconstrained, to has a type variable. The name of the
         pattern variable get bound to this type variable in the returned
         environment extension. *)
      let pat_ty = W_CoreTypes.type_variable () in
      let pat_scheme = W_SchemeGenAndInst.trivial_scheme pat_ty in
      let annotmap =
        QmlAnnotMap.add_ty
          (QmlAst.QAnnot.pat pattern) pat_ty QmlAnnotMap.empty in
      (([pat_ty], pat_ty, [(var_name, pat_scheme)]), annotmap)
  | QmlAst.PatAs (_, pat, ident) ->
      (* Infer the aliased pattern's type. This is also the type of the whole
         pattern. *)
      let ((found_catchalls, pre_alias_ty, found_bindings),
           alias_pat_annotmap) =
        infer_pattern_type typing_env pat in
      (* We now need to break the link between column variables in the inferred
         type and the type scheme we insert in the environment for the alias.
         In effect, that's a bit like if the alias was let-bound in the right
         side part of the pattern.
         To do so, we apply [freshen_column_var] on the type inferred for the
         pattern. This has the effect of replacing column variables (except
         those bound in types forall) by fresh ones. Because we do this at a
         binding level increased, when we will generalize this type to bind
         the alias in the environment, the fresh column variables will be in
         fact generalized. *)
      W_CoreTypes.begin_definition () ;
      (* Replace column variables by fresh ones. *)
      let alias_ty = freshen_column_var pre_alias_ty in
      W_CoreTypes.end_definition () ;
      (* And now generalize the type to bind to the alias in the typing
         environment. *)
      let alias_scheme =
        W_SchemeGenAndInst.generalize ~forbid_non_gen_vars: false alias_ty in
      (* Attention, *type* (and not scheme) of the pattern and type to put in
         the annotation map must still be the inferred type and not the
         modified type otherwise, the presence of generalized variables inside
         will break unification through [lowerize_level_in_column]. *)
      let annotmap =
        QmlAnnotMap.add_ty
          (QmlAst.QAnnot.pat pattern) pre_alias_ty alias_pat_annotmap in
      let annotmap_alias_scheme =
        W_PublicExport.type_scheme_to_annotmap_type_scheme alias_scheme in
      let annotmap2 =
        annotmap_add_scheme_creation
          (QmlAst.QAnnot.pat pattern) annotmap_alias_scheme annotmap in
      (* Now, record the binding of the sub-pattern to the identifier. *)
      ((found_catchalls, pre_alias_ty, (ident, alias_scheme) :: found_bindings),
       annotmap2)
  | QmlAst.PatConst (_, const_expr) ->
      let ty = infer_constant_type const_expr in
      let annotmap =
        QmlAnnotMap.add_ty (QmlAst.QAnnot.pat pattern) ty QmlAnnotMap.empty in
      W_TypeInfo.add_loc_object ty.W_Algebra.sty_desc loc ;
      (* Since no variable is bound, we return an empty environment
         extension. *)
      (([(* Not a catchall. *)], ty, []), annotmap)
  | QmlAst.PatRecord (_, fields, row_ending) ->
      let ((found_catchall, ty, found_bindings), annotmap) =
        infer_record_pattern_type typing_env fields row_ending in
      let annotmap' =
        QmlAnnotMap.add_ty (QmlAst.QAnnot.pat pattern) ty annotmap in
      W_TypeInfo.add_loc_object ty.W_Algebra.sty_desc loc ;
      ((found_catchall, ty, found_bindings), annotmap')
  | QmlAst.PatCoerce (_, pat, coercing_ty_expr) ->
      (* First, typecheck the sub-pattern. *)
      let ((found_catchalls, pat_ty, found_bindings), sub_pat_annotmap) =
        infer_pattern_type typing_env pat in
      (* Recover the QML purified type AST in which type names are "processed",
         sum-sugar are flattened and various magic things are done. Without
         doing this, warncoerce pass fails, it gets possible to redefine type
         names with different bodies and confuse them because no stamp is
         added.
         This magic stuff is transparently done for type definitions by the guy
         who handle ... type definitions.
         For type coercions, this must be done by hand. So, do it ! *)
      let coercing_ty_expr =
        (try
          fst
            (QmlTypes.type_of_type
               typing_env.W_TypingEnv.ty_env_qml_global coercing_ty_expr)
         with QmlTyperException.Exception (_, error) ->
           (* Just set a correct source position. *)
           let err_loc =
             QmlTyperException.loc_make
               (`Pat_loc pattern) QmlTyperException.loc_set_empty in
           (* Re-raise the exception, but with the more precise source
              location. *)
           raise (QmlTyperException.Exception (err_loc, error))) in
      (* Get the type represented by the constraint, i.e. convert the type
         expression into an internal typechecker type by following named types
         definitions. *)
      let coercing_ty =
        (try
          W_TypingEnv.qml_type_to_simple_type
            typing_env coercing_ty_expr ~is_type_annotation: true
        with W_TypingEnv.Importing_qml_abstract_ty ->
          (* The type annotation contained "external" which is a non-sense. *)
          let err_loc =
            QmlTyperException.loc_make
              (`Pat_loc pattern) QmlTyperException.loc_set_empty in
          raise
            (QmlTyperException.Exception
               (err_loc,
                QmlTyperException.InvalidType
                  (coercing_ty_expr, `abstract_in_ty_annotation)))) in
      W_TypeInfo.addrec_loc_object coercing_ty.W_Algebra.sty_desc loc ;
      (* Force unification between the type inferred for the pattern and the
         coercing type. *)
      (try W_Unify.unify_simple_type_in_coercion typing_env pat_ty coercing_ty
       with W_Unify.Unification_simple_type_conflict (err_t1, err_t2, detail) ->
         raise
           (W_InferErrors.Infer_detailled_unification_conflict
              (W_InferErrors.UCC_pattern_coerce (pattern, pat_ty, coercing_ty),
               err_t1, err_t2, detail))) ;
      (* The finally retained type is the coercing one, not the one used to
         ensure that the canonical representation of the constraint is
         unifiable with the type inferred for the constrained pattern. *)
      let annotmap =
        QmlAnnotMap.add_ty
          (QmlAst.QAnnot.pat pattern) coercing_ty sub_pat_annotmap in
      ((found_catchalls, coercing_ty, found_bindings), annotmap)



(* ************************************************************************** *)
(** {b Descr}: Infers the type of an expression, returns this type and
    an annotation map recording type information related to the expression.
    @raise Infer_detailled_unification_conflict
    @raise QmlTyperException.Exception
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let rec infer_expr_type ~bypass_typer typing_env original_expr =
  let loc = QmlAst.Label.expr original_expr in
  match original_expr with
  | QmlAst.Const (_, c) ->
      let cst_ty = infer_constant_type c in
      W_TypeInfo.add_loc_object cst_ty.W_Algebra.sty_desc loc ;
      perform_infer_expr_type_postlude
        original_expr W_AnnotMap.empty_annotmap cst_ty
  | QmlAst.Ident (_, id) -> (
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Ident start for %s@." (Ident.to_string id) ;
      #<End> ; (* <---------- END DEBUG *)
      try
        let (ident_ty, ident_annotmap_scheme) =
          W_TypingEnv.get_ident_type_and_annotmap_scheme id typing_env in
        W_TypeInfo.addrec_env_object ident_ty.W_Algebra.sty_desc (id, loc) ;
        (* Since this inference case implies a scheme instantiation, we must
           register it in the annotation map. *)
        let annotmap =
          annotmap_add_scheme_elimination
            (QmlAst.QAnnot.expr original_expr) ident_annotmap_scheme
            W_AnnotMap.empty_annotmap in
        #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
        OManager.printf "Ident nearly stop@." ;
        #<End> ; (* <---------- END DEBUG *)
        perform_infer_expr_type_postlude original_expr annotmap ident_ty
      with Not_found ->
        (* The identifier was not found in the typing environment. *)
        let err_loc =
          QmlTyperException.loc_make
            (`Expr_loc original_expr) QmlTyperException.loc_set_empty in
        raise
          (QmlTyperException.Exception
             (err_loc, (QmlTyperException.IdentifierNotFound (id, []))))
    )
  | QmlAst.LetIn (_, def_bindings, in_expr) ->
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "LetIn start@." ;
      #<End> ; (* <---------- END DEBUG *)
      (* The definition is not recursive. *)
      let (new_env, annotmap_from_bindings) =
        infer_nrec_let_definition_type
          ~lifted_module_field: false ~bypass_typer typing_env def_bindings in
      let (in_expr_ty, in_expr_annotmap) =
        infer_expr_type ~bypass_typer new_env in_expr in
      (* Merge the 2 annotation maps. *)
      let final_annotmap =
        QmlAnnotMap.merge in_expr_annotmap annotmap_from_bindings in
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "LetIn nearly stop@." ;
      #<End> ; (* <---------- END DEBUG *)
      (* Finally, return the type of the "in-part" of the let-definition and
         the annotation map cumulating all the encountered annotations. *)
      perform_infer_expr_type_postlude original_expr final_annotmap in_expr_ty
  | QmlAst.LetRecIn (_, def_bindings, in_expr) ->
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "LetRecIn start@." ;
      #<End> ; (* <---------- END DEBUG *)
      (* The definition is not recursive. *)
      let (new_env, annotmap_from_bindings) =
        infer_rec_let_definition_type
          ~lifted_module_field: false ~bypass_typer typing_env def_bindings in
      let (in_expr_ty, in_expr_annotmap) =
        infer_expr_type ~bypass_typer new_env in_expr in
      (* Merge the 2 annotation maps. *)
      let final_annotmap =
        QmlAnnotMap.merge in_expr_annotmap annotmap_from_bindings in
      (* Finally, return the type of the "in-part" of the let-definition and
         the annotation map cumulating all the encountered annotations. *)
      perform_infer_expr_type_postlude original_expr final_annotmap in_expr_ty
  | QmlAst.Lambda (_, params_names, body) ->
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Lambda start@." ;
      #<End> ; (* <---------- END DEBUG *)
      (* Create a fresh type variable for each argument of the function. *)
      let params_tys =
        List.map (fun _ -> W_CoreTypes.type_variable ()) params_names in
      (* Extend the current environment with each argument name bound to a
         trivial type scheme (no polymorphic recursion) whose body is the fresh
         type variable created for the argument. *)
      let extended_typing_env =
        List.fold_left2
          (fun accu_env param_name param_ty ->
            let trivial_scheme = W_SchemeGenAndInst.trivial_scheme param_ty in
            (* During inference's recursive descent, we always extend the
               "local" (i.e. private to the typechecker) typing environment. *)
            let accu_env' = {
              accu_env with
                W_TypingEnv.ty_env_local =
                  (param_name, trivial_scheme) ::
                    accu_env.W_TypingEnv.ty_env_local } in
            accu_env')
          typing_env params_names params_tys in
      let (body_ty, body_annotmap) =
        infer_expr_type ~bypass_typer extended_typing_env body in
      let fun_ty = W_CoreTypes.type_arrow params_tys body_ty in
      W_TypeInfo.addrec_loc_object fun_ty.W_Algebra.sty_desc loc ;
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Lambda nearly stop@." ;
      #<End> ; (* <---------- END DEBUG *)
      perform_infer_expr_type_postlude original_expr body_annotmap fun_ty
  | QmlAst.Apply (_, fun_part_expr, args_exprs) ->
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Apply start@." ;
      #<End> ; (* <---------- END DEBUG *)
      (* First, infer the type of the functional part of the application. *)
      let (fun_part_ty, fun_part_annotmap) =
        infer_expr_type ~bypass_typer typing_env fun_part_expr in
      (* Now, infer the type of each argument applied. *)
      let (reved_args_tys, args_annotmap) =
        List.fold_left
          (* Really left, to process arguments in their apparition order and
             hence merge the annotation maps in the right order. However, this
             builds the list of types in reverse order. So, we reverse it
             afterwards. *)
          (fun (args_tys_accu, annotmap_accu) arg ->
             let (arg_ty, arg_annotmap) =
               infer_expr_type ~bypass_typer typing_env arg in
             (* Accumulate the new argument type. *)
             let args_tys_accu' = arg_ty :: args_tys_accu in
              (* Accumulate the annotations of the map. *)
             let annotmap_accu' =
               QmlAnnotMap.merge annotmap_accu arg_annotmap in
             (args_tys_accu', annotmap_accu'))
          ([], fun_part_annotmap)
          args_exprs in
      (* Put back the list of arguments types in the correct order. *)
      let args_tys = List.rev reved_args_tys in
      (* Now, create a fresh variable to get a handle on the type of the
         application result. *)
      let ty_app_result = W_CoreTypes.type_variable () in
      (* Finally, we unify the type of the function with a functional type we
         create, having the above variable in positive part (to recover this
         type as the result of the application) and the types inferred for the
         arguments in negative part. *)
      let tmp_arrow_type = W_CoreTypes.type_arrow args_tys ty_app_result in
      W_TypeInfo.add_loc_object tmp_arrow_type.W_Algebra.sty_desc loc ;
      (try W_Unify.unify_simple_type typing_env fun_part_ty tmp_arrow_type
       with W_Unify.Unification_simple_type_conflict (err_t1, err_t2, detail) ->
         raise
           (W_InferErrors.Infer_detailled_unification_conflict
              (W_InferErrors.UCC_apply
                 (original_expr, fun_part_ty, tmp_arrow_type),
               err_t1, err_t2, detail))) ;
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Apply nearly stop@." ;
      #<End> ; (* <---------- END DEBUG *)
      perform_infer_expr_type_postlude original_expr args_annotmap ty_app_result
  | QmlAst.Match (_, matched_e, cases_list) -> (
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Match start@." ;
      #<End> ; (* <---------- END DEBUG *)
      (* Typecheck the matched expression. *)
      let (matched_ty, matched_annotmap) =
       infer_expr_type ~bypass_typer typing_env matched_e in
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Matched expression:@ %a@."
        W_PrintTypes.pp_simple_type matched_ty ;
      #<End> ; (* <---------- END DEBUG *)
      (* Now, get the individual type of each pattern. By the way, accumulate
         annotations of the maps. *)
      let (revd_patts_typing_info, patts_annotmap) =
        List.fold_left
          (* Really left, to process patterns in their apparition order and
             accumulate annotations in the right order. But this builds the
             result list in reverse order, so we will put it in the right order
             later. *)
          (fun (typing_info_accu, annotmap_accu) (pat, _) ->
             let (typing_info, pat_annotmap) =
               infer_pattern_type typing_env pat in
             let typing_info_accu' = typing_info :: typing_info_accu in
             (* Accumulate the annotations of the map. *)
             let annotmap_accu' =
               QmlAnnotMap.merge annotmap_accu pat_annotmap in
             (typing_info_accu', annotmap_accu'))
          (* Start with annotation map got from the matched expression. *)
          ([], matched_annotmap)
          cases_list in
      (* Recover the various interesting stuff from the pattern inference. *)
      let (found_catchalls, tys_of_pats, envs_extens) =
        BaseList.split3 (List.rev revd_patts_typing_info) in
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      List.iter
        (fun t ->
           OManager.printf "@[<1>Inferred pattern type: %a@]@."
             W_PrintTypes.pp_simple_type t)
        tys_of_pats ;
      #<End> ; (* <---------- END DEBUG *)
      (* Now, process the individual patterns types to make on unique final
         type and unify also with the type of the matched expression. This
         handles the opening of sums that are "under" catchall patterns. *)
      let _ty_for_patterns =
        W_PatternsProcessing.merge_patterns_types
          typing_env (List.flatten found_catchalls)
          ~pat_match_expr: original_expr  ~matched_expr_ty: matched_ty
          ~patterns_types: tys_of_pats in
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf
        "@[<1>Global pattern type and matched expression:@ %a@]@."
        W_PrintTypes.pp_simple_type _ty_for_patterns ;
      #<End> ; (* <---------- END DEBUG *)
      (* Now typecheck the right-side part of each branch of the matching in
         the extended environment. We first create a temporary type variable
         that will serve to accumulate by side effects of the unification the
         constraints induced by all the right-parts. *)
      let right_parts_ty = W_CoreTypes.type_variable () in
      W_TypeInfo.add_loc_object right_parts_ty.W_Algebra.sty_desc loc ;
      let final_annot_map =
        List.fold_left2
          (fun annotmap_accu (_, branch_expr) env_extens ->
             (* Locally extend the environment with the bindings induced by the
                pattern. *)
             let typing_env' = {
               typing_env with
                 W_TypingEnv.ty_env_local =
                   env_extens @ typing_env.W_TypingEnv.ty_env_local } in
             (* Infer the type of the branch. *)
             #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
             OManager.printf "Match ready to infer right-expr@." ;
             #<End> ; (* <---------- END DEBUG *)
             let (branch_ty, branch_annotmap) =
               infer_expr_type ~bypass_typer typing_env' branch_expr in
             #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
             OManager.printf "@[<1>Match right-expr done:@ %a@]@."
               W_PrintTypes.pp_simple_type branch_ty ;
             #<End> ; (* <---------- END DEBUG *)
             (* Unify the found type with the one found for the other
                branches. *)
             (try W_Unify.unify_simple_type typing_env right_parts_ty branch_ty
              with
              | W_Unify.Unification_simple_type_conflict
                    (err_t1, err_t2, detail) ->
                  raise
                    (W_InferErrors.Infer_detailled_unification_conflict
                       (W_InferErrors.UCC_match_ty_right_parts_vs_ty_branch
                          (branch_expr, right_parts_ty, branch_ty),
                        err_t1, err_t2, detail))) ;
             #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
             OManager.printf
               "@[<1>Match right-part, unify with accu gave:@ %a@]@."
               W_PrintTypes.pp_simple_type right_parts_ty ;
             #<End> ; (* <---------- END DEBUG *)
             (* Accumulate the annotations. *)
             let annotmap_accu' =
               QmlAnnotMap.merge annotmap_accu branch_annotmap in
             annotmap_accu')
          (* Start with the annotations map containing those of the matched
             expression and those of the patterns. *)
          patts_annotmap
          cases_list envs_extens in
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Match nearly stop@." ;
      #<End> ; (* <---------- END DEBUG *)
      (* Finally, the type of the whole expression is the one accumulated by
         side effect while typechecking the right-side parts of the
         matching. *)
      perform_infer_expr_type_postlude
        original_expr final_annot_map right_parts_ty
    )
  | QmlAst.Record (_, fields_exps) ->
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Record start@." ;
      #<End> ; (* <---------- END DEBUG *)
      (* A record expression leads to a closed record type plugged into an
         opened sum type (i.e. the column ends by a column-variable). *)
      let (reved_typed_fields, annotmap) =
        (* Really left, to process fields in their apparition order and hence
           merge the annotation maps in the right order. However, this builds
           the list of typed fields in reverse order. So, we reverse it
           afterwards. *)
        List.fold_left
          (fun (typed_fields_accu, annotmap_accu) (field_label, field_expr) ->
             let (field_ty, field_annotmap) =
               infer_expr_type ~bypass_typer typing_env field_expr in
             (* Accumulate the newly typed field. *)
             let typed_fields_accu' =
               (field_label, field_ty) :: typed_fields_accu in
             (* Accumulate the annotations of the map. *)
             let annotmap_accu' =
               QmlAnnotMap.merge annotmap_accu field_annotmap in
             (typed_fields_accu', annotmap_accu'))
          ([], QmlAnnotMap.empty)
          fields_exps in
      (* Put back the list of typed_fields in the right order. *)
      let typed_fields = List.rev reved_typed_fields in
      let record_ty = W_CoreTypes.type_closed_record typed_fields in
      W_TypeInfo.add_loc_object record_ty.W_Algebra.sty_desc loc ;
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Record nearly stop@." ;
      #<End> ; (* <---------- END DEBUG *)
      perform_infer_expr_type_postlude original_expr annotmap record_ty
  | QmlAst.Dot (_, record_expr, field_name) ->
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Dot start@." ;
      #<End> ; (* <---------- END DEBUG *)
      (* Typecheck the expression expected to host the field. *)
      let (record_ty, record_annotmap) =
        infer_expr_type ~bypass_typer typing_env record_expr in
      (* Create a fresh variable to finally get a handle on the type of the
         field. *)
      let field_type = W_CoreTypes.type_variable () in
      (* Now, create a temporary record type, having as minimal fields the one
         we access and giving it the type variable we create just above. Since
         we are in a dot access, the expected type is the one of a record,
         (possibly opened), i.e. something (a sum) having a closed column.  *)
      let expected_min_record_ty =
        W_CoreTypes.type_opened_record [(field_name, field_type)] in
      W_TypeInfo.add_loc_object expected_min_record_ty.W_Algebra.sty_desc loc ;
      (* Now, unify the temporary record type (that contains our fresh type
         variable that we kept under the hand to finally recover the field's
         type) and the type inferred for the hosting expression.
         This way, we ensure that the hosting expression is really a record,
         that it has a field named like the one we want to access, and by side
         effect record the type of the field in our fresh type variable. *)
      (try W_Unify.unify_simple_type typing_env record_ty expected_min_record_ty
       with W_Unify.Unification_simple_type_conflict (err_t1, err_t2, detail) ->
         raise
           (W_InferErrors.Infer_detailled_unification_conflict
              (W_InferErrors.UCC_dot
                 (original_expr, record_ty, expected_min_record_ty, field_name),
               err_t1, err_t2, detail))) ;
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Dot nearly stop@." ;
      #<End> ; (* <---------- END DEBUG *)
      let (final_ty, final_map) =
        perform_infer_expr_type_postlude original_expr record_annotmap field_type in
      W_TypeInfo.add_linked_object final_ty.W_Algebra.sty_desc
         record_ty.W_Algebra.sty_desc ;
      (final_ty, final_map)
  | QmlAst.ExtendRecord (_, field_name, field_expr, record_expr) -> (
      (* Extending a closed record gives a closed record, extending an opened
         record gives an opened record.
         In order to avoid creation of type with both row and column variable,
         we make the choice to plug this record in a *closed* sum. This means
         that the ExtendRecord construct always closes the column. *)
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "ExtendRecord start@." ;
      #<End> ; (* <---------- END DEBUG *)
      (* Typecheck the expression expected to host the field. *)
      let (record_ty, record_annotmap) =
        infer_expr_type ~bypass_typer typing_env record_expr in
      (* Typecheck the expression to bind to the field. *)
      let (field_ty, field_annotmap) =
        infer_expr_type ~bypass_typer typing_env field_expr in
      (* Merge the 2 obtained annotation maps. *)
      let final_annotmap = QmlAnnotMap.merge record_annotmap field_annotmap in
      (* Now we know the type to give to the field to extend, we must build
         two types: the one being the minimal record type we expect for the
         initial record expression to extend, the one that will be the result
         type after extension.
         The first one is a record containing the field with a type variable
         (to allow having a different type for this field in the result like
         in { { A = 4 } with A = "foo" }).
         The second is a record containing the field with the type inferred
         in the extending expression.
         Both record types share the same row variable to ensure sharing of
         the "other" fields.
         The first and second ones are in a closed column which in fact forbids
         to plug later the result into a sum, as well that the initial record
         to extend is not a sum. *)
      let (record_expected_min_ty, extended_record_ty) =
        W_CoreTypes.record_extention_min_expected_ty_and_result_ty
          field_name field_ty in
      W_TypeInfo.add_loc_object record_expected_min_ty.W_Algebra.sty_desc loc ;
      W_TypeInfo.add_loc_object extended_record_ty.W_Algebra.sty_desc loc ;
      (* We unify the type inferred for the expression we want to extend with
         the minimal record type we expect for it, i.e. with the record type
         containing at least the field bound to a type variable.
         This unification will instantiate by side effect the row variable of
         the result type since this latter and the minimal record type expected
         are sharing this variable. *)
      (try W_Unify.unify_simple_type typing_env record_ty record_expected_min_ty
       with W_Unify.Unification_simple_type_conflict (err_t1, err_t2, detail) ->
         raise
           (W_InferErrors.Infer_detailled_unification_conflict
              (W_InferErrors.UCC_record_extend
                 (original_expr, record_ty, record_expected_min_ty),
               err_t1, err_t2, detail))) ;
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "ExtendRecord nearly stop@." ;
      #<End> ; (* <---------- END DEBUG *)
      perform_infer_expr_type_postlude
        original_expr final_annotmap extended_record_ty
    )
  | QmlAst.Bypass (loc, bsl_key) -> (
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Bypass start@." ;
      #<End> ; (* <---------- END DEBUG *)
      match bypass_typer bsl_key with
      | Some qml_ty_expr ->
          (* See comment for the case [QmlAst.PatCoerce] in the function
             [infer_pattern_type] to understand the reason of this magic
             call. *)
          let (qml_ty_expr, _) =
            QmlTypes.type_of_type
              typing_env.W_TypingEnv.ty_env_qml_global qml_ty_expr in
          (* Now, convert the type expression into an internal typechecker
             type by following named types definitions. *)
          let ty =
            W_TypingEnv.qml_type_to_simple_type
              typing_env qml_ty_expr ~is_type_annotation: false in
          W_TypeInfo.add_loc_object ty.W_Algebra.sty_desc loc ;
          #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
          OManager.printf "@[Bypass nearly end with ty: %a@]@."
            W_PrintTypes.pp_simple_type ty ;
          #<End> ; (* <---------- END DEBUG *)
          perform_infer_expr_type_postlude
            original_expr W_AnnotMap.empty_annotmap ty
      | None ->
          raise
            (W_InferErrors.Infer_bypass_type_not_found (bsl_key, original_expr))
    )
  | QmlAst.Coerce (_, expr, coercing_ty_expr) -> (
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Coerce start@." ;
      #<End> ; (* <---------- END DEBUG *)
      (* First, infer the type of the coerced expression. Attention, DB paths
         are, by invariant, always embedded inside a [Coerce]. So we just
         make a special case to typecheck DB paths when we have one, otherwise
         we process the coerced expressions in a regular way. *)
      let (expr_ty, expr_annotmap) =
        (match expr with
         | QmlAst.Path (_, keys, kind, select) ->
             infer_db_path
               ~bypass_typer typing_env ~surrounding_path_expr: expr keys kind select
         | _ -> infer_expr_type ~bypass_typer typing_env expr) in
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Coerce end inferred expr@." ;
      #<End> ; (* <---------- END DEBUG *)
      (* See comment for the case [QmlAst.PatCoerce] in the function
         [infer_pattern_type] to understand the reason of this magic call. *)
      let coercing_ty_expr =
        (try
          fst
            (QmlTypes.type_of_type
               typing_env.W_TypingEnv.ty_env_qml_global coercing_ty_expr)
         with QmlTyperException.Exception (_, error) ->
           (* Just set a correct source position. *)
           let err_loc =
             QmlTyperException.loc_make
               (`Expr_loc original_expr) QmlTyperException.loc_set_empty in
           (* Re-raise the exception, but with the more precise source
              location. *)
           raise (QmlTyperException.Exception (err_loc, error))) in
      (* Now, convert the type expression into an internal typechecker type
         by following named types definitions. This will allow to ensure that
         the canonical representation of the type constraint is unifiable with
         the type inferred for the constrained expression. *)
      let coercing_ty =
        (try
          W_TypingEnv.qml_type_to_simple_type
            typing_env coercing_ty_expr ~is_type_annotation: true
        with W_TypingEnv.Importing_qml_abstract_ty ->
          (* The type annotation contained "external" which is a non-sense. *)
          let err_loc =
            QmlTyperException.loc_make
              (`Expr_loc original_expr) QmlTyperException.loc_set_empty in
          raise
            (QmlTyperException.Exception
               (err_loc,
                QmlTyperException.InvalidType
                  (coercing_ty_expr, `abstract_in_ty_annotation)))) in
      W_TypeInfo.addrec_loc_object coercing_ty.W_Algebra.sty_desc loc ;
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Coerce end converted ty constraint@." ;
      OManager.printf "@[<1>Must unify@\n%a@\nand@\n%a@]@."
        W_PrintTypes.pp_simple_type expr_ty
        W_PrintTypes.pp_simple_type coercing_ty ;
      #<End> ; (* <---------- END DEBUG *)
      (* Force unification between the type inferred for the expression and the
         coercing type. *)
      (try W_Unify.unify_simple_type_in_coercion typing_env expr_ty coercing_ty
       with W_Unify.Unification_simple_type_conflict (err_t1, err_t2, detail) ->
         raise
           (W_InferErrors.Infer_detailled_unification_conflict
              (W_InferErrors.UCC_coerce (original_expr, expr_ty, coercing_ty),
               err_t1, err_t2, detail))) ;
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Coerce nearly stop@." ;
      #<End> ; (* <---------- END DEBUG *)
      (* The finally retained type is the coercing one, not the one used to
         ensure that the canonical representation of the constraint is
         unifiable with the type inferred for the constrained expression. *)
      let final_result =
        perform_infer_expr_type_postlude
          original_expr expr_annotmap coercing_ty in
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "@[<1>Coerce final ty:@ %a@]@."
        W_PrintTypes.pp_simple_type (fst final_result) ;
      #<End> ; (* <---------- END DEBUG *)
      final_result
    )
  | QmlAst.Directive (_, variant, exprs, tys) ->
      (* Since we have several kind of directives, prefer to have a dedicated
         function handling their type inference instead of in-lining all here.
         This will be much more readable. *)
      infer_directive_type
        ~bypass_typer typing_env original_expr variant exprs tys
  | QmlAst.Path (_, _, _, _) ->
      (* By invariant, the DbPathCoercion (see pass_DbPathCoercion.ml that
         calls pass schema_private.ml) must have instrumented the [QmlAst.Path]
         code so that if it didn't plug them in a [QmlAst.Coerce] then it
         surrounded it by a call to a coercing function ([hack_coerce_default],
         [hack_coerce_option], [hack_coerce_vvpath] or [hack_coerce_vrpath]
         defined ) in qmlDbGen.ml).
         From this matter of fact, the present expression will be coerced by
         the surrounding function, and that's this function that gives the
         real type of the path. So we can safely type the path here with a
         variable, this latter being coerced after. *)
      let ty = W_CoreTypes.type_variable () in
      perform_infer_expr_type_postlude
        original_expr W_AnnotMap.empty_annotmap ty



(** [surrounding_path_expr] : [QmlAst.Path] surrouding expression, i.e. the
    expression containing the keys we are typechecking. This is the
    expression node in which the type annotation will be recorded. *)
and infer_db_path ~bypass_typer typing_env ~surrounding_path_expr _keys _kind _select =
  (* The type of the DB path is not given by the path itself: it is always
     coerced afterwards by the surrounding [QmlAst.Coerce]. So we simply return
     a type variable and let the regular processing coercing it. *)
  let ty = W_CoreTypes.type_variable () in
  let annotmap' =
    QmlAstWalk.Expr.traverse_fold
      (fun tra annotmap_accu -> function
       | QmlAst.Path _ as e -> tra annotmap_accu e
       | _ as e ->
           let _e_ty, e_annotmap = infer_expr_type ~bypass_typer typing_env e in
           QmlAnnotMap.merge annotmap_accu e_annotmap
      ) W_AnnotMap.empty_annotmap surrounding_path_expr in
  perform_infer_expr_type_postlude surrounding_path_expr annotmap' ty




(* ************************************************************************** *)
(** {b Descr}: Infers the type of a non-recursive let-definition. This mostly
    consists in inferring the type of the bound identifiers separately, then
    inferring the type of the "in"-part of the definition.
    ATTENTION: [SurfaceAstDependencies.rewrite_module] performs a source code
    transformation that extract the fields of a module definition and
    transforms it into a let-definition with the same name than the field.
    Then, once done for all the fields, the module expression is rebuilt by
    setting each field to its corresponding artificial let-definition.
    This breaks the mechanism of type variables introduced by explicit type
    annotation generalization because fields bodies get typechecked outside the
    scope of the module, hence without we could register the binding level at
    which type variables must be created when they appear in explicit
    annotations. To circumvent this, let-definitions created by modules
    transformation are embedded in a [`module_field_lifting] directive. When
    we encounter this directive in [infer_expr_type], we explicitly call the
    present function telling that [~lifted_module_field] is [true] to handle
    the issue. Otherwise, this flag is always set to [false].
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and infer_nrec_let_definition_type ~lifted_module_field ~bypass_typer
    typing_env def_bindings =

  (* If the current let-definition is a toplevel one or if it corresponds
     to a module field that has been transformed into a let-definition
     (case identified because [~lifted_module_field] is [true]), then we
     artificially increase the current binding level and remind this level to
     be the one at which type variables introduced by annotations must be
     created. *)
  let at_toplevel_or_lifted =
    !W_CoreTypes.current_binding_level = 0 || lifted_module_field in
  if at_toplevel_or_lifted then
    W_CoreTypes.new_annotations_generalizable_level () ;

  (* Non-recursive definitions. This is like successive let definitions.
     We simply typecheck each binding separately, in sequence, with the
     initial environment. *)
  let (reved_typed_bindings, annotmap_from_bindings) =
    (* Really left, to process bindings in their apparition order and hence
       merge the annotation maps in the right order. However, this builds
       the list of typed bindings in reverse order. So, we reverse it
       afterwards. *)
    List.fold_left
      (fun (typed_bindings_accu, annotmap_accu) (binding_name, binding_expr) ->
         let expansive = QmlAstUtils.is_expansive binding_expr in
         (* We use the trick that consists to say that if the expression
            is expansive, hence won't be legally subject to generalization,
            then to typecheck it, we don't change the binding level and
            later we also won't generalize its type. *)
         let (binding_ty, binding_annotmap) =
           if not expansive then (
             W_CoreTypes.begin_definition () ;
             let (t, a) =
               infer_expr_type ~bypass_typer typing_env binding_expr in
             W_CoreTypes.end_definition () ;
             (t, a)
           )
           else infer_expr_type ~bypass_typer typing_env binding_expr in
         (* Now, add the typed binding and annotation map to the accus.
            Note that we remind if the expression of the binding was
            non-expansive to avoid computing this again. We also remind the
            annotation key to be able later to add a scheme creation information
            in the annotmap, as explicit instantiation expects. *)
         let typed_bindings_accu' =
           (binding_name, binding_ty, binding_expr, expansive) ::
             typed_bindings_accu in
         let annotmap_accu' =
           QmlAnnotMap.merge annotmap_accu binding_annotmap in
         (* Finally return the typed bindings and the annotation map. *)
         (typed_bindings_accu', annotmap_accu'))
      ([], QmlAnnotMap.empty)
      def_bindings in

  (* Restore the previously artificially increased binding level and update
     the new level at which type variables of explicit type annotations must
     now be created. *)
  if at_toplevel_or_lifted then
    W_CoreTypes.release_annotations_generalizable_level () ;

  (* Put back the typed bindings in the right order. *)
  let typed_bindings = List.rev reved_typed_bindings in

  (* Ok, now the bindings are typechecked, we have their type and the
     annotation map cumulating all the annotations found during the bindings
     typechecking. We must now create the environment induced by these
     definitions. We create here the environment extension, i.e. the delta to
     later add to the environment to typecheck the "in-part" of the
     let-definition. *)
  let (typing_env_extension, final_annotmap) =
    List.fold_left
       (* Really left, to process bindings in their apparition order and hence
          merge the annotation maps in the right order. Moreover, this builds
          the list representing the environment in the right order, i.e. with
          bound name inserted in head of the list as they are encountered. *)
      (fun (env_ext_accu, annotmap_accu)
           (name, ty, body, expansive) ->
         (* Check if are have the right to generalize the type. *)
         let scheme =
           (if not expansive then
              (try
                (* If the definition is toplevel, then forbid non-generalized
                   variables remaining in the scheme.
                   TODO: this is a bit broken because in case of trivial_scheme,
                   no check is done ! *)
                W_SchemeGenAndInst.generalize
                  ~forbid_non_gen_vars: (!W_CoreTypes.current_binding_level = 0)
                  ty
               with W_SchemeGenAndInst.Private_type_not_opaque t ->
                 raise
                   (W_InferErrors.Infer_private_type_not_opaque
                      (name, body, ty, t)))
            else W_SchemeGenAndInst.trivial_scheme ty) in
         (* Since we created a type scheme, we need to record it in the
            annotation map. *)
         let annotmap_scheme =
           W_PublicExport.type_scheme_to_annotmap_type_scheme scheme in
         let annotmap_accu' =
           annotmap_add_scheme_creation
             (QmlAst.QAnnot.expr body) annotmap_scheme annotmap_accu in
         let env_ext_accu' = (name, scheme) :: env_ext_accu in
         (* Finally, return the extended accumulators. *)
         (env_ext_accu', annotmap_accu'))
      ([], annotmap_from_bindings)
      typed_bindings in

  #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
  List.iter
    (fun (n, sch) ->
       OManager.printf "@[<1>Env extended with@ %s:@ %a@]@."
         (Ident.to_string n) W_PrintTypes.pp_scheme sch)
    typing_env_extension ;
  #<End> ; (* <---------- END DEBUG *)

  if at_toplevel_or_lifted then
    List.iter
      (fun (ident, sch) ->
         W_TypingEnv.extend_toplevel_valdefs_env_memo ident sch)
      typing_env_extension ;

  (* And finally, we return the environment extended by the extension we just
     created above and the annotation map. *)
  let extended_typing_env = {
    typing_env with
      W_TypingEnv.ty_env_local =
        typing_env_extension @ typing_env.W_TypingEnv.ty_env_local } in
  (extended_typing_env, final_annotmap)




and infer_rec_let_definition_type ~lifted_module_field ~bypass_typer typing_env
    def_bindings =

  #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
  OManager.printf "Starting let-rec typechecking@." ;
  #<End> ;
  (* Same thing about binding level at which type variables introduced by
     type annotations must be created than in [infer_nrec_let_definition_type].
     Read comment at the early lines of [infer_nrec_let_definition_type]. *)
  let at_toplevel_or_lifted =
    !W_CoreTypes.current_binding_level = 0 || lifted_module_field in
  if at_toplevel_or_lifted then
    W_CoreTypes.new_annotations_generalizable_level () ;

  (* Since we are in the case of recursive definitions, we start by creating
     temporary types to assign to the bound expressions. *)
  let tmp_types =
    List.map
      (fun (binding_name, binding_expr) ->
        (* The variable will be allowed for generalization only if the
           expression it is bound to is non-expansive. *)
        let expansive = QmlAstUtils.is_expansive binding_expr in
        if not expansive then (
          W_CoreTypes.begin_definition () ;
          let tmp = W_CoreTypes.type_variable () in
          W_CoreTypes.end_definition () ;
          W_TypeInfo.add_loc_object
            tmp.W_Algebra.sty_desc
            (QmlAst.Label.expr binding_expr) ;
          (* We keep the bound expression to be able to issue a more accurate
             error message in case of unification error later. *)
          (binding_name, binding_expr, tmp, expansive)
         )
        else (
          let tmp = W_CoreTypes.type_variable () in
          W_TypeInfo.add_loc_object
            tmp.W_Algebra.sty_desc
            (Annot.Magic.label binding_expr) ;
          (binding_name, binding_expr, tmp, expansive)
        )
      )
      def_bindings in

  (* Now, create the environment with ourselves *non* generalized. This
     environment is the one we will use to infer the type of the bodies of
     the let-rec definition. *)
  let typing_env_for_defs_bodies = {
    typing_env with
      W_TypingEnv.ty_env_local =
        (List.map
           (fun (n, _, t, _) -> (n, W_SchemeGenAndInst.trivial_scheme t))
           tmp_types)
        @ typing_env.W_TypingEnv.ty_env_local } in


  (* While creating the temporary types, we checked by the way the expansivity
     of the expressions. So to avoid computing it again later, recover this
     information now. *)
  let expansive_flags = List.map (fun (_, _, _, flag) -> flag) tmp_types in

  (* Now, typecheck bodies of the definitions. We don't update annotation map
     now with creation of the definitions type schemes will be done later, once
     we will have unified the temporary and the inferred types of the
     typechecked bodies. However, we accumulate the annotations obtained during
     the bodies typechecking. *)
  let (reved_bodies_tys, annotmap_from_bodies) =
    List.fold_left2
      (* Really left, to process bindings in their apparition order and hence
         merge the annotation maps in the right order. However, this builds
         the list of typed bodies in reverse order. So, we reverse it
         afterwards. *)
      (fun (typed_bodies_accu, annotmap_accu)
           (_binding_name, binding_expr) expansive ->
        (* Same technics as above for variables that must not be generalized. *)
        let (body_ty, body_annotmap) =
          if not expansive then (
            W_CoreTypes.begin_definition () ;
            let (t, a) =
              infer_expr_type
                ~bypass_typer typing_env_for_defs_bodies binding_expr in
            W_CoreTypes.end_definition () ;
            (t, a)
           )
          else
            infer_expr_type
              ~bypass_typer typing_env_for_defs_bodies binding_expr in
        (* Now, add the typed body and annotation map to the accus. *)
        let typed_bodies_accu' = body_ty :: typed_bodies_accu in
        let annotmap_accu' = QmlAnnotMap.merge annotmap_accu body_annotmap in
        (* Finally return the typed body and the annotation map. *)
        (typed_bodies_accu', annotmap_accu'))
      ([], QmlAnnotMap.empty)
      def_bindings
      expansive_flags in

  (* Like in [infer_nrec_let_definition_type] to deals with explicit type
     variables introduced by type annotations. *)
  if at_toplevel_or_lifted then
    W_CoreTypes.release_annotations_generalizable_level () ;

  (* Put back the types of bodies in the right order. *)
  let bodies_tys = List.rev reved_bodies_tys in

  (* Now we unify the type found for each body with the temporary type we gave
     it earlier. *)
  W_CoreTypes.begin_definition () ;
  List.iter2
    (fun found_ty (binding_name, body_expr, tmp_ty, _) ->
       #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
       OManager.printf "Let-rec: %s,  found type: %a, tmp type: %a@."
         (Ident.to_string binding_name) W_PrintTypes.pp_simple_type found_ty
         W_PrintTypes.pp_simple_type tmp_ty ;
       #<End> ;
       (* when you have a definition
          [rec f(x:t) = f(1)],
          the type of [f] is [int -> 'a]
          and the type of the body is [t -> 'a]
          to keep the names from the coercions (which appear
          in the body type), you need to put the body type
          on the right of the unification
       *)
       try W_Unify.unify_simple_type typing_env tmp_ty found_ty
       with W_Unify.Unification_simple_type_conflict (err_t1, err_t2, detail) ->
         raise
           (W_InferErrors.Infer_detailled_unification_conflict
              (W_InferErrors.UCC_let_rec_body
                 (binding_name, body_expr, found_ty, tmp_ty),
               err_t1, err_t2, detail)))
    bodies_tys tmp_types ;
  W_CoreTypes.end_definition () ;

  (* Finally, we create the environment extension induced by these definitions
     and update the annotation map here since we create the final type schemes
     at this point. *)
  let (typing_env_extension, annotmap_after_final_schemes) =
    List.fold_left
       (* Really left, to process bindings in their apparition order and hence
          merge the annotation maps in the right order. Moreover, this builds
          the list representing the environment in the right order, i.e. with
          bound name inserted in head of the list as they are encountered. *)
      (fun (env_ext_accu, annotmap_accu) (name, body, ty, expansive) ->
        (* Check if are have the right to generalize the type. *)
        let scheme =
          (if not expansive then
             (try
               (* If the definition is toplevel, then forbid non-generalized
                  variables remaining in the scheme.
                TODO: this is a bit broken because in case of trivial_scheme,
                no check is done ! *)
               W_SchemeGenAndInst.generalize
                 ~forbid_non_gen_vars: (!W_CoreTypes.current_binding_level = 0)
                 ty
              with W_SchemeGenAndInst.Private_type_not_opaque t ->
                raise
                  (W_InferErrors.Infer_private_type_not_opaque
                     (name, body, ty, t)))
          else W_SchemeGenAndInst.trivial_scheme ty) in
        (* In any case, since we created a type scheme (generalized or not),
           we need to record it in the annotation map. *)
        let annotmap_scheme =
          W_PublicExport.type_scheme_to_annotmap_type_scheme scheme in
        let annotmap_accu' =
          annotmap_add_scheme_creation
            (QmlAst.QAnnot.expr body) annotmap_scheme annotmap_accu in
        let env_ext_accu' = (name, scheme) :: env_ext_accu in
        (* Finally, return the extended accumulators. *)
        (env_ext_accu', annotmap_accu'))
      ([], annotmap_from_bodies)
      tmp_types in

  #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
  OManager.printf "Let rec final schemes:@." ;
  List.iter
    (fun (ident, sch) ->
       OManager.printf " - %s: %a@."
         (Ident.to_string ident) W_PrintTypes.pp_scheme sch)
  typing_env_extension ;
  #<End>;

  if at_toplevel_or_lifted then
    List.iter
      (fun (ident, sch) ->
         W_TypingEnv.extend_toplevel_valdefs_env_memo ident sch)
      typing_env_extension ;

  (* We will return the environment extended by the extension we just created
     above and the annotation map. So, build it. *)
  let extended_typing_env = {
    typing_env with
      W_TypingEnv.ty_env_local =
        typing_env_extension @ typing_env.W_TypingEnv.ty_env_local } in

  (* Now, we must update all the recursive call points of the bound bodies
     because we temporarily registered that at these points we instantiated a
     scheme that was the temporary scheme we gave to the recursive identifier
     used, not the real one  obtained once the fix point is reached. *)
  let final_annotmap =
    update_recursive_calls_annotations
      extended_typing_env annotmap_after_final_schemes def_bindings in

  (extended_typing_env, final_annotmap)




(* ************************************************************************** *)
(** {b Descr} : Infers the type of a directive. Returns both the inferred type
    and the annotation map recording type information related to the directive.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and infer_directive_type ~bypass_typer typing_env original_expr core_directive dir_exprs dir_tys =
  let loc = QmlAst.Label.expr original_expr in
  let () =
    List.iter
      (fun ty ->
         let ty, _ =
           QmlTypes.type_of_type typing_env.W_TypingEnv.ty_env_qml_global ty in
         let _wty =
           W_TypingEnv.qml_type_to_simple_type ~save_mapping:true
             typing_env ty ~is_type_annotation: true
         in ()) dir_tys
  in
  match (core_directive, dir_exprs) with
  | (`module_field_lifting,
     [ (QmlAst.LetIn (_, def_bindings, in_expr)
       | QmlAst.LetRecIn (_, def_bindings, in_expr) ) as field_def ]) ->
      (* Directive telling that the embedded let-definition is in fact a
         module field definition that has been expatriated outside its original
         module by the source code transformed performed by
         [SurfaceAstDependencies.rewrite_module].
         In such a case, we must deal with updating and recording the binding
         level at which type variables introduced by explicit type annotations
         must be created.
         This is the same idea than for toplevel let-definitions which allow
         such type variables to be generalized when the toplevel definition
         ends, not before. In effect, for modules, such type variables can
         be generalized at the end of the field's definition. But since fields
         are transformed into let-definitions, to remind that a let-definition
         must be considered as a module's field definition, hence allow
         variables generalization at its definition's end, the directive is
         used to embed the let-definition. *)
      (* Pretty like the case [QmlAst.LetIn] of [infer_expr_type], except that
         we call [infer_nrec_let_definition_type] with [~lifted_module_field]
         set to [true]. The remaining of the process is identical. *)
      let (new_env, annotmap_from_bindings) =
        (match field_def with
         | QmlAst.LetIn _ ->
             (* Transformed into non-recursive let-definition. *)
             infer_nrec_let_definition_type
               ~lifted_module_field: true ~bypass_typer typing_env def_bindings
         | QmlAst.LetRecIn _ ->
             (* Transformed into non-recursive let-definition. *)
             infer_rec_let_definition_type
               ~lifted_module_field: true ~bypass_typer typing_env def_bindings
         | _ -> assert false) in
      let (in_expr_ty, in_expr_annotmap) =
        infer_expr_type ~bypass_typer new_env in_expr in
      (* Merge the 2 annotations maps. *)
      let annotmap_for_whole_def =
        QmlAnnotMap.merge in_expr_annotmap annotmap_from_bindings in
      (* Finally, the return type is the one of the "in-part" of the
         let-definition. Get annotations map cumulating all the encountered
         annotations of the bindings and the "in"-part and especially containing
         type information for the let-definition embedded in our directive.
         In effect, beware we call [perform_infer_expr_type_postlude] with
         [field_def] as expression for which we have to register the type in
         the annotmap we will get as result. *)
      let (final_ty_for_whole_def, final_annotmap_for_whole_def) =
        perform_infer_expr_type_postlude
          field_def annotmap_for_whole_def in_expr_ty in
      W_TypeInfo.addrec_dir_object
        final_ty_for_whole_def.W_Algebra.sty_desc (core_directive, loc) ;
      (* Just annotate the whole original expression before returning. In
         effect, annotation was done just before by
         [perform_infer_expr_type_postlude] but for the inner
         let-definition, not on the whole hosting expression. *)
      let final_annotmap =
        QmlAnnotMap.add_ty
          (QmlAst.QAnnot.expr original_expr) final_ty_for_whole_def
          final_annotmap_for_whole_def in
      (final_ty_for_whole_def, final_annotmap)
  | (`module_field_lifting, _) ->
      (* A "lifted_module_field" directive with an expression not being a
         unique non-recursive let-definition is non-sense. This should never
         happen. *)
      let err_ctxt = QmlError.Context.expr original_expr in
      QmlError.error err_ctxt
        ("Module field lifted definition expected to be a non-recursive " ^^
         "let-definition.")
  | (`module_, [ (QmlAst.Record (_, fields_exps)) as record_expr ]) -> (
      (* A module expression leads to a closed record type plugged into an
         closed sum type (i.e. the column **doesn't** end by a column-variable).
         Each field of the module get "pseudo-generalized", i.e. turned into
         a "forall-type" if it contains type variables and is non-expansive.
         The column needs to be closed otherwise, when making a sub-module, when
         we "foralize" the type of this sub-module, which would have a column
         variable, this variable would be generalized and then the type of this
         sub-module would be effectively turned into a type-forall: i.e. it
         would not fall in the trivial case where no variable during
         "forall-ization" leads to in fact a non-forall-type (i.e. leads to
         the original type unchanged). This is an issue because type expressions
         for modules are never plugged into a type forall. Hence, trying to
         put a type constraint (so, unify with the type created from a type
         expression) on a module expression would fail because the type-forall
         we would have got for the module expression would not unify with the
         non-type-forall obtained in the constraint. *)
      let (reved_typed_fields, annotmap) =
        (* Really left, to process fields in their apparition order and hence
           merge the annotation maps in the right order. However, this builds
           the list of typed fields in reverse order. So, we reverse it
           afterwards. *)
        List.fold_left
          (fun (typed_fields_accu, annotmap_accu) (field_label, field_expr) ->
             (* Modules are a bit like toplevel let-definitions for type
                variables introduced by explicit type annotations. See
                [infer_nrec_let_definition_type] for information.
                ATTENTION: In fact, since modules are rewritten into
                let-definitions by [SurfaceAstDependencies.rewrite_module],
                practically, the present code is used and works only with
                opatop. When the compiler runs, it typechecks the transformed
                source into which let-definitions created by transformation
                are embedded in a [`module_field_lifting] directive. *)
             W_CoreTypes.new_annotations_generalizable_level () ;
             (* Typecheck the field expression. *)
             let expansive = QmlAstUtils.is_expansive field_expr in
             let (field_ty, field_annotmap) =
               if not expansive then (
                 W_CoreTypes.begin_definition () ;
                 (* Create a new variables mapping to insulate the scope of
                    type variables introduced in each field's constraints.
                    But, creating this mapping, we must not forget variables
                    already introduced by the possibly surrounding expression
                    that host the current module expression. So we use the
                    "inheriting" version of variables mapping creation. *)
                 W_TypingEnv.new_inheriting_variables_mapping () ;
                 let (t, a) =
                   infer_expr_type ~bypass_typer typing_env field_expr in
                 (* Now, forget the local variables mapping. *)
                 W_TypingEnv.release_variables_mapping () ;
                 W_CoreTypes.end_definition () ;
                 (t, a)
               )
               else (
                 (* Same thing about the variables mapping than in the other
                    case of this conditional. *)
                 W_TypingEnv.new_inheriting_variables_mapping () ;
                 let (t, a) =
                   infer_expr_type ~bypass_typer typing_env field_expr in
                 (* Now, forget the local variables mapping. *)
                 W_TypingEnv.release_variables_mapping () ;
                 (t, a)
               ) in

             (* Restore the previously artificially increased binding level and
                update the new level at which type variables of explicit type
                annotations must now be created.
                It's like in [infer_nrec_let_definition_type]. *)
             W_CoreTypes.release_annotations_generalizable_level () ;

             #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
             OManager.printf "@[Module field: %s of type: %a@]@."
               field_label W_PrintTypes.pp_simple_type field_ty
             #<End> ; (* <---------- END DEBUG *)

             (* Now plug the inferred type into a type-forall. *)
             let forallized_field_ty =
               W_SchemeGenAndInst.type_forall field_ty in

             #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
             OManager.printf "@[Module field: %s of forall-ized type: %a@]@."
               field_label W_PrintTypes.pp_simple_type forallized_field_ty
             #<End> ; (* <---------- END DEBUG *)

             (* We created a new type from [field_ty], and not simply
                got a type from [infer_expr_type], conversely to what happens in
                the simpler case of record typechecking.
                **HOWEVER IT APPEARS** that explicit instantiation doesn't want
                the annotation for the field's type to be the forall-ized type !
                So, **DON'T** override the type annotation written in the
                annotation map even if forall-ization has turned the type
                originally inferred for the field into a type-forall ! *)
             (* However, explicit instantiation expect to record the creation
                of a type scheme in the annotations map for the field if
                forall-ization leaded to an effective type forall. *)
             let final_field_annotmap = (
               match
                 W_SchemeGenAndInst.get_type_forall_scheme forallized_field_ty
               with
                | Some scheme_of_forall_type ->
                    (* Ok, the type is really a forall-type, so add an
                       annotation telling that we created a type scheme in the
                       annotations map. *)
                    let scheme_for_annotmap =
                      W_PublicExport.type_scheme_to_annotmap_type_scheme
                        scheme_of_forall_type in
                    annotmap_add_scheme_creation
                      (QmlAst.QAnnot.expr field_expr) scheme_for_annotmap
                      field_annotmap
                | None ->
                    (* In fact, the type was not turned into an effective
                       forall-type, so leave the annotations map untouched. *)
                    field_annotmap
              ) in
             (* Accumulate the newly typed field. *)
             let typed_fields_accu' =
               (field_label, forallized_field_ty) :: typed_fields_accu in
             (* Accumulate the annotations of the map. *)
             let annotmap_accu' =
               QmlAnnotMap.merge annotmap_accu final_field_annotmap in
             (typed_fields_accu', annotmap_accu'))
          ([], QmlAnnotMap.empty)
          fields_exps in

      (* Put back the list of typed_fields in the right order. *)
      let typed_fields = List.rev reved_typed_fields in
      (* Finally, build the module type as a closed record plugged into an
         closed sum type. *)
      let module_ty = W_CoreTypes.type_module_record typed_fields in
      W_TypeInfo.addrec_dir_object
        module_ty.W_Algebra.sty_desc (core_directive, loc) ;
      let (module_ty', annotmap') =
        perform_infer_expr_type_postlude record_expr annotmap module_ty in
      W_TypeInfo.add_loc_object
        (module_ty').W_Algebra.sty_desc loc ;
      (* Just annotate the whole original expression before returning. In
         effect, annotation was done just before by
         [perform_infer_expr_type_postlude] but for the inner record
         expression, not on the whole hosting expression. *)
      let annotmap'' =
        QmlAnnotMap.add_ty
          (QmlAst.QAnnot.expr original_expr) module_ty' annotmap' in
      (module_ty', annotmap'')
    )
  | (`module_, _) ->
      (* A "module" directive with an expression not being a unique record is
         non-sense. This should never happen. *)
      let err_ctxt = QmlError.Context.expr original_expr in
      QmlError.error err_ctxt "Module expression expected to be a record."
  | (`opensums, [expr]) -> (
      (* First, typecheck the expression to "open". *)
      let (ty, annotmap) = infer_expr_type ~bypass_typer typing_env expr in
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "@[Opensum expr: %a@]@." W_PrintTypes.pp_simple_type ty ;
      #<End> ; (* <---------- END DEBUG *)
      try
        let opened_ty =
          W_OpenSumsDirective.open_sum_simple_type typing_env ty in
        #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
        OManager.printf
          "@[Opensum opened_ty: %a@]@." W_PrintTypes.pp_simple_type opened_ty ;
        #<End> ; (* <---------- END DEBUG *)
        perform_infer_expr_type_postlude original_expr annotmap opened_ty
      with W_OpenSumsDirective.Open_sum_failure reason ->
        (* Issue an error message depending on if the failure occurred by trying
           to open a type variable or another (hard) type. *)
        let err_msg =
          if reason then
            "Expression@ argument@ of@ the@ @@opensums@ directive@ has@ a@ " ^^
            "type@ that@ can't@ be@ subtype@ of@ another@ type@ (sum@ or@ " ^^
            "record).@\n" ^^
            "@[<2>@{<bright>Hint@}:@\n"^^
            "Its@ type@ is@ currently@ unconstrained.@ Consider@ explicitly@ "^^
            "adding@ a@ type@ constraint@ toward@ a sum@ type@ to@ " ^^
            "precise@ the@ expression's@ type.@]"
          else
            "Expression@ argument@ of@ the @@opensums@ directive@ has@ a@ " ^^
            "type@ that@ can't@ be@ subtype@ of@ another@ type@ (sum@ or@ " ^^
            "record).@\n" in
        let err_ctxt = QmlError.Context.expr original_expr in
        QmlError.error err_ctxt err_msg
    )
  | (`opensums, _) ->
      (* An "opensums" directive with 0 or several expressions is non-sense.
         This should never happen. *)
      let err_ctxt = QmlError.Context.expr original_expr in
      QmlError.error
        err_ctxt
        ("Opensums@ directive@ expected@ to@ be@ applied@ to@ onl@y one@ " ^^
         "expression.")
  | (`extendwith, [expr]) -> (
      match expr with
        | QmlAst.ExtendRecord _ -> (
          let rec collect_fields ack e =
            match e with
             | QmlAst.ExtendRecord (_, field_name, field_expr, record_expr) ->
                collect_fields ((field_name, field_expr, e)::ack) record_expr
             | e -> (ack, e) in
          let fields_es, record_expr = collect_fields [] expr in
          (* First, typecheck the record expression. *)
          let (record_ty, record_annotmap) =
            infer_expr_type ~bypass_typer typing_env record_expr in
          #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
          OManager.printf "@[Extendwith record type: %a@]@."
            W_PrintTypes.pp_simple_type record_ty ;
          #<End> ; (* <---------- END DEBUG *)
          (* Since record is extended it can not have an open Column,
            if it does close it.*)
          (match record_ty.W_Algebra.sty_desc with
          | W_Algebra.SType_sum_of_records column_type ->
              column_type.W_Algebra.ct_value <-
                (fst column_type.W_Algebra.ct_value, W_Algebra.Closed_column)
          | _ -> () );
          (*find fields types*)
          let find_field_type (init_annotmap, record_ty)
                (field_name, field_expr, extend_exp) =
            let (field_ty, annotmap) =
             infer_expr_type ~bypass_typer typing_env field_expr in
            #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
            OManager.printf "@[Extendwith field type: %a@]@."
            W_PrintTypes.pp_simple_type field_ty ;
            #<End> ; (* <---------- END DEBUG *)
            let extended_ty =
              W_ExtendWithDirective.extend_record_simple_type
              typing_env record_ty field_name field_ty in
            W_TypeInfo.add_loc_object
              extended_ty.W_Algebra.sty_desc loc ;
            let (extended_ty', annotmap') = perform_infer_expr_type_postlude
              extend_exp annotmap extended_ty in
            ( QmlAnnotMap.merge ~no_conflict_if_equal:true init_annotmap annotmap',
              extended_ty' ) in
          try(
            let field_annotmap, extended_ty =
              List.fold_left find_field_type (record_annotmap, record_ty) fields_es in
            let final_annotmap =
              QmlAnnotMap.merge ~no_conflict_if_equal:true record_annotmap field_annotmap in
              #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
              OManager.printf
                "@[Extendwith extended_ty: %a@]@."
                W_PrintTypes.pp_simple_type extended_ty ;
              #<End> ; (* <---------- END DEBUG *)
            let (_, final_annotmap') = perform_infer_expr_type_postlude
              expr final_annotmap extended_ty in
            perform_infer_expr_type_postlude
              original_expr final_annotmap' extended_ty)
          with W_ExtendWithDirective.Extend_with_failure reason ->
            (* Issue an error message depending on if the failure occurred by trying
               to open a type variable or another (hard) type. *)
            let err_msg =
              if reason then
                "@[<2>The expression argument of the " ^^
                "@@extendwith directive has a @\n" ^^
                "type that can't be subtype of a record type.@\n" ^^
                "@[<2>@{<bright>Hint@}:@\n"^^
                "Its@ type@ is@ currently@ unconstrained.@ Consider@ explicitly "^^
                "adding@ a@ type@ constraint@ toward@ a@ record@ type@ to@ " ^^
                "precise@ the@ expression's@ type.@]@."
              else
                "@[<2><@>The expression argument of the " ^^
                "@@extendwith directive has " ^^
                "a type that can't be subtype of a record type.@\n@]" in
            let err_ctxt = QmlError.Context.expr original_expr in
            QmlError.error err_ctxt err_msg
        )
        | _ ->
       let err_ctxt = QmlError.Context.expr original_expr in
      QmlError.error
        err_ctxt
        ("@@extendwith@ directive@ expects@ to@ be@ applied@ to@ only@ " ^^
         "an@ with@ expression expression.")
  )
  | (`extendwith, _) ->
      let err_ctxt = QmlError.Context.expr original_expr in
      QmlError.error
        err_ctxt
        ("@@extendwith@ directive@ expected@ to@ be@ applied@ to@ only@ " ^^
         "one@ expression.")
  | (`openrecord, [expr]) -> (
      (* First, typecheck the expression to "open". *)
      let (ty, annotmap) = infer_expr_type ~bypass_typer typing_env expr in
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "@[Openrecord expr: %a@]@."
        W_PrintTypes.pp_simple_type ty ;
      #<End> ; (* <---------- END DEBUG *)
      try
        let opened_ty =
          W_OpenRecordDirective.open_record_simple_type typing_env ty in
        #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
        OManager.printf
          "@[Openrecord opened_ty: %a@]@."
          W_PrintTypes.pp_simple_type opened_ty ;
        #<End> ; (* <---------- END DEBUG *)
        perform_infer_expr_type_postlude original_expr annotmap opened_ty
      with W_OpenRecordDirective.Open_record_failure reason ->
        (* Issue an error message depending on if the failure occurred by trying
           to open a type variable or another (hard) type. *)
        let err_msg =
          if reason then
            "Expression@ argument@ of@ the@ @@openrecord@ directive@ has a@ " ^^
            "type@ that@ can't@ be@ subtype@ of@ another@ type@ (record).@\n" ^^
            "@[<2>@{<bright>Hint@}:@\n"^^
            "Its@ type@ is@ currently@ unconstrained.@ Consider@ explicitly "^^
            "adding@ a@ type@ constraint@ toward@ a@ record@ type@ to@ " ^^
            "precise@ the@ expression's@ type.@]"
          else
            "Expression@ argument@ of@ the@ @@openrecord@ directive@ has@ " ^^
            "a@ type@ that@ can't@ be@ subtype@ of@ a@ record@ type.@\n" in
        let err_ctxt = QmlError.Context.expr original_expr in
        QmlError.error err_ctxt err_msg
    )
  | (`openrecord, _) ->
      (* An "openrecord" directive with 0 or several expressions is non-sense.
         This should never happen. *)
      let err_ctxt = QmlError.Context.expr original_expr in
      QmlError.error
        err_ctxt
        ("@@openrecord@ directive@ expected@ to@ be@ applied@ to@ only@ " ^^
         "one@ expression.")
  | (`throw, [expr]) -> (
      let (thrown_ty, annotmap) =
        infer_expr_type ~bypass_typer typing_env expr in
      (* Get the current type of exceptions. *)
      let exn_ty = W_Exceptions.type_exception () in
      (try W_Unify.unify_simple_type typing_env thrown_ty exn_ty
       with W_Unify.Unification_simple_type_conflict (err_t1, err_t2, detail) ->
         raise
           (W_InferErrors.Infer_detailled_unification_conflict
              (W_InferErrors.UCC_throw (expr, exn_ty, thrown_ty),
               err_t1, err_t2, detail))) ;
      (* Since exception raising is a failure, it has a type being a fresh
         type variable. *)
      let quote_beta = W_CoreTypes.type_variable () in
      let (final_ty, final_map) =
        perform_infer_expr_type_postlude original_expr annotmap quote_beta in
      W_TypeInfo.add_loc_object
        final_ty.W_Algebra.sty_desc loc ;
    (final_ty, final_map)
    )
  | (`catch, [handler_expr ; tried_expr]) -> (
      let (tried_ty, tried_annotmap) =
        infer_expr_type ~bypass_typer typing_env tried_expr in
      let (handler_ty, handler_annotmap) =
        infer_expr_type ~bypass_typer typing_env handler_expr in
      (* Merge the 2 annotation maps. *)
      let annotmap = QmlAnnotMap.merge tried_annotmap handler_annotmap in
      (* The handler must be of the form: exn -> t with t being unifiable with
         [tried_ty]. *)
      let tmp_arrow_type =
        W_CoreTypes.type_arrow [W_Exceptions.type_exception ()] tried_ty in
      W_TypeInfo.add_loc_object
        tmp_arrow_type.W_Algebra.sty_desc loc ;
      (try W_Unify.unify_simple_type typing_env tmp_arrow_type handler_ty
       with W_Unify.Unification_simple_type_conflict (err_t1, err_t2, detail) ->
         raise
           (W_InferErrors.Infer_detailled_unification_conflict
              (W_InferErrors.UCC_catch
                 (original_expr, tmp_arrow_type, handler_ty),
               err_t1, err_t2, detail))) ;
       let (final_ty, final_map) =
        perform_infer_expr_type_postlude original_expr annotmap tried_ty in
      W_TypeInfo.add_loc_object
        final_ty.W_Algebra.sty_desc loc ;
     (final_ty, final_map)
    )
  | _ -> (
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "Other directive start@." ;
      #<End> ; (* <---------- END DEBUG *)
      (*  We first use the function [QmlDirectives.ty] to get the type of the
          directive. A directive is typed as an arrow taking a number of
          arguments corresponding to the number of expressions the directive
          has as arguments in the AST. *)
      let dir_qml_ty = QmlDirectives.ty core_directive dir_exprs dir_tys in
      (* See comment for the case [QmlAst.PatCoerce] in the function
         [infer_pattern_type] to understand the reason of this magic call. *)
      let (dir_qml_ty, _) =
        QmlTypes.type_of_type
          typing_env.W_TypingEnv.ty_env_qml_global dir_qml_ty in
      let dir_ty =
        W_TypingEnv.qml_type_to_simple_type
          typing_env dir_qml_ty ~is_type_annotation: false in
      W_TypeInfo.addrec_dir_object
       dir_ty.W_Algebra.sty_desc (core_directive, loc) ;
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "@[Other directive func ty: %a@]@."
        W_PrintTypes.pp_simple_type dir_ty
      #<End> ; (* <---------- END DEBUG *)
      (* Now, typecheck the arguments of the directive. *)
      let (reved_dir_args_tys, dir_args_annotmap) =
        List.fold_left
          (fun (args_tys_accu, annotmap_accu) arg ->
             let (ty, annotmap) =
               infer_expr_type ~bypass_typer typing_env arg in
             (* Accumulate the newly typed argument. *)
             let args_tys_accu' = ty :: args_tys_accu in
             (* Accumulate the annotations of the map. *)
             let annotmap_accu' = QmlAnnotMap.merge annotmap_accu annotmap in
             (args_tys_accu', annotmap_accu'))
          ([], QmlAnnotMap.empty)
          dir_exprs in

      (* Put back the list of arguments types in the correct order. *)
      let dir_args_tys = List.rev reved_dir_args_tys in

       (* Finally, type the node, as an application of the directive type to
          the arguments types. Create a fresh variable to get a handle on the
          type of the application result. *)
      let ty_app_result = W_CoreTypes.type_variable () in
      (* Finally, we unify the type of the directive with a functional type we
         create, having the above variable in positive part (to recover this
         type as the result of the application) and the types inferred for the
         arguments in negative part. *)
      let tmp_arrow_type = W_CoreTypes.type_arrow dir_args_tys ty_app_result in
      W_TypeInfo.add_loc_object tmp_arrow_type.W_Algebra.sty_desc loc ;
      W_TypeInfo.add_loc_object ty_app_result.W_Algebra.sty_desc loc ;
      (try W_Unify.unify_simple_type typing_env dir_ty tmp_arrow_type
       with W_Unify.Unification_simple_type_conflict (err_t1, err_t2, detail) ->
         raise
           (W_InferErrors.Infer_detailled_unification_conflict
              (W_InferErrors.UCC_unknown_directive
                 (original_expr, dir_ty, tmp_arrow_type),
               err_t1, err_t2, detail))) ;
      #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
      OManager.printf "@[Other directive result ty: %a@]@."
        W_PrintTypes.pp_simple_type ty_app_result
      #<End> ; (* <---------- END DEBUG *)
      let (final_ty, final_map) =
        perform_infer_expr_type_postlude
          original_expr dir_args_annotmap ty_app_result in
      W_TypeInfo.add_loc_object final_ty.W_Algebra.sty_desc loc ;
      (final_ty, final_map)
    )
