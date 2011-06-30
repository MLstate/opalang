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
   Author: 2011, François Pessaux         <francois.pessaux@mlstate.com>
*)

(**
   Strongly mapped on hmx_Compare.ml of the old HMX typechecker.
   This module implements comparison on QML types with the semantics that
   old HMX typechecker had when it first transformed QML types into HMX types
   then compared the HMX types.
   The idea is to remove the bridge between QML and HMX primitives since HMX
   is not used anymore. However, QML still used some parts of HMX functions
   for various things. That's the reason why this module exists.
*)

(* depends *)
module List = BaseList

(* shorthands *)
module Q = QmlAst



module TyPairSet : (BaseSetSig.S with type elt = QmlAst.ty * QmlAst.ty) =
  BaseSet.Make (
    struct
      type t = QmlAst.ty * QmlAst.ty
      let compare = Pervasives.compare
    end
  )



let typeident_is_abstract ti = QmlAst.TypeIdent.is_external_ty ti



(**
   sort the list of type variables [bvs] bounds in a TypeForall
   according to the apparition order in the quantified type [t]
*)
(* TODO : extend to row and column variables *)
let sort_bound_vars env bvs t =
  let rec aux memo (sbvs, bvs) t =
    match t with
    | Q.TypeSumSugar _ ->
        (* They should have all disappeared. *)
        assert false
    | Q.TypeConst _ -> (sbvs, bvs)
    | Q.TypeVar v ->
        if List.mem v bvs then ((v :: sbvs), (List.remove_first v bvs))
        else (sbvs, bvs)
    | Q.TypeArrow (lt, u) ->
        aux memo (List.fold_left (aux memo) (sbvs, bvs) lt) u
    | Q.TypeRecord (Q.TyRow (fields, _)) -> aux_fields memo (sbvs, bvs) fields
    | Q.TypeSum (Q.TyCol (l_fields, _)) ->
        List.fold_left (aux_fields memo) (sbvs, bvs) l_fields
    | Q.TypeName (lt, tn) ->
        let tsc =
          QmlTypes.Env.TypeIdent.find ~visibility_applies: true tn env in
        let acc =
          if List.mem t memo then (sbvs, bvs)
          else
            let t' = QmlTypes.Scheme.specialize ~typeident: tn ~ty: lt tsc in
            aux (t :: memo) (sbvs, bvs) t' in
        (* explore phantom types, because they don't appear in the body *)
        let (vars, _, _) = QmlGenericScheme.export_unsafe tsc in
        let (typevars, _, _) = QmlTypeVars.FreeVars.export_as_lists vars in
        let count_t_c_freevars t _c = QmlTypes.freevars_of_ty t in
        let phantomvars =
          QmlGenericScheme.phantomvars_with_cache count_t_c_freevars tsc in
        let fold_fun acc v t =
          if QmlTypeVars.FreeVars.mem_typevar v phantomvars then aux memo acc t
          else acc in
        List.fold_left2 fold_fun acc typevars lt
    | Q.TypeAbstract -> (sbvs, bvs)
    | Q.TypeForall (_, _, _, t) -> aux memo (sbvs, bvs) t

  and aux_fields memo (sbvs, bvs) fields =
    let lt = List.map snd fields in
    List.fold_left (aux memo) (sbvs, bvs) lt in

  (* Note that it is legal to have variable not "seen", i.e. variable remaining
     in [bvs] and not sent to [sbvs] after sorting because when a type name is
     defined as an  abstract type, since this latter doesn't have parameter,
     parameters of the type name do not appear in the instantiation hence are
     not "seen". As a result, they in effect are not removed from the list [bvs]
     and not added in the list [sbvs]. *)
  let (sbvs, _) = aux [] ([], bvs) t in
  List.rev sbvs



let sort_row_fields fields =
  List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2) fields



let sort_col_cases (Q.TyCol (cases, ending)) =
  (* First, for each case, sort the fields of the row forming the case. *)
  let cases' = List.map sort_row_fields cases in
  (* Now, sort each case according to the fields it has. *)
  let cases'' =
    List.sort
      (fun case1 case2 ->
         List.make_compare
           (fun (n1, _) (n2, _) -> String.compare n1 n2) case1 case2)
      cases' in
  QmlAst.TyCol (cases'', ending)



let rec test_tys_eq env ?(bound_vars=[]) memo t1 t2 =
  let call_aux ?(bound_vars=bound_vars) = test_tys_eq env ~bound_vars memo in
  let is_bound v = List.mem v bound_vars in
  if t1 == t2 then true (* speedup *) else
  match t1, t2 with
  | ((Q.TypeConst c), (Q.TypeConst d)) -> c = d
  | ((Q.TypeVar v1), (Q.TypeVar v2)) when is_bound v1 && not (is_bound v2) ->
      false
  | ((Q.TypeVar v1), (Q.TypeVar v2)) when not (is_bound v1) && is_bound v2 ->
      false
  | ((Q.TypeVar v1), (Q.TypeVar v2)) -> v1 = v2
  | ((Q.TypeArrow (lt, t)), (Q.TypeArrow (lu, u))) ->
      ((List.length lt) = (List.length lu)) &&
      (List.for_all2 (call_aux ~bound_vars) lt lu) &&
      (call_aux ~bound_vars t u)
  | ((Q.TypeSum ts), (Q.TypeSum us)) -> cmp_col env memo ts us
  | ((Q.TypeRecord rx), (Q.TypeRecord ry)) -> cmp_row env memo rx ry
  | (Q.TypeRecord (Q.TyRow (tfs, trv))), (Q.TypeSum (Q.TyCol (ufs, ucv))) -> (
      match trv with
      | Some _ -> false
      | None ->
          match ucv with
          | Some _ -> false
          | None ->
              (* No row/column variables -> compare fields *)
              (match ufs with
              | [ one ] ->
                  (* To be "equal", the sum must have only 1 case. *)
                  sort_and_cmp_fields env memo tfs one
              | _ -> false)
     )
  | ((Q.TypeSum _), (Q.TypeRecord _)) -> call_aux ~bound_vars t2 t1
  | ((Q.TypeName (ts, tn)), (Q.TypeName (us, un))) ->
      if TyPairSet.mem (t1, t2) memo then true (* already seen, assumed equal *)
      else (
        let memo = TyPairSet.add (t1, t2) (TyPairSet.add (t2, t1) memo) in
        let (tn, te) =
          QmlTypes.Env.TypeIdent.findi ~visibility_applies: true tn env in
        let (un, ue) =
          QmlTypes.Env.TypeIdent.findi ~visibility_applies: true un env in
        (* First, perform a test ensuring that both types have the same names
           and the same arguments (by induction, they hence will be the
           same). *)
        let are_equal_by_name =
          (QmlAst.TypeIdent.equal tn un) &&
           ((List.length ts) = (List.length us)) &&
           (List.for_all2 (call_aux ~bound_vars) ts us) in
        (* Types are equal just by their name and inductively by their
           argument, then we are done, test is true. *)
        if are_equal_by_name then true
        else (
          (* Ok, we are in the case where the 2 types are not equal by names.
             This means that they have different name and/or different
             arguments.
             If these types are abstract, then they have no more internal
             representation (i.e. they are not abbreviations that could
             point on a same representation, and they don't have any own
             structure as told just above) and hence they are *different*. *)
          if (typeident_is_abstract tn && typeident_is_abstract un) then false
          else (
            let t =
              if typeident_is_abstract tn then QmlAst.TypeName (ts, tn)
              else QmlTypes.Scheme.specialize ~typeident: tn ~ty: ts te in
            let u =
              if typeident_is_abstract un then QmlAst.TypeName (us, un)
              else QmlTypes.Scheme.specialize ~typeident: un ~ty: us ue in
            (* FPE: Dirty fix for ticket OPA-485. In fact, when comparing
               1 type named different, testing if they are both abstract
               (hence not equal) using the above:
                  [(typeident_is_abstract tn && typeident_is_abstract un)]
               is wrong. This test only check the tag of the ident. With
               new @private and @abstract types, this tag seems not to be
               informed whether a type is @abstract or @private. Hence, it
               doesn't see that the types are @abstract and continues testing
               on their structure. Since their structure are both
               [QmlAstTypeAbstract], we go in the case where types are == and
               and then the 2 different abstract types finally are considered
               equal, which is totally wrong.
               So, when we arrive here, we know that both named types have
               not already be seen, have different names, the test on whether
               they are both abstract based on their idents failed, so we
               ensure that the bodies of the schemes bound to these 2 different
               named types are not both [QmlAst.TypeAbstract]. If they are not,
               we can safely recurse, otherwise, if they are both, we know
               that they are 2 different named types abstract hence they are
               not equal.
               A better fix should be to make the tags in the ident consistent
               with the new notion of @abstract / @private types. I don't know
               if this is possible since such types are not abstract in their
               definition package and are either not visible or abstract outside
               their definition package. So I don't know yet if the mechanism
               of tags in idents can support this change of status.
               Other solution, may be remove the tags in idents mechanism if
               this has no real meaning or is not consistent.
               This issue is to be investigated to finally arrive to a better
               fix of the ticket OPA-485. *)
            if (t = QmlAst.TypeAbstract) && (u = QmlAst.TypeAbstract) then false
            else test_tys_eq env ~bound_vars memo t u
           )
         )
       )
  | ((Q.TypeForall _), _) | (_, (Q.TypeForall _)) ->
      (* let env_vars = env.Hmx_env.freevars_of_idents in *)
      let t = (* generalize_in_type env_vars *) t1 in
      let u = (* generalize_in_type env_vars *) t2 in
      let res = (
        match (t, u) with
        | Q.TypeForall (t_bvs, t_row_vars, t_col_vars, t),
          Q.TypeForall (u_bvs, _, _, u) ->
            (* TODO : comparison is not correct !!! *)
            (* quantification over row and column variables is not handled *)
            let t_bvs = sort_bound_vars env t_bvs t in
            let u_bvs = sort_bound_vars env u_bvs u in
            if (List.length t_bvs) <> (List.length u_bvs) then false
            else (
              (* At this point, we are sure that the lists of quantified vars
                 are the same length. So we can safely transform the type t
                 into a pseudo scheme, call QmlTypes.instantiate on it with
                 a fake type ident since lists of quantified vars being the
                 same length, we won't get an error, so the fake ident won't
                 re-spring. *)
              let fake_ident =
                QmlAst.TypeIdent.of_string "__icmpty_broken_if_seen " in
              let fake_quantif =
                QmlTypeVars.FreeVars.add_list
                  (t_bvs, t_row_vars, t_col_vars) QmlTypeVars.FreeVars.empty in
              let fake_sch = QmlGenericScheme.import fake_quantif t () in
              let fake_ml_vars = List.map (fun v -> QmlAst.TypeVar v) t_bvs in
              let fake_row_vars =
                List.map (fun v -> QmlAst.TyRow ([], (Some v))) t_row_vars in
              (* Instantiate the pseudo scheme built from the body of t.
                 Instantiation is done by replacing its variables by those of
                 u. *)
              let t =
                QmlTypes.Scheme.specialize
                  ~typeident: fake_ident ~ty: fake_ml_vars
                  ~ty_row: fake_row_vars fake_sch in
              call_aux ~bound_vars: (t_bvs @ bound_vars) t u
             )
        | ((Q.TypeForall _), _) | (_, (Q.TypeForall _)) -> false
        | _ -> assert false (* at least one of them should be a TypeForall *)
       ) in
       res
  | ((Q.TypeName (ts, tn)), u) ->
      (* beware of type 'a nil = {nil:unit}, then int nil = float nil *)
      if TyPairSet.mem (t1, t2) memo then false
      else (
        let memo = TyPairSet.add (t1, t2) memo in
        let (tn, te) =
          QmlTypes.Env.TypeIdent.findi ~visibility_applies: true tn env in
        if typeident_is_abstract tn then false
        else (
          let t = QmlTypes.Scheme.specialize ~typeident: tn ~ty: ts te in
          test_tys_eq env memo t u
        )
      )
  | (_, (Q.TypeName (_, _))) -> call_aux ~bound_vars t2 t1
  | (Q.TypeAbstract, _) | (_, Q.TypeAbstract) -> false
  | (_, _) ->
      (* Different constructors or not matching extern abstract type:
         fall back on Pervasives' comparison *)
      t1 = t2



and sort_and_cmp_fields env memo tfs ufs =
  let cmp_field (tfn, tft) (ufn, uft) =
    (tfn = ufn) && (test_tys_eq env memo tft uft) in
  (* First, compare lists lengths. *)
  if (List.length tfs) <> (List.length ufs) then false
  else (
    (* Sort the fields by name. *)
    let tfs' = sort_row_fields tfs in
    let ufs' = sort_row_fields ufs in
    List.for_all2 cmp_field tfs' ufs'
   )




(* Fields will be sorted by name by calling [sort_and_cmp_fields]. No need
   to sort them in this function. *)
and cmp_row env memo (Q.TyRow (tfs, tv)) (Q.TyRow (ufs, uv)) =
  let row_var_eq =
    (match (tv, uv) with
    | (None, None) -> true
    | (None, (Some _)) | ((Some _), None) -> false
    | ((Some v1), (Some v2)) -> (QmlAst.RowVar.equal v1 v2)) in
  row_var_eq && (sort_and_cmp_fields env memo tfs ufs)



(* Since to sort cases, we need to sort fields of each row forming a case,
   to avoid sorting several times, we don't use [cmp_row] and
   [sort_and_cmp_fields] and directly sort the whole column in one shot here. *)
and cmp_col env memo (Q.TyCol (_tfs, tv) as tcol) (Q.TyCol (_ufs, uv) as ucol) =
    let col_var_eq =
      (match (tv, uv) with
      | (None, None) -> true
      | (None, (Some _)) | ((Some _), None) -> false
      | ((Some v1), (Some v2)) -> QmlAst.ColVar.equal v1 v2) in
    if not col_var_eq then false
    else (
      (* Sort the cases. *)
      let tcol = sort_col_cases tcol in
      let ucol = sort_col_cases ucol in
      let ts = Q.column_to_records tcol in
      let us = Q.column_to_records ucol in
      ((List.length ts) = (List.length us)) &&
      (List.for_all2 (test_tys_eq env memo) ts us)
     )


let equal_ty env t u = test_tys_eq env TyPairSet.empty t u
