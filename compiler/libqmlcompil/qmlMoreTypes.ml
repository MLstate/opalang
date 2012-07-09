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

(* see .mli *)
module Q = QmlAst
module List = BaseList


let equal_ty ?(gamma = QmlTypes.Env.empty) t1 t2 =
  QmlTypesCompare.equal_ty gamma t1 t2



(* this function is used instead of unification because:
   - we don't want unification, although we can use it because
     instantiation happens only on one side
   - we want to make sure only the type vars on one side are substituted
     this would require some big old hack to incorporate that into the typer
*)
let compare_field (s1,_) (s2,_) = String.compare s1 s2
let compare_rec l1 l2 = List.make_compare compare_field l1 l2

let is_private_or_external gamma n =
  let (tsc, _) =
    QmlTypes.Env.TypeIdent.find ~visibility_applies:false n gamma in
  let (_, body, ()) = QmlGenericScheme.export_unsafe tsc in
  match body with
  | Q.TypeAbstract -> true
  | _ -> false

let show_instantiation ~allow_partial_application gamma quant vars rows cols spec gen =
  let need_expansion expansion_history t1 t2 =
    let cpl = (t1,t2) in
    if List.mem (t1,t2) expansion_history then None
    else Some (cpl::expansion_history)
  in
  let rec aux eh(* expansion history *) spec gen =
    let aux_eh = aux in
    let aux = aux_eh eh in
    match spec, gen with
    | t, Q.TypeVar v ->
        if QmlTypeVars.FreeVars.mem_typevar v quant then
          vars := QmlTypeVars.TypeVarMap.add v t !vars
    | Q.TypeConst _ , Q.TypeConst _ -> ()

    | Q.TypeArrow (tyl1,ty1), Q.TypeArrow (tyl2,ty2) ->
        if allow_partial_application then (
          aux_arrow eh ty1 ty2 tyl1 tyl2
        ) else (
          List.iter2 aux tyl1 tyl2;
          aux ty1 ty2
        )
     (* we need to look at that case after the one with two arrows
      * so that unify -> 'a and -> 'b unify 'a with 'b and not 'a with -> 'b
      * actually i am not sure that this cases are needed anymore...
      * if ei was patched to generate real n-ary types this 'partial' flag
      * could be removed altogether *)
    | ty1, Q.TypeArrow ([],ty2)
    | Q.TypeArrow ([],ty1), ty2 when allow_partial_application ->
        aux ty1 ty2

    (* casting typerecord into typesum if needed and possible *)
    | Q.TypeRecord (Q.TyRow (fields,None)), Q.TypeSum _ ->
        aux (Q.TypeSum (Q.TyCol ([fields],None))) gen
    | Q.TypeSum _, Q.TypeRecord (Q.TyRow (fields,None)) ->
        aux spec (Q.TypeSum (Q.TyCol ([fields],None)))

    (* casting typesum into typerecord if needed and possible *)
    | Q.TypeRecord _, Q.TypeSum (Q.TyCol ([fields],None)) ->
        aux spec (Q.TypeRecord (Q.TyRow (fields, None)))
    | Q.TypeSum (Q.TyCol ([fields],None)), Q.TypeRecord _ ->
        aux (Q.TypeRecord (Q.TyRow (fields, None))) gen

    | Q.TypeRecord row1, Q.TypeRecord row2 ->
        aux_row eh row1 row2
    | Q.TypeSum sum1, Q.TypeSum sum2 ->
        aux_sum eh sum1 sum2
    | Q.TypeSumSugar _, _
    | _, Q.TypeSumSugar _ -> assert false
    | Q.TypeName (tyl1, n1), Q.TypeName (tyl2, n2) when Q.TypeIdent.equal n1 n2 ->
        List.iter2 aux tyl1 tyl2
    | Q.TypeName (tyl1, n1), t2 when not (is_private_or_external gamma n1) ->
      begin match need_expansion eh spec gen with
      | None -> ()
      | Some eh ->
        let (tsc, _) =
          QmlTypes.Env.TypeIdent.find ~visibility_applies:false n1 gamma in
        let t1 = QmlTypes.Scheme.specialize ~typeident:n1 ~ty:tyl1 tsc in
        aux_eh eh t1 t2
      end
    | t1, Q.TypeName (tyl2, n2) when not (is_private_or_external gamma n2) ->
      begin match need_expansion eh spec gen with
      | None -> ()
      | Some eh ->
        let (tsc, _) =
          QmlTypes.Env.TypeIdent.find ~visibility_applies:false n2 gamma in
        let t2 = QmlTypes.Scheme.specialize ~typeident:n2 ~ty:tyl2 tsc in
        aux_eh eh t1 t2
      end
    | Q.TypeForall (_,_,_,t1), Q.TypeForall (_,_,_,t2) ->
        (* assuming unicity of vars *)
        aux t1 t2
    | _ ->
        OManager.i_error "Fail to instantiate1:@\n%a@\nvs@\n%a@." QmlPrint.pp_base#ty spec QmlPrint.pp_base#ty gen

  and aux_arrow eh ret1 ret2 l1 l2 =
    match l1, l2 with
    | [], [] -> aux eh ret1 ret2
    | _, [] -> aux eh (Q.TypeArrow (l1,ret1)) ret2
    | [], _ -> aux eh ret1 (Q.TypeArrow (l2,ret2))
    | h1 :: l1, h2 :: l2 -> aux eh h1 h2; aux_arrow eh ret1 ret2 l1 l2

  and aux_row eh (Q.TyRow (fields1,o1) as row1) (Q.TyRow (fields2,o2) as row2) =
    let fields1 = List.StringAssoc.sort fields1 in
    let fields2 = List.StringAssoc.sort fields2 in
    let rec aux_left_only acc (fields1:(string*Q.ty) list) (fields2:(string*Q.ty) list) =
      match fields1, fields2 with
      | l, [] -> List.rev_append l acc
      | [], _ ->
        OManager.i_error "Fail to instantiate2:@\n%a@\nvs@\n%a@."
          QmlPrint.pp_base#tyrow row1 QmlPrint.pp_base#tyrow row2
      | (s1,ty1) :: t1, (s2,ty2) :: t2 ->
          assert (s1 <= s2);
          if s1 = s2 then (
            aux eh ty1 ty2;
            aux_left_only acc t1 t2
          ) else
            aux_left_only ((s1,ty1) :: acc) t1 fields2 in
    let left_only = aux_left_only [] fields1 fields2 in
    match left_only, o1, o2 with
    | l, o, Some v ->
        if QmlTypeVars.FreeVars.mem_rowvar v quant then
          rows := QmlTypeVars.RowVarMap.add v (Q.TyRow (l, o)) !rows
    | [], None, None -> ()
    | _, _, None -> assert false

  and aux_sum eh (Q.TyCol (fieldss1,o1) as col1) (Q.TyCol (fieldss2,o2) as col2) =
    let fieldss1 = List.sort compare_rec (List.map List.StringAssoc.sort fieldss1) in
    let fieldss2 = List.sort compare_rec (List.map List.StringAssoc.sort fieldss2) in
    let rec aux_rec l1 l2 =
      List.iter2
        (fun ((s1:string),ty1) (s2,ty2) ->
           assert (s1 = s2);
           aux eh ty1 ty2) l1 l2 in
    let rec aux_left_only acc (fieldss1:(string*Q.ty) list list) (fieldss2:(string*Q.ty) list list) =
      match fieldss1, fieldss2 with
      | l, [] -> List.rev_append l acc
      | [], _ ->
        OManager.i_error "Fail to instantiate3:@\n%a@\nvs@\n%a.@\nThese records should not be present: %a@."
          QmlPrint.pp_base#tycol col1 QmlPrint.pp_base#tycol col2 QmlPrint.pp_base#tycol (Q.TyCol (fieldss2, None))
      | rec1 :: t1, rec2 :: t2 ->
          let c = compare_rec rec1 rec2 in
          assert (c <= 0);
          if c = 0 then (
            aux_rec rec1 rec2;
            aux_left_only acc t1 t2
          ) else
            aux_left_only (rec1 :: acc) t1 fieldss2 in
    let left_only = aux_left_only [] fieldss1 fieldss2 in
    match left_only, o1, o2 with
    | l, o, Some v ->
        if QmlTypeVars.FreeVars.mem_colvar v quant then
          cols := QmlTypeVars.ColVarMap.add v (Q.TyCol (l, o)) !cols
    | [], None, None -> ()
    | _, _, None -> assert false in
  aux [] spec gen



let unifiable ?(gamma = QmlTypes.Env.empty) t1 t2 =
  let env = W_TypingEnv.from_qml_typing_env gamma in
  (* Attention, since we transform QML types into W types, we must create a
     variables mapping. Since the present function is used outside the context
     of type inference, there is no previously existing mapping, so a fresh new
     empty one is fully suitable. *)
  W_TypingEnv.new_empty_variables_mapping () ;
  let ty1 =
    W_TypingEnv.qml_type_to_simple_type env t1 ~is_type_annotation: false in
  let ty2 =
    W_TypingEnv.qml_type_to_simple_type env t2 ~is_type_annotation: false in
  try
    W_Unify.unify_simple_type env ty1 ty2 ;
    (* Release the variables mapping not useful anymore. *)
    W_TypingEnv.release_variables_mapping () ;
    true
  with W_Unify.Unification_simple_type_conflict (_, _, _) ->
   (* In case of error, reset the variables mappings stack to empty. *)
   W_TypingEnv.reset_empty_variables_mapping_on_error () ;
   false



let unify_and_show_instantiation ~(gamma:QmlTypes.gamma) ~allow_partial_application spec tsc =
  let (quant, gen, ()) = QmlGenericScheme.export_unsafe tsc in
  let vars = ref QmlTypeVars.TypeVarMap.empty in
  let rows = ref QmlTypeVars.RowVarMap.empty in
  let cols = ref QmlTypeVars.ColVarMap.empty in
  show_instantiation ~allow_partial_application gamma quant vars rows cols spec gen;
  let (v,r,c) = QmlTypeVars.FreeVars.export_as_lists quant in
  let lt = List.map (fun v -> try QmlTypeVars.TypeVarMap.find v !vars with Not_found -> Q.TypeVar v) v in
  let lrow = List.map (fun v -> try QmlTypeVars.RowVarMap.find v !rows with Not_found -> Q.TyRow ([], Some v)) r in
  let lcol = List.map (fun v -> try QmlTypeVars.ColVarMap.find v !cols with Not_found -> Q.TyCol ([], Some v)) c in
  (lt, lrow, lcol)
