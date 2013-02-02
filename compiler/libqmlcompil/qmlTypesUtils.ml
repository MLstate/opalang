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

(* see .mli *)

(* depends *)
module Format = Base.Format
module List = BaseList

(* shorthands *)
module Q = QmlAst

(* aliases *)
module TypeIdent = QmlAst.TypeIdent
module TypeIdentSet = QmlAst.TypeIdentSet
module TypeVarSet = QmlTypeVars.TypeVarSet

(* refactoring in progress *)
let (|>) = InfixOperator.(|>)
let (@*) = InfixOperator.(@*)

(* go *)

module Basic = struct
  let string = Q.TypeConst(Q.TyString)
end

module Inspect =
struct
(** Way too many functions with very close names !! Which one should I use ? Needs doc !! *)
  (* Here there are many function in "*_no_sum" version, which are
     intended for compatibility with old qmlkernel (where there were lists
     instead of sums). There is no good reason to use them any more.
  *)

  (* Can raise exception [QmlTyperException.Exception]. *)
  let find_and_specialize gamma typeident args =
    (* This function is used by the back-end and needs to access representation
       of types anyway. The typechecking will have to have ensured that types
       even if not legally visible were used in a consistent way. *)
    let (typescheme, _) =
      QmlTypes.Env.TypeIdent.find ~visibility_applies: false typeident gamma in
    let out = QmlTypes.Scheme.specialize ~typeident ~ty: args typescheme in
    begin
      #<If:TYPER>
        let compare_record (a, _) (b, _) = Pervasives.compare a b in
        let sort_record r = List.sort compare_record r in
        let compare_sum a b =
          let a = sort_record a in
          let b = sort_record b in
          (List.make_compare compare_record) a b
        in
        let check compare print li =
          match li with
          | hd::tl ->
              if List.fold_left
                (fun (b, s) s2 ->
                  let c = compare s s2 in
                  if c >= 0 then (true, s2)
                  else (b, s2)
              )
                (false, hd) tl
                |> fst
              then (
                print ();
                let err_ctxt = QmlError.Context.ty out in
                QmlError.i_error
                  None err_ctxt
                  "the typename : %s has non-ordonned fields"
                  (TypeIdent.to_string typeident)
              )
              else ()
          | _ -> ()
        in
        begin match out with
        | Q.TypeSum (Q.TyCol (li, _)) ->
            check compare_sum (fun () ->
              List.iter (fun li ->
                prerr_endline (String.concat ", " (List.map fst li))
              ) li
            ) li
        | Q.TypeRecord (Q.TyRow (li, _) ) ->
            check compare_record (fun () -> List.iter (prerr_endline @* fst) li) li
        | _ -> ()
        end;
        #<End>;
      out
    end

  let follow_alias gamma = function
    | Q.TypeName (l, s)-> Some (find_and_specialize gamma s l)
    | _ -> None

  let follow_alias_noopt ?until gamma t =
    let rec aux memo t =
      match t with
      | Q.TypeName (_, s) when Some (QmlAst.TypeIdent.to_string s) = until -> t
      | Q.TypeName (l, s) ->
          if TypeIdentSet.mem s memo then t (* infinite named type *) else
          let memo = TypeIdentSet.add s memo in
          aux memo (find_and_specialize gamma s l)
      | ty -> ty
    in
    aux TypeIdentSet.empty t

  let follow_alias_noopt_private ?until gamma t =
    (* TODO : Something else of follow_alias_noopt... *)
    follow_alias_noopt ?until gamma t



  (* ************************************************************************ *)
  (** {b Descr}: See .mli file.
      {b Visibility}: Exported outside this module.                           *)
  (* ************************************************************************ *)
  exception Escaping_private_type of QmlAst.ty



  (* ************************************************************************ *)
  (** {b Descr}: See .mli file.
      This function makes a descent on the type. Each time it finds a named type
      it looks in the environment for the bound type definition and visibility.
      If the visibility is @private, an error is raise.
      If the visibility is @abstract, then since the abstraction hides anything,
      it also hides possibly present private types that are hence not visible
      anymore.
      If it is @public, it continues descending on the body of the scheme
      bound to the name. Note that we do not specialize the scheme: we only
      inspect the arguments of the named type expression and the body of the
      scheme, telling that if there is no escaping type in the arguments, then
      when these arguments instantiate the scheme, they wont add any escaping
      possibility. And by inspecting the body of the scheme, we inspect its
      structure without boring about the arguments since they were already
      checked. If the structure of the scheme's body doesn't cause escaping,
      then finally all is fine.
      {b Visibility}: Exported outside this module.                           *)
  (* ************************************************************************ *)
  let check_no_private_type_escaping gamma initial_ty =
    let rec inspect memo ty =
      match ty with
      | Q.TypeConst _ | Q.TypeVar _ | Q.TypeAbstract -> ()
      | Q.TypeArrow (args_ty, res_ty) ->
          List.iter (inspect memo) args_ty ;
          inspect memo res_ty
      | Q.TypeRecord (Q.TyRow (fields, _)) ->
          List.iter (fun (_, field_ty) -> inspect memo field_ty) fields
      | Q.TypeSum (Q.TyCol (cases, _)) ->
          List.iter
            (fun fields ->
              List.iter (fun (_, field_ty) -> inspect memo field_ty) fields)
            cases
      | Q.TypeSumSugar _ -> assert false  (* Should not remain some. *)
      | Q.TypeName (args_ty, name) ->
          let memo' =
            if not (TypeIdentSet.mem name memo) then (
              (* Recover the definition bound to this name. *)
              let (sch, _, visibility) =
                QmlTypes.Env.TypeIdent.raw_find name gamma in
              let extended_memo = TypeIdentSet.add name memo in
              (match visibility with
              | Q.TDV_public ->
                  (* As writen in the header of this function, since we always
                     inspect the arguments of the named type expression, we
                     don't need to explicitely instantiate the scheme and
                     directly inspect its body. Instantiation would only have
                     grafted in the body the arguments types. And because these
                     ones will be checked apart, no need to do the job twice. *)
                  let (_, body, _) = QmlGenericScheme.export_unsafe sch in
                  inspect extended_memo body
              | Q.TDV_private _ ->
                  (* This type being private, we can't allow it. *)
                  raise (Escaping_private_type ty)
              | Q.TDV_abstract _ ->
                  (* Ok, no need to descend deeper since the abstraction hides
                     anything, also possibly present private types. *)
                  ()) ;
              extended_memo
             )
            else memo in
          (* In any case, inspect the arguments of the named type expression. *)
          List.iter (inspect memo') args_ty
      | Q.TypeForall (_, _, _, body_ty) -> inspect memo body_ty in
    inspect TypeIdentSet.empty initial_ty

  let rec get_deeper_typename gamma ty =
    match ty with
    | Q.TypeName (args, n) -> (
        let (_, _, vis) = QmlTypes.Env.TypeIdent.raw_find n gamma in
        match vis with
        | QmlAst.TDV_public -> (
            let aliased_ty = find_and_specialize gamma n args in
            match aliased_ty with
            | Q.TypeName _ -> get_deeper_typename gamma aliased_ty
            | _ -> ty
           )
        | QmlAst.TDV_abstract _ | QmlAst.TDV_private _ -> ty
       )
    | _ -> ty

  let rec get_deeper_type_until gamma f ty =
    if f ty then ty
    else
      match follow_alias gamma ty with
      | Some ty -> get_deeper_type_until gamma f ty
      | None -> ty

  let rec is_type_arrow gamma ty =
    match follow_alias_noopt gamma ty with
    | Q.TypeArrow _ -> true
    | _ -> false

  let is_type_void gamma ty =
    match follow_alias_noopt gamma ty with
    | Q.TypeRecord (Q.TyRow ([], None)) ->
        true
    | Q.TypeSum (Q.TyCol ([ [ ] ], None)) ->
        true
    | _ -> false

  let rec is_type_bool gamma ty =
    match follow_alias_noopt gamma ty with
    | Q.TypeRecord (Q.TyRow ([ ( "false" | "true" ), ty], None)) ->
        is_type_void gamma ty
    | Q.TypeSum (Q.TyCol ([ ["false", tyf ] ; ["true", tyt ] ], None)) ->
        (is_type_void gamma tyt) && (is_type_void gamma tyf)
    | _ -> false

  let get_arrow_params gamma ty =
    match follow_alias_noopt gamma ty with
    | Q.TypeArrow (tl, _t2) -> Some tl
    | _ -> None

  let rec get_arrow_through_alias_and_private gamma ty =
    match follow_alias_noopt gamma ty with
    | Q.TypeArrow (tl, t2) -> Some (tl,t2)
    | _ -> None

  let get_data_type_of_map gamma ty =
    match follow_alias_noopt_private ~until:"ordered_map" gamma ty with
    | Q.TypeName ([_; dty; _], _) -> dty
    | _ -> raise Not_found

  let get_type_potentially_in_non_pure_type gamma ty=
    let rec aux memo ty acc =
      QmlAstWalk.Type.traverse_fold (fun tra acc ty ->
        match ty with
        | Q.TypeArrow (args, res) ->
          (* TODO see if return type really need to be here *)
          (res::args) @ acc (* potentially hidden by partial application *)
        | Q.TypeName (params, _) ->
          let ty = get_deeper_typename gamma ty in
          (match follow_alias gamma ty with
          | Some Q.TypeAbstract
          | None -> params@acc (* no type name implementation => potentially not pure *)
          | Some implem -> if List.mem ty memo then acc else aux (ty::memo) implem acc
          )
        | Q.TypeAbstract -> assert false
        | Q.TypeForall (q1, q2, q3, body) -> (*ty::acc*) (* TODO do not ignore for all quantification *)
          let acc1 = aux memo body [] in
          (List.map (fun ty -> Q.TypeForall(q1,q2,q3,ty)) acc1)@acc
        | ty -> tra acc ty (* recurse in type components *)
      ) acc ty
    in
    aux [] ty []
end

module TypeArrow =
struct
  type 'a type_arrow_utils = Q.ty list -> Q.ty -> 'a

  let curryfied_arity args ty =
    let rec aux cpt = function
      | Q.TypeArrow (args,t) -> aux (cpt + (List.length args)) t
      | _ -> cpt
    in
    aux (List.length args) ty
end
