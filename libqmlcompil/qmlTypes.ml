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

(* cf mli *)
(* depends *)
module List = BaseList

(* shorthands *)
module Q = QmlAst
module QTV = QmlTypeVars

(* aliases *)
module TypeIdent = QmlAst.TypeIdent
module TypeIdentMap = QmlAst.TypeIdentMap

module FreeVars = QmlTypeVars.FreeVars
module TypeVar = QmlTypeVars.TypeVar
module RowVar = QmlTypeVars.RowVar
module ColVar = QmlTypeVars.ColVar

module TypeVarSet = QmlTypeVars.TypeVarSet
module RowVarSet = QmlTypeVars.RowVarSet
module ColVarSet = QmlTypeVars.ColVarSet

module RowVarMap = QmlTypeVars.RowVarMap
module ColVarMap = QmlTypeVars.ColVarMap
module TypeVarMap = QmlTypeVars.TypeVarMap

let (@*) = InfixOperator.(@*)
(* -- *)

type error = TyperError of Q.code_elt * (exn * exn list) (** guard for a non empty list *)
exception Exception of error

let warning fmt = OManager.warning ~wclass:QmlTyperWarnings.typer fmt

let type_err_raise t exn =
  let loc =
    QmlTyperException.loc_make (`Ty_loc t) QmlTyperException.loc_set_empty
  in
  raise (QmlTyperException.Exception (loc, exn))

let invalidTypeDefinition (tvars, tname, ty) =
  let typedef = Q.TypeName (List.map (fun t -> Q.TypeVar t) tvars, tname) in
  QmlTyperException.InvalidTypeDefinition (typedef, ty)

type bypass_typer = BslKey.t -> Q.ty option
type typescheme = (Q.ty, unit) QmlGenericScheme.tsc (* no constraints in public env *)

module ImplFieldMap = SetMap.Make(String)(TypeIdent)
module ImplFieldMapQuick = SetMap.Make(StringSet)(TypeIdent)

type abbrev_height = int

type gamma = {
  ident : typescheme IdentMap.t ;
  type_ident :
    (typescheme * abbrev_height * QmlAst.type_def_visibility) TypeIdentMap.t ;
  field_map : ImplFieldMap.t ;
  field_map_quick : ImplFieldMapQuick.t ;
}


type options =
    {
      explicit_instantiation : bool;
      value_restriction : [`disabled|`normal|`strict];
    }

let default_options =
    {
      explicit_instantiation = true; (* in case Explicit Instantiation used *)
      value_restriction = `disabled; (* in case value restriction used *)
    }

module type QML_LOW_LEVEL_TYPER =
sig
  val type_of_expr :
    ?options : options ->
    ?annotmap : Q.annotmap ->
    bypass_typer : bypass_typer ->
    gamma: gamma ->
    Q.expr ->
    gamma * Q.annotmap * Q.ty

  (* Voir mli *)
end
(** empty *)
let gamma_empty = {
  ident = IdentMap.empty ;
  type_ident = TypeIdentMap.empty ;
  field_map = ImplFieldMap.empty ;
  field_map_quick = ImplFieldMapQuick.empty ;
}

let rec ty_ty ~with_forall ~free = function
  | Q.TypeAbstract
  | Q.TypeConst _ -> free
  | Q.TypeVar typevar -> FreeVars.add_ty typevar free
  | Q.TypeArrow (le1, e2) ->
      let free = ty_ty_list ~with_forall ~free le1 in
      ty_ty ~with_forall ~free e2
  | Q.TypeRecord _ty_row -> ty_row ~with_forall ~free _ty_row
  | Q.TypeName (params, _) -> ty_ty_list ~with_forall ~free params
  | Q.TypeSum sum -> ty_sums ~with_forall ~free sum
  | Q.TypeSumSugar sum -> ty_sums_sugar ~with_forall ~free sum
  | Q.TypeForall (vars, rvars, cvars, t) ->
      if with_forall
      then ty_ty ~with_forall ~free t
      else
        let ts = TypeVarSet.from_list vars in
        let rs = RowVarSet.from_list rvars in
        let cs = ColVarSet.from_list cvars in
        let vars = FreeVars.import_from_sets ts rs cs in
        FreeVars.diff (ty_ty ~with_forall ~free t) vars

and ty_ty_list ~with_forall ~free tyl =
  List.fold_left (fun free -> ty_ty ~with_forall ~free) free tyl

and ty_row ~with_forall ~free (Q.TyRow (fields, rv)) =
  let free = List.fold_left (fun free (_, tau) -> ty_ty ~with_forall ~free tau) free fields in
  let free = Option.default_map free (fun v -> FreeVars.add_row v free) rv
  in free

and ty_sums ~with_forall ~free sum =
  let Q.TyCol (_, cv) = sum in
  let free = Option.default_map free (fun v -> FreeVars.add_col v free) cv in
  let lt = Q.column_to_records sum in
  List.fold_left (fun free _ty -> ty_ty ~with_forall ~free _ty) free lt

and ty_sums_sugar ~with_forall ~free sum =
  List.fold_left (fun free _ty -> ty_ty ~with_forall ~free _ty) free sum

let freevars_of_ty ?(with_forall=false) ?(free=FreeVars.empty) t =
  ty_ty ~with_forall ~free t
let freevars_of_row ?(with_forall=false) ?(free=FreeVars.empty) t =
  ty_row ~with_forall ~free t
let freevars_of_col ?(with_forall=false) ?(free=FreeVars.empty) t =
  ty_sums ~with_forall ~free t

  let freevars_of_typescheme =
    let f body () = freevars_of_ty body in
    QmlGenericScheme.freevars_with_cache f

  let freevars_of_gamma g =
    let free = FreeVars.empty in
    let free = IdentMap.fold (fun _ sh acc -> let free = freevars_of_typescheme sh in FreeVars.union acc free) g.ident free in
    free

(** a function to specialize types from 3 functions of maping for typevars *)
(** for rows and cols, it DOES NOT perform the merge (merge in map_row and map_col if you want) *)
let map_vars_of_ty map_ty map_row map_col =
  let ty_row ((Q.TyRow (_, rv)) as row) =
    Option.default_map row (map_row row) rv
  and ty_sums ((Q.TyCol (_, cv)) as col) =
    Option.default_map col (map_col col) cv
  in
  QmlAstWalk.Type.map_up
    (function
      | (Q.TypeVar v) as ty -> map_ty ty v
      | Q.TypeRecord row -> Q.TypeRecord (ty_row row)
      | Q.TypeSum sum -> Q.TypeSum (ty_sums sum)
      | ty -> ty)

module Scheme =
struct
  type t = typescheme
  type renaming = TypeVar.t TypeVarMap.t * RowVar.t RowVarMap.t * ColVar.t ColVarMap.t

  let empty_renaming = (TypeVarMap.empty,RowVarMap.empty,ColVarMap.empty)
  let next v = QmlGenericScheme.import FreeVars.empty (Q.TypeVar v) ()

  (* TODO: we will also need a version which keeps track between runs
     which variable is changed to which, e.g. to rename user-written
     vars consistently in a freshly parsed code. It may be good
     to write the general function and then instantiate it for
     typesheme refresh below
   *)
  let refresh_quantifier_and_renaming s =
    let ordered_quantif = QmlGenericScheme.export_ordered_quantif s in
    (** BEWARE : refreshing set and ordered quantification with coherence *)
    let typevarmap, new_typevar =
      (* we fold over ordered_quantif, to reflect the order of vars
         by the order in which we generate them (and so the order in the set);
         to keep it correct, refresh has to be performed in the order given
         and generate variables which are stricly greater than any
         variables generated before (e.g. using a global counter) *)
      List.fold_left (fun (map, set) elt ->
        let index = TypeVar.refresh elt in
        let map = TypeVarMap.add elt index map in
        let set = TypeVarSet.add index set in
        (map, set))
        (TypeVarMap.empty, TypeVarSet.empty)
        ordered_quantif.QTV.typevar
    in
    let rowvarmap, new_rowvar =
      List.fold_left (fun (map, set) elt ->
        let index = RowVar.refresh elt in
        let map = RowVarMap.add elt index map in
        let set = RowVarSet.add index set in
        (map, set))
        (RowVarMap.empty, RowVarSet.empty)
        ordered_quantif.QTV.rowvar
    in
    let colvarmap, new_colvar =
      List.fold_left (fun (map, set) elt ->
        let index = ColVar.refresh elt in
        let map = ColVarMap.add elt index map in
        let set = ColVarSet.add index set in
        (map, set))
        (ColVarMap.empty, ColVarSet.empty)
        ordered_quantif.QTV.colvar
    in
    let new_quantif =
      { QTV.
        typevar = new_typevar;
        rowvar = new_rowvar;
        colvar = new_colvar
      } in
    new_quantif, (typevarmap, rowvarmap, colvarmap)

  let apply_renaming (((typevarmap, rowvarmap, colvarmap) as renaming) : renaming) ty =
    if renaming = empty_renaming then ty else (
      (** /!\ Beware here, use the refresh substitution to refresh all variable
          according to the new quantification *)
      let map_ty ty v =
        match TypeVarMap.find_opt v typevarmap with
        | Some t -> Q.TypeVar t
        | None -> ty
      in
      let map_row ((Q.TyRow (r, _)) as row) v =
        match RowVarMap.find_opt v rowvarmap with
        | Some t -> Q.TyRow (r, Some t)
        | None -> row
      in
      let map_col ((Q.TyCol (l, _)) as col) v =
        match ColVarMap.find_opt v colvarmap with
        | Some t -> Q.TyCol (l, Some t)
        | None -> col
      in
      map_vars_of_ty map_ty map_row map_col ty
    )

  let refresh_and_renaming s =
    let new_quantif, renaming = refresh_quantifier_and_renaming s in
    let (_, body, ()) = QmlGenericScheme.export_unsafe s in
    let new_body = apply_renaming renaming body in
    let new_s = QmlGenericScheme.import new_quantif new_body () in
    new_s, renaming

  let refresh s =
    fst (refresh_and_renaming s)

  let instantiate t =
    let s = refresh t in
    let (_, body, ()) = QmlGenericScheme.export_unsafe s in
    body

  let export t =
    let t = refresh t in
    let ordered_quantif = QmlGenericScheme.export_ordered_quantif t in
    (ordered_quantif.QTV.typevar,
     ordered_quantif.QTV.rowvar,
     ordered_quantif.QTV.colvar),
    let (_, body, ()) = QmlGenericScheme.export_unsafe t in
    body

  let generalize gamma ty =
    let free_ty = freevars_of_ty ty in
    let free_gamma = freevars_of_gamma gamma in
    let quantif = FreeVars.diff free_ty free_gamma in
    QmlGenericScheme.import quantif ty ()

  let quantify ty =
    let quantif = freevars_of_ty ty in
    QmlGenericScheme.import quantif ty ()



  (* ************************************************************************ *)
  (** We check that for recursive type definitions the recursive reference
      uses the same type parameters, i.e.:
        type 'a t = ... 'b t ..., if a <> b raises InvalidTypeUsage
      We check that the type definition is not trivially cyclic, i.e we forbid
        type t('a) = t('a).
      We check that sum types do not have cases containing the same labels,
      i.e. we forbid
        type t = { A } / { B } / { A }
        type t = { A; B } / { A; B }
        type t = { A : int } / { A : char }
      but we do not forbid
        type t = { A; B } / { A; C }
        type t = { A; B } / { A }                                             *)
  (* ************************************************************************ *)
  (* TODO: This will have to be extended to mutually recursive types, once those are
     handled properly *)
  let check_definition tname tvars ty =
    let rec check_row (Q.TyRow (fields, _)) =
      List.iter (check ~top:false @* snd) fields

    and check_col (Q.TyCol (l, _)) =
      let seen_sum_cases = ref [] in
      List.iter
        (fun a_case_fields ->
           (* For the current case of the sum, check the fields making this case
              and by the way, recover all the labels presents in the record
              forming this case. *)
           let the_case_labels =
             List.map
               (fun (label, field_ty) ->
                  (* Recursively check the type of the label. *)
                  check ~top: false field_ty ;
                  (* Return the label found for this field of the row. *)
                  label)
               a_case_fields in
           (* Now, sort the labels of this case of the sum to compare them with
              those already found for the other cases of the sum type. *)
           let the_case_labels_sorted = List.sort compare the_case_labels in
           if not (List.mem the_case_labels_sorted !seen_sum_cases) then
             seen_sum_cases := the_case_labels_sorted :: !seen_sum_cases
           else
             let exc = invalidTypeDefinition (tvars, tname, ty) in
             type_err_raise ty exc)
        l


    and check ?(top=false) t =
      match t with
      | Q.TypeConst _ | Q.TypeVar _ -> ()
      | Q.TypeArrow (lt, u) ->
          List.iter check lt;
          check u
      | Q.TypeSumSugar s -> List.iter check s
      | Q.TypeSum col -> check_col col
      | Q.TypeRecord r -> check_row r
      | Q.TypeName (vars, name) ->
          List.iter check vars ;
          if TypeIdent.equal name tname then
            let ok =
              if ty == t then (
                (* We want to reject definitions trivially cyclic like
                   type t = t. This is the case when the initial type expression
                   is the same than the current one. *)
                false
               )
              else (
                try
                  let c = List.combine tvars vars in
                  let pos_ok (v, t) =
                    match t with
                    | Q.TypeVar v' -> TypeVar.equal v v'
                    | _ -> false in
                  List.for_all pos_ok c
                with Invalid_argument "List.combine" -> false) in
            if not ok then
              let exc = invalidTypeDefinition (tvars, tname, ty) in
              type_err_raise t exc
      | Q.TypeAbstract ->
          if (not top) then
            let exc =
              QmlTyperException.InvalidType (ty, `abstract_in_ty_annotation) in
            type_err_raise t exc (* "abstract" nested inside a type def *)
      | Q.TypeForall (_, _, _, t) -> check t
    in
    let rec repeated_var = function
      | [] -> false
      | x::xs -> List.exists (fun y -> TypeVar.equal x y) xs || repeated_var xs
    in
    if repeated_var tvars then
      let exc = invalidTypeDefinition (tvars, tname, ty) in
      type_err_raise ty exc
    else
      check ~top:true ty

  let definition_no_check ?(typevar=[]) ?(ty_row=[]) tname typ =
    let error () =
      let exn = invalidTypeDefinition (typevar, tname, typ) in
      type_err_raise typ exn
    in
    let build_typevar = List.fold_left
      (fun free v ->
         if TypeVarSet.mem v free then error ()
         else TypeVarSet.add v free
      ) TypeVarSet.empty
    in
    let build_rowvar = List.fold_left
      (fun free v ->
         if RowVarSet.mem v free then error ()
         else RowVarSet.add v free
      ) RowVarSet.empty
    in
    let ty_params = build_typevar typevar in
    let row_params = build_rowvar ty_row in

    let free = freevars_of_ty typ in
    let new_free =
      { QTV.
        typevar = ty_params;
        rowvar = row_params;
        colvar = ColVarSet.empty
      }
    in

    if
      TypeVarSet.subset free.QTV.typevar ty_params
        (* for now other kinds of variables are forbidden in type defs: *)
      && RowVarSet.is_empty free.QTV.rowvar
    then
      (* TODO: why do we refresh here? isn't it enough to refresh at each access *)
      refresh (QmlGenericScheme.import new_free typ ())

    else error ()

  let definition ?(typevar=[]) ?(ty_row=[]) tname typ =
    check_definition tname typevar typ;
    definition_no_check ~typevar ~ty_row tname typ

  (* we could also specialize column variables if needed *)
  let specialize ~typeident ?(ty=[]) ?(ty_row=[]) s =
    let error () =
      let exn = QmlTyperException.InvalidTypeUsage (typeident, (QmlGenericScheme.export_ordered_quantif s).QTV.typevar, ty) in
    let (_, body, ()) = QmlGenericScheme.export_unsafe s in
    type_err_raise body exn
    in
    (* The different maps are built from a fold_left2 between parameters provided and the
       ordered representation of the quantification
       Any problem by doing the fold_left2 means that the arity of type constructors are not respected
       We raise error when instantiating type with too many or too few parameters,
       but not with no parameters, since it is a nice abbreviation (e.g., [] : list, means [] : 'a list) *)
    let typevarmap, rowvarmap =
      try
        let build cons add empty refresh a b =
          let a, b =
            match a, b with
            | _::_, [] ->
                (** this is the case we want to allow *)
                a, List.map (fun var -> cons (refresh var)) a
            | _, _ -> a, b in

          List.fold_left2 (fun map index ty -> add index ty map) empty a b in

        let typevar_refresh = TypeVar.refresh in
        let rowvar_refresh = RowVar.refresh in
        build (fun s -> Q.TypeVar s) TypeVarMap.add TypeVarMap.empty typevar_refresh (QmlGenericScheme.export_ordered_quantif s).QTV.typevar ty,
        build (fun s -> Q.TyRow ([], Some s)) RowVarMap.add RowVarMap.empty rowvar_refresh (QmlGenericScheme.export_ordered_quantif s).QTV.rowvar ty_row
      with
      | Invalid_argument _ -> error ()
    in
    let map_ty ty v = Option.default ty (TypeVarMap.find_opt v typevarmap) in
    let map_row row v =
      match RowVarMap.find_opt v rowvarmap with
      | Some t ->
          let cmp_fields (f1, _) (f2, _) = String.compare f1 f2 in
          let Q.TyRow (fields1, _) = row in
          let Q.TyRow (fields2, rv) = t in
          let fields = List.uniq_unsorted ~cmp:cmp_fields
            (fields1 @ fields2) in (* so we prefer duplicates from fields1 rather than from fields2 *)
          Q.TyRow (fields, rv)
      | None -> row in
    let (_, body, ()) = QmlGenericScheme.export_unsafe s in
    map_vars_of_ty map_ty map_row (fun col _ -> col) body

  let id ty = QmlGenericScheme.import FreeVars.empty ty ()

  let explicit_forall tsc =
    let (tv,rv,cv),ty = export tsc in
      match tv,rv,cv with
        | [],[],[] -> ty
        | _ -> Q.TypeForall(tv,rv,cv,ty)

end

module Env =
struct
  type t = gamma
  let empty = gamma_empty
  module Ident =
  struct
    let find_opt id g = IdentMap.find_opt id g.ident
    let find id g = match find_opt id g with
      | Some t -> t
      | None ->
          raise (QmlTyperException.Exception
                   (QmlTyperException.loc_empty,
                    QmlTyperException.IdentifierNotFound
                      (id, IdentMap.keys g.ident)))
    let add id s g = { g with ident = IdentMap.add id s g.ident }
    let remove id g = { g with ident = IdentMap.remove id g.ident }
    let mem id g = IdentMap.mem id g.ident
    let iter f g = IdentMap.iter f g.ident
    let fold f g = IdentMap.fold f g.ident
    let map f g = {g with ident = IdentMap.map f g.ident}
    let fold_map f gamma acc =
      let acc, ident = IdentMap.fold_map f gamma.ident acc in
      acc, {gamma with ident = ident}
    let from_map map gamma =
      {gamma with ident = map}
    let to_map gamma = gamma.ident
    let pp f gamma =
      iter (fun ident tsc ->
              Format.fprintf f "@[<2>%s -> %a@]@\n" (Ident.to_string ident) QmlPrint.pp#tsc tsc
           ) gamma
  end

  module TypeIdent =
  struct
    module T = TypeIdent

    (** [TODO] Documentation. *)
    let apply_visibility scheme abbrev_height = function
      | QmlAst.TDV_public -> Some (scheme, abbrev_height)
      | QmlAst.TDV_private package ->
          (* Since types private to a package are not visible at all from
             other packages, this type must be considered as non-existant
             if we are not in its definition package. *)
          if package <> (ObjectFiles.get_current_package_name ()) then None
          else Some (scheme, abbrev_height)
      | QmlAst.TDV_abstract package ->
          (* If we are not in the type's definition package, then it
             must be considered as abstract. *)
          if package <> (ObjectFiles.get_current_package_name ()) then (
            (* Turn the body of the scheme into a [TypeAbstract]. *)
            let (quantif, _, constraints) =
              QmlGenericScheme.export_unsafe scheme in
            Some
              ((QmlGenericScheme.import quantif
                  QmlAst.TypeAbstract constraints),
               0)
          )
          else Some (scheme, abbrev_height)

    (** [TODO] Documentation of [~visibility_applies] for passes that anyway
        need to see types' structure once the typechecker ensured these types,
        even not visible are used in a consistent way. *)
    let find_opt ~visibility_applies id g =
      let opt_found = TypeIdentMap.find_opt id g.type_ident in
      match opt_found with
      | None -> None
      | Some (sch, abbrev_height, visibility) ->
          if visibility_applies then
            apply_visibility sch abbrev_height visibility
          else Some (sch, abbrev_height)

    let findi_opt ~visibility_applies id g =
      let opt_found = TypeIdentMap.findi_opt id g.type_ident in
      match opt_found with
      | None -> None
      | Some (i, (sch, abbrev_height, visibility)) -> (
          if not visibility_applies then Some (i, (sch, abbrev_height))
          else
            match apply_visibility sch abbrev_height visibility with
            | None -> None
            | Some sch' -> Some (i, sch')
        )

    let find ~visibility_applies id g =
      Option.get_exn
        (QmlTyperException.Exception
           (QmlTyperException.loc_empty,
            QmlTyperException.TypeIdentNotFound id))
        (find_opt ~visibility_applies id g)
    let findi ~visibility_applies id g =
      Option.get_exn
        (QmlTyperException.Exception
           (QmlTyperException.loc_empty,
            QmlTyperException.TypeIdentNotFound id))
        (findi_opt ~visibility_applies id g)


    (* ********************************************************************** *)
    (** {b Descr}: See .mli file.
        {b Visibility}: Exported outside this module.                         *)
    (* ********************************************************************** *)
    let raw_find id g =
      (* Since we return both the bound scheme and its visibility information,
         fetch in the environment is done ignoring visibility. *)
      let opt_found = TypeIdentMap.find_opt id g.type_ident in
      Option.get_exn
        (QmlTyperException.Exception
           (QmlTyperException.loc_empty,
            QmlTyperException.TypeIdentNotFound id))
        opt_found


    let records_field_names t =
      let handle_ty_row acc (Q.TyRow (fields, _)) =
        let handle_field_t acc (name, _) = name :: acc in
        List.fold_left handle_field_t acc fields
      in
      let rec handle_ty acc = function
        | Q.TypeSum (Q.TyCol (l, _)) ->
            List.flatten (List.map (List.map fst) l) @ acc
        | Q.TypeSumSugar sum ->
            List.fold_left handle_ty acc sum
        | Q.TypeRecord r -> handle_ty_row acc r
        | _  -> acc (* record cannot have any other unnamed type *)
      in handle_ty [] t

    let records_field_names_quick t =
      let handle_ty_row (Q.TyRow (fields, _)) = List.map fst fields in
      let rec handle_ty acc = function
        | Q.TypeSum (Q.TyCol (l, _)) ->
            List.map (List.map fst) l @ acc
        | Q.TypeSumSugar sum ->
            List.fold_left handle_ty acc sum
        | Q.TypeRecord r -> handle_ty_row r :: acc
        | _  -> acc (* record cannot have any other unnamed type *)
      in handle_ty [] t

    let add id (s, abbrev_height, visibility) g =
      let field_map =
        (* Update field map : only in the case of type sum and type record.
           Abstract type are obviously skipped. *)
        let fields =
          let (_, ty) = Scheme.export s in
          records_field_names ty in
        List.fold_left
          (fun map f -> ImplFieldMap.add f id map) g.field_map fields in
      let field_map_quick =
        (* Update field map : only in the case of type sum and type record. *)
        let fields =
          let (_, ty) = Scheme.export s in
          records_field_names_quick ty in
        let fields = List.map StringSet.from_list fields in
        List.fold_left
          (fun map f -> ImplFieldMapQuick.add f id map)
          g.field_map_quick fields in
      let type_ident =
        TypeIdentMap.add id (s, abbrev_height, visibility) g.type_ident in
      { g with
        type_ident = type_ident ; field_map = field_map ;
        field_map_quick = field_map_quick }

    let mem id g = TypeIdentMap.mem id g.type_ident

    let iter f g = TypeIdentMap.iter f g.type_ident

    let fold f g = TypeIdentMap.fold f g.type_ident

    let to_list gamma = TypeIdentMap.to_list gamma.type_ident

    let fold_map f gamma acc =
      let acc, type_ident = TypeIdentMap.fold_map f gamma.type_ident acc in
      (acc, {gamma with type_ident = type_ident })

    let map f gamma =
      let type_ident = TypeIdentMap.map f gamma.type_ident in
      { gamma with type_ident = type_ident }

    let pp f gamma =
      iter
        (fun typeident (tsc, _, _) ->
           Format.fprintf f "@[<2>%s -> %a@]@\n"
             (TypeIdent.to_string typeident) QmlPrint.pp#tsc tsc)
        gamma
  end

  module FieldMap =
  struct
    let find s g =
      let s = ImplFieldMap.find s g.field_map in
      ImplFieldMap.S.elements s
  end



  let pp f gamma =
    Format.fprintf f "@[<v>@[<v2>ident:@ %a@]@ @[<v2>types:@ %a@]@]" Ident.pp gamma TypeIdent.pp gamma

  (* Appends the definition in g2 to those of g1 *)
  let append g1 g2 =
    let ident = IdentMap.merge (fun _ x -> x) g1.ident g2.ident
    and type_ident = TypeIdentMap.merge (fun _ x -> x) g1.type_ident g2.type_ident
    and field_map = ImplFieldMap.M.merge ImplFieldMap.S.union g1.field_map g2.field_map
    and field_map_quick = ImplFieldMapQuick.M.merge ImplFieldMapQuick.S.union g1.field_map_quick g2.field_map_quick in
    { ident = ident ; type_ident = type_ident ; field_map = field_map ;
      field_map_quick = field_map_quick }

end

(** More Common Types, needed in order that differents HighTyper could share the type env *)
type typed_code_elt = (Q.ty, Scheme.t) Q.maped_code_elt
type 'schema public_env =
    {
      exported_values_idents : IdentSet.t ;
      gamma        : gamma ;
      schema       : 'schema ;
      annotmap     : Q.annotmap ;
      bypass_typer : bypass_typer ;
      had_error    : bool ;
      exception_handler : 'schema public_env -> exn -> unit ;
      display      : bool ;     (** false by default *)
      options : options ;
    }

(** Helper functions to normalize types wrt a gamma (process typenames, remove
    sugared sums, etc.) *)
let unsugar_type gamma ty =
  let error kind =
    type_err_raise ty (QmlTyperException.InvalidType (ty, kind)) in

  (** {b Descr}: Local function to unwind named type expression, i.e. to
      replace them by the efective structure that are bound to, with their
      effective arguments used to instantiate parameters of the definition the
      name is bound to. *)
  let unwind_type gamma = function
    | Q.TypeName (params, ti) ->
        let (ti, (tsc, _)) =
          Env.TypeIdent.findi ~visibility_applies: true ti gamma in
        if (Scheme.instantiate tsc) = Q.TypeAbstract then (
          (* The type name is bound to a definition that is said "abstract" or
             "extern", so return a type that is a named type with the same
             name than the initial one. This way, since the type has no
             representation, i.e. the name is not bound to a definition
             providing an explicit structure for this name, it will be
             considered as being structured as itself, hence it will be
             compatible only with itself. And that, this is really the meaning
             of an abstract type. *)
          Q.TypeName (params, ti)
        )
        else (
          (* Ok, the name of this type is bound to a type definition. So we
             instantiate this definition's scheme by the effective arguments
             aplied to the type name. Hence, we get a new type that is an
             instance of the name's definition in which parameters are replaced
             by the effective types applied to the type name. *)
          Scheme.specialize ~typeident: ti ~ty: params tsc
        )
    | t ->
        (* The type is not a named type, hence it has its own structure and
           doesn't unwind. So return it unchanged. *)
        t in

  let deal_with_duplicates l =
    let module SpecialMapForDuplicates = BaseMap.Make ( StringSet ) in
    let safe_add f s =
      if StringSet.mem f s then (
        (* There is a duplicate field inside a same record. *)
        error `duplicate_field
      )
      else StringSet.add f s in
    let undup acc fields =
      let (s, m) =
        List.fold_left
          (fun (s, m) (f, (t : QmlAst.ty)) ->
             (safe_add f s, StringMap.add f t m))
          (StringSet.empty, StringMap.empty)
          fields in
      match SpecialMapForDuplicates.find_opt s acc with
      | None -> ((SpecialMapForDuplicates.add s m acc), (Some fields))
      | Some m' when StringMap.compare (Pervasives.compare) m m' = 0 ->
          (* We are in the case where several cases of the sum have exactly
             the same fields with the same types. Hence, we have duplicate
             sum cases. Just drop this redundant case. *)
          (acc, None)
      | _ ->
          (* There is a duplicate field in different cases of the sum with
             different types. *)
          error `duplicate_field_with_diff_ty_in_sum_cases in
    snd (List.fold_left_filter_map undup SpecialMapForDuplicates.empty l) in

  let make_typesum = function
    | [r] -> Q.TypeRecord (Q.TyRow (r, None))
    | l -> Q.TypeSum (Q.TyCol (l, None)) in

  let get_fields = function
    | Q.TypeRecord (Q.TyRow (fields, None)) -> [fields]
    | Q.TypeSum (Q.TyCol (fields, None)) -> fields
    | Q.TypeRecord (Q.TyRow (_, Some _)) -> error `record_not_closed
    | _ -> error `not_a_record in

  let aux = function
    | Q.TypeSumSugar l ->
        (make_typesum @* deal_with_duplicates)
          (List.concat_map (get_fields @* unwind_type gamma) l)
    | Q.TypeRecord (Q.TyRow (fields, _)) as t ->
        ignore (deal_with_duplicates [fields]) ;
        t
    | Q.TypeSum (Q.TyCol (cases, _)) as t ->
        (* For each case of the sum, check that the record representing this
           case doesn't have duplicate fields and that is a field appears in
           different sum cases, then it doesn't have different types. *)
        ignore (deal_with_duplicates cases) ;
        t
    | t -> t in
  QmlAstWalk.Type.map_up aux ty



(* FPE says: after so many refactoring, it seems tat now this function only
   checks that names of types are used with the right arity... *)
let process_typenames ?(typedef=false) gamma ty =
  let aux ty =
    match ty with
    | Q.TypeName (tl, ti) ->
        let (ti, (ts, _)) =
          Env.TypeIdent.findi ~visibility_applies: true ti gamma in
        if (typedef || tl <> []) &&
           (List.length tl <> QmlGenericScheme.arity ts) then (
          let exn =
            QmlTyperException.InvalidTypeUsage
              (ti, (QmlGenericScheme.export_ordered_quantif ts).QTV.typevar,
               tl) in
          let (_, body, ()) = QmlGenericScheme.export_unsafe ts in
          type_err_raise body exn
         )
        else Q.TypeName (tl, ti)
    | _ -> ty in
  QmlAstWalk.Type.map_up aux ty



(** {b Visibility}: Not exported outside this module. *)
(* FPE says: test cases like type 'at = 'a   type 'a u = 'a t to see. *)
let get_abbreviation_height gamma ty =
  match ty with
  | Q.TypeName (_, ti) ->
      let (_, (_, abb_height)) =
        Env.TypeIdent.findi ~visibility_applies: true ti gamma in
      abb_height
  | Q.TypeVar _ -> -1
  | _ -> 0



let type_of_type ?(typedef = false) ?(tirec = []) gamma ty =
  let gamma =
    List.fold_left
      (fun gamma (ti,vars) ->
         let fake_rec_def =
           Scheme.definition_no_check ~typevar: vars ti Q.TypeAbstract in
         Env.TypeIdent.add
           ti
           (fake_rec_def,
            (* Abbreviations height is 0 since the definition is considered as
               binding an abstract type. *)
            0,
            QmlAst.TDV_private (ObjectFiles.get_current_package_name ()))
           gamma)
      gamma tirec in
  let ty = process_typenames ~typedef gamma ty in
  let ty = unsugar_type gamma ty in
  let abb_height = get_abbreviation_height gamma ty in
  (ty, abb_height)

let process_scheme gamma tsc =
  QmlGenericScheme.map_body_unsafe2 (type_of_type gamma) tsc
    (* safe as long as type_of_type doesn't touch type variables, etc. *)

let process_gamma ~gamma target_gamma =
  let new_gamma = Env.empty in
  let new_gamma =
    Env.Ident.fold
      (fun id tsc new_gamma ->
         let (tsc', _) = process_scheme gamma tsc in
         Env.Ident.add id tsc' new_gamma)
      target_gamma new_gamma in
  let new_gamma =
    Env.TypeIdent.fold
      (fun id (tsc, height, visibility) new_gamma ->
         let (tsc', height') = process_scheme gamma tsc in
         (* FPE says: I think this should be the case... *)
         assert (height = height') ;
         Env.TypeIdent.add id (tsc', height, visibility) new_gamma)
      target_gamma new_gamma in
  new_gamma

let process_typenames_annotmap ~gamma annotmap =
  QmlAnnotMap.map (process_typenames ~typedef:false gamma) annotmap

let process_annotmap ~gamma annotmap =
  QmlAnnotMap.map (fun ty -> fst (type_of_type gamma ty)) annotmap



let check_no_duplicate_type_defs =
  let cmp x y =
    let c = TypeIdent.compare x y in
    if c = 0 then
      raise
        (QmlTyperException.Exception
           (QmlTyperException.loc_empty,
            QmlTyperException.DuplicateTypeDefinitions
              (TypeIdent.to_string x))) ;
    c in
  ignore @* (List.sort cmp) @*
    (List.concat_map
       (function Q.NewType (_, l) ->
          List.map (fun ty_def -> ty_def.QmlAst.ty_def_name) l | _ -> []))
