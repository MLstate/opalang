(*
    Copyright © 2011-2013 MLstate

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
(* cf mli *)

(* depends *)
module List = BaseList

(* aliases *)
module Db = QmlAst.Db
module TU = Traverse.Utils

(* shorthands *)
module Q = QmlAst

let add_void_acc f = fun () v->(), f v
let rm_void_acc f = fun v-> snd (f () v)
let make_fold fold_map f acc elt = fst (fold_map (fun acc v-> f acc v,   v) acc elt)
let make_map  fold_map f     elt = rm_void_acc  (fold_map (add_void_acc f)) elt
let make_iter fold_map f     elt = fst (fold_map (fun ()  v-> (f v:unit),   v) ()  elt)


module Row =
struct

  let rec aux_flag tau = Some tau

  let fold_left fct acc r =
    let Q.TyRow (fs, _) = r in
    List.fold_left (fun acc (f, t) -> fct acc f t) acc fs

  let fold_left_map fct acc r =
    let Q.TyRow (fs, v) = r in
    let acc, ffs = List.fold_left_map_stable
      (fun acc ((f, t) as c) ->
         let acc, ft = fct acc t in
         acc,
         if t == ft then c else
           (f, ft)
      ) acc fs in
    acc,
    if fs == ffs then r else
      Q.TyRow (ffs, v)

  let fold_right fct r acc =
    let Q.TyRow (fs, _) = r in
    List.fold_right (fun (f, t) acc -> fct f t acc) fs acc

  let elements (Q.TyRow (fs, _)) = fs

  let length (Q.TyRow (fs, _)) = List.length fs

  let compare (f, _) (f', _) = String.compare f f'

  let ordered_elements row = List.sort compare (elements row) (** eventually use a heap with a fold_left *)

  let ordered_fold fct acc row =
    let fct acc (f, t) = fct acc f t in
    List.fold_left fct acc (ordered_elements row)

  let pos_of_field row =
    let len = ref 0 in
    let li = fold_left (fun acc f o -> incr(len); (f, o)::acc) [] row in
    let len = !len and li = List.rev li in
    let fields = Array.make len ("", QmlAst.typeNull) in
    List.iteri (fun fo i -> fields.(i) <- fo) li;
    let map = List.fold_left_i (fun map (f, o) i -> StringMap.add f (i, o) map) StringMap.empty li in
    fields, map

  let has_field f (Q.TyRow (fs, _)) = List.mem_assoc f fs
  let get_field f (Q.TyRow (fs, _)) = List.assoc_opt f fs
end

module Col =
struct
  let fold_left_map fct acc col =
    let Q.TyCol (fsl, cv) = col in
    let acc, ffsl =
      List.fold_left_map_stable
        (List.fold_left_map_stable
           (fun acc ((f, t) as c) ->
              let acc, ft = fct acc t in
              acc,
              if t == ft then c else
                (f, ft)
           )
        )
        acc fsl in
    acc,
    if fsl == ffsl then col else
      Q.TyCol (ffsl, cv)

  let fold_records f acc col =
    let Q.TyCol (fldslist, _colvar) = col in
    List.fold_left (fun acc flds -> f acc (Q.TypeRecord (Q.TyRow (flds, None)))) acc fldslist
end

(** over interface, cf mli *)
module Ty_sums =
struct
  let elements l = l
  let fold f l a = List.fold_left (fun t a -> f a t) a l
end

module Type =
struct
(*
  module Subs : TraverseInterface.S with type 'a t = ty constraint 'a = 'b * 'c * 'd = struct
    type 'a t = ty constraint 'a = 'b * 'c * 'd

    let subs_cons ty =
      let fun0 e = function [] -> e | _ -> assert false in
      let fun1 f = function [x] -> f x | _ -> assert false in
      let fun2 f = function [x; y] -> f x y | _ -> assert false in
      let rec list_combine_rem l1 l2 = match l1, l2 with
        | x1::r1,x2::r2 -> let r,rem = list_combine_rem r1 r2 in
          (x1, x2)::r, rem
        | [],l -> [],l
        | _, [] -> assert false in
      match ty with
      | (TypeConst _ | TypeVar _ | TypeAbstract) -> fun0 ty, []
      | TypeArrow (lt, t) ->
        ((fun tlt ->
          match tlt with
          | [] -> assert false
          | t :: lt -> TypeArrow (lt, t)),
         t :: lt)
      | TypeRecord (TyRow (row, rv)) ->
          let fields, tys = List.split row in
          (fun tys ->
            let row, rest = list_combine_rem fields tys in
            assert (rest = []);
            TypeRecord (TyRow (row, rv))), tys
      | TypeSum (TyCol (col, cv)) ->
          (* something special is happening here (this may or may not be what you want) *)
          let recs = List.map (fun row -> TypeRecord (TyRow (row, None))) col in
          (fun recs ->
            let col = List.map (function
              | TypeRecord (TyRow (row, None)) -> row
              | _ -> assert false) recs in
            TypeSum (TyCol (col, cv))), recs
      | TypeSumSugar tys ->
          (fun tys -> TypeSumSugar tys), tys
      | TypeName (tys, ti) ->
          (fun tys -> TypeName (tys, ti)), tys
      | TypeForall (vars, rvars, cvars, t) ->
          fun1 (fun t -> TypeForall (vars, rvars, cvars, t)), [t]
  end
*)
  module S2 : TraverseInterface.S2 with type 'a t = Q.ty constraint 'a = 'b * 'c * 'd =
  struct
    type 'a t = Q.ty constraint 'a = 'b * 'c * 'd

     let foldmap_field_stable tra acc ((x, y) as c) =
       let acc, y' = tra acc y in
       acc,
       if y == y' then c else (x, y')

     let foldmap tra acc t =
       match t with
       | Q.TypeConst _
       | Q.TypeVar _ -> acc, t
       | Q.TypeArrow (tyl, ty) ->
           let acc, ftyl = List.fold_left_map_stable tra acc tyl in
           let acc, fty = tra acc ty in
           acc,
           if tyl == ftyl && ty = fty then t else
             Q.TypeArrow (ftyl, fty)
       | Q.TypeRecord (Q.TyRow (fields, rowvar)) ->
           let acc, ffields = List.fold_left_map_stable (foldmap_field_stable tra) acc fields in
           acc,
           if fields == ffields then t else
             Q.TypeRecord (Q.TyRow (ffields, rowvar))
       | Q.TypeSum (Q.TyCol (l_fields, colvar)) ->
           let acc, fl_fields = List.fold_left_map_stable (List.fold_left_map_stable (foldmap_field_stable tra)) acc l_fields in
           acc,
           if l_fields == fl_fields then t else
             Q.TypeSum (Q.TyCol (fl_fields, colvar))
       | Q.TypeSumSugar tyl ->
           let acc, ftyl = List.fold_left_map_stable tra acc tyl in
           acc,
           if tyl == ftyl then t else
             Q.TypeSumSugar ftyl
       | Q.TypeName (tyl, typeident) ->
           let acc, ftyl = List.fold_left_map_stable tra acc tyl in
           acc,
           if tyl == ftyl then t else
             Q.TypeName (ftyl, typeident)
       | Q.TypeAbstract -> acc, t
       | Q.TypeForall (typevars, rowvars, colvars, ty) ->
           let acc, fty = tra acc ty in
           acc,
           if ty == fty then t else
             Q.TypeForall (typevars, rowvars, colvars, fty)

     let iter_field tra (_, ty) = tra ty

     let iter tra t =
      match t with
      | Q.TypeConst _
      | Q.TypeVar _ -> ()
      | Q.TypeArrow (tyl, ty) ->
          List.iter tra tyl;
          tra ty
      | Q.TypeRecord (Q.TyRow (fields, _rowvar)) ->
          List.iter (iter_field tra) fields
      | Q.TypeSum (Q.TyCol (l_fields, _colvar)) ->
          List.iter (List.iter (iter_field tra)) l_fields
      | Q.TypeSumSugar tyl ->
          List.iter tra tyl
      | Q.TypeName (tyl, _typeident) ->
          List.iter tra tyl
      | Q.TypeAbstract -> ()
      | Q.TypeForall (_typevars, _rowvars, _colvars, ty) ->
          tra ty

     let map_field_stable tra ((x, y) as c) =
       let y' = tra y in
       if y == y' then c else (x, y')

     let map tra t =
       match t with
       | Q.TypeConst _
       | Q.TypeVar _ -> t
       | Q.TypeArrow (tyl, ty) ->
           let mtyl = List.map_stable tra tyl in
           let mty = tra ty in
           if tyl == mtyl && ty = mty then t else
             Q.TypeArrow (mtyl, mty)
       | Q.TypeRecord (Q.TyRow (fields, rowvar)) ->
           let mfields = List.map_stable (map_field_stable tra) fields in
           if fields == mfields then t else
             Q.TypeRecord (Q.TyRow (mfields, rowvar))
       | Q.TypeSum (Q.TyCol (l_fields, colvar)) ->
           let ml_fields = List.map_stable (List.map_stable (map_field_stable tra)) l_fields in
           if l_fields == ml_fields then t else
             Q.TypeSum (Q.TyCol (ml_fields, colvar))
       | Q.TypeSumSugar tyl ->
           let mtyl = List.map_stable tra tyl in
           if tyl == mtyl then t else
             Q.TypeSumSugar mtyl
       | Q.TypeName (tyl, typeident) ->
           let mtyl = List.map_stable tra tyl in
           if tyl == mtyl then t else
             Q.TypeName (mtyl, typeident)
       | Q.TypeAbstract -> t
       | Q.TypeForall (typevars, rowvars, colvars, ty) ->
           let mty = tra ty in
           if ty == mty then t else
             Q.TypeForall (typevars, rowvars, colvars, mty)

     let fold_field tra acc (_, ty) = tra acc ty

     let fold tra acc t =
       match t with
       | Q.TypeConst _
       | Q.TypeVar _ -> acc
       | Q.TypeArrow (tyl, ty) ->
           let acc = List.fold_left tra acc tyl in
           let acc = tra acc ty in
           acc
       | Q.TypeRecord (Q.TyRow (fields, _rowvar)) ->
           let acc = List.fold_left (fold_field tra) acc fields in
           acc
       | Q.TypeSum (Q.TyCol (l_fields, _colvar)) ->
           let acc = List.fold_left (List.fold_left (fold_field tra)) acc l_fields in
           acc
       | Q.TypeSumSugar tyl ->
           let acc = List.fold_left tra acc tyl in
           acc
       | Q.TypeName (tyl, _typeident) ->
           let acc = List.fold_left tra acc tyl in
           acc
       | Q.TypeAbstract -> acc
       | Q.TypeForall (_typevars, _rowvars, _colvars, ty) ->
           let acc = tra acc ty in
           acc
  end

  (* TODO: bench with this one *)
  include Traverse.Make2 (S2)

  (* For now, Keeping old implementation *)
  (* include Traverse.Make (Subs) *)
end


module Top =
struct
  (*let iter_expr f = function
    | Database _
    | NewDbValue (Db_Q.TypeDecl _)
    | NewType _ -> ()
    | NewDbValue (Db_Constraint _) -> assert false
    | NewVal bnd | NewValRec bnd -> List.iter (fun (_,e) -> f e) bnd
    | NewDbValue (Db_Default (p, dflt)) -> f dflt

  let map_expr map elt = match elt with
    | Database _
    | NewDbValue (Db_TypeDecl _)
    | NewType _ -> elt
    | NewDbValue (Db_Constraint _) -> assert false
    | NewDbValue (Db_Default (p, dflt)) -> NewDbValue (Db_Default (p, map dflt))
    | NewVal se -> NewVal (List.map (fun (s, e) -> (s, map e)) se)
    | NewValRec se -> NewValRec (List.map (fun (s, e) -> (s, map e)) se)

  let fold_expr f acc elt = match elt with
    | Database _
    | NewDbValue (Db_TypeDecl _)
    | NewType _ -> acc
    | NewDbValue (Db_Constraint _) -> assert false
    | NewVal bnd | NewValRec bnd -> List.fold_left (fun acc (id,e) -> f acc e) acc bnd
    | NewDbValue (Db_Default (p, dflt)) -> f acc dflt*)

  let fold_map_name_expr f acc elt =
    match elt with
    | Q.Database _
    | Q.NewDbValue (_, Db.Db_TypeDecl _)
    | Q.NewDbValue (_, Db.Db_Alias _)
    | Q.NewType _ -> acc, elt
    | Q.NewDbValue (label, Db.Db_Constraint (p, cstr)) ->
        (match cstr with
           | Db.C_Ordering e ->
               let acc, (_, fe) = f acc (Ident.source  "dbconstr", e) in
               acc,
               if e == fe then elt else
                 Q.NewDbValue (label, Db.Db_Constraint (p, Db.C_Ordering fe))
           | Db.C_Validation e ->
               let acc, (_, fe) =  f acc (Ident.source  "dbconstr", e) in
               acc,
               if e == fe then elt else
                 Q.NewDbValue (label, Db.Db_Constraint (p, Db.C_Validation fe))
           | Db.C_Inclusion _ | Db.C_Inverse _ | Db.C_Private _ ->
               acc, elt)
    | Q.NewDbValue (label, Db.Db_Default (p, dflt)) ->
        let acc, (_, fdflt) = f acc (Ident.source "dbdefault", dflt) in
        acc,
        if dflt == fdflt then elt else
          Q.NewDbValue (label, Db.Db_Default (p, fdflt))
    | Q.NewDbValue (label, Db.Db_Virtual (p, e)) ->
        let acc, (_, fe) = f acc (Ident.source "dbdefault", e) in
        acc,
        if e == fe then elt else
          Q.NewDbValue (label, Db.Db_Virtual (p, fe))
    | Q.NewVal (label, se) ->
        let acc, fse = List.fold_left_map_stable f acc se in
        acc,
        if se == fse then elt else
          Q.NewVal (label, fse)
    | Q.NewValRec (label, se) ->
        let acc, fse = List.fold_left_map_stable f acc se in
        acc,
        if se == fse then elt else
          Q.NewValRec (label, fse)

  let fold_map_expr foldmap acc elt =
    let f acc ((s, e) as c) =
      let acc, fe = foldmap acc e in
      acc,
      if e == fe then c else
        (s, fe)
    in
    fold_map_name_expr f acc elt

  let map_name_expr map elt =
    let f () bnd = (), map bnd in
    snd (fold_map_name_expr f () elt)

  let iter_name_expr f = make_iter fold_map_name_expr f

  let fold_name_expr f = make_fold fold_map_name_expr f

  let fold_expr f = make_fold fold_map_expr f
  let map_expr  f = make_map  fold_map_expr f
  let iter_expr f = make_iter fold_map_expr f

  let fold_names f acc = function
    | Q.NewVal (_,iel)
    | Q.NewValRec (_,iel) -> List.fold_left (fun acc (ident,_) -> f acc ident) acc iel
    | Q.Database (_,ident,_,_) -> f acc ident
    | Q.NewDbValue _
    | Q.NewType _ -> acc
end

module CodeExpr =
struct
  let iter f c =
    List.iter (fun ce -> Top.iter_expr f ce) c

  let map f c =
    List.map_stable (fun ce -> Top.map_expr f ce) c

  let fold f acc c =
    List.fold_left (fun acc ce -> Top.fold_expr f acc ce) acc c

  let fold_name_expr f acc c =
    List.fold_left (fun acc ce -> Top.fold_name_expr f acc ce) acc c

  let fold_names f acc c =
    List.fold_left (Top.fold_names f) acc c

  let fold_map_name_expr f acc c =
    List.fold_left_map_stable (fun acc ce -> Top.fold_map_name_expr f acc ce) acc c

  let map_name_expr f c =
    List.map (fun ce -> Top.map_name_expr f ce) c

  let fold_map f acc c =
    List.fold_left_map_stable (fun acc ce -> Top.fold_map_expr f acc ce) acc c

  let iter_with_code_elt f c =
    List.iter (fun ce -> Top.iter_expr (fun e -> f ce e) ce) c

  let exists f c =
    try iter (fun e -> if f e then raise Exit) c; false
    with Exit -> true
end

module Code =
struct
  let filter_binding f code =
    List.filter_map
      (function
         | Q.NewVal (label, iel) -> (
             match List.filter f iel with
             | [] -> None
             | l -> Some (Q.NewVal (label, l))
           )
         | Q.NewValRec (label, iel) -> (
             match List.filter f iel with
             | [] -> None
             | l -> Some (Q.NewValRec (label, l))
           )
         | c -> Some c) code
  let iter_binding f code =
    List.iter
      (function
       | Q.NewValRec (_,iel)
       | Q.NewVal (_,iel) ->
           List.iter f iel
       | _ -> ()) code
end

module Pattern =
struct
  module S2 =
  struct
    type 'a t = QmlAst.pat constraint 'a = _ * _ * _
    let foldmap tra acc input_p =
      match input_p with
      | Q.PatConst _ | Q.PatVar _ | Q.PatAny _ -> acc, input_p
      | Q.PatRecord (label, fields, rowvar) ->
          let acc, fields' =
            List.fold_left_map_stable
              (fun acc ((f,p) as c) ->
                 let acc, p' = tra acc p in
                 if p == p' then acc, c else acc, (f,p')
              ) acc fields in
          if fields == fields' then
            acc, input_p
          else
            acc, Q.PatRecord (label, fields', rowvar)
      | Q.PatCoerce (label, pat, t) ->
          let acc, pat' = tra acc pat in
          if pat == pat' then
            acc, input_p
          else
            acc, Q.PatCoerce (label, pat', t)
      | Q.PatAs (label, pat, id) ->
          let acc, pat' = tra acc pat in
          if pat == pat' then
            acc, input_p
          else
            acc, Q.PatAs (label, pat', id)

    let fold tra acc e = Traverse.Unoptimized.fold foldmap tra acc e
    let iter tra e = Traverse.Unoptimized.iter foldmap tra e
    let map tra e = Traverse.Unoptimized.map foldmap tra e
  end

  include Traverse.Make2(S2)

  let get_fields pat =
    let rec top pat0 =
      match pat0 with
      | Q.PatRecord (_, fields, rowvar) -> Some (fields, rowvar = `open_)
      | Q.PatCoerce (_, p, _) -> top p
      | _ -> None
    in top pat
end

module Expr =
struct
  module S2 =
  struct
    type 'a t = QmlAst.expr constraint 'a = _ * _ * _

    let foldmap tra acc input_e =
      match input_e with
      | Q.Directive (label, `hybrid_value, [e_client;e_server], z) ->
          (* When we traverse this directive we traverse only the client value.
           * Because this directive can be only on client code... *)
          let acc, e_client' = tra acc e_client in
          acc,
          if e_client == e_client' then input_e else
            Q.Directive (label, `hybrid_value, [e_client';e_server], z)
      | Q.Directive (_, `hybrid_value, [_e_server], _) ->
          acc, input_e
      | Q.Directive (_, `hybrid_value, _, _) -> assert false
      | Q.Const _
      | Q.Ident _
      | Q.Bypass _
          -> acc, input_e
      | Q.Directive (label, variant, exprs, tys) ->
          let acc, exprs' = List.fold_left_map_stable tra acc exprs in
          acc,
          if exprs == exprs' then input_e else
            Q.Directive (label, variant, exprs', tys)
      | Q.LetIn (label, bnd, e) ->
          let acc, bnd' =
            List.fold_left_map_stable
              (fun acc ((i, e) as p) ->
                 let acc, e' = tra acc e in
                 acc,
                 if e == e' then p else (i, e')
              ) acc bnd in
          let acc, e' = tra acc e in
          acc,
          if bnd == bnd' && e == e' then input_e else
            Q.LetIn (label, bnd', e')
      | Q.LetRecIn (label, bnd, e) ->
          let acc, bnd' =
            List.fold_left_map_stable
              (fun acc ((i, e) as p) ->
                 let acc, e' = tra acc e in
                 acc,
                 if e == e' then p else (i, e')
              ) acc bnd in
          let acc, e' = tra acc e in
          acc,
          if bnd == bnd' && e == e' then input_e else
            Q.LetRecIn (label, bnd', e')
      | Q.Lambda (label, params, e) ->
          let acc, e' = tra acc e in
          acc,
          if e == e' then input_e else
            Q.Lambda (label, params, e')
      | Q.Apply (label, e1, args) ->
          let acc, e1' = tra acc e1 in
          let acc, args' = List.fold_left_map_stable tra acc args in
          acc,
          if e1 == e1' && args == args' then input_e else
            Q.Apply (label, e1', args')
      | Q.Match (label, e, clist) ->
          let acc, e' = tra acc e in
          let acc, clist' =
            List.fold_left_map_stable
              (fun acc ((p,e) as rule_) ->
                 let acc, e' = tra acc e in
                 acc,
                 if e == e' then rule_ else (p, e')
              ) acc clist in
          acc,
          if e == e' && clist == clist' then input_e else
            Q.Match (label, e',clist')
      | Q.Record (label, rl) ->
          let acc, rl' =
            List.fold_left_map_stable
              (fun acc ((s,e) as bnd) ->
                 let acc, e' = tra acc e in
                 acc,
                 if e == e' then bnd else (s, e')
              ) acc rl in
          acc,
          if rl == rl' then input_e else
            Q.Record (label, rl')
      | Q.Dot (label, e, field) ->
          let acc, e' = tra acc e in
          acc,
          if e == e' then input_e else
            Q.Dot (label, e', field)
      | Q.ExtendRecord (label, field, e1, e2) ->
          let acc, e1' = tra acc e1 in
          let acc, e2' = tra acc e2 in
          acc,
          if e1 == e1' && e2 == e2' then input_e else
            Q.ExtendRecord (label, field, e1', e2')
      | Q.Coerce (label, e, ty) ->
          let acc, e' = tra acc e in
          acc,
          if e == e' then input_e else
            Q.Coerce (label, e', ty)
      | Q.Path (label, p, kind, select) ->
          let aux subs acc elt =
            let (rebuild, exprs) =
              subs TU.sub_current TU.sub_ignore elt
            in let (acc, exprs') =
              List.fold_left_map_stable
                (fun acc e ->
                   let acc, e' = tra acc e in
                   acc, if e == e' then e else e')
                acc exprs
            in acc, if exprs' == exprs then elt else rebuild exprs'
          in
          let acc, p' =
            List.fold_left_map_stable
              (fun acc e1 -> aux Q.Db.sub_path_elt acc e1)
              acc p
          in
          let acc, select' = aux Q.Db.sub_db_select acc select in
          let acc, kind' = aux Q.Db.sub_db_kind acc kind in
          acc,
          if p == p' && kind == kind' && select == select' then input_e else
            Q.Path (label, p', kind', select')

    let fold tra acc e =
      match e with
      | Q.Const _
      | Q.Ident _
      | Q.Bypass _ -> acc

      | Q.Coerce (_, e, _)
      | Q.Dot (_, e, _)
      | Q.Lambda (_, _, e) -> tra acc e

      | Q.ExtendRecord (_, _,e1, e2) -> tra (tra acc e1) e2

      | Q.Apply (_, f, args) ->
          let acc = tra acc f in
          List.fold_left tra acc args

      | Q.LetIn (_, bnd, e)
      | Q.LetRecIn (_, bnd, e) ->
          let acc =
            List.fold_left
              (fun acc (_,e) -> tra acc e) acc bnd in
          tra acc e
      | Q.Match (_, e, clist) ->
          let acc = tra acc e in
          List.fold_left
            (fun acc (_,e) -> tra acc e)
            acc clist
      | Q.Record (_, rl) ->
          List.fold_left
            (fun acc (_,e) -> tra acc e) acc rl
      | Q.Path (_, p, kind, select) ->
          let aux subs acc elt =
            let (_rebuild, exprs) =
              subs TU.sub_current TU.sub_ignore elt
            in let acc =
              List.fold_left
                (fun acc e -> tra acc e)
                acc exprs
            in acc
          in
          let acc =
            List.fold_left
              (fun acc e -> aux Q.Db.sub_path_elt acc e)
              acc p
          in
          let acc = aux Q.Db.sub_db_select acc select in
          let acc = aux Q.Db.sub_db_kind acc kind in
          acc
      | Q.Directive (_, _, exprs, _) -> List.fold_left tra acc exprs

    let iter tra e = Traverse.Unoptimized.iter foldmap tra e
    let map tra e = Traverse.Unoptimized.map foldmap tra e
  end
  include Traverse.Make2(S2)

  let fold_with_env add_env env folder acc expr =
    let isrec = function
      | Q.LetIn _ -> false
      | Q.LetRecIn _ -> true
      | _ -> assert false in
    let add_bnd env (n,expr) = add_env env n (Some expr) in
    let add_lambda env n = List.fold_left (fun env n -> add_env env n None) env n in
    let add_pat pat env =
      Pattern.fold_down (* TODO: OPTIMIZE IT *)
        (fun env pat->
           match pat with
           | Q.PatVar (_, v) | Q.PatAs (_,_,v) -> add_env env v None
           | _ -> env
        ) env pat in
    let rec process_expr tra env acc expr =
      let acc = folder env acc expr in
      let orig_e0 = expr in
      match orig_e0 with
      | Q.Lambda (_, ids, expr) ->
          process_expr tra (add_lambda env ids) acc expr
      | Q.LetIn (_, bnd, expr)
      | Q.LetRecIn (_, bnd, expr) ->
          let env_bnd = List.fold_left add_bnd env bnd in
          let env_let = if isrec orig_e0 then env_bnd else env in
          let acc = List.fold_left (fun acc (_,expr) -> process_expr tra env_let acc expr) acc bnd in
          process_expr tra env_bnd acc expr
      | Q.Match (_, expr, clist) ->
          let acc = process_expr tra env acc expr in
          List.fold_left (fun acc rule_ -> process_pattern_expr tra env acc rule_) acc clist
      | _ -> tra env acc expr
    and process_pattern_expr tra env acc (pat,expr) =
      let env_bnd = add_pat pat env in
      process_expr tra env_bnd acc expr in
    traverse_fold_context_down process_expr env acc expr

  let fold_with_exprmap ?(env=IdentMap.empty) f a e =
    fold_with_env (fun map id optval -> IdentMap.add id optval map) env f a e

end

module ExprPatt = struct
  let iter_down f_expr f_pat =
    Expr.iter_down
      (fun e ->
         f_expr e;
         match e with
           | Q.Match (_, _, clst) -> List.iter (fun (p, _e) -> Pattern.iter_down f_pat p) clst
           | _ -> ())
  let iter = iter_down

  let extend_pat f_expr f_pat acc e =
    let acc, e = f_expr acc e in
    match e with
    | Q.Match (label, x, clst) ->
        let acc, clst = List.fold_left_map
          (fun acc (p, e) ->
             let acc, p = Pattern.foldmap_down f_pat acc p
             in acc, (p, e)) acc clst in
        acc, Q.Match (label, x, clst)
    | e0 -> acc, e0

  let foldmap_down f_expr f_pat = Expr.foldmap_down (extend_pat f_expr f_pat)
  let foldmap = foldmap_down

  let  map_down f_expr f_pat =  make_map Expr.foldmap_down (rm_void_acc (extend_pat (add_void_acc f_expr) (add_void_acc f_pat)))
  let map = map_down

  let fold_down f_expr f_pat a e =
    let f_expr a e = (f_expr a e, e)
    and f_pat a p = (f_pat a p, p) in
    fst (foldmap_down f_expr f_pat a e)
  let fold = fold_down
end

(** todo : return an expr0 option instead of saying just yes or no will upgrade error messages *)
module UseDb =
struct
  let expr expr =
    let use_db = function
      | Q.Path _ ->
          true
            (* FIXME: what about library calls that need the DB ? *)
      | _ ->
          false
    in
    Expr.exists_down use_db expr

  let code_elt ?(ignore_declaration=false) = function
    | Q.Database _
    | Q.NewDbValue _ -> not ignore_declaration
    | Q.NewType _ -> false
    | Q.NewVal (_, v)
    | Q.NewValRec (_, v) -> List.exists (fun (_, e) -> expr e) v

  let code ?(ignore_declaration=false) code =
    List.exists (code_elt ~ignore_declaration) code
end

module Misc =
struct
  let rec remove_coerce = function
    | Q.Coerce (_, x, _) -> remove_coerce x
    | x -> x

  let code_size code = CodeExpr.fold (fun acc e -> Expr.fold (fun acc _ -> acc + 1) acc e) 0 code
end

module DbWalk =
struct
  module Query = Traverse.Make2
    (struct
       type 'a t = ('b, 'c) QmlAst.Db.query constraint 'a = ('b * 'c * _)

       let foldmap tra acc input =
         let binop build q0 q1 =
           let acc, q0' = tra acc q0 in
           let acc, q1' = tra acc q1 in
           acc, if q0 == q0' && q1 == q1' then input else build q0' q1'
         in
         match input with
         | Db.QMod    _
         | Db.QExists _
         | Db.QEq     _
         | Db.QGt     _
         | Db.QLt     _
         | Db.QGte    _
         | Db.QLte    _
         | Db.QNe     _
         | Db.QIn     _        -> acc, input
         | Db.QOr     (q0, q1) -> binop (fun q0 q1 -> Db.QOr (q0, q1)) q0 q1
         | Db.QAnd    (q0, q1) -> binop (fun q0 q1 -> Db.QAnd (q0, q1)) q0 q1
         | Db.QNot    q        ->
             let acc, q' = tra acc q in
             acc, if q == q' then input else Db.QNot q'
         | Db.QFlds   flds ->
             let acc, flds' =
               List.fold_left_map_stable
                 (fun acc ((s,f) as bnd) ->
                    let acc, f' = tra acc f in
                    acc, if f == f' then bnd else (s, f')
                 ) acc flds in
             acc, if flds == flds' then input else Db.QFlds flds'

       let fold tra acc input =
         let binop q0 q1 = tra (tra acc q0) q1 in
         match input with
         | Db.QMod    _
         | Db.QExists _
         | Db.QEq     _
         | Db.QGt     _
         | Db.QLt     _
         | Db.QGte    _
         | Db.QLte    _
         | Db.QNe     _
         | Db.QIn     _        -> acc
         | Db.QOr     (q0, q1) -> binop q0 q1
         | Db.QAnd    (q0, q1) -> binop q0 q1
         | Db.QNot    q        -> tra acc q
         | Db.QFlds   flds ->
             List.fold_left (fun acc (_,f) -> tra acc f) acc flds

       let iter tra input = Traverse.Unoptimized.iter foldmap tra input
       let map tra input = Traverse.Unoptimized.map foldmap tra input
     end)

  module Update = Traverse.Make2
    (struct
       type 'a t = 'b QmlAst.Db.update constraint 'a = ('b * _ * _)

       let foldmap tra acc input =
         match input with         (* Simple updating*)
         | Db.UExpr _
         | Db.UIncr _
         | Db.UAppend    _
         | Db.UAppendAll _
         | Db.URemove    _
         | Db.URemoveAll _
         | Db.UPop
         | Db.UShift -> acc, input
         | Db.UId (e, u) ->
             let acc, u' = tra acc u in
             acc, if u == u' then input else Db.UId (e, u')
         | Db.UFlds flds ->
             let acc, flds' =
               List.fold_left_map_stable
                 (fun acc ((s,f) as bnd) ->
                    let acc, f' = tra acc f in
                    acc, if f == f' then bnd else (s, f')
                 ) acc flds in
             acc, if flds == flds' then input else Db.UFlds flds'

       let fold tra acc input =
         match input with
         | Db.UExpr _
         | Db.UIncr _
         | Db.UAppend    _
         | Db.UAppendAll _
         | Db.URemove    _
         | Db.URemoveAll _
         | Db.UPop
         | Db.UShift -> acc
         | Db.UId (_, u) -> tra acc u
         | Db.UFlds   flds ->
             List.fold_left (fun acc (_,f) -> tra acc f) acc flds

       let iter tra input = Traverse.Unoptimized.iter foldmap tra input
       let map tra input = Traverse.Unoptimized.map foldmap tra input
     end)

end
