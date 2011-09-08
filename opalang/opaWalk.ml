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

(* depends *)
module List = BaseList

open SurfaceAst

let foldmap_2_stable tra acc ((s,p) as c) =
  let acc, p' = tra acc p in
  acc,
  if p == p' then c else (s,p')
let foldmap_1_stable tra acc ((p,s) as c) =
  let acc, p' = tra acc p in
  acc,
  if p == p' then c else (p',s)
let eq_string s1 (s2:string) = s1 = s2

module Pattern =
struct
  module S2 =
  struct
    type 'a t = 'b pat constraint 'a = 'b * _ * _
    let foldmap tra acc ((p,lab) as orig_pat) =
      match p with
      | PatRecord (pr, rowvar) ->
          let acc,pr' =
            List.fold_left_map_stable
              (fun acc c -> foldmap_2_stable tra acc c)
              acc pr in
          acc,
          if pr == pr' then orig_pat else
            (PatRecord (pr', rowvar), lab)
      | PatAny _
      | PatConst _
      | PatVar _ -> acc, orig_pat
      | PatAs (p,i) ->
          let acc, p' = tra acc p in
          acc,
          if p == p' then orig_pat else
          (PatAs (p',i), lab)
      | PatCoerce (p,ty) ->
          let acc, p' = tra acc p in
          acc,
          if p == p' then orig_pat else
          (PatCoerce (p',ty),lab)
    let map tra e = Traverse.Unoptimized.map foldmap tra e
    let fold tra acc e = Traverse.Unoptimized.fold foldmap tra acc e
    let iter tra e = Traverse.Unoptimized.iter foldmap tra e
  end
  include Traverse.Make2(S2)
  let get_vars ?(acc=[]) pat =
    fold (fun acc -> function
            | (PatVar v,_)
            | (PatAs (_,v),_) -> v.ident :: acc
            | _ -> acc) acc pat
  let appears_eq equal target_ident pat =
    exists (function
             | (PatVar v,_)
             | (PatAs (_,v),_) -> equal (v.ident) target_ident
             | _ -> false) pat
  let appears_str ident pat = appears_eq eq_string ident pat
  let appears ident pat = appears_eq Ident.equal ident pat
end

module Type =
struct
  module S2 =
  struct
    type 'a t = 'b ty constraint 'a = 'b * _ * _
    let foldmap tra acc ((t,lab) as orig_ty) =
      match t with
      | TypeConst _
      | TypeVar _
      | TypeExternal _ -> acc, orig_ty
      | TypeArrow (((TyRow (fields,rowvar),lab2) as tyrow),ty) ->
          let acc, fields' =
            List.fold_left_map_stable
              (fun acc p -> foldmap_2_stable tra acc p)
              acc fields in
          let acc, ty' = tra acc ty in
          acc,
          if fields == fields' then
            if ty == ty' then
              orig_ty
            else
              (TypeArrow  (tyrow,ty'),lab)
          else
            (TypeArrow ((TyRow (fields',rowvar),lab2),ty'),lab)
      | TypeRecord (TyRow (fields,rowvar)) ->
          let acc, fields' =
            List.fold_left_map_stable
              (fun acc p -> foldmap_2_stable tra acc p)
              acc fields in
          acc,
          if fields == fields' then orig_ty else
          (TypeRecord (TyRow (fields',rowvar)),lab)
      | TypeSumSugar suml ->
          let acc, suml' =
            List.fold_left_map_stable
              (fun acc sum_t_node ->
                 foldmap_1_stable
                   (fun acc -> function
                      | SumName (ident,tyl) as ty ->
                          let acc, tyl' = List.fold_left_map_stable tra acc tyl in
                          acc,
                          if tyl == tyl' then ty else
                            SumName (ident,tyl')
                      | SumRecord (TyRow (fields,rowvar)) as ty ->
                          let acc, fields' =
                            List.fold_left_map_stable
                              (fun acc p -> foldmap_2_stable tra acc p)
                              acc fields in
                          acc,
                          if fields == fields' then ty else
                            SumRecord (TyRow (fields',rowvar))
                      | SumVar _ as ty -> acc, ty)
                   acc sum_t_node
              ) acc suml in
          acc,
          if suml == suml' then orig_ty else
          (TypeSumSugar suml', lab)
      | TypeNamed (ident,tyl) ->
          let acc, tyl' = List.fold_left_map_stable tra acc tyl in
          acc,
          if tyl == tyl' then orig_ty else
          (TypeNamed (ident,tyl'),lab)
      | TypeForall (vars,ty) ->
          let acc, ty' = tra acc ty in
          acc,
          if ty == ty' then orig_ty else
          (TypeForall (vars,ty'),lab)
      | TypeModule fields ->
          let acc, fields' =
            List.fold_left_map_stable
              (fun acc p -> foldmap_2_stable tra acc p)
              acc fields in
          acc,
          if fields == fields' then orig_ty else
          (TypeModule fields', lab)
    let map tra e = Traverse.Unoptimized.map foldmap tra e
    let fold tra acc e = Traverse.Unoptimized.fold foldmap tra acc e
    let iter tra e = Traverse.Unoptimized.iter foldmap tra e
  end
  include Traverse.Make2(S2)
  let get_typenames_with_acc acc ty =
    fold (fun acc -> function
          | (TypeNamed (Typeident ident,_),_) ->  ident :: acc
          | (TypeSumSugar l,_) ->
              List.fold_left
                (fun acc -> function
                   | (SumName (Typeident ident,_),_) -> ident :: acc
                   | _ -> acc) acc l
          | _ -> acc
         ) acc ty
  let get_typenames ty = get_typenames_with_acc [] ty
  let get_typenames_from_arrow_type (arrow_node,label) =
    get_typenames (TypeArrow arrow_node, label)

end

module Expr =
struct
  module S2 =
  struct
    type 'a t = ('b,'c) expr constraint 'a = 'b * 'c * _
    let foldmap tra acc ((e,lab) as orig_e) =
      match e with
      | Bypass _
      | Const _
      | Ident _ -> acc, orig_e
      | Apply (e,record) ->
          let acc, e' = tra acc e in
          let acc, record' =
            foldmap_1_stable
              (fun acc l ->
                 List.fold_left_map_stable
                   (fun acc p -> foldmap_2_stable tra acc p)
                   acc l)
              acc record in
          acc,
          if e == e' && record == record' then orig_e else
            (Apply (e',record'),lab)
      | LetIn (rec_,iel,e) ->
          let acc, iel' =
            List.fold_left_map_stable
              (fun acc ie ->
                 foldmap_2_stable tra acc ie
              ) acc iel in
          let acc, e' = tra acc e in
          acc,
          if e == e' && iel == iel' then orig_e else
          (LetIn (rec_,iel',e'),lab)
      | Lambda (r,e) ->
          let acc, e' = tra acc e in
          acc,
          if e == e' then orig_e else
            (Lambda (r,e'),lab)
      | Match (e,pel) ->
          let acc, e' = tra acc e in
          let acc, pel' = List.fold_left_map_stable
            (fun acc pe ->
               foldmap_2_stable tra acc pe) acc pel in
          acc,
          if e == e' && pel == pel' then orig_e else
            (Match (e',pel'),lab)
      | Record record ->
          let acc, record' =
            List.fold_left_map_stable
              (fun acc p -> foldmap_2_stable tra acc p)
              acc record in
          acc,
          if record == record' then orig_e else
            (Record record', lab)
      | ExtendRecord (record,e) ->
          let acc, record' =
            List.fold_left_map_stable
            (fun acc p -> foldmap_2_stable tra acc p)
              acc record in
          let acc, e' = tra acc e in
          acc,
          if e == e' && record == record' then orig_e else
        (ExtendRecord (record',e'),lab)
      | Dot (e,s) ->
          let acc, e' = tra acc e in
          acc,
          if e == e' then orig_e else
            (Dot (e',s),lab)
      | DBPath (dbelt,kind) ->
          let acc, dbelt' =
            foldmap_1_stable
              (fun acc node ->
                 List.fold_left_map_stable
                   (fun acc db_elt ->
                      foldmap_1_stable
                        (fun acc -> function
                           | FldKey _
                           | NewKey as v -> acc, v
                           | ExprKey e as v ->
                               let acc, e' = tra acc e in
                               acc,
                               if e == e' then v else
                                 ExprKey e'
                        ) acc db_elt
                   ) acc node
              ) acc dbelt in
          acc,
          if dbelt == dbelt' then orig_e else
            (DBPath (dbelt',kind),lab)
      | Directive (variant,el,t) ->
          let acc, el' = List.fold_left_map_stable tra acc el in
          acc,
          if el == el' then orig_e else
            (Directive (variant,el',t),lab)
    let map tra e = Traverse.Unoptimized.map foldmap tra e
    let fold tra acc e = Traverse.Unoptimized.fold foldmap tra acc e
    let iter tra e = Traverse.Unoptimized.iter foldmap tra e
  end
  include Traverse.Make2(S2)
  let appears_eq eq i e =
    exists (function
              | (Ident j,_) -> eq i j
              | _ -> false) e
  let appears_str i e = appears_eq eq_string i e
  let appears i e = appears_eq Ident.equal i e
  let used_vars_eq eq vars e =
    fold
      (fun acc -> function
       | (Ident j,_) ->
           if List.exists (eq j) vars && not (List.exists (eq j) acc)
           then j :: acc
           else acc
       | _ -> acc) [] e
  let used_vars_str vars e = used_vars_eq eq_string vars e
  let used_vars vars e = used_vars_eq Ident.equal vars e
end


module CodeElt =
struct
  module Lift2 =
  struct
    type 'a t = ('b,'c) expr constraint 'a = 'b * 'c * _
    type 'a container = ('b,'c) code_elt constraint 'a = 'b * 'c * _
    let foldmap tra acc ((code_elt_node, lab) as orig_code_elt) =
      match code_elt_node with
      | Package _
      | Database _
      | NewType _ -> acc, orig_code_elt
      | NewVal (pel,rec_) ->
          let acc, pel' =
            List.fold_left_map_stable
              (fun acc pe ->
                 foldmap_2_stable tra acc pe
              ) acc pel in
          acc,
          if pel == pel' then orig_code_elt else
          (NewVal (pel',rec_),lab)
      | NewDbDef dbdef ->
          let rebuild, exprs =
            QmlAst.Db.sub_db_def
              Traverse.Utils.sub_current
              Traverse.Utils.sub_ignore
              dbdef in
          let acc, exprs' = List.fold_left_map_stable tra acc exprs in
          acc, (NewDbDef (rebuild exprs'), lab)
    let map tra e = Traverse.Unoptimized.map foldmap tra e
    let fold tra acc e = Traverse.Unoptimized.fold foldmap tra acc e
    let iter tra e = Traverse.Unoptimized.iter foldmap tra e
  end
  include Traverse.MakeLift1(Lift2)(Expr)
end

module Code =
struct
  module Lift2 =
  struct
    type 'a t = ('b,'c) expr constraint 'a = 'b * 'c * _
    type 'a container = ('b,'c) code constraint 'a = 'b * 'c * _
    let foldmap tra acc code = List.fold_left_map_stable (fun acc e -> CodeElt.Lift2.foldmap tra acc e) acc code
    let map tra e = Traverse.Unoptimized.map foldmap tra e
    let fold tra acc e = Traverse.Unoptimized.fold foldmap tra acc e
    let iter tra e = Traverse.Unoptimized.iter foldmap tra e
  end
  include Traverse.MakeLift1(Lift2)(Expr)
  let size code = fold (fun acc _ -> acc + 1) 0 code
  let length = List.length
end

module CodeEltTopPattern =
struct
  module Lift2 =
  struct
    type 'a t = 'b pat constraint 'a = 'b * _ * _
    type 'a container = ('b,'c) code_elt constraint 'a = 'b * 'c * _
    let foldmap tra acc ((code_elt_node,lab) as code_elt) =
      match code_elt_node with
      | NewVal (pel,rec_) ->
          let acc, pel' =
            List.fold_left_map_stable
              (fun acc pe ->
                 foldmap_1_stable tra acc pe)
              acc pel in
          acc,
          if pel == pel' then code_elt else
          (NewVal (pel',rec_),lab)
      | _ -> acc, code_elt
    let map tra e = Traverse.Unoptimized.map foldmap tra e
    let fold tra acc e = Traverse.Unoptimized.fold foldmap tra acc e
    let iter tra e = Traverse.Unoptimized.iter foldmap tra e
  end
  include Traverse.MakeLift1(Lift2)(Pattern)
end

module CodeTopPattern =
struct
  module Lift2 =
  struct
    type 'a t = 'b pat constraint 'a = 'b * _ * _
    type 'a container = ('b,'c) code constraint 'a = 'b * 'c * _
    let foldmap tra acc code = List.fold_left_map_stable (fun acc e -> CodeEltTopPattern.Lift2.foldmap tra acc e) acc code
    let map tra e = Traverse.Unoptimized.map foldmap tra e
    let fold tra acc e = Traverse.Unoptimized.fold foldmap tra acc e
    let iter tra e = Traverse.Unoptimized.iter foldmap tra e
  end
  include Traverse.MakeLift1(Lift2)(Pattern)
end
