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
module List = Base.List

module Q = QmlAst
module PackageTbl = ObjectFiles.PackageTbl
module Package = ObjectFiles.Package

let map_on_type_from_pat f pat =
  QmlAstWalk.Pattern.map_down
    (fun pat ->
       match pat with
       | Q.PatCoerce (label, p, ty) ->
           let fty = f ty in
           if ty == fty then pat else
             Q.PatCoerce (label, p, fty)
       | _ -> pat)
    pat
let map_on_type_from_expr f expr =
  QmlAstWalk.ExprPatt.map_down
    (fun expr ->
       match expr with
       | Q.Coerce (label, e, ty) ->
           let fty = f ty in
           if ty == fty then expr else
             Q.Coerce (label, e, fty)
       | _ -> expr)
    (fun pat ->
       match pat with
       | Q.PatCoerce (label, p, ty) ->
           let fty = f ty in
           if ty == fty then pat else
             Q.PatCoerce (label, p, fty)
       | _ -> pat)
    expr

module MakeFind(Tbl:Hashtbl.S with type key = Fresh.t_fresh)(Map:BaseMapSig.S with type key = Fresh.t_fresh)(Var:Fresh.FRESH) =
struct
  let h = PackageTbl.create 10
  let clear () = PackageTbl.clear h
  let rec just_find (typevar, package_being_refreshed) =
    try just_find (Tbl.find (PackageTbl.find h package_being_refreshed) typevar)
    with Not_found -> typevar, package_being_refreshed
  let find package_being_refreshed typevar =
    try
      let (old_var,original_package) =
        Tbl.find (PackageTbl.find h package_being_refreshed) typevar in
      assert (original_package = ObjectFiles.get_current_package ());
      old_var
    with
      Not_found ->
        (* the var is defined in package_being_refreshed *)
        let newvar = Var.refresh typevar in
        let h2 =
          try
            PackageTbl.find h package_being_refreshed
          with Not_found ->
            let h2 = Tbl.create 10 in
            PackageTbl.add h package_being_refreshed h2;
            h2 in
        Tbl.add h2 typevar (newvar, ObjectFiles.get_current_package ());
        newvar
  let rec fill package_being_refreshed typevar (old_var, original_package) =
    let old_var, original_package = just_find (old_var,original_package) in
    let current_package = ObjectFiles.get_current_package () in
    if Package.equal original_package current_package then (
      let h2 =
        try PackageTbl.find h package_being_refreshed
        with Not_found ->
          let h2 = Tbl.create 10 in
          PackageTbl.add h package_being_refreshed h2;
          h2 in
      Tbl.add h2 typevar (old_var, original_package)
    ) else (
      let h2 =
        try PackageTbl.find h original_package
        with Not_found ->
          let h2 = Tbl.create 10 in
          PackageTbl.add h original_package h2;
          h2 in
      let newvar = Var.refresh old_var in
      Tbl.add h2 old_var (newvar,current_package);
      fill package_being_refreshed typevar (old_var, original_package)
    )
  let reverse () =
    PackageTbl.fold
      (fun package h2 acc ->
         Tbl.fold
           (fun oldvar (freshvar,its_package) acc ->
              if Package.equal its_package (ObjectFiles.get_current_package ()) then
                Map.add freshvar (oldvar,package) acc
              else
                acc
           ) h2 acc
      ) h Map.empty
  let show f =
    PackageTbl.iter
      (fun ident acc ->
         Format.fprintf f "@\n@[<2>package: %a" Package.pp ident;
         Tbl.iter
           (fun var (freshvar,package) ->
              Format.fprintf f "@\n%s -> %s-%a" (Var.to_string var) (Var.to_string freshvar) Package.pp package;
           ) acc;
         Format.fprintf f "@]"
      ) h
end
module M_typ = MakeFind(QmlTypeVars.TypeVarTbl)(QmlTypeVars.TypeVarMap)(QmlTypeVars.TypeVar)
module M_row = MakeFind(QmlTypeVars.RowVarTbl)(QmlTypeVars.RowVarMap)(QmlTypeVars.RowVar)
module M_col = MakeFind(QmlTypeVars.ColVarTbl)(QmlTypeVars.ColVarMap)(QmlTypeVars.ColVar)
let find_t = M_typ.find
let find_r = M_row.find
let find_c = M_col.find

let refresh_typevars_from_ty package ty =
  QmlAstWalk.Type.map
    (function
     | Q.TypeVar typevar ->
         let typevar = find_t package typevar in
         Q.TypeVar typevar
     | Q.TypeRecord (Q.TyRow (f,Some rowvar)) ->
         let rowvar = find_r package rowvar in
         Q.TypeRecord (Q.TyRow (f,Some rowvar))
     | Q.TypeSum (Q.TyCol (f,Some colvar)) ->
         let colvar = find_c package colvar in
         Q.TypeSum (Q.TyCol (f,Some colvar))
     | Q.TypeForall (ts,rows,cols,ty) ->
         let ts = List.map (find_t package) ts in
         let rows = List.map (find_r package) rows in
         let cols = List.map (find_c package) cols in
         Q.TypeForall (ts,rows,cols,ty)
     | typ -> typ
    ) ty
let refresh_typevars_from_expr package expr =
  map_on_type_from_expr (refresh_typevars_from_ty package) expr
let refresh_typevars_from_pat package pat =
  map_on_type_from_pat (refresh_typevars_from_ty package) pat

let refresh_typevars_from_tsc package tsc =
  let {QmlTypeVars.typevar=tl;
       QmlTypeVars.rowvar=rl;
       QmlTypeVars.colvar=cl;
      } = QmlGenericScheme.export_ordered_quantif tsc in
  let _,ty,() = QmlGenericScheme.export_unsafe tsc in
  let tl = List.map (find_t package) tl in
  let rl = List.map (find_r package) rl in
  let cl = List.map (find_c package) cl in
  let ty = refresh_typevars_from_ty package ty in
  let ts = QmlTypeVars.TypeVarSet.from_list tl in
  let rs = QmlTypeVars.RowVarSet.from_list rl in
  let cs = QmlTypeVars.ColVarSet.from_list cl in
  let tsc = QmlGenericScheme.import (QmlTypeVars.FreeVars.import_from_sets ts rs cs) ty () in
  tsc

let refresh_gamma package gamma =
  let refresh_tsc = refresh_typevars_from_tsc package in
  let gamma = QmlTypes.Env.Ident.map refresh_tsc gamma in
  QmlTypes.Env.TypeIdent.map
    (fun (tsc, height, visibility) -> ((refresh_tsc tsc), height, visibility))
    gamma

let refresh_typevars_from_code package code =
  List.map
    (function
     | Q.Database _ as c -> c
     | Q.NewDbValue (a,db_def) ->
         let (), db_def =
           Q.Db.foldmap_expr
             (fun () v -> (), refresh_typevars_from_expr package v) () db_def in
         let (), db_def =
           Q.Db.foldmap_ty
             (fun () v -> (), refresh_typevars_from_ty package v) () db_def in
         Q.NewDbValue (a, db_def)
     | Q.NewType (a, ty_defs) ->
         let ty_defs' =
           List.map
             (fun ty_def ->
                let params' =
                  List.map (find_t package) ty_def.QmlAst.ty_def_params in
                let body' =
                  refresh_typevars_from_ty package ty_def.QmlAst.ty_def_body in
                { ty_def with
                    QmlAst.ty_def_params = params' ;
                    QmlAst.ty_def_body = body' })
             ty_defs in
         Q.NewType (a, ty_defs')
     | Q.NewVal (a, iel) ->
         let iel =
           List.map
             (fun (i, e) ->
                let e = refresh_typevars_from_expr package e in
                (i,e)) iel in
         Q.NewVal (a, iel)
     | Q.NewValRec (a, iel) ->
         let iel =
           List.map
             (fun (i, e) ->
                let e = refresh_typevars_from_expr package e in
                (i, e)) iel in
         Q.NewValRec (a, iel))
    code

let refresh_annotmap package annotmap =
  QmlAnnotMap.map_ty_tsc
    ~ty: (refresh_typevars_from_ty package)
    ~tsc: (refresh_typevars_from_tsc package)
    annotmap

let refresh_pat package ~annotmap_old annotmap pat =
  let pat = refresh_typevars_from_pat package pat in
  let (annotmap, pat) =
    QmlAstCons.TypedPat.copy_new_when_possible ~annotmap_old annotmap pat in
  (annotmap, pat)

let refresh_expr package ~annotmap_old annotmap expr =
  let expr = refresh_typevars_from_expr package expr in
  let (annotmap, expr) =
    QmlAstCons.TypedExpr.copy_new_when_possible ~annotmap_old annotmap expr in
  (annotmap, expr)

let refresh_expr_no_annotmap package expr =
  let expr = refresh_typevars_from_expr package expr in
  QmlAstWalk.ExprPatt.map Q.QAnnot.Refresh.expr Q.QAnnot.Refresh.pat expr

let refresh_schema2 package ~refreshed_annotmap_old annotmap schema =
  let schema =
    QmlDbGen.Schema.map_types (refresh_typevars_from_ty package) schema in
  QmlDbGen.Schema.foldmap_expr
    (refresh_expr package ~annotmap_old: refreshed_annotmap_old) annotmap schema

let refresh_schema package ~annotmap_old annotmap schema =
  let annotmap_old = refresh_annotmap package annotmap_old in
  refresh_schema2 package ~refreshed_annotmap_old: annotmap_old annotmap schema


let restrict_annotmap_expr annotmap ?(acc=QmlAnnotMap.empty) expr =
  let f extract acc e =
    let annot = extract e in
    let annot_content = QmlAnnotMap.find annot annotmap in
    QmlAnnotMap.add annot annot_content acc in
  QmlAstWalk.ExprPatt.fold (f Q.QAnnot.expr) (f Q.QAnnot.pat) acc expr
let restrict_annotmap_fold_expr fold annotmap ?(acc=QmlAnnotMap.empty) v =
  fold (fun acc e -> restrict_annotmap_expr annotmap ~acc e) acc v

let restrict_annotmap_pat annotmap ?(acc=QmlAnnotMap.empty) expr =
  let f acc e =
    let annot = Q.QAnnot.pat e in
    let annot_content = QmlAnnotMap.find annot annotmap in
    QmlAnnotMap.add annot annot_content acc in
  QmlAstWalk.Pattern.fold_down f acc expr
let restrict_annotmap_fold_pat fold annotmap ?(acc=QmlAnnotMap.empty) v =
  fold (fun acc e -> restrict_annotmap_pat annotmap ~acc e) acc v

module Ssubst =
struct
  type t = (
    (QmlTypeVars.TypeVar.t * Package.t) QmlTypeVars.TypeVarMap.t *
    (QmlTypeVars.RowVar.t * Package.t) QmlTypeVars.RowVarMap.t *
    (QmlTypeVars.ColVar.t * Package.t) QmlTypeVars.ColVarMap.t
  )
  let pass = "subst"
  let pp f _ = Format.pp_print_string f "<dummy>"
end
module Rsubst = ObjectFiles.Make(Ssubst)

let clear () =
  M_typ.clear (); M_row.clear (); M_col.clear ()
let save () =
  (*let pp f (v,p) =
    Format.fprintf f "(%s,%a)" (QmlTypeVars.TypeVar.to_string v) Package.pp p in*)
  let m1 = M_typ.reverse () in
  let m2 = M_row.reverse () in
  let m3 = M_col.reverse () in
  (*Format.printf "@[<2>saving:@\n";
  QmlTypeVars.TypeVarMap.iter (fun k v -> Format.printf "%s -> %a@\n" (QmlTypeVars.TypeVar.to_string k) pp v) m1;
  Format.printf "@]@.";*)
  Rsubst.save (m1, m2, m3);
  clear ()
let load () =
  Rsubst.iter_with_name
    ~packages:true
    ~deep:true (* this is probably unneeded when the frontier is after ei *)
    (fun package (ty,row,col) ->
       QmlTypeVars.TypeVarMap.iter (M_typ.fill package) ty;
       QmlTypeVars.RowVarMap.iter (M_row.fill package) row;
       QmlTypeVars.ColVarMap.iter (M_col.fill package) col
    )
