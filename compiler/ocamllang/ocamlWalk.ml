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
(* CF mli *)

(* depends *)
module List = Base.List

(* refactoring in progress *)
module OcamlAst = Ocaml

(* shorthands *)
module O = OcamlAst

(* alias *)


(* -- *)

module Ty_Subs : TraverseInterface.S2
  with type 'a t = OcamlAst.type_expr constraint 'a = _ * _ * _ =
struct
  type 'a t = OcamlAst.type_expr constraint 'a = _ * _ * _

  let foldmap tra acc ty =
    match ty with

    | O.TypeVar _ ->
        acc, ty

    | O.TypeName (params, type_name) ->
        let acc, f_params = List.fold_left_map_stable tra acc params in
        acc,
        if params == f_params then ty else
          O.TypeName (f_params, type_name)

    | O.TypeConst _ ->
        acc, ty

    | O.TypeRef tr ->
        let acc, f_tr = tra acc tr in
        acc,
        if tr == f_tr then ty else
          O.TypeRef f_tr

    | O.TypeTuple tyl ->
        let acc, f_tyl = List.fold_left_map_stable tra acc tyl in
        acc,
        if tyl == f_tyl then ty else
          O.TypeTuple f_tyl

    | O.TypeRecord fields ->
        let fmap acc ((bool, field, ty) as tpl) =
          let acc, fty = tra acc ty in
          acc,
          if ty = fty then tpl else (bool, field, fty)
        in
        let acc, f_fields = List.fold_left_map_stable fmap acc fields in
        acc,
        if fields == f_fields then ty else
          O.TypeRecord f_fields

    | O.TypeConstructor ctl ->
        let fmap acc ((k, opt) as cpl) =
          let acc, f_opt = Option.foldmap_stable tra acc opt in
          acc,
          if opt == f_opt then cpl else (k, f_opt)
        in
        let acc, fctl = List.fold_left_map_stable fmap acc ctl in
        acc,
        if ctl == fctl then ty else
          O.TypeConstructor fctl

    | O.TypeArrow (a, b) ->
        let acc, fa = tra acc a in
        let acc, fb = tra acc b in
        acc,
        if a == fa && b == fb then ty else
          O.TypeArrow (fa, fb)

    | O.TypeLabel (bool, label, tb) ->
        let acc, ftb = tra acc tb in
        acc,
        if tb == ftb then ty else
          O.TypeLabel (bool, label, ftb)

    | O.TypeVerbatim _ ->
        acc, ty

  let iter x = Traverse.Unoptimized.iter foldmap x
  let map x = Traverse.Unoptimized.map foldmap x
  let fold x = Traverse.Unoptimized.fold foldmap x
end

module Ty = Traverse.Make2 ( Ty_Subs )

module Pat_Subs : TraverseInterface.S2
  with type 'a t = OcamlAst.pattern constraint 'a = _ * _ * _ =
struct
  type 'a t = OcamlAst.pattern constraint 'a = _ * _ * _
  let foldmap tra acc pat =
    match pat with

    | O.PatVar _ ->
        acc, pat

    | O.PatList (hd, tl) ->
        let acc, fhd = tra acc hd in
        let acc, ftl = tra acc tl in
        acc,
        if hd == fhd && tl == ftl then pat else
          O.PatList (fhd, ftl)

    | O.PatEmptyList ->
        acc, pat

    | O.PatRecord fields ->
        let fmap acc ((label, pat) as cpl) =
          let acc, fpat = tra acc pat in
          acc,
          if pat == fpat then cpl else (label, fpat)
        in
        let acc, f_fields = List.fold_left_map_stable fmap acc fields in
        acc,
        if fields == f_fields then pat else
          O.PatRecord f_fields

    | O.PatConstructor (ident, ptl) ->
        let acc, f_ptl = List.fold_left_map_stable tra acc ptl in
        acc,
        if ptl == f_ptl then pat else
          O.PatConstructor (ident, f_ptl)

    | O.PatVariant (ident, ptl) ->
        let acc, f_ptl = List.fold_left_map_stable tra acc ptl in
        acc,
        if ptl == f_ptl then pat else
          O.PatVariant (ident, f_ptl)

    | O.PatPVariant (ident, ptl) ->
        let acc, f_ptl = List.fold_left_map_stable tra acc ptl in
        acc,
        if ptl == f_ptl then pat else
          O.PatPVariant (ident, f_ptl)

    | O.PatConst _ ->
        acc, pat

    | O.PatAny ->
        acc, pat

    | O.PatAnnot (pa, ty) ->
        let acc, fpa = tra acc pa in
        acc,
        if pa == fpa then pat else
          O.PatAnnot (fpa, ty)

    | O.PatTuple ptl ->
        let acc, f_ptl = List.fold_left_map_stable tra acc ptl in
        acc,
        if ptl == f_ptl then pat else
          O.PatTuple f_ptl

    | O.PatAs (pa, ident) ->
        let acc, fpa = tra acc pa in
        acc,
        if pa == fpa then pat else
          O.PatAs (fpa, ident)

    | O.PatArray ptl ->
        let acc, f_ptl = List.fold_left_map_stable tra acc ptl in
        acc,
        if ptl == f_ptl then pat else
          O.PatArray f_ptl

    | O.PatLazy p ->
        let acc, fp = tra acc p in
        acc,
        if p == fp then pat else
          O.PatLazy fp

    | O.PatOr ptl ->
        let acc, f_ptl = List.fold_left_map_stable tra acc ptl in
        acc,
        if ptl == f_ptl then pat else
          O.PatOr f_ptl

  let iter x = Traverse.Unoptimized.iter foldmap x
  let map x = Traverse.Unoptimized.map foldmap x
  let fold x = Traverse.Unoptimized.fold foldmap x
end

module Pat = Traverse.Make2 ( Pat_Subs )

module Expr_Subs : TraverseInterface.S2
  with type 'a t = OcamlAst.expr constraint 'a = _ * _ * _ =
struct
  type 'a t = OcamlAst.expr constraint 'a = _ * _ * _

  let rec foldmap tra acc expr =
    match expr with
    | O.Type _ ->
        acc, expr

    | O.Val _ ->
        acc, expr

    | O.Open _ ->
        acc, expr

    | O.Module (name, expr2, code, expr3) ->
        let acc, fexpr2 = Option.foldmap_stable tra acc expr2 in
        let acc, fcode = foldmap_code tra acc code in
        let acc, fexpr3 = Option.foldmap_stable tra acc expr3 in
        acc,
        if expr2 == fexpr2 && code == fcode && expr3 == fexpr3 then expr else
          O.Module (name, fexpr2, fcode, fexpr3)

    | O.ModuleType (name, code) ->
        let acc, fcode = foldmap_code tra acc code in
        acc,
        if code == fcode then expr else
          O.ModuleType (name, fcode)

    | O.Structure code ->
        let acc, fcode = foldmap_code tra acc code in
        acc,
        if code == fcode then expr else
          O.Structure fcode

    | O.Signature signature ->
        let acc, fsignature = foldmap_signature tra acc signature in
        acc,
        if signature == fsignature then expr else
          O.Signature fsignature

    | O.DeclareFunctor (name, seol, eo, e) ->
        let fmap acc ((s, (eo : O.expr option)) as cpl) =
          let acc, feo = Option.foldmap_stable tra acc eo in
          acc,
          if eo == feo then cpl else (s, feo)
        in
        let acc, fseol = List.fold_left_map_stable fmap acc seol in
        let acc, feo = Option.foldmap_stable tra acc eo in
        let acc, fe = tra acc e in
        acc,
        if seol == fseol && eo == feo && e == fe then expr else
          O.DeclareFunctor (name, fseol, feo, fe)

    | O.Constructor (ident, el) ->
        let acc, fel = List.fold_left_map_stable tra acc el in
        acc,
        if el == fel then expr else
          O.Constructor (ident, fel)

    | O.ConstructorPV (ident, el) ->
        let acc, fel = List.fold_left_map_stable tra acc el in
        acc,
        if el == fel then expr else
          O.ConstructorPV (ident, fel)

    | O.Const _ ->
        acc, expr

    | O.Var ep ->
        let acc, fep = foldmap_effective_param tra acc ep in
        acc,
        if ep == fep then expr else
          O.Var fep

    | O.MakeRef e ->
        let acc, fe = tra acc e in
        acc,
        if e == fe then expr else
          O.MakeRef fe

    | O.GetRef e ->
        let acc, fe = tra acc e in
        acc,
        if e == fe then expr else
          O.GetRef fe

    | O.SetRef (a, b) ->
        let acc, fa = tra acc a in
        let acc, fb = tra acc b in
        acc,
        if a == fa && b == fb then expr else
          O.SetRef (fa, fb)

    | O.SetMutable (a, b) ->
        let acc, fa = tra acc a in
        let acc, fb = tra acc b in
        acc,
        if a == fa && b == fb then expr else
          O.SetMutable (fa, fb)

    | O.Lazy e ->
        let acc, fe = tra acc e in
        acc,
        if e == fe then expr else
          O.Lazy fe

    | O.Tuple el ->
        let acc, fel = List.fold_left_map_stable tra acc el in
        acc,
        if el == fel then expr else
          O.Tuple fel

    | O.Cons (a, b) ->
        let acc, fa = tra acc a in
        let acc, fb = tra acc b in
        acc,
        if a == fa && b == fb then expr else
          O.Cons (fa, fb)

    | O.EmptyList ->
        acc, expr

    | O.Cond (a, b, c) ->
        let acc, fa = tra acc a in
        let acc, fb = tra acc b in
        let acc, fc = tra acc c in
        acc,
        if a == fa && b == fb && c == fc then expr else
          O.Cond (a, b, c)

    | O.App (a, b) ->
        let acc, fa = tra acc a in
        let acc, fb = tra acc b in
        acc,
        if a == fa && b == fb then expr else
          O.App (fa, fb)

    | O.Abs (fps, e) ->
        let acc, f_fps = List.fold_left_map_stable (foldmap_formal_param tra) acc fps in
        let acc, fe = tra acc e in
        acc,
        if fps == f_fps && e == fe then expr else
          O.Abs (f_fps, fe)

    | O.Let bind ->
        let fmap acc ((fp, e) as cpl) =
          let acc, f_fp = foldmap_formal_param tra acc fp in
          let acc, fe = tra acc e in
          acc,
          if fp == f_fp && e == fe then cpl else (f_fp, fe)
        in
        let acc, fbind = List.fold_left_map_stable fmap acc bind in
        acc,
        if bind == fbind then expr else
          O.Let fbind

    | O.Letrec bind ->
        let fmap acc ((fp, e) as cpl) =
          let acc, f_fp = foldmap_formal_param tra acc fp in
          let acc, fe = tra acc e in
          acc,
          if fp == f_fp && e == fe then cpl else (f_fp, fe)
        in
        let acc, fbind = List.fold_left_map_stable fmap acc bind in
        acc,
        if bind == fbind then expr else
          O.Letrec fbind

    | O.Letin (bind, e) ->
        let fmap acc ((fp, e) as cpl) =
          let acc, f_fp = foldmap_formal_param tra acc fp in
          let acc, fe = tra acc e in
          acc,
          if fp == f_fp && e == fe then cpl else (f_fp, fe)
        in
        let acc, fbind = List.fold_left_map_stable fmap acc bind in
        let acc, fe = tra acc e in
        acc,
        if bind == fbind && e == fe then expr else
          O.Letin (fbind, fe)

    | O.Letrecin (bind, e) ->
        let fmap acc ((fp, e) as cpl) =
          let acc, f_fp = foldmap_formal_param tra acc fp in
          let acc, fe = tra acc e in
          acc,
          if fp == f_fp && e == fe then cpl else (f_fp, fe)
        in
        let acc, fbind = List.fold_left_map_stable fmap acc bind in
        let acc, fe = tra acc e in
        acc,
        if bind == fbind && e == fe then expr else
          O.Letrecin (fbind, fe)

    | O.Record (rec_opt, fields) ->
        let fmap acc ((f, e) as cpl) =
          let acc, fe = tra acc e in
          acc,
          if e == fe then cpl else (f, fe)
        in
        let acc, f_fields = List.fold_left_map_stable fmap acc fields in
        acc,
        if fields == f_fields then expr else
          O.Record (rec_opt, f_fields)

    | O.Dot (e, f) ->
        let acc, fe = tra acc e in
        acc,
        if e == fe then expr else
          O.Dot (fe, f)

    | O.Match (e, pl) ->
        let fmap acc ( (p, g, e) as tpl )=
          let acc, fg = Option.foldmap_stable tra acc g in
          let acc, fe = tra acc e in
          acc,
          if g == fg && e == fe then tpl else (p, fg, fe)
        in
        let acc, fe = tra acc e in
        let acc, fpl = List.fold_left_map_stable fmap acc pl in
        acc,
        if e == fe && pl == fpl then expr else
          O.Match (fe, fpl)

    | O.Sequence (a, b) ->
        let acc, fa = tra acc a in
        let acc, fb = tra acc b in
        acc,
        if a == fa && b == fb then expr else
          O.Sequence (fa, fb)

    | O.Annot (e, ty) ->
        let acc, fe = tra acc e in
        acc,
        if e == fe then expr else
          O.Annot (fe, ty)

    | O.Function fpel ->
        let fmap acc ( (p, g, e) as tpl )=
          let acc, fg = Option.foldmap_stable tra acc g in
          let acc, fe = tra acc e in
          acc,
          if g == fg && e == fe then tpl else (p, fg, fe)
        in
        let acc, f_fpel = List.fold_left_map_stable fmap acc fpel in
        acc,
        if fpel == f_fpel then expr else
          O.Function f_fpel

    | O.Exception _ ->
        acc, expr

    | O.Raise (ident, eo) ->
        let acc, feo = Option.foldmap_stable tra acc eo in
        acc,
        if eo == feo then expr else
          O.Raise (ident, feo)

    | O.Try (e, pl) ->
        let fmap acc ( (p, g, e) as tpl )=
          let acc, fg = Option.foldmap_stable tra acc g in
          let acc, fe = tra acc e in
          acc,
          if g == fg && e == fe then tpl else (p, fg, fe)
        in
        let acc, fe = tra acc e in
        let acc, fpl = List.fold_left_map_stable fmap acc pl in
        acc,
        if e == fe && pl == fpl then expr else
          O.Try (fe, fpl)

    | O.AnArray el ->
        let acc, fel = List.fold_left_map_stable tra acc el in
        acc,
        if el == fel then expr else
          O.AnArray fel

    | O.Comment _ ->
        acc, expr

    | O.LineAnnot (i, s, e) ->
        let acc, fe = tra acc e in
        acc,
        if e == fe then expr else
          O.LineAnnot (i, s, fe)

    | O.Comments (s, e) ->
        let acc, fe = tra acc e in
        acc,
        if e == fe then expr else
          O.Comments (s, fe)

    | O.Assert e ->
        let acc, fe = tra acc e in
        acc,
        if e == fe then expr else
          O.Assert fe

    | O.Verbatim _ ->
        acc, expr

  and foldmap_formal_param tra acc fp =
    match fp with
    | O.Label _ ->
        acc, fp

    | O.Opt (label, ty, expr) ->
        let acc, fexpr = Option.foldmap_stable tra acc expr in
        acc,
        if expr == fexpr then fp else
          O.Opt (label, ty, fexpr)

    | O.Pat _ ->
        acc, fp

  and foldmap_effective_param tra acc ep =
    match ep with
    | O.Labeled (label, expr) ->
        let acc, fexpr = Option.foldmap_stable tra acc expr in
        acc,
        if expr == fexpr then ep else
          O.Labeled (label, fexpr)

    | O.Pated _ ->
        acc, ep

  and foldmap_code tra acc code = List.fold_left_map_stable tra acc code

  and foldmap_signature tra acc sign =
    match sign with
    | O.Inlined code ->
        let acc, fcode = foldmap_code tra acc code in
        acc,
        if code == fcode then sign else
          O.Inlined fcode

    | O.Referenced _ ->
        acc, sign

  let iter x = Traverse.Unoptimized.iter foldmap x
  let map x = Traverse.Unoptimized.map foldmap x
  let fold x = Traverse.Unoptimized.fold foldmap x
end

module Expr = Traverse.Make2 ( Expr_Subs )

module PatExpr =
struct
  let formal_param_pat_non_rec f_pat acc fp =
    match fp with
    | O.Label (s, pat, t) ->
        let acc, f_pat = Option.foldmap_stable f_pat acc pat in
        acc,
        if pat == f_pat then fp else
          O.Label (s, f_pat, t)
    | O.Opt _ ->
        acc, fp
    | O.Pat pat ->
        let acc, f_pat = f_pat acc pat in
        acc,
        if pat == f_pat then fp else
          O.Pat f_pat

  let fmap_fp_e f_pat acc ((fp, e) as cpl) =
    let acc, f_fp = formal_param_pat_non_rec f_pat acc fp in
    acc,
    if fp == f_fp then cpl else (f_fp, e)

  let fmap_pge f_pat acc ((p, g, e) as tpl) =
    let acc, fp = f_pat acc p in
    acc,
    if p == fp then tpl else (fp, g, e)

  let foldmap_expr_pat_non_rec f_expr f_pat acc expr =
    let foldmap acc expr =
      let acc, expr =
        match expr with

        | O.Abs (fpl, e) ->
            let acc, f_fpl = List.fold_left_map_stable (formal_param_pat_non_rec f_pat) acc fpl in
            acc,
            if fpl == f_fpl then expr else
              O.Abs (f_fpl, e)

        | O.Let fpel ->
            let acc, f_fpel = List.fold_left_map_stable (fmap_fp_e f_pat) acc fpel in
            acc,
            if fpel == f_fpel then expr else
              O.Let f_fpel

        | O.Letrec fpel ->
            let acc, f_fpel = List.fold_left_map_stable (fmap_fp_e f_pat) acc fpel in
            acc,
            if fpel == f_fpel then expr else
              O.Letrec f_fpel

        | O.Letin (fpel, e) ->
            let acc, f_fpel = List.fold_left_map_stable (fmap_fp_e f_pat) acc fpel in
            acc,
            if fpel == f_fpel then expr else
              O.Letin (f_fpel, e)

        | O.Letrecin (fpel, e) ->
            let acc, f_fpel = List.fold_left_map_stable (fmap_fp_e f_pat) acc fpel in
            acc,
            if fpel == f_fpel then expr else
              O.Letrecin (f_fpel, e)

        | O.Match (e, pgel) ->
            let acc, f_pgel = List.fold_left_map_stable (fmap_pge f_pat) acc pgel in
            acc,
            if pgel == f_pgel then expr else
              O.Match (e, f_pgel)

        | O.Function pgel ->
            let acc, f_pgel = List.fold_left_map_stable (fmap_pge f_pat) acc pgel in
            acc,
            if pgel == f_pgel then expr else
              O.Function f_pgel

        | O.Try (e, pgel) ->
            let acc, f_pgel = List.fold_left_map_stable (fmap_pge f_pat) acc pgel in
            acc,
            if pgel == f_pgel then expr else
              O.Try (e, f_pgel)

        | _ -> acc, expr
      in
      f_expr acc expr
    in
    Expr.foldmap foldmap acc expr

  let foldmap f_expr f_pat acc expr =
    let f_pat acc pat = Pat.foldmap f_pat acc pat in
    foldmap_expr_pat_non_rec f_expr f_pat acc expr

  let fold f_expr f_pat acc expr =
    let f_expr acc expr = f_expr acc expr, expr in
    let f_pat acc pat = f_pat acc pat, pat in
    let acc, _ = foldmap f_expr f_pat acc expr in
    acc

  let map f_expr f_pat expr =
    let f_expr () expr = (), f_expr expr in
    let f_pat () pat = (), f_pat pat in
    let (), expr = foldmap f_expr f_pat () expr in
    expr

  let iter f_expr f_pat expr =
    let f_expr expr = let () = f_expr expr in expr in
    let f_pat pat = let () = f_pat pat in pat in
    let _ = map f_expr f_pat expr in
    ()

  let foldmap_code f_expr f_pat acc code = List.fold_left_map_stable (foldmap f_expr f_pat) acc code
  let fold_code f_expr f_pat acc code = List.fold_left (fold f_expr f_pat) acc code
  let map_code f_expr f_pat code = List.map_stable (map f_expr f_pat) code
  let iter_code f_expr f_pat code = List.iter (iter f_expr f_pat) code
end
