(*
    Copyright © 2012 MLstate

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

module List = BaseList
module Format = BaseFormat

module Q = QmlAst

module J = JsAst
module C = JsCons
module CE = C.Expr
module CS = C.Statement
module CI = C.Ident

type info = {
  cps : [
  | `skipped of Ident.t * Ident.t
  | `unskipped of Ident.t
  ];
  type_ : QmlAst.ty;
  ei : bool;
  fields : infos;
}

and infos = info StringMap.t

type env = {
  infos : infos;
  gamma : QmlTypes.Env.t;
  package : ObjectFiles.package;
}

let pp_info fmt info = Format.fprintf fmt "{@[cps: %a;@]@\n@[type_: %a;@]@\n@[ei: %b;@]@\n@[fields: %a;@]}"
  (fun fmt -> function
   | `skipped (skip, cps) ->
       Format.fprintf fmt "skipped(%a, %a)"
         QmlPrint.pp#ident skip
         QmlPrint.pp#ident cps
   | `unskipped (cps) ->
       Format.fprintf fmt "unskipped(%a)"
         QmlPrint.pp#ident cps)
  info.cps
  QmlPrint.pp#ty info.type_
  info.ei
  (StringMap.pp ", " (fun fmt k _ -> Format.fprintf fmt "%s" k)) info.fields

let build_env ~package ~renaming ~gamma ~undot ~skipped ~ei =
  ignore ei;
  let add_value path ident type_ fields env =
    let cps =
      try
        `skipped (IdentMap.find ident skipped, ident)
      with Not_found -> `unskipped ident
    in
    let info = {cps; type_; ei = false; fields} in
    StringMap.add path info env
  in
  let infos =
    StringMap.fold
      (fun s (i, _) env ->
         let rec aux path ident env =
           let tsc = QmlTypes.Env.Ident.find ident gamma in
           let oty = QmlTypes.Scheme.instantiate tsc in
           let ity = QmlTypesUtils.Inspect.follow_alias_noopt gamma oty in
           let fields =
             match ity with
             | Q.TypeRecord Q.TyRow (fields, _rvar) ->
                 begin try
                   let fmap = IdentMap.find ident undot in
                   List.fold_left
                     (fun env (f, _type_) ->
                        match StringMap.find f fmap with
                        | Q.Ident (_, ident) ->
                            aux f ident env
                        | _ -> assert false
                     ) StringMap.empty fields
                 with Not_found -> StringMap.empty
                 end
             | _ -> StringMap.empty
           in
           add_value path ident oty fields env
         in
         aux s i env
      ) renaming StringMap.empty
  in {infos; gamma; package}

let genid level i =
  CI.native (
    if level = 0 then IdentGenerator.alphanum i
    else Printf.sprintf "%s%i" (IdentGenerator.alphanum i) level
  )

let rev_way = function
  | `opa2js c -> `js2opa c
  | `js2opa c -> `opa2js c

let funproj, tmpproj, setproj, getprojs =
  let tbl = Hashtbl.create 11 in
  (fun (ident, args, way) ->
     try
       begin match Hashtbl.find tbl (ident, args, way) with
       | `tmp i | `fun_ (i, _) -> Some (`ident i)
       | `noproj -> Some `noproj
       end
     with Not_found -> None
  ),
  (fun (ident, args, way) ->
     let r = (Ident.refreshf ~map:"proj%s" ident) in
     Hashtbl.add tbl (ident, args, way) (`tmp r);
     r
  ),
  (fun (ident, args, way) x ->
     Hashtbl.add tbl (ident, args, way) x),
  (fun () ->
     let x = Hashtbl.fold
       (fun _ e acc ->
          match e with
          | `fun_ (_,e) -> e::acc
          | `noproj -> acc
          | `tmp i ->
              Format.eprintf "%a\n" QmlPrint.pp#ident i;
              acc
       ) tbl [] in
     Hashtbl.clear tbl;
     x)

let get_tmp_var, flush_tmp_var =
  let rvars = ref (0, []) in
  (fun () ->
     let nb, vars = !rvars in
     let newv = CI.native (Printf.sprintf "tmp%i" nb) in
     rvars := (nb+1, newv::vars);
     newv),
  (fun () ->
     let _, vars = !rvars in
     rvars := (0, []);
     vars)

(* TODO: No direct call to "cont", we should really resolve the cont_native
   bypass*)
let rec cont_native level gamma k (ret:QmlAst.ty) =
  let k =
    let kparam = CI.native "r" in
    let proj, e = project level gamma (CE.ident kparam) ret (`opa2js `cps) in
    if proj then
      CE.function_ None [kparam] [CS.return (CE.call (CE.ident k) [e])]
    else (CE.ident k)
  in CE.call (CE.native_global "cont") [k]


(* Project an Opa continuation to a JavaScript callback
   function(r){return_(k, r)} *)
and uncont_native level gamma k (ret:QmlAst.ty) =
  let rpar = CI.native "r" in
  let rarg =
    match project level gamma (CE.ident rpar) ret (`js2opa `cps) with
    | false, _ -> CE.ident rpar
    | true, e -> e
  in
  CE.function_ None [rpar]
    [CS.expr (CE.call (CE.native_global "return_") [(CE.ident k); rarg])]

and project_lambda_args level gamma args way =
  List.fold_left
    (fun (i, proj, params, args) t ->
       let ident = genid level i in
       let expr = CE.ident ident in
       let p, expr = project (level+1) gamma expr t (rev_way way) in
       i+1, p || proj, ident :: params, expr :: args
    ) (0, false, [], []) args

(* function(..., k){
     if (k==undefined){
       return (uncps(expr))(...);
     } else {
       return expr(..., cont_native(k))}
     }
   }
*)
and project_lambda_opa2js_cps level gamma args ret expr =
  let nb, _, rparams, rargs = project_lambda_args level gamma args (`opa2js `cps) in
  let k = genid 0 (nb+1) in
  let params = List.rev (k::rparams) in
  let cpse =
    let args = List.rev ((cont_native 0 gamma k ret)::rargs) in
    CS.return (CE.call expr args)
  in
  let uncps =
    let uncps = CE.call (CE.native_global "uncps") [CE.null (); expr] in
    let _, ret = project 0 gamma (CE.call uncps (List.rev rargs)) ret (`opa2js `cps) in
    CS.return ret
      in
  true,
  CE.function_ None params
    [CS.if_ (CE.equality (CE.ident k) (CE.undefined ()))
       uncps cpse
    ]

and project_lambda level gamma args (ret:QmlAst.ty) expr way =
  let nb, proj, rparams, rargs = project_lambda_args level gamma args way in
  match way with
  | `opa2js `cps ->
      project_lambda_opa2js_cps level gamma args ret expr

  | `js2opa `cps ->
      (* expr.length == arity
         ? function(..., k){return_(k, expr(...))}
         : function(..., k){return expr(..., function(r){return_(k, r)})}
      *)
      let k = genid level (nb+1) in
      let params = List.rev (k::rparams) in
      let cpsjs =
        let args = List.rev ((uncont_native level gamma k ret)::rargs) in
        CE.function_ None params [CS.return (CE.call expr args)]
      in
      let nocpsjs =
        let args = List.rev rargs in
        let _, expr = project (level+1) gamma (CE.call expr args) ret (`js2opa `no) in
        CE.function_ None params [
          CS.expr (CE.call (CE.native_global "return_") [(CE.ident k); expr])
        ]
      in
      true,
      CE.cond
        (CE.equality (CE.dot expr "length") (CE.int (List.length args)))
        nocpsjs
        cpsjs

  | `opa2js `no
  | `js2opa `no ->
      (* function(...){return proj(expr(...))} *)
      let p, ret = project (level+1) gamma
        (CE.call ~pure:false expr (List.rev rargs)) ret
        (rev_way way)
      in
      let proj = p || proj in
      proj,
      if proj then CE.function_ None (List.rev rparams) [CS.return ret]
      else expr

and project_option level gamma expr ty way =
  true,
  match way with
  | `opa2js _ ->
      let expr = CE.call Imp_Common.ClientLib.udot [expr; CE.string "some"] in
      let tmp = get_tmp_var () in
      CE.cond (CE.equality (CE.assign (CE.ident tmp) expr) (CE.undefined ()))
        (CE.undefined ())
        (snd (project level gamma expr ty way))
  | `js2opa _ ->
      let tmp = get_tmp_var () in
      let _, expr = project level gamma expr ty way in
      CE.cond (CE.equality (CE.assign (CE.ident tmp) expr) (CE.undefined ()))
        (CE.obj ["none", CE.obj []])
        (CE.obj ["some", (CE.ident tmp)])

and project_bool expr way =
  match way with
  | `opa2js _ -> true, CE.call (CE.native_global "un_uniformize_bool") [expr]
  | `js2opa _ -> false, expr

and project level gamma expr (ty:QmlAst.ty) way =
  match ty with
  | Q.TypeConst _
  | Q.TypeVar _
  | Q.TypeAbstract _ -> false, expr
  | Q.TypeArrow (args, ret) -> project_lambda level gamma args ret expr way
  | Q.TypeRecord Q.TyRow (fields, _rvar) ->
      let fields, proj = List.fold_right_map
        (fun (s, t) proj ->
           let p, expr = project (level+1) gamma (CE.dot expr s) t way in
           (s, expr), p || proj
        ) fields false
      in
      proj, (if proj then CE.obj fields else expr)
  | Q.TypeName (args, ident) ->
      let pargs = List.map
        (function |Q.TypeVar _v -> Q.TypeVar (QmlTypeVars.get_canonical_typevar 0)
         | x -> x) args
      in
      begin
        match funproj (ident, pargs, way) with
        | Some `ident i -> true, CE.call (CE.exprident i) [expr]
        | Some `noproj -> false, expr
        | None ->
            match args, Ident.original_name ident with
            | [p], "option" ->
                project_option level gamma expr p way
            | _, "bool" ->
                project_bool expr way
            | _ ->
                let i = tmpproj (ident, pargs, way) in
                let param = CI.native "p" in
                let proj, body =
                  project level gamma (CE.ident param)
                    (QmlTypesUtils.Inspect.find_and_specialize gamma ident args)
                    way
                in
                if proj then (
                  setproj (ident, pargs, way)
                    (`fun_ (i, CS.function_ (CI.ident i) [param] [CS.return body]));
                  true, CE.call (CE.exprident i) [expr]
                ) else (
                  setproj (ident, pargs, way) `noproj;
                  false, expr
                )
      end
  | Q.TypeForall (_vars, _rvars, _cvars, ty) ->
      project level gamma expr ty way

  | Q.TypeSum _
  | Q.TypeSumSugar _ as ty ->
      Format.eprintf "FIXME NO PROJECTION %a\n%!" QmlPrint.pp#ty ty;
      false, expr

let top_lambda gamma args ret cps_info =
  let finalize statements =
    match flush_tmp_var () with
    | [] -> statements
    | vars -> List.rev_map_append CS.def vars statements
  in
  match cps_info with
  | `unskipped cps ->
      (* function(..., k){
           if (k==undefined){
             return (uncps(expr))(...);
           } else {
             return expr(..., cont_native(k))}
           }
         }
      *)
      begin match project_lambda_opa2js_cps 0 gamma args ret (CE.exprident cps) with
      | _, J.Je_function (label, i, p, b) ->
          CE.function_ ~label i p (finalize b)
      | _ -> assert false
      end

  | `skipped (skip, cps) ->
      (* function(..., k){
           if(k == undefined) return skip(...)
           else return cps(..., cont_native(k))
         }
      *)
      let nb, _, rparams, rargs = project_lambda_args 0 gamma args (`opa2js `no) in
      let _, _, _, cpsrargs = project_lambda_args 0 gamma args (`opa2js `cps) in
      let k = genid 0 (nb+1) in
      let params = List.rev (k::rparams) in
      let cps =
        let args = List.rev ((cont_native 0 gamma k ret)::cpsrargs) in
        CS.return (CE.call (CE.exprident cps) args)
      in
      let skip =
        CS.return (CE.call (CE.exprident skip) (List.rev rargs))
      in
      CE.function_ None params
        (finalize [
           CS.if_ (CE.equality (CE.ident k) (CE.undefined ()))
             skip cps
         ])

let rec info_to_js gamma info =
  match QmlTypesUtils.Inspect.follow_alias_noopt gamma info.type_ with
  | Q.TypeArrow (args, ret) ->
      assert (info.fields = StringMap.empty);
      top_lambda gamma args ret info.cps
  | _ ->
      match info.cps with
      | `unskipped ident ->
          if StringMap.is_empty info.fields then
            snd (project 0 gamma (CE.exprident ident) info.type_ (`opa2js `cps))
          else
            CE.obj (
              StringMap.fold
                (fun s info acc -> (s, info_to_js gamma info) :: acc)
                info.fields []
            )
      | _ -> assert false

let process {gamma; infos; package} =
  let require =
    CS.expr
      (CE.call
         (CE.native_global "require")
         [CE.string (Printf.sprintf "%s.opx" (fst package))])
  in
  let exports =
    StringMap.fold
      (fun s info acc ->
         let expr = info_to_js gamma info in
         let exports = CE.native_global "exports" in
         let exports = CE.dot exports s in
         (CS.expr (CE.assign exports expr)) :: acc
      ) infos []
  in
  require :: getprojs () @ exports
