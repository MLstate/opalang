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
module Format = Base.Format
module List = Base.List
module String = Base.String
module Q = QmlAst

(*-----------------------------------*)
(*---- removing distant bypasses ----*)
(*-----------------------------------*)

let locally_defined_bypass ~bymap ~lang bypass =
  match BslLib.BSL.ByPassMap.find_opt bymap bypass with
  | None -> true (* may happen with wrong bypasses, so just say true
                  * so that it is left untouched *)
  | Some bypass ->
      let langs = BslLib.BSL.ByPass.langs bypass in
      List.mem lang langs

let fresh_ty () =  Q.TypeVar (Q.TypeVar.next ())

type info =
  { rewritten : IdentSet.t }
  (* the set of identifier that became functions -> its uses need to be applied *)

let empty_info =
  { rewritten = IdentSet.empty }

let make_error_expr ~annotmap ~bypass ~label =
  let pos = Annot.pos label in
  let error_string =
    Printf.sprintf "Error: trying to use a discarded remote call to %s"
      (BslKey.to_string bypass)
  in
  let arity =
    match QmlAnnotMap.find_ty_label label annotmap with
    | QmlAst.TypeArrow (l,_) -> List.length l
    | _ -> 1 in
  let args = List.init arity (fun _ -> Ident.next "_", fresh_ty ()) in
  let annotmap, error_string_expr =
    QmlAstCons.TypedExpr.string annotmap error_string in
  let annotmap, body =
    QmlAstCons.TypedExpr.directive ~pos annotmap `fail [error_string_expr] [] in
  let annotmap, e =
    QmlAstCons.TypedExpr.lambda annotmap args body in
  annotmap, e

(* assuming bypass hoisting has been done
 * so bypass are named and are only at the toplevel *)
let gather_info ~bymap ~lang ~annotmap (code : QmlAst.code) =
  QmlAstWalk.CodeExpr.fold_map_name_expr
    (fun ((annotmap, info) as acc) ((i,e) as binding) ->
       match e with
       | Q.Directive (label, `expanded_bypass, [ Q.Bypass (_, bypass) ], _) ->
           if locally_defined_bypass ~bymap ~lang bypass then
             acc, binding
           else
             (* non functional bypass -> we make it a function *)
             let info = {rewritten = IdentSet.add i info.rewritten} in
             let annotmap, e = make_error_expr ~bypass ~label ~annotmap in
             (annotmap, info), (i, e)
       | Q.Directive (_, `expanded_bypass, [e], _) ->
           let bypass =
             Option.get (
               QmlAstWalk.Expr.findmap
                 (function
                    | Q.Bypass (_, s) -> Some s
                    | _ -> None) e
             ) in
           if locally_defined_bypass ~bymap ~lang bypass then
             (* the bypass exists: do not change anything *)
             acc, binding
           else
             (* the bypass does not exists: replace it with the error *)
             let annotmap, e = make_error_expr ~bypass ~label:(Q.Label.expr e) ~annotmap in
             (annotmap,info), (i,e)
       | _ ->
           acc, binding) (annotmap,empty_info) code

let propagate_functionalization info ~gamma ~annotmap (code : QmlAst.code) =
  QmlAstWalk.CodeExpr.fold_map
    (fun annotmap e ->
       QmlAstWalk.Expr.foldmap
         (fun annotmap -> function
            | Q.Ident (label, s) when IdentSet.mem s info.rewritten ->
                let annotmap, unit = QmlAstCons.TypedExpr.unit annotmap in
                let pos = Annot.pos label in
                let annotmap, ident = QmlAstCons.TypedExpr.ident ~pos annotmap s (fresh_ty ()) in
                QmlAstCons.TypedExpr.apply gamma annotmap ident [unit]
            | e -> annotmap, e) annotmap e
    ) annotmap code

let discard_remote_bypasses ~bymap ~lang gamma annotmap code =
  let (annotmap, info), code = gather_info ~bymap ~lang ~annotmap code in
  let annotmap, code = propagate_functionalization info ~gamma ~annotmap code in
  (gamma, annotmap), code
