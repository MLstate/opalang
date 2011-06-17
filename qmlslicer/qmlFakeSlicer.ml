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

(* assuming bypass hoisting has been done
 * so bypass are named and are only at the toplevel *)
let gather_info ~bymap ~lang ~annotmap (code : QmlAst.code) =
  QmlAstWalk.CodeExpr.fold_map
    (QmlAstWalk.Expr.traverse_foldmap
       (fun tra annotmap e ->
          match e with
          | Q.Bypass (label,key) when not (locally_defined_bypass ~bymap ~lang key) ->
              let error_string =
                Printf.sprintf "Error: trying to use a discarded remote call to %s"
                  (BslKey.to_string key) in
              let annotmap, error_string_expr = QmlAstCons.TypedExpr.string annotmap error_string in
              annotmap, Q.Directive (label,`fail,[error_string_expr],[])
          | _ ->
              tra annotmap e
       )
    ) annotmap code

let discard_remote_bypasses ~bymap ~lang gamma annotmap code =
  let annotmap, code = gather_info ~bymap ~lang ~annotmap code in
  (gamma, annotmap), code
