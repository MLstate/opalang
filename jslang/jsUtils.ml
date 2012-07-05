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
module J = JsAst

let arguments_ident = JsCons.Ident.native "arguments"

let maybe_globalize_ident local_vars = function
  | J.ExprIdent _ as i -> i
  | J.Native (`global,_) as i -> i
  | J.Native (`local,j) as i ->
      if JsIdentSet.mem i local_vars then i else J.Native (`global,j)

let collect_local_var local_vars stm =
  (* not going inside the functions that are inside expressions on purpose.
   * The identifiers defined there are not in scope *)
  JsWalk.OnlyStatement.fold
    (fun local_vars -> function
     | J.Js_function (_,name,_,_)
     | J.Js_var (_,name,_) -> JsIdentSet.add name local_vars
     | _ -> local_vars) local_vars stm
let collect_local_vars local_vars stms =
  List.fold_left collect_local_var local_vars stms

let globalize_native_ident stm =
  let rec traverse_stm local_vars =
    JsWalk.TStatement.map_nonrec
      (aux_stm local_vars) (aux_expr local_vars)
  and traverse_expr local_vars =
    JsWalk.TExpr.map_nonrec
      (aux_expr local_vars) (aux_stm local_vars)
  and aux_stm local_vars stm =
    match stm with
    | J.Js_function (label, name, params, body) ->
      let fname = maybe_globalize_ident local_vars name in
      let local_vars = JsIdentSet.add arguments_ident local_vars in
      let local_vars =
        List.fold_left (fun local_vars i ->
          JsIdentSet.add i local_vars
        ) local_vars params
      in
      let local_vars = collect_local_vars local_vars body in
      let stm =
        if name == fname then
          stm
        else
          J.Js_function (label,fname,params,body)
      in
      traverse_stm local_vars stm
    | J.Js_var (label, name, o) ->
      let fname = maybe_globalize_ident local_vars name in
      let stm =
        if name == fname then
          stm
        else
          J.Js_var (label,fname, o)
      in
      traverse_stm local_vars stm
    | J.Js_trycatch (label, body, catches, finally) ->
      (* We traverse the catch statements by ourselves since
         the standard JsWalk fold doesn't take binding variables
         into account *)
      let fcatches =
        List.map (fun (ident, expr, body) ->
          let local_vars = JsIdentSet.add ident local_vars in
          (ident, expr, traverse_stm local_vars body)
        ) catches
      in
      J.Js_trycatch (label, body, fcatches, finally)
    | _ ->
      traverse_stm local_vars stm
  and aux_expr local_vars expr =
    match expr with
    | J.Je_function (_, _, params, body) ->
      let local_vars = JsIdentSet.add arguments_ident local_vars in
      let local_vars =
        List.fold_left (fun local_vars i ->
          JsIdentSet.add i local_vars
        ) local_vars params
      in
      let local_vars = collect_local_vars local_vars body in
      traverse_expr local_vars expr
    | J.Je_ident (label, name) ->
      let fname = maybe_globalize_ident local_vars name in
      let expr = if name == fname then expr else J.Je_ident (label,fname) in
      traverse_expr local_vars expr
    | _ ->
      traverse_expr local_vars expr
  in
  aux_stm JsIdentSet.empty stm

let prefix_global (name : string) : J.expr =
  JsCons.Expr.dot (JsCons.Expr.native_global "global") name

let export_to_global_namespace stm =
  JsWalk.TStatement.map
    (fun stm ->
      match stm with
      | J.Js_function (_, J.Native (`global, name), params, body) ->
        JsCons.Statement.assign (prefix_global name)
          (JsCons.Expr.function_ None params body)
      | J.Js_function (_, ((J.ExprIdent _) as i), params, body) ->
        let name = JsPrint.string_of_ident i in
        JsCons.Statement.assign (prefix_global name)
          (JsCons.Expr.function_ None params body)
      | J.Js_var (_, J.Native (`global, name), o) ->
        let rhs =
          match o with
          | Some e -> e
          | None -> JsCons.Expr.undefined () in
        JsCons.Statement.assign (prefix_global name) rhs
      | _ -> stm
    )
    (fun e ->
      match e with
      | J.Je_ident (_, J.Native (`global, "exports")) ->
        (* Since exports is not defined in the global scope
           when commonjs modules are loaded, we need to replace
           it *)
        JsCons.Expr.native_global "global"
      | J.Je_ident (_, J.Native (`global, name)) when
          name <> "global" &&
          name <> "require" (* Hack to avoid require scope problem *) ->
        prefix_global name
      | _ -> e
    ) stm
