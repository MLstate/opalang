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

module List = BaseList

module J = JsAst

let arguments_ident = JsCons.Ident.native "arguments"

let maybe_globalize_ident local_vars = function
  | J.ExprIdent _ as i -> i
  | J.Native (`global _,_) as i -> i
  | J.Native (`local,j) as i ->
      if JsIdentSet.mem i local_vars then i else J.Native (`global true,j)

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
      let fbody = traverse_stm local_vars body in
      let ffinally = Option.map (traverse_stm local_vars) finally in
      J.Js_trycatch (label, fbody, fcatches, ffinally)
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

let prefix_with (prefix : string) (name : string) : J.expr =
  (* When prefixing with identifiers such as [global], we
     must not ignore properties inherited from object, otherwise
     we lose global functions such as toString. *)
  JsCons.Expr.dot ~own_property:false
     (JsCons.Expr.native_global prefix) name

let export_to_global_namespace_aux stm =
  JsWalk.TStatement.map
    (fun stm ->
      match stm with
      | J.Js_function (_, J.Native (`global _, name), params, body) ->
        JsCons.Statement.assign (prefix_with "global" name)
          (JsCons.Expr.function_ None params body)
      | J.Js_function (_, ((J.ExprIdent _) as i), params, body) ->
        let name = JsPrint.string_of_ident i in
        JsCons.Statement.assign (prefix_with "global" name)
          (JsCons.Expr.function_ None params body)
      | J.Js_var (_, J.Native (`global _, name), o) ->
        let rhs =
          match o with
          | Some e -> e
          | None -> JsCons.Expr.undefined () in
        JsCons.Statement.assign (prefix_with "global" name) rhs
      | _ -> stm
    )
    (fun e ->
      match e with
      | J.Je_ident (_, J.Native (`global _, "exports")) ->
        (* Since exports is not defined in the global scope
           when commonjs modules are loaded, we need to replace
           it *)
        JsCons.Expr.native_global "global"
      | J.Je_ident (_, J.Native (`global _, name)) when
          name <> "global" &&
          name <> "require" (* Hack to avoid require scope problem *) ->
        prefix_with "global" name
      | _ -> e
    ) stm

let export_to_global_namespace code =
  List.map (fun stm ->
    export_to_global_namespace_aux (globalize_native_ident stm)
  ) code

let export_global_declarations_aux exports stm =
  let maybe_export_ident exports ident =
    match ident with
    | J.Native (`global _, _)
    | J.ExprIdent _ ->
      JsIdentSet.add ident exports
    | _ -> exports in
  JsWalk.OnlyStatement.fold (fun exports stm ->
    match stm with
    | J.Js_function (_, ident, _, _) ->
      maybe_export_ident exports ident
    | J.Js_var (_, ident, _) ->
      maybe_export_ident exports ident
    | _ -> exports
  ) exports stm

let export_global_declarations code =
  let code = List.map globalize_native_ident code in
  let exports = List.fold_left (fun exports stm ->
    export_global_declarations_aux exports stm
  ) JsIdentSet.empty code in
  let exports = JsIdentSet.fold (fun ident exports ->
    let export =
      JsCons.Statement.assign
        (prefix_with "exports" (JsPrint.string_of_ident ident))
        (JsCons.Expr.ident ident) in
    export :: exports
  ) exports [] in
  code @ exports

let basic_package_json ?(version="0.1.0") name main =
  Printf.sprintf (
    "{\n" ^^
    "  \"name\": \"%s\",\n" ^^
    "  \"version\": \"%s\",\n" ^^
    "  \"main\": \"%s\"\n" ^^
    "}\n"
  ) name version main

let compares x cmp x0 x1 = if x <> 0 then x else cmp x0 x1

let compare_ident i0 i1 =
  match i0, i1 with
  | J.ExprIdent i0, J.ExprIdent i1 -> Ident.compare i0 i1
  | J.Native (t0, s0), J.Native (t1, s1) ->
      compares (Pervasives.compare t0 t1) String.compare s0 s1
  | _, _ -> Pervasives.compare i0 i1

let compare_unop = Pervasives.compare

let compare_binop = Pervasives.compare

let rec compare_expr e1 e2 =
  match e1, e2 with
  | J.Je_this (_), J.Je_this (_) -> 0
  | J.Je_ident (_ , i0), J.Je_ident (_ , i1) -> compare_ident i0 i1
  | J.Je_array (_ , l0), J.Je_array (_ , l1) -> List.make_compare compare_expr l0 l1
  | J.Je_comma (_ , l0, e0), J.Je_comma (_ , l1, e1) ->
      compares (List.make_compare compare_expr l0 l1) compare_expr e0 e1
  | J.Je_object (_ , l0), J.Je_object (_ , l1) ->
      List.make_compare
        (fun (f0, e0) (f1, e1) -> compares (String.compare f0 f1) compare_expr e0 e1)
        l0 l1
  | J.Je_string (_ , s0, b0), J.Je_string (_ , s1, b1) -> Pervasives.compare (s0, b0) (s1, b1)
  | J.Je_num (_ , s0), J.Je_num (_ , s1) -> String.compare s0 s1
  | J.Je_null (_), J.Je_null (_) -> 0
  | J.Je_undefined (_), J.Je_undefined (_) -> 0
  | J.Je_bool (_ , b0), J.Je_bool (_ , b1) -> Pervasives.compare b0 b1
  | J.Je_regexp (_ , s0 , s0'), J.Je_regexp (_ , s1 , s1') ->
      Pervasives.compare (s0, s0') (s1, s1')
  | J.Je_function (_ , _i0, p0, b0), J.Je_function (_ , _i1, p1, b1) ->
      compares (List.make_compare compare_ident p0 p1) compare_code b0 b1
  | J.Je_dot (_ , e0, s0), J.Je_dot (_ , e1, s1) ->
      compares (compare_expr e0 e1) String.compare s0 s1
  | J.Je_unop (_ , u0, e0), J.Je_unop (_ , u1, e1) ->
      compares (compare_unop u0 u1) compare_expr e0 e1
  | J.Je_binop (_ , b0, r0, l0), J.Je_binop (_ , b1, r1, l1) ->
      compares (compare_binop b0 b1) (compares (compare_expr l0 l1) compare_expr) r0 r1
  | J.Je_cond (_ , c0, e0, a0), J.Je_cond (_ , c1, e1, a1) ->
      compares (compare_expr c0 c1) (compares (compare_expr a0 a1) compare_expr) e0 e1
  | J.Je_call (_ , f0, a0, _), J.Je_call (_ , f1, a1, _) ->
      compares (compare_expr f0 f1) (List.make_compare compare_expr) a0 a1
  | J.Je_new (_ , e0, a0), J.Je_new (_ , e1, a1) ->
      compares (compare_expr e0 e1) (List.make_compare compare_expr) a0 a1
  | J.Je_hole (_ , e0), J.Je_hole (_ , e1) ->Pervasives.compare e0 e1
  | J.Je_runtime (_ , e0), J.Je_runtime (_ , e1) -> Pervasives.compare e0 e1
  | x0, x1 -> Pervasives.compare x0 x1

and compare_statement s0 s1 =
  match s0, s1 with
  | J.Js_var (_, i0, e0), J.Js_var (_, i1, e1) ->
      compares (compare_ident i0 i1) (Option.make_compare compare_expr) e0 e1
  | J.Js_function (_, _i0, p0, b0), J.Js_function (_, _i1, p1, b1) ->
      compares (List.make_compare compare_ident p0 p1) compare_code b0 b1
  | J.Js_return (_, e0), J.Js_return (_, e1) -> (Option.make_compare compare_expr) e0 e1
  | J.Js_continue (_, l0), J.Js_continue (_, l1) ->
      Option.make_compare String.compare l0 l1
  | J.Js_break (_, l0), J.Js_break (_, l1) ->
      Option.make_compare String.compare l0 l1
  | J.Js_switch (_, e0, c0, d0), J.Js_switch (_, e1, c1, d1) ->
      compares (compare_expr e0 e1)
        (compares (Option.make_compare compare_statement d0 d1)
           (List.make_compare (fun (e0, s0) (e1, s1) ->
                            compares (compare_expr e0 e1) compare_statement s0 s1))
        )
        c0 c1
  | J.Js_if (_ , c0, e0, a0), J.Js_if (_ , c1, e1, a1) ->
      compares (compare_expr c0 c1) (compares (Option.make_compare compare_statement a0 a1) compare_statement) e0 e1
  | J.Js_throw (_, e0), J.Js_throw (_, e1) -> compare_expr e0 e1
  | J.Js_expr (_, e0), J.Js_expr (_, e1) -> compare_expr e0 e1
  | J.Js_trycatch (_, b0, c0, f0), J.Js_trycatch (_, b1, c1, f1) ->
      let aux_cmp (i0, e0, s0) (i1, e1, s1) =
        compares (compare_ident i0 i1)
          (compares (Option.make_compare compare_expr e0 e1) compare_statement) s0 s1
      in
      compares (compare_statement b0 b1)
        (compares (Option.make_compare compare_statement f0 f1) (List.make_compare aux_cmp))
        c0 c1
  | J.Js_for (_, i0, c0, e0, b0), J.Js_for (_, i1, c1, e1, b1) ->
      compares (Option.make_compare compare_expr i0 i1)
        (compares
           (Option.make_compare compare_expr c0 c1)
           (compares (compare_statement b0 b1) (Option.make_compare compare_expr))
        ) e0 e1
  | J.Js_forin (_, lhs0, rhs0, b0), J.Js_forin (_, lhs1, rhs1, b1) ->
      compares (compare_expr lhs0 lhs1) (compares (compare_expr rhs0 rhs1) compare_statement) b0 b1
  | J.Js_dowhile (_, b0, e0), J.Js_dowhile (_, b1, e1) ->
      compares (compare_statement b0 b1) compare_expr e0 e1
  | J.Js_while (_, e0, b0), J.Js_while (_, e1, b1) ->
      compares (compare_statement b0 b1) compare_expr e0 e1
  | J.Js_block (_, b0), J.Js_block (_, b1) ->
      compare_code b0 b1
  | J.Js_with (_, e0, b0), J.Js_with (_, e1, b1) ->
      compares (compare_statement b0 b1) compare_expr e0 e1
  | J.Js_label (_, l0, s0), J.Js_label (_, l1, s1) ->
      compares (String.compare l0 l1) compare_statement s0 s1
  | J.Js_empty _, J.Js_empty _ -> 0
  | J.Js_comment (_, _), J.Js_comment (_, _) -> 0
  | s0, s1 -> Pervasives.compare s0 s1

and compare_code s0 s1 = List.make_compare compare_statement s0 s1
