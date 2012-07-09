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
(**
   Traversing JsAst
   @author Mathieu Barbin
*)

module List = BaseList
module J = JsAst

let foldmapA traA traB acc e =
  match e with
  | J.Je_this _ ->
      acc, e

  | J.Je_ident (_, _) ->
      acc, e

  | J.Je_array (label, list) ->
      let acc, flist = List.fold_left_map_stable traA acc list in
      acc,
      if list == flist then e else
        J.Je_array (label, flist)

  | J.Je_comma (label, list, last) ->
      let acc, flist = List.fold_left_map_stable traA acc list in
      let acc, flast = traA acc last in
      acc,
      if list == flist && last == flast then e else
        J.Je_comma (label, flist, flast)

  | J.Je_object (label, fields) ->
      let fmap acc ((field, b) as c) =
        let acc, fb = traA acc b in
        acc,
        if b == fb then c else
          (field, fb)
      in
      let acc, ffields = List.fold_left_map_stable fmap acc fields in
      acc,
      if fields == ffields then e else
        J.Je_object (label, ffields)

  | J.Je_string (_, _, _) ->
      acc, e

  | J.Je_num (_, _) ->
      acc, e

  | J.Je_null _ ->
      acc, e

  | J.Je_undefined _ ->
      acc, e

  | J.Je_bool (_, _) ->
      acc, e

  | J.Je_regexp _ ->
      acc, e

  | J.Je_function (label, ident, params, body) ->
      let acc, fbody = List.fold_left_map_stable traB acc body in
      acc,
      if body == fbody then e else
        J.Je_function (label, ident, params, fbody)

  | J.Je_dot (label, expr, field) ->
      let acc, fexpr = traA acc expr in
      acc,
      if expr == fexpr then e else
        J.Je_dot (label, fexpr, field)

  | J.Je_unop (label, op, expr) ->
      let acc, fexpr = traA acc expr in
      acc,
      if expr == fexpr then e else
        J.Je_unop (label, op, fexpr)

  | J.Je_binop (label, op, expr1, expr2) ->
      let acc, fexpr1 = traA acc expr1 in
      let acc, fexpr2 = traA acc expr2 in
      acc,
      if expr1 == fexpr1 && expr2 == fexpr2 then e else
        J.Je_binop (label, op, fexpr1, fexpr2)

  | J.Je_cond (label, cond, then_, else_) ->
      let acc, fcond = traA acc cond in
      let acc, fthen_ = traA acc then_ in
      let acc, felse_ = traA acc else_ in
      acc,
      if cond == fcond && then_ == fthen_ && else_ == felse_ then e else
        J.Je_cond (label, fcond, fthen_, felse_)

  | J.Je_call (label, fun_, args, pure) ->
      let acc, ffun_ = traA acc fun_ in
      let acc, fargs = List.fold_left_map_stable traA acc args in
      acc,
      if fun_ == ffun_ && args == fargs then e else
        J.Je_call (label, ffun_, fargs, pure)

  | J.Je_new (label, obj, args) ->
      let acc, fobj = traA acc obj in
      let acc, fargs = List.fold_left_map_stable traA acc args in
      acc,
      if obj == fobj && args == fargs then e else
        J.Je_new (label, fobj, fargs)

  | J.Je_hole (_, _) ->
      acc, e

  | J.Je_runtime _ ->
      acc, e

let foldmapB traB traA acc e =
  match e with
  | J.Js_var (_, _, None) ->
      acc, e
  | J.Js_var (label, ident, Some expr) ->
      let acc, fexpr = traA acc expr in
      acc,
      if expr == fexpr then e else
        J.Js_var (label, ident, Some fexpr)

  | J.Js_function (label, ident, params, body) ->
      let acc, fbody = List.fold_left_map_stable traB acc body in
      acc,
      if body == fbody then e else
        J.Js_function (label, ident, params, fbody)

  | J.Js_return (label, expr) ->
      let acc, fexpr = Option.foldmap_stable traA acc expr in
      acc,
      if expr == fexpr then e else
        J.Js_return (label, fexpr)

  | J.Js_continue (_, _) ->
      acc, e

  | J.Js_break (_, _) ->
      acc, e

  | J.Js_switch (label, expr, cases, default) ->
      let fmap acc ((expr, stat) as c) =
        let acc, fexpr = traA acc expr in
        let acc, fstat = traB acc stat in
        acc,
        if expr == fexpr && stat == fstat then c else
          (fexpr, fstat)
      in
      let acc, fexpr = traA acc expr in
      let acc, fcases = List.fold_left_map_stable fmap acc cases in
      let acc, fdefault = Option.foldmap_stable traB acc default in
      acc,
      if expr == fexpr && cases == fcases && default == fdefault then e else
        J.Js_switch (label, fexpr, fcases, fdefault)

  | J.Js_if (label, cond, then_, else_) ->
      let acc, fcond = traA acc cond in
      let acc, fthen_ = traB acc then_ in
      let acc, felse_ = Option.foldmap_stable traB acc else_ in
      acc,
      if cond == fcond && then_ == fthen_ && else_ == felse_ then e else
        J.Js_if (label, fcond, fthen_, felse_)

  | J.Js_throw (label, expr) ->
      let acc, fexpr = traA acc expr in
      acc,
      if expr == fexpr then e else
        J.Js_throw (label, fexpr)

  | J.Js_expr (label, expr) ->
      let acc, fexpr = traA acc expr in
      acc,
      if expr == fexpr then e else
        J.Js_expr (label, fexpr)

  | J.Js_trycatch (label, body, catches, finally) ->
      let fmap acc ((ident, expr, stat) as t) =
        let acc, fexpr = Option.foldmap_stable traA acc expr in
        let acc, fstat = traB acc stat in
        acc,
        if expr == fexpr && stat = fstat then t else
          (ident, fexpr, fstat)
      in
      let acc, fbody = traB acc body in
      let acc, fcatches = List.fold_left_map_stable fmap acc catches in
      let acc, ffinally = Option.foldmap_stable traB acc finally in
      acc,
      if body == fbody && catches == fcatches && finally == ffinally then e else
        J.Js_trycatch (label, fbody, fcatches, ffinally)

  | J.Js_for (label, init, cond, incr, body) ->
      let acc, finit = Option.foldmap_stable traA acc init in
      let acc, fcond = Option.foldmap_stable traA acc cond in
      let acc, fincr = Option.foldmap_stable traA acc incr in
      let acc, fbody = traB acc body in
      acc,
      if init == finit && cond == fcond && incr == fincr && body == fbody then e else
        J.Js_for (label, finit, fcond, fincr, fbody)

  | J.Js_forin (label, lhs, rhs, body) ->
      let acc, flhs = traA acc lhs in
      let acc, frhs = traA acc rhs in
      let acc, fbody = traB acc body in
      acc,
      if flhs == lhs && frhs == rhs && fbody == body then e else
        J.Js_forin (label, flhs, frhs, fbody)

  | J.Js_dowhile (label, body, cond) ->
      let acc, fbody = traB acc body in
      let acc, fcond = traA acc cond in
      acc,
      if body == fbody && cond == fcond then e else
        J.Js_dowhile (label, fbody, fcond)

  | J.Js_while (label, cond, body) ->
      let acc, fcond = traA acc cond in
      let acc, fbody = traB acc body in
      acc,
      if cond == fcond && body == fbody then e else
        J.Js_while (label, fcond, fbody)

  | J.Js_block (label, body) ->
      let acc, fbody = List.fold_left_map_stable traB acc body in
      acc,
      if body == fbody then e else
        J.Js_block (label, fbody)

  | J.Js_with (label, expr, body) ->
      let acc, fexpr = traA acc expr in
      let acc, fbody = traB acc body in
      acc,
      if expr == fexpr && body == fbody then e else
        J.Js_with (label, fexpr, fbody)

  | J.Js_label (label, string, stmt) ->
      let acc, fstmt = traB acc stmt in
      acc,
      if stmt == fstmt then e else
        J.Js_label (label, string, fstmt)

  | J.Js_comment (_, _, _) ->
      acc, e

module AB : TraverseInterface.AB
  with type 'a tA = JsAst.expr
  constraint 'a = 'b * 'c * 'd
  and type 'a tB = JsAst.statement
  constraint 'a = 'b * 'c * 'd
=
struct
  type 'a tA = JsAst.expr
  constraint 'a = 'b * 'c * 'd

  type 'a tB = JsAst.statement
  constraint 'a = 'b * 'c * 'd

  let foldmapA = foldmapA
  let foldmapB = foldmapB

  let mapA traA traB e = Traverse.Unoptimized.mapAB foldmapA traA traB e
  let mapB traB traA e = Traverse.Unoptimized.mapAB foldmapB traB traA e

  let iterA traA traB e = Traverse.Unoptimized.iterAB foldmapA traA traB e
  let iterB traB traA e = Traverse.Unoptimized.iterAB foldmapB traB traA e

  let foldA traA traB acc e = Traverse.Unoptimized.foldAB foldmapA traA traB acc e
  let foldB traB traA acc e = Traverse.Unoptimized.foldAB foldmapB traB traA acc e
end


module T = Traverse.MakeAB(AB)

module TExpr = T.A
module TStatement = T.B
module Expr = T.AinA
module Statement = T.BinB
module ExprInStatement = T.AinB
module StatementInExpr = T.BinA
module OnlyExpr = T.OnlyA
module OnlyStatement = T.OnlyB

(* Refreshing the annotations of an expression or a statement *)
module Refresh =
struct
  let aux_expr expr = J.JNewAnnot.expr expr (Annot.next ())
  let aux_stm stm = J.JNewAnnot.stm stm (Annot.next ())
  let expr expr = TExpr.map aux_expr aux_stm expr
  let stm stm = TStatement.map aux_stm aux_expr stm
end
