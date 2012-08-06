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
(* CF mli *)

(* depends *)
module List = Base.List
module String = BaseString

(* shorthands *)
module J = JsAst

(* -- *)

let dummy_pos = FilePos.nopos "Javascript Constructor"
let label () =
  Annot.next_label dummy_pos

let def_label = label

(*
  If we need to have unicity of annotation,
  we must replace rlabel by :
  {[
  let rlabel = Annot.refresh
  ]}
*)
external rlabel : Annot.label -> Annot.label = "%identity"


let object_prototype = StringSet.add_list [
  "constructor"; "eval"; "hasOwnProperty"; "isPrototypeOf"; "propertyIsEnumerable";
  "toSource"; "toLocalString"; "toString"; "unwatch"; "valueOf"; "watch";
] StringSet.empty

let can_object_field field =
  StringSet.mem field object_prototype ||
    String.is_prefix "__" field

let stmt_def ?(label=def_label()) ident =
  J.Js_var (label, ident, None)

module Expr =
struct

  let array ?(label=def_label()) content =
    J.Je_array (label, content)

  let assign ?(label=def_label()) e1 e2 =
    J.Je_binop (label, J.Jb_assign, e1, e2)

  let assign_ident ?(label=def_label()) ident e =
    assign ~label (J.Je_ident (label, ident)) e

  let binop ?(label=def_label()) binop e1 e2 =
    J.Je_binop (label, binop, e1, e2)

  let bool ?(label=def_label()) bool =
    J.Je_bool (label, bool)

  let true_ ?label () = bool ?label true
  let false_ ?label () = bool ?label false

  let call ?(label=def_label()) ~pure f args =
    J.Je_call (label, f, args, pure)

  let comma ?(label=def_label()) list last =
    if List.is_empty list then last else
      J.Je_comma (label, list, last)

  let cond  ?(label=def_label()) cond then_ else_ =
    J.Je_cond (label, cond, then_, else_)

  let dot ?(label=def_label()) expr field =
    (* Check if the field can be an inherit field from Object, in this
       case use hasOwnProperty to ensure the field is really owned by the
       object.
       Credit: Bug reported by Erling Ellingsen <reg.opa@alf.nu>
    *)
    if can_object_field field then
      (* (e.hasOwnProperty("field") && e.field) || undefined *)
      let check = J.Je_dot (label, expr, "hasOwnProperty") in
      let check = J.Je_call (label, check, [J.Je_string (label, field, true)], true) in
      let realldot = J.Je_dot (label, expr, field) in
      let checkanddot = J.Je_binop (label, J.Jb_land, check, realldot) in
      J.Je_binop (label, J.Jb_lor, checkanddot, J.Je_undefined label)
    else
      J.Je_dot (label, expr, field)

  let equality ?(label=def_label()) e = binop ~label J.Jb_eq e

  let exprident ?(label=def_label()) ident =
    J.Je_ident (label, J.ExprIdent ident)

  let hashref ?label expr1 expr2 =
    binop ?label J.Jb_hashref expr1 expr2

  let ident ?(label=def_label()) i =
    J.Je_ident (label, i)

  let native ?(label=def_label()) ident =
    J.Je_ident (label, J.Native (`local, ident))

  let native_global ?(pure=false) ?(label=def_label()) ident =
    J.Je_ident (label, J.Native (`global pure, ident))

  let field = dot

  let float ?(label=def_label()) float =
    let s =
      match classify_float float with
      | FP_normal
      | FP_subnormal
      | FP_zero -> string_of_float float
      | FP_infinite -> if float > 0. then "Infinity" else "-Infinity"
      | FP_nan -> "NaN" in
    J.Je_num (label, s)

  let function_ ?(label=def_label()) ident params body =
    J.Je_function (label, ident, params, body)

  let greater ?(label=def_label()) e = binop ~label J.Jb_gt e

  let hole ?(label=def_label()) expr =
    J.Je_hole (label, expr)

  let ident ?(label=def_label()) ident =
    J.Je_ident (label, ident)

  let in_ ?(label=def_label()) a b =
    binop ~label J.Jb_in a b

  let int ?(label=def_label()) int =
    J.Je_num (label, string_of_int int)

  let int_as_string ?(label=def_label()) int =
    J.Je_num (label, int)

  let land_ ?(label=def_label()) a b = binop ~label J.Jb_land a b

  let list ?(label=def_label()) list =
    J.Je_array (label, list)

  let lor_ ?(label=def_label()) a b = binop ~label J.Jb_lor a b

  let neq ?(label=def_label()) e = binop ~label J.Jb_neq e

  let not_ ?(label=def_label()) e =
    J.Je_unop (label, J.Ju_not, e)

  let null ?(label=def_label()) () =
    J.Je_null label

  let obj ?(label=def_label()) fields =
    J.Je_object (label, fields)

  let runtime ?(label=def_label()) expr =
    J.Je_runtime (label, expr)

  let strict_equality ?(label=def_label()) e = binop ~label J.Jb_seq e

  let strict_neq ?(label=def_label()) e = binop ~label J.Jb_sneq e

  let string ?(label=def_label()) string =
    J.Je_string (label, string, true)

  let this ?(label=def_label()) () =
    J.Je_this label

  let unop ?(label=def_label()) unop e1 =
    J.Je_unop (label, unop, e1)

  let undefined ?(label=def_label()) () =
    J.Je_undefined label

  let scope l e =
    let decls = List.map (fun v -> J.Js_var (def_label (), v, None)) l in
    let return = J.Js_return (def_label (), Some e) in
    let fun_ = function_ None [] (decls @ [return]) in
    call ~pure:false fun_ []

  let maybe_scope l e =
    if l = [] then e else scope l e

  (* deprecated *)
  let deprecated_lambda ?(label=def_label()) params locals expr =
    let locals = List.rev_map (fun ident -> stmt_def ~label:(rlabel label) ident) locals in
    let return = J.Js_return (rlabel label, Some expr) in
    let body = return :: locals in
    let body = List.rev body in
    function_ ~label None params body

  let deprecated_letin ?(label=def_label()) bindings expr =
    let map (id, expr) =
      let id = ident ~label:(rlabel label) id in
      J.Je_binop (rlabel label, J.Jb_assign, id, expr)
    in
    J.Je_comma (label, List.map map bindings, expr)
end

module Statement =
struct

  let def = stmt_def

  let assign ?(label=def_label()) e1 e2 =
    J.Js_expr (label, Expr.assign ~label e1 e2)

  let assign_ident ?(label=def_label()) ident e =
    J.Js_expr (label, Expr.assign_ident ~label ident e)

  let block ?(label=def_label()) stms =
    J.Js_block (label, stms)

  let comment ?(label=def_label()) string =
    J.Js_comment (label, J.Jc_simple (label, string))

  let continue ?(label=def_label()) ?label:label2 () =
    J.Js_continue (label, label2)

  let expr ?(label=def_label()) e =
    J.Js_expr (label, e)

  let function_ ?(label=def_label()) ident params body =
    J.Js_function (label, ident, params, body)

  let if_ ?(label=def_label()) expr stm1 stm2 =
    J.Js_if (label, expr, stm1, Some stm2)

  let if_no_else ?(label=def_label()) expr stm1 =
    J.Js_if (label, expr, stm1, None)

  let return ?(label=def_label()) expr =
    J.Js_return (label, Some expr)

  let switch ?(label=def_label()) ?default e l =
    J.Js_switch (label, e, l, default)

  let var ?(label=def_label()) ?expr ident =
    J.Js_var (label, ident, expr)

  let while_ ?(label=def_label()) expr stm =
    J.Js_while (label, expr, stm)

  (* deprecated *)
  let deprecated_function ?(label=def_label()) ident params locals expr =
    let locals = List.rev_map (fun ident -> def ~label:(rlabel label) ident) locals in
    let return = J.Js_return (rlabel label, Some expr) in
    let body = return :: locals in
    let body = List.rev body in
    function_ ~label ident params body
end

(* exported at the end, for not covering compilerlib.Ident *)
module Ident =
struct
  let ident id = J.ExprIdent id
  let fresh_qml ident =
    J.ExprIdent (Ident.refresh ~map:(fun s -> "js_internal_" ^ s) ident)
  let fresh ident =
    J.ExprIdent (Ident.next ("js_internal_" ^ (JsPrint.string_of_ident ident)))
  let native id = J.Native (`local, id)
  let native_global ?(pure=false) id = J.Native (`global pure, id)
end
