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



(* refactoring in progress *)
(* depends *)
module List = BaseList
module String = BaseString

(* alias *)
module TypeIdent = QmlAst.TypeIdent

(* shorthands *)
module Q = QmlAst

(* -- *)


let rec traverse_coerce e = match e with
| Q.Coerce (_, e, _) -> traverse_coerce e
| _ -> e

let map_exprident code f =
  let f' x = match x with
    | Q.Ident (label, y) ->
        let fy = f y in
        if y == fy then x else Q.Ident (label, fy)
    | _ -> x
  in QmlAstWalk.CodeExpr.map (QmlAstWalk.Expr.map_up f') code

let rec get_deeper_expr ?(except=fun _ -> false) e =
  if except e then e
  else
    match e with
      (* special forms to document !!! *)
    | Q.LetIn      (_, [id, e1], Q.Ident (_, id'))
    | Q.LetRecIn   (_, [id, e1], Q.Ident (_, id')) when Ident.equal id id' ->
        get_deeper_expr ~except e1

    (* forms with a unique inner expr *)
    | Q.LetIn (_, _, e) | Q.LetRecIn (_, _, e)
    | Q.Lambda (_, _, e) | Q.Coerce (_, e, _)
    | Q.Match (_, _, [_, e])

      -> get_deeper_expr ~except  e

    | Q.Dot _   | Q.Path _
    | Q.Bypass _
    | Q.Ident _ | Q.Const _
    | Q.Record _ | Q.ExtendRecord _
    | Q.Apply  _ | Q.Directive _
    | Q.Match _ -> e


let substitute old_expr new_expr e =
  let old_annot = Q.QAnnot.expr old_expr in
  let aux tra e =
    if Annot.equal (Q.QAnnot.expr e) old_annot
    then new_expr
    else tra e
  in
  QmlAstWalk.Expr.traverse_map aux e

let collect_annot old_annot e =
  let coll tra acc e =
    if Annot.equal (Q.QAnnot.expr e) old_annot
    then e::acc
    else tra acc e
  in
  QmlAstWalk.Expr.traverse_fold coll [] e

let collect old_expr e =
  let old_annot = Q.QAnnot.expr old_expr in
  collect_annot old_annot e

type stop_expansiveness =
    [ `nonexpansive
    | `unsafe_cast
    | `fail
    | `todo
    ]
type ('a,'b,'c) strictly_non_expansive =
    [ `expand of 'a
    | `doctype of 'b
    | `sliced_expr
    | `warncoerce
    | `extendwith
    | `specialize of 'c
    | `may_cps
    | `worker
    | Q.opavalue_directive
    | `async
    | `deprecated
    ]
type non_expansive = [
  | `module_
  | `recval
  | Q.slicer_directive
  | Q.closure_instrumentation_directive
]

let is_expansive =
  QmlAstWalk.Expr.traverse_exists
    (fun tra -> function
    | Q.Const _
    | Q.Ident _
    | Q.Lambda _
    | Q.Bypass _ -> false

    | Q.Directive (_, `llarray, [], _) ->
      false (* the empty array is the only one that is not expansive
             * because it is not mutable *)

    | Q.Directive (_, #stop_expansiveness, _, _) ->
      false

    | Q.Directive (_, (#strictly_non_expansive | #non_expansive), _exprs, _) as d
      -> tra d

    | Q.Directive _ -> true
    | Q.Apply _ -> true
    | e -> tra e)

let is_expansive_strict =
  QmlAstWalk.Expr.traverse_exists
    (fun tra -> function
    | Q.Const _
    | Q.Ident _
    | Q.Lambda _
    | Q.Bypass _ -> false
    | Q.Apply _
    | Q.Record _ -> true

    | Q.Directive (_, #strictly_non_expansive, _exprs, _) as d
      -> tra d

    | Q.Directive _ -> true
    | e -> tra e
    )

(* only elements taking part in the expression type counts *)
let expansive_nodes_related_to_type ?(strict=false) =
  QmlAstWalk.Expr.traverse_fold
    (fun tra acc -> function
    | Q.Const _
    | Q.Ident _
    | Q.Lambda _
    | Q.Bypass _ -> acc

    | Q.Directive (_, `llarray, [], _) when not(strict) ->
      acc   (* the empty array is the only one that is not expansive
             * because it is not mutable *)

    | Q.Directive (_, #stop_expansiveness, _, _) when not(strict) ->
      acc

    | Q.Directive (_, #non_expansive, _exprs, _) as d when not(strict) ->
      tra acc d

    | Q.Directive (_, #strictly_non_expansive, _exprs, _) as d
      -> tra acc d

    | (Q.Directive(a, _, _, _)
    | Q.Apply(a, _, _)) as e ->
      tra (a::acc) e

    | e -> tra acc e) []
let is_expansive_with_options = function
  | `disabled -> (fun _ -> false)
  | `normal -> is_expansive
  | `strict -> is_expansive_strict
let expansive_nodes_related_to_type_with_options = function
  | `disabled -> (fun _ -> [])
  | `normal -> expansive_nodes_related_to_type ~strict:false
  | `strict -> expansive_nodes_related_to_type ~strict:true

module App =
struct
  type 'a util = Q.expr -> Q.expr list -> 'a

  let to_list ?(strict=true) e =
    match e with
    | Q.Apply (_, f, args) -> f::args
    | _ ->
        if strict then invalid_arg "QmlAstUtils.App.to_list"
        else [e]

  let from_list l =
    match l with
    | f::args -> QmlAstCons.UntypedExpr.apply f args
    | _ -> invalid_arg "QmlAstUtils.App.from_list"

  let nary_args_number _f args = List.length args

  let curryfied_args_number f _x =
    let rec aux cpt e =
      match e with
        (*         | Directive (#structural_ignored_directive, ...) *)
      | Q.Apply (_, f, args) -> aux (cpt + List.length args) f
      | _ -> cpt
    in
    aux 1 f
end

module ExprIdent =
struct
  let string = function
    | Q.Ident (_, n) -> Ident.to_uniq_string n
    | _ -> assert false

  let change_ident id expr =
    match expr with
    | Q.Ident (label, _) -> Q.Ident (label, id)
    | _ -> invalid_arg "QmlAstUtils.Ident.change_ident"

  let substitute ident_map expr =
    let aux expr =
      match expr with
      | Q.Ident (_, i) -> (
          match IdentMap.find_opt i ident_map with
          | Some e -> e ()
          | None -> expr
        )
      | _ -> expr
    in
    QmlAstWalk.Expr.map_up aux expr
end

module Lambda =
struct
  type 'a util = Ident.t list -> Q.expr -> 'a

  let nary_arity params _body = List.length params

  let curryfied_arity params body =
    let rec aux cpt e =
      match e with
        (*       | Directive (#structural_ignored_directive, ...) -> aux cpt expr *)
      | Q.Coerce (_, e, _) -> aux cpt e
      | Q.Lambda (_, params, body) -> aux (cpt + List.length params) body
      | _ -> cpt
    in aux (List.length params) body

  (* deprecated *)
  let count e =
    match e with
    | Q.Lambda (_, params, body)  -> curryfied_arity params body
    | _ -> 0



  (* ************************************************************************ *)
  (** {b Visibility}: Exported outside this module.                           *)
  (* ************************************************************************ *)
  let eta_expand_ast arity e =
    (* Use as position for of generated pieces of code, the position of the
       currently processed expression. *)
    let pos = Q.Pos.expr e in
    let idents =
      List.init
        arity (fun i -> Ident.next (Printf.sprintf "eta_%d_%d" i arity)) in
    let exps =
      List.map
        (fun i ->
          let label = Annot.next_label pos in
          QmlAstCons.UntypedExprWithLabel.ident ~label i)
        idents in
    let label_lambda = Annot.next_label pos in
    let label_apply = Annot.next_label pos in
    QmlAstCons.UntypedExprWithLabel.lambda
      ~label: label_lambda idents
      (QmlAstCons.UntypedExprWithLabel.apply ~label: label_apply e exps)
end

module Coerce =
struct

  let uncoerce e =
    let rec aux e acc =
      match e with
      | Q.Coerce (_, e, ty)-> aux e ((Q.Label.expr e, ty)::acc)
      | _ -> e, acc
    in aux e []

  let recoerce e lanty =
    List.foldl (fun (label, ty) e -> QmlAstCons.UntypedExprWithLabel.coerce ~label e ty) lanty e

  let rm_coerces e = fst (uncoerce e)
end

module FreeVars =
struct

  let pat_fold f pat acc0 =
    let aux acc pat = match pat with
      | Q.PatVar (label, i) | Q.PatAs (label, _, i) ->
          f acc (Annot.annot label) i
      | _ -> acc
    in
    QmlAstWalk.Pattern.fold_down aux acc0 pat

  let pat pat = pat_fold (fun acc _ i -> IdentSet.add i acc) pat IdentSet.empty

  let expr_fold f expr acc0 =
    QmlAstWalk.Expr.fold_with_exprmap
      (fun bound acc e -> match e with
         | Q.Ident (label, i) when IdentMap.find_opt i bound = None ->
             f acc (Annot.annot label) i
         | _ -> acc)
      acc0 expr

  let expr pat = expr_fold (fun acc _ i -> IdentSet.add i acc) pat IdentSet.empty

end

module Const =
struct

  let limits byte =
    Big_int.minus_big_int (Big_int.power_int_positive_int 2 byte),
    Big_int.pred_big_int (Big_int.power_int_positive_int 2 byte)

  let int_js_limits (* -2^53, 2^53 - 1*) =
    limits 53

  let int_ml_limits (* -2^62, 2^62 - 1*) =
    #<Ifstatic:OCAML_WORD_SIZE 64>
    limits 62
    #<Else>
    limits 30
    #<End>

  let int_limits = ref int_js_limits

  let compare a b =
    match a, b with
    | Q.Int a, Q.Int b -> Big_int.compare_big_int a b
    | Q.Float a, Q.Float b -> Pervasives.compare a b
    | Q.String a, Q.String b -> String.compare a b
    | _ -> Pervasives.compare a b

  let equal a b = compare a b = 0

  let check_int i =
    let min, max = !int_limits in
    Big_int.le_big_int i max && Big_int.ge_big_int i min

  let min_int () =
    fst !int_limits

  let max_int () =
    snd !int_limits

  let set_limits = function
    | `js -> int_limits := int_js_limits
    | `ml -> int_limits := int_ml_limits
end

module Record =
struct
  type 'a util = (string * Q.expr) list -> 'a

  let uncons_tuple fields =
    let mapi i (f, e) =
      let field = QmlAstCons.Tuple.field (succ i) in
      if String.compare f field <> 0
      then raise Not_found
      else e
    in
    try Some (List.mapi mapi fields)
    with
    | Not_found -> None

  let uncons_qml_tuple fields =
    let (@=) s s' = String.compare s s' = 0 in
    let s_fst = QmlAstCons.Tuple.qml_fst in
    let s_snd = QmlAstCons.Tuple.qml_snd in
    let rec aux ?(fail=true) acc fields =
      match fields with
      | [ ( ss_fst, fst ) ; ( ss_snd, Q.Record (_, fields)) ]
          when s_fst @= ss_fst && s_snd @= ss_snd
            -> aux ~fail:false (fst::acc) fields
      | [ ( ss_fst, fst ) ; ( ss_snd, snd ) ]
          when s_fst @= ss_fst && s_snd @= ss_snd
            -> List.rev (snd::fst::acc)
      | _ ->
          if fail then raise Not_found
          else
            List.rev ((QmlAstCons.UntypedExpr.record fields)::acc)
    in
    try
      Some (aux [] fields)
    with Not_found -> None

  let uncons fields_exprs_list = List.split fields_exprs_list

  let cons fields exprs =
    QmlAstCons.UntypedExpr.record (List.combine fields exprs)
end

module Tuple =
struct
  let uncons e =
    match (traverse_coerce e) with
    | Q.Record (_, fields) -> Record.uncons_tuple fields
    | _ -> None

  let uncons_typeident typeident =
    match String.split_char '_' (QmlAst.TypeIdent.to_string typeident) with
    | "tuple", r -> Base.int_of_string_opt r
    | _ -> None

  let uncons_qml_tuple e =
    match (traverse_coerce e) with
    | Q.Record (_, fields) -> Record.uncons_qml_tuple fields
    | _ -> None
end

module Pat = QmlAstWatch.Pat

module Match =
struct
  type 'a util = Q.expr -> (Q.pat * Q.expr) list -> 'a

  let uncons_ifthenelse = QmlAstWatch.uncons_ifthenelse

  let uncons if_ pats_exprs =
    let pats, expr = List.split pats_exprs in
    (if_, pats, expr)

  let cons if_ pats exprs =
    let p = List.combine pats exprs in
    QmlAstCons.UntypedExpr.match_ if_ p

end

module LetIn =
struct
  type 'a util = (Q.ident * Q.expr) list -> Q.expr -> 'a

  let rev_uncons (l : (Q.ident * Q.expr) list) e =
    let rec aux acc e =
      match e with
      | Q.LetIn (_, l, e) -> aux (l::acc) e
      | _ -> acc,e
    in aux [l] e

  let uncons (l : (Q.ident * Q.expr) list) e =
    let rev_u,e = rev_uncons l e in
    List.rev rev_u, e

  let cons l e =
    List.fold_right
      (fun l e -> QmlAstCons.UntypedExpr.letin l e) l e
end

module LetRecIn =
struct
  type 'a util = (Q.ident * Q.expr) list -> Q.expr -> 'a

  let rev_uncons (l : (Q.ident * Q.expr) list) e =
    let rec aux acc e =
      match e with
      | Q.LetRecIn (_, l, e) -> aux (l::acc) e
      | _ -> acc,e
    in aux [l] e

  let uncons (l : (Q.ident * Q.expr) list) e =
    let rev_u,e = rev_uncons l e in
    List.rev rev_u, e

  let cons l e =
    List.fold_right
      (fun l e -> QmlAstCons.UntypedExpr.letrecin l e) l e
end

module Code =
struct
  let insert ~deps ~insert code =
   let last = function
     | Q.NewVal (_, bindings)
     | Q.NewValRec (_, bindings) ->
         List.exists (fun (i, _) -> IdentSet.mem i deps) bindings
     | _ -> false
    in
    let rec aux acc = function
      | [] ->
          insert @ acc
      | code_elt :: tl ->
          if last code_elt
          then
            List.rev_append tl (code_elt ::(insert @ acc))
          else
            aux (code_elt::acc) tl
    in
    aux [] (List.rev code)
end
