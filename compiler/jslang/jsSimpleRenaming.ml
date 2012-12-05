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

module J = JsAst
module Cons = JsCons

let (|>) = InfixOperator.(|>)

module IdentMap = JsAst.IdentMap;;
module IdentSet = JsAst.IdentSet;;

module List = Base.List
module String = Base.String

type jsp = JsAst.code -> JsAst.code

(*
  If we need to have unicity of annotation,
  we must replace rlabel by :
  {[
  let rlabel = Annot.refresh
  ]}
*)
external rlabel : Annot.label -> Annot.label = "%identity"

module Rename :
sig
  type env
  val empty : env
  val add : JsAst.ident -> env -> env
  val new_binding : env -> JsAst.ident -> env * JsAst.ident
  val resolve : env -> JsAst.ident -> JsAst.ident
  val assert_resolve : env -> JsAst.ident -> JsAst.ident
end =
struct

  (*
    Generate a short JS identifier from an int.
    In case the identifier returned is a js keyword,
    skip it, and inspect the next generated one.

    The function return the next int to use for generating
    the next short ident.
  *)
  let rec name_of_int i =
    let name = IdentGenerator.alphanum i in
    if JsAst.is_keyword name then name_of_int (i+1) else JsCons.Ident.native name, (i+1)

  type env = JsAst.ident IdentMap.t * int
  let empty = (IdentMap.empty, 0)

  let new_binding (map, number) ident =
    let new_ident, number = name_of_int number in
    let map = IdentMap.add ident new_ident map in
    (map, number), new_ident

  let add ident (map, number) =
    if IdentMap.mem ident map then (map, number)
    else
      let new_ident, number = name_of_int number in
      let map = IdentMap.add ident new_ident map in
      (map, number)

  let resolve (map, _) ident =
    match IdentMap.find_opt ident map with
    | Some ident -> ident
    | None -> ident

  let assert_resolve (map, _) ident =
    match IdentMap.find_opt ident map with
    | Some ident -> ident
    | None ->
        assert false
end

(*
  Collect vars and function local to a statement, without entering
  internal function inside other functions.
*)
let stmt_collect_locals acc s =
  JsWalk.OnlyStatement.traverse_fold (
    fun tra acc -> function
    | J.Js_var (_, ident, _) -> Rename.add ident acc
    | J.Js_function (_, ident, _, _) -> Rename.add ident acc (* NOT traversing *)
    | J.Js_trycatch (_,_,catches,_) as s ->
        let acc = List.fold_left (fun acc (ident,_,_) -> Rename.add ident acc) acc catches in
        tra acc s
    | s ->
        tra acc s
  ) acc s

(*
  Cf the notice for the 3 following recursive functions.

  {[
  let rec rename_expr
  and rename_function
  and rename_statement
  ]}
*)

let rec rename_expr (acc : Rename.env) e =
  JsWalk.OnlyExpr.traverse_map (
    fun tra e ->
      match e with
      | J.Je_function (label, ident, params, body) ->
          let recons (ident, params, body) = J.Je_function (label, ident, params, body) in
          recons (rename_function acc ident params body)

      | J.Je_ident (label, ident) ->
          let ident = Rename.resolve acc ident in
          let e = J.Je_ident (label, ident) in
          e

      | e ->
          tra e
  ) e

and rename_function acc ident params body =
  let ident = Option.map (Rename.resolve acc) ident in
  let acc, params = List.fold_left_map Rename.new_binding acc params in
  let acc = List.fold_left stmt_collect_locals acc body in
  let body = List.tail_map (rename_statement acc) body in
  (ident, params, body)

and rename_statement acc stmt =
  JsWalk.TStatement.traverse_map (
    fun traS _traE s ->
      match s with
      | J.Js_var (label, ident, expr) ->
          let ident = Rename.resolve acc ident in
          let expr = Option.map (rename_expr acc) expr in
          J.Js_var (label, ident, expr)

      | J.Js_function (label, ident, params, body) ->
          let recons (ident, params, body) =
            let ident = Option.get ident in
            J.Js_function (label, ident, params, body) in
          recons (rename_function acc (Some ident) params body)

      | J.Js_trycatch (label, body, catches, finally) ->
          let catches = List.map (fun (ident, e, s) -> (Rename.resolve acc ident, e, s)) catches in
          let s = J.Js_trycatch (label, body, catches, finally) in
          traS s

      (*
        the node with is not supported by the local renaming
      *)
      | J.Js_with _ -> assert false

      | s -> traS s
  )
    (fun _traE _traS e -> rename_expr acc e)
    stmt


(*
  Renaming function parameters, and local variables.
  This renaming does not affect toplevel identifiers
*)
let local_alpha_stm stm =
  let acc = Rename.empty in
  rename_statement acc stm
let local_alpha code =
  let acc = Rename.empty in
  List.tail_map (rename_statement acc) code

(*
  NOTICE:

let rec rename_expr (acc : Rename.env) e =
  let rec aux e =
    ExprOnly.map_down
      ou un traverse_map_down où on fait gaffe aux je_function
    map_down utilisant acc
      sauf dans le cas Je_function,
    où on appelle une regle de renommage des fonctions
    qui appelle rename_statement avec (acc + quelque chose)
  in
  aux_expr e

and rename_function recons acc ident params body =
  1) on rename ident avec ce acc,

  2) collect les var et les function dans body sans rentrer dans les function
   fold sur statement only, pas de tra sur Js_function
   StatementOnly.traverse_fold

  3) ca en fait un acc2,
     on met params dans acc2
     on renomme le body avec acc2 (rename_statement)
     et on recons

and rename_statement acc s =

  - si tombe sur Js_function, simplement appliquer rename_function
  - si var : simplement appliquer le renommage

  sinon : rename_expr avec le meme acc
  et tra acc sur les statement
  TStatement.traverse_map
    avec rename_expr sur les expr
    et tra sur les statement

TStatement.traverse_map
  (fun traS traE e -> rename_expr acc e)
  (fun traS traE s ->
     match s with
     | Js_function -> rename_function
     | JsVar -> lookup acc pour renommer
     | s -> traS s)
  s
*)
