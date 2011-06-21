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

(* ======================================================================= *)
(*
  PASS 1: TAILREC MACHINE
*)
(* ======================================================================= *)

(*
   [map_tl f e]
   map the output of the expression [e] :
   it just rewrites the returned expression when it's a Call node.

   The function [f] rewrite a node [g(args)] by taking the function
   [g] and the parameters list [args] in argument. (plus the [pure] flag)
*)
let expr_map_tl map e =
  let rec aux e =
    match e with

    | J.Je_comma (label, comma, last) ->
        let last = aux last in
        JsCons.Expr.comma ~label comma last

    | J.Je_cond (label, cond, then_, else_) ->
        let then_ = aux then_ in
        let else_ = aux else_ in
        JsCons.Expr.cond ~label cond then_ else_

    | J.Je_call (label, func, el, pure) ->
        map label func el pure

    | e -> e
  in
  aux e

(*
  rewrite tail call apply with tco property
*)
let expr_rewrite_tl expr =
  expr_map_tl
    (fun label f el pure ->
       let recons f = JsCons.Expr.call ~label ~pure f el in
       let tco () = recons (JsCons.Expr.field ~label:(rlabel label) f "tco") in
       match f with
       | J.Je_ident (_, J.Native (_, n)) ->
           (*Special optimization: don't [tco-ize] calls to the BSL, unless we know we need it*)
           if String.is_prefix "clos_apply" n
           then tco ()
           else recons f
       | _ -> tco ()
    ) expr

(*
  map the expression returned
*)
let statement_map_return map = function
  | J.Js_return (label, Some e) -> J.Js_return (label, Some (map e))
  | s -> s

(*
  map all expression contained in all tail call position of a statement
*)
let statement_map_tl map stmt =
  JsWalk.Statement.map_down (statement_map_return map) stmt

(*
  process to the tco rewriting in all tail call positions of a statement
*)
let statement_rewrite_tl stmt = statement_map_tl expr_rewrite_tl stmt

(*
   Once all tail call have been rewriten using [tco], other call should
   be rewriten using [t].
*)
let search_and_rewrite_fun e =
  match e with
  | J.Je_call ( _, J.Je_dot (_, _, "tco"), _, _pure) -> e
  | J.Je_call (label, e, li, pure) ->
      JsCons.Expr.call ~label ~pure (JsCons.Expr.field ~label:(rlabel label) e "t") li
  | _ -> e

let mktl (code:JsAst.code) : JsAst.code =
  let code = List.tail_map
    (function
     | J.Js_var (label, ident, Some e) ->
         let e = JsWalk.Expr.map_down search_and_rewrite_fun e in
         JsCons.Statement.var ~label ident ~expr:e

     | J.Js_function (label, ident, params, body) ->
         let body = List.map statement_rewrite_tl body in
         let body = List.map (JsWalk.ExprInStatement.map_down search_and_rewrite_fun) body in
         JsCons.Statement.function_ ~label ident params body

     | J.Js_var (_,_,None)
     | J.Js_comment _ as s -> s

     | s ->
         OManager.printf "statement: %a@\n" JsPrint.pp#statement s ;
         assert false
    )
    code
  in code

(* ======================================================================= *)
(*
  PASS 2: LOCAL RENAMING
*)
(* ======================================================================= *)

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
    | J.Js_trycatch (_,_,catches,_) ->
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

(*

    match e with
    | J.Je_function (label, ident, params, body) ->
        assert ident = None;
        let acc, params = List.fold_left_map Rename.new_binding acc params in
        let _, body = List.fold_left_map self_statement acc body in
        let e = J.Je_function (label, ident, params, body) in
        e

    (*
      We check there than not renamed identifier are not modified
      This would be a bug detection, meaning than a function modify a toplevel variable
      in a letin e.g.
    *)

    | J.Je_unop (l1, op, (J.Je_ident (l2, ident))) ->
        if JsAst.is_side_effect_unop op
        then
          let ident = Rename.assert_resolve acc ident in
          let e = J.Je_unop (l1, op, (J.Je_ident (l2, ident)))
          acc, e
        else
          tra acc e

    | J.Je_binop (l1, op, (J.Je_ident (l2, ident)), e2) ->
        if JsAst.is_side_effect_binop op
        then
          let ident = Rename.assert_resolve acc ident in
          let acc, e2 = self_expr acc e2 in
          let e = J.Je_binop (l1, op, (J.Je_ident (l2, ident)), e2) in
          acc, e
        else
          tra acc e

    (* end of check *)

    | J.Je_ident (label, ident) ->
        let ident = Rename.resolve acc ident in
        let e = J.Je_ident (label, ident) in
        acc, e

    | e ->
        tra acc e


(* deep statement *)

    match s with
    | J.Js_var (label, ident, expr) ->
        let acc, ident = Rename.new_binding acc ident in
        let acc, expr = self_expr acc expr in
        let s = J.Js_var (label, ident, expr) in
        acc, s

    | J.Js_function (label, ident, params, body) ->
        let acc as init_acc,  = acc in
        let acc, params = List.fold_left_map Rename.new_binding acc params in
        let acc, body = List.fold_left_map self_statement acc body in

        let acc, ident = Rename.new_binding

    | s ->
        tra acc s

(* toplevel statement *)

    match s with

    | J.Js_var (label, ident, expr) ->
        let _, expr = rename_expr Rename.empty expr in
        JsCons.Statement.var ~label ident expr

    | J.Js_function (label, ident, params, body) ->
        let process (map, n) i =
          let ni, n1 = name_of_int n in
          let map = IdentMap.add i ni map in
          (map, n1)
          in
        let (newnames, _) as acc = List.fold_left process (IdentMap.empty, 0) params in
        let params =
          let find x = IdentMap.find x newnames in
          List.map find params
        in

        let _, body =
          List.fold_left_map (
            fun acc stmt ->
              JsWalk.ExprInStatement
                rename (newnames, number)
          ) acc body in

        JsCons.Statement.function_ ~label ident params body

    | ( J.Js_comment _ ) as s -> s

    | s ->
        OManager.printf "statement: %a@\n" JsPrint.pp#statement s ;
        assert false
    )

*)

(*
let rename (((_ : JsAst.ident IdentMap.t), (_ : int)) as acc) expr  =
  JsWalk.self_traverse_foldmap
    (fun self tra acc e ->
       match e with

       | J.LetIn (annot, bindings, expr) ->
           let acc, bindings =
             List.fold_left_map
               ( fun (map, number) (i, expr) ->
                   let ni, number = match IdentMap.find_opt i map with
                     | Some ni -> ni, number
                     | None -> assert false
                   in
                   let map = IdentMap.add i ni map in
                   let acc, expr = self (map, number) expr in
                   acc, (ni, expr)
               ) acc bindings in
           let acc, expr = self acc expr in
           acc, J.LetIn (annot, bindings, expr)

       | J.Func (annot, args, local, body) ->
           let on_list acc idents =
             List.fold_left_map
               (fun (map, number) i ->
                  let ni, number =
                    match IdentMap.find_opt i map with
                    | Some ni -> ni, number
                    | None -> name_of_int number in
                  let map = IdentMap.add i ni map in
                  (map, number), ni
               ) acc idents in
           let acc, args = on_list acc args in
           let acc, local = on_list acc local in
           let acc, body = self acc body in
           acc, (J.Func (annot, args, local, body))

       | J.Ident (annot, i) ->
           let map = fst acc in
           let i = Option.default i (IdentMap.find_opt i map) in
           acc, J.Ident (annot, i)

       | _ -> tra acc e

    ) acc expr

let local_alpha code =
  List.map
    (function

      | J.Js_var (label, ident, expr) ->
          let _, expr = rename (IdentMap.empty, 0) expr in
          JsCons.Statement.var ~label ident expr

      | J.Js_function (label, ident, params, body) ->
          let process (map, n) i =
            let ni, n1 = name_of_int n in
            let map = IdentMap.add i ni map in
            (map, n1)
          in
          let (newnames, _) as acc = List.fold_left process (IdentMap.empty, 0) params in
          let params =
            let find x = IdentMap.find x newnames in
            List.map find params
          in

          let _, body =
            List.fold_left_map (
              fun acc stmt ->
                JsWalk.ExprInStatement
                rename (newnames, number)
            ) acc body in

          JsCons.Statement.function_ ~label ident params body

      | ( J.Js_comment _ ) as s -> s

      | s ->
          OManager.printf "statement: %a@\n" JsPrint.pp#statement s ;
          assert false
    )
    code

*)

(* ======================================================================= *)
(*
  PASS 3: SPLIT DEEP EXPRESSIONS
*)
(* ======================================================================= *)

(*
  Adding identifiers in a list and a set simultanously.
*)
let add_idents acc idents =
  List.fold_left (
    fun ((li, idents) as acc) i ->
      if IdentSet.mem i idents
      then acc
      else (i :: li, (IdentSet.add i idents))
  ) acc idents

(*
  Compute the depth of an Js expression
*)
let rec depth e =
  1 + JsWalk.OnlyExpr.fold_nonrec
    (fun acc e ->
       max acc (depth e)
    )
    0 e

(*
  doc ?
*)
let maxp = 12

(*

(**
   @param [ident] used only for giving a name related to the original name of the [code_elt]
   containing the expression being rewriten.
*)
let rewrite
    (ident:JsAst.ident)
    (local:IdentSet.t)
    (e:JsAst.expr)
    (p:int)
    before
    (local_set_toremove:IdentSet.t)
    =
  let rec rewrite
      (local:IdentSet.t)
      (e:JsAst.expr)
      (p:int)
      before
      (local_set_toremove:IdentSet.t)
      =
  if (depth e) < maxp
  then
    (before, local_set_toremove), e
  else
    let local =
      match e with
      | J.Func (_annot, idents, idents2, _) ->
          IdentSet.union local
            (IdentSet.from_list (List.rev_append idents idents2))
      | _ -> local
    in
    if p = 0 then
      let local_newexpr, local_set = JsWalk.fold_down
        (fun acc e -> match e with
         | J.LetIn (_annot, bindings, _) ->
             add_idents acc (List.map fst bindings)
         | J.Func (_annot, idents, _, _) ->
             add_idents acc idents
         | _ -> acc
        ) ([], IdentSet.empty) e
      in
      let tobind = JsWalk.fold_down
        (fun idents e -> match e with
         | J.Ident (_, i) ->
             if IdentSet.mem i local
             then IdentSet.add i idents
             else idents
         | _ -> idents
        ) IdentSet.empty e
      in
      let tobind = IdentSet.diff tobind local_set |> IdentSet.elements in
      let (before, local_set_toremove), ne =
        rewrite local e maxp before (IdentSet.union local_set_toremove local_set) in
      let nident = JsCons.Ident.fresh ident in
      let ndefs = J.Defun(Cons.annot (), nident, tobind, local_newexpr, ne) :: before in
      let nexpr = Cons.call ~pure:false (Cons.ident nident) (List.map Cons.ident tobind)
      in (ndefs, local_set_toremove), nexpr

    else
      JsWalk.foldmap_nonrec (
        fun (before, local_set_toremove) e ->
          rewrite local e (p - 1) before local_set_toremove
      ) (before, local_set_toremove) e

  in
  rewrite
    (local:IdentSet.t)
    (e:JsAst.expr)
    (p:int)
    before
    (local_set_toremove:IdentSet.t)

*)

(*
  doc ?
*)
let split code = code

(*

  let rewrite ident construct local e =
    let (before, local_set_toremove), e =
      rewrite ident (IdentSet.from_list local) e maxp [] IdentSet.empty
    in
    List.rev ((construct e local_set_toremove) :: before)
  in
  List.concat_map (
    function
    | J.Def (annot, ident, expr) ->
        let recons e _ = J.Def (annot, ident, e) in
        rewrite ident recons [] expr

    | J.Defun(annot, ident, args, local, body) ->
        let recons e local_toremove =
          let locals = IdentSet.elements (IdentSet.diff (IdentSet.from_list local) local_toremove) in
          J.Defun (annot, ident, args, locals, e)
        in
        rewrite ident recons (List.append args local) body

    | x -> [x]

  ) code

*)




(*


... acc ...

let rec rename_expr acc e =
  let rec aux_expr e =
    ExprOnly.map_down
      où un traverse_map_down ou on fait gaffe aux je_function
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
