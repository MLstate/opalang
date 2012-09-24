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

module G = Graph.Persistent.Digraph.Concrete(JsIdent)

let rec deps_in_expr t (vars, graph) expr = JsWalk.TExpr.traverse_fold
  (fun trae _tras (vars, graph) -> function
   | J.Je_ident (_, i) when not(JsIdentSet.mem i vars) ->
       (* Format.eprintf "Add : %a => %a\n" JsPrint.pp#ident t JsPrint.pp#ident i; *)
       vars, (G.add_edge graph t i)
   | e -> trae (vars, graph) e
  )(fun tras trae (vars, graph) s ->
      deps_in_stms t tras trae (vars, graph) [s]
   ) (vars, graph) expr

and deps_in_stms t tras trae (vars, graph) stms =
  let _, graph = List.fold_left
    (fun (vars, graph) s -> match s with
     | J.Js_var (_, i, None) -> JsIdentSet.add i vars, graph
     | J.Js_var (_, i, Some e) ->
         let _, graph = trae (vars, graph) e in
         JsIdentSet.add i vars, graph
     | J.Js_function (_, i, il, stms) ->
         let _, graph =
           let vars = JsIdentSet.add_list il vars in
           let vars = JsIdentSet.add i vars in
           deps_in_stms t tras trae (vars, graph) stms
         in vars, graph
     | s -> tras (vars, graph) s
    ) (vars, graph) stms
  in vars, graph

let deps_of_roots roots graph =
  JsIdentSet.fold
    (fun r deps ->
       let rec aux deps r =
         List.fold_left
           (fun deps s ->
              if JsIdentSet.mem s deps then deps
              else
                let deps = JsIdentSet.add s deps in
                aux deps s
           ) deps (try G.succ graph r with Invalid_argument _ -> [])
       in aux deps r
    ) roots roots

let process_code ~keep code =
  let graph = G.empty in
  let roots = JsIdentSet.empty in
  let roots, graph = List.fold_left
    (fun (roots, graph) stm -> match stm with
     | J.Js_var (_, _, None) -> assert false
     | J.Js_var (_, i, Some e) ->
         let _, graph = deps_in_expr i (JsIdentSet.empty, graph) e in
         let roots =
           if Imp_Common.does_side_effects e || keep i then
             JsIdentSet.add i roots
           else roots
         in
         roots, graph
     | J.Js_function (l, i, il, s) ->
         let vars = JsIdentSet.add_list il JsIdentSet.empty in
         let vars = JsIdentSet.add i vars in
         let _, graph = deps_in_expr i (vars, graph) (J.Je_function (l, Some i, il, s)) in
         let roots = if keep i then JsIdentSet.add i roots else roots in
         roots, graph
     | J.Js_expr (_, J.Je_runtime _) -> roots, graph
     | J.Js_expr (_, _e) ->
         (* Format.eprintf "%a\n%!" (JsPrint.pp#expr ~leading:false) e; *)
         roots, graph
     | _ -> assert false
    ) (roots, graph) code
  in
  let deps = deps_of_roots roots graph in
  List.filter
    (function
     | J.Js_var (_, i, _)
     | J.Js_function (_, i, _, _)
     | J.Js_expr (_, J.Je_binop(_, J.Jb_assign, J.Je_dot(_, J.Je_ident (_, i), _), _))
         when not(JsIdentSet.mem i deps) ->
         Format.eprintf "clean %a\n%!" JsPrint.pp#ident i;
         false
     | _ -> true
    ) code

