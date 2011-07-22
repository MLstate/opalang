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
module String = Base.String
module List = Base.List


exception NotImplemented

(*--------------------------------*)
(*----  control flow -------------*)
(*--------------------------------*)
type expr_or_stm =
  | Expr of J.expr
  | Stm of J.statement

type node = {
  name : string; (* this name is for debug *)
  id : int; (* this id is used to define comparison/hashing on nodes *)
  label : Annot.t option; (* the label is used to identify the expr of statement
                           * that generated the current node
                           * it is only meant for cleaning useless assigments *)
  def : JsIdentSet.t; (* FIXME: should be small sets *)
  use : JsIdentSet.t; (* FIXME: should be small sets *)
  mutable live_in : JsIdentSet.t; (* FIXME: should be small sets *)
  mutable live_out : JsIdentSet.t; (* FIXME: should be small sets *)
  content : expr_or_stm; (* unused, could be removed *)
  alias : bool; (* when true, then the current node is an alias
                 * and so it is treated specially when building the
                 * interference graph *)
}

let next_id =
  let id_ref = ref 0 in
  fun () -> incr id_ref; !id_ref
let node_of_gen name ?(alias=false) ?(use=[]) ?def gen =
  let alias = #<If:JS_RENAMING$contains "alias">false#<Else>alias#<End> in
  let def, label =
    match def with
    | None -> [], None
    | Some (d, None) -> d, None
    | Some (d, Some loc) -> d, Some (Annot.annot loc) in
  assert (not alias || List.length use = 1 && List.length def = 1);
  {
    name = name;
    id = next_id ();
    label = label;
    def = JsIdentSet.from_list def;
    use = JsIdentSet.from_list use;
    live_in = JsIdentSet.empty;
    live_out = JsIdentSet.empty;
    content = gen;
    alias = alias;
  }
let node_of_stm name ?alias ?use ?def stm = node_of_gen name ?alias ?use ?def (Stm stm)
let node_of_expr name ?alias ?use ?def expr = node_of_gen name ?alias ?use ?def (Expr expr)

(* this environment is used for building the control flow graph *)
type env = {
  labels : node StringMap.t;
  current_break : node option; (* the statement where we go when we say break *)
  current_continue: node option; (* the statement where we go when we say continue
                                   * possibly not the same as the one before because
                                   * a switch 'catches' break but not continue
                                   *)
  current_return: node option;
}

module Node =
struct
  type t = node
  let compare n1 n2 = compare n1.id n2.id
  let hash n = Hashtbl.hash n.id
  let equal n1 n2 = n1.id = n2.id
end

module G = Graph.Imperative.Digraph.Concrete(Node)
module SCC = Graph.Components.Make (G)

(* DEBUG *)
let vertex_name n =
  n.name ^
    "_DEF_"^
    String.concat_map "_" JsIdent.stident (JsIdentSet.elements n.def) ^
    "_USE_"^
    String.concat_map "_" JsIdent.stident (JsIdentSet.elements n.use) ^
    "_ID_" ^
    string_of_int n.id
let vertex_name2 n =
  vertex_name n ^
    "_IN_" ^
    String.concat_map "_" JsIdent.stident (JsIdentSet.elements n.live_in) ^
    "_OUT_" ^
    String.concat_map "_" JsIdent.stident (JsIdentSet.elements n.live_out)
module Viz = GraphUtils.DefaultGraphviz(G)(struct let vertex_name = vertex_name end)
module Viz2 = GraphUtils.DefaultGraphviz(G)(struct let vertex_name = vertex_name2 end)
(* END DEBUG *)

module GIdent = Graph.Imperative.Graph.Concrete(JsIdent)
module Coloring = Graph.Coloring.Make(GIdent)
(* DEBUG *)
module Viz3 = GraphUtils.DefaultGraphviz(GIdent)(struct let vertex_name = JsIdent.stident end)
(* END DEBUG *)

(*
 * This function build a graph where there is a node for each assigment
 * to an identifier and each use of an identifier (local or global)
 * (plus some more nodes that are used only for building the graph
 * especially for statements)
 * There is an edge from a node [a] to a node [b] when [b] can be executed after [a]
 * For instance when you have the program [x = y], you need to read [y] and then
 * you write [x]
 * The control flow graph would be [(y-use) -> (x-def)]
 *)
let build_control_flow_graph ?name params body =
  let g = G.create () in
  let node_of_stm name ?alias ?use ?def stm =
    let node = node_of_stm name ?alias ?use ?def stm in
    G.add_vertex g node;
    node in
  let node_of_expr name ?alias ?use ?def expr =
    let node = node_of_expr name ?alias ?use ?def expr in
    G.add_vertex g node;
    node in
  let link n1 n2 =
    G.add_edge g n1 n2 in
  let env = {
    labels = StringMap.empty;
    current_break = None;
    current_continue = None;
    current_return = None;
  } in
  let local_vars = ref JsIdentSet.empty in

  (* [aux] returns the entry node and output node of the control flow graph
   * of the [orig_stm] *)
  let rec aux_stm env orig_stm =
    match orig_stm with
    | J.Js_while (_, expr, stm) ->
        let while1 = node_of_stm "while1" orig_stm in
        let while2 = node_of_stm "while2" orig_stm in
        let from_e = aux_expr while1 expr in
        let to_s, from_s =
          aux_stm
            {env with
               current_break = Some while2;
               current_continue = Some while1;
            } stm in
        link from_e to_s;
        link from_s while1;
        link from_e while2;
        while1, while2
    | J.Js_for (_, e1, e2, e3, s) ->
        let for1 = node_of_stm "for1" orig_stm in
        let for2 = node_of_stm "for2" orig_stm in
        let for3 = node_of_stm "for3" orig_stm in
        let for4 = node_of_stm "for4" orig_stm in
        let from_e1 = aux_expr_option for1 e1 in
        let to_s,from_s =
          aux_stm
            {env with
               current_break = Some for4;
               current_continue = Some for3;
            } s in
        link from_s for3;
        link from_e1 for2;
        let from_e2 = aux_expr_option for2 e2 in
        link from_e2 to_s;
        let from_e3 = aux_expr_option for3 e3 in
        link from_e3 for2;
        link from_e2 for4;
        for1, for4
    | J.Js_forin _ ->
        raise NotImplemented
    | J.Js_var (_,_,None) ->
        let dummy = node_of_stm "var_no_assign" orig_stm in
        dummy, dummy
    | J.Js_var (label,i,Some e) ->
        aux_stm env (J.Js_expr (label, J.Je_binop (label, J.Jb_assign, J.Je_ident (label,i), e)))
    | J.Js_with _ ->
        assert false
    | J.Js_block (_,sl) ->
        aux_stms env sl
    | J.Js_function _ ->
        raise NotImplemented (* dealing with local function seems to be pretty hard without a global analysis *)
    | J.Js_return (_, Some e) ->
        let return = node_of_stm "return" orig_stm in
        let to_ = aux_expr return e in
        link to_ (Option.get env.current_return);
        (* i think this is conservative but i am not so sure *)
        (* FIXME: should probably return (return, `return)
         * and that way, we don't the env anymore *)
        return, to_
    | J.Js_return (_, None) ->
        let return = node_of_stm "return" orig_stm in
        link return (Option.get env.current_return);
        (* FIXME: same problem as above *)
        return, return
    | J.Js_continue (_, o) ->
        (* FIXME: same problem as above *)
        let continue = node_of_stm "continue" orig_stm in
        link continue (
          match o with
          | None ->  Option.get env.current_continue
          | Some label -> StringMap.find label env.labels);
        continue, continue
    | J.Js_break (_, o) ->
        let break = node_of_stm "break" orig_stm in
        link break (
          match o with
          | None -> Option.get env.current_break
          | Some label -> StringMap.find label env.labels
        );
        (* FIXME same problem as above *)
        break, break
    | J.Js_switch (_,e,esl,o) ->
        let start = node_of_stm "switch1" orig_stm in
        let end_ = node_of_stm "switch2" orig_stm in
        let from_e = aux_expr start e in
        let env = {env with current_break = Some end_} in
        (match esl with
         | [] -> assert false
         | (e',s) :: esl ->
             let from_e' = aux_expr from_e e' in
             let start_s, end_s = aux_stm env s in
             link from_e' start_s;
             let last_end_s =
               List.fold_left
                 (fun last_end_s (e',s) ->
                    let from_e' = aux_expr from_e e' in
                    let start_s, end_s = aux_stm env s in
                    link from_e' start_s;
                    link last_end_s start_s;
                    end_s
                 ) end_s esl in
             match o with
             | None -> link from_e end_; link last_end_s end_
             | Some s ->
                 let start_s, end_s = aux_stm env s in
                 link last_end_s start_s;
                 link from_e start_s;
                 link end_s end_);
        start, end_
    | J.Js_throw _ ->
        (* exceptions are not dealt with
         * presumably, you should say that a throw flows to the exit of
         * the current function *)
        raise NotImplemented
    | J.Js_label (_, label, s) ->
        let node = node_of_stm "label" orig_stm in
        let env = {env with labels = StringMap.add label node env.labels} in
        aux_stm env s
    | J.Js_if (_,e,s,o) ->
        let start = node_of_stm "if1" orig_stm in
        let end_ = node_of_stm "if2" orig_stm in
        let from_e = aux_expr start e in
        let to_s, from_s = aux_stm env s in
        link from_e to_s;
        link from_s end_;
        (match o with
         | None ->
             link from_e end_
         | Some s ->
             let to_s, from_s = aux_stm env s in
             link from_e to_s;
             link from_s end_
        );
        start, end_
    | J.Js_expr (_, e) ->
        let start = node_of_stm "expr" orig_stm in
        start, aux_expr start e
    | J.Js_trycatch _ ->
        (* that one is possible is to do, but you have to assume that every function call
         * can possibly raise exceptions *)
        raise NotImplemented
    | J.Js_dowhile _ ->
        (* this one is just lazyness, because nobody uses it *)
        raise NotImplemented
    | J.Js_comment _ ->
        let dummy = node_of_stm "comment" orig_stm in
        dummy, dummy

  and aux_stms env stms =
    (match stms with
     | [] ->
         let dummy = node_of_stm "emptyblock" (JsCons.Statement.block []) in
         dummy, dummy
     | s :: stms ->
         let to_s, from_s = aux_stm env s in
         let from_stms =
           List.fold_left
             (fun from s ->
                let to_, from2 = aux_stm env s in
                link from to_;
                from2
             ) from_s stms in
         to_s, from_stms)

  and aux_expr_option from = function
    | None -> from
    | Some e -> aux_expr from e

  (* [aux_expr] returns the output node of the control flow graph
   * of [expr] that starts at [from] *)
  and aux_expr from orig_expr =
    match orig_expr with
    | J.Je_ident (_,i) when JsIdentSet.mem i !local_vars
      ->
        let node = node_of_expr "ident_use" ~use:[i] orig_expr in
        link from node;
        node

    | J.Je_ident _
    | J.Je_this _
    | J.Je_string _
    | J.Je_num _
    | J.Je_null _
    | J.Je_undefined _
    | J.Je_bool _
    | J.Je_regexp _
      ->
        from

    | J.Je_function _ ->
        (* presumably we should analyse the body of the function and
         * local variables from our scope used inside the local function
         * flow to the function entry point
         * and when we see a call to the function then the flow of the control
         * goes to the caller, the arugments, the entry point and then comes out of its exit
         * (thus the variables captured by the closures are used
         * when the closure is used)
         * and what if the closure escape the scope?
         * the closure just flows to the exit of the function which
         * should possibly count as a use of the function *)
        raise NotImplemented (* what should i do ?? *)

    | J.Je_array (_,el) ->
        List.fold_left aux_expr from el
    | J.Je_comma (_, el, e) ->
        aux_expr (List.fold_left aux_expr from el) e
    | J.Je_object (_,sel) ->
        List.fold_left (fun from (_s,e) -> aux_expr from e) from sel
    | J.Je_call (_,e,el,_)
    | J.Je_new (_,e,el) ->
        List.fold_left aux_expr (aux_expr from e) el

    | J.Je_unop (label,( J.Ju_add2_pre
                       | J.Ju_sub2_pre
                       | J.Ju_add2_post
                       | J.Ju_sub2_post
                   ), J.Je_ident (_,i)) when JsIdentSet.mem i !local_vars ->
        let node = node_of_expr "ident_incr" ~def:([i],Some label) ~use:[i] orig_expr in
        link from node;
        node

    | J.Je_dot (_,e,_)
    | J.Je_unop (_,_,e) ->
        aux_expr from e

    | J.Je_binop (label, J.Jb_assign, J.Je_ident (_,i), J.Je_ident (_,j)) when JsIdentSet.mem i !local_vars ->
        (* special case for aliases
         * if we don't do that, then we can not squash some aliases
         * in expression such as
         * (x = y, $an expression using x and y$) *)
        let alias, use = if JsIdentSet.mem j !local_vars then true, [j] else false, [] in
        let node = node_of_expr "ident_alias" ~alias ~def:([i],Some label) ~use orig_expr in
        link from node;
        node

    | J.Je_binop (label,
        ( J.Jb_assign
        | J.Jb_mul_assign
        | J.Jb_div_assign
        | J.Jb_mod_assign
        | J.Jb_add_assign
        | J.Jb_sub_assign
        | J.Jb_lsl_assign
        | J.Jb_lsr_assign
        | J.Jb_asr_assign
        | J.Jb_and_assign
        | J.Jb_xor_assign
        | J.Jb_or_assign as op ), J.Je_ident (_,i), e) as orig_expr when JsIdentSet.mem i !local_vars ->
        (* [i += e] must first read [i], and then evaluate [e] (because [e] may change the value of [i]) *)
        let node =
          if op = J.Jb_assign then
            from
          else (
            let node = node_of_expr "ident_def_use" ~use:[i] orig_expr in
            link from node;
            node
          ) in
        let to_e = aux_expr node e in
        let node = node_of_expr "ident_def" ~def:([i],Some label) orig_expr in
        link to_e node;
        node
    | J.Je_binop (_,_,e1,e2) ->
        (* when you have an assigmment to something that is not an ident
         * (like [r.field]) then it doesn't count as defining [r]
         * it is actually a use of [r] ! *)
        aux_expr (aux_expr from e1) e2
    | J.Je_cond (_,e1,e2,e3) ->
        let to_1 = aux_expr from e1 in
        let to_2 = aux_expr to_1 e2 in
        let to_3 = aux_expr to_1 e3 in
        let node = node_of_expr "ift" orig_expr in
        link to_2 node;
        link to_3 node;
        node

    | J.Je_runtime (_, e) -> (
        match e with
        | JsAstRuntime.SetDistant _ -> raise Exit
        | JsAstRuntime.TaggedString _ -> from
      )

    | J.Je_hole _
      ->
        raise Exit (* we cannot do anything in that case
                    * so we abort the analysis *) in
  let build_graph_for_a_function code_elt ?name params body =
    let arguments = JsCons.Ident.native "arguments" in
    local_vars := JsIdentSet.from_list params;
    local_vars :=
      List.fold_left (
        JsWalk.OnlyStatement.fold
          (fun local_vars -> function
           | J.Js_var (_,i,_)
           | J.Js_function (_,i,_,_) ->
               if JsIdent.equal i arguments then
                 raise Exit (* if you can use a parameter by saying arguments[i]
                             * then some uses of your parameters are hidden
                             * and squashing won't be correct *)
               else
                 JsIdentSet.add i local_vars
           | _ -> local_vars
          )
      ) !local_vars body;

    let node = node_of_stm "function_entry" code_elt in
    let node1 = node_of_stm "function_param" code_elt in
    let node2 = node_of_stm "function_return" code_elt in
    let node_params =
      List.map (fun param -> node_of_stm ~use:params ~def:([param],None) "param" code_elt) params in
    List.iter
      (fun n1 ->
         link node n1;
         link n1 node1;
         List.iter (fun n2 -> link n1 n2) node_params
      ) node_params;
    try
      let to_, from = aux_stms {env with current_return = Some node2} body in
      link node1 to_;
      link from node2;
      let _file =
        match name with
        | Some J.ExprIdent s ->
            let s = Ident.stident s in
            if String.length s > 100 then String.sub s 0 100 else s
        | Some J.Native (_,s) ->
            if String.length s > 100 then String.sub s 0 100 else s
        | None ->
            "anon" in
      #<If:JS_RENAMING$is_contained _file>Viz.to_file_and_ps (_file^"_0_cfg") g#<End>;
      Some (_file, to_, g)
    with
    | Exit ->
      (* someone aborted the analysis for good reasons *)
      None
    | NotImplemented ->
        (* the analysis failed on a construct
         * that it cannot handle for now *)
        None
  in
  let code_elt = JsCons.Statement.block [] in (* FIXME: a bit dirty, but useless for now anyway *)
  build_graph_for_a_function code_elt ?name params body

(*
 * This function updates the control flow graph
 * so that we know at each point which variables are needed
 * and which aren't
 *)
let liveliness_analysis g =
  (* i think i remember that SCC.scc_list is buggy *)
  let groups = Array.to_list (SCC.scc_array g) in
  List.iter
    (fun nodes ->
       while (* fixpoint *) (
         List.fold_left
           (fun continue node ->
              let live_out =
                G.fold_succ (fun vertex acc -> JsIdentSet.union vertex.live_in acc) g node JsIdentSet.empty in
              let new_live_out = JsIdentSet.union live_out node.live_out in
              node.live_out <- new_live_out;
              let old_live_in = node.live_in in
              let new_live_in = JsIdentSet.union node.use (JsIdentSet.diff new_live_out node.def) in
              node.live_in <- new_live_in;
              (* whenever one [live_in] set is not stable in an iteration
               * then we must continue looping *)
              continue || JsIdentSet.size old_live_in <> JsIdentSet.size new_live_in
           )
           false nodes
       ) do () done
    ) groups

(*
 * This function uses the control flow graph decorated by the liveliness
 * analysis to create the inteference graph, ie a graph when local identifiers
 * are nodes and there are edges between identifiers that cannot be squashed

 * This function also returns the set of useless bindings
 * (ie assigments that are never read)
 *)
let build_interference_graph control_flow_graph =
  let g = GIdent.create () in
  G.iter_vertex
    (fun node ->
       JsIdentSet.iter
         (fun v ->
            GIdent.add_vertex g v
         ) node.def
    ) control_flow_graph;
  let dummy_bindings = ref AnnotSet.empty in
  G.iter_vertex
    (fun node ->
       let set1 = node.def in
       let set2 = JsIdentSet.diff node.live_out node.def in
       let set2 = if node.alias then JsIdentSet.diff set2 node.use else set2 in
       if JsIdentSet.inter set1 node.live_out = JsIdentSet.empty
       && node.label <> None
       && #<If:JS_RENAMING$contains "binding">false#<Else>true#<End>
       then (
         (* beware: here we are not building the interference in the graph
          * this is correct only because we know that the binding will be removed later *)
         dummy_bindings := AnnotSet.add (Option.get node.label) !dummy_bindings;
       ) else
         JsIdentSet.iter
           (fun v1 ->
              if GIdent.mem_vertex g v1 then
                JsIdentSet.iter
                  (fun v2 ->
                     if GIdent.mem_vertex g v2 then
                       GIdent.add_edge g v1 v2
                  ) set2
           ) set1
    ) control_flow_graph;
  g, !dummy_bindings

(*
 * Coloring the interference graph
 * Each color then becomes one variable name
 * Since several variables can be given the same color,
 * variables can be squashed
 *
 * Here we do not try very hard to find a good coloring
 * (currently, ocamlgraph implements a simple greedy algorithm)
 * because trying harder completely blew up compilation times
 * and it turns out to be satisfactory as is
 *)
let color_interference_graph g =
  let size = max 1 (GIdent.nb_vertex g) in
  (size, Coloring.coloring g size)

(*
 * This function uses to the result of the coloring
 * to rename the code
 * It also removes removes useless bindings as identified when
 * building the interference graph
 * Some care is taken:
 * - to rename identifiers in a predictable order
 *   (you can't use colors directly as identifiers, it is too fragile)
 * - to remove variable declarations
 *   that arise because several variables were squashed together
 *)
let squash_variables dummy_bindings renaming params body =
  (* colors seems to be numbered from 1 *)
  (* the seen table allow one to avoid renaming *)
  let length = Coloring.H.length renaming + 1 in
  let seen = Array.make length false in
  let var_of_int_unseen =
    (* FIXME: could use an array instead of a hashtbl because *)
    let next = let r = ref (-1) in fun () -> incr r; !r in
    let h = Hashtbl.create length in
    fun color ->
      try Hashtbl.find h color
      with Not_found ->
        let ident = JsCons.Ident.native (IdentGenerator.alphanum (next ())) in
        Hashtbl.add h color ident;
        ident in
  let var_of_int color =
    seen.(color) <- true;
    var_of_int_unseen color in
  let orig_params = params in
  let params =
    let aux param = var_of_int (Coloring.H.find renaming param) in
    List.map aux params in

  (* first renaming variables in expressions *)
  let body =
    List.map
      (JsWalk.ExprInStatement.map
         (fun e ->
            match e with
            | J.Je_binop (label,_,_,e) when AnnotSet.mem (Annot.annot label) dummy_bindings ->
                e
            | J.Je_ident (label,s) ->
                (try J.Je_ident (label, var_of_int (Coloring.H.find renaming s))
                 with Not_found -> e)
            | J.Je_function _ -> assert false
            | _ -> e)
      ) body in

  (* the variables renamed so far are the only used variables (and not just defined variables) *)
  List.iter (fun p ->
               let color = Coloring.H.find renaming p in
               seen.(color) <- false (* no need to put a var on a variable that is a parameter *)
            ) orig_params;

  (* rewriting the Js_var nodes:
     - remove duplicate [var] arising from squashed variables
     - renaming the variables
     - removing some bindings that were detected as useless
  *)
  let body =
    List.map
      (JsWalk.OnlyStatement.map_up (* map up because we must not call ourself recursively THERE *)
         (fun s ->
            match s with
            | J.Js_var (label, s, Some e) when AnnotSet.mem (Annot.annot label) dummy_bindings ->
                let color = Coloring.H.find renaming s in
                if seen.(color) then (
                  seen.(color) <- false;
                  (* THERE *)
                  JsCons.Statement.block [
                    J.Js_var (label, var_of_int_unseen color, None);
                    J.Js_expr (label,e);
                  ]
                ) else
                  J.Js_expr (label,e)
            | J.Js_var (label, s, e) ->
                (try
                   let color = Coloring.H.find renaming s in
                   if seen.(color) then (
                     seen.(color) <- false;
                     J.Js_var (label, var_of_int_unseen color, e)
                   ) else
                     (* keeping only one var (plus possibly the same declaration
                      * but from a function parameter:
                      * [function f(a) { var a; return a }]) *)
                     match e with
                     | None -> JsCons.Statement.block []
                     | Some e -> JsCons.Statement.assign_ident (var_of_int_unseen color) e
                 with Not_found ->
                   (* we are in that case if there is a var in the code
                    * but its value is never used (and so it doesn't end up in
                    * the graphs) (only if e is None) *)
                   match e with
                   | None -> JsCons.Statement.block [] (* local var *)
                   | Some _ -> assert false)
            | J.Js_function _ ->
                assert false
            | _ -> s
         )
      ) body in
  params, body

(* opera says the result of [function(){var x; {a:x, b:(x=1)}.a}()] is 1 when it should be undefined
 * to solve this problem, whenever the value of an identifier is used directly as the value of a field
 * in an object literal, it is replaced by [ident || ident] if it is overwritten in other fields
 * because [function(){var x; {a:(x||x), b:(x=1)}.a}()] gives undefined all right

 * A few examples
 * {a:x, b:x} -> nothing happens
 * {a:x, b:(x=1)} -> {a:x||x, b:(x=1)}
 * {a:x, b:x}, x=1 -> nothing happens
 * {a:(1,x), b:(x=1)} -> {a:(1,x||x), b:(x=1)}
 * {a:(x,1), b:(x=1)} -> nothing happens
 * {a:(y=x), b:(x=1)} -> {a:(y=x||x), b:(x=1)}
 *)
let hack_for_opera body =
  let map_stm stm =
    let _acc, stm =
      JsWalk.ExprInStatement.self_traverse_foldmap_context_down
        (fun self tra env acc e ->
           (* env:
            * It is Some _ when we are in the rhs in an object literal
            * only when we are directly under the colon of [field:expr]
            * (and we accept going though the last expression in a comma
            * the expression of an assignment, and the rhs of && and ||)
            * in this case, env contains the set of identifiers that are assigned to
            * by the following fields of the object literals
            * In all other cases, env is None
            *
            * acc:
            * accumulates the set of identifiers written to in the current expression
            * this value is reset when entering an object literal
            *)
           match e with
           | J.Je_object (label, sel) ->
               let new_acc, sel' =
                 List.fold_right_map_stable
                   (fun acc ((s,e) as p) ->
                      let acc, e' = self (Some acc) acc e in
                      acc, if e == e' then p else (s, e')
                   ) JsIdentSet.empty sel in
               let acc = JsIdentSet.union acc new_acc in
               acc,
               if sel' == sel then
                 e
               else
                 J.Je_object (label, sel')
           | J.Je_ident (label,x) -> (
               match env with
               | Some set when JsIdentSet.mem x set ->
                   let label2 = Annot.refresh label in
                   acc, JsCons.Expr.lor_ (J.Je_ident (label,x)) (J.Je_ident (label2,x))
               | _ ->
                   acc, e
             )
           | J.Je_binop (label, J.Jb_assign, e1, e2) -> (
               match e1 with
               | J.Je_ident (label, i) ->
                   let acc = JsIdentSet.add i acc in
                   let acc, e2' = self env acc e2 in
                   acc,
                   if e2 == e2' then e else J.Je_binop (label, J.Jb_assign, e1, e2')
               | _ ->
                   let acc, e1' = self None acc e1 in
                   let acc, e2' = self env acc e2 in
                   acc,
                   if e1 == e1' && e2 == e2' then e else J.Je_binop (label, J.Jb_assign, e1', e2')
             )
           | J.Je_comma (label, el, last_e) ->
               let acc, el' =
                 List.fold_left_map_stable
                   (fun acc e ->
                      let acc, e' = self None acc e in
                      acc, e'
                   ) acc el in
               let acc, last_e' = self env acc last_e in
               acc,
               if el == el' && last_e == last_e' then e else J.Je_comma (label, el',last_e')
                 (* special case for lazy operators,
                  * since they may not force the interpret to deference the pointers... *)
           | J.Je_binop (label, (J.Jb_land | J.Jb_lor as op), e1, e2) ->
               let acc, e1' = self None acc e1 in
               let acc, e2' = self env acc e2 in
               acc,
               if e1 == e1' && e2 == e2' then e else J.Je_binop (label, op, e1', e2')
           | _ -> tra None acc e
        ) None JsIdentSet.empty stm in
    stm in
  List.map map_stm body

(* TODO: simplify the graph:
 * when a node has def = [] and use = []
 * then it was used for building the graph but it can be short circuited now *)
let rename_function ?name params body =
  let _chrono = Chrono.make () in
  _chrono.Chrono.start ();
  match build_control_flow_graph ?name params body with
  | None -> params, body, true
  | Some (_file, _entry, g) ->
      #<If:JS_RENAMING$contains "time"> Printf.printf "** %s\n%!" _file #<End>;
      #<If:JS_RENAMING$contains "time"> Printf.printf "cfg: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
      liveliness_analysis g;
      #<If:JS_RENAMING$contains "time"> Printf.printf "liveliness: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
      #<If:JS_RENAMING$is_contained _file>Viz2.to_file_and_ps (_file^"_1_liv") g#<End>;
      let ig, dummy_bindings = build_interference_graph g in
      #<If:JS_RENAMING$contains "time"> Printf.printf "interference: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
      #<If:JS_RENAMING$is_contained _file>Viz3.to_file_and_ps (_file^"_2_interf") ig#<End>;
      let _k, h = color_interference_graph ig in
      #<If:JS_RENAMING$contains "time"> Printf.printf "coloring: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
      #<If:JS_RENAMING$is_contained _file>Printf.printf "colored with %d colors\n%!" _k#<End>;
      let params, body = squash_variables dummy_bindings h params body in
      #<If:JS_RENAMING$contains "time"> Printf.printf "squashing: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
      let body = hack_for_opera body in
      #<If:JS_RENAMING$contains "time"> Printf.printf "hack for opera: %fs\n%!" (_chrono.Chrono.read ()); _chrono.Chrono.restart () #<End>;
      params, body, false

let rename_code_elt code_elt =
  let failed, code_elt =
    JsWalk.TStatement.traverse_foldmap
      (fun tra _ acc stm ->
         match stm with
         | J.Js_function (label,name,params,body) ->
             let params, body, failed = rename_function ~name params body in
             if failed then
               (* if it failed, we can still try to rewrite inner functions *)
               tra true stm
             else
               acc, J.Js_function (label,name,params,body)
         | _ -> tra acc stm)
      (fun tra _ acc e ->
         match e with
         | J.Je_function (label,name,params,body) ->
             let params, body, failed = rename_function ?name params body in
             if failed then
               tra true e
             else
               acc, J.Je_function (label,name,params,body)
         | _ -> tra acc e)
      false code_elt in
  if failed then JsPasses.local_alpha_stm code_elt else code_elt

let rename code =
  List.map rename_code_elt code
