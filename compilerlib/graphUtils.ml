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
(** same module as the one from ocamlgraph, except that this one allows you to
    choose the size of the hashtbls used internally
    and so it is much faster (unless you're stupid) on small input graphs
*)
module Components =
struct
  module type G = sig
    type t
    module V : Graph.Sig.COMPARABLE
    val iter_vertex : (V.t -> unit) -> t -> unit
    val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  end

  module Make(G:G) =
  struct
    module Hash = Hashtbl.Make(G.V)

    (* Tarjan's algorithm for computings strongly connected components
     * http://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
     *)
    let scc ~size graph =
      let index = ref 0 in
      let stack = ref [] in
      let indexes = Hash.create size in
      let lowlinks = Hash.create size in
      let sccs = ref [] in
      let rec decant acc min_index = function
        | (node,index) :: tl when index >= min_index ->
            Hash.remove lowlinks node; (* satisfying the invariant described below *)
            decant (node :: acc) min_index tl
        | l ->
            sccs := acc :: !sccs;
            stack := l in
      let rec tarjan v = (* returns the value of the first test *)
        if not (Hash.mem indexes v) then (
          let local_index = !index in
          Hash.add indexes v local_index;
          Hash.add lowlinks v local_index; (* invariant: in lowlinks <=> in the stack *)
          stack := (v,local_index) :: !stack;
          incr index;
          G.iter_succ
            (fun v' ->
               if tarjan v' then
                 try Hash.replace lowlinks v (min (Hash.find lowlinks v) (Hash.find lowlinks v'))
                 with Not_found -> () (* happens when the value has been popped off the stack
                                       * in which case, it had a greater value anyway *)
               else if Hash.mem lowlinks v' (* using the invariant to check [v' in the stack] *) then
                 Hash.replace lowlinks v (min (Hash.find lowlinks v) (Hash.find indexes v'))
            ) graph v;
          if local_index = Hash.find lowlinks v then
            decant [] local_index !stack;
          true
        ) else
          false in
      G.iter_vertex (fun x -> ignore (tarjan x)) graph;
      List.rev !sccs (* need to reverse so that the result is topologically ordered
                      * if there is an arc from v to u, then v appears before in the list *)
  end
end


module Reachability
  (VSet:BaseSetSig.S)
  (VMap:BaseMapSig.S with type key = VSet.elt)
  (G:sig
     type t
     module V : Graph.Sig.COMPARABLE with type t = VSet.elt
     val mem_vertex : t -> V.t -> bool
     val iter_succ : (V.t -> unit) -> t -> V.t -> unit
     val add_vertex : t -> V.t -> unit
     val add_edge : t -> V.t -> V.t -> unit
     val create : ?size:int -> unit -> t
     val fold_vertex : (V.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
   end) : sig
  val give_unreachable_nodes : G.V.t list -> (G.V.t * VSet.t) list -> VSet.t
  val get_reachable_graph_from : ?addon_roots:G.V.t list VMap.t -> G.V.t list -> G.t -> G.t
  val graph_of_deps : (G.V.t * VSet.t) list -> G.t
end =
struct
  let rec add_successors g graph addon_roots from =
    if not(G.mem_vertex g from) then
      begin
        G.add_vertex g from;
        begin match VMap.find_opt from addon_roots with
        | None -> ()
        | Some li ->
            List.iter (add_successors g graph addon_roots) li
        end;
        G.iter_succ
          (fun to_ ->
             add_successors g graph addon_roots to_;
             G.add_edge g from to_
          ) graph from
      end


  (** computes the subgraph reachable from the roots
      * addon_roots is here to provide some conditional roots:
      * you may have magic_to_string -> [int_to_string, float_to_string, ...]
      * that means if the node magic_to_string is reachable, then you must also keep
      * int_to_string, etc
      * if you don't encounter magic_to_string, then you may or may not keep
      * magic_to_string (depending on whether other expressions need it)
  *)
  let get_reachable_graph_from ?(addon_roots=VMap.empty) roots graph =
    let g = G.create () in
    List.iter
      (fun root ->
         add_successors g graph addon_roots root)
      roots;
    g

  let graph_of_deps (depslist : (G.V.t * VSet.t) list) =
    let dep_g = G.create () in
    List.iter (fun (v0, _) -> G.add_vertex dep_g v0) depslist;
    List.iter (fun (v0, deps) ->
                 VSet.iter (fun v1 -> G.add_edge dep_g v0 v1) deps
              ) depslist;
    dep_g

  let vertices_of (graph:G.t) =
    G.fold_vertex VSet.add graph VSet.empty

  let give_unreachable_nodes roots deps_set =
    let graph = graph_of_deps deps_set in
    let reachable_graph = get_reachable_graph_from roots graph in
    let all_vertices = vertices_of graph in
    let reachable_vertices = vertices_of reachable_graph in
    VSet.diff all_vertices reachable_vertices

end

(** An already instantiated imperative graph with int labels on the nodes *)
module Int =
struct
  module V : Graph.Sig.COMPARABLE with type t = int =
  struct
    type t = int
    let equal : int -> int -> bool = (=)
    let hash = Hashtbl.hash
    let compare : int -> int -> int = Pervasives.compare
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectional (V)
  module SCC = Graph.Components.Make (G)

  let rec add_successors g graph addon_roots from =
    if not(G.mem_vertex g from) then
      begin
        G.add_vertex g from;
        begin match IntMap.find_opt from addon_roots with
        | None -> ()
        | Some li ->
            List.iter (add_successors g graph addon_roots) li
        end;
        G.iter_succ
          (fun to_ ->
             add_successors g graph addon_roots to_;
             G.add_edge g from to_
          ) graph from
      end

  include Reachability(IntSet)(IntMap)(G)
end


module String =
struct
  module V : Graph.Sig.COMPARABLE with type t = string =
  struct
    type t = string
    let equal : string -> string -> bool = (=)
    let hash = Hashtbl.hash
    let compare : string -> string -> int = Pervasives.compare
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectional (V)
  module SCC = Graph.Components.Make (G)
end
module DefaultGraphviz(G:Graph.Sig.G)(T:sig val vertex_name : G.V.t -> string end) =
struct
  include Graph.Graphviz.Dot(
    struct
      include G
      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      let vertex_name = T.vertex_name
      let vertex_attributes _ = []
      let get_subgraph _ = None
      let default_edge_attributes _ = []
      let edge_attributes _ = []
    end
  )
  let to_file filename g =
    let channel = open_out filename in
    output_graph channel g;
    close_out channel
  let to_file_and_ps filename_no_extension g =
    to_file (filename_no_extension^".dot") g;
    let exit_code =
      Sys.command (
        Printf.sprintf "dot -Tps %s.dot > %s.ps"
          filename_no_extension filename_no_extension) in
    if exit_code <> 0 then failwith "GraphUtils.DefaultGraphviz.to_file_and_ps: dot failed"
end
