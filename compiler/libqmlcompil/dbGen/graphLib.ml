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
   Private module for DB-Schema manipulation.
   @author Louis Gesbert
   @author Vincent Benayoun (refactoring)
*)

(* This module is a library over OcamlGraph.
   It gathers generic functions for graphs. *)

let internal_error fmt = OManager.i_error fmt

module SchemaGraph (Vertices: Graph.Sig.COMPARABLE) (Edges: Graph.Sig.ORDERED_TYPE_DFT) = struct
  
  module SchemaGraph0 = Graph.Persistent.Digraph.ConcreteLabeled (Vertices) (Edges)
  module V = SchemaGraph0.V
  module E = SchemaGraph0.E

  (* Returns an unspecified vertex from a graph *)
  exception Vertex_found of V.t
  let get_node t =
    try ignore (SchemaGraph0.map_vertex (fun n -> raise (Vertex_found n)) t);
      internal_error "get_node on empty graph"
    with Vertex_found n -> n
    
  (** Used for fresh nodeids. Starts from 1 (the root is 0) *)
  let new_nodeid =
    let z = ref 1 in
    fun () ->
      let a = !z in
      let package_name = ObjectFiles.get_current_package_name() in
      incr z; string_of_int(a) ^ package_name

  let out_edge sch node = match SchemaGraph0.succ_e sch node with [e] -> e | _ -> assert false
  let unique_next sch node = match SchemaGraph0.succ sch node with [n] -> n | _ -> assert false

  let replace_node t n1 n2 =
    let in_edges = SchemaGraph0.pred_e t n1 and out_edges = SchemaGraph0.succ_e t n1 in
    let remove_edges = List.fold_left (fun t e -> SchemaGraph0.remove_edge_e t e) in
    let t = remove_edges t in_edges in
    let t = remove_edges t out_edges in
    let t = SchemaGraph0.remove_vertex t n1 in
    let t = SchemaGraph0.add_vertex t n2 in
    let t = List.fold_left (fun t e -> SchemaGraph0.add_edge_e t (E.create (E.src e) (E.label e) n2)) t in_edges in
    let t = List.fold_left (fun t e -> SchemaGraph0.add_edge_e t (E.create n2 (E.label e) (E.dst e))) t out_edges in
    t
      
  let rec find_tail cond = function
    | x::r -> if cond x then Some (x::r) else find_tail cond r
    | [] -> None
        
  let detect_loops t root : E.t list list =
    let rec aux t chain node =
      List.fold_left
        (fun acc succ ->
           match find_tail (fun e -> E.src e = E.dst succ) chain with
           | Some loop -> (loop@[succ])::acc
           | None -> (aux t (chain@[succ]) (E.dst succ))@acc)
        [] (SchemaGraph0.succ_e t node)
    in aux t [] root

  let filter f t =
    (* TODO: improve complexity
     * Note that this function is optimized for the case where the filter
     * removes almost everything in the schema, because it is the common
     * case *)
    let added_everything = ref true in
    let acc =
      SchemaGraph0.fold_vertex
        (fun n acc ->
           if f n then SchemaGraph0.add_vertex acc n else (added_everything := false; acc)
        ) t SchemaGraph0.empty in
    if !added_everything then
      t
    else
      SchemaGraph0.fold_edges_e
        (fun edge acc ->
           if f (E.src edge) && f (E.dst edge) then
             SchemaGraph0.add_edge_e acc edge
           else
             acc
        ) t acc

end
