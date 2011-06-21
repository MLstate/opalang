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
(*
    @author Rudy Sicard
    @author Maxime Audouin
**)

(* TODO remove *)
open Base

let (|>) = InfixOperator.(|>)

(**)

module V = GraphUtils.Int.V
module G = GraphUtils.Int.G
module SCC = GraphUtils.Int.SCC

module Debug_int =
struct
  let r = ref IntMap.empty
  let clear() = r:= IntMap.empty
  let add i s = r:= IntMap.add i s !r
  let get i =  try IntMap.find i !r with Not_found -> "UNKNOWN FOR DEBUG_INT"
end

let get_reachable_graph_from roots addon_roots graph =
  GraphUtils.Int.get_reachable_graph_from ~addon_roots roots graph


let debug_deps depsmap =
  let () = prerr_endline "digraph g {"
  in let () = IntMap.fold
      (fun k v () ->
         IntSet.fold
           (fun v () -> prerr_endline (Printf.sprintf "%d -> %d;" k v))
           v
           ()
      ) depsmap ()
  in prerr_endline "}"

let depsToGraph = GraphUtils.Int.graph_of_deps

let create_group_list roots (addon_roots:(int list IntMap.t)) deps_set =
  let fold1(set,init)(f) =IntMap.fold f set init in
  let fold2(set,init)(f) =IntSet.fold f set init in
  let depsMap = IntMap.from_list deps_set in
  let graph = depsToGraph deps_set in
  let graph = if roots <> [] then get_reachable_graph_from roots addon_roots graph else graph in
  (* decomposition en composante fortement connexe *)
  let _groups = SCC.scc_array graph in
  let _groups = Array.to_list (Array.map (fun list ->
                                            let id_group = List.hd list in
                                              id_group, IntSet.from_list list) _groups
                              )
  in
    (* assign an integer to all top-level value
       assign an integer to all groups (the minimum of all top value)
       generate the topological order using this integer
    *)
    (* group priority = min decl priority *)
  let group_prio  =
    (* ordre des declarations *)
    let prior = List.mapi (fun i (n,_)-> (n,i)) deps_set |> IntMap.from_list in
    let map = List.map (fun (i,l) ->
                          let prio = fold2(l,max_int)(fun n int-> min int (IntMap.find n prior))
                          in (i,prio)
                       )  _groups   |> IntMap.from_list in
      fun n -> IntMap.find n map
  in
  let group_sort l =  List.sort  (fun g1 g2 -> (compare (group_prio g1) (group_prio g2))) l in
  let group_revsort l =  List.sort  (fun g1 g2 -> -(compare (group_prio g1) (group_prio g2))) l in
  let groups_source_order = group_sort (List.map fst  _groups) in
    (* nom de decl -> group *)
  let group_of =
    let map =
      List.fold_left
        (fun map (g,set) -> IntSet.fold (fun n map -> IntMap.add n g map) set map)
        IntMap.empty _groups
    in
      fun d -> (IntMap.find d map) in
  let group_deps  =
    let map =
      fold1(depsMap,IntMap.empty)
        (fun decl decldeps map->
           if G.mem_vertex graph decl then (
             (* normal case *)
             (*OpaEnv.EnvLib.debug_do "cleaning" (fun ()->warning ("Kept : " ^(Debug_int.get decl)));*)
             let deps =
               fold2(decldeps,IntSet.empty)
                 (fun d set -> IntSet.add (group_of d) set) in
             let g_of_decl = group_of decl in
             let update =
               try
                 IntSet.union deps (IntMap.find g_of_decl map)
               with
                 | Not_found -> deps
             in
               IntMap.add g_of_decl update map
           ) else (
             (* case where the declaration is not reachable from the roots *)
              (*OpaEnv.EnvLib.debug_do "cleaning" (fun ()->warning ("Cleaned : " ^(Debug_int.get decl)));*)
             map
           )
           )
    in IntMap.map (fun set-> group_revsort (IntSet.elements set) ) map
  in

  (* take group in source order, adding its non fullfilled dependencies in source order *)
  (* a group cannot be recursiv (if not will loop)!!! *)
  let rec order_group alltodo dones_set dones_list =
    let already_done x = IntSet.mem x dones_set in
    let deps x = IntMap.find x group_deps in
      match alltodo with
        | [] -> List.rev dones_list
        | todo :: remtodo when already_done todo -> order_group remtodo dones_set dones_list
        | todo :: remtodo ->
            let newalltodo  = List.fold_left (fun alltodo n-> if already_done n || n=todo then alltodo else (
                                                n::alltodo)
                              ) alltodo (deps todo)
            in
              if alltodo=newalltodo then order_group remtodo (IntSet.add todo dones_set) (todo::dones_list)
              else order_group newalltodo dones_set dones_list
  in
  let _groups_ordered =  order_group  groups_source_order IntSet.empty [] in
  let _groups_map = IntMap.from_list _groups in
    List.map (fun n->
                let group = IntMap.find n _groups_map in
                let is_rec =
                  IntSet.cardinal group > 1 ||
                    (let n = IntSet.choose group in
                     G.mem_edge graph n n) in
                n,is_rec,group) _groups_ordered , depsMap
