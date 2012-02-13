(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
(* CF mli *)

(* depends *)
module List = Base.List

module type ItemType =
sig
  type t
  val index : t -> string
  val depends : t -> string list
end

module type S =
sig
  type t
  exception CyclicDep of t
  exception IndexConflict of t * t
  exception Not_found_index of string
  val sort : t list -> t list * (string * t list) list

  type env
  val compute : t list -> env
  val get_order : env -> t list * (string * t list) list
  val transitive_dependencies : env -> t -> t list
end

module Make (Elemt: ItemType) : S with type t = Elemt.t =
struct
  type t = Elemt.t
  exception CyclicDep of t
  exception IndexConflict of t * t
  exception Not_found_index of string
  type infos_elt =
      {
        tri_topo_elt : Elemt.t;
        tri_topo_depends : StringSet.t
      }

  type env = infos_elt StringMap.t * (t list * (string * t list) list)

  let compute elt_list =
    (* Creation des environnements *)
    let f_fold (accu_map, accu_list) elt =
      let index = Elemt.index elt and depends = Elemt.depends elt in
      let topo_elt =
        match StringMap.find_opt index accu_map with
        | None -> { tri_topo_elt = elt; tri_topo_depends = StringSet.from_list depends }
        | Some infos -> raise (IndexConflict (elt, infos.tri_topo_elt))
      in
      (StringMap.add index topo_elt accu_map),
      ((index, topo_elt)::accu_list)
    in
    let map_dep, rev_list_dep = List.fold_left f_fold (StringMap.empty, []) elt_list in
    let list_dep = List.rev rev_list_dep in
    (* On lance le tri *)
    let not_referenced = Stack.create () in
    let visited = Hashtbl.create 10 in
    let detect_cycle = Hashtbl.create 10 in
    let ordered = Stack.create () in
    let rec visite s infos_elt =
      if Hashtbl.mem visited s
      then ()
      else
        if Hashtbl.mem detect_cycle s
        then raise (CyclicDep infos_elt.tri_topo_elt)
        else
          let tri_topo_elt = infos_elt.tri_topo_elt in
          begin
            (* se dire visitÃ© *)
            Hashtbl.add detect_cycle s ();
            (* visiter les voisins *)
            (* creer la liste des voisins viables *)
            let safe_voisins =
              let f_fold v accu =
                try
                  let voisin = StringMap.find v map_dep in
                  (v, voisin)::accu
                with
                | Not_found -> Stack.push (v, tri_topo_elt) not_referenced; accu
              in StringSet.fold f_fold infos_elt.tri_topo_depends []
            in
            List.iter (fun (a, b) -> visite a b) safe_voisins;
            (* se mettre dans le tri *)
            Hashtbl.add visited s ();
            Stack.push tri_topo_elt ordered
          end
    in
    (* Faire le parcours dans l'ordre donne : stabilite du tri topo *)
    List.iter (fun (a, b) -> visite a b) list_dep;
    (* StringMap.iter visite map_dep; : posait probleme *)
    (* acceder aux listes des index not_referenced, et de l'association ordered, elt *)
    let list_not_referenced =
      let rec aux accu =
        if Stack.is_empty not_referenced then accu
        else
          let missing, from = Stack.pop not_referenced in
          let was = Option.default [] (StringMap.find_opt missing accu) in
          let accu = StringMap.add missing (from::was) accu in
          aux accu
      in
      let accu = aux StringMap.empty in
      let fold key set accu = (key, set)::accu in
      StringMap.fold fold accu []
    in
    let list_assoc =
      let rec aux accu =
        if Stack.is_empty ordered then accu
        else
          let index = Stack.pop ordered in
          aux (index::accu)
      in aux []
    in
    map_dep, (list_assoc, list_not_referenced)

  let get_order (_, order) = order
  let sort elt_list = get_order (compute elt_list)

  let transitive_dependencies (map, (_, not_referenced)) t =
    (* since the order has been done, there are no loop *)
    let rec gather_children index acc =
      if StringMap.mem index acc then
        (* this shorcut gives a big speedup *)
        acc
      else
        match StringMap.find_opt index map with
        | Some elt ->
            let depends = elt.tri_topo_depends in
            let acc = StringMap.add index elt.tri_topo_elt acc in
            StringSet.fold gather_children depends acc
        | None ->
            if List.StringAssoc.mem index not_referenced
            then acc
            else raise (Not_found_index index)
    in
    let index = Elemt.index t in
    let all = gather_children index StringMap.empty in
    let children = StringMap.remove index all in
    StringMap.elts children
end
