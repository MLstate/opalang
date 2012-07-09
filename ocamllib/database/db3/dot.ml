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
module List = BaseList
  (************************)
  (* dot files generation *)
  (************************)

let print_content db node =
  match (Node.get_content ~f:(Hldb.get_node_of_uid db) node) with
  | Datas.Data d -> (
    let res = DataImpl.to_string d in
    if BaseString.is_substring "graph" res 0 then
      "graph ..."
    else
      res
  )
  | Datas.Link p -> Printf.sprintf "Link %s" (Path.to_string p)
  | Datas.Copy (r, p) -> Printf.sprintf "Copy (%s, %s)"
    (Option.to_string Revision.to_string r) (Path.to_string p)
  | Datas.UnsetData -> "UnsetData"

let get_name_of_eid eid =
  if Eid.value eid = 0
  then "Root"
  else ""

let dot_aux db rev node_map name node s =
  match Node.get_content ~f:(Hldb.get_node_of_uid db) node with
  | Datas.Link l ->
    let eid3, _ = Hldb.get_eid_of_path db rev l in
    let uid3 = Hldb.get_uid_of_eid db rev eid3 in
    let node3 = Hldb.get_node_of_uid db uid3 in
    let name3 = Printf.sprintf "%s(%s, %s) %s"
      (get_name_of_eid eid3) (Eid.to_string eid3)
      (Uid.to_string uid3) (String.escaped (print_content db node3))
    in
    db, rev, node_map, name,
    Printf.sprintf
      "%s    \"%s\" -> \"%s\" [style=\"dotted\"]\n" s name name3
  | _ -> db, rev, node_map, name, s

  (*    FIXME : essayer de decomposer  *)
let generation db fm =
  let f = Hldb.get_node_of_uid db in
  let rev = Hldb.get_rev db in
  let uid_map = Hldb.get_uid_map db in
  let node_map = Hldb.get_node_map db in
  let s, _ = EidMap.fold (
    fun eid map (acc, eids) ->
      if List.mem eid eids
      then (
        let uid = snd (RevisionMap.max map) in
        let node1 = Hldb.get_node_of_uid db uid in
        let eids = List.remove_all eid eids in
        let eids =
          let tmp = Node.get_children_eid ~f node1 in
          tmp @ eids
        in
        let _, _, _, _, s =
          let name = Printf.sprintf "%s(%d, %s) %s"
            (get_name_of_eid eid) (Eid.value eid) (Uid.to_string uid)
            (String.escaped (print_content db node1)) in
          let foo = fun k eid2 (db, rev, node_map, name, acc) ->
            let uid2 = Hldb.get_uid_of_eid db rev eid2 in
            let node2 = Hldb.get_node_of_uid db uid2 in
            let name2 = Printf.sprintf "%s(%s, %s) %s"
              (get_name_of_eid eid2) (Eid.to_string eid2)
              (Uid.to_string uid2) (String.escaped (print_content db node2)) in
            let s = Printf.sprintf "%s    \"%s\" -> \"%s\" [label=\"%s\"] \n"
              acc name name2 (Keys.to_string k) in
            db, rev, node_map, name, s
          in
          let db, rev, node_map, name, s = Node.fold ~f foo node1
            (db, rev, node_map, name, acc) in
          dot_aux db rev node_map name node1 s
        in s, eids
      )
      else acc, eids
  ) uid_map ("digraph {\n", [Hldb.root_eid])
  in
  let s = Printf.sprintf "%s}" s in
  IoManager.output_dot fm (Revision.value rev) s
