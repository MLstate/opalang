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
(* depends *)
module List = BaseList

(* -- *)

type full = {
  cur_rev : Revision.t ;
  content : Datas.t ;
  map : Eid.t KeyMap.t ;
}

type delta = {
  new_content : Datas.t option ;
  new_childs : Eid.t KeyMap.t ;
    (* TODO: verify: I think the semantics is that the children
       cannot exist in the old node; they are always added,
       never overwritten. *)
  prof : int ;
}

(* Reverse delta. Tells what to do to get the previous version of a node. *)
type rev_delta =
    { old_content : Datas.t option;
      (* The content as it was back then.*)
      extra_children : Keys.t list;
      (* Which children to remove to get the old list. *)
      rev_prof : int;
      (* Auxiliary: how many deltas to here, counting form older to newer. *)
    }

type io_t =
  | Full of full
  | Delta of (Uid.t * Revision.t * delta)
  | RevDelta of Uid.t * Revision.t * rev_delta

type t = io_t

let max_full = 103
let max_delta =
  #<If:DEBUG_DB_MAX_DELTA$defined>
    begin
      match DebugVariables.debug_db_max_delta with
      | Some s when int_of_string s >= 0 ->
          int_of_string s
      | _ ->
          failwith "Bad value for debug variable DEBUG_DB_MAX_DELTA"
    end
    #<Else>
    103
    #<End>


(*******************)
(* Screen printing *)
(*******************)

let print_delta_bis delta =
  match delta.new_content with
  | Some c -> (Printf.sprintf "content=%s" (Datas.to_string c))
  | None -> (Printf.sprintf "content=%s" (Datas.to_string (Datas.empty)))

let print_map map = KeyMap.fold (
  fun k eid acc -> Printf.sprintf "%s%s -> %s "
    acc (Keys.to_string k) (Eid.to_string eid)
) map ""

let print_delta delta =
  Printf.sprintf "{new_content = %s, new_chilren = %s}"
    (match delta.new_content with
    | Some c -> Datas.to_string c
    | _ -> "None")
    (print_map delta.new_childs)

let print_rev_delta delta =
  Printf.sprintf "{old_content = %s, extra_chilren = %s}"
    (match delta.old_content with
    | Some c -> Datas.to_string c
    | _ -> "None")
    (BaseString.concat_map
       ~left:"[" "; " Keys.to_string delta.extra_children ~right:"]")

let print_full n =
  Printf.sprintf
    "rev=%s, content=%s, map=%s"
    (Revision.to_string n.cur_rev)
    (Datas.to_string n.content)
    (print_map n.map)

let to_string = function
  | Full node -> Printf.sprintf "Full {%s}" (print_full node)
  | Delta (uid, rev, delta) -> Printf.sprintf "Delta {uid = %d; rev = %d; %s}"
      (Uid.value uid) (Revision.value rev) (print_delta delta)
  | RevDelta (uid, rev, rev_delta) ->
      Printf.sprintf "RevDelta {uid = %d; rev = %d; %s}"
        (Uid.value uid) (Revision.value rev)
        (print_rev_delta rev_delta)


(************************)
(* Access to the fields *)
(************************)

let rec get_map ~f node =
  match node with
  | Full node -> node.map
  | Delta (uid, _, delta) ->
      let delta_map = delta.new_childs in
      let rec_map = get_map ~f (f uid) in
      (* TODO: probably not true:
         Subsequent revisions may overwrite the same children,
         so [KeyMap.safe_merge] is too strict here. *)
      KeyMap.merge (fun a _ -> a) delta_map rec_map
  | RevDelta (uid, _rev, delta) ->
      let rec_map = get_map ~f (f uid) in
      List.fold_left
        (fun acc k -> KeyMap.remove k acc)
        rec_map delta.extra_children

let rec get_children ~f = function
  | Full full -> KeyMap.fold (fun k _ acc -> k :: acc) full.map []
  | Delta (uid, _, delta) ->
      let delta_children =
        let map = delta.new_childs in
        KeyMap.fold (fun k _eid acc -> k :: acc) map [] in
      let rec_children = get_children ~f (f uid) in
      List.merge (fun k1 k2 -> compare k1 k2)
        delta_children rec_children
  | RevDelta (_uid, _rev, _delta) as node ->
      let map = get_map ~f node in
      KeyMap.fold (fun k _eid acc -> k :: acc) map []

let rec get_children_eid ~f = function
  | Full full -> KeyMap.fold (fun _ eid acc -> eid :: acc) full.map []
  | Delta (uid, _, delta) ->
      let delta_children =
        let map = delta.new_childs in
        KeyMap.fold (fun _k eid acc -> eid :: acc) map [] in
      let rec_children = get_children_eid ~f (f uid) in
      List.merge (fun eid1 eid2 -> compare eid1 eid2)
        delta_children rec_children
  | RevDelta (_uid, _rev, _delta) as node ->
      let map = get_map ~f node in
      KeyMap.fold (fun _k eid acc -> eid :: acc) map []

let rec get_content ~f = function
  | Full node -> node.content
  | Delta (uid, _, delta) -> (
      match delta.new_content with
      | Some d -> d
      | None -> get_content ~f (f uid)
    )
  | RevDelta (uid, _rev, delta) ->
      begin match delta.old_content with
      | Some d -> d
      | None -> get_content ~f (f uid)
      end

let get_cur_rev = function
  | Full node -> node.cur_rev
  | Delta (_, rev, _) -> rev
  | RevDelta (_uid, rev, _delta) -> rev

let rec next_eid ~f k node =
  #<If:DEBUG_DB$minlevel 1000>
    Logger.log ~color:`green "DB : next_eid node(%s) k(%s)"
      (to_string node) (Keys.to_string k)
  #<End>;
  match node with
  | Full node -> KeyMap.find k node.map
  | Delta (uid, _, delta) -> (
      match KeyMap.find_opt k delta.new_childs with
      | Some neid -> neid
      | _ -> next_eid ~f k (f uid)
    )
  | RevDelta (uid, _rev, _delta) -> next_eid ~f k (f uid)

let rec find_opt ~f k n =
  match n with
  | Full node -> KeyMap.find_opt k node.map
  | Delta (uid, _, delta) -> (
      match KeyMap.find_opt k delta.new_childs with
      | Some neid -> Some neid
      | _ -> find_opt ~f k (f uid)
    )
  | RevDelta (uid, _rev, delta) ->
      if List.mem k delta.extra_children then None
      else find_opt ~f k (f uid)


(************************)
(* Creation and updates *)
(************************)

let create ?content rev =
  let content =
    match content with
    | Some d -> d
    | _ -> Datas.empty
  in
  Full { cur_rev = rev
       ; content = content
       ; map = KeyMap.empty }

let is_full_map node =
  match node with
  | Full node -> (KeyMap.size node.map >= max_full)
  | Delta (_, _, delta) -> (KeyMap.size delta.new_childs >= max_delta)
  | RevDelta (_uid, _rev, delta) ->
      (List.length delta.extra_children >= max_delta)

let update_full_to_full ?content ?child _uid rev node =
  let new_map =
    match child with
    | Some (k, eid) ->
        KeyMap.add k eid node.map
    | _ -> node.map
  in
  let new_rev = rev in
  let new_content =
    match content with
    | Some d -> d
    | _ -> node.content
  in
  Full { cur_rev = new_rev
       ; content = new_content
       ; map = new_map}

let update_full_to_delta ?content ?child uid rev node =
  if max_delta = 0 then
    update_full_to_full ?content ?child uid rev node
  else
    let delta =
      let new_content = content in
      let new_childs =
        match child with
        | Some (k, eid) -> KeyMap.add k eid KeyMap.empty
        | _ -> KeyMap.empty
      in
      {new_content = new_content
      ; new_childs = new_childs
      ; prof = 1}
    in
    Delta (uid, rev, delta)

let update_full_to_rev_delta ?content ?child uid rev node =
  (* TODO *)
  if max_delta = 0 then
    update_full_to_full ?content ?child uid rev node
  else
    let delta =
      let new_content = content in
      let new_childs =
        match child with
        | Some (k, eid) -> KeyMap.add k eid KeyMap.empty
        | _ -> KeyMap.empty
      in
      {new_content = new_content
      ; new_childs = new_childs
      ; prof = 1}
    in
    Delta (uid, rev, delta)

(* delta: if node updated by a new transaction *)
let update_delta ~f ?content ?child uid rev old_uid old_rev old_delta delta =
  if delta then
    match old_delta.prof with
    | x when x >= max_delta ->
        let old_node = Delta (old_uid, old_rev, old_delta) in
        let map = get_map ~f old_node in
        let map =
          match child with
          | Some (k, eid) -> KeyMap.add k eid map
          | _ -> map
        in
        let new_content =
          match content with
          | Some d -> d
          | _ -> get_content ~f old_node
        in
        Full { cur_rev = rev
             ; content = new_content
             ; map = map }
    | _ ->
        let new_delta =
          let new_content =
            match content with
            | Some d -> Some d
            | _ -> None
          in
          let new_childs =
            match child with
            | Some (k, eid) -> KeyMap.add k eid KeyMap.empty
            | _ -> KeyMap.empty
          in
          {new_content = new_content
          ; new_childs = new_childs
          ; prof = succ old_delta.prof}
        in
        Delta (uid, rev, new_delta)
  else
    match old_delta.prof with
    | x when x >= max_delta ->
        let old_node = Delta (old_uid, old_rev, old_delta) in
        let map = get_map ~f old_node in
        let map =
          match child with
          | Some (k, eid) -> KeyMap.add k eid map
          | _ -> map
        in
        let new_content =
          match content with
          | Some d -> d
          | _ -> get_content ~f old_node
        in
        Full { cur_rev = old_rev
             ; content = new_content
             ; map = map }
    | _ ->
        let new_delta =
          let new_content =
            match content with
            | Some d -> Some d
            | _ -> old_delta.new_content
          in
          let new_childs =
            match child with
            | Some (k, eid) -> KeyMap.add k eid old_delta.new_childs
            | _ -> old_delta.new_childs
          in
          {new_content = new_content
          ; new_childs = new_childs
          ; prof = old_delta.prof} in
        Delta (old_uid, old_rev, new_delta)

let update ~f uid node rev ?content ?child delta =
  let new_node =
    match (node, delta) with
    | (Full node, true) ->
        begin match child with
        | Some _ ->
            update_full_to_rev_delta ?content ?child uid rev node
            (* Old version: update_full_to_delta ?content ?child uid rev node *)
        | None -> update_full_to_full ?content uid rev node
        end
    | (Full node, false) -> update_full_to_full ?content ?child uid rev node
    | (Delta (old_uid, old_rev, old_delta), _) ->
        update_delta ~f ?content ?child
          uid rev old_uid old_rev old_delta delta
    | (RevDelta (_uid, _rev, _delta), _) ->
        (* Normally, this node should never be updated,
           because the last revision will alsways be the Full node. *)
        assert false
  in
  (new_node, is_full_map new_node)

let rec remove_child ~f rev node key =
  match node with
  | Full node ->
      let new_map = KeyMap.remove key node.map in
      Full {node with
              map = new_map; cur_rev = rev}
  | Delta (uid, _rev, _delta) ->
      let new_map = KeyMap.remove key (get_map ~f node) in
      let content = get_content ~f node in
      let new_node = create ~content rev in
      let new_node = KeyMap.fold (
        fun key eid node ->
          let node, _ = update ~f uid node rev ~child:(key, eid) false in
          node
      ) new_map new_node
      in
      new_node
  | RevDelta (uid, _rev, delta) ->
      let delta = { delta with extra_children = key :: delta.extra_children } in
      RevDelta (uid, rev, delta)


(***********)
(* Folding *)
(***********)

let fold ~f foo node acc = KeyMap.fold foo (get_map ~f node) acc

let fold_range (start_opt, length) ~f foo node acc =
  let map = get_map ~f node in
  if KeyMap.is_empty map then acc else
    let start =
      match start_opt with
      | Some start -> start
      | None -> if length >= 0 then fst (KeyMap.min map) else fst (KeyMap.max map)
    in
    if length = 0 then
      KeyMap.fold_range foo map start (fst (KeyMap.max map)) acc
    else
      KeyMap.fold_length ~start ~length foo map acc


(****************************)
(* Disk writing and reading *)
(****************************)

external write : t -> io_t = "%identity"
external read : io_t -> t = "%identity"
