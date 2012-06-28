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
    @author1 Henri Binsztok,
    @author2 Gregoire Makridis
    @author3 Mikolaj Konarski
**)

(* Obtain a node from [node_opt], defaulting to search in [db]
   at path [path_start] and current revision. *)
let get_low_node node_opt db path_start =
  match node_opt with
  | Some node -> node
  | None ->
      let cur_rev = Hldb.get_rev db in
      let node, _ = Hldb.get_node_of_path db cur_rev path_start in
      node

(* Obtains a child at key [k] of a given [node]. For optimization.
   Depending on the scenario, the use of this optimization function
   may actually harm performance, but it's very cheap compared
   to finding child nodes by traversing the whole path from the start,
   which may be obligatory at some point if we don't pass around nodes
   calculated via this function. *)
let get_child db k node =
  let cur_rev = Hldb.get_rev db in
  try
    let neid = Node.next_eid ~f:(Hldb.get_node_of_uid db) k node in
    Hldb.get_node_of_eid db cur_rev neid
  with Not_found -> raise Hldb.UnqualifiedPath

(* Get the root node at a given revision.
   This is cheap and works OK even if root modified (unlikely). *)
let get_root db rev = Hldb.get_node_of_eid db rev Hldb.root_eid

(** Every node is either affected by a query (via operations
    on the query list for that node or via a remove list
    (if the node is outside the actual query map, this case is represented
    by an empty query list) or it is unaffected and then can be
    looked up in the low-level db.
*)
type node_or_query =
  | Node of Node.t
  | Query of QueryMap.query list

(* See the .mli.
   Note that a node with [UnsetData] will result in [Node _],
   unless it's just created by the query map itself. No exception
   is ever raised in this case, unlike for a truly nonexistent node. *)
let rec follow_path db remove_list node_opt path_start path_end map qlist =
  #<If:DEBUG_DB$minlevel 10>
    Logger.log ~color:`green "DB : high-level following path from %s; remaining: %s"
     (Path.to_string path_start)
     (Path.to_string (Path.of_list path_end))
  #<End>;
  match path_end with
  | [] ->
      (* The path ends while we are within query map;
         check if it's set by query map or removed by [remove_list]
         (in the latter case it's recreated as empty). *)
      if (Option.is_some (QueryMap.find_set_in_query_list qlist)
          || List.mem path_start remove_list) then
        ([], Query qlist)
      else
        (* Within, but not affected by query map,
           so take the node from low-level db. *)
        begin try
          let node = get_low_node node_opt db path_start in
          ([], Node node)
        with
        | Hldb.UnqualifiedPath ->
            (* Will be created. because within query map. *)
            ([], Query qlist)
        end
  | k :: rest ->
      match QueryMap.find_set_in_query_list qlist with
      | Some (Datas.Link _)
      | Some (Datas.Copy _) -> (path_end, Query qlist)
      | Some _ ->
          (* If there was any low-level link or copy, it's overwritten,
             so no links nor copies of any kind possible here. *)
          query_path db remove_list node_opt path_start k rest map
      | None ->
          (* Still within the query map, but no writes to this node,
             so underlying low-level db node can have a copy inside,
             unless the path is removed in [remove_list]. *)
          let node_opt =
            try
              if List.mem path_start remove_list then
                None
              else
                Some (get_low_node node_opt db path_start)
            with
            | Hldb.UnqualifiedPath -> None
          in
          match node_opt with
          | None ->
              (* Do not complain about non-existing node, because
                 we are still within the query map area, so empty nodes
                 will be created along the path. Continue with
                 the query map instead. *)
              query_path db remove_list None path_start k rest map
          | Some node ->
              match
                Node.get_content ~f:(Hldb.get_node_of_uid db) node
              with
              | Datas.Link _
              | Datas.Copy (None, _) ->
                  (* No low-level links possible here, because the query
                     would rather be at the link destination and here
                     we have a query somewhere lower in the tree. *)
                  assert false
              | Datas.Copy (Some _, _) ->
                  (path_end, Node node)
              | _ ->
                  (* For next key, consult the query map first, again. *)
                  query_path db remove_list node_opt path_start k rest map

(* [node_opt] is the node at [path_start], [map] is query map
   at [path_start]. But here we lookup and analyze the node at [path_new],
   that is the [k]-th son of [node_opt] and query list at that path
   and we follow [path_end]. *)
and query_path db remove_list node_opt path_start k path_end map =
  let path_new = Path.add path_start k in
  match KeyRecMap.find_opt k map with
  | Some (qlist, map) ->
      let next_node_opt =
        try
          Option.map (get_child db k) node_opt
        with
          (* Do not complain about non-existing node, will be created,
             because we are within the query map area. *)
        | Hldb.UnqualifiedPath -> None
      in
      follow_path db remove_list next_node_opt path_new path_end map qlist
  | None ->
      (* Query map finished, let's continue from the same node
         (at [path_new]), but at the low level. *)
      if List.mem path_new remove_list then
        (* Since the query map does not create empty nodes
           on the path any more, check removal and complain if removed. *)
        raise Hldb.UnqualifiedPath
      else
        let next_node =
          match node_opt with
          | None -> get_low_node None db path_new
          | Some node -> get_child db k node
        in
        let cur_rev = Hldb.get_rev db in
        let (path_end', node) =
          Hldb.follow_path db cur_rev next_node path_end
        in
        (path_end', Node node)

(** In [stop_at_copy] mode, copies are unwound and if any node along the path
    does no exist or a link or a copy is dangling, exception will be raised.
    In [stop_at_last_link] mode, if the last node is a link, we stop at it,
    instead of unwinding it. All other links are always unwound.
*)
type unwind_mode =
    { stop_at_copy : bool;
      stop_at_last_link : bool }

(* TODO: later: taint link nodes used in unwinding, at least for writes,
   and signal error at prepare if the nodes changed in other transactions.
*)
(* Implementation remark:
   This function unwinds both virtual and low-level links in [path].
   The mutual recursion between scanning for virtual links and copies
   from queries and low-level links and copies from db is needed,
   because when we unwind low-level links, we may get a path affected
   by virtual links, for which we must run again the virtual links elimination
   and then again the low-level scanning. It's simpler for copies,
   which are at a given revision and so ignore queries until
   you escape the copy via a link (back into the fully general problem).

   Invariant: queries say nothing about a path between the first occurrence
   of copy in the unwound version of the path and the first subsequent
   occurrence of link (because any db operation changing anything inside
   a copy has to rewrite and replace the copy node together with the whole
   path, until a link jumps out of the old revision).
*)
let rec follow_link ~unwind_mode db remove_list map path =
  try
    let path_end = Path.to_list path in
    let (path_end, node_or_query) =
      (* The query [map] corresponds to the root path,
         so the precondition of [follow_path] is satisfied.
         Passing [None] as the argument is optimal, because we avoid
         processing the nodes until they are needed (e.g. until
         we get out of query map) and they may never be. *)
      follow_path db remove_list None Path.root path_end map []
    in
    match node_or_query with
    | Query ql ->
        (* Affected by query map or tr_remove_list. *)
        begin match QueryMap.find_set_in_query_list ql with
        | Some (Datas.Link l)
        | Some (Datas.Copy (None, l)) ->
            if path_end = [] && unwind_mode.stop_at_last_link then
              (path, Query ql)
            else
              (* Links possible both on [l] and [path_end], hence [concat]. *)
              let new_path = Path.concat l (Path.of_list path_end) in
              follow_link ~unwind_mode db remove_list map new_path
        | Some (Datas.Copy (Some copy_rev, l)) ->
            let new_path = Path.to_list l @ path_end in
            let stopped = (path, Query ql) in
            (* Assumption, query map ends at this node. Otherwise the Invariant
               is not satisfied and only the [stop_at_copy] mode is correct.
               TODO: the assumption is not satisfied currently. When it is,
               express the assumption as an assertion. *)
            handle_copy
              ~unwind_mode db remove_list map new_path copy_rev stopped
        | Some _ ->
            (* If there was any low-level link or copy, it's overwritten,
               so no links nor copies of any kind possible at low level,
               so if we stopped, we must be at the end of the path. *)
            assert (path_end = []);
            (path, Query ql)
        | None ->
            (* No node provided, so the link/copy has to be in query map,
               but it's not. So we must be at the end of the path
               and the node does not exist or is removed. *)
            assert (path_end = []);
            (path, Query ql)
        end
    | Node node ->
        (* Not affected by query map nor [remove_list],
           but it's either the end of the path or it contains link/copy.
           Dive low-level, query map won't affect this path any more,
           unless a link is found later on. *)
        let cur_rev = Hldb.get_rev db in
        low_level ~unwind_mode db remove_list map path cur_rev node path_end
  with
  | Hldb.UnqualifiedPath ->
      if unwind_mode.stop_at_copy then
        (* If we stop at copies, we are in write rather than read mode,
           so we don't worry about nonexistent nodes and dangling links,
           because they will be soon created during the write. Hence we catch
           the exception and continue.
           The path can be a newly to-be-created path, not present
           in the low-level db nor in the query map (but soon will be).
           No point to look any further, there is nothing ahead
           (removing nodes removes their children, too, so we wouldn't know
           where to go next). The returned path is the one from
           the last recursive call. *)
        (path, Query [])
      else
        raise Hldb.UnqualifiedPath

and low_level ?stop_after_link
    ~unwind_mode db remove_list map global_path rev node_start rest =
  (* Out of the query map. If there's a low-level link or copy,
     it's active and has to be handled. *)
  let (path_end, node) =
    Hldb.follow_path db rev node_start rest
  in
  match Node.get_content ~f:(Hldb.get_node_of_uid db) node with
  | Datas.Link l
  | Datas.Copy (None, l) ->
      if Option.is_some stop_after_link then
        (* Links possible both on [l] and [path_end], hence [concat]. *)
        let new_path = Path.concat l (Path.of_list path_end) in
        (new_path, Query []) (* We are at root node, query list is empty. *)
      else if path_end = [] && unwind_mode.stop_at_last_link then
        (global_path, Node node)
      else
        let new_path = Path.concat l (Path.of_list path_end) in
        (* Escape to the new revision, affected by query map. *)
        follow_link ~unwind_mode db remove_list map new_path
  | Datas.Copy (Some copy_rev, l) ->
      let new_path = Path.to_list l @ path_end in
      let stopped = (global_path, Node node) in
      (* We are out of the query map, so the Invariant is satisfied here. *)
      handle_copy ~unwind_mode db remove_list map new_path copy_rev stopped
  | _ ->
      assert (path_end = []);
      (global_path, Node node)

and handle_copy ~unwind_mode db remove_list map new_path copy_rev stopped =
  if unwind_mode.stop_at_copy then
    (* Unwind the copy a bit further and see if we've encountered a link.
       If so, the link escapes from the copy, even though we otherwise
       don't want to unwind the copy node itself. The unwound path won't
       have any links, except perhaps the last one ([stop_at_last_link]). *)
    try
      let root = get_root db copy_rev in
      let (p, node_or_query) =
        let unwind_mode = { unwind_mode with stop_at_copy = false } in
        low_level ~stop_after_link:() ~unwind_mode
          db remove_list map (Path.of_list new_path) copy_rev root new_path
      in
      match node_or_query with
      | Query _ ->
          (* If we are within the query map again, then there must
             have been a link along the way. We escaped. *)
          follow_link ~unwind_mode db remove_list map p
      | Node _ ->
          (* No links found, so we do not want to enter the copy, after all. *)
          stopped
    with
    | Hldb.UnqualifiedPath ->
        (* Nonexistent node or dangling copy before the first link
           encountered. This means that no link escapes from the copy. *)
        stopped
  else
    let root = get_root db copy_rev in
    (* By Invariant: queries irrelevant, until a link escapes, if any. *)
    low_level
      ~unwind_mode db remove_list map
      (Path.of_list new_path) copy_rev root new_path
