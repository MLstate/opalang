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
(* depends *)

(* shorthands *)
module DT = DbTypes

(* debug *)
#<Debugvar:DEBUG_DB$minlevel 1>

(* -- *)

type t = {
  tr_num : int ;
  tr_db : Hldb.t ;
  (** the db the transaction refers to *)

  tr_query_map : QueryMap.t ;
  (** the map of queries against the db *)

  tr_remove_list : Path.t list ;
  (** the list of deleted paths in the transaction *)

  tr_index_set : (Path.t * DataImpl.t) list ;
  (* for search index update *)
  (** auxiliary index *)

  tr_index_remove : (Path.t * DataImpl.t) list ;
  (* for search index *)
  (** auxiliary index *)

  tr_read_only : bool * (Revision.t option) ;
  (* TODO: redo or use in get, for Badop.start_at_revision *)

  tr_op_counter : int ;
}

(*******************************)
(* transaction's fields access *)
(*******************************)

let get_num tr = tr.tr_num
let get_db tr = tr.tr_db
let get_query_map tr = tr.tr_query_map
let is_read_only tr = fst tr.tr_read_only
let get_read_rev tr = snd tr.tr_read_only

(*********************)
(* DB reading access *)
(*********************)

let full_search tr slist path =
  #<If>
  Logger.log ~color:`yellow "DB : full search for %s at %s"
    (BaseList.to_string (fun s -> s ^ " ") slist) (Path.to_string path)
  #<End>;
  Hldb.full_search tr.tr_db slist path

exception Data_not_found

let rec find_data_in_query_list = function
  | [] -> raise Data_not_found
  | QueryMap.Set (Datas.Data d) :: _ -> d
  | QueryMap.Set (Datas.Link _) :: _ -> assert false
  | QueryMap.Set (Datas.Copy (_, _)) :: _ -> assert false
  | _ :: tl -> find_data_in_query_list tl

exception Removed

(* Raises [Not_found] if data absent from query, [Removed] if removed. *)
let get_query_at tr path =
  let _, (query_list, map) =
    Path.fold (
      fun (p, (_, m)) k ->
        let p = Path.add p k in
        try p, KeyRecMap.find k m
        with Not_found ->
          if List.mem p tr.tr_remove_list then raise Removed
          else raise Not_found
    ) (Path.root, ([], tr.tr_query_map)) path
  in
  query_list, map

let rec find_set_data_in_query_list = function
  | [] -> None
  | QueryMap.Set (Datas.Data d) :: _ -> Some d
  | QueryMap.Set Datas.UnsetData :: _ -> Some DataImpl.empty
  | _ :: tl -> find_set_data_in_query_list tl

(* Shorthands for path unwinding, plus assertions that
   the resulting path is unwound fully. *)

let unwind_path_but_last tr path =
  (* Except for reading operations (see the next function),
     we don't unwind the last key, because we may want
     to overwrite, e.g., a link, instead of following it. *)
  let unwind_mode =
    { Unwind.stop_at_copy = true;
      Unwind.stop_at_last_link = true }
  in
  let (p, _node_or_query) =
    Unwind.follow_link ~unwind_mode
      tr.tr_db tr.tr_remove_list tr.tr_query_map path
  in
  #<If>
    assert (
      try
        let (p2, _) =
          Unwind.follow_link ~unwind_mode
            tr.tr_db tr.tr_remove_list tr.tr_query_map p
        in
        Path.compare p p2 = 0
      with Hldb.UnqualifiedPath -> false);
  #<End>;
  p

let unwind_path ~unwind_mode tr path =
  let (path, node_or_query) =
    Unwind.follow_link ~unwind_mode
      tr.tr_db tr.tr_remove_list tr.tr_query_map path
  in
  #<If>
    assert (
      try
        match node_or_query with
        | Unwind.Node _ -> true
        | Unwind.Query _ ->
            let (p2, _) =
              Unwind.follow_link ~unwind_mode
                tr.tr_db tr.tr_remove_list tr.tr_query_map path
            in
            Path.compare path p2 = 0
      with Hldb.UnqualifiedPath -> not unwind_mode.Unwind.stop_at_copy);
  #<End>;
  (path, node_or_query)

(* Main objective: copies have to be fully transparent.
   We run path unwinding in the [stop_at_copy:false] mode to report paths that
   end in a non-existing node (dangling Links at the end are OK, though,
   and the revision returned for them is None). *)
let stat tr path =
  let (_path_for_kind, node_or_query) =
    let unwind_mode =
      { Unwind.stop_at_copy = false;
        Unwind.stop_at_last_link = true }
    in
    unwind_path ~unwind_mode tr path
  in
  let content =
    match node_or_query with
    | Unwind.Node node ->
        Node.get_content ~f:(Hldb.get_node_of_uid tr.tr_db) node
    | Unwind.Query qlist ->
        match QueryMap.find_set_in_query_list qlist with
        | None -> Datas.UnsetData (* Query map creates this node. *)
        | Some c -> c
  in
  let kind =
    match content with
    | Datas.Data _ -> `Data
    | Datas.Link _ -> `Link
    | Datas.Copy _ -> assert false
    | Datas.UnsetData -> `Unset
  in
  (* Below we use [stop_at_copy], because, for transparency
     of copies, we want to return the revision at which a copy was created,
     not the revision of the original nodes. *)
  let (returned_path, node_or_query) =
    let unwind_mode =
      { Unwind.stop_at_copy = true;
        Unwind.stop_at_last_link = false }
    in
    unwind_path ~unwind_mode tr path
  in
  let rev_opt =
    match node_or_query with
    | Unwind.Query _ -> None
    | Unwind.Node node -> Some (Node.get_cur_rev node)
  in
  (returned_path, rev_opt, kind)

let get tr path =
  #<If>
    Logger.log ~color:`yellow "DB : get data at %s" (Path.to_string path)
  #<End>;
  if fst tr.tr_read_only then
    let cur_rev = Hldb.get_rev tr.tr_db in
    let rev = Option.default cur_rev (snd tr.tr_read_only) in
    let (_path, node) = Hldb.follow_link tr.tr_db rev path in
    Hldb.get_data tr.tr_db node
    (* TODO: make sure we don't want to get value from new revision
       if we escape from [rev] via a link. In other words,
       does [tr_read_only] mean we want value as it was at that version
       of the db, or as it is in current db, but starting unwinding
       at the given revision? The second is the behaviour of Copy,
       so I assume for now [tr_read_only] is different and simpler. *)
  else
    let (_path, node_or_query) =
      (* Complain if the path does not exist. *)
      let unwind_mode =
        { Unwind.stop_at_copy = false;
          Unwind.stop_at_last_link = false }
      in
      unwind_path ~unwind_mode tr path
    in
    match node_or_query with
    | Unwind.Node node ->
        Hldb.get_data tr.tr_db node
    | Unwind.Query qlist ->
        try
          find_data_in_query_list qlist
        with
        | Data_not_found ->
            (* No data, but will be created. because within query map. *)
            DataImpl.empty

(* may raise Removed and Not_found *)
let virtual_get_children tr path rev =
  let _, map = get_query_at tr path in
  KeyRecMap.fold_rev (fun k _ acc -> (Path.add path k, rev)::acc) map []

let get_children tr rev range path =
  #<If>
    Logger.log ~color:`yellow "DB : get children at %s for rev %s"
     (Path.to_string path) (Revision.to_string rev)
  #<End>;
  try
    let virtual_children = virtual_get_children tr path rev in
    let virtual_children = List.sort compare virtual_children in
    try
      let real_children = Hldb.get_children tr.tr_db rev None path in
      let real_children = List.filter
        (fun (p,_) -> not (List.mem p tr.tr_remove_list)) real_children
      in
      let l =
        BaseList.uniq (List.merge (fun (p1, _) (p2, _) -> Path.compare p1 p2)
                         virtual_children real_children)
      in
      (* If we got there, it's the 1% of cases where
         the queried children are affected by the current transaction. *)
      BaseList.filterbounds range (fun (p, _) -> Path.last p) l
    with Hldb.UnqualifiedPath ->
      (* That's even more rare --- the father node has just been created. *)
      BaseList.filterbounds range (fun (p, _) -> Path.last p) virtual_children
  with
  | Removed -> []
  | Not_found ->
      (* Common case: no children added nor removed in current transaction. *)
      Hldb.get_children tr.tr_db rev (Some range) path

let get_all_rev_of_path tr path =
  #<If>
    Logger.log ~color:`yellow "DB : get all rev of %s" (Path.to_string path)
  #<End>;
  Hldb.get_all_rev_of_path tr.tr_db path

let get_last_rev_of_path tr path =
  #<If>
    Logger.log ~color:`yellow "DB : get last rev of %s" (Path.to_string path)
  #<End>;
  let db = tr.tr_db in
  let rev = Hldb.get_rev db in
  Hldb.get_last_rev_of_path db rev path

let trans_operation_counter_limit =
  #<If:DB3_TRANSACTION_LIMIT$minlevel 0>
    int_of_string (Option.get DebugVariables.db3_transaction_limit)
  #<Else> 0 #<End>

let add_to_query_map =
  let do_it tr path query =
    { tr with tr_query_map = QueryMap.add_to_query_map tr.tr_query_map path query }
  in
  (* enable the transaction count limit (plus some checks) if requested *)
  if trans_operation_counter_limit == 0 then do_it
  else
    fun tr path query ->
      (* A crucial invariant, needed for correct conflict detection.
         It's so complex, because we normally don't unwind the last link
         for writes (except when doing [Set] with a normal value, but this is
         not captured in the assertion below). *)
      #<If>
        assert (
          try
            let p = unwind_path_but_last tr path in
            Path.compare p path = 0
          with Hldb.UnqualifiedPath -> false
        )
      #<End>;
      let counter = tr.tr_op_counter in
      if counter < trans_operation_counter_limit then do_it tr path query
      else if counter = -1 then begin
        #<If>
          Logger.log ~color:`yellow "DB : operation count still exceeded for transaction #%d"
             tr.tr_num
        #<End>;
        tr
      end else begin
        Logger.warning "DB: transaction exceeded maximum number of operations (%d), aborting"
          trans_operation_counter_limit;
        #<If>
          Logger.log ~color:`red "DB : operation count now exceeded for transaction #%d"
            tr.tr_num
          #<End>;
        { tr with
            (* TODO: possible optimization, but empty transactions cannot be aborted!
               tr_query_map = KeyMap.empty; *)
            tr_op_counter = -1 }
      end

(* Lazily physically copy to the query map the content of nodes
   inside copies along the path, making all remaining children on the path
   copies pointing the children of the original nodes.
   The path is first unwound so that no links remain, except perhaps
   the last one, depending on [unwind_mode].  *)
let unshare_query_copy tr path unwind_mode =
  #<If:DEBUG_DB$minlevel 500>
    Logger.log ~color:`yellow "DB : unshare_query_copy at %s" (Path.to_string path)
  #<End>;
  assert (unwind_mode.Unwind.stop_at_copy);
  (* We don't modify the db nor the remove list, only the query map,
     regardless if the copy is in the query map or in a db node. *)
  let db = tr.tr_db in
  let remove_list = tr.tr_remove_list in
  let (p2, node_or_query) = unwind_path ~unwind_mode tr path in
  (* At this point there are no links on [p2], except possibly the last one.
     This means we can avoid quadratic cost by not calling linear
     [unwind_path] again but just descent the path key by key.
     There can still be nested copies, so the cost is linear not wrt
     to the length of [path], but to the sum of paths
     in all nested copies on [path]. We can't do any better, since we
     unwind the paths inside the copy data constructors as late as possible,
     so up to now they are not yet unwound.
     The cheap call to [follow_path] below serves to get both parts of [p2]. *)
  let get_paths p2 =
    (* TODO: optimization: let follow_path return [path_start], map, etc. *)
    let (path_end, _node_or_query) =
      Unwind.follow_path
        db remove_list None Path.root (Path.to_list p2) tr.tr_query_map []
    in
    let path_start_opt = Path.remaining_prefix p2 (Path.of_list path_end) in
    let path_start = Option.get path_start_opt in (* [p2] is unwound *)
    let parent, key =
      match Path.pop_last path_start with
      | None -> assert false
      | Some (k, p) -> p, k
    in
    (parent, key, path_end)
  in
  let rec aux_copy copy_rev l map_start key path_end =
    #<If:DEBUG_DB$minlevel 100>
      Logger.log ~color:`yellow "DB : aux_copy_unshare_query_copy; remaining %s"
        (Path.to_string (Path.of_list path_end))
    #<End>;
    let root_key = Path.add Path.root key in
    let (l, map_start) =
      try
        let (l, copy_data, copy_node) =
          (* No links in [l] (because we escaped from all links inside copies
             on the original [path], which involved the copy [l] is taken from),
             so we can unwind all copies in the old copy revision.
             So [copy_node] is neither link nor copy (asserted in [get_data]).
             TODO: check if this is efficient; we trade unwinding
             copies now for not having to unwind copies later;
             an alternative is to copy over the nodes without unwinding them,
             even if they are themselves copy nodes. *)
          let (l, node) = Hldb.follow_link tr.tr_db copy_rev l in
          let data = Hldb.get_data tr.tr_db node in
          (l, data, node)
        in
        let query = QueryMap.Set (Datas.Data copy_data) in
        let map_start = QueryMap.add_to_query_map map_start root_key query in
        let copy_child k _eid map_start =
          (* TODO: optimize: skip k that is on the path, it will be
             overwritten immediately, anyway. *)
          let path_k = Path.add l k in
          let content = Datas.Copy (Some copy_rev, path_k) in
          let query = QueryMap.Set content in
          let root_key_k = Path.add root_key k in
          QueryMap.add_to_query_map map_start root_key_k query
        in
        (l,
         Node.fold ~f:(Hldb.get_node_of_uid db) copy_child copy_node map_start)
      with
      | Hldb.UnqualifiedPath ->
          (* Don't create copies for children, since we are already
             outside the target subtree of the copy. *)
          (* TODO: optimization: do this also when overwriting a copy node
             with a copy or link node or when removing (only Set(data)
             needs copies in its children). *)
          let query = QueryMap.Set Datas.UnsetData in
          (l, QueryMap.add_to_query_map map_start root_key query)
    in
    match path_end with
    | [] ->
        map_start
    | k :: rest ->
        let l_new = Path.add l k in
        let (qlist_new, map_new) =
          match KeyRecMap.find_opt key map_start with
          | Some (qlist_new, map_new) ->
              let map_new = aux_copy copy_rev l_new map_new k rest in
              (qlist_new, map_new)
          | None ->
              let map_new = aux_copy copy_rev l_new KeyRecMap.empty k rest in
              ([], map_new)
        in
        KeyRecMap.add key (qlist_new, map_new) map_start
  in
  let tr =
    match node_or_query with
    | Unwind.Query ql ->
        begin match QueryMap.find_set_in_query_list ql with
        | Some (Datas.Copy (Some copy_rev, l)) ->
            let (parent, key, path_end) = get_paths p2 in
            let (_, map_start) =
              try
                get_query_at tr parent
              with
              | Not_found | Removed -> ([], KeyRecMap.empty)
            in
            (* The call is nontrivial even if [path_end = []]. *)
            let map_start = aux_copy copy_rev l map_start key path_end in
            let map =
              QueryMap.overwrite_in_query_map tr.tr_query_map parent map_start
            in
            { tr with tr_query_map = map }
        | _ -> tr
        end
    | Unwind.Node node ->
        match Node.get_content ~f:(Hldb.get_node_of_uid db) node with
        | Datas.Copy (Some copy_rev, l) ->
            (* TODO: copy-pasted code below. *)
            let (parent, key, path_end) = get_paths p2 in
            let (_, map_start) =
              try
                get_query_at tr parent
              with
              | Not_found | Removed -> ([], KeyRecMap.empty)
            in
            (* The call is nontrivial even if [path_end = []]. *)
            let map_start = aux_copy copy_rev l map_start key path_end in
            let map =
              QueryMap.overwrite_in_query_map tr.tr_query_map parent map_start
            in
            { tr with tr_query_map = map }
        | _ -> tr
  in
  #<If:DEBUG_DB$minlevel 100>
    Logger.log ~color:`yellow "DB : finish of unshare_query_copy at %s"
      (Path.to_string p2)
  #<End>;
  (tr, p2)

(* Removes all paths with prefix [path] from list [l],
   including [path] itself, if present. *)
let rm_all_with_prefix path l =
  let f p =
    Path.size p < Path.size path
    || Option.is_none (Path.remaining p path)
  in
  List.filter f l

let set_link tr path link =
  #<If>
    Logger.log ~color:`yellow "DB : set link at %s toward %s"
      (Path.to_string path) (Path.to_string link)
  #<End>;
  let unwind_mode =
    { Unwind.stop_at_copy = true;
      Unwind.stop_at_last_link = true }
  in
  let (tr, path) = unshare_query_copy tr path unwind_mode in
  (* Note: do not unwind [link], because it's supposed to unwind
     differently as the db changes (both within the same transaction
     and later on). *)
  let query = QueryMap.Set (Datas.Link link) in
  (* This removes additions to the whole subtree,
     because link node makes them all inaccessible. *)
  let new_map = QueryMap.remove_from_query_map tr.tr_query_map path in
  let tr = { tr with tr_query_map = new_map } in
  let tr = add_to_query_map tr path query in
  (* We don't have to remove anything below [path],
     so we remove all paths that begin with [path] from remove list. *)
  { tr with tr_remove_list = rm_all_with_prefix path tr.tr_remove_list }

(* The check if [target_path] is dangling in [target_rev]
   is done much later, in [execute_query_list], because if the copy
   is of a future revision (target_rev = None) then we don't know yet
   if the path is dangling or not. *)
let set_copy tr path (target_path, target_rev) =
  #<If>
    let tr_rev = Hldb.get_rev tr.tr_db in
    Logger.log ~color:`yellow "DB : set copy at %s toward %s for %s"
      (Path.to_string path) (Path.to_string target_path)
      (if Some tr_rev = target_rev then "new revision"
       else Printf.sprintf "rev %s"
         (Option.to_string Revision.to_string target_rev))
  #<End>;
  let unwind_mode =
    { Unwind.stop_at_copy = true;
      Unwind.stop_at_last_link = true }
  in
  let (tr, path) = unshare_query_copy tr path unwind_mode in
  let query = QueryMap.Set (Datas.Copy (target_rev, target_path)) in
  (* This removes additions to the whole subtree,
     because copy node makes them all inaccessible. *)
  let new_map = QueryMap.remove_from_query_map tr.tr_query_map path in
  let tr = { tr with tr_query_map = new_map } in
  let tr = add_to_query_map tr path query in
  (* We don't have to remove anything below [path],
     so we remove all paths that begin with [path] from remove list. *)
  { tr with tr_remove_list = rm_all_with_prefix path tr.tr_remove_list }

let set tr path data =
  #<If>
    Logger.log ~color:`yellow "DB : set %s at %s"
     (DataImpl.to_string data) (Path.to_string path)
  #<End>;
  let unwind_mode =
    { Unwind.stop_at_copy = true;
      Unwind.stop_at_last_link = false }
  in
  let (tr, path) = unshare_query_copy tr path unwind_mode in
  let query = QueryMap.Set (Datas.Data data) in
  let tr = add_to_query_map tr path query in
  (* Don't change the remove list, because if we remove a subtree
     and then set data in a node anywhere inside the tree,
     the result is not the same as when we don't remove at all. *)
  tr

(* This optimization is very hard to do correctly (copies),
   but needed to adhere to the "much work before prepare, little afterwards"
   principle. As long as [unshare_query_copy] is correct and called beforehand,
   the current form is probably correct.
   Anyway, disabling the optimization triggers bugs in transaction
   conflict detection, so it just has to stay. *)
let check_remove tr path =
  try
    ignore (Hldb.get tr.tr_db (Hldb.get_rev tr.tr_db) path);
    true
  with
  | Hldb.UnqualifiedPath ->
      (* Not in the current db, let's try the query map of [tr]. *)
      try
        ignore (get_query_at tr path);
        true
      with
      | Not_found | Removed ->  false

(* This operation removes a node from the database,
   making its subtree unreachable. The tree won't be ever reachable again.
   However, the operation [remove] below does not wipe up the subtree
   in the db, only in the query map (wiping up db would be too slow
   and the nodes have to remain there anyway, for history).
   Following a path with a removed node inside will result
   in [UnqualifiedPath] exception, but writing to such a path is OK.
   After writing, the path can be followed again, but not beyond
   the written nodes, because links to the old children are lost.
   There is no direct record in history at which revision a given node
   was removed, but you can trace through his ancestors to see at which
   revision it became inaccessible (=removed). *)
let remove tr path =
  #<If>
  Logger.log ~color:`yellow "DB : remove at %s" (Path.to_string path)
  #<End>;
  (* Don't unwind the last key for removing;
     otherwise we can't remove links nor copies. *)
  let unwind_mode =
    { Unwind.stop_at_copy = true;
      Unwind.stop_at_last_link = true }
  in
  let (tr, path) = unshare_query_copy tr path unwind_mode in
  let parent, key =
    match Path.pop_last path with
    | None -> assert false
    | Some (k, p) -> p, k
  in
  if not (check_remove tr path) then tr
  else
    let tr = add_to_query_map tr parent (QueryMap.Remove key) in
    (* This removes additions to the whole subtree,
       because it won't be accessible any more, so no point. *)
    let new_map = QueryMap.remove_from_query_map tr.tr_query_map path in
    (* We don't have to remove anything below [path],
       so we remove all paths that begin with [path] from remove list.
       This also takes care of repetitions on the remove list. *)
    { tr with
        tr_query_map = new_map;
        tr_remove_list = path :: rm_all_with_prefix path tr.tr_remove_list }

(* initialization of transactions *)

let init db ?read_only i =
  let read_only = match read_only with
  | Some r -> r
  | _ -> (false, None) in
  { tr_num = i
  ; tr_db = db
  ; tr_query_map = KeyRecMap.empty
  ; tr_remove_list = []
  ; tr_index_set = []
  ; tr_index_remove = []
  ; tr_read_only = read_only
  ; tr_op_counter = 0
  }


(*******************************)
(* le commit de la transaction *)
(*******************************)

let update_uid_list l eid uid =
  if List.mem_assoc eid l
  then (eid, uid) :: (List.remove_assoc eid l)
  else (eid, uid) :: l

let update_node_list l uid node =
  if List.mem_assoc uid l
  then (uid, node) :: (List.remove_assoc uid l)
  else (uid, node) :: l

let update_node old_uid old_node db rev q_option ?child replace =
  let delta = not replace in
  match q_option with
  | None ->
      begin match child with
      | Some (_, _) ->
          let node, b =
            Node.update
              ~f:(Hldb.get_node_of_uid db) old_uid old_node rev ?child delta
          in
          node, b
      | _ ->
          old_node, false
      end
  | Some d ->
      Node.update
        ~f:(Hldb.get_node_of_uid db) old_uid old_node rev
        ~content:(Datas.Data d) ?child delta

(* TODO: when unsharing copies works OK, this function does not need
   to write anyting to the db, except via the index. Copies and links
   should be written in execute_query_map, just as normal data. *)
let execute_query_list tr db rev path l =
  let cur_rev = Hldb.get_rev tr.tr_db in
  List.fold_left (
    fun (tr, db) query -> match query with
    | QueryMap.Set (Datas.Data d) ->
        let tr_index_set = (path, d) :: tr.tr_index_set in
        let tr_index_remove =
          try
            let d = Hldb.get tr.tr_db cur_rev path in
            (path, d) :: tr.tr_index_remove
          with Not_found | Hldb.UnqualifiedPath -> tr.tr_index_remove
        in
        { tr with tr_index_set = tr_index_set
            ; tr_index_remove = tr_index_remove
        }, db
    | QueryMap.Set (Datas.Link link) ->
        let db = Hldb.set_link db rev path link in
        let tr_index_set =
          (* Search index has to be updated with that, because
             the value is now available at a new path
             (and perhaps it won't be available at the old, soon).
             Warning: this is not accurate, because we may set up
             a dangling link and create the node later (in the same
             transaction or a later one). This won't be recorded
             in index, but the original path will, so the value
             is not completely lost. *)
          try
            let d = get tr link in
            (path, d) :: tr.tr_index_set
          with Not_found | Hldb.UnqualifiedPath -> tr.tr_index_set in
        let tr_index_remove =
          try
            let d = Hldb.get tr.tr_db cur_rev path in
            (path, d) :: tr.tr_index_remove
          with Not_found | Hldb.UnqualifiedPath -> tr.tr_index_remove in
        let tr = {tr with tr_index_set = tr_index_set
                    ; tr_index_remove = tr_index_remove} in
        tr, db
    | QueryMap.Set (Datas.Copy (r, link)) ->
        let r = Option.default rev r in
        (* In the following line we also check that [link] is not dangling. *)
        let db = Hldb.set_copy db rev path ~copy_rev:r link in
        let tr_index_set =
          try
            let gather_datas db rev path =
              let rec aux acc rev path =
                let d = Hldb.get db rev path in
                (* TODO: this is terribly slow (inspects the whole copy)
                   and loops if copy point upward.
                let l = Hldb.get_children db rev None path in
                *)
                let l = [] in
                List.fold_left (
                  fun acc (path, rev) ->
                    aux acc rev path
                ) ((path, d) :: acc) l
              in aux [] rev path
            in
            let l = gather_datas tr.tr_db r link in
            List.fold_left (
              fun acc (p, d) ->
                (p, d) :: acc
            ) tr.tr_index_set l
          with Not_found | Hldb.UnqualifiedPath -> tr.tr_index_set in
        let tr_index_remove =
          try
            let d = Hldb.get tr.tr_db cur_rev path in
            (path, d) :: tr.tr_index_remove
          with Not_found | Hldb.UnqualifiedPath -> tr.tr_index_remove in
        let tr = {tr with tr_index_set = tr_index_set
                    ; tr_index_remove = tr_index_remove} in
        tr, db
    | _ -> tr, db
  ) (tr, db) l

let execute_query_map tr db =
  #<If>
    Logger.log ~color:`yellow "%s" (QueryMap.print_query_map tr.tr_query_map)
  #<End>;
  let rev = Hldb.get_rev db in
  let f_acc q_option pos uid nuid
      (acc1, acc2, acc3, node, replace, neid, next_uid) k =
    if (k = Keys.newkey) then
      (acc1, acc2, acc3, node, replace, neid, next_uid)
    else
      match Node.find_opt ~f:(Hldb.get_node_of_uid db) k node with
      | Some eid ->
          let new_fold_list = (k, eid) :: acc3 in
          acc1, acc2, new_fold_list, node, replace, neid, next_uid
      | _ ->
          let replace = replace || List.mem_assoc nuid acc2 in (* TODO: ? *)
          let new_node, is_full =
            update_node uid node db rev q_option ~child:(k, neid) replace
          in
          (* TODO: the following two are probably redundant or can be
             moved outside of the loop *)
          let new_uid_list = update_uid_list acc1 pos nuid in
          let new_node_list = update_node_list acc2 nuid new_node in
          let new_uid_list = update_uid_list new_uid_list neid next_uid in
          let new_node_list =
            update_node_list new_node_list next_uid (Node.create rev)
          in
          let new_fold_list = (k, neid) :: acc3 in
          let next_eid = Eid.succ neid in
          let next_uid = Uid.succ next_uid in
          new_uid_list, new_node_list, new_fold_list,
          new_node, (replace && not is_full), next_eid, next_uid
  in
  let rec aux tr db pos qlist map path =
    #<If:DEBUG_DB$minlevel 500>
      Logger.log ~color:`yellow "DB : start of execute_query_map_aux [%s] at %s"
         (BaseString.concat_map ";" QueryMap.print_query qlist)
         (Path.to_string path)
      #<End>;
    if KeyRecMap.is_empty map && qlist = [] then
      (* Nothing to do here and no modifications to children. Bail out.*)
      (tr, db)
    else
      let old_uid = Hldb.get_uid_of_eid db rev pos in
      let old_node = Hldb.get_node_of_uid db old_uid in
      let content =
        Node.get_content ~f:(Hldb.get_node_of_uid db) old_node
      in
      let tr, db = execute_query_list tr db rev path qlist in
      let q_option = find_set_data_in_query_list qlist in
      if KeyRecMap.is_empty map && q_option = None then
        (* Nothing more to do here and no modifications to children. *)
        (tr, db)
      else
        let key_list = List.sort compare (KeyRecMap.keys map) in
        #<If:DEBUG_DB$minlevel 10>
          Logger.log ~color:`yellow "DB : execute_query_map_aux [%s] over %s at %s"
            (BaseString.concat_map ";" QueryMap.print_query qlist)
            (Datas.to_string content)
            (Path.to_string path)
          #<End>;
        match key_list, q_option, content with
        | [], None, _ ->
            (* Already checked. *)
            assert false
        | _, _, Datas.Copy (None, _) -> (* never written to the db *)
            assert false
        | _, None, Datas.Copy (Some copy_rev, _)
            when copy_rev <> rev ->
            (* Content can be a copy, but only when it's is overwritten
               in query map, via [unshare_query_copy]. *)
            assert false
        | _, _, Datas.Link _l ->
            (* The link has just been added. Any other operations for this
               path will be carried out on the unwound path, so they are
               on the query map elsewhere. Case impossible. *)
            assert false
        | _, _, Datas.Copy (Some copy_rev, _l)
            when copy_rev = rev ->
            (* Copy to the new revision --- link. *)
            assert false
        | [], Some _, _ ->
        let replace = Hldb.is_new_uid db old_uid in
        let new_node, is_full =
          update_node old_uid old_node db rev q_option replace
        in
        let uid_list, node_list =
          if (new_node = old_node) then [], []
          else
            let new_uid =
              if (replace && not is_full)
              then old_uid else Hldb.get_next_uid db in
            let new_uid_list = update_uid_list [] pos new_uid in
            let new_node_list =
              update_node_list [] new_uid new_node
            in
            new_uid_list, new_node_list
        in
        let db =
          match (uid_list, node_list) with
          | ([], []) -> db
          | _ -> Hldb.update_db db rev uid_list node_list
        in
        (tr, db)
    | _ ->
        let replace = Hldb.is_new_uid db old_uid in
        let nuid = Hldb.get_next_uid db in
        let next_uid, next_next_uid =
          if replace then old_uid, nuid
          else nuid, Uid.succ nuid
        in
        let uid_list, node_list, fold_list =
          let tcount = Eid.succ (Hldb.get_tcount db) in
          let uid_list, node_list, fold_list, _, _, _, _ =
            List.fold_left
              (f_acc q_option pos old_uid next_uid)
              ([], [], [],
               old_node, replace, tcount, next_next_uid)
              key_list
          in
          uid_list, node_list, fold_list
        in
        let db =
          match (uid_list, node_list) with
          | ([], []) -> db
          | _ -> Hldb.update_db db rev uid_list node_list
        in
        let sorted_fold_list =
          List.sort (fun (k1, _) (k2, _) -> Keys.compare k1 k2) fold_list
        in
        List.fold_left
          (fun (tr, db) (key, eid) ->
             match key with
             | k when k = Keys.newkey -> tr, db
             | _ ->
                 let new_qlist, new_map = KeyRecMap.find key map in
                 aux tr db eid new_qlist new_map (Path.add path key))
          (tr, db) sorted_fold_list
  in
  try
    aux tr db Hldb.root_eid [] tr.tr_query_map Path.root
  with
  | e ->
      #<If> Logger.log ~color:`red "Commit --> %s\n%s" (Printexc.to_string e) (Printexc.get_backtrace ())#<End>;
      raise e

let execute_remove_list tr rev db =
  match tr.tr_remove_list with
  | [] -> tr, db
  | l ->
      let l = List.sort (Path.compare) l in
      List.fold_left
        (fun (tr, db) path ->
           match Path.pop_last path with
           | None -> assert false
           | Some (k, p) ->
               let l =
                 try Hldb.get_descendants db path
                 with Hldb.UnqualifiedPath -> []
               in
               let remove_list = (List.rev l) @ tr.tr_index_remove in
               let tr = {tr with tr_index_remove = remove_list} in
               let db =
                 try Hldb.remove db rev p k
                 with Hldb.UnqualifiedPath -> db
               in
               tr, db)
        (tr, db) l

let modified tr =
  not (KeyRecMap.is_empty tr.tr_query_map && (tr.tr_remove_list = []))

let commit rev tr db =
  if modified tr then begin
    if tr.tr_op_counter = -1 then
      (* Too big query map, won't merge with any other. *)
      raise Hldb.Merge;
    let db = Hldb.set_rev db rev in
    (* Execute in the low-level db all remove requests. *)
    let tr, db = execute_remove_list tr rev db in
    (* Execute the writes that survived subsequent removals, etc. *)
    let tr, db = execute_query_map tr db in
    let db =
      match tr.tr_index_remove with
      | [] -> db
      | l -> Hldb.remove_from_index db l
    in
    let db =
      match tr.tr_index_set with
      | [] -> db
      | l -> Hldb.update_index db l
    in
    db
  end else
    db

(*************)
(* le disque *)
(*************)

let append_disk tr =
  if modified tr then
    Some { DT.
      querymap = tr.tr_query_map ;
      read_only = is_read_only tr ;
      remove_list = tr.tr_remove_list ;
    }
  else None

let apply_disk tr db rev =
  let tr_query_map = tr.DT.querymap in
  let tr_read_only = tr.DT.read_only, None in
  let tr_remove_list = tr.DT.remove_list in
  let tr = {
      tr_num = 0;
      tr_db = db;
      tr_query_map;
      tr_remove_list;
      tr_index_set = [];
      tr_index_remove = [];
      tr_read_only;
      tr_op_counter = 0;
    } in
  let rev = Revision.succ rev in
  let db = commit rev tr db in
  let db = Hldb.clean_tmp_maps db in
  (Hldb.get_rev db), db
