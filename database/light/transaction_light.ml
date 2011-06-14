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
module List = BaseList
let sprintf = Printf.sprintf
let eprintf = Printf.eprintf

(* debug *)
#<Debugvar:DEBUG_DB>

(* -- *)

type query =
  | Set of Datas.t
  | Remove of Path.t

let string_of_query = function
  | Set d -> sprintf "set %s" (Datas.to_string d)
  | Remove k -> sprintf "remove %s" (Path.to_string k)

let string_of_query_list list =
  List.to_string (fun q -> sprintf "%s " (string_of_query q)) list

let string_of_query_map qm =
  List.to_string (fun (p,q) -> sprintf "(%s,%s); " (Path.to_string p) (string_of_query q)) qm

type t = {
  tr_num : int ;
  tr_db : Db_light.t ;
  (** the db the transaction refers to *)

  tr_query_map : (Path.t * query) list;
  (** the map of queries against the db *)

  tr_remove_list : Path.t list ;
  (** the list of deleted paths in the transaction *)

  tr_index_set : (Path.t * DataImpl.t) list ;
  (* for search index update *)
  (** auxiliary index *)

  tr_index_remove : (Path.t * DataImpl.t) list ;
  (* for search index *)
  (** auxiliary index *)

  tr_op_counter : int ;
}

(*******************************)
(* transaction's fields access *)
(*******************************)

let get_num tr = tr.tr_num
let get_db tr = tr.tr_db
let get_query_map tr = tr.tr_query_map

(*********************)
(* DB reading access *)
(*********************)

let full_search tr slist path =
  #<If>
  Logger.log ~color:`yellow "DB-LIGHT : full search for %s at %s"
                 (BaseList.to_string (fun s -> sprintf "%s "s)slist)
                         (Path.to_string path)
  #<End>;
  Db_light.full_search tr.tr_db slist path

exception Data_not_found

let rec find_data_in_query_list = function
  | [] -> raise Data_not_found
  | Set (Datas.Data d) :: _ -> d
  | Set (Datas.Link _) :: _ -> assert false
  | Set (Datas.Copy (_, _)) :: _ -> assert false
  | _ :: tl -> find_data_in_query_list tl

exception Removed

(* Raises [Not_found] if data absent from query, [Removed] if removed. *)
let get_query_at tr path =
  let query_list =
    List.fold_left
      (fun query_list (p,q) ->
         if Path.compare path p = 0
         then (p,q)::query_list
         else query_list) [] tr.tr_query_map
  in
  if query_list = []
  then
    if List.mem path tr.tr_remove_list
    then raise Removed
    else raise Not_found;
  query_list

let rec find_set_data_in_query_list = function
  | [] -> None
  | Set (Datas.Data d) :: _ -> Some d
  | Set (Datas.UnsetData) :: _ -> Some DataImpl.empty
  | _ :: tl -> find_set_data_in_query_list tl

(* Main objective: copies have to be fully transparent.
   We run path unwinding in the [stop_at_copy:false] mode to report paths that
   end in a non-existing node (dangling Links at the end are OK, though,
   and the revision returned for them is None). *)
let stat tr path =
  let path, kind =
    let rec aux path =
      let (node, _) = Db_light.get_node_of_path tr.tr_db path in
      match Node_light.get_content node with
      | Datas.Data _ -> path, `Data
      | Datas.Link p -> p, `Link
      | Datas.Copy (_, p) -> aux p
      | Datas.UnsetData -> path, `Unset
    in
    aux path
  in
  (path, Some (Revision.make 0), kind)

let get tr path =
  #<If>Logger.log ~color:`yellow "DB-LIGHT : get data at %s" (Path.to_string path)#<End>;
    let (_path, node) = Db_light.follow_link tr.tr_db path in
    Db_light.get_data tr.tr_db (Db_light.node_node node)

(* may raise Removed and Not_found *)
let virtual_get_children tr path = List.map fst (get_query_at tr path)

let get_children tr range path =
  #<If>Logger.log ~color:`yellow "DB-LIGHT : get children at %s" (Path.to_string path)#<End>;
  try
    let virtual_children = virtual_get_children tr path in
    let virtual_children = List.sort compare virtual_children in
    try
      let real_children = Db_light.get_children tr.tr_db None path in
      let real_children = List.filter
        (fun p -> not (List.mem p tr.tr_remove_list)) real_children
      in
      let l =
        BaseList.uniq (List.merge (fun p1 p2 -> Path.compare p1 p2)
                         virtual_children real_children)
      in
      (* If we got there, it's the 1% of cases where
         the queried children are affected by the current transaction. *)
      BaseList.filterbounds range (fun p -> Path.last p) l
    with Db_light.UnqualifiedPath ->
      (* That's even more rare --- the father node has just been created. *)
      BaseList.filterbounds range (fun p -> Path.last p) virtual_children
  with
  | Removed -> []
  | Not_found ->
      (* Common case: no children added nor removed in current transaction. *)
      Db_light.get_children tr.tr_db (Some range) path

let trans_operation_counter_limit =
  #<If:DB3_TRANSACTION_LIMIT$minlevel 0>
    int_of_string (Option.get DebugVariables.db3_transaction_limit)
  #<Else>
    0
  #<End>

let add_to_query_map =
  let do_it tr path (query:query) =
    #<If$minlevel 3>Logger.log ~color:`blue
                              "Transaction_light.add_to_query_map: path=%s query=%s"
                              (Path.to_string path) (string_of_query query)#<End>;
    { tr with tr_query_map = (path, query) :: tr.tr_query_map  }
  in
  (* enable the transaction count limit (plus some checks) if requested *)
  if trans_operation_counter_limit == 0 then do_it
  else
    fun tr path query ->
      (* A crucial invariant, needed for correct conflict detection.
         It's so complex, because we normally don't unwind the last link
         for writes (except when doing [Set] with a normal value, but this is
         not captured in the assertion below). *)
      let counter = tr.tr_op_counter in
      if counter < trans_operation_counter_limit then do_it tr path query
      else if counter = -1 then begin
        #<If>
          Logger.log ~color:`yellow
          "DB-LIGHT : operation count still exceeded for transaction #%d"
             tr.tr_num
        #<End>;
        tr
      end else begin
        Logger.warning "DB-LIGHT : transaction exceeded maximum number of operations (%d), aborting"
          trans_operation_counter_limit;
        #<If>
          Logger.log ~color:`red
          "DB-LIGHT : operation count now exceeded for transaction #%d"
             tr.tr_num
          #<End>;
        { tr with tr_op_counter = -1 }
      end

(* Removes all paths with prefix [path] from list [l],
   including [path] itself, if present. *)
let rm_all_with_prefix path l =
  let f p =
    Path.size p < Path.size path
    || Option.is_none (Path.remaining p path)
  in
  List.filter f l

let remove_from_query_map qm path = List.filter (fun (p,_) -> Path.compare path p <> 0) qm

let set_link tr path link =
  #<If>
    Logger.log ~color:`yellow
      "DB-LIGHT : set link at %s toward %s"
        (Path.to_string path) (Path.to_string link)
  #<End>;
  (* Note: do not unwind [link], because it's supposed to unwind
     differently as the db changes (both within the same transaction
     and later on). *)
  let query = Set (Datas.Link link) in
  (* This removes additions to the whole subtree,
     because link node makes them all inaccessible. *)
  let new_map = remove_from_query_map tr.tr_query_map path in
  let tr = { tr with tr_query_map = new_map } in
  let tr = add_to_query_map tr path query in
  (* We don't have to remove anything below [path],
     so we remove all paths that begin with [path] from remove list. *)
  { tr with tr_remove_list = rm_all_with_prefix path tr.tr_remove_list }

(* The check if [target_path] is dangling in [target_rev]
   is done much later, in [execute_query_list], because if the copy
   is of a future revision (target_rev = None) then we don't know yet
   if the path is dangling or not. *)
let set_copy tr path (target_path, _target_rev) =
  #<If>
    Logger.log ~color:`yellow
      "DB-LIGHT : set copy at %s toward %s"
         (Path.to_string path) (Path.to_string target_path)
  #<End>;
  let query = Set (Datas.Copy ((Some (Revision.make 0)), target_path)) in
  (* This removes additions to the whole subtree,
     because copy node makes them all inaccessible. *)
  let new_map = remove_from_query_map tr.tr_query_map path in
  let tr = { tr with tr_query_map = new_map } in
  let tr = add_to_query_map tr path query in
  (* We don't have to remove anything below [path],
     so we remove all paths that begin with [path] from remove list. *)
  { tr with tr_remove_list = rm_all_with_prefix path tr.tr_remove_list }

let set tr path data =
  #<If>
  Logger.log ~color:`yellow
    "DB-LIGHT : set %s at %s"
       (DataImpl.to_string data) (Path.to_string path)
  #<End>;
  let query = Set (Datas.Data data) in
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
    ignore (Db_light.get tr.tr_db path);
    true
  with
  | Db_light.UnqualifiedPath ->
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
  #<If>Logger.log ~color:`yellow "DB-LIGHT : remove at %s"(Path.to_string path)#<End>;
  add_to_query_map tr path (Remove path)

(* initialization of transactions *)

let init db ?read_only i =
  let _ = read_only in
  { tr_num = i
  ; tr_db = db
  ; tr_query_map = []
  ; tr_remove_list = []
  ; tr_index_set = []
  ; tr_index_remove = []
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

let execute_query_map tr db =
  #<If>Logger.log ~color:`yellow "DB-LIGHT : execute_query_map %s" (string_of_query_map tr.tr_query_map)#<End>;
  try
    let ia, ir =
      List.fold_left
        (fun (ia,ir) (path,query) ->
           match query with
           | Set (data) ->
               ignore (Db_light.update db path data);
               let d = Db_light.get tr.tr_db path in
               ((path,d)::ia,ir)
           | Remove path ->
               ignore (Db_light.remove db path);
               (ia,(path,DataImpl.Unit)::ir))
        (tr.tr_index_set,tr.tr_index_remove) (List.rev tr.tr_query_map)
    in
    { tr with tr_query_map = []; tr_index_set = ia; tr_index_remove = ir }, db
  with
  | e ->
      let bt = Printexc.get_backtrace () in
      #<If> Logger.log ~color:`red "execute_query_map --> %s\n%s" (Printexc.to_string e) bt #<End>;
      raise e

let execute_remove_list tr db =
  match tr.tr_remove_list with
  | [] -> tr, db
  | l ->
      let l = List.sort (Path.compare) l in
      List.fold_left
        (fun (tr, db) path ->
           ignore (Db_light.remove db path);
           let d = Db_light.get tr.tr_db path in
           { tr with tr_index_remove = (path,d)::tr.tr_index_remove }, db)
        (tr, db) l

let modified tr = not ((tr.tr_query_map = []) && (tr.tr_remove_list = []))

let commit tr db =
  if modified tr then begin
    if tr.tr_op_counter = -1 then
      (* Too big query map, won't merge with any other. *)
      raise Db_light.Merge;
    (* Execute in the low-level db all remove requests. *)
    let tr, db = execute_remove_list tr db in
    (* Execute the writes that survived subsequent removals, etc. *)
    let tr, db = execute_query_map tr db in
    let db =
      match tr.tr_index_remove with
      | [] -> db
      | l -> Db_light.remove_from_index db l
    in
    let db =
      match tr.tr_index_set with
      | [] -> db
      | l -> Db_light.update_index db l
    in
    db
  end else
    db



