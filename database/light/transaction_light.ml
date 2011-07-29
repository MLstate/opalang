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
module String = BaseString
module List = BaseList
module Hashtbl = BaseHashtbl
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
  List.to_string (fun (i,_,q) -> sprintf "%d:%s " i (string_of_query q)) list

type query_element = (int * Path.t * query)
type query_map = (Path.t, query_element list) Hashtbl.t

let string_of_query_element (i,_,q) = sprintf "%d:%s " i (string_of_query q)

let string_of_query_map (qm:query_map) =
  let qs = Hashtbl.fold (fun p ql a -> (p,ql)::a) qm [] in
  List.to_string (fun (p,ql) -> sprintf "(%s,%s); " (Path.to_string p) (string_of_query_list ql)) qs

type t = {
  tr_num : int ;
  tr_db : Db_light.t ;
  (** the db the transaction refers to *)

  tr_pending_queries : query_element list ref;
  tr_query_map : query_map;
  (** the map of queries against the db *)

  mutable tr_remove_list : Path.t list;
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
let get_query_map _tr = assert false (*tr.tr_query_map*)

(*********************)
(* DB reading access *)
(*********************)

let full_search tr slist path =
  #<If>
  Logger.log ~color:`cyan "DB-LIGHT : full search for %s at %s"
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

exception Datas_not_found

let rec find_datas_in_query_list = function
  | [] -> raise Datas_not_found
  | (_,_,Set datas) :: _ -> datas
  | _ :: tl -> find_datas_in_query_list tl

exception Removed

(* Raises [Not_found] if data absent from query, [Removed] if removed. *)
(*let get_query_at tr path =
  let query_list = Option.default [] (Hashtbl.find_opt tr.tr_query_map path) in
  if query_list = []
  then
    (if List.mem path tr.tr_remove_list
     then raise Removed
     else raise Not_found);
  query_list*)

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
  try
    let path, kind =
      let rec aux path =
        let (node, _) = Db_light.get_node_of_path tr.tr_db path in
        #<If>Logger.log ~color:`green "Transaction_light.stat: path=%s node=%s"
                                 (Path.to_string path) (Datas.to_string (Node_light.get_content node))#<End>;
        match Node_light.get_content node with
        | Datas.Data _ -> path, `Data
        | Datas.Link p -> p, `Link
        | Datas.Copy (_, p) -> aux p
        | Datas.UnsetData -> path, `Unset
      in
      aux path
    in
    (path, Some (Revision.make 0), kind)
  with exn ->
    #<If>Logger.log ~color:`red "Transaction_light.stat: exn=%s" (Printexc.to_string exn)#<End>;
    raise exn

let datas_from_path tr path = Node_light.get_content (Db_light.node_node (snd (Db_light.follow_link tr.tr_db path)))

(*let map_pending_queries tr =
  #<If>Logger.log ~color:`cyan "DB-LIGHT : map_pending_queries"#<End>;
  let start = Unix.gettimeofday () in
  List.iter (function (_,path,_) as qe ->
               Hashtbl.replace tr.tr_query_map path
                 (qe::(Option.default [] (Hashtbl.find_opt tr.tr_query_map path)))) (!(tr.tr_pending_queries));
  eprintf(*Logger.log ~color:`cyan*) "DB-LIGHT : map_pending_queries: time=%f\n%!" ((Unix.gettimeofday()) -. start);
  tr.tr_pending_queries := []*)

let rec unwind tr path =
  (*if !(tr.tr_pending_queries) <> [] then map_pending_queries tr;*)
  let datas =
    (*match Option.default [] (Hashtbl.find_opt tr.tr_query_map path) with*)
    match List.fold_right (function (_,p,_) as qe -> fun a -> if p = path then qe::a else a) !(tr.tr_pending_queries) [] with
    | [] ->
        let datas = datas_from_path tr path in
        #<If>Logger.log ~color:`cyan "DB-LIGHT : unwind %s -> %s" (Path.to_string path) (Datas.to_string datas)#<End>;
        datas
    | qlist ->
        (try
           let datas = find_datas_in_query_list qlist in
           #<If>Logger.log ~color:`cyan "DB-LIGHT : unwind(qlist) %s -> %s"
                                          (Path.to_string path) (Datas.to_string datas)#<End>;
           datas
         with Datas_not_found ->
           let datas = datas_from_path tr path in
           #<If>Logger.log ~color:`cyan "DB-LIGHT : unwind(noset) %s -> %s" (Path.to_string path)
                                          (Datas.to_string datas)#<End>;
           datas)
  in
  match datas with
  | Datas.Data _ -> datas
  | Datas.Link p -> unwind tr p
  | Datas.Copy (_, p) -> unwind tr p
  | Datas.UnsetData -> datas

let get tr path =
  let data =
    match unwind tr path with
    | Datas.Data d -> d
    | Datas.UnsetData -> DataImpl.empty
    | _ -> assert false
  in
  #<If>Logger.log ~color:`cyan "DB-LIGHT : get data at %s = %s" (Path.to_string path) (DataImpl.to_string data)#<End>;
  data

(* may raise Removed and Not_found *)
let virtual_get_children tr path =
  (*if !(tr.tr_pending_queries) <> [] then map_pending_queries tr;
  Hashtbl.fold (fun p _ a -> if Path.is_prefix path p then p::a else a) tr.tr_query_map []*)
  List.fold_right (fun (_,p,_) a -> if Path.is_prefix path p then p::a else a) !(tr.tr_pending_queries) []

let get_children tr range path =
  #<If>Logger.log ~color:`cyan "DB-LIGHT : get children at %s" (Path.to_string path)#<End>;
  try
    let virtual_children = virtual_get_children tr path in
    let virtual_children = List.sort compare virtual_children in
    try
      let real_children = Db_light.get_children tr.tr_db None path in
      let real_children = List.filter (fun p -> not (List.mem p tr.tr_remove_list)) real_children in
      let l = BaseList.uniq (List.merge (fun p1 p2 -> Path.compare p1 p2) virtual_children real_children) in
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

let query_index = ref 0
let get_query_index () = incr query_index; !query_index

let add_to_query_map =
  let do_it tr path (query:query) =
    #<If$minlevel 3>Logger.log ~color:`cyan
                              "DB-LIGHT : add_to_query_map: path=%s query=%s"
                              (Path.to_string path) (string_of_query query)#<End>;
    (*let start = Unix.gettimeofday () in*)
    (*Hashtbl.replace tr.tr_query_map path
                    ((get_query_index(),path,query)::(Option.default [] (Hashtbl.find_opt tr.tr_query_map path)));*)
    tr.tr_pending_queries := (get_query_index(),path,query)::(!(tr.tr_pending_queries));
    (*eprintf(*Logger.log ~color:`cyan*) "DB-LIGHT : add_to_query_map: replace time=%f\n%!" ((Unix.gettimeofday()) -. start);*)
    tr
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
          Logger.log ~color:`cyan
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

let remove_subtree path tr =
  (*if !(tr.tr_pending_queries) <> [] then map_pending_queries tr;
  let rl = Hashtbl.fold (fun p _ a -> if Path.is_prefix path p then p::a else a) tr.tr_query_map [] in
  List.iter (fun p -> Hashtbl.remove tr.tr_query_map p) rl*)
  tr.tr_pending_queries := List.filter (fun (_,p,_) -> not (Path.is_prefix path p)) !(tr.tr_pending_queries)

let set_link tr path link =
  #<If>
    Logger.log ~color:`cyan
      "DB-LIGHT : set link at %s toward %s"
        (Path.to_string path) (Path.to_string link)
  #<End>;
  (* Note: do not unwind [link], because it's supposed to unwind
     differently as the db changes (both within the same transaction
     and later on). *)
  let query = Set (Datas.Link link) in
  (* This removes additions to the whole subtree,
     because link node makes them all inaccessible. *)
  remove_subtree path tr;
  let tr = add_to_query_map tr path query in
  (* We don't have to remove anything below [path],
     so we remove all paths that begin with [path] from remove list. *)
  tr.tr_remove_list <- rm_all_with_prefix path tr.tr_remove_list;
  tr

(* The check if [target_path] is dangling in [target_rev]
   is done much later, in [execute_query_list], because if the copy
   is of a future revision (target_rev = None) then we don't know yet
   if the path is dangling or not. *)
let set_copy tr path (target_path, _target_rev) =
  #<If>
    Logger.log ~color:`cyan
      "DB-LIGHT : set copy at %s toward %s"
         (Path.to_string path) (Path.to_string target_path)
  #<End>;
  let query = Set (Datas.Copy ((Some (Revision.make 0)), target_path)) in
  (* This removes additions to the whole subtree,
     because copy node makes them all inaccessible. *)
  remove_subtree path tr;
  let tr = add_to_query_map tr path query in
  (* We don't have to remove anything below [path],
     so we remove all paths that begin with [path] from remove list. *)
  tr.tr_remove_list <- rm_all_with_prefix path tr.tr_remove_list;
  tr

let set tr path data =
  #<If>
  Logger.log ~color:`cyan
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
(*let check_remove tr path =
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
      | Not_found | Removed ->  false*)

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
  #<If>Logger.log ~color:`cyan "DB-LIGHT : remove at %s"(Path.to_string path)#<End>;
  add_to_query_map tr path (Remove path)

(* initialization of transactions *)

let init db ?read_only i =
  let _ = read_only in
  { tr_num = i
  ; tr_db = db
  ; tr_pending_queries = ref []
  ; tr_query_map = Hashtbl.create 100000
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

let compare_q (i1,_,_) (i2,_,_) = Pervasives.compare i1 i2
let get_sorted_queries tr =
  (*if !(tr.tr_pending_queries) <> [] then map_pending_queries tr;*)
  (*let start = Unix.gettimeofday() in*)
  (*let l = List.sort compare_q (Hashtbl.fold (fun _ ql a -> ql@a) tr.tr_query_map []) in*)
  let l = List.sort compare_q !(tr.tr_pending_queries) in
  (*eprintf(*Logger.log ~color:`cyan*) "DB-LIGHT : get_sorted_queries: time=%f\n%!" ((Unix.gettimeofday()) -. start);*)
  l

let execute_query_map tr db =
  let qs = get_sorted_queries tr in
  #<If>Logger.log ~color:`cyan "DB-LIGHT : execute_query_map %d"
                               (List.length !(tr.tr_pending_queries))
                               (*(Hashtbl.length tr.tr_query_map)*)
                               (*string_of_query_map tr.tr_query_map*)#<End>;
  (*#<If>Logger.log ~color:`cyan "DB-LIGHT : execute_query_map(sorted)[%s]"
                               (String.concat_map "; " string_of_query_element qs)#<End>;*)
  let update_time = ref 0.0 in
  Db_light.add_tree_1 := 0.0;
  Db_light.ondemand_add_1 := 0.0;
  Db_light.update_data_1 := 0.0;
  Db_light.update_data_2 := 0.0;
  Db_light.update_data_3 := 0.0;
  try
    let ia, ir, rl =
      List.fold_left
        (fun (ia,ir,rl) (_i,path,query) ->
           #<If>Logger.log ~color:`cyan "DB-LIGHT : execute_query_map %s" (string_of_query_element (_i,path,query))#<End>;
           match query with
           | Set (data) ->
               let start = Unix.gettimeofday () in
               ignore (Db_light.update db path data);
               update_time := !update_time +. ((Unix.gettimeofday()) -. start);
               (match data with
                | Datas.Data d -> ((path,d)::ia,ir,rl)
                | Datas.UnsetData -> ((path,DataImpl.Unit)::ia,ir,rl)
                | Datas.Link _p
                | Datas.Copy (_,_p) ->
                    (try
                       let d = Db_light.get tr.tr_db path in
                       ((path,d)::ia,ir,rl)
                     with Db_light.UnqualifiedPath ->
                       #<If>Logger.log ~color:`red "DB-LIGHT : execute_query_map: dangling link or copy %s -> %s"
                                                   (Path.to_string path) (Path.to_string _p)#<End>;
                       (ia,ir,rl)))
           | Remove path ->
               let ch = try Db_light.get_all_children db None path with Db_light.UnqualifiedPath -> [] in
               #<If>Logger.log ~color:`cyan "DB-LIGHT : execute_query_map(Remove) ch=[%s]"
                                              (String.concat_map "; " Path.to_string ch)#<End>;
               ignore (Db_light.remove db path);
               let allir = (path,DataImpl.Unit)::(List.map (fun p -> (p,DataImpl.Unit)) ch) in
               (ia,allir@ir,ch@rl))
        (tr.tr_index_set,tr.tr_index_remove,tr.tr_remove_list) qs
    in
    (*eprintf(*Logger.log ~color:`cyan*) "DB-LIGHT : execute_query_map: update_time=%f\n%!" !update_time;
    eprintf(*Logger.log ~color:`cyan*) "DB-LIGHT : execute_query_map: add_tree_1=%f\n%!" !(Db_light.add_tree_1);
    eprintf(*Logger.log ~color:`cyan*) "DB-LIGHT : execute_query_map: ondemand_add_1=%f\n%!" !(Db_light.ondemand_add_1);
    eprintf(*Logger.log ~color:`cyan*) "DB-LIGHT : execute_query_map: update_data_1=%f\n%!" !(Db_light.update_data_1);
    eprintf(*Logger.log ~color:`cyan*) "DB-LIGHT : execute_query_map: update_data_2=%f\n%!" !(Db_light.update_data_2);
    eprintf(*Logger.log ~color:`cyan*) "DB-LIGHT : execute_query_map: update_data_3=%f\n%!" !(Db_light.update_data_3);*)
    tr.tr_remove_list <- rl;
    { tr with tr_index_set = ia; tr_index_remove = ir; }, db
  with
  | e ->
      let _bt = Printexc.get_backtrace () in
      #<If>Logger.log ~color:`red "DB-LIGHT : execute_query_map --> %s\n%s" (Printexc.to_string e) _bt#<End>;
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

let modified tr = not ((!(tr.tr_pending_queries) = []) && (Hashtbl.length tr.tr_query_map = 0) && (tr.tr_remove_list = []))

let commit tr db =
  if modified tr then begin
    if tr.tr_op_counter = -1 then
      (* Too big query map, won't merge with any other. *)
      raise Db_light.Merge;
    (* Execute in the low-level db all remove requests. *)
    (*let start = Unix.gettimeofday () in*)
    let tr, db = execute_remove_list tr db in
    (*eprintf(*Logger.log ~color:`cyan*) "DB-LIGHT : commit: execute_remove_list time=%f\n%!" ((Unix.gettimeofday()) -. start);*)
    (* Execute the writes that survived subsequent removals, etc. *)
    (*let start = Unix.gettimeofday () in*)
    let tr, db = execute_query_map tr db in
    (*eprintf(*Logger.log ~color:`cyan*) "DB-LIGHT : commit: execute_query_map time=%f\n%!" ((Unix.gettimeofday()) -. start);*)
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



