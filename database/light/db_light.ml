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
#<Debugvar:DEBUG_DB>

(* depends *)
module List = BaseList
module String = BaseString
let sprintf fmt = Printf.sprintf fmt
let printf fmt = Printf.printf fmt
let rev = Revision.make 0

(* -- *)

(* Exceptions *)

exception UnqualifiedPath
exception Merge

(* Datatypes *)

type index = ((Path.t * float) list) StringMap.t

type tree = {
  sts : (Keys.t, tree) Hashtbl.t;
  uid : Uid.t;
  key : Keys.t;
  mutable node : Node_light.t;
  mutable up : tree ref;
}

type t = {
  mutable version : string;
  mutable tcount : Eid.t ;
  mutable next_uid : Uid.t;
  mutable index : index;
  tree : tree;
}

(* Constructors *)

let make_node t key data =
  t.next_uid <- Uid.succ t.next_uid;
  let tree = { sts = Hashtbl.create 10;
               uid = t.next_uid;
               key = key;
               node = Node_light.create ~content:data ();
               up = ref (Obj.magic 0);
             } in
  tree.up := tree;
  tree

let make_t () =
  let t = { version = "<new>";
            tcount = Eid.make 0;
            next_uid = Uid.make 0;
            tree = { sts = Hashtbl.create 10;
                     uid = Uid.make 0;
                     key = Keys.StringKey "";
                     node = Node_light.create ();
                     up = ref (Obj.magic 0) };
            index = StringMap.empty;
          } in
  t.tree.up := t.tree;
  t

let rec copy_node t parent tree =
  t.next_uid <- Uid.succ t.next_uid;
  let ntree = { sts = Hashtbl.create 10;
                uid = t.next_uid;
                key = tree.key;
                node = tree.node;
                up = ref parent;
              } in
  Hashtbl.iter (fun k st -> Hashtbl.add ntree.sts k (copy_node t ntree st)) tree.sts;
  t.tcount <- Eid.succ t.tcount;
  ntree

let sts_of_list l =
  let ht = Hashtbl.create 10 in
  List.iter (fun (k,v) -> Hashtbl.add ht k v) l;
  ht

(* Basic database operations *)

let set_version t version = t.version <- version

let update_data tree data =
  let _old_data = tree.node.Node_light.content in
  (match tree.node.Node_light.content, data with
   | Datas.UnsetData, Datas.UnsetData -> ()
   | _, Datas.UnsetData -> ()
   | Datas.UnsetData, _ -> tree.node.Node_light.content <- data
   | _, _ -> tree.node.Node_light.content <- data);
  #<If$minlevel 3>Logger.log ~color:`cyan "update_data: data=%s old_data=%s new_data=%s"
                                                  (Datas.to_string data)
                                                  (Datas.to_string _old_data)
                                                  (Datas.to_string tree.node.Node_light.content)#<End>

let add_tree t path data =
  #<If>Logger.log ~color:`cyan "add_tree: path=%s data=%s" (Path.to_string path) (Datas.to_string data)#<End>;
  let rec aux pt tree = function
    | [] ->
        tree.up := pt;
        update_data tree data
    | k::rest ->
        (try
           let st = Hashtbl.find tree.sts k in
           aux tree st rest
         with Not_found ->
           let st = make_node t k Datas.UnsetData in
           st.up <- ref tree;
           Hashtbl.add tree.sts k st;
           aux tree st rest)
  in
  aux t.tree t.tree (Path.to_list path);
  t.tcount <- Eid.succ t.tcount

let remove_tree t path =
  #<If>Logger.log ~color:`cyan "remove_tree: path=%s" (Path.to_string path)#<End>;
  let rec aux tree = function
    | [] -> false
    | [k] ->
        (try
           let _st = Hashtbl.find tree.sts k in
           #<If$minlevel 2>Logger.log ~color:`cyan "remove_tree(rmv): path=%s data=%s"
                                                   (Path.to_string path) (Datas.to_string _st.node.Node_light.content)#<End>;
           Hashtbl.remove tree.sts k;
           true
         with Not_found -> false)
    | k::rest ->
        (try
           let st = Hashtbl.find tree.sts k in
           let removed = aux st rest in
           if removed && Hashtbl.length st.sts = 0 && st.node.Node_light.content = Datas.UnsetData then Hashtbl.remove tree.sts k;
           removed
         with Not_found -> false)
  in
  let removed = aux t.tree (Path.to_list path) in
  (match removed, Eid.pred t.tcount with
   | true, Some eid -> t.tcount <- eid
   | _, _ -> ());
  removed

(* Node-level navigation:
   Note that we can't export this yet because the Badop.S sig doesn't support it.
*)

exception At_root
exception At_leaf

let node_uid node = node.uid
let node_key node = node.key
let node_node node = node.node
let node_up node = !(node.up)

let node_is_root node = !(node.up) == node

let node_is_leaf node = Hashtbl.length node.sts = 0

let up_node node = if node_is_root node then raise At_root else !(node.up)

let up_node_n node n =
  let rec aux tree = function
    | 0 -> tree
    | n -> aux (up_node tree) (n-1)
  in
  aux node n

let up_node_opt node = try Some (up_node node) with At_root -> None

let down_path tree path =
  let rec aux tree = function
    | [] -> tree
    | k::rest -> aux (Hashtbl.find tree.sts k) rest
  in
  aux tree (Path.to_list path)

let down_node node key = if node_is_leaf node then raise At_leaf else Hashtbl.find node.sts key

let down_node_opt node key = try Some (down_node node key) with | Not_found -> None | At_leaf -> None

let find_node t path = down_path t.tree path

let find_node_opt t path = try Some (find_node t path) with Not_found -> None

let find_data t path = (down_path t.tree path).node.Node_light.content

let find_data_opt t path = try Some (find_data t path) with Not_found -> None

let path_from_node node =
  let rec aux node l =
    if node_is_root node
    then Path.of_list (List.rev l)
    else aux !(node.up) (node.key::l)
  in
  aux node []

let string_of_node { sts=_; uid; key; node; up } =
  sprintf "%d(^%d): %s -> %s"
    (Uid.value uid) (Uid.value ((!up).uid)) (Keys.to_string key) (Datas.to_string node.Node_light.content)

let rec string_of_tree0 indent node =
  let s = sprintf "%s%s\n" indent (string_of_node node) in
  Hashtbl.fold
    (fun k v acc ->
       sprintf "%s%s%s ->\n%s%s" acc indent (Keys.to_string k) indent (string_of_tree0 (indent^" ") v))
    node.sts s
let string_of_tree = string_of_tree0 ""
let print_t t = printf "%s\n" (string_of_tree t.tree)

(* the root of the database *)
let root_eid = Eid.make 0
let start = root_eid


  (******************)
  (* screen display *)
  (******************)

  let print_index db =
    let index = db.index in
    if StringMap.is_empty index then "Empty"
    else
      StringMap.fold (fun name path_list acc ->
                        sprintf "%s%s : %s\n" acc name
                          (Base.List.to_string (fun (p, _) -> sprintf "%s " (Path.to_string p))
                             path_list))
        index ""

  let print_db db =
    let tcount = sprintf "tcount = %s" (Eid.to_string db.tcount) in
    let next_uid = sprintf "next_uid = %s" (Uid.to_string db.next_uid) in
    let index = sprintf "index = %s" (print_index db) in
    sprintf "db : \n%s\n%s\n%s\n%s" tcount next_uid index (string_of_tree db.tree)


  (**********************)
  (* db fields accessors*)
  (**********************)

  let get_rev _db = Revision.make 0
  let get_tcount db = db.tcount
  let get_next_uid db = db.next_uid
  let is_empty db = (Eid.value db.tcount = 0)

  let get_index db = db.index


  (*****************************)
  (* navigation through the db *)
  (*****************************)

  let get_tree_of_path db path =
    try find_node db path
    with Not_found -> raise UnqualifiedPath

  let get_node_of_path db path =
    try ((find_node db path).node,rev)
    with Not_found -> raise UnqualifiedPath

  (************************************)
  (* database creation and rebuilding *)
  (************************************)

  let make = make_t

  let set_rev db _rev = db

  (******************)
  (* basic DB reads *)
  (******************)

  (* may raise UnqualifiedPath *)
  let rec get db path =
    #<If:DEBUG_DB$minlevel 20>Logger.info "Db_light.get: path=%s%!" (Path.to_string path)#<End>;
    let node, _rev = get_node_of_path db path in
    match Node_light.get_content node with
    | Datas.Data d -> d
    | Datas.Link l
    | Datas.Copy (None, l) -> get db l
    | Datas.Copy (Some _, p) -> get db p
    | Datas.UnsetData -> DataImpl.empty

  let get_data (db:t) node =
    let _ = db in
    match Node_light.get_content node with
    | Datas.Data d -> d
    | Datas.UnsetData -> DataImpl.empty
    | _ -> assert false

  let in_range (start_opt, len) key (pllen:int) =
    let res =
      (len == 0 || abs len > pllen) &&
        (match start_opt with
         | Some start ->
             if len < 0
             then ((*printf "%s <= %s -> %b\n%!" (Keys.to_string start) (Keys.to_string key) (Keys.compare start key <= 0);*)
                   Keys.compare start key <= 0)
             else ((*printf "%s >= %s -> %b\n%!" (Keys.to_string start) (Keys.to_string key) (Keys.compare start key >= 0);*)
                   Keys.compare start key >= 0)
         | None -> true)
    in
    (*printf "in_range: key=%s pllen=%d res=%b\n%!" (Keys.to_string key) pllen res;*)
    res

  let get_ch db tree range_opt path max_depth allow_empty =
    let range = match range_opt with Some range -> range | None -> (None,0) in
    let rec aux tree path len start depth =
      (*printf "get_ch: path=%s len=%d\n%!" (Path.to_string path) len;*)
      let inrange, tree, start =
        if Path.to_list path = []
        then (true, db.tree, ([], len))
        else (in_range range (Path.last path) len, tree, start)
      in
      if inrange
      then
        Hashtbl.fold
          (fun key sn (pl,pllen) ->
             let spath = Path.add path key in
             let start = if allow_empty || Node_light.is_occupied sn.node then ([spath],pllen+1) else ([],pllen) in
             let spl,spllen =
               if depth < max_depth
               then aux sn spath pllen start (depth+1)
               else ([],pllen)
             in
             (pl@spl,spllen)) tree.sts start
      else
        ([],len)
    in
    aux tree path 0 ([],0) 0

  let rec _get_children db range_opt path max_depth allow_empty raise_on_unqualified =
    let tree =
      if raise_on_unqualified
      then
        Some (get_tree_of_path db path)
      else
        try
          Some (get_tree_of_path db path)
        with UnqualifiedPath -> None
    in
    match tree with
    | Some tree ->
        (match Node_light.get_content tree.node with
         | Datas.Link p
         | Datas.Copy (_, p) -> _get_children db range_opt p max_depth allow_empty raise_on_unqualified
         | _ -> fst (get_ch db tree range_opt path max_depth allow_empty))
    | None -> []

  (* may raise UnqualifiedPath *)
  let get_children db range_opt path =
    let ch = _get_children db range_opt path 1 true true in
    #<If:DEBUG_DB$minlevel 20>Logger.info "Db_light.get_children: %s -> [%s]%!"
      (Path.to_string path) (String.concat_map "; " Path.to_string ch)#<End>;
    ch

  (* won't raise UnqualifiedPath *)
  let get_all_children db range_opt path =
    let ch = _get_children db range_opt path max_int false false in
    #<If:DEBUG_DB$minlevel 20>Logger.info "Db_light.get_all_children: %s -> [%s]%!"
      (Path.to_string path) (String.concat_map "; " Path.to_string ch)#<End>;
    ch

  (********************)
  (* basics DB writes *)
  (********************)

  let update db path data = add_tree db path data; db

  let remove db path = ignore (remove_tree db path); db

  (* index management *)

  let update_index db update_list =
    #<If$minlevel 3>
      Logger.log ~color:`cyan
        "update_index: [%s]"
        (String.concat_map "; "
           (fun (p,d) -> sprintf "(%s,%s)" (Path.to_string p) (DataImpl.to_string d)) update_list)
    #<End>;
    let new_index =
      List.fold_left
        (fun acc (path, data) ->
           let map = DataImpl.index_fun data in
           let count = StringMap.fold (fun _k v acc -> acc + v) map 0 in
           StringMap.fold
             (fun name score acc ->
                let score = (float_of_int score) /. (float_of_int count) in
                let new_path_list =
                  match StringMap.find_opt name acc with
                  | Some pl -> (path, score) :: pl
                  | None -> [path, score]
                in
                StringMap.add name new_path_list acc
             ) map acc
        ) db.index update_list
    in
    {db with index = new_index}

  let remove_from_index db remove_list =
    #<If$minlevel 3>
      Logger.log ~color:`cyan
        "remove_from_index: [%s]"
        (String.concat_map "; "
           (fun (p,d) -> sprintf "(%s,%s)" (Path.to_string p) (DataImpl.to_string d)) remove_list)
    #<End>;
    let new_index =
      List.fold_left
        (fun index (path, data) ->
           let map = DataImpl.index_fun data in
           StringMap.fold
             (fun str _ index ->
                let new_list =
                  match StringMap.find_opt str index with
                  | Some l -> List.remove_assoc path l
                  | None -> []
                in
                match new_list with
                | [] -> StringMap.remove str index
                | _ -> StringMap.add str new_list index
             ) map index
        ) db.index remove_list
    in
    {db with index = new_index}



  (******************************************************)
  (*  full search managment (only for current revision) *)
  (******************************************************)

  (** Takes a list of decreasing-relevance lists of results; merges them to turn
      individual searches to an AND search, ordered by decreasing minimal
      rank. Lists should not contain duplicates. *)
  let merge_search_results ll =
    let n = List.length ll in
    let occur = Hashtbl.create 23 in
      (* table from key to number of occurences. When that number equals n, we got a result *)
    let results = ref [] in
    let add key =
      let nb_occur = try Hashtbl.find occur key + 1 with Not_found -> 1 in
      if nb_occur < n then Hashtbl.replace occur key nb_occur else
        (results := key::!results; Hashtbl.remove occur key)
    in
    let rec aux ll =
      let nempty, ll = Base.List.fold_left_map
        (fun nempty -> function key::r -> add key; nempty, r | [] -> nempty+1, [])
        0 ll in
      if nempty < n then aux ll
    in
    aux ll;
    List.rev !results

  let full_search db words path =
    let (|>) a f = f a in
    let results =
      Base.List.filter_map
        (fun word ->
           StringMap.find_opt word db.index
           |> Option.map
               (Base.List.filter_map
                  (fun (p,r) -> Path.remaining path p |> Option.map (fun p -> List.hd p, r))))
        words
    in
    let results =
      List.tail_map
        (fun l -> l
           |> List.sort
               (fun (k1, r1) (k2, r2) -> let c = Pervasives.compare r1 r2 in if c <> 0 then - c else - Keys.compare k1 k2)
           |> List.tail_map fst
           |> Base.List.uniq)
        results
    in
    merge_search_results results


  (* Links *)

  let set_link db path link =
    let node = get_tree_of_path db path in
    node.node.Node_light.content <- Datas.Link link;
    db

  (* Copies *)

  (* Just behave like links for now... *)
  let set_copy db path link =
    let tree = get_tree_of_path db path in
    tree.node.Node_light.content <- Datas.Copy (Some rev, link);
    db

  (*Unfinished...
    let set_physical_copy db path link =
    let tree = get_tree_of_path db path in
    let target = get_tree_of_path db link in
    tree.node.Node_light.content <- Node_light.get_content copy.node;
    tree.node.Node_light.content <- Datas.Copy (Some rev, link);
    db*)

  let rec follow_path (db:t) node path_end =
    (*#<If:DEBUG_DB$minlevel 10>
      Logger.log ~color:`green
        (sprintf "DB : low-level following path; remaining: %s"
           (Path.to_string (Path.of_list path_end)))
    #<End>;*)
    match path_end with
    | [] -> ([], node)
    | k :: rest ->
        try
          match Node_light.get_content node.node with
          | Datas.Link _
          | Datas.Copy _ -> (path_end, node)
          | _ ->
              let node = Hashtbl.find node.sts k in
              follow_path db node rest
        with Not_found -> raise UnqualifiedPath

  let follow_link db path =
    #<If:DEBUG_DB$minlevel 20>Logger.info "Db_light.follow_link: path=%s" (Path.to_string path)#<End>;
    let rec aux db path =
      let path_end = Path.to_list path in
      let (path_end, node) = follow_path db db.tree path_end in
      match Node_light.get_content node.node with
      | Datas.Link l ->
          (* Links possible both on [l] and [path_end], hence the [concat]. *)
          let new_path = Path.concat l (Path.of_list path_end) in
          aux db new_path
      | Datas.Copy (_, l) ->
          let new_path = Path.concat l (Path.of_list path_end) in
          aux db new_path
      | _ ->
          assert (path_end = []);
          (path, node)
    in
    aux db path

(*
let tt_ref = ref (make_t ())

let _ = 
  let tt = make_t () in
  let _K_a = Keys.StringKey "a" in
  let _K_b = Keys.StringKey "b" in
  let _K_c = Keys.StringKey "c" in
  let _K_d = Keys.StringKey "d" in
  let _K_e = Keys.StringKey "e" in
  let _K_f = Keys.StringKey "f" in
  let _K_x = Keys.StringKey "x" in
  let _K_y = Keys.StringKey "y" in
  let _K_z = Keys.StringKey "z" in
  let a = Path.of_list [_K_a] in
  let de = Path.of_list [_K_d; _K_e] in
  let ab = Path.of_list [_K_a; _K_b] in
  let abc = Path.of_list [_K_a; _K_b; _K_c] in
  let abd = Path.of_list [_K_a; _K_b; _K_d] in
  let def = Path.of_list [_K_d; _K_e; _K_f] in
  let xyz = Path.of_list [_K_x; _K_y; _K_z] in
  add_tree tt abc (Datas.Data (DataImpl.Int 123));
  print_t tt;
  add_tree tt abd (Datas.Data (DataImpl.Int 124));
  print_t tt;
  add_tree tt a (Datas.Data (DataImpl.Int 1));
  print_t tt;
  add_tree tt def (Datas.Data (DataImpl.Int 456));
  tt_ref := tt;
  print_t tt;
  printf "get_eid(tt)=%d\n" (Eid.value tt.tcount);
  printf "find_data(abc)=%s\n" (Option.to_string Datas.to_string (find_data_opt tt abc));
  printf "find_node(abc)=%s\n" (Option.to_string (fun tree -> Uid.to_string tree.uid) (find_node_opt tt abc));
  printf "node_is_root(tt.tree)=%b\n" (node_is_root tt.tree);
  printf "node_is_root(find_node(abc))=%b\n" (node_is_root (Option.get (find_node_opt tt abc)));
  let node_ab = find_node_opt tt ab in
  set_link tt de ab;
  print_t tt;
  ignore (remove_tree tt de);
  print_t tt;
  set_copy tt de ab;
  print_t tt;
  printf "node_ab=%s\n" (Option.to_string string_of_node node_ab);
  printf "down_node(node_ab,\"c\")=%s\n" (Option.to_string string_of_node (down_node_opt (Option.get node_ab) _K_c));
  printf "up_node(node_ab)=%s\n" (Option.to_string string_of_node (up_node_opt (Option.get node_ab)));
  printf "up_node(tt.tree)=%s\n" (Option.to_string string_of_node (up_node_opt tt.tree));
  printf "find_data(abd)=%s\n" (Option.to_string Datas.to_string (find_data_opt tt abd));
  printf "find_data(a)=%s\n" (Option.to_string Datas.to_string (find_data_opt tt a));
  printf "find_data(def)=%s\n" (Option.to_string Datas.to_string (find_data_opt tt def));
  printf "find_data(xyz)=%s\n" (Option.to_string Datas.to_string (find_data_opt tt xyz));
  printf "get_children(root)=[%s]\n"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_children tt (Some (None,0)) (Path.of_list [])));
  printf "get_children(a)=[%s]\n"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_children tt (Some (None,0)) a));
  printf "get_children(a,<=c)=[%s]\n"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_children tt (Some (Some _K_c,0)) a));
  printf "get_children(a,3)=[%s]\n"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_children tt (Some (None,3)) a));
  printf "get_children(ab,>=b)=[%s]\n"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_children tt (Some (Some _K_b,-3)) ab));
  ignore (remove_tree tt abc);
  print_t tt;
  ignore (remove_tree tt abd);
  print_t tt;
  ignore (remove_tree tt a);
  print_t tt;
  ignore (remove_tree tt def);
  print_t tt
*)

