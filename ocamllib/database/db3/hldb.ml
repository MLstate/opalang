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
(*
    @author1 Henri Binsztok,
    @author2 Gregoire Makridis
**)

(* depends *)
module List = BaseList

(* alias *)

(* -- *)

exception UnqualifiedPath
exception Merge

type index = ((Path.t * float) list) StringMap.t

type node_map = Node.t UidMap.t

type t = {
  rev : Revision.t ;
  tcount : Eid.t ;
  next_uid : Uid.t ;
  uid_of_eid : (Uid.t RevisionMap.t) EidMap.t ;
  node_of_uid : node_map ;
  index : index ;
  tmp_uid_of_eid : Uid.t Eid.Map.t ;
  tmp_node_of_uid : Node.t Uid.Map.t ;
}

  (*
    - field rev is the current revision of the DB
    - field tcount is the number of nodes in the DB
    - field next_uid is the new uid available for node creation
    - field uid_of_eid is the map (eid -> rev -> uid)
    - field node_of_uid is the map (uid -> Node.t)
    - field index is the map (string -> Path.t list) that for a word gives the list
    of paths where the word can be found and the pf_idf score for this word on
    each path
    - fields tmp_uid_of_eid and tmp_node_of_uid are current version of
    eid -> uid and uid -> Node.t, usefull to know which nodes have been created
    and must been writen on disk.
  *)

  (*
    the nodes are stored either in an IntMap (mostly when the db is used for debuging).
  *)

  (* the root of the database *)
  let root_eid = Eid.make 0
  let start = root_eid


  (******************)
  (* screen display *)
  (******************)

  let print_tmp_node_map db =
    if Uid.Map.is_empty db.tmp_node_of_uid 
    then "Empty"
    else Uid.Map.fold (
      fun uid node acc -> Printf.sprintf "%s\t%d -> %s\n"
        acc (Uid.value uid) (Node.to_string node)
    ) db.tmp_node_of_uid "\n"

  let print_tmp_uid_map db =
    if Eid.Map.is_empty db.tmp_uid_of_eid
    then "Empty"
    else Eid.Map.fold (
      fun eid uid acc -> Printf.sprintf "%s\t%d -> %s\n"
        acc (Eid.value eid) (Uid.to_string uid)
    ) db.tmp_uid_of_eid "\n"

  let print_uid_map_bis map =
    if map = RevisionMap.empty then "Empty"
    else
      fst (RevisionMap.fold(
        fun rev uid (acc, sep) ->
          let res = Printf.sprintf "%s%s%d -> %s\n"
            acc sep (Revision.value rev) (Uid.to_string uid) in
          res, "     "
      ) map ("", ""))

  let print_uid_map db =
    if EidMap.is_empty db.uid_of_eid then "Empty"
    else
      EidMap.fold (
        fun eid map acc -> Printf.sprintf "%s%d -> %s"
          acc (Eid.value eid) (print_uid_map_bis map)
      ) db.uid_of_eid "\n"

  let print_node_map db =
    UidMap.fold (
      fun uid node acc -> Printf.sprintf "%s\t%d -> %s\n"
        acc (Uid.value uid) (Node.to_string node)
    ) db.node_of_uid "\n"

  let print_index db =
    let index = db.index in
    if StringMap.is_empty index then "Empty"
    else
      StringMap.fold (
        fun name path_list acc ->
          Printf.sprintf "%s%s : %s\n" acc name
            (Base.List.to_string (
               fun (p, _) -> Printf.sprintf "%s " (Path.to_string p)
             ) path_list)
      ) index ""

  let print_db db =
    let rev =
      Printf.sprintf "current_revision = %s"
        (Revision.to_string db.rev)
    in
    let tcount = Printf.sprintf "tcount = %s" (Eid.to_string db.tcount) in
    let next_uid = Printf.sprintf "next_uid = %s" (Uid.to_string db.next_uid) in
    let uid_map = Printf.sprintf "uid_map = %s" (print_uid_map db) in
    let node_map = Printf.sprintf "node_map = %s" (print_node_map db) in
    let tmp_uid_map = Printf.sprintf "tmp_uid_map = %s" (print_tmp_uid_map db) in
    let tmp_node_map = Printf.sprintf "tmp_node_map = %s" (print_tmp_node_map db) in
    let index = Printf.sprintf "index = %s" (print_index db) in
    Printf.sprintf "db : \n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s"
      rev tcount next_uid uid_map node_map tmp_uid_map
      tmp_node_map index


  (**********************)
  (* db fields accessors*)
  (**********************)

  let get_rev db = db.rev
  let get_tcount db = db.tcount
  let get_next_uid db = db.next_uid
  let is_empty db = (Eid.value db.tcount = 0)

  let get_uid_map db = db.uid_of_eid
  let get_node_map db = db.node_of_uid
  let get_last_nodes db = db.tmp_node_of_uid
  let get_index db = db.index


  (*****************************)
  (* navigation through the db *)
  (*****************************)

  (* Raise Not_found if the eid is not in the map *)
  (* Raise Not_found if the revision looked for is the smallest one *)
  let _get_uid_of_eid db rev eid =
    let map = EidMap.find eid db.uid_of_eid in
    snd (RevisionMap.find_inf rev map)

  (* may raise Not_found from _get_uid_of_eid *)
  let get_uid_of_eid db rev eid =
    #<If:DEBUG_DB$minlevel 1000>
      Logger.log ~color:`green
         "DB : get_uide_of_eid eid(%s) rev(%s)"
           (Eid.to_string eid)
           (Revision.to_string rev)
    #<End>;

    if rev < db.rev then _get_uid_of_eid db rev eid
    else
      try Eid.Map.find eid db.tmp_uid_of_eid
      with Not_found -> _get_uid_of_eid db rev eid

  (* looking for an uid from an eid into the different maps of the db,
     or reading from disk if no found in all the maps. *)
  (* Raise Not_found id the uid is not in the temporary or full nodemap *)
  let get_node_of_uid db uid =
    #<If:DEBUG_DB$minlevel 1000>
      Logger.log ~color:`green
        "DB : get_node_of_uid uid(%s)"
           (Uid.to_string uid)
    #<End>;

      match Uid.Map.find_opt uid db.tmp_node_of_uid with
      | Some node -> node
      | None -> UidMap.find uid db.node_of_uid

  let get_node_of_eid db rev eid =
    #<If:DEBUG_DB$minlevel 1000>
      Logger.log ~color:`green
         "DB : get_node_of_eid eid(%s) rev(%s)"
           (Eid.to_string eid)
           (Revision.to_string rev)
    #<End>;

    let uid = get_uid_of_eid db rev eid in
    get_node_of_uid db uid

  (* raise UnqualifiedPath if unable to find a node from its eid and the given
     revision, or if the current key of the path is not found in the map of the
     current node. *)
  let rec get_eid_of_path db rev path =
    #<If:DEBUG_DB$minlevel 1000>
      Logger.log ~color:`green
        "DB : get_eid_of_path rev(%s) path(%s)"
           (Revision.to_string rev)
           (Path.to_string path)
    #<End>;

    let rec aux db pos rev cur_path path =
      match path with
      | [] -> pos, rev
      | k :: tl ->
          try
            let node = get_node_of_eid db rev pos in
            match Node.get_content ~f:(get_node_of_uid db) node with
            | Datas.Link l
            | Datas.Copy (None, l) ->
                get_eid_of_path db rev (Path.concat l (Path.of_list path))
            | Datas.Copy (Some r, p) ->
                get_eid_of_path db r (Path.concat p (Path.of_list path))
            | _ ->
                let neid = Node.next_eid ~f:(get_node_of_uid db) k node in
                aux db neid rev (Path.add cur_path k) tl
          with Not_found -> raise UnqualifiedPath
    in aux db start rev Path.root (Path.to_list path)

  (* raise UnqualifiedPath if unable to find the eid from the path or unable to
     find the node associated with the eid if it has been found *)
  (* TODO: the [rev] result does not seem used a lot; use or simplify *)
  let get_node_of_path db rev path =
    #<If:DEBUG_DB$minlevel 1000>
      Logger.log ~color:`green
        "DB : get_node_of_path rev(%s) path(%s)"
           (Revision.to_string rev)
           (Path.to_string path)
    #<End>;

    let eid, rev = get_eid_of_path db rev path in
    try get_node_of_eid db rev eid, rev
    with Not_found -> raise UnqualifiedPath

  let is_new_uid db uid = Uid.Map.mem uid db.tmp_node_of_uid

  (* cleaning the temporary maps (mostly when the current revision has been
     writen on disk. *)
  let clean_tmp_maps db =
    { db with
        tmp_uid_of_eid = Eid.Map.empty  ;
        tmp_node_of_uid = Uid.Map.empty ;
    }

  (************************************)
  (* database creation and rebuilding *)
  (************************************)

  (* parameter 'weak' is a function that for an uid reads the disk to find the
     associated node. *)
  let make ?weak () =
    ignore weak;
    let rev = Revision.make 0 in
    let t = Node.create rev in
    let uid_0 = Uid.make 0 in
    let uid_1 = Uid.make 1 in
    let uid_of_eid =
      let eid_0 = Eid.make 0 in
      let rev_0 = Revision.make 0 in
      let content = RevisionMap.add rev_0 uid_0 RevisionMap.empty in
      EidMap.add eid_0 content (EidMap.empty ()) in
    let node_of_uid, tmp_node_of_uid = (UidMap.add uid_0 t (UidMap.empty ())), Uid.Map.empty in

    let tcount = Eid.make 0 in
    let next_uid = uid_1 in
    let index = StringMap.empty in
    let tmp_uid_of_eid = Eid.Map.empty in
    { rev;
      tcount;
      next_uid;
      uid_of_eid;
      node_of_uid;
      index;
      tmp_uid_of_eid;
      tmp_node_of_uid;
    }


  let restart ?index rev tcount next_uid uid_of_eid node_of_uid =
    let index = Option.default StringMap.empty index in
    { rev;
      tcount;
      next_uid;
      uid_of_eid;
      node_of_uid;
      index;
      tmp_uid_of_eid = Eid.Map.empty;
      tmp_node_of_uid = Uid.Map.empty;
    }

  let set_rev db rev = {db with rev = rev}


  (******************)
  (* basic DB reads *)
  (******************)

  (* may raise UnqualifiedPath *)
  let rec get db rev path =
    let node, rev = get_node_of_path db rev path in
    match (Node.get_content ~f:(get_node_of_uid db) node) with
    | Datas.Data d -> d
    | Datas.Link l
    | Datas.Copy (None, l) -> get db rev l
    | Datas.Copy (Some r, p) -> get db r p
    | Datas.UnsetData -> DataImpl.empty

  let get_data db node =
    match (Node.get_content ~f:(get_node_of_uid db) node) with
    | Datas.Data d -> d
    | Datas.UnsetData -> DataImpl.empty
    | _ -> assert false

  let _get_children db rev node range path =
    let foo = fun k _eid acc -> (Path.add path k, rev) :: acc in
    match range with
    | Some range ->
        let l = Node.fold_range range ~f:(get_node_of_uid db) foo node [] in
        if snd range < 0 then l else List.rev l
    | None ->
        List.rev (Node.fold ~f:(get_node_of_uid db) foo node [])

  (* may raise UnqualifiedPath *)
  let rec get_children db rev range path =
    let node, rev = get_node_of_path db rev path in
    match Node.get_content ~f:(get_node_of_uid db) node with
    | Datas.Link p
    | Datas.Copy (None, p) ->
        get_children db rev range p
    | Datas.Copy (Some r, p) ->
        get_children db r range p
    | _ -> _get_children db rev node range path

  (* may raise UnqualifiedPath *)
  let get_last_rev_of_path db rev path =
    let node, _rev = get_node_of_path db rev path in
    Node.get_cur_rev node

  (* may raise UnqualifiedPath *)
  let _get_all_rev_of_path db path =
    let rec aux eid acc path =
      let map =
        match EidMap.find_opt eid db.uid_of_eid with
        | Some m -> m
        | None -> raise UnqualifiedPath
      in
      match path with
      | k :: path ->
          RevisionMap.fold (
            fun _old_rev uid acc ->
              let node = get_node_of_uid db uid in
              match Node.find_opt ~f:(get_node_of_uid db) k node with
              | Some eid2 -> aux eid2 acc path
              | None -> acc
          ) map acc
      | [] ->
          RevisionMap.fold (
            fun _old_rev uid acc ->
              let node = get_node_of_uid db uid in
              let rev = Node.get_cur_rev node in
              if List.mem rev acc then acc
              else rev :: acc
          ) map acc
    in aux start [] (Path.to_list path)

  (* explores all past revisions, even if the path does not exist
     for the current revision; more expensive in this particular case;
     raises UnqualifiedPath if the path does not exist for any revision *)
  let get_all_rev_of_path db path =
   try
      let cur_rev = db.rev in
      let eid, _= get_eid_of_path db cur_rev path in
      let revisions = RevisionMap.keys (EidMap.find eid db.uid_of_eid) in
      let revisions =
        if Option.is_some (Eid.Map.find_opt eid db.tmp_uid_of_eid)
        then cur_rev::revisions else revisions in
     revisions
    with Not_found | UnqualifiedPath ->
      List.rev (_get_all_rev_of_path db path)

  (* returns the sub-tree of the given path *)
  (* may raise UnqualifiedPath *)
  (* TODO rewrite *)
  let get_descendants db path =
    let f = get_node_of_uid db in
    let rec aux accu path node =
      let accu =
        match Node.get_content ~f node with
        | Datas.Data d -> (path, d) :: accu
        | _ -> accu in
      let chld = get_children db db.rev None path in
      if chld = [] then accu
      else
        List.fold_left(
          fun accu (path, _r) ->
            try
              let node, _rev = get_node_of_path db db.rev path in
              match Node.get_content ~f node with
              | Datas.Data _d -> aux accu path node
              | _ -> accu
            with UnqualifiedPath -> accu
        ) accu chld
    in
    let node, _rev = get_node_of_path db db.rev path in
    aux [] path node


  (********************)
  (* basics DB writes *)
  (********************)

  let update_uid_of_eid db uid_list rev =
    List.fold_left (
      fun (acc1, acc2) (eid, uid) ->
        try
          let map = EidMap.find eid db.uid_of_eid in
          let new_map = RevisionMap.add rev uid map in
          EidMap.add eid new_map acc1,
          Eid.Map.add eid uid acc2
            with Not_found ->
              EidMap.add eid (RevisionMap.add rev uid RevisionMap.empty) acc1,
              Eid.Map.add eid uid acc2
              ) (db.uid_of_eid, db.tmp_uid_of_eid) uid_list

  let update_node_of_uid db node_list =
    List.fold_left (
      fun (acc1, acc2) (uid, node) ->
            (UidMap.add uid node acc1), Uid.Map.add uid node acc2
    ) (db.node_of_uid, db.tmp_node_of_uid) node_list

  let print_uid_list l =
    Base.List.to_string
      (fun (eid, uid) -> Printf.sprintf "%s -> %s \n\t\t"
         (Eid.to_string eid) (Uid.to_string uid))
      l

  let print_node_list l =
    Base.List.to_string
      (fun (uid, node) -> Printf.sprintf "%s -> %s \n\t\t"
         (Uid.to_string uid)(Node.to_string node))
      l

  let update_db db rev uid_list node_list =
    #<If:DEBUG_DB$minlevel 10>
      Logger.log ~color:`black
        "update_db( %s , %s )"
          (print_uid_list uid_list)
          (print_node_list node_list)
    #<End>;
    let new_uid_of_eid, tmp_map_1 = update_uid_of_eid db uid_list rev in
    let new_tcount = fst (EidMap.max new_uid_of_eid) in
    let new_node_map, tmp_map_2 = update_node_of_uid db node_list in
    let next_uid =
      try
        let uid1 = fst (Uid.Map.max tmp_map_2) in
        let uid = Uid.make (succ (Uid.value uid1)) in
        Uid.max uid db.next_uid
      with Not_found -> db.next_uid
    in
    let new_tcount = Eid.max new_tcount db.tcount in
    { rev = db.rev
    ; tcount = new_tcount
    ; next_uid = next_uid
    ; uid_of_eid = new_uid_of_eid
    ; node_of_uid = new_node_map
    ; index = db.index
    ; tmp_uid_of_eid = tmp_map_1
    ; tmp_node_of_uid = tmp_map_2
    }

  let remove db rev path key =
    #<If:DEBUG_DB$minlevel 10>
    Logger.log ~color:`yellow
      "DB : Hldb.remove child %s at %s"
        (Keys.to_string key)
        (Path.to_string path)
    #<End>;
    let f = get_node_of_uid db in
    let aux uid eid node =
      let replace = Uid.Map.mem uid db.tmp_node_of_uid in
      (* FIXME replace db.next_uid by the next one? *)
      let nuid, _next_uid =
        if replace
        then
          uid, db.next_uid
        else
          let uid = Uid.succ db.next_uid in
          db.next_uid, uid
      in
      let new_father = Node.remove_child ~f rev node key in
      let uid_list = [(eid, nuid)] in
      let node_list = [(nuid, new_father)] in
      update_db db rev uid_list node_list
    in
    let eid_father, _ = get_eid_of_path db db.rev path in
    let uid_father = get_uid_of_eid db db.rev eid_father in
    let father = get_node_of_uid db uid_father in
    aux uid_father eid_father father

  (* index management *)

  let index_path = Path.add Path.root (Keys.unsafe_make (Keys.IntKey 1001))

  let update_index db update_list =
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
      (* table from key to number of occurrences. When that number equals n, we got a result *)
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
               (fun (k1, r1) (k2, r2) -> let c = compare r1 r2 in if c <> 0 then - c else - Keys.compare k1 k2)
           |> List.tail_map fst
           |> Base.List.uniq)
        results
    in
    merge_search_results results


  (* Stuff from the old hldb.ml (above everything is from the old lldb.ml *)

  let make ?weak () =
    let db = make ?weak () in
    let rev = get_rev db in
    let eid0 = Eid.make 0 and eid1 = Eid.make 1 in
    let uid0 = Uid.make 0 and uid1 = Uid.make 1 in
    let k = Keys.unsafe_make (Keys.IntKey 0) in
    let child = (k, eid1) in
    let root = Node.create rev in
    let root, _ = Node.update ~f:(fun _ -> assert false) uid0 root rev ~child false in
    let dns = Node.create rev in
    let uid_list = [(eid0, uid0); (eid1,uid1)] in
    let node_list = [(uid0, root); (uid1, dns)] in
    update_db db rev uid_list node_list

  (* Links *)
  let set_link db rev path link =
    (* Note: do not check if the [link] path exists: it may come into
       existence later in the same transaction or it might have just vanished.
       It's supposed to be a dynamic description of a node,
       dependent on the current and future state of db. *)
    let db_rev = get_rev db in
    let old_eid, _ = get_eid_of_path db db_rev path in
    let old_uid = get_uid_of_eid db db_rev old_eid in
    let old_node = get_node_of_uid db old_uid in
    let rev_node = Node.get_cur_rev old_node in
    let nuid = if rev_node = rev then old_uid else get_next_uid db in
    let delta = false in
    let content = Datas.Link link in
    let new_node, _=
      Node.update
        ~f:(get_node_of_uid db) old_uid old_node rev ~content delta in
    let uid_list = [old_eid, nuid] in
    let node_list = [nuid, new_node] in
    update_db db rev uid_list node_list

  (* Copies *)
  let set_copy db rev path ?copy_rev link =
    let lrev =
      match copy_rev with
      | Some rev -> rev | None -> rev
    in
    let _ =
      (* Check if link is dangling.
         TODO: this is an overkill, probably, because it checks recursively
         and not only copies, but links as well. *)
      try get db lrev link
      with UnqualifiedPath ->
        let db = clean_tmp_maps db in
        get db lrev link (* Reraise the exception. *)
    in
    let db_rev = get_rev db in
    let old_eid, _ = get_eid_of_path db db_rev path in
    let old_uid = get_uid_of_eid db db_rev old_eid in
    let old_node = get_node_of_uid db old_uid in
    let rev_node = Node.get_cur_rev old_node in
    let nuid = if rev_node = rev then old_uid else get_next_uid db in
    let content = Datas.Copy (Some lrev, link) in
    let new_node = Node.create ~content rev in
    let uid_list = [old_eid, nuid] in
    let node_list = [nuid, new_node] in
    update_db db rev uid_list node_list

  (* See the .mli. *)
  let rec follow_path db rev node path_end =
    #<If:DEBUG_DB$minlevel 10>
      Logger.log ~color:`green
        "DB : low-level following path at rev %s; remaining: %s"
          (Revision.to_string rev)
          (Path.to_string (Path.of_list path_end))
    #<End>;
    match path_end with
    | [] -> ([], node)
    | k :: rest ->
        try
          match Node.get_content ~f:(get_node_of_uid db) node with
          | Datas.Link _
          | Datas.Copy _ -> (path_end, node)
          | _ ->
              let neid = Node.next_eid ~f:(get_node_of_uid db) k node in
              let node = get_node_of_eid db rev neid in
              follow_path db rev node rest
        with Not_found -> raise UnqualifiedPath

  let follow_link db original_rev path =
    #<If:DEBUG_DB$minlevel 10>
      Logger.log ~color:`green
        "DB : following link %s at rev %s"
          (Revision.to_string original_rev)
          (Path.to_string path)
    #<End>; 
    let rec aux db rev path =
      let path_end = Path.to_list path in
      let root = get_node_of_eid db rev root_eid in
      let (path_end, node) = follow_path db rev root path_end in
      match Node.get_content ~f:(get_node_of_uid db) node with
      | Datas.Link l ->
          (* Links possible both on [l] and [path_end], hence the [concat]. *)
          let new_path = Path.concat l (Path.of_list path_end) in
          aux db original_rev new_path
      | Datas.Copy (Some copy_rev, l) ->
          let new_path = Path.concat l (Path.of_list path_end) in
          aux db copy_rev new_path
      | Datas.Copy (None, _) ->
          assert false (* We are at [original_rev]! *)
      | _ ->
          assert (path_end = []);
          (path, node)
    in
    aux db original_rev path

  (* TODO: use the 2 functions below, instead of above, in the future,
     because we unwind paths elsewhere. *)
  (* raise UnqualifiedPath if unable to find a node from its eid and the given
     revision, or if the current key of the path is not found in the map of the
     current node. *)
  let rec get_eid_of_path db rev path =
    let rec aux db pos rev cur_path path =
      match path with
      | [] -> pos, rev
      | k :: tl ->
          try
            let node = get_node_of_eid db rev pos in
            let neid = Node.next_eid ~f:(get_node_of_uid db) k node in
            aux db neid rev (Path.add cur_path k) tl
          with Not_found -> raise UnqualifiedPath
    in aux db start rev Path.root (Path.to_list path)

  (* raise UnqualifiedPath if unable to find the eid from the path or unable to
     find the node associated with the eid if it has been found *)
  (* TODO: the [rev] result does not seem used a lot; use or simplify *)
  let get_node_of_path db rev path =
    let eid, rev = get_eid_of_path db rev path in
    try get_node_of_eid db rev eid, rev
    with Not_found -> raise UnqualifiedPath

  let update_aborted db trdb =
    UidMap.resize db.node_of_uid (Uid.value db.next_uid);
    EidMap.resize db.uid_of_eid (succ (Eid.value db.tcount));
    let rec filter_rev max lst = 
        if Revision.compare (fst (RevisionMap.max lst)) max <= 0 then
          lst
        else
          filter_rev max (RevisionMap.remove_last lst)
    in

        
    Eid.Map.iter (fun k _ -> 
      if Eid.compare k db.tcount <= 0 then
        ignore (EidMap.add k (filter_rev db.rev (EidMap.find k db.uid_of_eid)) db.uid_of_eid)
    )  trdb.tmp_uid_of_eid;

    db
