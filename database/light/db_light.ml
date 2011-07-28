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
module Hashtbl = BaseHashtbl
module Mtl = Mem_tree_light

let sprintf fmt = Printf.sprintf fmt
let eprintf fmt = Printf.eprintf fmt
let printf fmt = Printf.printf fmt
let rev = Revision.make 0

(* -- *)

(* Exceptions *)

exception UnqualifiedPath
exception Merge
exception At_root
exception At_leaf

(* Flags *)
let verify = ref false
let use_od = ref true
let od_early = ref false

(* Datatypes *)

type index = ((Path.t * float) list) StringMap.t

type tree = {
  sts : (Keys.t, tree) Hashtbl.t;
  uid : Uid.t;
  key : Keys.t;
  mutable node : Node_light.t;
  mutable up : tree ref;
  mutable disk : bool;
}

type t = {
  mutable version : string;
  mutable db_filemanager : Io_light.t option;
  mutable tcount : Eid.t;
  mutable next_uid : Uid.t;
  mutable index : index;
  mutable max_size : int;
  mutable mtree : Mtl.mem_tree;
  tree : tree;
}

let string_of_sts sts =
  let l = Hashtbl.fold (fun k _ acc -> k::acc) sts [] in
  String.concat_map ~left:"[" ~right:"]" ~nil:"[]" ";" Keys.to_string l

(* Constructors *)

let create_node ?max_size ?filemanager ?content () =
  match max_size, filemanager with
  | Some _max_size, Some fm ->
      if _max_size < max_int
      then
        let disk_file = Io_light.get_content_file_name fm in
        #<If$minlevel 10>Logger.log ~color:`yellow "stuffing data to %s" disk_file#<End>;
        Node_light.create ~disk_file ?max_size ?content ()
      else
        Node_light.create ?max_size ?content ()
  | _, _ ->
      Node_light.create ?content ()

let make_node t key data =
  t.next_uid <- Uid.succ t.next_uid;
  let tree = { sts = Hashtbl.create 10;
               uid = t.next_uid;
               key = key;
               node = create_node ~max_size:t.max_size ?filemanager:t.db_filemanager ~content:data ();
               up = ref (Obj.magic 0);
               disk = false;
             } in
  tree.up := tree;
  tree

let make_t ?filemanager ?(max_size=max_int) () =
  #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : make_t: filemanager=%s max_size=%d"
                                             (if Option.is_some filemanager then "Some" else "None") max_size#<End>;
  let t = { version = "<new>";
            db_filemanager = filemanager;
            tcount = Eid.make 0;
            next_uid = Uid.make 0;
            tree = { sts = Hashtbl.create 10;
                     uid = Uid.make 0;
                     key = Keys.StringKey "";
                     node = create_node ~max_size ?filemanager ();
                     up = ref (Obj.magic 0);
                     disk = false;
                   };
            mtree = Mtl.make (Keys.StringKey "");
            index = StringMap.empty;
            max_size = max_size;
          } in
  t.tree.up := t.tree;
  t

(* For later, physical copy...
let rec copy_node t parent tree =
  t.next_uid <- Uid.succ t.next_uid;
  let ntree = { sts = Hashtbl.create 10;
                uid = t.next_uid;
                key = tree.key;
                node = tree.node;
                up = ref parent;
                disk = false;
                (*subkeys = tree.subkeys;*)
              } in
  Hashtbl.iter (fun k st -> Hashtbl.add ntree.sts k (copy_node t ntree st)) tree.sts;
  t.tcount <- Eid.succ t.tcount;
  ntree
*)

(* Basic database operations *)

let set_version t version = t.version <- version
let set_filemanager t filemanager = t.db_filemanager <- filemanager
let set_max_size t max_size = t.max_size <- max_size
let set_mtree t mtree = t.mtree <- mtree

let getdbm t =
  match t.db_filemanager with
  | Some io -> io.Io_light.dbm
  | None -> None

let ondemand_read not_quiet t path =
  match getdbm t with
  | Some dbm ->
      (try
         let node = snd (Encode_light.decode_node (Dbm.find dbm (Encode_light.encode_path path)) 0) in
         if not_quiet then
         #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : ondemand read path %s -> %s"
                                                    (Path.to_string path) (Node_light.to_string node)#<End>;
         Some node
       with Not_found ->
         if not_quiet then
         #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : ondemand read path %s -> None" (Path.to_string path)#<End>;
         None)
  | None ->
      #<If>Logger.log ~color:`red "DB-LIGHT : ondemand_read Dbm is closed"#<End>;
      None

let ondemand_prime t path tree =
  #<If$minlevel 30>Logger.log ~color:`yellow "DB-LIGHT : ondemand prime path=%s tree.disk=%b"
                                             (Path.to_string path) tree.disk#<End>;
  if not tree.disk
  then
    ((match ondemand_read true t path with
      | Some node ->
          #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : ondemand prime path=%s to %s"
                                                     (Path.to_string path) (Node_light.to_string node)#<End>;
          tree.node <- node
      | None ->
          #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : ondemand prime path=%s not present"
                                                     (Path.to_string path)#<End>;
          ());
     tree.disk <- true)

let ondemand_add t path node =
  match getdbm t with
  | Some dbm ->
      #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : ondemand add path=%s to %s"
                                                 (Path.to_string path) (Node_light.to_string node)#<End>;
      Dbm.replace dbm (Encode_light.encode_path path) (Encode_light.encode_node node)
  | None ->
      #<If>Logger.log ~color:`red "DB-LIGHT : ondemand_add Dbm is closed"#<End>

let ondemand_remove _what t path =
  match getdbm t with
  | Some dbm ->
      (* TODO: delete file *)
      #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : ondemand removing %s %s" _what (Path.to_string path)#<End>;
      (try Dbm.remove dbm (Encode_light.encode_path path)
       with Dbm.Dbm_error "dbm_delete" -> Logger.log ~color:`red "ondemand_remove: error")
  | None ->
      #<If>Logger.log ~color:`red "DB-LIGHT : ondemand_remove Dbm is closed"#<End>

type od_act =
  | OD_Add of t * Node_light.t
  | OD_Remove of t * string

let string_of_od_act p = function
  | OD_Add (_, n) -> sprintf "Add (%s,%s)" (Path.to_string p) (Node_light.to_string n)
  | OD_Remove (_, what) -> sprintf "Remove (%s,\"%s\")" (Path.to_string p) what

let odacts = ((Hashtbl.create 100):(Path.t, od_act) Hashtbl.t)

let string_of_odacts () =
  let l = Hashtbl.fold (fun p act acc -> (string_of_od_act p act)::acc) odacts [] in
  String.concat_map ~left:"[" ~right:"]" ~nil:"[]" "; " (fun s -> s) l

let add_od_act p act =
  #<If$minlevel 30>
    (match Hashtbl.find_opt odacts p with
     | Some old_act -> eprintf "Replacing OD_ACT: %s -> %s\n%!" (string_of_od_act p old_act) (string_of_od_act p act)
     | None -> ())
  #<End>;
  Hashtbl.replace odacts p act

let same_t t1 t2 =
  match (t1.db_filemanager, t2.db_filemanager) with
  | Some fm1, Some fm2 -> fm1.Io_light.location = fm2.Io_light.location
  | _, _ -> false

let od_read not_quiet t path =
  if !use_od
  then
    (match Hashtbl.find_opt odacts path with
     | Some (OD_Add (tt, node)) -> if same_t t tt then Some node else ondemand_read not_quiet tt path
     | Some (OD_Remove (tt, _)) -> if same_t t tt then None else ondemand_read not_quiet tt path
     | None -> ondemand_read not_quiet t path)
  else ondemand_read not_quiet t path

let od_add t path node =
  if !use_od
  then add_od_act path (OD_Add (t, node))
  else ondemand_add t path node

let od_rmv what t path =
  if !use_od
  then add_od_act path (OD_Remove (t, what))
  else ondemand_remove what t path

let action_od () =
  if !use_od
  then
    ((*eprintf "od_acts: %s\n%!" (string_of_odacts ());*)
      Hashtbl.iter (fun p -> function
                    | OD_Add (t, n) -> ondemand_add t p n
                    | OD_Remove (t, what) -> ondemand_remove what t p) odacts;
      Hashtbl.clear odacts)

let rec ondemand_remove_subtree t path tree_opt =
  (*eprintf "ondemand_remove_subtree %s tree=%s\n%!"
          (Path.to_string path) (Option.to_string (fun tree -> Uid.to_string tree.uid) tree_opt);*)
  (match Mtl.find_mtree_sks t.mtree path with
   | Some sks ->
       (*eprintf "ondemand_remove_subtree: sks=%s\n%!" (String.concat_map ~left:"[" ~right:"]" "; " Keys.to_string sks);*)
       List.iter
         (fun k ->
            ondemand_remove_subtree t (Path.add path k)
              (match tree_opt with
               | Some tree -> (try Some (Hashtbl.find tree.sts k) with Not_found -> None)
               | None -> None)) sks;
   | None -> ());
 od_rmv "subtree" t path

(*
let refresh_data t path node tree =
  if tree.disk
  then ((*eprintf "refresh_data: path=%s content=%s node=%s subkeys=%s ks=%s\n%!"
          (Path.to_string path) (Datas.to_string (Node_light.get_content tree.node)) (Node_light.to_string node)
          (string_of_keyset tree.subkeys) (string_of_keyset ks);*)
        if not (Node_light.equals tree.node node) then od_add t path node)
  else (match od_read true t path with
        | Some nodes ->
            (*eprintf "refresh_data: path=%s nodes=%s node=%s kss=%s ks=%s\n%!"
              (Path.to_string path) (Node_light.to_string nodes) (Node_light.to_string node) (string_of_keyset kss) (string_of_keyset ks);*)
            if not (Node_light.equals nodes node) then od_add t path node
        | None -> od_add t path node);
  tree.node <- node;
  (*Node_light.set_content ~max_size:t.max_size tree.node data;*)
  tree.disk <- true;
  Mtl.refresh_mtree t.mtree path (Node_light.is_occupied node)
*)

let verify_mtree_at_path t path =
  match getdbm t with
  | Some dbm ->
      let sks =
        match Mtl.find_mtree_sks t.mtree path with
        | Some sks -> sks
        | None -> []
      in
      List.for_all (fun k ->
                      (try ignore (Dbm.find dbm (Encode_light.encode_path (Path.add path k))); true
                       with Not_found -> false)) sks
  | None -> false

let verify_mtree t =
  match getdbm t with
  | Some dbm ->
      Mtl.fold
        (fun path _k data valid ->
           valid &&
             (try
                let datastr = Dbm.find dbm (Encode_light.encode_path path) in
                let node = snd (Encode_light.decode_node datastr 0) in
                let res = data = (node.Node_light.content <> Datas.UnsetData) in
                if not res then Logger.log ~color:`red "verify_mtree: fails on path %s" (Path.to_string path);
                res
              with Not_found -> false))
        true t.mtree Path.root
  | None -> false

let verify_database t =
  match getdbm t with
  | Some dbm ->
      let disk_pathnodes =
        let pathnodes = ref [] in
        Dbm.iter (fun pathstr nodestr ->
                    match pathstr with
                    | "version" | "ondemand" | "max_size" | "timestamp" | "lock_pid" | "lock_hostname" ->
                        ()
                    | _ ->
                        let path = snd (Encode_light.decode_path pathstr 0) in
                        let node = snd (Encode_light.decode_node nodestr 0) in
                        pathnodes := (path,node)::!pathnodes
                 ) dbm;
        !pathnodes
      in
      let dpaths = List.map (fun (p,_) -> p) disk_pathnodes in
      let mem_pathdatas = Mtl.fold (fun p _k d pds -> (p,d)::pds) [] t.mtree Path.root in
      let mpaths = List.map (fun (p,_) -> p) mem_pathdatas in
      let module PS = Set.Make(Path) in
      let dpset = List.fold_right PS.add dpaths PS.empty in
      let mpset = List.fold_right PS.add mpaths PS.empty in
      let not_on_disk = PS.diff mpset dpset in
      let not_in_mem = PS.diff dpset mpset in
      if (PS.is_empty not_on_disk) && (PS.is_empty not_in_mem)
      then (let pncompare (p1,_) (p2,_) = Path.compare p1 p2 in
            let pdcompare (p1,_) (p2,_) = Path.compare p1 p2 in
            let dpns = List.sort pncompare disk_pathnodes in
            let dpds = List.sort pdcompare mem_pathdatas in
            List.iter2 (fun (p1,n) (p2,d) ->
                          if Path.compare p1 p2 <> 0
                          then Logger.error "verify_database: path mismatch %s %s" (Path.to_string p1) (Path.to_string p2)
                          else
                            (if d <> Node_light.is_occupied n
                             then Logger.log ~color:`red "verify_database: data mismatch on path %s node=%s mdata=%b"
                                                         (Path.to_string p1) (Node_light.to_string n) d))
              dpns dpds;
            Logger.log ~color:`green "verify_database: verifies");
      if not (PS.is_empty not_on_disk)
      then (let nodl = PS.fold (fun p ps -> p::ps) not_on_disk [] in
            Logger.log ~color:`red "verify_database: not_on_disk=[%s]" (String.concat_map "; " Path.to_string nodl));
      if not (PS.is_empty not_in_mem)
      then (let niml = PS.fold (fun p ps -> p::ps) not_in_mem [] in
            Logger.log ~color:`red "verify_database: not_in_mem=[%s]" (String.concat_map "; " Path.to_string niml))
  | None ->
      Logger.log ~color:`red "verify_database: Dbm file is closed!"

let verify_data t path tree_opt =
  let msg =
    match tree_opt with
    | Some tree ->
        (match od_read false t path with
         | Some node ->
             sprintf "verify_data(disk=%b): path=%s\n" tree.disk (Path.to_string path)^
             (if Node_light.equals tree.node node
              then sprintf "  data: OK=%s\n%!" (Node_light.to_string node)
              else sprintf "  data: MEM=%s\n        DSK=%s\n%!" (Node_light.to_string tree.node) (Node_light.to_string node))
       | None ->
           sprintf "verify_data(disk=%b): path=%s\n" tree.disk (Path.to_string path)^
           (if Node_light.equals_data tree.node Datas.UnsetData
            then sprintf "  data: OK=%s\n%!" (Node_light.to_string tree.node)
            else sprintf "  data: MEM=%s\n%!" (Node_light.to_string tree.node)))
  | None ->
      (match od_read false t path with
       | Some node ->
           sprintf "verify_data(no tree): path=%s\n" (Path.to_string path)^
           sprintf "  data: MEM=None\n        DSK=%s\n%!" (Node_light.to_string node)
       | None ->
           sprintf "verify_data(no tree): path=%s\n" (Path.to_string path)^
           sprintf "  data: OK=None\n%!")
  in
  Logger.log ~color:`red "%s" msg

let verifies t path = function
  | Some tree ->
      (match od_read false t path with
       | Some node -> Node_light.equals node tree.node
       | None -> not (Node_light.is_occupied tree.node))
  | None ->
      (match od_read false t path with
       | Some _ -> false
       | None -> true)

let update_data t path data tree =
  (*eprintf "update_data: path=%s ks=%s data=%s tree=%d\n%!"
          (Path.to_string path) (string_of_keyset ks) (Datas.to_string data) (Uid.value tree.uid);*)
  let _old_data = Node_light.get_content tree.node in
  (if not tree.disk
   then (match od_read true t path with
         | Some nodes ->
             (*eprintf "nodes=%s data=%s kss=%s ks=%s\n%!"
                         (Node_light.to_string nodes) (Datas.to_string data) (string_of_keyset kss) (string_of_keyset ks);*)
             if not (Node_light.equals_data nodes data)
             then (Node_light.set_content ~max_size:t.max_size tree.node data;
                   od_add t path tree.node)
         | None ->
             Node_light.set_content ~max_size:t.max_size tree.node data;
             od_add t path tree.node)
   else
     if not (Node_light.equals_data tree.node data)
     then (Node_light.set_content ~max_size:t.max_size tree.node data;
           od_add t path tree.node));
  tree.disk <- true;
  #<If$minlevel 3>Logger.log ~color:`yellow "DB-LIGHT : update_data: data=%s old_data=%s, using %s data"
                                            (Datas.to_string data) (Datas.to_string _old_data)
                                            (if Node_light.equals_data tree.node data then "new" else "old")#<End>

let add_tree ?(no_write=false) t path data =
  #<If>Logger.log ~color:`yellow "DB-LIGHT : add_tree: path=%s data=%s" (Path.to_string path) (Datas.to_string data)#<End>;
  let rec aux pt here tree = function
    | [] ->
        tree.up := pt;
        if no_write
        then Node_light.set_content ~max_size:t.max_size tree.node data
        else update_data t path data tree
    | k::rest ->
        (try
           let st = Hashtbl.find tree.sts k in
           aux tree (Path.add here k) st rest
         with Not_found ->
           let st = make_node t k Datas.UnsetData in
           st.up <- ref tree;
           od_add t here tree.node;
           Hashtbl.add tree.sts k st;
           aux tree (Path.add here k) st rest)
  in
  aux t.tree Path.root t.tree (Path.to_list path);
  if !od_early then action_od ();
  t.tcount <- Eid.succ t.tcount;
  Mtl.add_mtree t.mtree path data

let add_bare_tree t tree k =
  let st = make_node t k Datas.UnsetData in
  st.up <- ref tree;
  Hashtbl.add tree.sts k st;
  st

let rec find_st t path tree k =
  if !verify
  then (ignore (verify_mtree t);
        if not (verifies t path (Some tree)) then verify_data t path (Some tree));
  try
    #<If$minlevel 30>Logger.log ~color:`yellow "DB-LIGHT : find_st: trying sts(%s)" (string_of_sts tree.sts)#<End>;
    let st = Hashtbl.find tree.sts k in
    #<If$minlevel 30>Logger.log ~color:`yellow "DB-LIGHT : find_st: path=%s tree=%d k=%s from sts %d (st.disk=%b)"
                                               (Path.to_string path) (Uid.value tree.uid)
                                               (Keys.to_string k) (Uid.value st.uid) st.disk#<End>;
    if st.disk
    then st
    else (ondemand_prime t (Path.add path k) st;
          st)
  with Not_found ->
    #<If$minlevel 30>
      let sks =
        match Mtl.find_mtree t.mtree path with
        | Some mtree -> Hashtbl.fold (fun k _ ks -> k::ks) mtree.Mtl.msts []
        | None -> []
      in
      Logger.log ~color:`yellow "DB-LIGHT : find_st: trying subkeys([%s])" (String.concat_map "; " Keys.to_string sks);
      Logger.log ~color:`yellow "DB-LIGHT : find_st: tree.disk=%b" tree.disk
    #<End>;
    if tree.disk
    then 
      if (match Mtl.find_mtree t.mtree path with
          | Some mtree -> Hashtbl.mem mtree.Mtl.msts k
          | None -> false)
      then
        let st = add_bare_tree t tree k in
        ondemand_prime t (Path.add path k) st;
        #<If$minlevel 30>Logger.log ~color:`yellow "DB-LIGHT : find_st: from prime %d" (Uid.value st.uid)#<End>;
        st
      else raise Not_found
    else (#<If$minlevel 30>Logger.log ~color:`yellow "DB-LIGHT : find_st: priming %s %d"
                                                     (Path.to_string path) (Uid.value tree.uid)#<End>;
          ondemand_prime t path tree;
          find_st t path tree k)

let remove_tree t path =
  #<If>Logger.log ~color:`yellow "DB-LIGHT : remove_tree: path=%s" (Path.to_string path)#<End>;
  let rec aux here tree mtree kl =
    (*eprintf "remove_tree(aux): here=%s tree=%d kl=[%s]\n%!"
            (Path.to_string here) (Uid.value tree.uid) (String.concat_map ";" Keys.to_string kl);*)
    match kl with
    | [] -> false
    | [k] ->
        (try
           let st = find_st t here tree k in
           #<If$minlevel 2>Logger.log ~color:`yellow
                                      "DB-LIGHT : remove_tree(rmv): path=%s data=%s"
                                      (Path.to_string path) (Datas.to_string (Node_light.get_content st.node))#<End>;
           (*eprintf "remove_tree: here_path=%s\n%!" (Path.to_string (Path.add here k));*)
           ondemand_remove_subtree t path (Some st);
           #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : remove_tree: here=%s remove key %s from tree %d"
                                                      (Path.to_string here) (Keys.to_string k) (Uid.value tree.uid)#<End>;
           (*refresh_data t here tree.node tree;*)
           Node_light.delete st.node;
           Hashtbl.remove tree.sts k;
           Hashtbl.remove mtree.Mtl.msts k;
           true
         with Not_found -> false)
    | k::rest ->
        (try
           let st = find_st t here tree k in
           let mst = Hashtbl.find mtree.Mtl.msts k in
           (*eprintf "remove_tree: at key=%s st=%d\n%!" (Keys.to_string k) (Uid.value st.uid);*)
           let removed = aux (Path.add here k) st mst rest in
           (*eprintf "remove_tree: at key=%s st=%d removed=%b #sts=%d content=%s\n%!"
                   (Keys.to_string k) (Uid.value st.uid)
                   removed (Hashtbl.length st.sts) (Datas.to_string (Node_light.get_content st.node));*)
           if removed && Hashtbl.length mst.Mtl.msts = 0 && Node_light.equals_data st.node Datas.UnsetData
           then ((*eprintf "remove_tree: here=%s remove key %s from tree %d\n%!"
                         (Path.to_string here) (Keys.to_string k) (Uid.value tree.uid);*)
                 (*refresh_data t here tree.node tree; (* <-- TODO: Check if this is redundant (no keyset any more) *)*)
                 #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : remove_tree: here=%s remove k=%s from tree=%d sts"
                                                            (Path.to_string here) (Keys.to_string k) (Uid.value tree.uid)#<End>;
                 od_rmv "path" t (Path.add here k);
                 (*eprintf "remove_tree: removing %s\n%!" (Path.to_string (Path.add here k));*)
                 Node_light.delete st.node;
                 Hashtbl.remove tree.sts k;
                 Hashtbl.remove mtree.Mtl.msts k);
           removed
         with Not_found -> false)
  in
  let removed = aux Path.root t.tree t.mtree (Path.to_list path) in
  if !od_early then action_od ();
  (match removed, Eid.pred t.tcount with
   | true, Some eid -> t.tcount <- eid
   | _, _ -> ());
  removed

(* Node-level navigation:
   Note that we can't export this yet because the Badop.S sig doesn't support it.
*)

let node_uid node = node.uid
let node_key node = node.key
let node_node node = node.node
let node_up node = !(node.up)

let node_is_root node = !(node.up) == node

let path_from_node node =
  let rec aux node l =
    if node_is_root node
    then Path.of_list l
    else aux !(node.up) (node.key::l)
  in
  aux node []

let node_is_leaf t node =
  (*eprintf "node_is_leaf: path=%s\n%!" (Path.to_string (path_from_node node));*)
  (match Mtl.find_mtree t.mtree (path_from_node node) with
   | Some mtree -> Hashtbl.length mtree.Mtl.msts = 0
   | None -> true)

let up_node node = if node_is_root node then raise At_root else !(node.up)

let up_node_n node n =
  let rec aux tree = function
    | 0 -> tree
    | n -> aux (up_node tree) (n-1)
  in
  aux node n

let up_node_opt node = try Some (up_node node) with At_root -> None

let down_path t path =
  #<If$minlevel 20>Logger.log ~color:`yellow "DB-LIGHT : down_path %s" (Path.to_string path)#<End>;
  let rec aux here tree kl =
    #<If$minlevel 20>Logger.log ~color:`yellow "DB-LIGHT : down_path here=%s kl=[%s]"
                                               (Path.to_string here) (String.concat_map "; " Keys.to_string kl)#<End>;
    match kl with
    | [] -> tree
    | k::rest ->
        (*eprintf "down_path: trying find_st here=%s tree=%d k=%s\n%!"
                (Path.to_string here) (Uid.value tree.uid) (Keys.to_string k);*)
        let st = find_st t here tree k in
        (*eprintf "down_path: found st=%d\n%!" (Uid.value st.uid);*)
        aux (Path.add here k) st rest
  in
  aux Path.root t.tree (Path.to_list path)

let down_node t node key = if node_is_leaf t node then raise At_leaf else find_st t (path_from_node node) node key

let down_node_opt t node key = try Some (down_node t node key) with | Not_found -> None | At_leaf -> None

let find_node t path =
  #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : find_node: path=%s" (Path.to_string path)#<End>;
  let tree = down_path t path in
  if not tree.disk then ondemand_prime t path tree;
  tree

let find_node_opt t path =
  #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : find_node_opt: path=%s" (Path.to_string path)#<End>;
  try Some (find_node t path) with Not_found -> None

let find_data t path =
  #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : find_data: path=%s" (Path.to_string path)#<End>;
  Node_light.get_content (find_node t path).node

let find_data_opt t path = try Some (find_data t path) with Not_found -> None

let string_of_node { sts=_; uid; key; node; up; } =
  sprintf "%d(^%dvs): %s -> %s"
    (Uid.value uid) (Uid.value ((!up).uid)) (*string_of_keyset subkeys*)
    (Keys.to_string key) (Datas.to_string (Node_light.get_content node))

let rec string_of_tree0 indent node =
  let s = sprintf "%s%s\n" indent (string_of_node node) in
  Hashtbl.fold
    (fun k v acc ->
       sprintf "%s%s%s ->\n%s%s" acc indent (Keys.to_string k) indent (string_of_tree0 (indent^" ") v))
    node.sts s
let string_of_tree = string_of_tree0 ""

let print_t t = printf "mtree: \n%s\ntree: %s\n%!" (Mtl.string_of_mtree t.mtree) (string_of_tree t.tree)

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

  let get_index _db = assert false (*db.index*)
  let get_mtree db = db.mtree


  (*****************************)
  (* navigation through the db *)
  (*****************************)

  let get_tree_of_path db path =
    #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : get_tree_of_path: path=%s" (Path.to_string path)#<End>;
    try find_node db path
    with Not_found -> raise UnqualifiedPath

  let get_node_of_path db path =
    #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : get_node_of_path: path=%s" (Path.to_string path)#<End>;
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
    let node, _rev = get_node_of_path db path in
    let res =
    match Node_light.get_content node with
    | Datas.Data d -> d
    | Datas.Link p
    | Datas.Copy (_, p) -> get db p
    | Datas.UnsetData -> DataImpl.empty
    in
    #<If$minlevel 20>Logger.log ~color:`yellow "DB-LIGHT : get: path=%s returning data=%s"
                                               (Path.to_string path) (DataImpl.to_string res)#<End>;
    res

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
      (*eprintf "get_ch: path=%s len=%d is_root=%b\n%!" (Path.to_string path) len (path = Path.root);*)
      let inrange, tree, start =
        if path = Path.root
        then (true, db.tree, ([], len))
        else (in_range range (Path.last path) len, tree, start)
      in
      if inrange
      then
        (if not tree.disk then ondemand_prime db path tree;
         List.fold_right
           (fun key (pl,pllen) ->
              let sn = find_st db path tree key in
              let spl,spllen =
                (*eprintf "get_ch: depth=%d max_depth=%d\n%!" depth max_depth;*)
                if depth < max_depth
                then
                  let spath = Path.add path key in
                  (*eprintf "get_ch: spath=%s allow_empty=%b occupied=%b\n%!"
                          (Path.to_string spath) allow_empty (Node_light.is_occupied sn.node);*)
                  let start = if allow_empty || Node_light.is_occupied sn.node then ([spath],pllen+1) else ([],pllen) in
                  aux sn spath pllen start (depth+1)
                else ([],pllen)
              in
              (pl@spl,spllen)) (match Mtl.find_mtree_sks db.mtree path with
                                | Some sks -> sks
                                | None -> assert false) start)
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
    #<If$minlevel 20>Logger.log ~color:`yellow "DB-LIGHT : get_children: %s -> [%s]"
                                                        (Path.to_string path) (String.concat_map "; " Path.to_string ch)#<End>;
    ch

  (* won't raise UnqualifiedPath *)
  let get_all_children db range_opt path =
    let ch = _get_children db range_opt path max_int false false in
    #<If$minlevel 20>Logger.log ~color:`yellow "DB-LIGHT : get_all_children: %s -> [%s]%!"
                                                        (Path.to_string path) (String.concat_map "; " Path.to_string ch)#<End>;
    ch

  (********************)
  (* basics DB writes *)
  (********************)

  let update ?no_write db path data = add_tree ?no_write db path data; db

  let remove db path = ignore (remove_tree db path); db

  (* index management *)

  let update_index db _update_list = db
(*
    #<If$minlevel 3>
      Logger.log ~color:`yellow
        "DB-LIGHT : update_index: [%s]"
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
*)

  let remove_from_index db _remove_list = db
(*
    #<If$minlevel 3>
      Logger.log ~color:`yellow
        "DB-LIGHT : remove_from_index: [%s]"
        (String.concat_map "; " (fun (p,d) -> sprintf "(%s,%s)" (Path.to_string p) (DataImpl.to_string d)) remove_list)
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
*)


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
    add_tree db path (Datas.Link link);
    db

  (* Copies *)

  (* Just behave like links for now... *)
  let set_copy db path link =
    add_tree db path (Datas.Copy (None, link));
    db

  (*Unfinished...
    let set_physical_copy db path link =
    let tree = get_tree_of_path db path in
    let target = get_tree_of_path db link in
    tree.node.Node_light.content <- Node_light.get_content copy.node;
    tree.node.Node_light.content <- Datas.Copy (Some rev, link);
    db*)

  let rec follow_path (db:t) node path_end =
    #<If$minlevel 10>
      Logger.log ~color:`green "DB-LIGHT : low-level following path; remaining: %s"
                               (Path.to_string (Path.of_list path_end))
    #<End>;
    match path_end with
    | [] -> ([], node)
    | k :: rest ->
        try
          match Node_light.get_content node.node with
          | Datas.Link _
          | Datas.Copy _ -> (path_end, node)
          | _ ->
              let node = find_st db (path_from_node node) node k in
              follow_path db node rest
        with Not_found -> raise UnqualifiedPath

  let follow_link db path =
    #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : follow_link: path=%s" (Path.to_string path)#<End>;
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
let dbl = ref []
let file = "/tmp/db_light_self_test"

let dodb file f =
  let db = Dbm.opendbm file [(*Dbm.Dbm_create;*) Dbm.Dbm_rdwr] 0O664 in
  let res = f db in
  Dbm.close db;
  res

let km1 = Keys.IntKey (-1)
let k0 = Keys.IntKey 0
let k1 = Keys.IntKey 1
let k2 = Keys.IntKey 2
let path = Path.of_list ([Keys.IntKey 1; Keys.IntKey 0; Keys.IntKey 2; Keys.IntKey 1; Keys.IntKey 583955; Keys.IntKey 0])
let path1 = Path.of_list ([k1])
let path20 = Path.of_list ([k2;k0])
let path2m1 = Path.of_list ([k2;km1])
let path10 = Path.add path1 k0
let path102 = Path.add path10 k2
let path1021 = Path.add path102 k1
let path1021n n = Path.add path1021 (Keys.IntKey n)
let path1021mn m n = Path.add (Path.add path1021 (Keys.IntKey m)) (Keys.IntKey n)

let rawfind file str = dodb file (fun db -> Dbm.find db str)
let find file path = dodb file (fun db -> snd (Encode_light.decode_node (Dbm.find db (Encode_light.encode_path path)) 0))

let set_dbl file decode =
  let dbl = ref [] in
  dodb file (fun db ->
  Dbm.iter (fun k d ->
              (match k with
               | "version" -> ()
               | "timestamp" -> ()
               | _ -> dbl := (Path.to_string (snd (Encode_light.decode_path k 0)),
                              snd (decode d 0))::!dbl);
              (*print_endline (String.escaped (Printf.sprintf "%s -> %s" k d)) *) ) db);
  !dbl

let set_dbld file = set_dbl file Encode_light.decode_datas
let set_dbln file = set_dbl file Encode_light.decode_kln

let all_disk_nodes file =
  let nodes = ref [] in
  dodb file (fun db ->
  Dbm.iter (fun k _d ->
              (match k with
               | "version" -> ()
               | "timestamp" -> ()
               | _ -> nodes := ((snd (Encode_light.decode_path k 0))::!nodes))) db);
  !nodes

let cleardb file =
  let db = Dbm.opendbm file [(*Dbm.Dbm_create;*) Dbm.Dbm_rdwr] 0O664 in
  let keys = ref [] in
  Dbm.iter (fun k _ -> keys := k::!keys) db;
  List.iter (Dbm.remove db) !keys;
  Dbm.close db

let verify n t path =
  let node_opt = find_node_opt t path in
  if not (verifies t path node_opt) then (eprintf "%d) " n; verify_data t path node_opt)
let verifies t path = verifies t path (find_node_opt t path)
let verify_all n t = List.iter (fun p -> verify n t p)
let all_verify t = List.for_all (fun p -> verifies t p)

let _ = 
  let filemanager = Io_light.make Io_light.Create file in
  let tt = make_t ~filemanager () in
  let _K_a = Keys.StringKey "a" in
  let _K_b = Keys.StringKey "b" in
  let _K_c = Keys.StringKey "c" in
  let _K_d = Keys.StringKey "d" in
  let _K_e = Keys.StringKey "e" in
  let _K_f = Keys.StringKey "f" in
  let _K_g = Keys.StringKey "g" in
  let _K_h = Keys.StringKey "h" in
  let _K_i = Keys.StringKey "i" in
  let _K_x = Keys.StringKey "x" in
  let _K_y = Keys.StringKey "y" in
  let _K_z = Keys.StringKey "z" in
  let a = Path.of_list [_K_a] in
  let ab = Path.of_list [_K_a; _K_b] in
  let abc = Path.of_list [_K_a; _K_b; _K_c] in
  let abd = Path.of_list [_K_a; _K_b; _K_d] in
  let d = Path.of_list [_K_d] in
  let de = Path.of_list [_K_d; _K_e] in
  let def = Path.of_list [_K_d; _K_e; _K_f] in
  let defg = Path.of_list [_K_d; _K_e; _K_f; _K_g] in
  let defghi = Path.of_list [_K_d; _K_e; _K_f; _K_g; _K_h; _K_i] in
  let xyz = Path.of_list [_K_x; _K_y; _K_z] in
  let pl = [Path.root;a;ab;abc;abd;d;de;def] in
  let vfy n pl = List.iter (verify n tt) pl in
  eprintf "add abc\n%!"; add_tree tt abc (Datas.Data (DataImpl.Int 123)); print_t tt; vfy 1 pl;
  tt_ref := tt;
  eprintf "add abd\n%!"; add_tree tt abd (Datas.Data (DataImpl.Int 124)); print_t tt; vfy 2 pl;
  eprintf "add a\n%!"; add_tree tt a (Datas.Data (DataImpl.Int 1)); print_t tt; vfy 3 pl;
  eprintf "add defghi\n%!"; add_tree tt defghi (Datas.Data (DataImpl.Int 456789)); print_t tt; vfy 4 pl;
  printf "get_eid(tt)=%d\n%!" (Eid.value tt.tcount);
  printf "find_data(abc)=%s\n%!" (Option.to_string Datas.to_string (find_data_opt tt abc));
  printf "find_node(abc)=%s\n%!" (Option.to_string (fun tree -> Uid.to_string tree.uid) (find_node_opt tt abc));
  printf "node_is_root(tt.tree)=%b\n%!" (node_is_root tt.tree);
  printf "node_is_root(find_node(abc))=%b\n%!" (node_is_root (Option.get (find_node_opt tt abc)));
  eprintf "remove defg\n%!"; ignore (remove_tree tt defg); print_t tt; vfy 5 pl;
  eprintf "link de -> ab\n%!"; ignore (set_link tt de ab); print_t tt; vfy 6 pl;
  eprintf "add def\n%!"; add_tree tt def (Datas.Data (DataImpl.Int 456)); print_t tt; vfy 7 pl;
  eprintf "copy de -> ab\n%!"; ignore (set_copy tt de ab); print_t tt; vfy 8 pl;
  eprintf "unset de\n%!"; add_tree tt de Datas.UnsetData; print_t tt; vfy 9 pl;
  let node_ab = find_node_opt tt ab in
  printf "node_ab=%s\n%!" (Option.to_string string_of_node node_ab);
  printf "down_node(node_ab,\"c\")=%s\n%!" (Option.to_string string_of_node (down_node_opt tt (Option.get node_ab) _K_c));
  printf "up_node(node_ab)=%s\n%!" (Option.to_string string_of_node (up_node_opt (Option.get node_ab)));
  printf "up_node(tt.tree)=%s\n%!" (Option.to_string string_of_node (up_node_opt tt.tree));
  printf "find_data(abd)=%s\n%!" (Option.to_string Datas.to_string (find_data_opt tt abd));
  printf "find_data(a)=%s\n%!" (Option.to_string Datas.to_string (find_data_opt tt a));
  printf "find_data(def)=%s\n%!" (Option.to_string Datas.to_string (find_data_opt tt def));
  printf "find_data(xyz)=%s\n%!" (Option.to_string Datas.to_string (find_data_opt tt xyz));
  printf "get_children(root)=[%s]\n%!"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_children tt (Some (None,0)) Path.root));
  printf "get_children(a)=[%s]\n%!"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_children tt (Some (None,0)) a));
  printf "get_children(a,<=c)=[%s]\n%!"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_children tt (Some (Some _K_c,0)) a));
  printf "get_children(a,3)=[%s]\n%!"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_children tt (Some (None,3)) a));
  printf "get_children(ab,>=b)=[%s]\n%!"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_children tt (Some (Some _K_b,-3)) ab));
  printf "get_all_children(root)=[%s]\n%!"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_all_children tt (Some (None,0)) Path.root));
  printf "get_all_children(a)=[%s]\n%!"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_all_children tt (Some (None,0)) a));
  printf "get_all_children(a,<=c)=[%s]\n%!"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_all_children tt (Some (Some _K_c,0)) a));
  printf "get_all_children(a,3)=[%s]\n%!"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_all_children tt (Some (None,3)) a));
  printf "get_all_children(ab,>=b)=[%s]\n%!"
         (List.to_string (fun p -> Path.to_string p^"; ") (get_all_children tt (Some (Some _K_b,-3)) ab));
  eprintf "remove abc\n%!"; ignore (remove_tree tt abc); print_t tt; vfy 10 pl;
  eprintf "remove abd\n%!"; ignore (remove_tree tt abd); print_t tt; vfy 11 pl;
  eprintf "remove a\n%!"; ignore (remove_tree tt a); print_t tt; vfy 12 pl;
  eprintf "remove def\n%!"; ignore (remove_tree tt def); print_t tt; vfy 13 pl;
  Io_light.close filemanager;
  dbl := set_dbln file
*)

