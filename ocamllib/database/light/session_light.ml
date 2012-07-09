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

(* Debug *)

#<Debugvar:DEBUG_DB>

(* Functor *)

module Session_light_ (DB : DbSig.DB) =
struct

  (* Built modules *)
  module Transaction_light = Transaction_light.Transaction_light_(DB)
  module Db_light = Transaction_light.Db_light
  module Io_light = Db_light.Io_light
  module DB = Io_light.DB

  (* Exports *)
  exception UnqualifiedPath = Db_light.UnqualifiedPath

  (* Depends *)
  module String = BaseString
  module List = BaseList

  (* Shorthands *)
  type 'a intmap = 'a IntMap.t
  module Tr = Transaction_light
  let eprintf fmt = Printf.eprintf fmt
  let sprintf fmt = Printf.sprintf fmt

  (* Types *)
  (* The queue of transaction numbers, stored in order of appearance,
     helps in choosing the next prepare to do (the longest waiting).
     TODO: it's imperative; perhaps do this functionally? *)
  type tr_FIFO = (Tr.t * (Tr.t * bool -> unit)) Queue.t

  let is_empty_FIFO queue = Queue.is_empty queue

  let create_FIFO () = Queue.create ()

  let add_FIFO trans k queue = Queue.add (trans, k) queue

  let take_FIFO queue = Queue.take queue

  type lock = (Tr.t * Db_light.t) option
      (* It stores a transaction and the new db after applying it,
         which will become official if the commit of the
         transaction is requested and succeeds. Whichever part of the code
         releases the lock is responsible for taking the oldest transaction
         from the waiting FIFO and preparing it. *)

  type t = { mutable trans_num : int     (* counter for fresh transaction serial numbers *)
           ; mutable db_ref : Db_light.t (* the reference db passed to new transactions *)
           ; with_dot : bool (* Not used *)
           ; with_ondemand : bool (* No db preload, load values on access *)
           ; with_direct : bool (* Direct disk write *)
           ; with_max_size : int (* Save data bigger than this in files *)
           ; is_weak : bool (* Not used *)
           ; file_manager : Io_light.t
           ; mutable session_lock : lock
           ; waiting_FIFO : tr_FIFO
             (* The queue stores the waiting transactions with their revisions,
                as well as the continuations to execute asynchronously,
                when prepare of the transactions is over
                (usually the continuation will execute a commit operation
                or send a confirmation to the client, which may then request
                the commit operation). *)
          }

  (* Exceptions *)
  exception Open of (t option * string)
  exception DiskError of string


  (*******************)
  (* ecriture disque *)
  (*******************)

  let write_trans dbm trans =
    List.iter
      (fun (_i,path,query) ->
         match query with
         | Tr.Set datas ->
             #<If>Logger.log ~color:`magenta "DB-LIGHT(%d) : updating path %s to %s"
                                             _i (Path.to_string path) (Datas.to_string datas)#<End>;
             DB.replace dbm (Encode_light.encode_path path) (Encode_light.encode_datas datas)
         | Tr.Remove path ->
             #<If>Logger.log ~color:`magenta "DB-LIGHT(%d) : (qm) removing path %s" _i (Path.to_string path)#<End>;
             try DB.remove dbm (Encode_light.encode_path path)
             with DB.DB_error "dbm_delete" -> ())
      (Tr.get_sorted_queries trans);
    List.iter (fun path ->
                 #<If>Logger.log ~color:`magenta "DB-LIGHT : (rl) removing path %s" (Path.to_string path)#<End>;
                 try DB.remove dbm (Encode_light.encode_path path)
                 with DB.DB_error "dbm_delete" -> ()
              ) (List.rev trans.Tr.tr_remove_list)

  let disk_writing t trans =
    if not t.with_ondemand
    then 
      match Io_light.get_dbm t.file_manager with
      | Some dbm ->
          (try
             t.file_manager.Io_light.timestamp <- Time.now ();
             write_trans dbm trans
           with e -> (
             let cause = Printexc.to_string e in
             let bt = Printexc.get_backtrace() in
             #<If>Logger.log ~color:`red "DB-LIGHT : error during disk writing for\n%s\n%s" cause bt#<End>;
             raise (DiskError (sprintf "%s\n%s" cause bt))))
      | None ->
          #<If>Logger.log ~color:`red "DB-LIGHT : warning Dbm closed during disk writing transaction #%d"
                                      (Tr.get_num trans)#<End>
    else
      #<If>Logger.log ~color:`red "DB-LIGHT : No disk write for (ondemand) transaction #%d"
                                  (Tr.get_num trans)#<End>
      

  (************************)
  (* timestamps managment *)
  (************************)

  let get_timestamp t =
    let timestamp = Io_light.get_timestamp t.file_manager in
    #<If>Logger.log ~color:`magenta "DB-LIGHT : get timestamp = %s" (Date.rfc1123 (Time.localtime timestamp))#<End>;
    timestamp


  (************************************)
  (* ouverture / fermeture de session *)
  (************************************)

  let position file =
    if Filename.is_relative file
    then sprintf "%s/" (Unix.getcwd ())
    else ""

  let init_mtree t =
    let has_mtree =
      if not t.with_direct
      then
        let mtree_file = Io_light.get_location t.file_manager^"_mtree" in
        #<If$minlevel 20>Logger.log "DB-LIGHT : init_mtree: mtree_file=%s" mtree_file#<End>;
        try
          let ic = open_in mtree_file in
          let mtree = Mem_tree_light.input_mt ic in
          Db_light.set_mtree t.db_ref mtree;
          close_in ic;
          true
        with Sys_error _ -> false
      else false
    in
    Logger.log "DB-LIGHT : init_mtree: has_mtree=%b" has_mtree;
    has_mtree

  let init_mst t =
    let has_mst =
      if t.with_direct
      then (try
              let mst_file = Io_light.get_location t.file_manager^"_mst" in
              #<If$minlevel 20>Logger.log "DB-LIGHT : init_mst: mst_file=%s" mst_file#<End>;
              let mst = Mst.create ~create:true ~hint:100000 mst_file in
              Db_light.set_mst t.db_ref (Some mst);
              true
            with Sys_error _ -> false)
      else false
    in
    Logger.log "DB-LIGHT : init_mst: has_mst=%b" has_mst;
    has_mst

  let init_db ?ondemand ?direct ?max_size mode file =
    let rep = Filename.dirname file in
    let _ =
      try
        if not (File.check_create_path rep) then
          raise (Open (None, (sprintf "%s: unable to create path" rep)))
      with
      | Unix.Unix_error (r, f, p) ->
          let s = sprintf "%s %s => %s" f p (Unix.error_message r) in
          raise (Open (None,s))
      | e -> raise (Open (None, Printexc.to_string e)) in
    let filemanager = Io_light.make mode file in
    let with_ondemand = match ondemand with Some ondemand -> ondemand | None -> false in
    let with_direct = match direct with Some direct -> direct | None -> false in
    let with_max_size = match max_size with Some max_size -> max_size | None -> max_int in
    let db =
      if with_ondemand
      then Db_light.make ~filemanager ?max_size ()
      else Db_light.make ()
    in
    { trans_num = 0
    ; db_ref = db
    ; with_dot = false
    ; is_weak = false
    ; with_ondemand = with_ondemand
    ; with_direct = with_direct
    ; with_max_size = with_max_size
    ; file_manager = filemanager
    ; session_lock = None
    ; waiting_FIFO = create_FIFO ()
    }

  let make ?dot ?weak ?ondemand ?direct ?max_size file =
    let _ = (dot,weak) in
    Logger.info "DB-LIGHT : Session_light.make: file=%s" file;
    let t = init_db ?ondemand ?direct ?max_size Io_light.Create file in
    let _ = init_mst t in
    let _dot, with_dot = (*match dot with
    | Some true -> "with", true
    | Some false | None ->*) "without", false in
    let _disk, _weak, is_weak = (*match weak with
    | Some true -> "reading on disk", Some (read_node_from_disk t), true
    | Some false | None ->*) "ram only", None, false in
    #<If>
      let _position = position file in
      let _ondemand = match ondemand with | Some true -> " ondemand " | Some false | None -> " " in
      let _direct = match direct with | Some true -> " direct " | Some false | None -> " " in
      let _max_size =
        match max_size with
        | Some n when (n < 0 || n = max_int) -> " "
        | Some ms -> sprintf " max_size=%d " ms
        | None -> " "
      in
      Logger.log "DB-LIGHT : Opening a new DB %s dot files, %s%s%s%sat %s%s by %s"
                 _dot _disk _ondemand _direct _max_size _position file (Sys.executable_name)
    #<End>;
    { t with is_weak = is_weak; with_dot = with_dot; }

  let close_db ?(donothing=false) t =
    let _ = donothing in
    let file = Io_light.get_location t.file_manager in
    let mtree_file = file^"_mtree" in
    let oc = open_out mtree_file in
    Mem_tree_light.output_mt oc (Db_light.get_mtree t.db_ref);
    close_out oc;
    if t.with_direct then ignore (Option.map Mst.close (Db_light.get_mst t.db_ref));
    #<If$minlevel 20>Logger.log "close_db: mtree_file=%s" mtree_file#<End>;
    let _position = position file in
    Logger.info "DB-LIGHT : Closing the database at %s" file;
    Io_light.close t.file_manager;
    #<If>Logger.log ~color:`magenta "DB-LIGHT : '%s%s' closed(%b)"
                                           _position file (Io_light.is_closed t.file_manager)#<End>

  let restart_db_from_last t =
    let db = t.db_ref in
    let has_mtree = init_mtree t in
    let _ = init_mst t in
    if t.with_ondemand && not t.with_direct && not has_mtree
    then Logger.warning "DB-LIGHT : Warning: unable to read mem_tree file, rebuilding from Dbm";
    (match Io_light.get_dbm t.file_manager with
     | Some dbm ->
         DB.iter (fun pathstr datastr ->
                     match pathstr with
                     | "version" ->
                         if Io_light.version <> datastr
                         then Logger.log ~color:`red "DB-LIGHT : Warning: Dbm file version %s does not match DB %s"
                                                             datastr Io_light.version;
                         #<If>Logger.log ~color:`magenta "DB-LIGHT : Dbm file version %s" datastr#<End>;
                         Db_light.set_version db datastr
                     | "ondemand" ->
                         let ondemand =
                           try bool_of_string datastr
                           with Invalid_argument "bool_of_string" ->
                             Logger.log ~color:`red "DB-LIGHT : Warning: Dbm file nonsensical bool string for ondemand = '%s'" datastr;
                             false
                         in
                         Db_light.set_filemanager db (if ondemand then Some t.file_manager else None);
                         #<If>Logger.log ~color:`magenta "DB-LIGHT : Dbm file ondemand %s" datastr#<End>
                     | "direct" ->
                         let direct =
                           try bool_of_string datastr
                           with Invalid_argument "bool_of_string" ->
                             Logger.log ~color:`red "DB-LIGHT : Warning: Dbm file nonsensical bool string for direct = '%s'" datastr;
                             false
                         in
                         if direct && Option.is_none (Db_light.get_mst db) then ignore (init_mst t);
                         #<If>Logger.log ~color:`magenta "DB-LIGHT : Dbm file direct %s" datastr#<End>
                     | "max_size" ->
                         let max_size =
                           try int_of_string datastr
                           with Invalid_argument "int_of_string" ->
                             Logger.log ~color:`red "DB-LIGHT : Warning: Dbm file nonsensical int string for max_size = '%s'" datastr;
                             max_int
                         in
                         Db_light.set_max_size db max_size;
                         #<If>Logger.log ~color:`magenta "DB-LIGHT : Dbm file max_size %s" datastr#<End>
                     | "timestamp" ->
                         #<If>Logger.log ~color:`magenta "DB-LIGHT : Dbm file timestamp %s" datastr#<End>;
                         t.file_manager.Io_light.timestamp <- (try Date.of_string datastr
                                                               with Not_found -> Time.now ())
                     | "lock_pid" ->
                         #<If>Logger.log ~color:`magenta "DB-LIGHT : Dbm file lock PID %s" datastr#<End>
                     | "lock_hostname" ->
                         #<If>Logger.log ~color:`magenta "DB-LIGHT : Dbm file lock hostname %s" datastr#<End>
                     | _ ->
                         if t.with_ondemand
                         then
                           (if not has_mtree
                            then
                              (let path = snd (Encode_light.decode_path pathstr 0) in
                               let node = snd (Encode_light.decode_node datastr 0) in
                               #<If>Logger.log ~color:`magenta "DB-LIGHT : set mtree %s -> %s"
                                                               (Path.to_string path)
                                                               (Datas.to_string node.Node_light.content)#<End>;
                               Mem_tree_light.add_mtree (Db_light.get_mtree db) path node.Node_light.content)
                            else
                              (if !(Db_light.verify)
                               then
                                 let path = snd (Encode_light.decode_path pathstr 0) in
                                 let node = snd (Encode_light.decode_node datastr 0) in
                                 match Mem_tree_light.find_mtree_data (Db_light.get_mtree db) path with
                                 | Some true ->
                                     if node.Node_light.content = Datas.UnsetData
                                     then Logger.debug "DB-LIGHT : (verify fail) path %s data in mtree but not in Dbm file"
                                                       (Path.to_string path)
                                 | Some false ->
                                     if node.Node_light.content <> Datas.UnsetData
                                     then Logger.debug "DB-LIGHT : (verify fail) path %s data in Dbm file but not in mtree"
                                                       (Path.to_string path)
                                 | None ->
                                     Logger.debug "DB-LIGHT : (verify fail) path %s data in Dbm file but node not in mtree"
                                                  (Path.to_string path)))
                         else
                           let path = snd (Encode_light.decode_path pathstr 0) in
                           let datas = snd (Encode_light.decode_datas datastr 0) in
                           #<If>Logger.log ~color:`magenta "DB-LIGHT : set %s -> %s"
                                                           (Path.to_string path) (Datas.to_string datas)#<End>;
                           (match datas with
                            | Datas.Data dataImpl -> ignore (Db_light.update_index db [(path,dataImpl)])
                                (* FIXME: Links!!! *)
                            | _ -> ());
                           ignore (Db_light.update ~no_write:true db path datas))
           dbm
     | None -> ());
    db

  let restart_db ?dot ?weak ?restore ?openat_rev ?ondemand ?direct ?max_size file =
    let _ = dot, weak, restore, openat_rev in
    let t = init_db ?ondemand ?direct ?max_size Io_light.Append file in
    let _position = position file in
    let _dot, with_dot = (*match dot with
    | Some true -> "with", true
    | Some false | None ->*) "without", false in
    let _disk, _weak, is_weak = (*match weak with
    | Some true -> "reading on disk", Some (read_node_from_disk t), true
    | Some false | None ->*) "ram only", None, false in
    let _ondemand = match ondemand with | Some true -> ", ondemand " | Some false | None -> " " in
    let _direct = match direct with | Some true -> ", direct " | Some false | None -> " " in
    let _max_size =
      match max_size with
      | Some n when n < 0 || n = max_int -> " "
      | Some max_size -> sprintf ", max_size=%d " max_size
      | None -> " "
    in
    #<If>Logger.log "DB-LIGHT : Opening an existing DB %s dot files, %s%s%s%sat %s%s by %s"
                   _dot _disk _ondemand _direct _max_size _position file (Sys.executable_name)#<End>;
    let t = { t with is_weak = is_weak; with_dot = with_dot } in
    let db =
      try restart_db_from_last t
      with _exn ->
        #<If>Logger.log "DB-LIGHT : restart_db:  Can't open Dbm %s %s" file (Printexc.to_string _exn)#<End>;
        raise (Open (None, "Corrupted files"))
    in
    t.db_ref <- db;
    t

  let open_db_aux ?dot ?weak ?rev ?restore ?ondemand ?direct ?max_size file =
    let _ = (rev, restore) in
    let _starting_time = Unix.gettimeofday() in
    let pretty_location = #<If:TESTING> "" #<Else> " at "^file #<End> in
    if file = "" then raise (Open (None, "empty name"))
    else
      let is_new, session =
        if Sys.file_exists (file^".dir")
        then (Logger.info "DB-LIGHT : Opening database%s" pretty_location;
              false, restart_db ?dot ?weak ?restore ?ondemand ?direct ?max_size file)
        else (#<If:TESTING> () #<Else> Logger.notice "DB-LIGHT : Initialising empty database%s" pretty_location #<End>;
              true, make ?dot ?weak ?ondemand ?direct ?max_size file)
      in
      #<If>Logger.log "DB-LIGHT : time to open = %f" (Unix.gettimeofday() -. _starting_time)#<End>;
      session, is_new

  let open_db ?dot ?weak ?rev ?restore ?ondemand ?direct ?max_size file =
    try open_db_aux ?dot ?weak ?rev ?restore ?ondemand ?direct ?max_size file
    with Open (db, s) ->
      (Option.iter (fun db -> close_db ~donothing:true db) db;
      Logger.critical "DB-LIGHT : Error during database opening :\n%s" s;
      exit 1)


  let is_empty t = Db_light.is_empty t.db_ref

  let get_rev t = Db_light.get_rev t.db_ref


  (*******************)
  (* les transactions*)
  (*******************)

  let is_closed_db t = not (Io_light.is_open t.file_manager)

  let new_trans ?read_only t =
    assert (not (is_closed_db t));
      let trans_num = (succ t.trans_num) mod max_int in
      t.trans_num <- trans_num;
      #<If>
        Logger.log ~color:`magenta
        "DB-LIGHT : Initialisation of a new transaction%swith number #%d on a DB"
           (match read_only with
            | Some (true, _) -> " read only "
            | _ -> " ")
           trans_num
        #<End>;
      let res = Tr.init t.db_ref ?read_only trans_num in
      Logger.log ~color:`magenta "DB-LIGHT : Initialisation done";
      res

  let abort_of_unprepared t _trans =
    assert (t.session_lock = None);
    (* No transaction is prepared at this time, so the one from
       argument must be unprepared, so do nothing. GC will take care
       of cleaning it. *)
    #<If>
      Logger.log ~color:`red
      "DB-LIGHT : Abort of unprepared transaction or of the continuation of committed transaction #%d."
         (Tr.get_num _trans)
      #<End>;
    (* Not removed from init_map, because at the higher level
       it may be wiped up and rebuilt differently, so it still exists. *)
    ()

  let _prepare_commit db_ref trans =
    #<If>Logger.log ~color:`magenta "DB-LIGHT : Preparing commit of transaction #%d on a DB." (Tr.get_num trans)#<End>;
    Tr.commit trans db_ref

  (* Never runs the continuation [k]. *)
  let prepare_commit t trans k =
    match t.session_lock with
    | None ->
        #<If>Logger.log ~color:`magenta "DB-LIGHT : Preparing transaction #%d (no FIFO)." (Tr.get_num trans)#<End>;
        let db = _prepare_commit t.db_ref trans in
        t.session_lock <- Some (trans, db);
        Some (trans, k)
    | Some _ ->
        #<If>Logger.info "DB-LIGHT : Previous prepared transaction not committed yet. Stashed transaction #%d on the waiting FIFO."
                         (Tr.get_num trans) #<End>;
        (* Assumption: this won't raise exceptions. If the data structure
           gets complicated and exceptions are possible, change
           [abort_of_unprepared] in the next function, because here
           the transaction is prepared (partially). *)
        add_FIFO trans k t.waiting_FIFO;
        None

  (* Calls a continuation, but never catches its exceptions. *)
  let rec try_prepare_commit t trans k =
    try
      prepare_commit t trans k
    with
    | Db_light.Merge
    | Db_light.UnqualifiedPath | DiskError _ ->
        (* The preparation may be half-done, so we rollback to revert it. *)
        abort_of_unprepared t trans;
        k (trans, false);
        (* This trans is in conflict, so it won't get committed,
           so the commit function won't pop from the FIFO, when it finishes.
           So try another one from the waiting list, until one merges OK. *)
        pop_trans_k t
    | e ->
        (* The preparation may be half-done, so we rollback to revert it
           and reraise the exception in a saner internal state. *)
        (* do not reraise the excpetion, coonsider that the transaction failed
         * apply the continuation with [false], and continue popping *) 
        (Logger.error "DB-LIGHT : Error During db transaction : %s\n%s" (Printexc.to_string e) (Printexc.get_backtrace ());
        abort_of_unprepared t trans;
        k (trans, false);
        pop_trans_k t)

  and pop_trans_k t =
      if is_empty_FIFO t.waiting_FIFO then begin
        #<If> Logger.log ~color:`red "DB-LIGHT : Nothing popped from FIFO." #<End>;
        None
      end else begin
        let (trans, k) = take_FIFO t.waiting_FIFO in
        #<If>
          Logger.log ~color:`red
          "DB-LIGHT : Commit of transaction #%d popped from FIFO; %d commits waiting."
             (Tr.get_num trans) (Queue.length t.waiting_FIFO)
             #<End>;
        try_prepare_commit t trans k
      end

  (* Calls a continuation, but never catches its exceptions. *)
  let try_trans_prepare t trans k =
    match try_prepare_commit t trans k with
    | None -> ()
    | Some (trans2, k2) -> k2 (trans2, true)

  (* Calls a continuation, but never catches its exceptions. *)
  let pop_trans_prepare t =
    match pop_trans_k t with
    | None -> ()
    | Some (trans, k) -> k (trans, true)

  let abort_or_rollback t trans =
    #<If>
      Logger.log ~color:`red
      "DB-LIGHT : Rollback of prepared or abort of unprepared or of the continuation of committed transaction #%d."
         (Tr.get_num trans)
    #<End>;
    match t.session_lock with
    | None ->
        abort_of_unprepared t trans
    | Some (transl, _db) ->
        if Tr.get_num transl <> Tr.get_num trans then begin
          (* The transaction is not the one prepared. For now, to keep
             rollbacks deterministic from the point of view of a single thread,
             we do nothing, so the transaction will be prepaired in the future
             and commited, if the commit request is, e.g., in the prepare
             callback continuation. If needed, as an optimiztion,
             the commit may be removed from the waiting list together with
             the callback, but we are in trouble if the commit request was not
             in the callback, but in another thread and so it will crash.
             In other words, we for now we treat this as abort, not rollback. *)
          #<If>
            Logger.log ~color:`red
            "DB-LIGHT : Abort of unprepared transaction #%d (while another, prepared transaction waits for commit)."
               (Tr.get_num trans)
            #<End>;
          (* Not removed from init_map, because at the higher level
             it may be wiped up and rebuilt differently, so it still exists. *)
        end else begin
          (* Release the lock. *)
          t.session_lock <- None;
          pop_trans_prepare t;
          #<If>
            Logger.log ~color:`red
            "DB-LIGHT : Rollback of prepared transaction #%d"
               (Tr.get_num trans)
            #<End>;
        end

  let really_commit t trans =
    match t.session_lock with
    | Some (transl, db) ->
        let success =
          try
            assert (Tr.get_num transl = Tr.get_num trans);
            t.db_ref <- db;
            disk_writing t trans;
            if not (!(Db_light.od_early)) then Db_light.action_od ();
            (* Release the lock. *)
            t.session_lock <- None;
            true
          with
          | Db_light.UnqualifiedPath | DiskError _ ->
              false
        in
        if success then begin
          #<If> Logger.info "DB-LIGHT : Finished a commit." #<End>
        end else begin
          #<If> Logger.info "DB-LIGHT : Failed a commit." #<End>
        end;
        pop_trans_prepare t;
        if !(Db_light.verify) then Db_light.verify_database t.db_ref;
        success
    | None ->
        Logger.error "DB-LIGHT : Inconsistent state: it should be locked before commit.";
        assert false

  (* reading from DB *)

  let get t tr path =
    if t.with_direct
    then Tr.get_direct tr path
    else Tr.get tr path

  let get_children t trans range path =
    if t.with_direct
    then List.sort compare (Tr.get_children_direct trans range path)
    else List.sort compare (Tr.get_children trans range path)

  let stat t trans path =
    if t.with_direct
    then Tr.stat_direct trans path
    else Tr.stat trans path

  let full_search _t tr slist path =  Tr.full_search tr slist path


  (* writing to DB *)

  (*let last = ref (Unix.gettimeofday())*)
  let set t trans path data =
    (*eprintf(*Logger.log ~color:`magenta*) "DB-LIGHT : Session_light.set: since last=%f\n%!" ((Unix.gettimeofday()) -. !last);*)
    (*let start = Unix.gettimeofday () in*)
    let res =
      if t.with_direct
      then Tr.set_direct trans path data
      else Tr.set trans path data
    in
    (*eprintf(*Logger.log ~color:`magenta*) "DB-LIGHT : Session_light.set: time=%f\n%!" ((Unix.gettimeofday()) -. start);*)
    (*last := Unix.gettimeofday ();*)
    res

  let remove t trans path =
    if t.with_direct
    then Tr.remove_direct trans path
    else Tr.remove trans path

  let set_link t trans path link =
    if t.with_direct
    then Tr.set_link_direct trans path link
    else Tr.set_link trans path link

  let set_copy t trans path (target_path, target_rev) =
    if t.with_direct
    then Tr.set_copy_direct trans path (target_path, target_rev)
    else Tr.set_copy trans path (target_path, target_rev)

end
