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
let sprintf = Printf.sprintf

(* debug *)
#<Debugvar:DEBUG_DB>

(* -- *)


  (* shorthands *)
  type 'a intmap = 'a IntMap.t
  module List = BaseList
  module Tr = Transaction_light

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

  (* exceptions *)
  exception Open of (t option * string)
  exception DiskError of string


  (*******************)
  (* ecriture disque *)
  (*******************)

  let write_trans dbm trans =
    List.iter (fun (path,query) ->
                 match query with
                 | Tr.Set datas ->
                     #<If>Logger.log ~color:`magenta "DB-LIGHT : updating path %s to %s"
                                                             (Path.to_string path) (Datas.to_string datas)#<End>;
                     Dbm.replace dbm (Encode_light.encode_path path) (Encode_light.encode_datas datas)
                 | Tr.Remove path ->
                     (* TODO: subtrees!!! *)
                     #<If>Logger.log ~color:`magenta "DB-LIGHT : (qm) removing path %s" (Path.to_string path)#<End>;
                     try Dbm.remove dbm (Encode_light.encode_path path)
                     with Dbm.Dbm_error "dbm_delete" -> ()
              ) (List.rev trans.Tr.tr_query_map);
    List.iter (fun path ->
                 #<If>Logger.log ~color:`magenta "DB-LIGHT : (rl) removing path %s" (Path.to_string path)#<End>;
                 try Dbm.remove dbm (Encode_light.encode_path path)
                 with Dbm.Dbm_error "dbm_delete" -> ()
              ) (List.rev trans.Tr.tr_remove_list)

  let disk_writing t trans =
    match Io_light.get_dbm t.file_manager with
    | Some dbm ->
        (try
           Dbm.replace dbm "timestamp" (Date.rfc1123 (Time.localtime (Time.now())));
           write_trans dbm trans
         with e -> (
           let cause = Printexc.to_string e in
           let bt = Printexc.get_backtrace() in
           #<If>Logger.log ~color:`red "DB-LIGHT : error during disk writing for\n%s\n%s" cause bt#<End>;
           raise (DiskError (sprintf "%s\n%s" cause bt))))
    | None ->
        #<If>Logger.log ~color:`red "DB-LIGHT : warning Dbm closed during disk writing transaction #%d"
                                            (Tr.get_num trans)#<End>

  (************************)
  (* timestamps managment *)
  (************************)

  let get_timestamp t =
    let timestamp = Io_light.get_timestamp t.file_manager in
    #<If>Logger.log ~color:`yellow "DB-LIGHT : get timestamp = %s" (Date.rfc1123 (Time.localtime timestamp))#<End>;
    timestamp


  (************************************)
  (* ouverture / fermeture de session *)
  (************************************)

  let position file =
    if Filename.is_relative file
    then sprintf "%s/" (Unix.getcwd ())
    else ""

  let init_db mode file =
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
    let db = Db_light.make () in
    { trans_num = 0
    ; db_ref = db
    ; with_dot = false
    ; is_weak = false
    ; file_manager = Io_light.make mode file
    ; session_lock = None
    ; waiting_FIFO = create_FIFO ()
    }

  let make ?dot ?weak file =
    let _ = (dot,weak) in
    Logger.info "make: file=%s" file;
    let t = init_db Io_light.Create file in
    let _position = position file in
    let _dot, with_dot = (*match dot with
    | Some true -> "with", true
    | Some false | None ->*) "without", false in
    let _disk, _weak, is_weak = (*match weak with
    | Some true -> "reading on disk", Some (read_node_from_disk t), true
    | Some false | None ->*) "ram only", None, false in
    #<If>Logger.log "Opening a new DB %s dot files, %s at %s%s by %s"
                            _dot _disk _position file (Sys.executable_name)#<End>;
    let db = Db_light.make () in
    {t with db_ref = db
       ; is_weak = is_weak
       ; with_dot = with_dot }

  let close_db ?(donothing=false) t =
    let _ = donothing in
    let file = Io_light.get_location t.file_manager in
    let _position = position file in
    Logger.info "Closing the database at %s" file;
    Io_light.close t.file_manager;
    #<If>Logger.log ~color:`yellow "DB-LIGHT : '%s%s' closed(%b)"
                                           _position file (Io_light.is_closed t.file_manager)#<End>

  let restart_db_from_last t =
    let db = Db_light.make () in
    (match Io_light.get_dbm t.file_manager with
     | Some dbm ->
         Dbm.iter (fun pathstr datastr ->
                     match pathstr with
                     | "version" ->
                         if Io_light.version <> datastr
                         then Logger.log ~color:`red "Warning: Dbm file version %s does not match DB %s"
                                                             datastr Io_light.version;
                         #<If>Logger.log ~color:`magenta "Dbm file version %s" datastr#<End>;
                         Db_light.set_version db datastr
                     | "timestamp" ->
                         #<If>Logger.log ~color:`magenta "Dbm file timestamp %s" datastr#<End>;()
                     | "lock_pid" ->
                         #<If>Logger.log ~color:`magenta "Dbm file lock PID %s" datastr#<End>;()
                     | "lock_hostname" ->
                         #<If>Logger.log ~color:`magenta "Dbm file lock hostname %s" datastr#<End>;()
                     | _ ->
                         let path = snd (Encode_light.decode_path pathstr 0) in
                         let datas = snd (Encode_light.decode_datas datastr 0) in
                         #<If>Logger.log ~color:`magenta "DB-LIGHT : set %s -> %s"
                                                                 (Path.to_string path) (Datas.to_string datas)#<End>;
                         (match datas with Datas.Data dataImpl -> ignore (Db_light.update_index db [(path,dataImpl)]) | _ -> ());
                         ignore (Db_light.update db path datas))
           dbm
     | None -> ());
    db

  let restart_db ?dot ?weak ?restore ?openat_rev file =
    let _ = dot, weak, restore, openat_rev in
    let t = init_db Io_light.Append file in
    let _position = position file in
    let _dot, with_dot = (*match dot with
    | Some true -> "with", true
    | Some false | None ->*) "without", false in
    let _disk, _weak, is_weak = (*match weak with
    | Some true -> "reading on disk", Some (read_node_from_disk t), true
    | Some false | None ->*) "ram only", None, false in
    #<If>Logger.log "Opening an existing DB %s dot files, %s at %s%s by %s"
                   _dot _disk _position file (Sys.executable_name)#<End>;
    let t = { t with is_weak = is_weak; with_dot = with_dot; } in
    let db =
      try restart_db_from_last t
      with exn ->
        #<If>Logger.log "restart_db:  Can't open Dbm %s %s" file (Printexc.to_string exn)#<End>;
        raise (Open (None, "Corrupted files"))
    in
    t.db_ref <- db;
    t

  let open_db_aux ?dot ?weak ?rev ?restore file =
    let _ = (rev, restore) in
    let _starting_time = Unix.gettimeofday() in
    let pretty_location = #<If:TESTING> "" #<Else> " at "^file #<End> in
    if file = "" then raise (Open (None, "empty name"))
    else
      let is_new, session =
        if Sys.file_exists (file^".dir")
        then (Logger.info "Opening database%s" pretty_location;
              false, restart_db ?dot ?weak ?restore file)
        else (Logger.notice "Initialising empty database%s" pretty_location;
              true, make ?dot ?weak file)
      in
      #<If>Logger.log "time to open = %f" (Unix.gettimeofday() -. _starting_time)#<End>;
      session, is_new

  let open_db ?dot ?weak ?rev ?restore file =
    try open_db_aux ?dot ?weak ?rev ?restore file
    with Open (db, s) ->
      (Option.iter (fun db -> close_db ~donothing:true db) db;
      Logger.critical "Error during database opening :\n%s" s;
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
        Logger.log ~color:`white
        "Initialisation of a new transaction%swith number #%d on a DB"
           (match read_only with
            | Some (true, _) -> " read only "
            | _ -> " ")
           trans_num
        #<End>;
      Tr.init t.db_ref ?read_only trans_num

  let abort_of_unprepared t _trans =
    assert (t.session_lock = None);
    (* No transaction is prepared at this time, so the one from
       argument must be unprepared, so do nothing. GC will take care
       of cleaning it. *)
    #<If>
      Logger.log ~color:`red
      "Abort of unprepared transaction or of the continuation of committed transaction #%d."
         (Tr.get_num _trans)
      #<End>;
    (* Not removed from init_map, because at the higher level
       it may be wiped up and rebuilt differently, so it still exists. *)
    ()

  let _prepare_commit db_ref trans =
    #<If>Logger.log ~color:`white "Preparing commit of transaction #%d on a DB." (Tr.get_num trans)#<End>;
    Tr.commit trans db_ref

  (* Never runs the continuation [k]. *)
  let prepare_commit t trans k =
    match t.session_lock with
    | None ->
        #<If>Logger.log ~color:`cyan "Preparing transaction #%d (no FIFO)." (Tr.get_num trans)#<End>;
        let db = _prepare_commit t.db_ref trans in
        t.session_lock <- Some (trans, db);
        Some (trans, k)
    | Some _ ->
        #<If>Logger.info "Previous prepared transaction not committed yet. Stashed transaction #%d on the waiting FIFO."
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
        (Logger.error "Error During db transaction : %s\n%s" (Printexc.to_string e) (Printexc.get_backtrace ());
        abort_of_unprepared t trans;
        k (trans, false);
        pop_trans_k t)

  and pop_trans_k t =
      if is_empty_FIFO t.waiting_FIFO then begin
        #<If> Logger.log ~color:`red "Nothing popped from FIFO." #<End>;
        None
      end else begin
        let (trans, k) = take_FIFO t.waiting_FIFO in
        #<If>
          Logger.log ~color:`red
          "Commit of transaction #%d popped from FIFO; %d commits waiting."
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
      "Rollback of prepared or abort of unprepared or of the continuation of committed transaction #%d."
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
            "Abort of unprepared transaction #%d (while another, prepared transaction waits for commit)."
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
            "Rollback of prepared transaction #%d"
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
            (* Release the lock. *)
            t.session_lock <- None;
            true
          with
          | Db_light.UnqualifiedPath | DiskError _ -> false
        in
        if success then begin
          #<If> Logger.info "Finished a commit." #<End>
        end else begin
          #<If> Logger.info "Failed a commit." #<End>
        end;
        pop_trans_prepare t;
        success
    | None ->
        Logger.error "Inconsistent state: it should be locked before commit.";
        assert false

  (* reading from DB *)

  let get _t tr path = Tr.get tr path

  let get_children _t trans range path =
    List.sort compare (Tr.get_children trans range path)

  let stat trans path = Tr.stat trans path

  let full_search tr slist path =  Tr.full_search tr slist path


  (* writing to DB *)

  let set trans path data = Tr.set trans path data

  let remove trans path = Tr.remove trans path

  let set_link trans path link = Tr.set_link trans path link

  let set_copy _t trans path (target_path, target_rev) =
    Tr.set_copy trans path (target_path, target_rev)



