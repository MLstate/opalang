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


  (* shorthands *)
  type 'a intmap = 'a IntMap.t
  module List = BaseList
  module Tr = Transaction

  module WIM = Weak.Make(struct
    type t = Tr.t
    let equal a b = compare (Tr.get_num a) (Tr.get_num b) = 0
    let hash = Tr.get_num
  end)

  (* The queue of transaction numbers, stored in order of appearance,
     helps in choosing the next prepare to do (the longest waiting).
     TODO: it's imperative; perhaps do this functionally? *)
  type tr_FIFO = (Tr.t * (Tr.t * bool -> unit)) Queue.t

  let is_empty_FIFO queue = Queue.is_empty queue

  let create_FIFO () = Queue.create ()

  let add_FIFO trans k queue = Queue.add (trans, k) queue

  let take_FIFO queue = Queue.take queue

  type lock = (Tr.t * Hldb.t) option
      (* It stores a transaction and the new db after applying it,
         which will become official if the commit of the
         transaction is requested and succeeds. Whichever part of the code
         releases the lock is responsible for taking the oldest transaction
         from the waiting FIFO and preparing it. *)

  type t = { mutable trans_num : int
               (* counter for fresh transaciton serial numbers *)
           ; init_map : WIM.t
             (* map of the not commited and not rolled back transactions
                initialised under given revisions; no empty lists allowed;
                used only for optimization, to shorten db_to_merge *)
           ; mutable db_ref : Hldb.t
             (* the reference db passed to new transactions *)
           ; mutable db_to_merge : QueryMap.t intmap
             (* query maps of transactions, indexed by revisions at which
                the transactions were commited, from the first revision
                which can still cause a conflict (minimal revision of init_map)
                to the current revision
                TODO: later, use [GC.finalise f x] on the transactions
                or weak references in init_map to prune db_to_merge
                and init_map whenever we verify that
                up to certain revision all transactions are not accessible
                from OPA anymore (GC-cleaned). *)
           ; with_dot : bool
           ; is_weak : bool
           ; is_readonly: bool
           ; file_manager : IoManager.t
           ; mutable session_lock : lock
           ; waiting_FIFO : tr_FIFO
             (* The queue stores the waiting transactions with their revisions,
                as well as the continuations to execute asynchronously,
                when prepare of the transactions is over
                (usually the continuation will execute a commit operation
                or send a confirmation to the client, which may then request
                the commit operation). *)
           ; mutable inital_revision : Revision.t
             (* First revision of the db at the opening. Used to write or not last db state *)
          }

  (* exceptions *)
  exception Open of (t option * string)
  exception DiskError of string


  let write_limit = 1000

  (******************)
  (* lecture disque *)
  (******************)

  let read_node_from_disk t uid =
    IoManager.read_specific_node t.file_manager uid

  let read_uid_rev ?rev t =
    IoManager.read_uid_rev ?rev t.file_manager

  (** The version number of the DB code is hardcoded here. Version is written
      to the DB _config file. Other general summary information about
      the DB may go into that file, too, in the future. If there is
      no _config file, the version is assumed to be correct.
      Warning: Whenever the DB format changes to incompatible one,
      please bump the version number up. *)

  let db_code_version_number = 25

  let read_version t =
    let fm = t.file_manager in
    let vers = IoManager.read_version fm in
    if vers <> db_code_version_number then
      Migration.update vers db_code_version_number (IoManager.get_filemanager fm)

  let write_version t =
    IoManager.write_version t.file_manager db_code_version_number


  let read_config t =
    let conf = IoManager.read_config t.file_manager in
    let vers =  conf.DT.version in
    if vers <> db_code_version_number then
      Migration.update vers db_code_version_number (IoManager.get_filemanager t.file_manager);
    conf.DT.snapshot_rev

  let write_config t rev =
    IoManager.write_config t.file_manager
      { DT.
          version = db_code_version_number ;
          snapshot_rev = rev ;
      }

  (*******************)
  (* ecriture disque *)
  (*******************)

  let write_last_db_state t =
    let fm = t.file_manager in
    let uidmap = Hldb.get_uid_map t.db_ref in
    let index = Hldb.get_index t.db_ref in
    IoManager.write_dbstate fm ~uidmap ~index ;
    write_config t (Hldb.get_rev t.db_ref)

  let write_uid_rev t =
    let fm = t.file_manager in
    let tcount = Hldb.get_tcount t.db_ref in
    let next_uid = Hldb.get_next_uid t.db_ref in
    let rev = Hldb.get_rev t.db_ref in
    let uidrevfile = {
      DT.
        eid = tcount ;
        uid = next_uid ;
        rev
    } in
    IoManager.write_uid_rev fm uidrevfile

  let write_trans t tr =
    let fm = t.file_manager in
    let transfile = Tr.append_disk tr in
    IoManager.write_trans fm transfile

  let write_nodes t =
    let fm = t.file_manager in
    let last_nodes = Hldb.get_last_nodes t.db_ref in
    IoManager.write_nodes fm last_nodes

  let write_timestamp t ts =
    let fm = t.file_manager in
    IoManager.write_timestamp fm ts

  let write_flags t =
    let fm = t.file_manager in
    IoManager.write_flags fm

  let disk_writing t ?trans rev =
    if t.with_dot then Dot.generation t.db_ref t.file_manager;
    if t.is_readonly then
      Logger.error "Database opened only on read_only mode. Will not write the transaction"
    else
    try
      #<If> Logger.log ~color:`green "DB : writing nodes" #<End>;
      write_nodes t;
      #<If> Logger.log ~color:`green "DB : writing the rest" #<End>;
      write_timestamp t (Time.now());
      Option.iter (fun tr -> write_trans t tr) trans;
      write_uid_rev t;
      write_flags t;
      let vrev = Revision.value rev in
      if (vrev mod write_limit = 0 && vrev > 0) then
        (#<If> Logger.log ~color:`green "Write a database snapshot, revision %d" vrev #<End>;
        write_last_db_state t)
    with e -> (
      let cause = Printexc.to_string e in
      let bt = Printexc.get_backtrace() in
      #<If>
        Logger.error "DB : error during disk writing for revision\n%s\n%s\n%s"
          (Revision.to_string rev) cause bt
      #<End>;
      raise (DiskError (Printf.sprintf "%s\n%s" cause bt)))


  (************************)
  (* timestamps managment *)
  (************************)

  let get_timestamp = Time.now

  let get_timestamp_from_rev t rev =
    #<If>
    Logger.log ~color:`yellow
      "DB : get timestamp for revision %s" (Revision.to_string rev)
    #<End>;
    try IoManager.read_timestamp t.file_manager (Revision.value rev)
    with DT.CrashTimestamp ->
      raise (DiskError (Printf.sprintf "Timestamp: try to read an uncommitted revision (%s vs %s)"
                (Revision.to_string rev) (Revision.to_string dbrev)))


  (************************************)
  (* ouverture / fermeture de session *)
  (************************************)

  let make_lock_file fm =
    let file = IoManager.get_location fm in
    if IoManager.lock_file fm then
      #<If>
      Logger.log ~color:`yellow "DB : create lock_file %s_lock" file
      #<End>
    else
      (Logger.critical "The database '%s' is currently used by anoter application." file;
      IoManager.close fm;
      exit 1)

  let position file =
    if Filename.is_relative file
    then Printf.sprintf "%s/" (Unix.getcwd ())
    else ""

  let init_db mode file =
    let rep = Filename.dirname file in
    let _ =
      try
        if not (File.check_create_path rep) then
          raise (Open (None, (Printf.sprintf "%s: unable to create path" rep)))
      with
      | Unix.Unix_error (r, f, p) ->
          let s = Printf.sprintf "%s %s => %s" f p (Unix.error_message r) in
          raise (Open (None,s))
      | e -> raise (Open (None, Printexc.to_string e)) in
    let db = Hldb.make () in
    { trans_num = 0
    ; init_map = WIM.create 11
    ; db_ref = db
    ; db_to_merge = IntMap.empty
    ; with_dot = false
    ; is_weak = false
    ; is_readonly = false
    ; file_manager = IoManager.create mode file
    ; session_lock = None
    ; waiting_FIFO = create_FIFO ()
    ; inital_revision = Revision.make 0;
    }

  let make ?(readonly=false) ?dot ?weak file =
    if readonly then
      raise (Open (None, Printf.sprintf "Can not open on readonly a new database. check path '%s'" file));

    let t = init_db `create file in
    make_lock_file t.file_manager;
    let _position = position file in
    let _dot, with_dot = match dot with
    | Some true -> "with", true
    | Some false | None -> "without", false in

    let _disk, weak, is_weak = match weak with
    | Some true -> "reading on disk", Some (read_node_from_disk t), true
    | Some false | None -> "ram only", None, false in
    #<If>
      Logger.log "Opening a new DB %s dot files, %s at %s%s%s by %s"
        _dot _disk _position file (if readonly then ", on readonly," else "")
        (Sys.executable_name)
    #<End>;
    let db = Hldb.make ?weak () in
    {t with db_ref = db
       ; is_weak = is_weak
       ; with_dot = with_dot }

  let close_db ?(donothing=false) t =
    let file = IoManager.get_location t.file_manager in
    let _position = position file in
    Logger.info "Closing the database%s" (#<If:TESTING> "" #<Else> " at "^ file #<End>);
    #<If>
      Logger.log ~color:`yellow "Please wait, closing DB at %s%s" _position file
    #<End>;
    let cur_rev = Hldb.get_rev t.db_ref in

    #<If:DB3_NO_FINAL_SNAPSHOT>
      ()
    #<Else>
      if donothing then ()
      else
      if Revision.equal t.inital_revision cur_rev
      then (
        #<If> Logger.log ~color:`yellow "No changes in the db : skip snapshot" #<End>;
        (*  write_config t cur_rev *) ()
      )
      else
        write_last_db_state t
    #<End>;

    IoManager.close t.file_manager;

    #<If>
    Logger.log ~color:`yellow "DB '%s%s' closed" _position file
    #<End>

  let read_last_db_state t cur_rev tcount nuid =
    let { DT.uidmap ; index } = IoManager.read_dbstate t.file_manager in
    let nodemap = IoManager.read_nodes t.file_manager in
    Hldb.restart ~index cur_rev tcount nuid uidmap nodemap


  let replay_trans ?until t db =
    let transs = IoManager.read_transs t.file_manager in
    let continue : Revision.t -> bool=
      match until with
      | None -> (fun _ -> true)
      | Some openat -> (fun r -> Revision.compare r openat = -1) in
    let _, db =
      List.fold_left
        (fun (rev, db) trans ->
          if continue rev then Tr.apply_disk trans db rev
          else (rev, db))
        ((Hldb.get_rev db), db) transs in
    db


  let check_coherence t vlastsn vrev =
    if not (vlastsn <= vrev) then
      (Logger.critical "Database error : Incoherente state.\nLast snapshot's revision (%d) is greater than last revision (%d)" vlastsn vrev;
      IoManager.close t.file_manager;
      exit 1)

  let restart_db_from_last ?weak t lastsn uidrevfile =
    let tcount = uidrevfile.DT.eid
    and nuid = uidrevfile.DT.uid
    and rev = uidrevfile.DT.rev in
    let vrev = Revision.value rev in
    let vlastsn = Revision.value lastsn in

    check_coherence t vlastsn vrev;

    t.inital_revision <- rev;

    let db =
      (* no taken snapshot *)
      if vlastsn = 0 then
        (let db = Hldb.make ?weak () in
        let db = Hldb.clean_tmp_maps db in
        replay_trans t db)
          else if vlastsn = vrev then
            (* we are on the last snapshot rev *)
              read_last_db_state t rev tcount nuid
            else (
              (* we have to replay transactions to complete snapshot *)
              let old_uidrevfile = read_uid_rev ~rev:lastsn t in
              let old_tcount = old_uidrevfile.DT.eid
              and old_nuid = old_uidrevfile.DT.uid
              and old_cur_rev = old_uidrevfile.DT.rev in
              let db = read_last_db_state t old_cur_rev old_tcount old_nuid in
              replay_trans t db)
    in db

    let restart_db_from_rev open_at_rev t lastsn uidrevfile =

      t.inital_revision <- open_at_rev;

      let tcount = uidrevfile.DT.eid
      and nuid = uidrevfile.DT.uid
      and rev = uidrevfile.DT.rev in
      let vrev = Revision.value rev in
      let vlastsn = Revision.value lastsn in

      check_coherence t vlastsn vrev;
      (* Read the db state, and if we want to open before last taken snapshot, we restore previous state *)
      let read_and_restore_db_state t rev eid uid =
        let db = read_last_db_state t rev eid uid in
        if not (Revision.equal rev open_at_rev) then
          RevisionMachine.gotorevision t.file_manager db open_at_rev
        else db in

      (* replay transaction, until wanted revision *)
      let replay_trans_and_seek t db =
        let db = replay_trans ~until:open_at_rev t db in
        RevisionMachine.overwrite_files t.file_manager db;
        db in

      let db =
        (* no taken snapshot *)
        if vlastsn = 0 then
          (let db = Hldb.make () in
          let db = Hldb.clean_tmp_maps db in
          replay_trans_and_seek t db)
        else if vlastsn = vrev then
          (* we are on the last snapshot rev *)
            read_and_restore_db_state t rev tcount nuid
          else (
            (* we have to replay transactions to complete snapshot *)
            let old_uidrevfile = read_uid_rev ~rev:lastsn t in
            let old_tcount = old_uidrevfile.DT.eid
            and old_nuid = old_uidrevfile.DT.uid
            and old_cur_rev = old_uidrevfile.DT.rev in
            let db = read_and_restore_db_state t old_cur_rev old_tcount old_nuid in
            (* if we want to open before last_snapshot, no need to replay transactions
             * in the other case, we replay only transaction that we need *)
            if Revision.compare lastsn open_at_rev = -1 then db
            else replay_trans_and_seek t db
          )
      in db

  let restart_db ?(readonly=false) ?dot ?weak ?restore ?openat_rev file =
    (* just set some options *)
    let restore = Option.is_some restore in

    let mode = if readonly then `readonly else `append in
    let t = init_db mode file in
    (* the check is after opening all files because we need a new filemanager to get lock filename *)
    if not readonly then
    make_lock_file t.file_manager;

    read_version t;
    let lastsn = read_config t in

    let uidrevfile, lastsn =
      try (read_uid_rev t, lastsn)
      with DT.CrashUidRev _ ->
        (match RevisionMachine.restore_uid_file ~restore t.file_manager with
        | None -> raise (Open (None,"Corrupted files"))
        | Some uidrev ->
            let sn = if Revision.compare lastsn uidrev.DT.rev = 1 then uidrev.DT.rev else lastsn in
            uidrev,sn) in


    let opt_weak = weak in
    let _position = position file in
    let _dot, with_dot = match dot with
    | Some true -> "with", true
    | Some false | None -> "without", false in
    let _disk, weak, is_weak = match weak with
    | Some true -> "reading on disk", Some (read_node_from_disk t), true
    | Some false | None -> "ram only", None, false in

    #<If>
      Logger.log "Opening an existing DB %s dot files, %s at %s%s%s by %s"
        _dot _disk _position file (if readonly then ", on readonly," else "")
       (Sys.executable_name)
    #<End>;
    let t = {t with is_weak = is_weak
    ; with_dot = with_dot }
    in

    let restart =
      match openat_rev with
      | Some openat_rev ->
          let from =
            if Option.default false opt_weak then
              (Logger.warning "DB: the db is opened on weak mode, can't do the downgrade";
              false)
            else
              if Revision.compare openat_rev uidrevfile.DT.rev <> -1 then
                (Logger.warning "DB: Can not downgrade the db to an upper revision. Last revison is %s, you want to open at %s"
                  (Revision.to_string uidrevfile.DT.rev) (Revision.to_string openat_rev);
                false)
              else true in

          if from then restart_db_from_rev openat_rev t
          else restart_db_from_last ?weak t
    | None ->
        restart_db_from_last ?weak t
    in

    let db =
      try restart lastsn uidrevfile
      with DT.CrashUidRev _ | DT.CrashStateMap _ | DT.CrashNode _| DT.CrashTrans _ | DT.CrashStateIndex _ ->
        (let rest = RevisionMachine.restore_db ~uidrev:uidrevfile ~restore t.file_manager in
        match rest with
        | Some db -> db
        | None ->  raise (Open (None, "Corrupted files")))
    in

    t.db_ref <- db;
    IoManager.cleanup_hashtbls () ;
    t


  let open_db_aux ?(readonly=false) ?dot ?weak ?rev ?restore file =
    let _starting_time = Unix.gettimeofday() in
    let pretty_location = #<If:TESTING> "" #<Else> " at "^file #<End> in
    if file = "" then raise (Open (None, "empty name"))
    else
      let is_new, session = match rev with
      | Some r ->
          if IoManager.is_uidfile_existing file
          then (
            Logger.info "Opening database%s (using revision %d)" pretty_location r;
            false, restart_db ~readonly ?dot ?weak ?restore ~openat_rev:(Revision.make r) file
          )
          else raise (Open (None,(Printf.sprintf "%s : no such file or directory" file)))
      | None ->
          if IoManager.is_uidfile_existing file
          then (
            Logger.info "Opening database%s" pretty_location;
            false, restart_db ~readonly ?dot ?weak ?restore file
          )
          else (
            Logger.info "Initialising empty database%s" pretty_location;
            true, make ~readonly ?dot ?weak file
          )
      in
      let db = session.db_ref in
      if is_new then (
        let _ =
          try
            write_version session;
            disk_writing session (Revision.make 0)
          with DiskError s -> (
            Logger.error "disk writing error : %s" s;
            (* an error occured during disk writing for the first revision.
               doesn't seems good for the further writings.
               so the db is being closed.
            *)
            close_db session;
            let s = "an error happened during disk writing. We advise that you choose another place for your db." in
            raise (Open (Some session, s))
          ) in
        let db = Hldb.clean_tmp_maps db in
        session.db_ref <- db
      );
      #<If>
        Logger.log "time to open = %f" (Unix.gettimeofday() -. _starting_time)
      #<End>;
      session, is_new

  let open_db ?(readonly=false) ?dot ?weak ?rev ?restore file =
    try open_db_aux ~readonly ?dot ?weak ?rev ?restore file
    with Open (db, s) ->
      (Option.iter (fun db -> close_db ~donothing:true db) db;
      Logger.critical "Error during database opening :\n%s" s;
      exit 1)


  let is_empty t = Hldb.is_empty t.db_ref

  let get_rev t = Hldb.get_rev t.db_ref


  (*******************)
  (* les transactions*)
  (*******************)

  let is_closed_db t =
    not(IoManager.is_open t.file_manager )

  let new_trans ?read_only t =
    assert (not (is_closed_db t));
    let rightsreadonly = (Option.default_map false fst read_only) || t.is_readonly in
    let tr_read_only = if rightsreadonly then Some(true, Option.default_map None snd read_only) else None in


      let trans_num = (succ t.trans_num) mod max_int in
      t.trans_num <- trans_num;
      #<If>
        Logger.log ~color:`white
          "Initialisation of a new transaction%swith number #%d on a DB at revision %s"
           (if rightsreadonly then " read only " else " ")
           trans_num
           (Revision.to_string (Hldb.get_rev t.db_ref))
        #<End>;
      let tr =
        match tr_read_only with
        | Some read_only -> Tr.init t.db_ref ~read_only trans_num
        | None ->  Tr.init t.db_ref trans_num
      in
    
      match read_only with
      | Some (true, _) -> tr
      | _ -> WIM.add t.init_map tr; tr

  let shrink_db_to_merge t =
    if (IntMap.is_empty t.db_to_merge
        || WIM.count t.init_map = 0)
    then
      IntMap.empty
    else
      let (min, _) = IntMap.min t.db_to_merge in
      let min_used =
        (WIM.fold
        (fun tr acc ->
          if acc = -1 then
            (Revision.value (Hldb.get_rev (Tr.get_db tr)))
          else
            (Pervasives.min acc (Revision.value (Hldb.get_rev (Tr.get_db tr)))))
         t.init_map (-1)) in
      let rm k _v acc = IntMap.remove k acc in
      IntMap.fold_range rm t.db_to_merge min min_used t.db_to_merge

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

  let _prepare_commit db_ref db_to_merge cur_rev trans =
    #<If>
      Logger.log ~color:`white "Preparing commit of transaction #%d with revision %s on a DB at revision %s."
       (Tr.get_num trans) (Revision.to_string cur_rev) (Revision.to_string (Hldb.get_rev db_ref))
    #<End>;
    (* Here we looking for conflicts by trying to merge with query maps
       from all revisions from the first revision that we haven't taken
       into account when starting (trans_vrev + 1) to the revision previous
       to the one we will be commited under (cur_vrev - 1).
       This is linear in the number of concurrently started transactions,
       so the time to prepare n transactions is quadratic in n.
       Conclusion: it's much cheaper to start transactions sequentially. *)
    (* TODO: this is still wrong when the interviening transactions
       change links and then conflicting writes are not registered.
       Louis says we should unwind the links and keep only unwound
       writes in the query maps. TODO: Check if we already do. *)
    let cur_vrev = Revision.value cur_rev in
    let trans_rev = Hldb.get_rev (Tr.get_db trans) in
    let trans_vrev = Revision.value trans_rev in
    let trqm = Tr.get_query_map trans in
    let check _rev qmap () = QueryMap.mergeable_query_maps qmap trqm
    in
    IntMap.fold_range check db_to_merge (trans_vrev + 1) (cur_vrev - 1) ();
    Tr.commit cur_rev trans db_ref

  (* Never runs the continuation [k]. *)
  let prepare_commit t trans k =
    if not (WIM.mem t.init_map trans) then begin
      #<If>
        Logger.log ~color:`magenta
          "DB : transaction %d at revision %d has already been comitted or aborted. Cannot prepare it again."
          (Tr.get_num trans) (Revision.value (Hldb.get_rev (Tr.get_db trans)))
      #<End>;
      raise Hldb.Merge (* merge conflict of 0 transactions *)
    end else begin
      match t.session_lock with
      | None ->
          let cur_rev = Revision.succ (Hldb.get_rev t.db_ref) in
          let db = _prepare_commit t.db_ref t.db_to_merge cur_rev trans in
          t.session_lock <- Some (trans, db);
          Some (trans, k)
      | Some _ ->
          #<If> Logger.info "Previous prepared transaction not committed yet. Stashed transaction #%d on the waiting FIFO." (Tr.get_num trans) #<End>;
          (* Assumption: this won't raise exceptions. If the data structure
             gets complicated and exceptions are possible, change
             [abort_of_unprepared] in the next function, because here
             the transaction is prepared (partially). *)
          add_FIFO trans k t.waiting_FIFO;
          None
    end

  (* Calls a continuation, but never catches its exceptions. *)
  let rec try_prepare_commit t trans k =
    try
      prepare_commit t trans k
    with
    | Hldb.Merge
    | Hldb.UnqualifiedPath | DiskError _ ->
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
        #<If> Logger.log ~color:`magenta "Nothing popped from FIFO." #<End>;
        None
      end else begin
        let (trans, k) = take_FIFO t.waiting_FIFO in
        #<If>
          Logger.log ~color:`magenta
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
            Logger.log ~color:`magenta
               "Abort of unprepared transaction #%d (while another, prepared transaction waits for commit)."
               (Tr.get_num trans)
          #<End>;
          (* Not removed from init_map, because at the higher level
             it may be wiped up and rebuilt differently, so it still exists. *)
        end else begin
          WIM.remove t.init_map trans;
          t.db_to_merge <- shrink_db_to_merge t;
          (* Release the lock. *)
          t.session_lock <- None;
          t.db_ref <- Hldb.update_aborted t.db_ref _db;
          pop_trans_prepare t;
          #<If>
            Logger.log ~color:`magenta
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
            WIM.remove t.init_map trans;
            t.db_ref <- db;
            let cur_rev = Hldb.get_rev db in
            if WIM.count t.init_map = 0 then
              (* The most common case. No messing around with query maps. *)
              t.db_to_merge <- IntMap.empty
            else begin
              (* Here we add our original query map at the current rev
                 (cur_vrev), so the started, but not yet commited
                 transactions can merge with us.
                 We can't add our map at the rev we started the transaction at,
                 because some transactions started later, but not yet commited
                 would not detect this transaction as a potential conflict. *)
              let tr_map = Tr.get_query_map trans in
              let cur_vrev = Revision.value cur_rev in
              let db_to_merge = IntMap.add cur_vrev tr_map t.db_to_merge in
              t.db_to_merge <- db_to_merge;
              t.db_to_merge <- shrink_db_to_merge t;
            end;

            disk_writing t ~trans cur_rev;
            t.db_ref <- Hldb.clean_tmp_maps db;
            (* Release the lock. *)
            t.session_lock <- None;
            true
          with
          | Hldb.UnqualifiedPath | DiskError _ -> false
        in
        if success then begin
          #<If> Logger.info "Finished a commit." #<End>
        end else begin
          #<If> Logger.info "Failed a commit." #<End>
        end;
        pop_trans_prepare t;
        success
    | None ->
        Logger.critical "Inconsistent state: it should be locked before commit.";
        assert false

  (* reading from DB *)

  let check_rev ?rev t tr =
    match rev with
    | Some rev -> rev
    | None ->
        match Tr.get_read_rev tr with
        | Some rev -> rev
        | None -> Hldb.get_rev t.db_ref

  let get _t tr path =
    Tr.get tr path

  let get_children t trans ?rev range path =
    let rev = check_rev ?rev t trans in
    let l = Tr.get_children trans rev range path in
    assert (l = List.sort compare l);
    l

  let stat trans path = Tr.stat trans path

  let get_all_rev_of_path tr path =
    let l = Tr.get_all_rev_of_path tr path in
    let l = List.rev l in
    assert (if l <> List.uniq (List.sort compare l) then (Printf.printf "pbl! %s vs %s\n%!" (List.print Revision.to_string l) (List.print Revision.to_string (List.uniq (List.sort compare l))); false) else true);
    l

  let get_last_rev_of_path tr path = Tr.get_last_rev_of_path tr path

  let full_search tr slist path =  Tr.full_search tr slist path


  (* writing to DB *)
  let update_init_map t tr =
    WIM.remove t.init_map tr;
    WIM.add t.init_map tr;
    tr

  let set t trans path data = update_init_map t (Tr.set trans path data)

  let remove t trans path = update_init_map t (Tr.remove trans path)

  let set_link t trans path link = update_init_map t (Tr.set_link trans path link)

  let set_copy t trans path (target_path, target_rev) =
    update_init_map t (Tr.set_copy trans path (target_path, target_rev))
