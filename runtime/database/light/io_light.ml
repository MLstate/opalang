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

(* Debug *)

#<Debugvar:DEBUG_DB>

(* Functor *)

module Io_light_ (DB : DbSig.DB) : Io_light_sig.IO_LIGHT =
struct

  (* Depends *)
  module List = BaseList
  module Hashtbl = BaseHashtbl

  (* Exports *)
  module DB = DB

  (* Shorthands *)
  let sprintf fmt = Printf.sprintf fmt
  let fprintf fmt = Printf.fprintf fmt

  (* Version *)
  let version = "0.0"

  (* Types *)
  type mode = Create | Append | ReadOnly

  type t = {
    location : string;
    mode : mode;
    mutable dbm : DB.t option;
    mutable link_count : int;
    mutable has_lock : bool;
    mutable timestamp : Time.t;
    mutable next_file_idx : int;
  }

  (* Values *)
  let dbtbl = ((Hashtbl.create 10) : (string,t) Hashtbl.t)

  (* Code *)
  let is_open t = Option.is_some t.dbm
  let is_closed t = not (is_open t)

  let get_content_file_name t =
    let rec aux n =
      let name = t.location^"_content_"^(string_of_int n) in
      if File.exists name
      then aux (n+1)
      else (t.next_file_idx <- t.next_file_idx + 1; name)
    in
    aux t.next_file_idx

  let really_remove_lock_file t =
    let lock_file_name = t.location^"_lock" in
    if Sys.file_exists lock_file_name
    then (try
            let ic = open_in lock_file_name in
            let (pid,hostname) = Scanf.fscanf ic "%d\n%s\n" (fun i s -> (i,s)) in
            close_in ic;
            if pid = Unix.getpid() && hostname = Unix.gethostname()
            then (#<If>Logger.log ~color:`magenta "DB-LIGHT : removing lock file: %s" lock_file_name #<End>;
              (match t.dbm with
               | Some dbm ->
                   (try
                      DB.remove dbm "lock_pid";
                      DB.remove dbm "lock_hostname"
                    with DB.DB_error "dbm_delete" -> ())
               | None -> ());
              t.has_lock <- false;
              Unix.unlink lock_file_name)
          with _exn ->
            #<If>Logger.log ~color:`red "DB-LIGHT : Warning exception removing lock file: %s"
            (Printexc.to_string _exn)#<End>)
    else ()

  let close t =
    #<If>Logger.log ~color:`magenta "DB-LIGHT : Close DB %s (lc:%d)" t.location t.link_count#<End>;
    if t.link_count > 0
    then (t.link_count <- t.link_count - 1;
          if t.link_count = 0
          then (really_remove_lock_file t;
                (match t.dbm with
                 | Some dbm ->
                     DB.replace dbm "timestamp" (Date.rfc1123 (Time.localtime t.timestamp));
                     DB.close dbm
                 | None -> ());
                t.dbm <- None))

  let critical_error t errstr =
    Logger.critical "%s" errstr;
    really_remove_lock_file t;
    while t.link_count > 0 do close t done;
    exit 1

  let make_lock_file t =
    let lock_file_name = t.location^"_lock" in
    #<If>Logger.log ~color:`magenta "DB-LIGHT : making lock file: %s" lock_file_name #<End>;
    if Sys.file_exists lock_file_name
    then critical_error t "DB-LIGHT : Attempt to create existing lock file"
    else (try
            let pid = Unix.getpid () in
            let hostname = Unix.gethostname () in
            let fd = Unix.openfile lock_file_name [Unix.O_WRONLY; Unix.O_CREAT] File.default_rights in
            let msg = sprintf "%d\n%s\n" pid hostname in
            ignore (Unix.write fd msg 0 (String.length msg));
            Unix.close fd;
            (match t.dbm with
             | Some dbm ->
                 DB.replace dbm "lock_pid" (string_of_int pid);
                 DB.replace dbm "lock_hostname" hostname
             | None -> ());
            t.has_lock <- true
          with _exn ->
            critical_error t (sprintf "DB-LIGHT : Can't create lock file %s" (Printexc.to_string _exn)))

  let remove_lock_file t =
    let lock_file_name = t.location^"_lock" in
    #<If>Logger.log ~color:`magenta "DB-LIGHT : removing lock file: %s" lock_file_name#<End>;
    if Sys.file_exists lock_file_name
    then (try
            (match t.dbm with
             | None -> ()
             | Some dbm ->
                 (try
                    DB.remove dbm "lock_pid";
                    DB.remove dbm "lock_hostname"
                  with DB.DB_error "dbm_delete" -> ()));
            Unix.unlink lock_file_name;
            t.has_lock <- false
          with _exn ->
            critical_error t (sprintf "DB-LIGHT : Can't remove lock file %s" (Printexc.to_string _exn)))
    else ()

  let read_lock_file t =
    let lock_file_name = t.location^"_lock" in
    if Sys.file_exists lock_file_name
    then (try
            let ic = open_in lock_file_name in
            let (pid,hostname) = Scanf.fscanf ic "%d\n%s\n" (fun i s -> (i,s)) in
            close_in ic;
            Some (hostname,pid)
          with _exn ->
            (#<If>Logger.log ~color:`red "DB-LIGHT : Warning exception reading lock file: %s"
              (Printexc.to_string _exn)#<End>;
              None))
    else None

  let check_other_used t =
    let lock_file_name = t.location^"_lock" in
    let error () =
      critical_error t
        (sprintf "The DB-LIGHT database%s is currently used by anoter application or was not closed properly.\n\
                If you are sure that no other application is using the db, you can remove file '%s'."
           (if t.location = "" then "" else " "^t.location) lock_file_name)
    in
    if Sys.file_exists lock_file_name then
      (if Sys.os_type = "Unix" then
         (match read_lock_file t with
          | Some (host,pid) ->
              if host = (Unix.gethostname ()) then
                (let procfile = sprintf "/proc/%d/status" pid in
                 if not (Sys.file_exists procfile) then
                   (#<If> Logger.log ~color:`yellow "DB-LIGHT : REMOVE lock file %s, process died" lock_file_name #<End>;
                     Sys.remove lock_file_name;
                     make_lock_file t)
                 else error())
              else error()
          | None ->
              Sys.remove lock_file_name;
              make_lock_file t)
       else error())
    else make_lock_file t

  let reopen t =
    match t.dbm with
    | Some _ ->
        #<If>Logger.log ~color:`yellow "DB-LIGHT : Reopen: Attempt to re-open already open DB file %s" t.location#<End>;
        t.link_count <- t.link_count + 1
    | None ->
        (try
           check_other_used t;
           let dir_file = t.location^".dir" in
           if Sys.file_exists dir_file
           then (#<If>Logger.log ~color:`magenta "DB-LIGHT : Reopening DB file %s" t.location#<End>;
             t.link_count <- t.link_count + 1;
             t.dbm <- Some (DB.opendb t.location (match t.mode with
                                                  | ReadOnly -> [DB.DB_rdonly]
                                                  | _ -> [DB.DB_rdwr]) File.default_rights))
           else (#<If>Logger.log ~color:`yellow "DB-LIGHT : Reopen: DB file has disappeared, recreating %s" t.location#<End>;
             t.link_count <- t.link_count + 1;
             t.dbm <- Some (DB.opendb t.location (match t.mode with
                                                  | ReadOnly -> [DB.DB_rdonly;DB.DB_create]
                                                  | _ -> [DB.DB_rdwr;DB.DB_create]) File.default_rights))
         with _exn ->
           failwith (sprintf "Can't reopen DB file %s %s" t.location (Printexc.to_string _exn)))

  let make mode file =
    let cfile = File.explicit_path file (Some (Unix.getcwd())) in
    match Hashtbl.find_opt dbtbl cfile with
    | Some t ->
        #<If>Logger.log ~color:`magenta "DB-LIGHT : Returning existing DB data %s" cfile#<End>;
        if is_open t
        then (t.link_count <- t.link_count + 1; t)
        else (reopen t; t)
    | None ->
        let t = { dbm = None; location = cfile; mode = mode;
                  link_count = 0; has_lock = false; timestamp = Time.now();
                  next_file_idx = Random.int 10000;
                } in
        check_other_used t;
        let dir_file = cfile^".dir" in
        let pag_file = cfile^".pag" in
        let dbm =
          try
            (match mode with
             | Create ->
                 if Sys.file_exists dir_file
                 then (#<If>Logger.log ~color:`yellow "DB-LIGHT : New db, purge: deleting file %s" dir_file#<End>;
                   (try Sys.remove dir_file
                    with _exn -> #<If>Logger.log ~color:`yellow "DB-LIGHT : Error deleting file %s %s"
                      dir_file (Printexc.to_string _exn)#<End>; ());
                   (try Sys.remove pag_file
                    with _exn -> #<If>Logger.log ~color:`yellow "DB-LIGHT : Error deleting file %s %s"
                      pag_file (Printexc.to_string _exn) #<End>; ()));
                 #<If>Logger.log ~color:`magenta "DB-LIGHT : Opened new DB file %s" dir_file#<End>;
               let dbm = DB.opendb cfile [DB.DB_rdwr;DB.DB_create] File.default_rights in
               DB.replace dbm "version" version;
               DB.replace dbm "timestamp" (Date.rfc1123 (Time.localtime (Time.now())));
               dbm
             | Append ->
                 #<If>Logger.log ~color:`magenta "DB-LIGHT : Opened DB file for RdWr %s" dir_file#<End>;
                 DB.opendb file [DB.DB_rdwr] File.default_rights
             | ReadOnly ->
                 #<If>Logger.log ~color:`magenta "DB-LIGHT : Opened DB file for Read %s" dir_file#<End>;
                 DB.opendb file [DB.DB_rdonly] File.default_rights)
          with _exn ->
            failwith (sprintf "DB-LIGHT : Can't open DB file %s %s" file (Printexc.to_string _exn))
        in
        t.dbm <- Some dbm;
        t.link_count <- t.link_count + 1;
        Hashtbl.add dbtbl cfile t;
        t

  (* Accessors *)

  let get_timestamp t = t.timestamp
  let get_location t = t.location
  let get_dbm t = t.dbm
  let get_link_count t = t.link_count
  let get_has_lock t = t.has_lock

end

