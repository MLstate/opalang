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
(* ftpServerType.ml:
 * Support code for ftpServerCore.proto.
 * TODO:
 *   1) Reset cwd when connection closed.
 *   2) Arrange for log messages.
 *   3) Handle missing files.
 *)

let protocol = NetAddr.mk_protocol "FTP"

let (<|) f a = f a
let (|>) a f = f a
let ( @* ) g f x = g(f(x))
module List = Base.List
module String = Base.String

type form_code = N | T | C

let str_of_form_code fc = match fc with N -> "N" | T -> "T" | C -> "C"

type type_code =
    A of form_code option
  | E of form_code option
  | I
  | L of int

let str_of_type_code tc =
    match tc with
	A (Some fc) -> "A "^(str_of_form_code fc)
      | A None -> "A"
      | E (Some fc) -> "E "^(str_of_form_code fc)
      | E None -> "E"
      | I -> "I"
      | L num -> Printf.sprintf "L %d" num

type structure_code = F | R | P

let str_of_structure_code sc = match sc with F -> "F" | R -> "R" | P -> "P"

type transfer_mode = S | B | C

let str_of_transfer_mode tm = match tm with S -> "S" | B -> "B" | C -> "C"

type web_info = unit

type state = {
    version: string;                                      (** ftpServer version string *)
    hello_message: string list;                           (** Message for new connection *)
    goodbye_message: string;                              (** End of connection message *)
    is_admin: bool;                                   	  (** admin mode *)
    user: string option;                                  (** current user *)
    data_port_spec: Network.port_spec;                	  (** current data channel port spec *)
    data_secure_mode: Network.secure_mode;             	  (** current data channel secure mode *)
    passive : bool;                                   	  (** passive mode *)
    pasv_port_min:int;                                	  (** minimum port for passive connection *)
    pasv_port_max:int;                                	  (** maximum port for passive connection *)
    pasv_port_spec: Network.port_spec option ref;         (** passive mode port spec *)
    pasv_secure_mode: Network.secure_mode option ref;     (** passive mode secure mode *)
    pasv_port_conn: Scheduler.connection_info option ref; (** the passive port connection *)
    local_ip_num:string;                              	  (** string of server's IP number *)
    data_conn: Scheduler.connection_info option;      	  (** [Some] if currently open *)
    data_blocksize: int;                              	  (** blocksize for transfers *)
    data_type: type_code;                             	  (** FTP data transfer type *)
    binary: bool;                                         (** transfer binary mode flag *)
    start_position: int;                                  (** marker for REST verb *)
    structure_code: structure_code;                   	  (** FTP data structure *)
    transfer_mode: transfer_mode;                     	  (** FTP transfer mode *)
    folder: Folder.folder;                            	  (** restricted filespace, see folder.mli *)
    default_folder: string;                           	  (** starting folder for new connections *)
    rename_string: string option;                         (** from path for RNFR verb *)
    timeout: Time.t;                                	  (** global connection timeout *)
    ssl_cert: string;
    ssl_key: string;
    ssl_pass: string;
}

(** FTP servers seem to think they have the root dir.
    We don't implement this for now but we might want to think
    about a virtual root dir for our server.
*)
let mk_rel (*state*)_ filename =
  (*if not (Filename.is_relative filename) then Filename.concat state.default_folder filename else*) filename

(** Predicate for valid folder. *)
let valid_folder state dir = Folder.valid_folder state.folder (mk_rel state dir)

(** Check if folder is writable, dir or file *)
let writable_folder state dir = Folder.writable_folder state.folder (mk_rel state dir)

(** Change the working dir {b in [folder.state] not the OS working dir}. *)
let cwd state dir =
  try
    Folder.chfolder state.folder (mk_rel state dir);
    prerr_endline (Printf.sprintf "cwd: %s" dir);
    true
  with Folder.Folder_error _ -> false

(** Return working dir {b from [folder.state] not the OS working dir}. *)
let pwd state = (*String.remove_prefix_if_possible state.default_folder*) (Folder.current_folder state.folder)

let rename_folder state from_name to_name =
  prerr_endline (Printf.sprintf "rename_folder: %s as %s" from_name to_name);
  Folder.rename_folder state.folder (mk_rel state from_name) (mk_rel state to_name)

let delete_folder state name =
  prerr_endline (Printf.sprintf "delete_folder: %s" name);
  Folder.delete_folder state.folder (mk_rel state name)

let create_directory state name =
  prerr_endline (Printf.sprintf "create_directory: %s" name);
  Folder.create_directory state.folder (mk_rel state name)

let delete_directory state name =
  prerr_endline (Printf.sprintf "delete_directory: %s" name);
  Folder.delete_directory state.folder (mk_rel state name)

(** set_port:
 *  We turn the ftp stuff into an inet_addr and we also
 *  have to remember to do the arithmetic on the port number.
 *  All values are range checked, the inet_addr is done by the
 *  Unix lib parser but we need to do the port manually.
 *)

let chk255 i = i < 0 || i > 255

let get255 str = let i = int_of_string str in if chk255 i then raise (Failure "get255") else i

let get_port_number p1 p2 = (get255 p1) * 256 + (get255 p2)

let set_port state str =
  try
    match Str.split (Str.regexp ",") str with
	[h1;h2;h3;h4;p1;p2] ->
	  let addr = Unix.inet_addr_of_string (Printf.sprintf "%s.%s.%s.%s" h1 h2 h3 h4) in
	  let port = get_port_number p1 p2 in
	  let port_spec = Network.make_port_spec ~protocol addr port in
	  let state' = {state with data_port_spec=port_spec} in
	    prerr_endline ("setting port to "^(Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port));
	    (state',true)
      | _ -> (state,false)
  with Failure _ -> (state,false)
    
(** set_type:
 *  Initially, we only have ASCII non-printable.
 *  We need three outcomes, success, failure and not implemented.
 *  We would also like some slight flexibility in the format
 *  for example, we might allow "A  N".  Hence the rather silly
 *  regexp which is guaranteed to match even if there is nothing
 *  in the optional part.  We also want to return the message
 *  from here but we can't "send" an ocaml value in the DSL.
 *  Instead, we just encode the return value and get the required
 *  behaviour by constructing if..then trees in the DSL.
 *  What we would really like is a "match" construct in the DSL.
 *)
let get_type str =
  if Str.string_match (Str.regexp "[ ]*\\([AEIL]\\)[ ]*\\([^ ]*\\)[ ]*") str 0
  then
    match (Str.matched_group 1 str,Str.matched_group 2 str) with
	("A","N") -> (false, A (Some N))
      | ("A","T") -> raise (Failure "504")
      | ("A","C") -> raise (Failure "504")
      | ("A","") -> (false, A None)
      | ("E","N") -> raise (Failure "504")
      | ("E","T") -> raise (Failure "504")
      | ("E","C") -> raise (Failure "504")
      | ("E","") -> raise (Failure "504")
      | ("I","") -> (true, I)
      | ("L",numstr) ->
	  let num = int_of_string numstr in
	    if num >= 0 && num <= 255
	    then
              if num = 8
              then  (true, L 8)
              else raise (Failure "504")
	    else raise (Failure "501")
      | _ -> raise (Failure "501")
  else raise (Failure "501")

let set_type state str =
  try
    let bin, type_code = get_type str in
    let state = {state with data_type=type_code; binary=bin} in
      prerr_endline ("Setting TYPE to "^str_of_type_code type_code);
      (state,"200")
  with
    | Failure "504" -> (state,"504")
    | Failure _ -> (state,"501")

(** Return a string representing the type in the 150 reply *)
let get_binary_mode state =
  match state.data_type with
  | A _ -> "ASCII"
  | E _ -> "EBCDIC"
  | I | L 8 -> "BINARY"
  | _ -> "UNKOWN"

(** set_structure_code:
 *  Same model as above except there are fewer options.
 *)
let set_structure_code state str =
  try
    if Str.string_match (Str.regexp "[ ]*\\([FRP]\\)") str 0
    then
      let sc =
	match Str.matched_group 1 str with
	    "F" -> F
	  | "R" -> R
	  | "P" -> raise (Failure "504")
	  | _ -> raise (Failure "504") in
      let state' = {state with structure_code=sc} in
      prerr_endline ("Setting STRU to "^str_of_structure_code sc);
	(state',"200")
    else raise (Failure "501")
  with 
    | Failure "504" -> (state,"504")
    | Failure _ -> (state,"501")

(** set_transfer_mode:
 *  Same again.
 *)
let set_transfer_mode state str =
  try
    if Str.string_match (Str.regexp "[ ]*\\([SBC]\\)") str 0
    then
      let tm =
	match Str.matched_group 1 str with
	    "S" -> S
	  | "B" -> raise (Failure "504")
	  | "C" -> raise (Failure "504")
	  | _ -> raise (Failure "504") in
      let state' = {state with transfer_mode=tm} in
      prerr_endline ("Setting MODE to "^str_of_transfer_mode tm);
	(state',"200")
    else raise (Failure "501")
  with 
    | Failure "504" -> (state,"504")
    | Failure _ -> (state,"501")

(** cr2crlf:
    In ASCII mode we need to ensure that all \n characters are turned into \n\r.
    This method creates a string of twice the size and then transforms the original
    string into that and then truncates it.  This involves a lot of copying so it
    would be better to do this in a stream of some kind.
*)
let cr2crlf str =
  let len = String.length str in
  let s2 = String.create (len * 2) in
  let j = ref 0 in
    for i = 0 to len - 1 do
      if str.[i] = '\n'
      then (s2.[!j] <- '\r'; s2.[(!j)+1] <- '\n'; j := !j + 2)
      else (s2.[!j] <- str.[i]; j := !j + 1)
    done;
    String.sub s2 0 (!j);;

(** crlf2cr:
    In fact, we also need to to the reverse...
    Again, we are all {i in situ} so it's pretty inefficient.
*)
let crlf2cr str =
  let len = String.length str in
  let s2 = String.create len in
  let i,j = ref 0,ref 0 in
    while (!i) <= len - 2 do
      if (str.[(!i)] = '\n' && str.[(!i)+1] = '\r') || (str.[(!i)] = '\r' && str.[(!i)+1] = '\n')
      then (s2.[!j] <- '\n'; i := !i + 2)
      else (s2.[!j] <- str.[(!i)]; i := !i + 1);
      j := !j + 1
    done;
    if (!i) <= len - 1 then begin s2.[!j] <- str.[(!i)]; j := !j + 1 end;
    String.sub s2 0 (!j);;

(** set_folder_start:
    Try to seek to the given position in the given file desc.
    Any failure, close file and return None.
*)
let set_folder_start state fd_opt =
  if state.start_position > 0
  then
    match fd_opt with
      Some fd ->
        (try
           let pos = Folder.lseek fd state.start_position in
           if pos = state.start_position
           then
             (prerr_endline (Printf.sprintf "set_folder_start: pos=%d" pos);
              Some fd)
           else (Folder.closefile fd; None)
         with Unix.Unix_error _ -> (Folder.closefile fd; None))
       | None -> None
  else fd_opt

(** open_folder_read:
    safe replacement for open_file in read mode, check if in valid directory
    special processing required for start_position
*)
let open_folder_read state filename =
  prerr_endline (Printf.sprintf "open_folder_read: %s" filename);
  set_folder_start state (Folder.openfileread state.folder filename)

(** open_folder_write: safe replacement for open_filein write mode, check if in valid directory *)
let open_folder_write state filename =
  prerr_endline (Printf.sprintf "open_folder_write: %s" filename);
  Folder.openfilewrite state.folder filename

(** open_folder_append: safe replacement for open_filein append mode, check if in valid directory *)
let open_folder_append state filename =
  prerr_endline (Printf.sprintf "open_folder_append: %s" filename);
  Folder.openfileappend state.folder filename

(** read_folder:
    just a write-through to the OS read function except that we do the crlf
    conversion on the data.
*)
let read_folder state fd cnt =
  if state.binary
  then Some (Folder.read fd cnt)
  else Some (cr2crlf <| Folder.read fd cnt)

(** write_folder:
    This time we need to extract the data from a buffer provided by Scheduler.read and
    perform the reverse transformation on crlf.
*)
let write_folder state fd (buff:string) (* (buff:FBuffer.t) *) size =
  (*let str = FBuffer.sub buff 0 size in*)
  let str = String.sub buff 0 size in
  let str = if state.binary then crlf2cr str else str in
    Folder.write fd str (String.length str)

(** close_folder: another write-through *)
let close_folder fd =
  prerr_endline (Printf.sprintf "close_folder");
  Folder.closefile fd

(** get_unique_filename: create a unique filename for current directory *)
let get_unique_filename temp pre post = Filename.temp_file ?temp_dir:(Some temp) pre post

(** Return list of files in ascii format (ie. crlf terminated) *)
let plain_file (*folder*)_ filenames = filenames
let ls_file folder filenames = Folder.ls_files folder filenames
let list folder dir list_fn =
  prerr_endline (Printf.sprintf "list: dir=%s" dir);
  match Folder.list folder dir with
      Some files ->
        Array.sort String.compare files;
        (Array.fold_right (fun ss s -> ss^"\r\n"^s) (list_fn folder files) "", true)
    | None -> ("",false)

(** get_passive_port:
    Return a suitable port_spec for using as the passive port.
    Since we can't close a listener, we have to reuse the existing one, if found.
    So we store the port_spec in a reference.
    This may cause a problem for multiple connections.
    We also return the string definition for the PORT reply.
    For now, the IP number is simply set by the caller.
*)
let get_passive_port state (*sched*)_ =
  try
    let state, addr_str, port, port_spec_opt, sec_mode_opt =
      match !(state.pasv_port_spec) with
	  Some port_spec ->
	    prerr_endline (Printf.sprintf "get_passive_port: using old port=%d" port_spec.Network.port);
	    state, Unix.string_of_inet_addr port_spec.Network.addr, port_spec.Network.port, None, None
	| None ->
	    let port = Random.int (state.pasv_port_max - state.pasv_port_min) + state.pasv_port_min in
	    let addr = Unix.inet_addr_of_string state.local_ip_num in
	    let port_spec = Network.make_port_spec ~protocol addr port in
              state.pasv_port_spec := Some port_spec;
              state.pasv_secure_mode := Some Network.Unsecured;
	      prerr_endline (Printf.sprintf "get_passive_port: port=%d" port);
	      prerr_endline (Printf.sprintf "get_passive_port: addr=%s" (Unix.string_of_inet_addr addr));
	      state, state.local_ip_num, port, Some port_spec, Some Network.Unsecured
    in
      match Str.split (Str.regexp_string ".") addr_str with
	  [h1;h2;h3;h4] ->
	    let str = Printf.sprintf "%d,%d,%d,%d,%d,%d" (get255 h1) (get255 h2) (get255 h3) (get255 h4)
                                                  ((port / 256) land 0xff) (port land 0xff) in
	    prerr_endline (Printf.sprintf "get_passive_port: str=%s" str);
	    state, Some (str, port_spec_opt, sec_mode_opt)
	| _ ->
	    prerr_endline (Printf.sprintf "get_passive_port: failed ipnum=%s" state.local_ip_num);
	    state, None
  with exn ->
    prerr_endline (Printf.sprintf "get_passive_port: exn=%s" (Printexc.to_string exn));
    state, None

let all_commands =
  [("ACCT",true);  ("ALLO",true);  ("APPE",true);  ("CDUP",true);  ("CWD ",true);  ("DELE",true);
   ("EPRT",false); ("EPSV",false); ("FEAT",false); ("HELP",true);  ("LIST",true);  ("MDTM",false);
   ("MKD ",true);  ("MODE",true);  ("NLST",true);  ("NOOP",true);  ("OPTS",false); ("PASS",true);
   ("PASV",true);  ("PORT",true);  ("PWD ",true);  ("QUIT",true);  ("REIN",false); ("REST",false);
   ("RETR",true);  ("RMD ",true);  ("RNFR",true);  ("RNTO",true);  ("SITE",false); ("SIZE",true);
   ("SMNT",false); ("STAT",true);  ("STOR",true);  ("STOU",true);  ("STRU",true);  ("SYST",true);
   ("TYPE",true);  ("USER",true);  ("XCUP",false); ("XCWD",false); ("XMKD",false); ("XPWD",false);
   ("XRMD",false)]

let recognized_commands brk =
  (snd <| List.fold_left (fun (i,s) (nm,rc) ->
                            if rc
                            then (i+1,s^" "^nm^(if i mod brk = (brk-1) then "\r\n" else ""))
                            else (i,s)) (0,"") all_commands)^"\r\n"

let server_status state conn =
  Printf.sprintf "    Connected to %s\n\
    %s\n\
    TYPE: %s\n\
    STRU: %s\n\
    MODE: %s\n\
    Session timeout is %.0f seconds\n\
    MLstate ftpServer version %s\n\
"
    (NetAddr.to_string conn.Scheduler.addr)
    (match state.user with Some user -> "Logged in as "^user | None -> "Not logged in")
    (str_of_type_code state.data_type)
    (str_of_structure_code state.structure_code)
    (str_of_transfer_mode state.transfer_mode)
    (Time.in_seconds state.timeout)
    state.version

let get_hello_message state = (List.fold_left (fun ss s -> ss^"220-"^s^"\r\n") "" state.hello_message)^"220 \r\n"

let set_start_position state str =
  try
    let pos = int_of_string str in
    if pos > 0
    then ({state with start_position = pos },string_of_int pos)
    else ({state with start_position = 0 },"0")
  with Failure "int_of_string" -> ({state with start_position = 0 },"0")

(* End of file: ftpServerType.ml *)
