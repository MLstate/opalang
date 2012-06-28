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

module NA = NetAddr
type secure_mode =
  | Unsecured
  | Secured of SslAS.secure_type

type socket_type = TCP | UDP

type port_spec = {
  addr : Unix.inet_addr;
  port : int;
  prot : NetAddr.protocol;
  stype : Connection.socket_type
}

type port = {
  conn_incoming :
    SslAS.secure_response -> Scheduler.connection_info -> unit;
  conn_terminating : unit -> unit;
  secure_mode : secure_mode;
  port_spec : port_spec
}

type socket = Unix.file_descr

let _SUSPICIOUS_ mess =
  Logger.debug "!!!\nSUSPICIOUS %s\n!!!!" mess;
  ()

exception Unknown_machine of string

let inet_addr_of_name machine =
  try (Unix.gethostbyname machine).Unix.h_addr_list.(0)
  with Unix.Unix_error _ | Not_found -> try Unix.inet_addr_of_string machine
  with Unix.Unix_error _ | Failure _ -> raise (Unknown_machine machine)

let addr_of_ipv4 (ip1, ip2, ip3, ip4) =
  Unix.inet_addr_of_string (Printf.sprintf "%d.%d.%d.%d" ip1 ip2 ip3 ip4)

let string_of_ipv4 (ip1, ip2, ip3, ip4) =
  Printf.sprintf "%d.%d.%d.%d" ip1 ip2 ip3 ip4

let name_of_addr addr =
  try (Unix.gethostbyaddr addr).Unix.h_name
  with Not_found -> Unix.string_of_inet_addr addr

let make_port_spec ?(socket_type = TCP) ~protocol addr port =
  let stype =
    match socket_type with
    | TCP -> Connection.TCP
    | UDP -> Connection.UDP
  in
  { addr = addr
  ; port = port
  ; prot = protocol
  ; stype = stype
  }

let get_port p = p.port

let get_addr p = p.addr

let get_socket_type p =
  match p.stype with
  | Connection.TCP -> TCP
  | Connection.UDP -> UDP


(* == LISTEN == *)

let listen_port port_spec = Connection.listen ~socket_type:port_spec.stype (Unix.ADDR_INET (port_spec.addr, port_spec.port))

  (* Only used by a normal connection (listen_normal),
     but listen_ssl uses listen_normal *)
let new_client_UNIX sched _conn (server_fun: Scheduler.connection_info -> unit) conn () =
  try
    let sock = Scheduler.get_connection_fd conn in
    let client_sock, host =  Connection.accept sock in
    let addr = NA.mk_tcp
      ~protocol:(NA.get_protocol conn.Scheduler.addr)
      ~fd:client_sock ~addr:host
    in
    let conn = Scheduler.make_connection sched addr in
    server_fun conn
  with Connection.Error -> ()

(* let rec new_client_WINDOWS sched _unused_conn (server_fun: Scheduler.connection_info -> unit) (\*sock*\) conn () = *)
(*   try *)
(*     (\* listen for other clients *\) *)
(*     (\* let _id = Iocp.async_accept sock in *\) *)
(*     (\* _SUSPICIOUS_ "What is that self#new_clien callback IN the new_client method ????" ; *\) *)
(*     let callback () = () *)
(*       (\* new_client_WINDOWS conn server_fun sock () *\) *)
(*     in *)
(*     ignore(Scheduler.listen sched conn (\* TODO ~async_id:id *\) callback); *)
(*     let sd = Iocp.get_socket() in *)
(*     let host = Unix.inet_addr_loopback in (\*FIXME*\) *)
(*     let conn = Scheduler.make_connection sched (Scheduler.Normal (sd, host)) in *)
(*     server_fun conn *)
(*   with Connection.Error -> () *)
(*       (\* Unix.Unix_error _ as e -> *\) *)
(*       (\*   Logger.warning "Net.server: can't accept connection (%s)" (Printexc.to_string e) *\) *)


let new_client_WINDOWS sched _conn (server_fun: Scheduler.connection_info -> unit) conn () =
  try
    let sock = Scheduler.get_connection_fd conn in
    let client_sock, host =  Connection.accept sock in
    let addr = NA.mk_tcp
      ~protocol:(NA.get_protocol conn.Scheduler.addr)
      ~fd:client_sock ~addr:host
    in
    let conn = Scheduler.make_connection sched addr in
    server_fun conn
  with Connection.Error -> ()

let new_client = Mlstate_platform.platform_dependent ~unix:new_client_UNIX ~windows:new_client_WINDOWS ()

let listen_normal sched conn server_fun =
  Scheduler.listen sched conn (new_client sched conn server_fun conn)
(*     platform_dependent *)
(*       ~unix:(fun ()-> Scheduler.listen sched conn (new_client sched conn server_fun conn)) *)
(*       ~windows:(fun ()-> *)
(*                   let _id = Iocp.async_accept sock in *)
(*                   Scheduler.listen (\* TODO ~async_id:id *\) sched conn (new_client sched conn server_fun conn)) () () *)


(* == Public functions == *)

let listen sched port_spec secure_mode ?socket_flags server_fun =
  let socket = Connection.listen ~socket_type:port_spec.stype ?socket_flags (Unix.ADDR_INET (port_spec.addr, port_spec.port)) in
  let addr = NA.mk_tcp ~protocol:port_spec.prot
    ~fd:socket ~addr:Unix.inet_addr_loopback
  in
  let conn = Scheduler.make_connection sched addr in
  let listen_key =
    match secure_mode with
    | Unsecured ->
        listen_normal sched conn (server_fun SslAS.UnsecuredRes)
    | Secured params ->
        listen_normal sched conn (SslAS.get_listen_callback sched params server_fun)
  in
  (fun () ->
     Scheduler.abort sched listen_key;
     Scheduler.remove_connection sched conn
  )

let connect sched port_spec secure_mode ?socket_flags ?err_cont cont =
  let sockaddr = Unix.ADDR_INET (port_spec.addr, port_spec.port) in
  let sock = Connection.connect ?socket_flags sockaddr in
  let addr = NA.mk_tcp ~protocol:port_spec.prot
    ~fd:sock ~addr:port_spec.addr
  in
  let conn = Scheduler.make_connection sched addr in
  let err_cont e =
    Scheduler.remove_connection sched conn;
    match err_cont with
    | Some f -> f e
    | None -> ()
  in
  match secure_mode with
  | Unsecured -> Scheduler.connect sched conn (fun () -> cont conn) ~err_cont
  | Secured ssl_params ->
      let normal_cont () = SslAS.connect sched conn ssl_params ~err_cont cont in
      (* Wait for normal connect to be done *)
      Scheduler.connect sched conn normal_cont ~err_cont

let secure_mode_from_params certificate verify_params =
  match certificate, verify_params with
  | Some _, _
  | _, Some _ -> Secured (certificate, verify_params)
  | _ -> Unsecured


let loop sched =
  (* Printexc.record_backtrace true; (\* for get_backtrace below *\) *)
  Sys.catch_break true; (* turn on handlig of Ctrl-c in async *)
  let loop = ref true in
  while !loop do
    try
      loop := Scheduler.wait sched ~block:true
    with
      | Sys.Break -> loop := false
      | Failure "Interrupted system call" -> loop := false
      | e -> Logger.log_error "%s" (Printexc.to_string e)
  done
