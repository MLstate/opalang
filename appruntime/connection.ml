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

(*
  This module deals with file descriptors and synchronous operations over them.

  @author Henri Binsztok
  @author Laurent Le Brun
  @author Frederic Ye
  @author Cedric Soulas
*)

module MP = Mlstate_platform
module NA = NetAddr

let (|>) = InfixOperator.(|>)

module Const =
struct
  let unix_max_pending_requests = 1024
end

type socket_type = TCP | UDP

(* ============================== *)
(*       In / Out Operations      *)
(* ============================== *)

exception Busy
exception Error

(* Private function *)
let nonblocking_try f zero =
  try f () with
  | Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _)
  | Ssl.Write_error (Ssl.Error_want_read | Ssl.Error_want_write)
  | Ssl.Read_error (Ssl.Error_want_read | Ssl.Error_want_write) -> raise Busy
  | Ssl.Read_error _
  | Ssl.Write_error _ -> zero
  | Unix.Unix_error((Unix.EPIPE | Unix.ECONNRESET), _, _) -> zero
  | Unix.Unix_error(err, ctx1, ctx2) as e -> Logger.error "Unix error: %s, %s, %s\n" (Unix.error_message err) ctx1 ctx2; raise e
  | e -> raise e

(* FIXME: use FBuffers for large inputs ? *)
let write conn ?(pos=0) buf len =
  nonblocking_try (
    fun () ->
      match NA.get_type_and_fd conn with
      | `File fd -> Unix.write fd buf pos len
      | `Tcp fd -> Unix.send fd buf pos len []
      | `Udp fd -> Unix.sendto fd buf pos len [] (Unix.getpeername fd)
      | `Ssl s -> Ssl.write s buf pos len
   ) 0

(* FIXME, should that really only work for UDP sockets? *)
let write_to conn addr ?(pos=0) buf len =
  nonblocking_try (
    fun () ->
      match NA.get_type_and_fd conn with
      | `Udp fd -> Unix.sendto fd buf pos len [] addr
      | _ -> failwith "[Connection] write_to used on a non-UDP socket"
   ) 0

let read_aux conn tmp to_read : int * Unix.sockaddr option =
  let no_addr res = res, None in
  nonblocking_try (
    fun () ->
      match NA.get_type_and_fd conn with
      | `File fd -> Unix.read fd tmp 0 to_read |> no_addr
      | `Tcp fd -> Unix.recv fd tmp 0 to_read [] |> no_addr
      | `Udp fd ->
          let len, addr = Unix.recvfrom fd tmp 0 to_read [] in
          len, Some addr
      | `Ssl s -> Ssl.read s tmp 0 to_read |> no_addr
  ) (no_addr 0)

let read_more conn buf to_read =
  let tmp = String.create to_read in
  let nread, _ = read_aux conn tmp to_read  in
  let buf = FBuffer.add_substring buf tmp 0 nread in
  nread, buf

let read_content conn content to_read =
  let tmp = String.create to_read in
  let nread, _ = read_aux conn tmp to_read  in
  let content = Rcontent.content_add (String.sub tmp 0 nread) content in
  nread, content

let read_buff_length = 32768
let read_buff = String.create read_buff_length

let read_more2 conn buf =
  let nread, _ = read_aux conn read_buff read_buff_length in
  let () = Buffer.add_substring buf read_buff 0 nread in
  nread, buf

let read_more4 conn buf =
  let nread, _ = read_aux conn read_buff read_buff_length in
  let () = Buf.add_substring buf read_buff 0 nread in
  nread, buf

let read_from conn =
  let nread, addr = read_aux conn read_buff read_buff_length in
  let get_peer = lazy (Unix.getpeername (NA.get_fd conn)) in
  let from = Option.default_lazy get_peer addr in
  nread, from, (String.sub read_buff 0 nread)

let read conn =
  let nread, _ = read_aux conn read_buff read_buff_length in
  nread, (String.sub read_buff 0 nread)

let _ =
    MP.on_windows Iocp.async_init;

exception PermissionDenied
exception UnixError

(* Private function *)
let make_socket ?(socket_flags=([] : Unix.socket_bool_option list)) socket_type =
  let sock =
    match socket_type with
    | TCP ->
        MP.platform_dependent
          ~unix:   (fun()-> Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0)
          ~windows:(fun()-> Iocp.socket())
          () ()
    | UDP ->
        MP.platform_dependent
          ~unix:   (fun()-> Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0)
          ~windows:(fun()-> assert false)
          () ()
  in
  Unix.set_nonblock sock;
  List.iter (fun opt -> Unix.setsockopt sock opt true)  socket_flags;
  sock

let accept sock =
  try
    let (sd, sa) = Unix.accept sock in
    match sa with
    | Unix.ADDR_INET (host, _) ->
        Unix.set_nonblock sd;
        sd, host
    | _ ->
        Logger.error "Connection refused (unknown client)";
        (try Unix.close sd with Unix.Unix_error _ -> ()); raise Error
  with
    Unix.Unix_error _ as e ->
      Logger.error "Impossible to accept connection: (%s)" (Printexc.to_string e);
      raise Error

let connect ?(socket_type = TCP) ?socket_flags addr =
  let sock = make_socket ?socket_flags socket_type in
  try
    begin
      try Unix.connect sock addr
        (* Use epoll to be warned when connect is finished *)
      with Unix.Unix_error (Unix.EINPROGRESS, _, _) -> ()
    end;
    sock
  with
  | Unix.Unix_error(e, fct, arg) as exn ->
      Logger.error "Unix error opening connection: %s for %s %s" (Unix.error_message e) fct arg;
      raise exn
  | e ->
      Logger.error "Fatal error opening connection. Closing socket...";
      Unix.close sock ;
      raise e

let listen ?(socket_type = TCP) ?socket_flags addr =
  let sock = make_socket ?socket_flags socket_type in
  MP.on_unixes (fun()->Unix.set_close_on_exec sock);
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  let _ = try Unix.bind sock addr;
  with
  | Unix.Unix_error(Unix.EACCES, _fct, _arg) ->
      Logger.critical "Error binding to [%s]: Permission denied" (NA.string_of_sockaddr addr);
      (match addr with Unix.ADDR_INET (_addr,port) when port < 1024 ->
         Logger.critical "Hint: you probably need to be root to run servers on ports < 1024"
       | _ -> ());
      exit 20
  | Unix.Unix_error(Unix.EADDRINUSE, _fct, _arg) ->
      Logger.critical "Error binding to [%s]: Address already in use" (NA.string_of_sockaddr addr);
      let port = match addr with Unix.ADDR_INET (_addr,port) -> port | _ -> assert false in
      Logger.critical "Hint: a server seems to be running already on port %d, either close it or use a different port number" port;
      exit 21
  | Unix.Unix_error(e, fct, arg) ->
      Logger.critical "Error binding on [%s]: %s for %s %s" (NA.string_of_sockaddr addr) (Unix.error_message e) fct arg;
      raise UnixError
  in
  begin match socket_type with
  | TCP -> Unix.listen sock Const.unix_max_pending_requests
  | UDP -> () (* we don't call listen for UDP, binding the socket is enough *)
  end;
  sock

(* ============================== *)
(*             Misc               *)
(* ============================== *)

let close descr =
  let fd = NA.get_fd descr in
  (try Unix.close fd
   with e -> Logger.error "unix close error: %s " (Printexc.to_string e);
      );
  (try match NA.get_type_and_fd descr with
   | `Ssl s ->
       Ssl.shutdown s; Unix.shutdown fd Unix.SHUTDOWN_SEND
   | `Tcp fd -> Unix.shutdown fd Unix.SHUTDOWN_SEND
   | `Udp _ -> () (* UDP does not require a shutdown *)
   | `File fd -> Unix.close fd
   with Unix.Unix_error _ -> ())

let name_of_addr addr =
  try (Unix.gethostbyaddr addr).Unix.h_name
  with Not_found -> Unix.string_of_inet_addr addr
