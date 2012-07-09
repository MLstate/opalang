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
   @author Adam Koprowski
*)

type protocol = string

type fd = Unix.file_descr

type addr = Unix.inet_addr

type connection_type = TCP | UDP | SSL | FILE

(** The address of a connection *)
type t =
  | Tcp of (protocol * Unix.file_descr * Unix.inet_addr)
  | Udp of (protocol * Unix.file_descr * Unix.inet_addr)
  | Ssl of (protocol * Ssl.socket * Unix.inet_addr)
  | File of Unix.file_descr

let mk_protocol x = x

let mk_tcp ~protocol ~fd ~addr =
  Tcp (protocol, fd, addr)

let mk_udp ~protocol ~fd ~addr =
  Udp (protocol, fd, addr)

let mk_file ~fd =
  File fd

let get_type = function
  | Tcp _ -> TCP
  | Udp _ -> UDP
  | Ssl _ -> SSL
  | File _ -> FILE

let get_addr = function
  | Tcp (_, _, addr) -> addr
  | Udp (_, _, addr) -> addr
  | Ssl (_, _, addr) -> addr
  | File _ -> raise (Invalid_argument "NetAddr.get_addr on a File")

let get_type_and_fd = function
  | Tcp (_, fd, _) -> `Tcp fd
  | Udp (_, fd, _) -> `Udp fd
  | Ssl (_, fd, _) -> `Ssl fd
  | File fd -> `File fd

let get_fd = function
  | Tcp (_, fd, _) -> fd
  | Udp (_, fd, _) -> fd
  | Ssl (_, s, _) -> Ssl.file_descr_of_socket s
  | File fd -> fd

let get_protocol = function
  | Tcp (prot, _, _) -> prot
  | Udp (prot, _, _) -> prot
  | Ssl (prot, _, _) -> prot
  | File _ -> "FILE"

let secured_from_normal secured_fd =
  function
  | Tcp (prot, _, addr) -> Ssl (prot, secured_fd, addr)
  | Udp _ -> assert false
  | Ssl _ -> assert false
  | File _ ->  assert false

let string_of_sockaddr = function
  | Unix.ADDR_UNIX fd -> "Unix FD:" ^ fd
  | Unix.ADDR_INET (addr, port) -> Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

let to_string addr =
  let show t prot addr =
    Printf.sprintf "%s [%s,%s]"
      (Unix.string_of_inet_addr addr) t prot
  in
  match addr with
  | Tcp (prot, _, addr) -> show "TCP" prot addr
  | Udp (prot, _, addr) -> show "UDP" prot addr
  | Ssl (prot, _, addr) -> show "SSL" prot addr
  | File fd -> Printf.sprintf "FILE(%d)" (Obj.magic fd : int)
