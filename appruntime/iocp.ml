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
type supported_event = Read | Write | Accept | Connect | Err
(* epoll: type supported_event = In | Out | Hup | Err | Unsupported of int *)

#<Ifstatic:OS Win.*>
type event = int

external associate_iocp  : Unix.file_descr -> unit                   = "iocp_associate_iocp" (* associate a socket to IOCP *)
external iocp_async_wait : int -> (Unix.file_descr * event) array    = "iocp_ml_wait"
external async_accept    : Unix.file_descr -> Unix.file_descr        = "iocp_ml_accept"
external async_connect   : Unix.file_descr -> Unix.sockaddr -> unit  = "iocp_ml_connect"
external iocp_async_read : Unix.file_descr -> int -> bool -> unit    = "iocp_ml_read" (* bool = UDP *)
external iocp_async_write: Unix.file_descr -> string -> int -> Unix.sockaddr option -> unit  = "iocp_ml_write"

(* clear all buffers from last call to free up memory, mainly useful after a read *)
external clear_last_context: unit -> unit                            = "iocp_ml_clear_last_context"

(* retrieves the buffer from the last completed io, useful after a READ completion is signaled *)
external get_last_buffer : Unix.file_descr -> string                 = "iocp_ml_get_last_buffer"

(* retrieves the address of the sender of the last packet - UDP only *)
external get_last_addr   : Unix.file_descr -> Unix.sockaddr          = "iocp_ml_get_socket_addr"

let addr_of_sockaddr = function
    Unix.ADDR_INET(addr, _) -> addr
  | _ -> assert false

let get_last_addr_inet fd =
  addr_of_sockaddr (get_last_addr fd)

(* for debug purpose to print the Unix.file_descr address, dropped using Obj.magic because of random results *)
external int_of_filedescr : Unix.file_descr -> int                    = "fd_to_int"

(* iocp operation constants (Read, Write, Accept, Err), allows to know what type of io completed *)
external ex_iocp_read    : unit -> int                               = "io_IOCP_READ"
external ex_iocp_write   : unit -> int                               = "io_IOCP_WRITE"
external ex_iocp_accept  : unit -> int                               = "io_IOCP_ACCEPT"
external ex_iocp_connect : unit -> int                               = "io_IOCP_CONNECT"
external ex_iocp_err     : unit -> int                               = "io_IOCP_ERR"

let iocp_read, iocp_write, iocp_accept, iocp_connect, iocp_err = (ex_iocp_read(), ex_iocp_write(), ex_iocp_accept(), ex_iocp_connect(), ex_iocp_err())

let event_of_op op =
  if (op == iocp_read) then         Read
  else if (op == iocp_write) then   Write
  else if (op == iocp_accept) then  Accept
  else if (op == iocp_connect) then Connect
  else                              Err

(* epoll: type supported_event = In | Out | Hup | Err | Unsupported of int *)
let epoll_list_of_iocp_op op =
  if (op == iocp_read) then         [ Epoll.In ]
  else if (op == iocp_write) then   [ Epoll.Out ]
  else if (op == iocp_accept) then  [ Epoll.In ]
  else if (op == iocp_connect) then [ Epoll.Out ]
  else                              [ Epoll.Err ]

let op_of_event event =
  match event
    with Read -> iocp_read
	|    Write -> iocp_write
	|    Accept -> iocp_accept
	|    Connect -> iocp_connect
	|    Err -> iocp_err

let async_wait tout =
  let a = iocp_async_wait tout in
  (* Logger.error "nb of events received: %d" (Array.length a); *)
  Array.map (fun (fd, op) -> (fd, epoll_list_of_iocp_op op)) a

let async_read fd len =			(* TCP only *)
  iocp_async_read fd len false

let async_read_from fd len =	(* UDP only *)
  iocp_async_read fd len true

let async_write fd buf len =
  iocp_async_write fd buf len None

let async_write_to fd buf len addr =
  iocp_async_write fd buf len (Some addr)

let str_of_op op =
  if (op == iocp_read) then         "IOCP_READ"
  else if (op == iocp_write) then   "IOCP_WRITE"
  else if (op == iocp_accept) then  "IOCP_ACCEPT"
  else if (op == iocp_connect) then "IOCP_CONNECT"
  else if (op == iocp_err) then     "IOCP_ERR"
  else                              "IOCP_UNKNOWN"

let str_of_event event =
  match event
  with Read     -> "READ"
  |    Write    -> "WRITE"
  |    Accept   -> "ACCEPT"
  |    Connect  -> "CONNECT"
  |    Err      -> "ERR"

#<Else>
(* external fct : unit -> int = "fct" *)
let dummy_fun1 _ = assert false
let dummy_fun2 _ _ = assert false
let dummy_fun3 _ _ _ = assert false

(* SHOULD BE EMPTY *)
let (socket,async_wait,async_accept,async_init,async_read,async_write,get_socket,get_buffer) =
    (dummy_fun1,dummy_fun1,dummy_fun1,dummy_fun1,dummy_fun2,dummy_fun3,dummy_fun1,dummy_fun1)
#<End>
