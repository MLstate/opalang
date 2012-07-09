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
type supported_event = In | Out | Err

(* external fct : unit -> int = "fct" *)
let dummy_fun1 _ = assert false
let dummy_fun2 _ _ = assert false
let dummy_fun3 _ _ _ = assert false

#<Ifstatic:MLSTATE_WINDOWS .*>
external socket       : unit -> Unix.file_descr                   = "iocp_ml_socket"
external iocp_async_wait   : int -> Unix.file_descr * int         = "iocp_ml_wait"
external async_accept : Unix.file_descr ->  Unix.file_descr       = "iocp_ml_accept"
external async_init   : unit -> unit                              = "iocp_ml_async_init"
external async_read   : Unix.file_descr -> int -> Unix.file_descr = "iocp_ml_read"
external async_write  : Unix.file_descr -> string -> int -> Unix.file_descr  = "iocp_ml_write"
external get_socket   : unit -> Unix.file_descr                   = "iocp_ml_get_socket"
external get_buffer   : unit -> string                            = "iocp_ml_get_buffer"

let async_wait t =
  let (fd, int_event) = iocp_async_wait t in
  let event =
    if int_event = 0 then In
    else if int_event = 1 then Out
    else Err
  in
  Printf.printf "new event: %d, %d" (Epoll.Debug.int_of_filedescr fd) int_event;
  flush stdout;
  (fd, event)

#<Else>
(* SHOULD BE EMPTY *)
let (socket,async_wait,async_accept,async_init,async_read,async_write,get_socket,get_buffer) =
    (dummy_fun1,dummy_fun1,dummy_fun1,dummy_fun1,dummy_fun2,dummy_fun3,dummy_fun1,dummy_fun1)
#<End>
