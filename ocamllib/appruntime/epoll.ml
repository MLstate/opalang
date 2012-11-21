(*
    Copyright © 2011, 2012 MLstate

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

(* THIS FILE HAS A DOCUMENTED MLI *)

(* error reporting *)
type error = int * string
let error_code = fst
let error_message = snd
exception Error of error

(* epoll descriptor *)
type epoll_descriptor = int

#<Ifstatic:MLSTATE_WINDOWS .*>
let last_error_message _ = assert false
let last_error_code _ = assert false
let ep_create _ = assert false
let ep_close _ = assert false
let ep_in _ = assert false
let ep_out _ = assert false
let ep_err _ = assert false
let ep_hup _ = assert false
let _on_mac _ = assert false
let on_mac = false
#<Else>
external last_error_message : unit -> string = "error_msg"
external last_error_code : unit -> int = "error_value"
external ep_create : int -> epoll_descriptor = "ep_create"
external ep_close : epoll_descriptor -> int = "ep_close"
external ep_in : unit -> int = "ep_EPOLLIN"
external ep_out : unit -> int = "ep_EPOLLOUT"
external ep_err : unit -> int = "ep_EPOLLERR"
external ep_hup : unit -> int = "ep_EPOLLHUP"
external _on_mac : unit -> bool = "on_mac"
let on_mac = _on_mac ()
#<End>

(* check_ret factory *)
let check_ret str ret =
  if ret = -1 then
    let message = Printf.sprintf "%s: %s" str (last_error_message ()) in
    let code = last_error_code () in
    Logger.error "Epoll error %d: %s" code message(* ; *)
    (* raise (Error (code, message)) *)

(* events *)
type supported_event = In | Out | Hup | Err | Unsupported of int

type event = int
type event_mask = int

let event_in, event_out, event_in_out, event_hup, event_err =
    let u() =(ep_in(), ep_out(), (ep_in() lor ep_out()), ep_hup(), ep_err()) in
    let z()= (0, 0, 0, 0, 0) in
    Mlstate_platform.platform_dependent
      ~unix:u
      ~cygwin:z
      ~windows:z
      () ()

let event_mask_to_list event_mask =
  (* Frequent cases have to be memoized *)
  if event_mask = event_in then [In] else
  if event_mask = event_out then [Out] else
  if event_mask = (event_in lor event_out) then [In; Out]
  else begin
    let f (r, l) (ie, ee) = if ((ie land r) = ie) then ((ie lxor r), ee::l) else (r, l) in
    let rec aux (event_mask, event_list) events =
      if event_mask = 0 then event_list
      else (
        match events with
        | [] -> (Unsupported event_mask)::event_list
        | h::t -> aux (f (event_mask, event_list) h) t
      )
    in
    aux (event_mask, []) [(event_in, In); (event_out, Out); (event_hup, Hup); (event_err, Err)]
  end


(* let event_list_to_mask event_list = *)
(*   List.fold_left *)
(*     (fun e_mask ee -> *)
(*        let ie = *)
(*          match ee with *)
(*          | In -> event_in *)
(*          | Out -> event_out *)
(*          | Err -> event_err *)
(*          | Hup -> event_hup *)
(*          | Unsupported e -> e *)
(*        in *)
(*        ie lor e_mask *)
(*     ) 0 event_list *)

(* requests *)

(* low level : private *)
#<Ifstatic:MLSTATE_WINDOWS .*>
let ep_add _ _ _ = assert false
let ep_del _ _ _ = assert false
let ep_mod _ _ _ = assert false
let ep_wait _ ~maxevents ~timeout =
  let _ = ignore (maxevents, timeout) in
    assert false
#<Else>
external ep_add : epoll_descriptor -> Unix.file_descr -> event_mask-> int = "ep_add"
external ep_del : epoll_descriptor -> Unix.file_descr -> event_mask -> int = "ep_del"
external ep_mod : epoll_descriptor -> Unix.file_descr -> event_mask -> int = "ep_mod"
external ep_wait : epoll_descriptor -> maxevents : int -> timeout : int -> (Unix.file_descr * event_mask) array = "ep_wait"
#<End>

(* high level : exported *)

(* The test [if int_of_filedesc fd >= 0 then] is there because weblib is also used with
   dummy_connection (cf doc in mli). Dummy connections build negative file descriptor
   In this case, Epoll ignore these file descriptors.
*)
external int_of_filedesc : Unix.file_descr -> int = "%identity"
(* external filedesc_of_int : int -> Unix.file_descr = "%identity" *)
(* external int_of_epoll_descriptor : epoll_descriptor -> int = "%identity" *)
external epoll_descriptor_of : int -> epoll_descriptor = "%identity"

let combine =
  List.fold_left (lor) 0

let create size =
  Mlstate_platform.on_windows (fun()->failwith "ep_create on windows");
  let ret = ep_create size in
  check_ret "epoll_create" ret;
  epoll_descriptor_of ret (* normally useless ?? *)

let close ed = check_ret "epoll_close" (ep_close ed)

let del epfd fd =
  Mlstate_platform.on_windows (fun()->failwith "ep_del on windows");
  if int_of_filedesc fd >= 0 then
    let ret =
      if on_mac then (
        let _ = ep_del epfd fd event_in in
        ep_del epfd fd event_out )
      else
        ep_del epfd fd 0
    in
    check_ret "epoll_del" ret

let listen_in_out epfd ?(is_new_fd=false) fd  =
  if int_of_filedesc fd >= 0 then
    let ret =
      if on_mac then (
        let _ = ep_add epfd fd event_in in
        ep_add epfd fd event_out )
      else (
        if is_new_fd then
          ep_add epfd fd event_in_out
        else
          ep_mod epfd fd event_in_out )
    in
    check_ret "listen_in_out" ret

let listen_x_only x y epfd is_new_fd fd =
  if int_of_filedesc fd >= 0 then
    let ret =
      if on_mac then (
        let _ = ep_del epfd fd y in
        ep_add epfd fd x )
      else (
        if is_new_fd then
          ep_add epfd fd x
        else
          ep_mod epfd fd x )
    in
    check_ret "listen_x_only" ret

let listen_in_only epfd is_new_fd fd = listen_x_only event_in event_out epfd is_new_fd fd
let listen_out_only epfd is_new_fd fd = listen_x_only event_out event_in epfd is_new_fd fd

let wait ?tout:(timeout = -1) epfd maxevents =
  Mlstate_platform.on_windows (fun()->failwith "ep_wait on windows");
  let a = ep_wait epfd ~maxevents ~timeout in
  Array.map (fun (fd, events) -> (fd, event_mask_to_list events)) a

module Debug =
struct
  (* This functions are used only for printing file descriptors and events (useful for debugging) *)
  (* http://caml.inria.fr/pub/ml-archives/caml-list/2002/06/b0e3d11df12ca90608634197c0792939.en.html *)
  external int_of_filedescr : Unix.file_descr -> int = "%identity"
  external filedescr_of_int : int -> Unix.file_descr = "%identity"
  external int_of_events : event_mask -> int = "%identity"
  external int_of_epoll_descriptor : epoll_descriptor -> int = "%identity"

  (* tests *)
(*   let test() = *)
(*     let fd = create (10) in *)
(*     add fd Unix.stdout event_in; *)
(*     add fd Unix.stdin event_in; *)
(*     let arr = wait fd 10 in *)
(*     Printf.printf "Got: %d\n" (Array.length arr) *)
end
