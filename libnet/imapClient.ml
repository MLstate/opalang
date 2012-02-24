(*
    Copyright © 2011, 2012 MLstate

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

(** This is a module for handling imap mail sending.
    It is NOT really RFC compliant. *)

module ICC = ImapClientCore
module List = Base.List
module String = Base.String
let (<|) f a = f a
let (|>) a f = f a
let ( @* ) g f x = g(f(x))

let sprintf = Printf.sprintf

exception Bad_address of string
exception Too_much_try
exception Unknown_address of string

let read_code s =
  let get i = int_of_char (String.unsafe_get s i) - 48 in
  let l = String.length s in
  if l > 3 then 100 * get 0 + 10 * get 1 + get 2, String.sub s 4 (4 - 3)
  else 0, "unknown server answer"

let analyze_error = Mailerror.parse_mailerror_error

let mail_recv_aux ?client_certificate ?verify_params ?(secure=false) sched
                  ~addr ?(port=993)
                  ~mailbox ~username ~password
                  ?(command=ICC.ImapNoop) cont () =
  let mail = { ICC.mailbox=mailbox;
               username=username;
               password=password;
               command=command;
               fetched=[];
               result=(ICC.Ok "nothing happened");
               from = ""; dests = []; data = "" } in
  let rec try_mx mail cont =
    let tools = {
      ICC.k = (function res -> cont res);
    } in
    let client = {
      ICC.runtime = {
        ICC.rt_plim = 128;
        ICC.rt_proto = {
          ICC.rt_name = "";
          rt_addr = "";
          rt_port = 0;
          rt_secure_mode = Network.Unsecured;
          rt_block_size = 4096;
          rt_backtrace = true;
          rt_server_write_timeout = Time.hours 2;
          rt_payload = ();
        };
      };
      ICC.err_cont = None;
      ICC.extra_params = (mail,tools)
    } in
    let secure_mode =
      if secure
      then Network.Secured (client_certificate, verify_params)
      else Network.Unsecured
    in ICC.connect client ~secure_mode sched addr port
  in try_mx mail cont

let mail_recv ?client_certificate ?verify_params ?secure sched
              ~addr ?port
              ~mailbox ~username ~password ?command
              (cont:ICC.result -> unit) () =
  mail_recv_aux ?client_certificate ?verify_params ?secure sched
                ~addr ?port
                ~mailbox ~username ~password ?command
                cont ()
