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

let mail_recv_aux ?client_certificate ?verify_params ?(secure=false) sched
                  ~addr ?(port=993)
                  ~username ~password
                  ?(commands=[]) cont ?err_cont () =
  let mail = { ICC.username=username;
               password=password;
               commands=commands;
               status={ ICC.flags=""; exists=(-1); recent=(-1); oks=[]; rwstatus="" };
               fetched=[]; list=[]; expunged=[];
               results=[];
               from=""; dests=[]; data="" } in
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
        ICC.rt_tmp =
          { ICC.rt_callback = None;
          };
      };
      ICC.err_cont = err_cont;
      ICC.extra_params = (mail,tools)
    } in
    let secure_mode =
      if secure
      then Network.Secured (client_certificate, verify_params)
      else Network.Unsecured
    in
    try
      ICC.connect client ~secure_mode sched addr port
    with exn -> cont ([ICC.Error (Printf.sprintf "Got connection exception: %s" (Printexc.to_string exn))])
  in
  try_mx mail cont

let mail_recv ?client_certificate ?verify_params ?secure sched
              ~addr ?port
              ~username ~password ?commands
              (cont:ICC.results -> unit) ?err_cont () =
  mail_recv_aux ?client_certificate ?verify_params ?secure sched
                ~addr ?port
                ~username ~password ?commands
                cont ?err_cont ()
