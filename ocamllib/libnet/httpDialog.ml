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

module A = ServerArg

type dialog = Scheduler.t -> HttpServerTypes.web_info -> unit

type t = {
  body : dialog Lazy.t;
  allowed : (Unix.inet_addr -> bool) ;
  content : (string -> int -> int -> unit) ;
  url_prefix : string
}

type options = {
  opt_allowed : Unix.inet_addr -> bool;
  dialog_name : string;
  dialog : unit -> dialog;
  opt_url_prefix : string;
}

type port = {
  set_dialog : t -> unit
}


let name = "httpDialog"
let version = "1.0"

let null_dialog = Obj.magic None

let default_options = {
  opt_allowed = (fun _ -> true);
  dialog_name = "default";
  dialog = null_dialog;
  opt_url_prefix = "";
}

let options_with_dialog dialog =
  {default_options with dialog = dialog}

let spec_args _name = []

let make _name opt _sched =
  if opt.dialog == null_dialog then begin
    Logger.error "No dialog provided"; exit 1
  end;
  {
    body = Lazy.lazy_from_fun opt.dialog;
    allowed = opt.opt_allowed ;
    url_prefix = opt.opt_url_prefix ;
    content = (fun _url _total _current -> ()) ;
  }

let get_ports _ _ = []

let get_description http_dialog _sched =
  `HttpDialog http_dialog

let run http_dialog _ = http_dialog

let close _ _ = ()

let body http_dialog sched winfo =
  (Lazy.force http_dialog.body) sched winfo

let content http_dialog = http_dialog.content

let is_allowed http_dialog conn =
  http_dialog.allowed (Scheduler.get_connection_inet_addr conn)
