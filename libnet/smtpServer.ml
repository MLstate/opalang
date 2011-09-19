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
(* smtpServer:
   SMTP server.
*)
module SCCp = SmtpServerCore_parse
module SCC = SmtpServerCore

#<Debugvar:HTTP_DEBUG>

let name = "smtpServer"
let version = "1.0beta"

type t = SCC.t

type options =
    { opt_addr: string;
      opt_port: int;
      opt_ssl_cert : string;
      opt_ssl_key : string;
      opt_ssl_pass : string;
      opt_ssl_accept_fun : Ssl.certificate -> bool;
      opt_ssl_always : bool;
      opt_ssl_ca_file : string;
      opt_ssl_ca_path : string;
      opt_ssl_client_ca_file : string;
      opt_ssl_client_cert_path : string;
      opt_ssl_certificate : SslAS.ssl_certificate option;
      opt_ssl_verify_params : SslAS.ssl_verify_params option;
      opt_dialog: string;
      opt_on_server_close : Scheduler.t -> unit;
      opt_name: string;
      opt_email_handler : SCC.email -> int * string
    }

let handle_email { SCC.from=_from; dests=_dests; body=_body } =
  #<If$minlevel 10>Logger.debug "handle_email:\n";
  Logger.debug "FROM: %s TO: [%s]\n" _from (String.concat ", " _dests);
  Logger.debug "%s\n" (Rcontent.get_content _body)#<End>;
  250, "Ok"

let default_options =
  { opt_addr = "0.0.0.0";
    opt_port = 2525;
    opt_ssl_cert = "";
    opt_ssl_key = "";
    opt_ssl_pass = "";
    opt_ssl_accept_fun = (fun _ -> true);
    opt_ssl_always = false;
    opt_ssl_ca_file = "";
    opt_ssl_ca_path = "";
    opt_ssl_client_ca_file = "";
    opt_ssl_client_cert_path = "";
    opt_ssl_certificate = None;
    opt_ssl_verify_params = None;
    opt_dialog = "default";
    opt_on_server_close = (fun _ -> ());
    opt_name = "smtpServerPort";
    opt_email_handler = handle_email
  }

let prefixed_opt name opt = [Printf.sprintf "--%s-%s" name opt; Printf.sprintf "--%s" opt]

let spec_args name =
  let p = prefixed_opt name in
  [
    (p"port")@["-p"],
    ServerArg.func ServerArg.int
      (fun o p -> if p > 0xffff then (Logger.critical "Bad port number: %d" p; exit 1) else { o with opt_port = p }),
    "<int>", "Sets the port on which the server should run";

    (*p"no-flood-prevention",
    ServerArg.func ServerArg.unit (fun o () -> { o with opt_dos_prevention = false }),
    "", "Disable the built-in protection against Denial-of-Service attacks";*)

    p"ssl-cert",
    ServerArg.func ServerArg.string (fun o s -> { o with opt_ssl_cert = s }),
    "<file>", "Location of your SSL certificate (requires ssl-key)";

    p"ssl-key",
    ServerArg.func ServerArg.string (fun o s -> { o with opt_ssl_key = s }),
    "<file>", "Location of your SSL key (requires ssl-cert)";

    p"ssl-pass",
    ServerArg.func ServerArg.string (fun o s -> { o with opt_ssl_pass = s }),
    "<string>", "Password of your SSL certificate (requires ssl-cert and ssl-key options)";

    p"dialog",
    ServerArg.func ServerArg.string (fun o s -> { o with opt_dialog = s }),
    "<string>", "Name of the ftp dialog to use"
  ]

let make_ssl_cert opt =
  match opt.opt_ssl_certificate with
  | Some x -> Some x
  | None ->
    if opt.opt_ssl_cert <> "" then
      if opt.opt_ssl_key <> "" then
        Some (SslAS.make_ssl_certificate opt.opt_ssl_cert opt.opt_ssl_key opt.opt_ssl_pass)
      else begin
        Logger.critical "Error : ssl-cert option MUST be used with ssl-key option";
        exit 1
      end
    else
      None

let make_ssl_verify opt =
  match opt.opt_ssl_verify_params with
  | Some x -> Some x
  | None ->
    if opt.opt_ssl_ca_file <> "" || opt.opt_ssl_ca_path <> "" || opt.opt_ssl_client_cert_path <> "" then
      Some (SslAS.make_ssl_verify_params ~client_ca_file:opt.opt_ssl_client_ca_file
              ~accept_fun:opt.opt_ssl_accept_fun ~always:opt.opt_ssl_always
              opt.opt_ssl_ca_file opt.opt_ssl_ca_path opt.opt_ssl_client_cert_path)
    else
      None


let handle_verify = function
  | _ -> (551,"User not local")
  (* or... *)
  (*| _ -> (553,"User ambiguous")*)
  (*| _ -> (502,"VRFY command is disabled")*)

let handle_expand = function
  | _ -> [(551,"User not local")]

let make (_name:string) (opt:options) (_sched:Scheduler.t) : t =
  let secure_mode = Network.secure_mode_from_params (make_ssl_cert opt) (make_ssl_verify opt) in
  let runtime = { SCC.rt_plim = 128;
                  rt_dialog_name = opt.opt_dialog;
                  rt_on_close = opt.opt_on_server_close;
                  rt_proto = { SCC.rt_name = opt.opt_name;
                               rt_addr = opt.opt_addr;
                               rt_port = opt.opt_port;
                               rt_block_size = 4096;
                               rt_secure_mode = secure_mode;
                               rt_backtrace = true; (* FIXME: put this into options. *)
                               rt_server_write_timeout = Time.seconds 36;
                               rt_payload = ();
                             };
                } in
  let state = { SCC.server_domain = "smtp.localhost.local";
                server_port = 2525;
                hello_message = "";
                client_domain = "";
                callback = opt.opt_email_handler;
                verify = handle_verify;
                expand = handle_expand;
                extended = false;
	      } in
  let lc = Unix.localtime (Unix.gettimeofday ()) in
  let server_msg = Printf.sprintf "MLstate SMTP server at %s:%d.\nStarted on %s, %s."
                           state.SCC.server_domain state.SCC.server_port (Date.date2 lc) (Date.time lc) in
  Logger.notice "%s" server_msg;
  {
    SCC.runtime = runtime;
    err_cont = None;
    extra_params = state;
  }

let get_ports (server:t) (sched:Scheduler.t) = SCC.get_ports server sched

let get_description _smtp_server _sched = `SmtpServer

let run smtp_server _sched = smtp_server

let close (smtp_server:t) sched = smtp_server.SCC.runtime.SCC.rt_on_close sched
