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
(* ftpServer:
   FTP server.
*)
module FSCp = FtpServerCore_parse
module FSC = FtpServerCore
module FST = FtpServerType
let protocol = FST.protocol

#<Debugvar:HTTP_DEBUG>

let hello =
["";
 "Welcome to the MLstate experimental FTP service";
 "";
 " - write access to user admin only";
 " - log in as user anonymous, give your email address as password";
 ""]

let goodbye = "Mlstate hopes you enjoyed the FTP experience. Goodbye."

let name = "ftpServer"
let version = "1.0beta"

type t = FSC.t

type options =
    { opt_version: string;                                      (** ftpServer version string *)
      opt_hello_message: string list;                           (** Message for new connection *)
      opt_goodbye_message: string;                              (** End of connection message *)
      opt_is_admin: bool;                                   	(** admin mode *)
      opt_user: string option;                                  (** current user *)
      opt_addr: string;
      opt_port: int;
      opt_data_port_spec: Network.port_spec;                	(** current data channel port spec *)
      opt_data_secure_mode: Network.secure_mode;             	(** current data channel secure mode *)
      opt_passive : bool;                                   	(** passive mode *)
      opt_pasv_port_min:int;                                	(** minimum port for passive connection *)
      opt_pasv_port_max:int;                                	(** maximum port for passive connection *)
      opt_pasv_port_spec: Network.port_spec option ref;         (** passive mode port spec *)
      opt_pasv_secure_mode: Network.secure_mode option ref;     (** passive mode secure mode *)
      opt_pasv_port_conn: Scheduler.connection_info option ref; (** the passive port connection *)
      opt_local_ip_num:string;                              	(** string of server's IP number *)
      opt_data_conn: Scheduler.connection_info option;      	(** [Some] if currently open *)
      opt_data_blocksize: int;                              	(** blocksize for transfers *)
      opt_data_type: FST.type_code;                             	(** FTP data transfer type *)
      opt_binary: bool;                                         (** transfer binary mode flag *)
      opt_start_position: int;                                  (** marker for REST verb *)
      opt_structure_code: FST.structure_code;                   	(** FTP data structure *)
      opt_transfer_mode: FST.transfer_mode;                     	(** FTP transfer mode *)
      opt_folder: Folder.folder;                            	(** restricted filespace, see folder.mli *)
      opt_default_folder: string;                           	(** starting folder for new connections *)
      opt_rename_string: string option;                         (** from path for RNFR verb *)
      opt_timeout: Time.t;                                  	(** global connection timeout *)
      opt_drop_privilege: bool;
      opt_ssl_cert : string;
      opt_ssl_key : string;
      opt_ssl_pass : string;
      opt_ssl_accept_fun : Ssl.certificate -> bool;
      opt_ssl_always : bool;
      opt_ssl_ca_file : string;
      opt_ssl_ca_path : string;
      opt_ssl_client_ca_file : string;
      opt_ssl_client_cert_path : string;
      opt_dialog: string;
      opt_on_server_close : Scheduler.t -> unit;
      opt_name: string;
    }

let initDir = Filename.concat (Lazy.force File.mlstate_dir) "ftp"
let folder = Folder.empty initDir

let default_options =
  { opt_version = version;
    opt_hello_message = hello;
    opt_goodbye_message = goodbye;
    opt_is_admin = false;
    opt_user = None;
    opt_addr = "0.0.0.0";
    opt_port = 2221;
    opt_data_port_spec = Network.make_port_spec ~protocol Unix.inet_addr_loopback 2220;
    opt_data_secure_mode = Network.Unsecured;
    opt_passive = false;
    opt_pasv_port_min = 49152; (*IANA-registered ephemeral port range*)
    opt_pasv_port_max = 65534;
    opt_pasv_port_spec = ref None;
    opt_pasv_secure_mode = ref None;
    opt_pasv_port_conn = ref None;
    opt_local_ip_num = "127.0.0.1";
    opt_data_conn = None;
    opt_data_blocksize = 4096;
    opt_data_type = FST.A (Some FST.N);
    opt_binary = false;
    opt_start_position = 0;
    opt_structure_code = FST.F;
    opt_transfer_mode = FST.S;
    opt_folder = folder;
    opt_default_folder = initDir;
    opt_rename_string = None;
    opt_timeout = Time.seconds 300;
    opt_drop_privilege = true;
    opt_ssl_cert = "";
    opt_ssl_key = "";
    opt_ssl_pass = "";
    opt_ssl_accept_fun = (fun _ -> true);
    opt_ssl_always = false;
    opt_ssl_ca_file = "";
    opt_ssl_ca_path = "";
    opt_ssl_client_ca_file = "";
    opt_ssl_client_cert_path = "";
    opt_dialog = "default";
    opt_on_server_close = (fun _ -> ());
    opt_name = "ftpServerPort";
  }

let prefixed_opt name opt = [Printf.sprintf "--%s-%s" name opt; Printf.sprintf "--%s" opt]

let spec_args name =
  let p = prefixed_opt name in
  [
    (p"port")@["-p"],
    ServerArg.func ServerArg.int
      (fun o p -> if p > 0xffff then (Logger.error "Bad port number: %d" p; exit 1) else { o with opt_port = p }),
    "<int>", "Sets the port on which the server should run";

    (*p"no-flood-prevention",
    ServerArg.func ServerArg.unit (fun o () -> { o with opt_dos_prevention = false }),
    "", "Disable the built-in protection against Denial-of-Service attacks";*)

    p"no-drop-privilege",
    ServerArg.func ServerArg.unit (fun o () -> { o with opt_drop_privilege = false }),
      "", "Disable the drop of privilege on server start";

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
  if opt.opt_ssl_cert <> "" then
    if opt.opt_ssl_key <> "" then
      Some (SslAS.make_ssl_certificate opt.opt_ssl_cert opt.opt_ssl_key opt.opt_ssl_pass)
    else begin
      Logger.log "Error : ssl-cert option MUST be used with ssl-key option";
      exit 1
    end
  else
    None

let make_ssl_verify opt =
  if opt.opt_ssl_ca_file <> "" || opt.opt_ssl_ca_path <> "" || opt.opt_ssl_client_cert_path <> "" then
    Some (SslAS.make_ssl_verify_params ~client_ca_file:opt.opt_ssl_client_ca_file
      ~accept_fun:opt.opt_ssl_accept_fun ~always:opt.opt_ssl_always
      opt.opt_ssl_ca_file opt.opt_ssl_ca_path opt.opt_ssl_client_cert_path)
  else
    None

let make (_name:string) (opt:options) (_sched:Scheduler.t) : t =
  if opt.opt_drop_privilege then Systools.change_user ();
  let secure_mode = Network.secure_mode_from_params (make_ssl_cert opt) (make_ssl_verify opt) in
  let runtime = { FSC.rt_plim = 128;
                  rt_dialog_name = opt.opt_dialog;
                  rt_on_close = opt.opt_on_server_close;
                  rt_proto = { FSC.rt_name = opt.opt_name;
                               rt_addr = opt.opt_addr;
                               rt_port = opt.opt_port;
                               rt_block_size = 4096;
                               rt_secure_mode = secure_mode;
                               rt_backtrace = true; (* FIXME: put this into options. *)
                               rt_server_write_timeout = Time.seconds 36;
                               rt_payload = ();
                             };
                } in
  let state = { FST.version = opt.opt_version;
                hello_message = opt.opt_hello_message;
                goodbye_message = opt.opt_goodbye_message;
                is_admin = opt.opt_is_admin;
                user = opt.opt_user;
		data_port_spec = opt.opt_data_port_spec;
                data_secure_mode = opt.opt_data_secure_mode;
		passive = opt.opt_passive;
		pasv_port_min = opt.opt_pasv_port_min;
		pasv_port_max = opt.opt_pasv_port_max;
		pasv_port_spec = opt.opt_pasv_port_spec;
                pasv_secure_mode = opt.opt_pasv_secure_mode;
		pasv_port_conn = opt.opt_pasv_port_conn;
		local_ip_num = opt.opt_local_ip_num;
		data_conn = opt.opt_data_conn;
		data_blocksize = opt.opt_data_blocksize;
		data_type = opt.opt_data_type;
                binary = opt.opt_binary;
                start_position = opt.opt_start_position;
		structure_code = opt.opt_structure_code;
		transfer_mode = opt.opt_transfer_mode;
		folder = opt.opt_folder;
                default_folder = opt.opt_default_folder;
                rename_string = opt.opt_rename_string;
		timeout = opt.opt_timeout;
                drop_privilege = opt.opt_drop_privilege;
                ssl_cert = opt.opt_ssl_cert;
                ssl_key = opt.opt_ssl_key;
                ssl_pass = opt.opt_ssl_pass;
	      } in
  {
    FSC.runtime = runtime;
    err_cont = None;
    extra_params = state;
  }

let get_ports (server:t) (sched:Scheduler.t) = FSC.get_ports server sched

let get_description _ftp_server _sched = `FtpServer

let run ftp_server _sched = ftp_server

let close (ftp_server:t) sched = ftp_server.FSC.runtime.FSC.rt_on_close sched

(* End of file: ftpServer.ml *)
