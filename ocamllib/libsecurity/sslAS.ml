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

(* depends *)
module String = BaseString

exception InvalidCertificate
(* exception Want_read *)
(* exception Want_write *)

(* Certificate to provide *)
type ssl_certificate =
    { cert_file : string (* certificate file in PEM format *)
    ; cert_privkey : string (* private RSA key file in PEM format *)
    ; cert_password : string (* password to use if private key protected *)
    ; cert_cafile : string option (* the server CA certificate *)
    ; cert_capath : string option (* the server CA path *) }

(* Certificates verifications rules *)
type ssl_verify_params =
    { cafile : string option (* file of CA certificates in PEM format to use for verifications *)
    ; capath : string option (* directory containing CA certificates in PEM format to use for verifications *)
    ; certpath : string option (* directory containing client certificates in PEM format that are allowed *)
    ; client_ca_file : string option (* list of CAs sent to the client when requesting a client certificate *)
    ; accept_fun : Ssl.certificate -> bool (* function to call when meeting an unknown certificate, in order to be able
                                              to accept in anyway (will not write the certificate into the certs path) *)
    ; always : bool (* always verify the presence of a certificate *) }

type secure_type = ssl_certificate option * ssl_verify_params option

type is_valid_cert = bool

type secure_response =
  | UnsecuredRes
  | SecuredRes of is_valid_cert * (Ssl.certificate option * ssl_verify_params option)

(* Digest name and size used for fingerprint *)
(* let digest_name, digest_size = (\* "SHA1", 40 *\) "SHA256", 64 (\* "SHA512", 128 *\) *)

(* ********** *)

#<Debugvar:SSL_DEBUG>

let dest = [Logger.make_rotating_destination "SslAS"]
let logger =  Logger.make_logger dest 10

let string_of_certificate ce =
  match ce with
    | None -> ""
    | Some c ->
        Printf.sprintf "[cert_file %s] [cert_privkey %s] [cert_password %s] %s [cert_capath %s]"
          c.cert_file
          c.cert_privkey
          c.cert_password
          (match c.cert_cafile with
             | None -> ""
             | Some v -> (Printf.sprintf "[cert_cafile %s]" v))
          (match c.cert_capath with
             | None -> ""
             | Some v -> (Printf.sprintf "[cert_capath %s]" v))

let string_of_param pe =
  match pe with
    | None -> ""
    | Some p ->
        Printf.sprintf "%s %s %s %s [always %s]"
          (match p.cafile with
             | None -> ""
             | Some v -> (Printf.sprintf "[cafile %s]" v))
          (match p.capath with
             | None -> ""
             | Some v -> (Printf.sprintf "[capath %s]" v))
          (match p.certpath with
             | None -> ""
             | Some v -> (Printf.sprintf "[certpath %s]" v))
          (match p.client_ca_file with
             | None -> ""
             | Some v -> (Printf.sprintf "[client_ca_file %s]" v))
          (match p.always with
             | true -> "true"
             | false -> "false")

let log priority color fmt =
  let _log fmt = Logger.log_error ~priority ~color ~logger fmt in
  let _nolog fmt = Format.ifprintf Format.std_formatter fmt in
  #<If>
    _log fmt
  #<Else>
    _nolog fmt
  #<End>

let info fct ?cert ?param fmt =
  log Logger.Info `cyan "[SSL] [%s] %s %s %s" fct (string_of_certificate cert) (string_of_param param) fmt

let warning fct fmt =
  log Logger.Warning `yellow "[SSL] [%s] %s" fct fmt

(* let error fct fmt  = *)
(*   log Logger.Error `red "[SSL] [%s] %s" fct fmt *)

(* ***************** *)

let make_ssl_verify_params ?(client_ca_file="") ?(accept_fun=fun _cert -> false) ?(always=true) cafile capath certpath =
  let p = { cafile = if cafile = "" then None else Some cafile
          ; capath = if capath = "" then None else Some capath
          ; certpath = if certpath = "" then None else Some certpath
          ; client_ca_file = if client_ca_file = "" then None else Some client_ca_file
          ; accept_fun = accept_fun
          ; always = always } in
    info "make_ssl_verify_params" ?param:(Some p) "";
    p

let make_ssl_certificate ?cafile ?capath certfile privkey password =
  let c = { cert_file = certfile
          ; cert_privkey = privkey
          ; cert_password = password
          ; cert_cafile = cafile
          ; cert_capath = capath } in
    info "make_ssl_certificate" ?cert:(Some c) "";
    c

let do_handshake sched conn ?retry ?timeout ssl_s ?err_cont cont =
  let retry = Option.default 4 retry in
  let err_cont = Option.default (fun _ -> ()) err_cont in
  let rec do_retry n () = match n with
    | 0 -> err_cont Scheduler.Timeout
    | n ->
        let no_err =
          try Ssl_ext.do_handshake ssl_s;
            true with
          | Ssl_ext.Handshake_error Ssl.Error_want_read ->
              Scheduler.listen_once sched conn ?timeout ~err_cont (do_retry (n-1));
              false
          | Ssl_ext.Handshake_error Ssl.Error_want_write ->
              Scheduler.connect sched conn ?timeout ~err_cont (do_retry (n-1));
              false
          | (Ssl_ext.Handshake_error e) as ex -> print_endline (Ssl_ext.error_to_string
              e);err_cont ex;false
          | e -> err_cont e;
              false
        in
        if no_err then cont ()
  in
  do_retry retry ()


let renegotiate sched conn ?timeout ?retry ssl_s ?err_cont cont =
  (*
  #<If$minlevel 5>
    debug "Net.renegotiate";
  #<End>;
  *)
  let err_cont = Option.default (fun _ -> ()) err_cont in
  (try
    Ssl_ext.renegotiate ssl_s
  with
    e -> err_cont e);
  let cont' () = do_handshake sched conn ?timeout ?retry ssl_s ~err_cont cont
  in do_handshake sched conn ?timeout ?retry ssl_s ~err_cont cont'

  (*
  #<If$minlevel 5>
    debug "Renegotiated cert:\n%s" (Ssl_ext.certificate_to_string certificate digest_name digest_size);
  #<End>;
  *)


let renegotiate_client sched conn ?timeout ?retry ssl_s ?err_cont cont =
  (*
  #<If$minlevel 5>
    debug "Net.renegotiate";
  #<End>;
  *)
  let err_cont = Option.default (fun _ -> ()) err_cont in
  (try
    Ssl_ext.renegotiate ssl_s
  with
    e -> err_cont e);
  do_handshake sched conn ?timeout ?retry ssl_s ~err_cont cont
  (*
  #<If$minlevel 5>
    debug "Renegotiated cert:\n%s" (Ssl_ext.certificate_to_string certificate digest_name digest_size);
  #<End>;
  *)



(* Initialize the SSL library *)

let ssl_init = lazy (
  (* DO NOT USE BECAUSE BUGS, and we don't use threads anyway...
     Ssl_threads.init () ; *)
  Ssl.init();
  Ssl_ext.init();
)

(* Create an SSL server context *)
let ssl_server_context() =
  Lazy.force ssl_init;
  Ssl.create_context Ssl.SSLv23 Ssl.Server_context

(* Create an SSL client context *)
let ssl_client_context() =
  Lazy.force ssl_init;
  Ssl.create_context Ssl.SSLv23 Ssl.Client_context

(* Digest name and size used for fingerprint *)
let digest_name, digest_size = (* "SHA1", 40 *) "SHA256", 64 (* "SHA512", 128 *)

let compute_fingerprint certificate = Ssl_ext.compute_digest certificate digest_name digest_size

(* Map of valid client certificates
   fingerprint -> subject *)
let certs = ref StringMap.empty

let reload_certs ?(extensions=["pem"]) verify_params =
  info "reload_certs" ?param:(Some verify_params) (Printf.sprintf "[extensions %s]" (List.fold_left (fun a b -> (Printf.sprintf "%s %s" a b)) "" extensions));
  (* Clean the map *)
  certs := StringMap.empty;
  (* Reload every files in certpath *)
  try match verify_params.certpath with
  | Some certpath -> File.iter_dir (
      fun ~name ~path -> (*name_of_addr*) (
        let is_pem = List.fold_left (
          fun acc elt -> acc || String.is_suffix elt name
        ) false extensions in
        if not is_pem then () (* Ignore file that does not have the right extension *)
        else
          begin try
            let certificate = Ssl.read_certificate path in
            let subject = Ssl.get_subject certificate
            and fingerprint = compute_fingerprint certificate in
            certs := StringMap.add fingerprint subject !certs;
            info "reload_certs" (Printf.sprintf "Certificate loaded:\n%s" (Ssl_ext.certificate_to_string certificate digest_name digest_size))
          with
          | Ssl.Certificate_error (* read_certificate *)
          | Not_found (* get_subject *) -> info "reload_certs" path (* Continue even if one file fails *)
          end)
    ) certpath; true
  | _ -> true
  with Unix.Unix_error _ (* File.iter_dir fails *)  -> false

let validate_certificate certificate verify_params =
  (* let hash = Ssl_ext.get_hash certificate in *)
  let has_certpath = Option.is_some verify_params.certpath in
  let fingerprint = compute_fingerprint certificate in
  if not has_certpath || ((* (hash = "" || fingerprint = hash) && *) StringMap.mem fingerprint !certs) then (
    info "validate_certificate" (Printf.sprintf "Valid certificate received:\n%s" (Ssl_ext.certificate_to_string certificate digest_name digest_size));
    true
  ) else if verify_params.accept_fun certificate then (
    info "validate_certificate" (Printf.sprintf "Certificate accepted:\n%s" (Ssl_ext.certificate_to_string certificate digest_name digest_size));
    true
  ) else (
    info "validate_certificate" (Printf.sprintf "Invalid certificate received:\n%s" (Ssl_ext.certificate_to_string certificate digest_name digest_size));
    false
  )

(* == Public functions == *)

let get_listen_callback sched (server_params, client_params) server_fun =
  info "get_listen_callback" "";
  let server_params =
    match server_params with
      | Some sp -> sp
      | None -> info "get_listen_callback" "Ssl listening : no server parameters provided"; assert false;
  in
  let has_cp, has_ca, has_client_ca_file = match client_params with
    | Some cp -> true, (Option.is_some cp.cafile || Option.is_some cp.capath), Option.is_some cp.client_ca_file
    | _ -> false, false, false in
  let has_server_ca = Option.is_some server_params.cert_cafile || Option.is_some server_params.cert_capath in
  info "glc" "ctx...";
  let ctx = ssl_server_context() in
  info "glc" "ctx ok";
  if server_params.cert_password <> "" then
    Ssl.set_password_callback ctx (fun _ -> server_params.cert_password);
  begin try
          info "glc" "use_certificate";
          Ssl.use_certificate ctx server_params.cert_file server_params.cert_privkey;
          info "glc" "load_verify";
          Ssl.load_verify_locations ctx (server_params.cert_file) "";
          if has_server_ca then (
            info "glc" "has_server";
            Ssl.load_verify_locations ctx (Option.default "" server_params.cert_cafile) (Option.default "" server_params.cert_capath);
          );
          info "glc" "set_session";
          Ssl_ext.set_session_id_context ctx; (* Must be set, because bugs with firefox otherwise, don't know why... *)
          if has_cp then
            begin
              info "glc" "has cp, set_ctx_opt";
              let _ = Ssl_ext.set_ctx_options ctx in
              let client_params = Option.get client_params in
              if client_params.always then begin
                info "glc" "always, verify";
                Ssl.set_verify ctx [Ssl.Verify_peer; Ssl.Verify_fail_if_no_peer_cert] (
                  if has_ca then Some Ssl.client_verify_callback else Some Ssl_ext.no_client_verify_callback
                )
              end;
              if has_ca then begin
                info "glc" "has_ca, load";
                (* http://www.openssl.org/docs/ssl/SSL_CTX_load_verify_locations.html *)
                Ssl.load_verify_locations ctx (Option.default "" client_params.cafile) (Option.default "" client_params.capath);
              end;
              if has_client_ca_file then (
                info "glc" "has_client, set";
                (* http://www.openssl.org/docs/ssl/SSL_load_client_CA_file.html *)
                Ssl.set_client_CA_list_from_file ctx (Option.default "" client_params.client_ca_file)
              );
            end
    with
      | Ssl.Private_key_error as e ->
        warning "get_listen_callback" (Printf.sprintf "Error while trying to read private key file %S.\n" server_params.cert_privkey);
        raise e
    (*ServerLib.do_*)(* exit 1 *)
      | Ssl.Certificate_error as e ->
        warning "get_listen_callback" (Printf.sprintf "Error while trying to read ssl certificate %S.\n" server_params.cert_file);
        raise e
    (*ServerLib.do_*)(* exit 1 *)
  end;

  if has_cp then (
    let client_params = Option.get client_params in
    (* Load client certificates into memory in order to check client certificates validity *)
    begin match client_params.certpath with
      | Some _path -> info "glc" "reload_certs..."; ignore (reload_certs client_params)
      | _ -> () end;
  );

    let f (conn: Scheduler.connection_info) =
      let fd = Scheduler.get_connection_fd conn in
      let ssl_s = Ssl.embed_socket fd ctx in
      let rec continuation () =
        try
          info "glc" "accepting...";
          Ssl.accept ssl_s;
          info "glc" "accepting: OK";
          let valid_cert, cert =
            if has_cp then try
              let client_params = Option.get client_params in
              if client_params.always then (
                info "glc" "always, get_certificate...";
                let certificate = Ssl.get_certificate ssl_s in
                info "glc" "ok get_certificate. validating...";
                validate_certificate certificate client_params, Some certificate
              )
              else true, None
            with Ssl.Certificate_error -> false, None
            else true, None
          in
          let sconn = Scheduler.get_connection_secured_from_normal conn ssl_s in
          server_fun (SecuredRes (valid_cert, (cert, client_params))) sconn
        with Ssl.Accept_error ssl_error ->
          match ssl_error with
          | Ssl.Error_want_read -> Scheduler.listen_once sched conn continuation
          | Ssl.Error_want_write -> Scheduler.connect sched conn continuation
          | _ ->
              warning "glc" (Printf.sprintf "Ssl.Error_%s : %s"
                (Ssl_ext.error_to_string ssl_error) (Ssl.get_error_string()));
              Scheduler.remove_connection sched conn
      in
      info "glc" "listening...";
      Scheduler.listen_once sched conn continuation
    in
    f

(* Private function *)
let verify_certificate certificate verify_params =
  info "gss" "get_cert";
  (* Check the server certificate validity, or accept it *)
  if Option.is_some verify_params.certpath then ignore (reload_certs verify_params);
  let valid_cert = validate_certificate certificate verify_params in
  if not valid_cert then raise InvalidCertificate;
  ()

let get_err_cont sched conn err_cont =
  let backtrace = Printexc.get_backtrace() in
  match err_cont with
  | None -> (fun e ->
               Scheduler.remove_connection sched conn;
               match e with
               | Ssl.Accept_error ssl_error ->
                   warning "SslAS" (Printf.sprintf "Ssl_ssl_error'%s'\n%s" (Ssl_ext.error_to_string  ssl_error) backtrace)
               | e -> warning "SslAS" (Printf.sprintf "%s\n%s" (Printexc.to_string e)  backtrace)
            )
  | Some f -> f


let connect sched conn (client_certificate, verify_cert) ?err_cont cont =
  let ctx = ssl_client_context() in
    (* Provide this client certificate if asked *)
  let err_cont = get_err_cont sched conn err_cont in
  begin match client_certificate with
    | Some params ->
      if params.cert_password <> "" then (
        info "gss" "set_pass_callb";
        Ssl.set_password_callback ctx (fun _ -> params.cert_password)
      );
      begin try
        info "gss" "use_certificate";
        Ssl.use_certificate ctx params.cert_file params.cert_privkey
        with
          | Ssl.Private_key_error as e ->
            warning "get_secure_socket" (Printf.sprintf "SslAS.client_connect: Error while trying to read private key file %s.\n" params.cert_privkey);
            err_cont e
        (*ServerLib.do_*)(* exit 1 *)
          | Ssl.Certificate_error as e ->
            warning "get_secure_socket" (Printf.sprintf "SslAS.client_connect: Error while trying to read ssl certificate %s.\n" params.cert_file);
            err_cont e
        (*ServerLib.do_*)(* exit 1 *)
      end;
    | _ -> () end;
    (* let _ = Ssl_ext.set_ctx_options ctx in *)
  info "gss" "open_conn";
  let ssl_sock = Ssl.embed_socket (Scheduler.get_connection_fd conn) ctx in
  let ssl_conn = Scheduler.get_connection_secured_from_normal conn ssl_sock in
  let cert_cont () =
    begin
      let certificate = Ssl.get_certificate ssl_sock in
      match verify_cert with
      | Some vp -> verify_certificate certificate vp
      | None -> ()
    end;
    cont ssl_conn
  in
  (* We don't use Ssl.open_connection_with_context because we are *)
  (* working with non blocking socket *)
  let rec connect_cont () =
    try
      Ssl.connect ssl_sock;
      cert_cont ()
    with
      (* Normal message on non blocking mode. *)
      (* We use Epoll to be warned when we can read or wirte *)
    | Ssl.Connection_error Ssl.Error_want_read -> info "gss" "want_read"; Scheduler.listen_once sched ssl_conn connect_cont
    | Ssl.Connection_error Ssl.Error_want_write -> info "gss" "want_write"; Scheduler.connect sched ssl_conn connect_cont
    | (Ssl.Connection_error e) as exn -> (Logger.error "%s" (Ssl_ext.error_to_string e);
                                          err_cont exn)
    | exn -> err_cont exn
  in
  connect_cont ()


let get_valid_certificate sched conn ?timeout ?retry ssl_s cp ?err_cont cont =
  let has_ca = Option.is_some cp.cafile || Option.is_some cp.capath in
  let validate_cert _ =
    let certif = Ssl.get_certificate ssl_s in
    let is_valid = validate_certificate certif cp
    in cont is_valid in
  match
    try
      info "gvc" "get_cert";
      let certif = Ssl.get_certificate ssl_s in
      Some (validate_certificate certif cp)
    with Ssl.Certificate_error ->
      info "gvc" "set_verify";
      let cb = if has_ca then Some Ssl.client_verify_callback else Some Ssl_ext.no_client_verify_callback
      in Ssl_ext.set_verify ssl_s [Ssl.Verify_peer;Ssl.Verify_fail_if_no_peer_cert] cb;
      renegotiate sched conn ?timeout ?retry ssl_s ?err_cont validate_cert;None
  with
  | Some b -> cont b
  | None -> ()
