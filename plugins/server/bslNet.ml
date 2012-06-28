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
module List = BaseList
module HSCp = HttpServerCore_parse

module BslNativeLib = OpabslgenMLRuntime.BslNativeLib
let caml_list_to_opa_list = BslAppSrcCode.caml_list_to_opa_list
(* The opa scheduler *)
let default_scheduler = BslScheduler.opa

(** TODO - plugins dependencies *)
##property[mli]
##extern-type time_t = int
##extern-type caml_list('a) = 'a list
##extern-type endpoint = Hlnet.endpoint
##extern-type llarray('a) = Obj.t array
##property[endmli]

##opa-type tuple_2('a, 'b)
##opa-type tuple_3('a, 'b, 'c)
##opa-type ThreadContext.t
##opa-type ThreadContext.client

let opa_tuple_2 t =
  wrap_opa_tuple_2 (BslNativeLib.unwrap_opa_tuple_2 (BslNativeLib.opa_tuple_2 t))
let opa_tuple_3 t =
  wrap_opa_tuple_3 (BslNativeLib.unwrap_opa_tuple_3 (BslNativeLib.opa_tuple_3 t))
let opa_list_to_ocaml_list f l =
  BslNativeLib.opa_list_to_ocaml_list f
    (BslNativeLib.wrap_opa_list (BslAppSrcCode.unwrap_opa_list l))
(** *****************************)

##extern-type web_server_status = Requestdef.status
##extern-type WebInfo.private.native_request = HttpServerTypes.request
##extern-type WebInfo.private.native_post_body_list = HttpServerTypes.post_body list
##extern-type WebInfo.private.native_response = HttpServerTypes.response
##extern-type WebInfo.private.native = HttpServerTypes.web_info
##extern-type WebInfo.private.native_connection = Scheduler.connection_info
##extern-type buffer = Buffer.t
##extern-type HttpRequest.multipart = HttpServerTypes.post_body list
##extern-type HttpRequest.payload = HttpServerCore.payload
##extern-type HttpRequest.msg_list = HttpServerCore_parse.msg list
##extern-type HttpRequest.request = HttpServerTypes.request
##extern-type remote_logs = HttpServerTypes.remote_logs option
##opa-type HttpRequest.part


##extern-type SSL.private_key = SslAS.ssl_certificate
##extern-type SSL.policy      = SslAS.ssl_verify_params
##extern-type SSL.certificate = Ssl.certificate
##extern-type SSL.secure_type = SslAS.secure_type

##extern-type WebInfo.private.native_http_header = HSCp.msg


##module convertHeader

  ##register set_cookie : string -> WebInfo.private.native_http_header
  let set_cookie value = HSCp.Set_Cookie value

  ##register last_modified : time_t -> WebInfo.private.native_http_header
  let last_modified date =
    let date = Time.milliseconds date in
      HSCp.Last_Modified (Date.rfc1123 (Time.gmtime date))

  ##register cache_control : string -> WebInfo.private.native_http_header
  let cache_control s = HSCp.Cache_Control s

  ##register pragma : string -> WebInfo.private.native_http_header
  let pragma s = HSCp.Pragma s

  ##register location : string -> WebInfo.private.native_http_header
  let location s = HSCp.Location s

  ##register cdisp_attachment : string -> WebInfo.private.native_http_header
  let cdisp_attachment s = HSCp.Content_Disposition ("attachment", ["filename="^s])

  ##register expires_at : option(time_t) -> WebInfo.private.native_http_header
  let expires_at t =
    let expires =
      match t with
      | None -> Time.infinity
      | Some x -> Time.milliseconds x
    in
    let time = Time.now () in (* TODO: HttpServerCommon use a
    request header here, we should probably do the same *)
    let time_now = Time.gmtime time in
    let exp_time =
      if Time.is_infinite expires then
        { time_now with Unix.tm_year = time_now.Unix.tm_year + 1 }
      else if Time.is_positive expires then
        Time.gmtime (Time.add time expires)
      else time_now
    in HSCp.Expires (Date.rfc1123 exp_time)

  ##register custom : string, string -> WebInfo.private.native_http_header
  let custom name value = HSCp.Custom (name, value)

##endmodule

(** Provides functions from OPA HTTP server, manipulating HTTP
    request, make HTTP response, etc.*)
##module http_server

    (** {6 Unconstruct/Reconstruct weblib info} *)

  (** Get [WebInfo.private.native_request] from a [WebInfo.private.native]. *)
  ##register web_info_request : WebInfo.private.native -> WebInfo.private.native_request
  let web_info_request x = x.HttpServerTypes.request

  (** Get weblib continuation ([WebInfo.private.native_response -> void]) from a
      [WebInfo.private.native]. *)
  ##register web_info_cont : WebInfo.private.native -> (WebInfo.private.native_response -> void)
  let web_info_cont x = x.HttpServerTypes.cont

  (** Get [WebInfo.private.native_connection] from a [WebInfo.private.native]. *)
  ##register web_info_conn : WebInfo.private.native -> WebInfo.private.native_connection
  let web_info_conn x = x.HttpServerTypes.connection

  (** Reconstruct an [WebInfo.private.native] from here three component (see
      above). *)
  ##register web_info_reconstruct : \
    (WebInfo.private.native_response -> void), \
    WebInfo.private.native_request, \
    WebInfo.private.native_connection -> \
    WebInfo.private.native
  let web_info_reconstruct f r c = {
    HttpServerTypes.cont        = f;
    HttpServerTypes.request     = r;
    HttpServerTypes.connection  = c;
    HttpServerTypes.certificate = None;
  }




  (** {6 Weblib request} *)


  ##register is_apple_mobile_webapp \ `HttpServer.is_apple_mobile_webapp` : WebInfo.private.native_request -> bool

  ##register is_apple_mobile \ `HttpServer.is_apple_mobile` : WebInfo.private.native_request -> bool

  ##register get_user_agent \ `HttpServer.get_user_agent` : WebInfo.private.native_request -> string

  ##register get_server_url \ `HttpServer.get_server_url` : WebInfo.private.native_request -> string

  ##register get_uri \ `HttpServer.get_uri` : WebInfo.private.native_request -> string

  ##register get_method \ `HttpServer.get_method` : WebInfo.private.native_request -> string

  ##register is_secured : WebInfo.private.native_request -> bool
  let is_secured req =
    req.HttpServerTypes.handle_request.HttpServerTypes.hr_is_secure

  ##register get_header_names: WebInfo.private.native_request -> opa[list(string)]
  let get_header_names r = caml_list_to_opa_list (fun x -> x) (HttpServer.get_header_names r)

  ##register get_header_values \ `HttpServer.get_header_by_name` : WebInfo.private.native_request -> (string -> option(string))

  (** {6 Weblib connection} *)

  ##register ip_of_web_info : WebInfo.private.native_connection -> string
  let ip_of_web_info c = Unix.string_of_inet_addr (Scheduler.get_connection_inet_addr c)

  ##register check_connection : WebInfo.private.native_connection -> bool
  let check_connection c = Scheduler.check_connection default_scheduler c




  (** {6 Make weblib response} *)
  ##register [cps-bypass] make_response : \
      option(time_t), \
      WebInfo.private.native_request, \
      web_server_status, \
      caml_list(WebInfo.private.native_http_header), \
      string, \
      string, \
      continuation(WebInfo.private.native_response) -> void
  let make_response ms req stat headers s1 s2 k =
    let modified_since = Option.map Time.milliseconds ms in
    HttpServer.make_response_with_headers ~modified_since ~req headers stat s1
      (Http_common.Result s2) (function r -> QmlCpsServerLib.return k r)

  ##register [cps-bypass] make_response_modified_since : \
      option(time_t), \
      WebInfo.private.native_request, \
      web_server_status, \
      string, \
      string, \
      continuation(WebInfo.private.native_response) -> void
  let make_response_modified_since modified_since req stat s1 s2 k =
    let ms = Option.map Time.milliseconds modified_since in
    HttpServer.make_response ~modified_since:ms ~expires:Time.zero ~req stat s1
      (Http_common.Result s2) (function r -> QmlCpsServerLib.return k r)

  ##register [cps-bypass] make_response_expires_at : \
      option(time_t), \
      option(time_t), \
      WebInfo.private.native_request, \
      web_server_status, \
      string, \
      string, \
      continuation(WebInfo.private.native_response) -> void
  let make_response_expires_at expires_at modified_since req stat s1 s2 k =
    let expires =
      match expires_at with
      | None -> Time.infinity
      | Some x -> Time.milliseconds x
    in
    let modified_since = Option.map Time.milliseconds modified_since in
    HttpServer.make_response ~expires ~modified_since ~req stat s1
      (Http_common.Result s2) (function r -> QmlCpsServerLib.return k r)

  ##register [cps-bypass] make_response_req : time_t, WebInfo.private.native_request, web_server_status, string, string, \
    continuation(WebInfo.private.native_response) -> void
  let make_response_req expires r stat s1 s2 k =
    HttpServer.make_response_req (Time.milliseconds expires) r stat s1 (Http_common.Result s2) (function r -> QmlCpsServerLib.return k r)

  ##register [cps-bypass] make_response_req_loc : \
      time_t, string, WebInfo.private.native_request, web_server_status, string, string, \
        continuation(WebInfo.private.native_response) -> void
  let make_response_req_loc expires url r stat s1 s2 k =
    HttpServer.make_response_req_loc (Time.milliseconds expires) url r stat s1 (Http_common.Result s2) (function r -> QmlCpsServerLib.return k r)

  ##register [cps-bypass] direct_expires : WebInfo.private.native_request, string, string, continuation(WebInfo.private.native_response) -> void
  let direct_expires wr s1 s2 k =
    HttpServer.direct ~expires:Time.zero wr s1 (Http_common.Result s2) (function r -> QmlCpsServerLib.return k r)

  ##register [cps-bypass] direct : WebInfo.private.native_request, string, string, continuation(WebInfo.private.native_response) -> void
  let direct wr s1 s2 k = HttpServer.direct wr s1 (Http_common.Result s2) (function r -> QmlCpsServerLib.return k r)




  (** {6 Header list - Never used in OPA ?  Used in social_network but doesn't call this...} *)

  ##extern-type header_list = (string * (string * (string * string) list) list) list
  ##extern-type msg_list = HttpServerCore_parse.msg list

  (** {6 Multipart management} *)

  (** Some helpers function for build opa structure. *)
  (* TODO : What about fold_headers...? Needs closure projection...*)
  let fname = ServerLib.static_field_of_name "name"
  let fvalue = ServerLib.static_field_of_name "value"
  let ffilename = ServerLib.static_field_of_name "filename"
  let fcontent = ServerLib.static_field_of_name "content"
  let fpartial = ServerLib.static_field_of_name "partial"
  let build_file_part name filename content =
    let rc = ServerLib.empty_record_constructor in
    let rc = ServerLib.add_field rc fname name in
    let rc = ServerLib.add_field rc ffilename filename in
    let rc = ServerLib.add_field rc fcontent content in
    wrap_opa_httprequest_part (ServerLib.make_record rc)
  let build_value_part name value =
    let rc = ServerLib.empty_record_constructor in
    let rc = ServerLib.add_field rc fname name in
    let rc = ServerLib.add_field rc fvalue value in
    wrap_opa_httprequest_part (ServerLib.make_record rc)

  let split_disposition =
    let rexp = Str.regexp " *\\([a-z]+\\)=\\\"\\(.*\\)\\\" *" in
    fun s ->
      if Str.string_match rexp s 0 then
        Str.matched_group 1 s, Str.matched_group 2 s
      else invalid_arg (Printf.sprintf "Unexpected disposition : %s" s)

  ##register get_multipart : WebInfo.private.native_request -> option(HttpRequest.multipart)
  let get_multipart r =
    match HttpServer.is_multipart r with
    | true -> Some (HttpServer.get_multipart_content r)
    | false -> None

  (* TODO - Make as cps-bypass when it will be really asynchronous *)
  (* BEWARE : SegFault whirh non-cps and/or no closure. *)
  ##register fold_multipart : HttpRequest.multipart, 'acc, (HttpRequest.part, ('a, (string, string, 'a -> 'a) -> 'a), 'acc -> 'acc) -> 'acc
  let fold_multipart multipart acc folder =
    List.fold_left
      (fun acc (headers, content) ->
         let fold_headers acc folder_h =
           List.fold_left
             (fun acc x -> match x with
              | HttpServerCore_parse.Content_Disposition _ -> acc
              | _ ->
                  let name = HttpServerCore.get_msg_name x in
                  folder_h name (HttpServer.get_header_string_value x name) acc
             ) acc headers in
         let r =
           List.find_map
             (function
                | HttpServerCore_parse.Content_Disposition (_, disp) ->
                    let disp = List.map split_disposition disp in
                    Some (snd (List.find (fun x -> fst x = "name") disp),
                          Option.map snd (List.find_opt (fun x -> fst x = "filename") disp))
                | _ -> None)
             headers in
         let part = match Option.get r with
         | name, None ->
             build_value_part name (ServerLib.wrap_string (Rcontent.get_content content))
         | name, Some filename ->
             (* Take care of cps *)
             let content_fun =
               (fun k ->
                  let r = ServerLib.empty_record_constructor in
                  let r = ServerLib.add_field r fcontent
                    (ServerLib.wrap_string (Rcontent.get_content content)) in
                  let r = ServerLib.make_record r in
                  QmlCpsServerLib.return k r) in
             (* Take care of closure *)
             let content_fun =
               QmlClosureRuntime.create_no_ident content_fun 1 in
             build_file_part name filename content_fun in
         folder part fold_headers acc)
      acc multipart



  (** {6 Initialize the opa server} *)

  ##register set_cookie_expiry_callback : (string, string -> void) -> void
  let set_cookie_expiry_callback callback = CookieLong.cookie_expiration_callback := callback

  (*##register init_server : string, int, option(string), option(string), option(string), (WebInfo.private.native -> void), (int, int, bool, string, string, string -> void) -> unit*)
  ##register init_server : \
      string, \
      int, \
      option(string), \
      option(string), \
      option(string), \
      SSL.secure_type, \
      (WebInfo.private.native -> void), \
      (string, HttpRequest.msg_list, int -> bool) ->\
      void
  let init_server name port certfileo privkeyo passwdo secure_type dispatcher ontransfer =
    let sop = function | Some s -> s | None -> "" in
    let dialog_name = Printf.sprintf "%s-dialog" name in
    let http_server_name = Printf.sprintf "%s" name in
    let dialog _ _sched = dispatcher in
    let callback (name,hdrs) i _buf = ontransfer name hdrs i (*buf*) in
    let ssl_certificate,ssl_verify_params=secure_type in
    Runtime.add_httpDialog dialog_name (HttpDialog.options_with_dialog dialog);
    Runtime.add_httpServer http_server_name
      {HttpServer.default_options with
         HttpServer.ssl_cert = sop certfileo;
         ssl_key = sop privkeyo;
         ssl_pass = sop passwdo;
         dialog = dialog_name;
         port = port;
         callback = Some callback;
         ssl_certificate = ssl_certificate;
         ssl_verify_params = ssl_verify_params;
      }

  (*##register init_server_cps : string, int, option(string), option(string), option(string), continuation(WebInfo.private.native), (int, int, bool, string, string, string -> void) -> unit*)
  ##register init_server_cps : \
      string, \
      int, \
      option(string), \
      option(string), \
      option(string), \
      SSL.secure_type, \
      continuation(WebInfo.private.native), \
      (string, HttpRequest.msg_list, int -> bool) -> \
      void
  let init_server_cps name port certfileo privkeyo passwdo secure_type dispatcher ontransfer =
    init_server name port certfileo privkeyo passwdo secure_type
      (fun winfo -> QmlCpsServerLib.return dispatcher winfo)
      ontransfer




  (** {6 Divers} *)

  ##register async_nb_file_descriptor :  -> int
  let async_nb_file_descriptor _ = Scheduler.nb_of_connection default_scheduler

  ##register get_port : option(string) -> int
  let get_port opt_string =
    try (HttpServer.get_options ?name:opt_string ()).HttpServer.port
    with Not_found -> (-1)

  ##register get_addr : option(string) -> string
  let get_addr opt_string =
    try (HttpServer.get_options ?name:opt_string ()).HttpServer.addr
    with Not_found -> "localhost"

  ##register get_addr_to_reach_distant_server : string, int -> string
  let get_addr_to_reach_distant_server addr port =
    let addr = Unix.inet_addr_of_string addr in
    let addr = Unix.ADDR_INET(addr,port) in
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let _ = Unix.connect socket addr in
    match Unix.getsockname socket with
    | Unix.ADDR_INET(addr,_port) -> Unix.string_of_inet_addr(addr)
    | _ -> assert false

  ##register get_remote_logs_params :  -> opa[option(tuple_3(string, int, string))]
  let get_remote_logs_params _ =
    match HttpServer.get_remote_logs_params () with
    | None -> ServerLib.none
    | Some p -> ServerLib.some (opa_tuple_3
            (p.HttpServerTypes.hostname, p.HttpServerTypes.port, p.HttpServerTypes.appkey))

##endmodule

##module ssl
(**
   Construct a SSL certificate + private key

   @param cafile The server CA certificate
   @param capath The server CA path
   @param certfile Complete path to the certificate file, in PEM format
   @param privkey Path to a file containing the private key
   @param password The password to use if private key protected
*)
   ##register make_key: option(string), option(string), string, string, string -> SSL.private_key
  let make_key cafile capath certfile privkey password = SslAS.make_ssl_certificate ?cafile ?capath certfile privkey password

 (**
   Construct a SSL policy, i.e. something that will decide whether
   to accept a third-party certificate

   @param client_ca_file A list of CAs sent to the client when requesting a client certificate
   @param fallback A fallback function, called when a certificate cannot be checked automatically (e.g. to prompt the user to check the certificate manually)
   @param always Always verify the presence of a certificate
   @param cafile A file containing CA certificates in PEM format, used for verification
   @param capath A directory containing CA certificates in PEM format, used for verification
   @param certpath A directory containing client certificates in PEM format
*)
  ##register make_policy: option(string), option(SSL.certificate -> bool), bool, string, string, string -> SSL.policy
  let make_policy client_ca_file fallback always cafile capath certpath =
    SslAS.make_ssl_verify_params ?client_ca_file ?accept_fun:fallback ~always cafile capath certpath

  ##register make_secure_type: option(SSL.private_key), option(SSL.policy) -> SSL.secure_type
  let make_secure_type key policy = (key, policy)
(**
   Consult the issuer of a certificate
*)
  ##register get_issuer \ `Ssl.get_issuer` : SSL.certificate -> string

(**
   Consult the subject of a certificate
*)
  ##register get_subject \ `Ssl.get_subject` : SSL.certificate -> string

##endmodule

(*
  FIXME: this look like to (unused?) opa-types
*)
##extern-type WebClient.success = ServerLib.ty_record
##extern-type WebClient.failure = ServerLib.ty_record

(** Provides function for make HTTP request. *)
##module http_client

  (** {6 Get} *)

  ##register get : string, int, string, bool, string, (string -> void) -> void
  let get machine port path is_secure auth cont =
    Http_client.get default_scheduler machine port path
      (fun (_, str) -> cont str) ~secure:is_secure ~auth:auth

  let field_network = ServerLib.static_field_of_name "network"
  let field_timeout = ServerLib.static_field_of_name "timeout"
  let field_other   = ServerLib.static_field_of_name "other"
  let field_ssl     = ServerLib.static_field_of_name "ssl"


    (**
       @param hostname The domain name
       @param port The port name. In case of doubt, use [80]
       @param path The path on the server
       @param request_kind The kind of the request (e.g. "POST", "GET", "PUT", "DELETE")
       @param data The content of the request -- generally meaningful only for POST.
       @param is_secure If [true], use [https], otherwise use [http]
       @param auth If [Some a], use [a] to provide http authentication
       @param private_key If using [https], the information required to authenticate with the server
       @param policy If using [https], the information required to decide whether the server is honnest
       @param timeout How long the client will wait for a complete server answer. By default, 36 seconds.
       @param custom_agent An optional identification for the web client. If not provided "Opa-webclient/xxxxxxx", where "xxxxxx" is a version number.
       @param more_headers Headers to add to this GET
       @param on_success Call this function in case of success (note that non-200 codes are successes, so you may need to consult the error code)
       @param on_failure Call this function in case of failure
    *)
  ##register [cps-bypass] place_request : \
      string, \
      int, \
      string, \
      string, \
      option(string), \
      bool, \
      option(string), \
      option(SSL.private_key), \
      option(SSL.policy), \
      option(time_t), \
      option(string), \
      opa[list(string)], \
      (string, \
       int, \
       string, \
       opa[list(string)], \
       (string, continuation(opa[option(string)]) -> void), \
       continuation(opa[void]) -> \
       void), \
      (continuation(WebClient.failure)), \
      continuation(opa[void]) -> \
      void
  let place_request hostname port path request_kind data is_secure auth private_key policy timeout custom_agent more_headers cont_success cont_failure cont_void =
    let success (status, (headers: Requestdef.Value.value Requestdef.ResponseHeader.t), content) =
      let mime = match Requestdef.ResponseHeader.get_string `Content_Type headers with
        | None -> "text/plain"
        | Some s -> s
      in
      let get x k =
        match Requestdef.response_header_of_string_safe x with
        | None -> QmlCpsServerLib.return k ServerLib.none
        | Some key -> match Requestdef.ResponseHeader.get_string key headers with
          | None        -> QmlCpsServerLib.return k ServerLib.none
          | Some string -> QmlCpsServerLib.return k (ServerLib.some (ServerLib.wrap_string string))
      and keys  =  caml_list_to_opa_list (fun x -> Requestdef.string_of_response_header x) (Requestdef.ResponseHeader.keys headers)
      in
      cont_success mime status content keys get  (QmlCpsServerLib.ccont_ml cont_void (fun _ -> ()))
    and failure e =
      let opa_e = match e with
       | `Unknown_machine _ ->
           ServerLib.make_simple_record field_network
       | `Timeout ->
           ServerLib.make_simple_record field_timeout
       | `Cannot_parse_response s ->
           let cons = ServerLib.empty_record_constructor in
           let cons = ServerLib.add_field cons field_other (ServerLib.wrap_string ("Cannot parse response: "^s)) in
           ServerLib.make_record cons
      in QmlCpsServerLib.return cont_failure opa_e
    and err_cont e =
      let opa_e =
          match e with
          | e ->
              let cons = ServerLib.empty_record_constructor in
              let cons = ServerLib.add_field cons field_other (ServerLib.wrap_string ("Exception: "^(Printexc.to_string e))) in
              ServerLib.make_record cons
      in QmlCpsServerLib.return cont_failure opa_e
    in
    let more_headers = opa_list_to_ocaml_list (fun h -> h) more_headers in
    let _ = Http_client.place_request default_scheduler ~request_kind ?data ~hostname ~port ~path
      ~secure:is_secure  ?auth ?client_certificate:private_key ?verify_params:policy
      ?timeout:(Option.map Time.milliseconds timeout)
      ?client_name:custom_agent ~more_headers ~err_cont
       ~success
       ~failure
      ()
    in QmlCpsServerLib.return cont_void ServerLib.void

  ##register [cps-bypass] get_cps : string, int, string, bool, string, (string, continuation(opa[void]) -> void), continuation(opa[void]) -> void
  let get_cps b c d e f g k =
    let g s =
      g s (QmlCpsServerLib.ccont_ml k (fun _ -> ())) in
    get b c d e f g;
    QmlCpsServerLib.return k ServerLib.void


  (** {6 Post} *)

  ##register post : string, int, string, bool, string, string, string, (string -> void) -> void
  let post machine port path is_secure auth mime_type data cont =
    Http_client.post default_scheduler machine port path mime_type data (fun (_, str) -> cont str) ~secure:is_secure ~auth:auth

  ##register [cps-bypass] post_cps : \
      string, \
      int, \
      string, \
      bool, \
      string, \
      string, \
      string, \
      (string, continuation(opa[void]) -> void), \
      continuation(opa[void]) -> \
      void
  let post_cps b c d e f g h i k =
    let i s =
      i s (QmlCpsServerLib.ccont_ml k (fun _ -> ())) in
    post b c d e f g h i;
    QmlCpsServerLib.return k ServerLib.void

##endmodule


(** Provides functions related to beblib request. *)
##module requestdef

  (** {6 Weblib status} *)

 (* 1xx *)
  ##register sc_Continue \ `Requestdef.SC_Continue` : web_server_status
  ##register sc_SwitchingProtocols \ `Requestdef.SC_SwitchingProtocols` : web_server_status

 (* 2xx *)
  ##register sc_OK \ `Requestdef.SC_OK` : web_server_status
  ##register sc_Created \ `Requestdef.SC_Created` : web_server_status
  ##register sc_Accepted \ `Requestdef.SC_Accepted` : web_server_status
  ##register sc_Non_AuthoritativeInformation \ `Requestdef.SC_Non_AuthoritativeInformation` : web_server_status
  ##register sc_NoContent \ `Requestdef.SC_NoContent` : web_server_status
  ##register sc_ResetContent \ `Requestdef.SC_ResetContent` : web_server_status
  ##register sc_PartialContent \ `Requestdef.SC_PartialContent` : web_server_status

 (* 3xx *)
  ##register sc_MultipleChoices \ `Requestdef.SC_MultipleChoices` : web_server_status
  ##register sc_MovedPermanently \ `Requestdef.SC_MovedPermanently` : web_server_status
  ##register sc_Found \ `Requestdef.SC_Found` : web_server_status
  ##register sc_SeeOther \ `Requestdef.SC_SeeOther` : web_server_status
  ##register sc_NotModified \ `Requestdef.SC_NotModified` : web_server_status
  ##register sc_UseProxy \ `Requestdef.SC_UseProxy` : web_server_status
  ##register sc_TemporaryRedirect \ `Requestdef.SC_TemporaryRedirect` : web_server_status

 (* 4xx *)
  ##register sc_BadRequest \ `Requestdef.SC_BadRequest` : web_server_status
  ##register sc_Unauthorized \ `Requestdef.SC_Unauthorized` : web_server_status
  ##register sc_PaymentRequired \ `Requestdef.SC_PaymentRequired` : web_server_status
  ##register sc_Forbidden : web_server_status
  let sc_Forbidden = Requestdef.SC_Forbidden None
  ##register sc_NotFound \ `Requestdef.SC_NotFound` : web_server_status
  ##register sc_MethodNotAllowed \ `Requestdef.SC_MethodNotAllowed` : web_server_status
  ##register sc_NotAcceptable \ `Requestdef.SC_NotAcceptable` : web_server_status
  ##register sc_ProxyAuthenticationRequired \ `Requestdef.SC_ProxyAuthenticationRequired` : web_server_status
  ##register sc_RequestTime_out \ `Requestdef.SC_RequestTime_out` : web_server_status
  ##register sc_Conflict \ `Requestdef.SC_Conflict` : web_server_status
  ##register sc_Gone \ `Requestdef.SC_Gone` : web_server_status
  ##register sc_LengthRequired \ `Requestdef.SC_LengthRequired` : web_server_status
  ##register sc_PreconditionFailed \ `Requestdef.SC_PreconditionFailed` : web_server_status
  ##register sc_RequestEntityTooLarge \ `Requestdef.SC_RequestEntityTooLarge` : web_server_status
  ##register sc_Request_URITooLarge \ `Requestdef.SC_Request_URITooLarge` : web_server_status
  ##register sc_UnsupportedMediaType \ `Requestdef.SC_UnsupportedMediaType` : web_server_status
  ##register sc_RequestedRangeNotSatisfiable \ `Requestdef.SC_RequestedRangeNotSatisfiable` : web_server_status
  ##register sc_ExpectationFailed \ `Requestdef.SC_ExpectationFailed` : web_server_status

 (* 5xx *)
  ##register sc_InternalServerError \ `Requestdef.SC_InternalServerError` : web_server_status
  ##register sc_NotImplemented \ `Requestdef.SC_NotImplemented` : web_server_status
  ##register sc_BadGateway \ `Requestdef.SC_BadGateway` : web_server_status
  ##register sc_ServiceUnavailable \ `Requestdef.SC_ServiceUnavailable` : web_server_status
  ##register sc_GatewayTime_out \ `Requestdef.SC_GatewayTime_out` : web_server_status
  ##register sc_HTTPVersionNotSupported \ `Requestdef.SC_HTTPVersionNotSupported` : web_server_status

  ##register status_code \ `Requestdef.status_code` : web_server_status -> int

  ##register reason_phrase \ `Requestdef.reason_phrase` : int -> string


  (** {6 Weblib request} see alse on [HttpServer]*)

  ##register [cps-bypass] get_request_message_body : WebInfo.private.native_request, continuation(string) -> void
  let get_request_message_body req k =
    QmlCpsServerLib.return k (Rcontent.get_content req.HttpServerTypes.request_message_body)

  (** Return User Agent ("unknown" if there is no user agent) *)
  ##register get_request_ua \ `HttpServer.get_user_agent` : WebInfo.private.native_request -> string

  ##register get_request_cookie \ `HttpServer.get_request_cookie` : WebInfo.private.native_request -> option(string)

  ##register get_cookie \ `HttpServer.get_cookie` : WebInfo.private.native_request -> string

  ##register request_with : WebInfo.private.native_request, string, string -> WebInfo.private.native_request
  let request_with request uri body = let open HttpServerTypes in {
    request with
      request_line = { request.request_line with request_uri = uri };
      request_message_body = (Rcontent.ContentString body)
  }

  (* Note : This a big hack see the nodejs comment. *)
  ##register [cps-bypass] request_with_cont : \
  WebInfo.private.native_request, string, string, \
  (string, continuation(opa[void]) -> void), \
  continuation(opa['a]) -> void
  let request_with_cont request uri body cont k =
    let request = request_with request uri body in
    let cont response v =
      match response.HttpServerTypes.body with
      | content -> cont (Rcontent.get_content content) v
    in
    let cont = QmlClosureRuntime.import cont 2 in
    let result = Obj.magic (opa_tuple_2 (request, cont)) in
    QmlCpsServerLib.return k result


##endmodule
