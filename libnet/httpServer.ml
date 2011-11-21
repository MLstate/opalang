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
let (<|) f a = f a
let (|>) a f = f a
let ( @* ) g f x = g(f(x))
module List = Base.List
module String = Base.String

module Rd = Requestdef
module Rc = Rcontent
module HSCp = HttpServerCore_parse
module HST = HttpServerTypes
module HSC = HttpServerCore
module HT = HttpTools
module HSCm = HttpServerCommon
module Mp = Mlstate_platform
module HD = HttpDialog

let sprintf = Printf.sprintf

#<Debugvar:HTTP_DEBUG>

type request = HST.request

let make_status = HSC.make_status

let remote_logs_params = ref None

(* Warning: it only returns the last defined remote-logs server option *)
(* TODO: getter for a specific http server name *)
let get_remote_logs_params() = !remote_logs_params

(* Private tools *)

let get_content_length req = Int64.to_int (Option.default 0L (HSC.get_Content_Length req.HST.request_header))

(* TODO: generate Map from DSL type generation code *)

let remove_header_first h request_header =
  let rec aux = function
    | [] -> []
    | h2::t ->
        if HSC.compare_msg (h,h2)
        then t
        else h2::aux t in
  aux request_header

let remove_header h request_header = List.filter (fun h2 -> not (HSC.compare_msg (h,h2))) request_header

let replace_header h request_header = h::(remove_header h request_header)

let replace_header_first h request_header = h::(remove_header_first h request_header)

let replace_request_header h req = {req with HST.request_header=replace_header h req.HST.request_header}

let string_of_request { HST.request_scheduler=_;
                        HST.request_line = rl; HST.request_header = rh; HST.request_message_body = rmb;
                        HST.request_post_body = _; HST.server_info = _; HST.is_multipart = _; HST.handle_request = _; } =
  (HSC.string_of_msg rl.HST._method)^(List.fold_left (fun s h -> s^(HSC.string_of_msg h)) "" rh)^"\r\n"^(Rc.get_content rmb)

let get_method req =
  match req.HST.request_line.HST._method with
  | HSCp.Opts (_,_) -> "OPTIONS"
  | HSCp.Get (_,_) -> "GET"
  | HSCp.Head (_,_) -> "HEAD"
  | HSCp.Post (_,_) -> "POST"
  | HSCp.Put (_,_) -> "PUT"
  | HSCp.Del (_,_) -> "DELETE"
  | HSCp.Trace (_,_) -> "TRACE"
  | HSCp.Conn (_,_) -> "CONNECT"
  | _ -> "UNKNOWN"

let get_header_names req = List.map HSC.get_msg_name req.HST.request_header

(* FIXME: improve this function *)
let get_header_string_value h name =
  (* FIXME: we assume the length of the name is the same as the length of the header name. *)
  let str = HSC.string_of_msg h in
  let strlen = String.length str in
  let rmv = min (String.length name) strlen in
  let str = String.sub str rmv (strlen-rmv) in
  String.trim (String.remove_suffix_if_possible "\r\n" (String.remove_prefix_if_possible ":" str))

let get_header_by_name_ rh name =
  match List.find_opt (fun h -> (HSC.get_msg_name h) = name) rh with
  | Some h -> Some (get_header_string_value h name)
  | None -> None
let get_header_by_name req = get_header_by_name_ req.HST.request_header

(* BSL-tools *)

let get_server_url req =
  match req.HST.server_info with
  | {HST.server_url=su; HST.server_id=_; HST.server_ip_or_name=_; HST.server_port=_; HST.server_secured=_; } -> su

let get_uri req = req.HST.request_line.HST.request_uri

let is_multipart req = req.HST.is_multipart

let get_multipart_name msglst name =
  let res =
  match List.find_opt (function HSCp.Content_Disposition _ -> true | _ -> false) msglst with
    Some (HSCp.Content_Disposition (s,l)) ->
      let al = HSC.parse_content_disposition (HSCp.Content_Disposition (s,l)) in
      #<If>Logger.debug "get_multipart_name: al=%s"
                   (String.concat ", " (List.map (fun (a,b) -> a^"->"^b) al)); flush stderr#<End>;
      String.strip_quotes (try List.assoc name al with Not_found -> "")
  | _ -> ""
  in #<If>Logger.debug "get_multipart_name(%s): returning '%s'" name res#<End>; res

let get_multipart_type msglst =
  let res =
  match List.find_opt (function HSCp.Content_Type _ -> true | _ -> false) msglst with
    Some (HSCp.Content_Type (s,_)) -> s
  | _ -> "unknown/unknown"
  in #<If>Logger.debug "get_multipart_type: returning '%s'" res#<End>; res

let get_multipart_content req =
  req.HST.request_post_body

let m2 =
  Lazy.lazy_from_fun
    (fun () ->
       let mime_types_file_content =
         try
           let res = "./mime.types" (* File.content ((Lazy.force File.mlstate_dir)^"/.mime.types")  *)in
           Logger.info "Loaded .mime.types file";
           res
         with Unix.Unix_error _ -> "" in
       let (_,(_,m1)) = Mime.parse_mime_file mime_types_file_content in
       let (_,(_,m2)) = Mime.parse_mime_file Mimes.mimetypes in
       StringMap.merge (fun x1 _ -> x1) m1 m2)

let mime_type f =
  let ext =
    match File.extension f with
    | "" -> f (* pour pouvoir directement trouver le type mime de "html" *)
    | e -> e in
  try StringMap.find (String.lowercase (ext)) (Lazy.force m2)
  with Not_found -> "unknown/unknown"

let get_user_agent req = Option.default "unknown" (HSC.get_User_Agent req.HST.request_header)

let get_request_cookie req =
  match req.HST.handle_request.HST.hr_ic with
  | "" -> None
  | ic -> Some ic

(* Returns the ic and ec cookies, contrary to get_request_cookie that just returns ic *)
let get_request_cookies req =
  let ic = match req.HST.handle_request.HST.hr_ic with
  | "" -> None
  | ic -> Some ic
  and ec = match req.HST.handle_request.HST.hr_ec with
  | "" -> None
  | ec -> Some ec
  in (ic, ec)

let get_cookie req =
  let s = req.HST.handle_request.HST.hr_ic in
  #<If>Logger.debug "HttpServer.get_cookie: '%s'" s; flush stderr#<End>;
  s
let get_resource_tracker req =
  let s = req.HST.handle_request.HST.hr_ic in
  let r = Cookie2.get_resource_tracker s in
  r

let is_ie req = String.is_contained "MSIE" <| get_user_agent req
let is_firefox req = String.is_contained "Firefox" <| get_user_agent req
let is_googlebot req = String.is_contained "googlebot" <| String.lowercase (get_user_agent req)
let is_apple_mobile req =
  let ua = get_user_agent req in
  (String.is_contained "Mobile" ua) && (String.is_contained "Apple" ua)
let is_safari_mobile req =
  let ua = get_user_agent req in
  (String.is_contained "Mobile" ua) && (String.is_contained "Safari" ua)
let is_apple_mobile_webapp req =
  let ua = get_user_agent req in
  (String.is_contained "Mobile" ua) && (String.is_contained "Apple" ua) && (not (String.is_contained "Safari" ua))

let make_error_response html code msg headers =
  let sl = HSCp.Sl (HSC.http_version_number, code, msg) in
  let headers = headers@[HSCp.Content_Length (Int64.of_int (String.length html))] in
    { HST.sl = sl ; HST.headers = headers ; HST.body = Rc.ContentString html }

let _not_found () =
  make_error_response "<html><head><title>404 Error</title></head><body>404 - Not Found</body></html>" 404 "Not Found" []

let _unauthorized () =
  make_error_response "<html><head><title>401 Error</title></head><body>401 - Unauthorized</body></html>" 401 "Unauthorized" []

let _not_modified ?(include_date=true) () =
  make_error_response
    (* SEE: RFC 2616 10.3.5 *)
    "" 304 "Not Modified"
    (if include_date
     then [HSCp.Date (Date.rfc1123 (Unix.gmtime (Unix.time())))] (* RFC2616: 10.3.5 - we have a clock *)
     else [])

let limstr ?(extra="") str lim =
  let len = String.length str in
    (String.sub str 0 (min lim len)) ^ (if len > lim then extra else "")

let str_of_result = function
  | Http_common.Result page -> page
  | _ -> "<html><head><title>Error</title></head><body>Error.</body></html>"

let make_response_with_headers ?(modified_since=None) ?(compression_level=6)
    ?(cache_response=true) ?(delcookies=false) ?req
    headers_out status_line _type content cont =
  #<If$minlevel 20>Logger.debug "make_response"#<End>;
  let code = Rd.status_code status_line in
  let reason = Rd.reason_phrase code in
  let sl = HSCp.Sl (HSC.http_version_number, code, reason) in
  let content = str_of_result content in
  #<If>Logger.debug "make_response: content=%s" (limstr ~extra:"..." content 100)#<End>;
  let (sched,hr_opt,uri,headers_in,include_body) =
    match req with
      Some req -> (req.HST.request_scheduler,
                   (Some req.HST.handle_request),
                   req.HST.request_line.HST.request_uri,
                   req.HST.request_header,
                   (match req.HST.request_line.HST._method with HSCp.Head _ -> false | _ -> true))
    | _ -> (Scheduler.default,None,"",[],true) in
  HSCm.process_content_with_headers sched ~modified_since ~compression_level
    ~cache_response ~_delcookies:delcookies ~_type hr_opt uri
    (Rc.ContentString content) headers_in headers_out include_body
    (function processed ->
       let r =
         match processed with
           Some (headers_out,body,len) -> {
             HST.sl = sl;
             HST.headers = HSCp.Content_Length len :: headers_out;
             HST.body = body;
           }
         | None -> _not_modified ()
       in cont r)

let make_response ?(modified_since=None) ?(compression_level=6) ?(cache_response=true) ?(expires=Time.zero) ?(cache=true)
    ?(delcookies=false) ?location ?req status_line _type ?content_dispo content cont =
  #<If$minlevel 20>Logger.debug "make_response"#<End>;
  let code = Rd.status_code status_line in
  let reason = Rd.reason_phrase code in
  let sl = HSCp.Sl (HSC.http_version_number, code, reason) in
  let content = str_of_result content in
  #<If>Logger.debug "make_response: content=%s" (limstr ~extra:"..." content 100)#<End>;
  let (sched,hr_opt,uri,headers_in,include_body) =
    match req with
      Some req -> (req.HST.request_scheduler,
                   (Some req.HST.handle_request),
                   req.HST.request_line.HST.request_uri,
                   req.HST.request_header,
                   (match req.HST.request_line.HST._method with HSCp.Head _ -> false | _ -> true))
    | _ -> (Scheduler.default,None,"",[],true) in
  HSCm.process_content sched ~modified_since ~compression_level ~cache_response ~expires
    ~cache ~_delcookies:delcookies ~_type ?content_dispo
    hr_opt uri (Rc.ContentString content) headers_in include_body (
      function e ->
        let r = match e with
            Some (headers_out,body,len) ->
              let headers_out = match location with
                | Some url -> (HSCp.Location url)::headers_out
                | None -> headers_out in
              { HST.sl = sl; HST.headers = HSCp.Content_Length len::headers_out; HST.body = body }
          | None -> _not_modified ()
        in cont r
    )

let make_response_result ?(modified_since=None) ?(compression_level=6) ?(cache_response=true) ?(expires=Time.zero)
    ?(cache=true) ?(delcookies=false) ?location ?req status _type ?content_dispo content =
  make_response ~modified_since:modified_since ~compression_level:compression_level
    ~cache_response:cache_response ~expires:expires ~cache:cache ~delcookies:delcookies
    ?location ?req status _type ?content_dispo
    (Http_common.Result content)

let make_response_modified_since date req = make_response ~modified_since:(Some date) ~req
let make_response_req expires req = make_response ~expires ~req
let make_response_req_loc expires location req = make_response ~expires ~location ~req

let get_req_type req = req.HST.request_line.HST._method

let move_page time url sc =
  let code = Rd.status_code sc in
  let reason = Rd.reason_phrase code in
  Http_common.Result (sprintf "<html><head><meta http-equiv=\"refresh\" content=\"%d; URL=%s\"></head>%s</html>" (truncate (Time.in_seconds time)) url
    (if Time.is_positive time then sprintf "<body><p>%s %d %s</p></body>" HSC.http_version code reason else ""))

let error_page server_url e =
  let sc = Rd.status_code e in
  let rp = Rd.reason_phrase sc in
  Http_common.Result
    (sprintf
       "<html><head><title>%d %s</title></head><body><h1>Error %d: %s</h1><hr /><p><a href=\"%s\">%s</a></body></p></html>"
       sc rp sc rp server_url HSC.server_name)

let make_error ?req status =
  match req with
  | Some req ->
      let server_url = get_server_url req in
      make_response ~req status "text/html" (error_page server_url status)
  | _ -> make_response status "text/html" (error_page "" status)

let make_moved ?(time=Time.zero) req url =
  make_response ~location:url ~req Rd.SC_MovedPermanently "text/html"
                (move_page time url Rd.SC_MovedPermanently)
let make_redirect ?(time=Time.zero) req url =
  make_response ~location:url ~req Rd.SC_TemporaryRedirect "text/html"
                (move_page time url Rd.SC_TemporaryRedirect)

let direct_pair req (m, c) = make_response ~expires:Time.infinity ~req Rd.SC_OK m (Http_common.Result c)
let direct ?(expires=Time.infinity) req = make_response ~expires ~req Rd.SC_OK

let html ?(compression_level=6) ?(cache_response=true) ?(cache=true) ?(delcookies=false) ?(charset="") req =
  let charset = if charset = "" then "" else "; charset=" ^ charset  in
  let mime = (mime_type ".html") ^ charset in
  make_response_result ~compression_level:compression_level ~cache_response:cache_response
                       ~cache:cache ~delcookies:delcookies ~req Rd.SC_OK mime

let get_fields req =
  let body = Rc.get_content req.HST.request_message_body in
  let s = String.sub body 0 (get_content_length req) in
  try
    let _pos, res = Encodings.http_body_rewrite s in
    res
  with Encodings.HttpBodyRewriteError _ -> []

(* MAIN FUNCTIONS *)

let make_error_response k sched conn uri hr _method html code msg headers =
  let req = { HST.request_scheduler = sched;
              request_line = { HST._method=_method; request_uri=uri; http_version=HSC.http_version_number };
              request_header = [HSCp.Sl (HSC.http_version_number, code, msg)]@headers
                               @[HSCp.Content_Length (Int64.of_int (String.length html))];
              request_message_body = Rc.ContentString html; request_post_body = [];
              server_info = hr.HST.hr_server_info; is_multipart = false; handle_request = hr } in
  { HST.cont=k; request=req; connection=conn; certificate=None }

let not_found k sched conn uri hr _method =
  make_error_response k sched conn uri hr _method
                      "<html><head><title>404 Error</title></head><body>404 - Not Found</body></html>" 404 "Not Found" []

let unauthorized k sched conn uri hr _method =
  make_error_response k sched conn uri hr _method
                      "<html><head><title>401 Error</title></head><body>401 - Unauthorized</body></html>" 401 "Unauthorized" []

let not_modified ?(include_date=true) k sched conn uri hr _method =
  make_error_response
    k sched conn uri hr _method
    (* SEE: RFC 2616 10.3.5 *)
    "" 304 "Not Modified"
    (if include_date
     then [HSCp.Date (Date.rfc1123 (Unix.gmtime (Unix.time())))] (* RFC2616: 10.3.5 - we have a clock *)
     else [])

let pre_headers hr request_type headers =
  #<If:HTTP_NO_COOKIE>
    (hr, headers)
  #<Else>
    (HSCm.cookies2In hr (HSC.request_type_uri request_type), headers)
  #<End>

let post_headers hr request_type headers_in headers_out =
  let connection_opt =
    match (List.find_opt (function HSCp.Connection _ -> true | _ -> false) headers_in) with
    | Some (HSCp.Connection s) -> Some s
    | _ -> None in
  let (header,close) =
    match (HSC.request_type_http_version request_type, connection_opt) with
    | ("1.0",None) -> ([HSCp.Connection "close"],true)
    | ("1.1",None) -> ([HSCp.Connection "Keep-Alive"],false)
    | (_,Some s) when String.is_substring_insensitive "keep-alive" s 0 -> ([HSCp.Connection "Keep-Alive"],false)
    | (_,Some s) when String.is_substring_insensitive "close" s 0 -> ([HSCp.Connection "Close"],true)
    | _ -> ([HSCp.Connection "Keep-Alive"],false) in
  #<If:HTTP_NO_COOKIE>
    (hr,(headers_out@header),close)
  #<Else>
    let cookies = HSCm.cookies2Out hr (HSC.request_type_uri request_type) false headers_in in
    (hr,(headers_out@header@cookies),close)
  #<End>

let handle_special sched _runtime _method hr body_value headers _conn k =
  let include_body = match _method with HSCp.Head _ -> false | _ -> true in
  HSCm.get_body_from_value sched hr body_value headers include_body (
    function res -> match res with
    | Some (ceheader,body,len) ->
        k { HST.sl=HSCp.Sl ("1.0", 200, "OK"); headers=[HSCp.Content_Length len]@ceheader; body=body }
    | None ->
        k (_not_modified ())
  )

let handle_get sched runtime _method hr (uri, headers) conn k =
  #<If>Logger.debug "handle_get: uri=%s" uri#<End>;
  HSCm.check_host headers;
  match uri, runtime.HSC.rt_server.HSC.rt_favicon_ico, runtime.HSC.rt_server.HSC.rt_favicon_gif with
  | ("/favicon.ico", (_,content,_,_), _) when content <> Rc.ContentNone ->
      handle_special sched runtime _method hr runtime.HSC.rt_server.HSC.rt_favicon_ico headers conn k
  | ("/favicon.gif", _, (_,content,_,_)) when content <> Rc.ContentNone ->
      handle_special sched runtime _method hr runtime.HSC.rt_server.HSC.rt_favicon_gif headers conn k
  | _, _, _ ->
      let req = { HST.request_scheduler=sched;
                  HST.request_line = { HST._method=_method; request_uri=uri; http_version=HSC.http_version_number };
                  request_header = headers; request_message_body = Rc.ContentNone; request_post_body = [];
                  server_info = hr.HST.hr_server_info; is_multipart = false; handle_request = hr } in
      (HD.body runtime.HSC.rt_server.HSC.rt_dialog_content sched) { HST.cont=k; request=req; connection=conn; certificate=None }

let handle_simple_post sched runtime _method hr uri headers body conn k =
  #<If>Logger.debug "handle_simple_post: uri=%s" uri#<End>;
  let req = { HST.request_scheduler=sched;
              HST.request_line = { HST._method = _method ; request_uri = uri ; http_version = HSC.http_version_number };
              request_header = headers; request_message_body = body; request_post_body = [];
              server_info = hr.HST.hr_server_info; is_multipart = false; handle_request = hr} in
  (HD.body runtime.HSC.rt_server.HSC.rt_dialog_content sched) { HST.cont=k; request=req; connection=conn; certificate=None }

let handle_multipart_post sched runtime _method hr mpr conn k =
  try
    #<If>Logger.debug "handle_multipart_post: uri=%s" mpr.HST.uri#<End>;
    (* Warning: this debug statement doesn't correctly reconstruct the message body, no boundaries *)
    #<If>let body = String.concat "\n"
      <| List.rev_map (fun (lst, content) ->
                         String.concat "\n" <| (List.map HSC.string_of_msg lst) @ [Rc.get_content content]) mpr.HST.request_body
    in Logger.debug "handle_multipart_post: body=%s" (String.escaped (String.limit 1024 body))#<End>;
    let rl = { HST._method=_method; request_uri=mpr.HST.uri; http_version=HSC.http_version_number } in
    let req = { HST.request_scheduler=sched;
                HST.request_line=rl; request_header=mpr.HST.request_headers; request_message_body=Rc.ContentNone;
                request_post_body=mpr.HST.request_body;
                server_info=hr.HST.hr_server_info; is_multipart=true; handle_request=hr; } in
    let cont = fun wi -> List.iter (fun f -> #<If>Logger.debug "deleting %s" f#<End>; Unix.unlink f) mpr.HST.tmpfiles; k wi in
    (HD.body runtime.HSC.rt_server.HSC.rt_dialog_content sched) { HST.cont=cont; request=req; connection=conn; certificate=None }
  with exn ->
    #<If>Logger.debug "handle_multipart_post: exn=%s" (Printexc.to_string exn)#<End>;
    List.iter (fun f -> #<If>Logger.debug "deleting %s" f#<End>; Unix.unlink f) mpr.HST.tmpfiles;
    raise exn

let handle_post sched runtime _method hr = function
  | HST.Simple (uri, headers, body) -> handle_simple_post sched runtime _method hr uri headers body
  | HST.Multipart record -> handle_multipart_post sched runtime _method hr record

let handle_request ?(cachetype="public") ?(is_secure=false) server_info tm lc =
  { HST.hr_delcookies = false;
    hr_cachetype = cachetype;
    hr_server_info = server_info;
    hr_is_secure = is_secure;
    hr_timestamp = tm;
    hr_timestamp_tm = lc;
    hr_inet_addr_str = "";
    hr_user_agent = "";
    hr_referer = "";
    hr_ec = "";
    hr_ic = "";
    hr_dt2 = Time.zero;
  }

(* Runtime layer *)

let name = "httpServer"
let version = "1.0"

type t = HSC.t

type options =
    { ssl_cert : string;
      ssl_key : string;
      ssl_pass : string;
      ssl_accept_fun : Ssl.certificate -> bool;
      ssl_always : bool;
      ssl_ca_file : string;
      ssl_ca_path : string;
      ssl_client_ca_file : string;
      ssl_client_cert_path : string;
      ssl_certificate : SslAS.ssl_certificate option;
      ssl_verify_params : SslAS.ssl_verify_params option;
      pid_file : string option;
      dialog : string;
      request_size_max : int;
      print_log_info : bool;
      print_server_info : bool;
      timeout : float;
      long_cookies : bool;
      cookie_expire_time_short : Time.t;
      cookie_expire_time_long : Time.t;
      dt1 : Time.t;
      dt2 : Time.t;
      max_external_cookies : int;
      rotate_cookies : bool;
      cachetype : string;
      server_send_buffer_size: int;
      cookie_gc_period: int;
      cookie_pool_size_min: int;
      cookie_pool_size_max: int;
      cookie_timer_interval: int;
      cookie_rate_max: float;
      cookie_period_max: int;
      cookie_rate_ultimate: float;
      cookie_period_ultimate: int;
      cookies_filename: string;
      server_wait_for_request_timeout: Time.t;
      server_wait_for_request_initial_timeout: Time.t;
      server_write_timeout: Time.t;
      maximum_number_of_connections: int;
      maximum_content_length: int;
      maximum_number_of_headers: int;
      remote_logs: HST.remote_logs option;
      favicon_ico: HSC.body_value;
      favicon_gif: HSC.body_value;
      backtrace: bool;
      name : string;
      addr : string;
      port : int;
      block_size : int;
      allowed_hosts : string list;
      dos_prevention : bool;
      on_server_run : options -> Scheduler.t -> unit;
      on_server_close : Scheduler.t -> unit;
      get : Scheduler.t -> HSC.runtime -> HSCp.msg -> HST.handle_request -> HST.get
            -> Scheduler.connection_info -> (HST.response -> unit) -> unit;
      post : Scheduler.t -> HSC.runtime -> HSCp.msg -> HST.handle_request -> HST.post
             -> Scheduler.connection_info -> (HST.response -> unit) -> unit;
      pre_headers : HST.handle_request -> HSCp.msg -> HST.header list -> (HST.handle_request * HST.header list);
      post_headers : HST.handle_request -> HSCp.msg -> HST.header list -> HST.header list
                     -> (HST.handle_request * HST.header list * bool);
      callback : (HSC.payload -> int -> Buffer.t -> bool) option;
    }

let null_callback (_str,_hdrs) _i _buf = (*Logger.debug "null_callback(%s): i=%d" str i;*) true

let null_body_value = (("<no file>", Rc.ContentNone, Time.zero, "unknown/unknown"):HSC.body_value)

let body_value_from_file ?(log=false) file : HSC.body_value =
  try
    let stat = Unix.stat file in
    let content =
      if stat.Unix.st_size > (1024*1024)
      then Rc.ContentFile (file,None,None,Some stat,false)
      else Rc.ContentString (File.content file) in
    let modified_time = Time.of_unix_time stat.Unix.st_mtime in
    let mime_type_string = mime_type file in
    if log then Logger.info "Loaded file: %s" file;
    file, content, modified_time, mime_type_string
  with Unix.Unix_error _ -> null_body_value

let body_value_from_home ?log file =
  try body_value_from_file ?log (Filename.concat (Lazy.force File.mlstate_dir) file)
  with Not_found -> null_body_value

let bv_file (file,_,_,_) = file

let default_options =
  { ssl_cert = "";
    ssl_key = "";
    ssl_pass = "";
    ssl_accept_fun = (fun _ -> true);
    ssl_always = false;
    ssl_ca_file = "";
    ssl_ca_path = "";
    ssl_client_ca_file = "";
    ssl_client_cert_path = "";
    ssl_certificate = None;
    ssl_verify_params = None;
    pid_file = None;
    dialog = "default";
    request_size_max = 10*1024*1024;
    print_log_info = true;
    print_server_info = true;
    timeout = 36.;
    long_cookies = true;
    cookie_expire_time_short = Time.seconds 5;
    cookie_expire_time_long = Time.seconds 50;
    dt1 = Time.days 10;
    dt2 = Time.infinity;
    max_external_cookies = 10;
    rotate_cookies = true;
    cachetype = "public";
    server_send_buffer_size = 1024;
    cookie_gc_period = 100;
    cookie_pool_size_min = 100;
    cookie_pool_size_max = 10000;
    cookie_timer_interval = 1;
    cookie_rate_max = 5.0;
    cookie_period_max = 5;
    cookie_rate_ultimate = 10.0;
    cookie_period_ultimate = 100;
    cookies_filename = ""(*(Lazy.force File.mlstate_dir)^"/cookies.txt"*);
    server_wait_for_request_timeout = Time.seconds 36;
    server_wait_for_request_initial_timeout = Time.seconds 36;
    server_write_timeout = Time.hours 1;
    maximum_number_of_connections = max_int;
    maximum_content_length = (50*1024*1024);
    maximum_number_of_headers = 200;
    remote_logs = None;
    favicon_ico = null_body_value (*(body_value_from_home ~log:true ".favicon.ico")*);
    favicon_gif = null_body_value (*(body_value_from_home ~log:true ".favicon.gif")*);
    backtrace = true;
    name = "httpServerPort";
    addr = "0.0.0.0";
    port = 8080;
    block_size = 4096; (* TODO: implement separate callbac blocksize *)
    allowed_hosts = [];
    dos_prevention = true;
    on_server_run = (fun _ _ -> ());
    on_server_close = (fun _ -> ());
    get = handle_get;
    post = handle_post;
    pre_headers = pre_headers;
    post_headers = post_headers;
    callback = Some null_callback;
  }

let prefixed_opt name opt =
  if name = "" then
    [sprintf "--%s" opt]
  else
    [sprintf "--%s-%s" name opt; sprintf "--%s" opt]

let opt_time = function
  | "inf" | "INF" | "Inf" | "infinity" | "Infinity" | "INFINITY" | "none" | "None" | "NONE" -> Time.infinity
  | s ->
      try Time.seconds (int_of_string s)
      with | Failure "int_of_string" -> failwith ("Argument '"^s^"' not valid time (<int> | \"inf\")")

let string_of_opt_time t = if t = Time.infinity then "inf" else sprintf "%7.0f" (Time.in_seconds t)

let spec_args name =
  let p = prefixed_opt name in
  [
    (p"addr")@["-a"],
    ServerArg.func ServerArg.string
      (fun o a ->
         ((try ignore (Unix.inet_addr_of_string a) with Failure _ -> (Logger.critical "Bad address: %s" a; exit 1));
            { o with addr = a })),
    "<string>", (sprintf "Sets the IP address on which the server should run (default:%s)"
                         default_options.addr);

    (p"port")@["-p"],
    ServerArg.func ServerArg.int
      (fun o p -> if p > 0xffff then (Logger.critical "Bad port number: %d" p; exit 1) else { o with port = p }),
    "<int>", (sprintf "Sets the port on which the server should run (default:%d)" default_options.port);

    p"long-cookies",
    ServerArg.func ServerArg.bool (fun o b -> { o with long_cookies = b }),
    "<bool>", (sprintf "Use long cookies (default:%b)" default_options.long_cookies);

    p"cookie-expire-short",
    ServerArg.func ServerArg.int (fun o i -> { o with cookie_expire_time_short = Time.seconds i }),
    "<int>", (sprintf "Cookie expire time (short) seconds (default:%1.0f)"
                      (Time.in_seconds default_options.cookie_expire_time_short));

    p"cookie-expire-long",
    ServerArg.func ServerArg.int (fun o i -> { o with cookie_expire_time_long = Time.seconds i }),
    "<int>", (sprintf "Cookie expire time (long) seconds (default:%2.0f)"
                      (Time.in_seconds default_options.cookie_expire_time_long));

    p"long-cookie-expire-variable",
    ServerArg.func ServerArg.string (fun o s -> { o with dt1 = opt_time s }),
    "<int>|\"inf\"", (sprintf "Long cookie variable expire time seconds (default:%s)"
                              (string_of_opt_time default_options.dt1));

    p"long-cookie-expire-fixed",
    ServerArg.func ServerArg.string (fun o s -> { o with dt2 = opt_time s }),
    "<int>|\"inf\"", (sprintf "Long cookie fixed expire time seconds (default:%s)"
                              (string_of_opt_time default_options.dt2));

    p"max-external-cookies",
    ServerArg.func ServerArg.int (fun o i -> { o with max_external_cookies = i }),
    "<int>", (sprintf "Maximum number of concurrent external cookies per internal cookie (default:%d)"
                      default_options.max_external_cookies);

    p"no-rotate-cookies",
    ServerArg.func ServerArg.unit (fun o () -> { o with rotate_cookies = false }),
    "", (sprintf "Switch off cookie rotation" (*default_options.rotate_cookies*));

    p"server-send-buffer-size",
    ServerArg.func ServerArg.int (fun o i -> { o with server_send_buffer_size = i }),
    "<int>", (sprintf "Server send buffer size (default: %d)" default_options.server_send_buffer_size);

    p"cookie-gc-period",
    ServerArg.func ServerArg.int (fun o i -> { o with cookie_gc_period = i }),
    "<int>", (sprintf "Cookie GC period in requests (default: %d)" default_options.cookie_gc_period);

    p"cookie-pool-size-min",
    ServerArg.func ServerArg.int (fun o i -> { o with cookie_pool_size_min = i }),
    "<int>", (sprintf "Cookie pool size minimum (default: %d)" default_options.cookie_pool_size_min);

    p"cookie-pool-size-max",
    ServerArg.func ServerArg.int (fun o i -> { o with cookie_pool_size_max = i }),
    "<int>", (sprintf "Cookie pool size maximum (default: %d)" default_options.cookie_pool_size_max);

    p"cookie-timer-interval",
    ServerArg.func ServerArg.int (fun o i -> { o with cookie_timer_interval = i }),
    "<int>", (sprintf "Cookie timer interval (seconds) (default: %d)" default_options.cookie_timer_interval);

    p"cookie-rate-max",
    ServerArg.func ServerArg.float (fun o f -> { o with cookie_rate_max = f }),
    "<float>", (sprintf "Cookie connection rate max (default: %3.1f)" default_options.cookie_rate_max);

    p"cookie-period-max",
    ServerArg.func ServerArg.int (fun o i -> { o with cookie_period_max = i }),
    "<int>", (sprintf "Cookie rotation period above max rate (default: %d)" default_options.cookie_period_max);

    p"cookie-rate-ultimate",
    ServerArg.func ServerArg.float (fun o f -> { o with cookie_rate_ultimate = f }),
    "<float>", (sprintf "Cookie connection rate ultimate (default: %3.1f)" default_options.cookie_rate_ultimate);

    p"cookie-period-ultimate",
    ServerArg.func ServerArg.int (fun o i -> { o with cookie_period_ultimate = i }),
    "<int>", (sprintf "Cookie rotation period above ultimate rate (default: %d)" default_options.cookie_period_ultimate);

    p"cookies-filename",
    ServerArg.func ServerArg.string (fun o s -> { o with cookies_filename = s }),
    "<filename>", (sprintf "Cookies filename (empty=disabled) (default: %s)" default_options.cookies_filename);

    p"wait-for-request-timeout",
    ServerArg.func ServerArg.float (fun o f -> { o with server_wait_for_request_timeout = Time.seconds_float f }),
    "<float>", (sprintf "Timeout while waiting for requests (default: %4.1f)"
                        (Time.in_seconds default_options.server_wait_for_request_timeout));

    p"wait-for-request-initial-timeout",
    ServerArg.func ServerArg.float (fun o f -> { o with server_wait_for_request_initial_timeout = Time.seconds_float f }),
    "<float>", (sprintf "Initial timeout while waiting for requests (default: %4.1f)"
                        (Time.in_seconds default_options.server_wait_for_request_initial_timeout));

    p"write-timeout",
    ServerArg.func ServerArg.float (fun o f -> { o with server_write_timeout = Time.seconds_float f }),
    "<float>", (sprintf "Timeout while writing data (default: %6.1f)" (Time.in_seconds default_options.server_write_timeout));

    p"remote-logs",
    ServerArg.func ServerArg.string (fun o s ->
       try
         let (hostname,port_appkey) = Base.String.split_char ':' s in
         let (port,appkey) = Base.String.split_char '/' port_appkey in
         let port = int_of_string port in
         let remote_logs = Some {HST.hostname=hostname; HST.port=port; HST.appkey=appkey} in
         remote_logs_params := remote_logs;
         {o with remote_logs = remote_logs}
       with
       | Not_found -> let _ = prerr_endline ("Bad option \""^s^"\" for --remote-logs") in o
       | Failure s -> let _ = prerr_endline ("Invalid port for --remote-logs."^s) in o
        ),
    "<hostname:port/appkey>", "Log access to a remote server (WARNING: this is experimental) (default: no log server).";

    (*(p"max-connections")@["-C"],
    ServerArg.func ServerArg.int (fun o i -> { o with maximum_number_of_connections = i }),
    "<int>", "Maximum number of active server connections (default: 100)";*)

    p"maximum-content-length",
    ServerArg.func ServerArg.int (fun o i -> { o with maximum_content_length = i }),
    "<int>", (sprintf "Maximum request content length (default: %d)" default_options.maximum_content_length);

    p"maximum-number-of-headers",
    ServerArg.func ServerArg.int (fun o i -> { o with maximum_number_of_headers = i }),
    "<int>", (sprintf "Maximum number of request headers (default: %d)" default_options.maximum_number_of_headers);

    p"no-print-log-info",
    ServerArg.func ServerArg.unit (fun o () -> { o with print_log_info = false }),
    "", (sprintf "Disable access and error logs" (*default_options.print_log_info*));

    p"no-print-server-info",
    ServerArg.func ServerArg.unit (fun o () -> { o with print_server_info = false }),
    "", (sprintf "Disable server information printout" (*default_options.print_server_info*));

    p"no-flood-prevention",
    ServerArg.func ServerArg.unit (fun o () -> { o with dos_prevention = false }),
    "", (sprintf "Disable the built-in protection against Denial-of-Service attacks" (*default_options.dos_prevention*));

    p"no-backtrace",
    ServerArg.func ServerArg.unit (fun o () -> { o with backtrace = false }),
      "", (sprintf "Disable backtrace printout for server exceptions" (*default_options.backtrace*));

    p"ssl-cert",
    ServerArg.func ServerArg.string (fun o s -> { o with ssl_cert = s }),
    "<file>", (sprintf "Location of your SSL certificate (requires ssl-key) (default:'%s')" default_options.ssl_cert);

    p"ssl-key",
    ServerArg.func ServerArg.string (fun o s -> { o with ssl_key = s }),
    "<file>", (sprintf "Location of your SSL key (requires ssl-cert) (default:'%s')" default_options.ssl_key);

    p"ssl-pass",
    ServerArg.func ServerArg.string (fun o s -> { o with ssl_pass = s }),
    "<string>", (sprintf "Password of your SSL certificate (requires ssl-cert and ssl-key options) (default:'%s')"
                         default_options.ssl_pass);

    p"dialog",
    ServerArg.func ServerArg.string (fun o s -> { o with dialog = s }),
    "<string>", (sprintf "Name of the http dialog to use  (default:'%s')                                     "
                         default_options.dialog);

    p"pidfile",
    ServerArg.func ServerArg.string (fun o s -> { o with pid_file = Some s }),
    "<string>", "File to dump server's pid. Server exits on error."

  ]

(* From httpServerOptions *)
let make_ssl_cert opt =
  match opt.ssl_certificate with
  | Some x -> Some x
  | None ->
      if opt.ssl_cert <> "" then
        if opt.ssl_key <> "" then
          Some (SslAS.make_ssl_certificate opt.ssl_cert opt.ssl_key opt.ssl_pass)
        else begin
          Logger.critical "Error : ssl-cert option MUST be used with ssl-key option";
          exit 1
        end
      else
        None

let make_ssl_verify opt =
  match opt.ssl_verify_params with
  | Some x -> Some x
  | None ->
      if opt.ssl_ca_file <> "" || opt.ssl_ca_path <> "" || opt.ssl_client_cert_path <> "" then
        Some (SslAS.make_ssl_verify_params ~client_ca_file:opt.ssl_client_ca_file
          ~accept_fun:opt.ssl_accept_fun ~always:opt.ssl_always
          opt.ssl_ca_file opt.ssl_ca_path opt.ssl_client_cert_path)
      else
        None

let init_server opt runtime server_info =
  if opt.print_log_info then HSCm.init_logger ();
  if opt.print_server_info then HSCm.banner runtime server_info

let options = ((Hashtbl.create 4):(string,options) Hashtbl.t)
let () = Hashtbl.add options "default" default_options

let get_options ?(name="default") () = Hashtbl.find options name

let make (name:string) (opt:options) (sched:Scheduler.t) : t =
  #<If>Logger.debug "HttpServer.make: name=%s addr=%s port=%d ssl_cert=%s" name opt.addr opt.port opt.ssl_cert#<End>;
  let _ = Lazy.force m2 in
  Hashtbl.add options name opt;
  let secure_mode = Network.secure_mode_from_params (make_ssl_cert opt) (make_ssl_verify opt) in
  let addr = Unix.inet_addr_of_string opt.addr in
  let server_info = HSCm.make_server_info addr opt.port (opt.ssl_cert <> "") in
  let is_secure = match secure_mode with Network.Secured _ -> true | _ -> false in
  let tm = Time.now () in
  let lc = Time.localtime tm in
  let hr = handle_request ~cachetype:opt.cachetype ~is_secure server_info tm lc in
  HSCm.set_allowed_hosts opt.allowed_hosts;
  HSCm.use_long_cookies := opt.long_cookies;
  (if !HSCm.use_long_cookies then CookieLong.init_cookies else Cookie2.init_cookies)
    ~sched ~gc_period:opt.cookie_gc_period
    ~pool_min:opt.cookie_pool_size_min
    ~pool_max:opt.cookie_pool_size_max
    ~timer_interval:opt.cookie_timer_interval
    ~rate_max:opt.cookie_rate_max
    ~period_max:opt.cookie_period_max
    ~rate_ultimate:opt.cookie_rate_ultimate
    ~period_ultimate:opt.cookie_period_ultimate
    ~expires_short:opt.cookie_expire_time_short
    ~expires_long:opt.cookie_expire_time_long
    ~dt1:opt.dt1
    ~dt2:opt.dt2
    ~max_external_cookies:opt.max_external_cookies
    ~rotate_cookies:opt.rotate_cookies
    ~cookies_filename:opt.cookies_filename
    ();
  let log_accesses = ref true in
  #<If:NO_ACCESS_LOG> log_accesses := false #<End>;
  let gm = Time.gmtime tm in
  let diff = lc.Unix.tm_hour - gm.Unix.tm_hour in
  let sign = if diff > 0 then "+" else if diff < 0 then "-" else "" in
  HST.time_diff := sprintf "%s%02d00" sign diff;
  let runtime = {
    HSC.rt_get = opt.get;
    rt_post = opt.post;
    rt_core =
      { HSC.rt_pre_headers = opt.pre_headers;
        rt_post_headers = opt.post_headers;
        rt_server_send_buffer_size = opt.server_send_buffer_size;
        rt_server_wait_for_request_timeout = opt.server_wait_for_request_timeout;
        rt_server_wait_for_request_initial_timeout = opt.server_wait_for_request_initial_timeout;
        rt_maximum_number_of_connections = opt.maximum_number_of_connections;
        rt_maximum_content_length = opt.maximum_content_length;
        rt_maximum_number_of_headers = opt.maximum_number_of_headers;
        rt_log_accesses = (!log_accesses);
        rt_remote_logs = opt.remote_logs;
        rt_time_diff = !(HST.time_diff);
        rt_plim = 128;
      };
    rt_server =
      { HSC.rt_dialog_content = Obj.magic None;
        rt_dialog_name = opt.dialog;
        rt_server_name = name;
        rt_on_run = opt.on_server_run opt;
        rt_on_close = opt.on_server_close;
        rt_favicon_ico = opt.favicon_ico;
        rt_favicon_gif = opt.favicon_gif;
      };
    rt_proto =
      { HSC.rt_name = opt.name;
        rt_addr = opt.addr;
        rt_port = opt.port;
        rt_secure_mode = secure_mode;
        rt_block_size = opt.block_size;
        rt_backtrace = opt.backtrace;
        rt_server_write_timeout = opt.server_write_timeout;
        rt_payload = HSC.null_payload;
      };
    rt_tmp =
      { HSC.rt_hr = hr;
        rt_conn = 0;
        rt_callback = opt.callback;
      };
  } in
  init_server opt runtime server_info;
  let () = (
    match opt.pid_file with
    | None -> ()
    | Some f -> (
        try begin
          let ochan = open_out f in
          let () = output_string ochan (sprintf "%d" (Unix.getpid())) in
          let () = close_out ochan in
          at_exit (fun () ->
                     try  Unix.unlink f
                     with Unix.Unix_error (Unix.ENOENT, _, s2) ->
                       Logger.critical "HttpServer.make: couldn't delete pid file '%s'\n" s2
                  )
        end with Sys_error e ->
          let () = Logger.critical "HttpServer.make:'%s'\n" e in exit 1
     )
  ) in
  { HSC.runtime = runtime; HSC.err_cont = None; HSC.extra_params = hr; }

let get_ports (server:t) (sched:Scheduler.t) =
  (HSC.get_ports server sched)
  @[(server.HSC.runtime.HSC.rt_server.HSC.rt_dialog_name,
     `HttpDialog { HttpDialog.set_dialog = fun dialog -> server.HSC.runtime.HSC.rt_server.HSC.rt_dialog_content <- dialog })]

let get_description _http_server _sched = `HttpServer

let run http_server sched = http_server.HSC.runtime.HSC.rt_server.HSC.rt_on_run sched; http_server

let close (http_server:t) sched = http_server.HSC.runtime.HSC.rt_server.HSC.rt_on_close sched
