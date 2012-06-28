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
(* httpServerCommon.ml
 *
 *)
module List = Base.List
module String = Base.String
module Char = Base.Char

module Rc = Rcontent
module HSCp = HttpServerCore_parse
module HST = HttpServerTypes
module HSC = HttpServerCore
module HT = HttpTools

#<Debugvar:HTTP_DEBUG>

let make_server_info addr port secure =
  let ip_or_name =
    match Unix.string_of_inet_addr addr with
    | "0.0.0.0" -> Unix.gethostname ()
    | ip -> ip
  in
  let portstr = if port = 80 then "" else Printf.sprintf ":%d" port in
  let server_url = Printf.sprintf "http%s://%s%s" (if secure then "s" else "") ip_or_name portstr in
  { HST.server_url=server_url; HST.server_id=0; HST.server_ip_or_name=ip_or_name;
    HST.server_port=port; HST.server_secured=secure; }

(* Protection against DNS-rebinding attacks *)
let set_allowed_hosts li = HSC.allowed_hosts := li
let check_host headers =
  match List.find_opt (function HSCp.Host _ -> true | _ -> false) headers with
    Some (HSCp.Host host) ->
      if !(HSC.allowed_hosts) <> [] && not (List.mem host !(HSC.allowed_hosts))
      then (Logger.error "Host is unknown: %s" host;
            failwith "error, aborting")
      else #<If$minlevel 10>Logger.debug "check_host %s OK.\n%!" ("["^(String.concat "," (!(HSC.allowed_hosts)))^"]")#<End>
  | None -> ()
  | _ -> assert false

(* Cookies:
   cookies2In: decides whether to call Cookie2 or CookieLong, or no cookies at all.
   cookies2Out: installs Set_Cookie in headers.
*)

let get_version version = if version = 1 then Printf.sprintf "; version=%d" version else ""

let get_expires delcookies expires =
  if delcookies
  then "; expires=Thu, 01-Jan-1970 00:00:01 GMT"
  else if Time.is_positive expires
  then "; expires="^(Date.rfc1123 (Time.gmtime (Time.add (Time.now()) expires)))
  else ""

let get_max_age delcookies expires =
  if delcookies
  then "; max-age=0"
  else if Time.is_positive expires
  then "; max-age="^(string_of_int (truncate (Time.in_seconds expires)))
  else ""

let get_object quotes name obj_opt =
  let q = if quotes then "\"" else "" in
  Option.default_map "" (fun obj -> if obj <> "" then "; "^name^"="^q^obj^q else "") obj_opt

let get_path quotes path_opt = get_object quotes "path" path_opt
let get_domain quotes domain_opt = get_object quotes "domain" domain_opt
let get_comment comment_opt = get_object true "comment" comment_opt
let get_commenturl commenturl_opt = get_object true "commenturl" commenturl_opt

let get_secure secure = if secure then "; secure" else ""
let get_discard discard = if discard then "; discard" else ""

let portstr num =
  if num >= 0 && num < 65535
  then string_of_int num
  else raise (Failure (Printf.sprintf "Cookie port num ouit of range %d" num))

let get_ports = function
    Some [] -> "; Port"
  | Some ports -> "; Port=\""^(String.concat "," (List.map portstr ports))^"\""
  | None -> ""

let use_long_cookies = ref true

let cookieless_uris = ref StringSet.empty
let add_cookieless_uri uri = cookieless_uris := StringSet.add uri !cookieless_uris
let _ = List.iter add_cookieless_uri ["/favicon.ico"; "/favicon.gif"]

let cookies2In (hr:HST.handle_request) (uri:string) =
  if StringSet.mem uri !cookieless_uris
  then hr
  else
    if !use_long_cookies
    then CookieLong.get_internal hr
    else
      let (_c2type_opt,(hr,_found,(_ec,_ic))) = Cookie2.get_internal hr in
      hr

let cookies2Out (hr:HST.handle_request) uri delcookies _headers =
  if StringSet.mem uri !cookieless_uris
  then []
  else
    match
      if delcookies
      then false, Time.zero, "ec=Null", "ic=Null"
      else
        (match hr.HST.hr_ec, hr.HST.hr_ic with
         | "", _
         | _, "" ->
             true, Time.zero, "", ""
         | _ec, ic ->
             if !use_long_cookies
             then
               let max_age, ecstr, icstr = CookieLong.get_external hr in
               (true, max_age, ecstr, icstr)
             else
               let max_age, ecstr, icstr = Cookie2.get_external ic in
               (false, max_age, ecstr, icstr))
    with
    | _, _, "", "" -> []
    | expiry_changed, max_age, ecstr, icstr ->
        (let pathstr = get_path false (Some "/") in
         let expirestr = if expiry_changed then get_expires delcookies max_age else "" in
         let str = expirestr^pathstr in
         [ HSCp.Set_Cookie (ecstr^str); HSCp.Set_Cookie (icstr^str) ])

(* Compression *)

let is_gzip_deflate headers =
  match List.find_opt (function HSCp.Accept_Encoding _ -> true | _ -> false) headers with
    Some (HSCp.Accept_Encoding s) ->
      ((String.is_contained "gzip" s) && not (String.is_contained "gzip;q=0" s),
       (String.is_contained "deflate" s) && not (String.is_contained "deflate;q=0" s))
  | _ -> (false,false)

type compression_limits =
    CL_Never | CL_Always | CL_Bounded of (int * int)

let (defMin,defMax) = (2048,2*1024*1024)

(* Always compress these *)
let always = [ "text/plain"; "text/javascript"; "text/css"; "text/html";
               "application/x-javascript"; "application/wsdl+xml"; "application/xhtml+xml"; ]

(* Never compress these *)
let never = [ "image/x-xbitmap"; "image/x-xpixmap"; "image/x-xwindowdump"; "image/x-cmu-raster"; "image/x-portable-anymap";
              "image/x-portable-bitmap"; "image/x-portable-graymap"; "image/x-rgb"; "image/gif"; "image/jpeg"; "image/tiff";
              "audio/basic"; "audio/x-wav";
              "video/mpeg"; "video/quicktime"; "video/x-sgi-movie";
              "application/zip"; "application/x-bcpio"; "application/x-cpio"; "application/x-shar"; "application/x-tar";
              "application/x-dvi"; "application/x-hdf"; "application/x-x509-ca-cert"; "multipart/x-zip";]

(* Compress these between given limits *)
let bounded = [ "application/octet-stream"; "application/postscript"; "application/pdf"; "application/java";
                "application/x-csh"; "application/x-sh"; "application/x-tcl"; "application/x-tex";
                "application/x-latex"; "application/x-texinfo"; "application/xml"; "unknown/unknown"; ]

let comp_lim_map =
  let clm = StringMap.empty in
  let clm = List.fold_left (fun clm mt -> StringMap.add mt CL_Always clm) clm always in
  let clm = List.fold_left (fun clm mt -> StringMap.add mt CL_Never clm) clm never in
  let clm = List.fold_left (fun clm mt -> StringMap.add mt (CL_Bounded (defMin,defMax)) clm) clm bounded in
  clm

let get_compression_limit mime_type =
  (* We find that the mime_type value actually includes the charset.  It's
     not supposed to but we can't do anything about that.  We'll just have to
     strip it out here. *)
  match String.slice ';' mime_type with
  | [] -> CL_Never
  | mt::_ ->
      (match StringMap.find_opt (String.trim mt) comp_lim_map with
       | Some cl -> cl
       | None -> ((* This was supposed to signal an unknown mime type which should then
                     have been added to the above lists. *)
                  (*Logger.warning "get_compression_limit: Unknown mime type \"%s\"" mime_type;*)
                  CL_Never))

let needs_compressed mime_type content_len =
  match get_compression_limit mime_type with
  | CL_Always -> true
  | CL_Never -> false
  | CL_Bounded(mn,mx) -> content_len > mn && content_len < mx

(* Caching *)

let is_valid get_md5 mtime_opt hs =
  let rec aux = function
    | (HSCp.If_None_Match md5in)::t ->
        let eq = md5in = (get_md5()) in
        #<If$minlevel 10>Logger.debug "is_valid: md5 match %b\n%!" eq#<End>;
        eq || aux t
    | (HSCp.If_Modified_Since mtimein)::t ->
        (match mtime_opt with
           Some mtime ->
             let ge = Date.of_string mtimein >= Time.round_to_sec mtime in
             #<If$minlevel 10>Logger.debug "is_valid: mod since %b\n%!" ge#<End>;
             ge || aux t
         | None -> aux t)
    | (HSCp.If_Unmodified_Since mtimein)::t ->
        (match mtime_opt with
           Some mtime ->
             let lt = Date.of_string mtimein < Time.round_to_sec mtime in
             #<If$minlevel 10>Logger.debug "is_valid: unmod since %b\n%!" lt#<End>;
             lt || aux t
         | None -> aux t)
    | (HSCp.Cache_Control "no-cache")::_ -> false
    (*| (HSCp.Cache_Control "max=age=0")::t -> false (don't use this) *)
    | (HSCp.Pragma "no-cache")::_ -> false
    | _::t -> aux t
    | [] -> false
  in aux hs

let process_content_with_headers sched hr_opt ?(modified_since=None) ?(compression_level=6) ?(cache_response=true)
    ?(_delcookies=false) ?(use_etag=false) ?(use_md5=false) ?(_type="text/plain")
    _uri content headers_in headers_out include_body cont =
  #<If$minlevel 10>Logger.debug "process_content: modified_since=%s\n%!"
    (Option.to_string (fun d -> (Date.rfc1123 (Time.gmtime d))) modified_since)#<End>;
  #<If>Logger.debug "process_content: _type=%s\n%!" _type#<End>;
  let md5 = ref (false,"") in
  let get_md5 () =
    if fst !md5
    then snd !md5
    else (let _md5 = Rc.content_md5 content in md5 := (true,_md5); _md5)
  in
  if is_valid get_md5 modified_since headers_in
  then (#<If>Logger.debug "not modified\n%!"#<End>;
        cont None)
  else
    let time = match hr_opt with Some hr -> hr.HST.hr_timestamp | None -> Time.now () in
    let time_now = Time.gmtime time in
    let content_len = Rc.content_length content in
    let gzip, deflate = is_gzip_deflate headers_in in
    let compression_level = if Base.is_windows then 0 else compression_level in
    #<If$minlevel 20>Logger.debug "process_content: compressing(%s)\n%!"
      (Rc.string_of_content_type (Rc.get_content_type content))#<End>;
    let f cont = cont (false, content) in
    (if compression_level > 0 && (needs_compressed _type content_len)
     then (HT.content_compress sched gzip deflate compression_level cache_response content content_len)
     else f)
      (function (compressed, content) ->
         #<If$minlevel 20>Logger.debug "process_content: compressed=%b\n%!" compressed#<End>;
         let content_out = if include_body then content else Rc.ContentNone in
         let cs = if String.is_contained "charset" _type then "" else "; charset=utf-8" in
         let typeval = (_type^cs,[]) in
         #<If$minlevel 10>Logger.debug "process_content: md5=%s" (get_md5())#<End>;
         let headers =
           [(HSCp.Date (Date.rfc1123 time_now));(HSCp.Server HSC.server_name)]
           @(if use_etag then [HSCp.ETag (get_md5())] else [])
           @(if use_md5 then [HSCp.Content_MD5 (get_md5())] else [])
           @([HSCp.Content_Type typeval])
           @ headers_out
         in
         #<If$minlevel 10>Logger.debug "process_content: headers=%s\n%!"
           (String.concat "" (List.map HSC.string_of_msg headers))#<End>;
         let content_encoding = if deflate then "deflate" else if gzip then "gzip" else "identity" in
         let res =
           Some (if compressed
                 then (#<If$minlevel 10>Logger.debug "content compressed\n%!"#<End>;
                       (headers@[HSCp.Content_Encoding content_encoding],
                        content_out, Int64.of_int (Rc.content_length content)))
                 else (headers, content_out, Int64.of_int content_len))
         in
         #<If$minlevel 20>Logger.debug "process_content: returning\n%!"#<End>;
         cont res)

let process_content sched hr_opt ?(modified_since=None) ?(compression_level=6) ?(cache_response=true) ?(expires=Time.zero)
    ?(cache=true) ?(_delcookies=false) ?(use_etag=false) ?(use_md5=false) ?(_type="text/plain") ?content_dispo
    _uri content headers_in include_body cont =
  #<If$minlevel 10>Logger.debug "process_content: modified_since=%s\n%!"
    (Option.to_string (fun d -> (Date.rfc1123 (Time.gmtime d))) modified_since)#<End>;
  #<If>Logger.debug "process_content: _type=%s\n%!" _type#<End>;
  let md5 = ref (false,"") in
  let get_md5 () =
    if fst !md5
    then snd !md5
    else (let _md5 = Rc.content_md5 content in md5 := (true,_md5); _md5)
  in
  if is_valid get_md5 modified_since headers_in
  then (#<If>Logger.debug "not modified\n%!"#<End>;
        cont None)
  else
    let time = match hr_opt with Some hr -> hr.HST.hr_timestamp | None -> Time.now () in
    let time_now = Time.gmtime time in
    let content_len = Rc.content_length content in
    let gzip, deflate = is_gzip_deflate headers_in in
    let compression_level = if Base.is_windows then 0 else compression_level in
    #<If$minlevel 20>Logger.debug "process_content: compressing(%s)\n%!"
      (Rc.string_of_content_type (Rc.get_content_type content))#<End>;
    let f cont = cont (false, content) in
    (if compression_level > 0 && (needs_compressed _type content_len)
     then (HT.content_compress sched gzip deflate compression_level cache_response content content_len)
     else f)
      (function (compressed, content) ->
         #<If$minlevel 20>Logger.debug "process_content: compressed=%b\n%!" compressed#<End>;
         let content_out = if include_body then content else Rc.ContentNone in
         let cs = if String.is_contained "charset" _type then "" else "; charset=utf-8" in
         let typeval = (_type^cs,[]) in
         #<If$minlevel 10>Logger.debug "process_content: md5=%s" (get_md5())#<End>;
         let headers =
           [(HSCp.Date (Date.rfc1123 time_now));(HSCp.Server HSC.server_name)]
           @(if use_etag then [HSCp.ETag (get_md5())] else [])
           @(match modified_since with
               Some date -> [ HSCp.Cache_Control "public"; HSCp.Last_Modified (Date.rfc1123 (Time.gmtime date)) ]
             | None -> [])
           @(if use_md5 then [HSCp.Content_MD5 (get_md5())] else [])
           @([HSCp.Content_Type typeval])
           @([HSCp.Expires (Date.rfc1123 (if Time.is_infinite expires then { time_now with Unix.tm_year = time_now.Unix.tm_year + 1 }
                                          else if Time.is_positive expires then Time.gmtime (Time.add time expires)
                                          else time_now))])
           @(if not cache then [ (HSCp.Cache_Control "no-cache") ; (HSCp.Pragma "no-cache") ] else [])
           @(match content_dispo with Some s -> [HSCp.Content_Disposition ("attachment",["filename="^s])] | _ -> [])
         in
         #<If$minlevel 10>Logger.debug "process_content: headers=%s\n%!"
           (String.concat "" (List.map HSC.string_of_msg headers))#<End>;
         let content_encoding = if deflate then "deflate" else if gzip then "gzip" else "identity" in
         let res =
           Some (if compressed
                 then (#<If$minlevel 10>Logger.debug "content compressed\n%!"#<End>;
                       (headers@[HSCp.Content_Encoding content_encoding],
                        content_out, Int64.of_int (Rc.content_length content)))
                 else (headers, content_out, Int64.of_int content_len))
         in
         #<If$minlevel 20>Logger.debug "process_content: returning\n%!"#<End>;
         cont res)

let get_body sched hr ?(compression_level=6) ?(cache_response=true) ?(use_etag=false) uri _type headers include_body =
  let stat = Unix.stat uri in
  #<If$minlevel 10>Logger.debug "get_body: uri=%s size=%d\n%!" uri stat.Unix.st_size#<End>;
  let content =
    if stat.Unix.st_size > (1024*1024)
    then Rc.ContentFile (uri,None,None,Some stat,false)
    else Rc.ContentString (File.content uri) in
  process_content sched (Some hr) ~modified_since:(Some (Time.of_unix_time stat.Unix.st_mtime))
                  ~_type ~compression_level ~cache_response ~use_etag
                  uri content headers include_body

let get_body_from_value sched hr ?(compression_level=6) ?(cache_response=true) ?(use_etag=false)
                        ((_,content,ms,mt):HSC.body_value) headers include_body cont =
  #<If$minlevel 10>Logger.debug "get_body_from_value: size=%d\n%!" (Rc.content_length content)#<End>;
  process_content sched (Some hr) ~modified_since:(Some ms) ~_type:mt ~compression_level ~cache_response ~use_etag
                  "" content headers include_body cont

let init_logger () =
  #<If:TESTING> () #<Else>
  let access_logger =
    String.concat ", " (Logger.get_access_logger_destinations())
  in
  let error_logger =
    String.concat ", " (Logger.get_error_logger_destinations())
  in
  Logger.notice "Accesses logged to %s" access_logger;
  Logger.notice "Messages logged to %s" error_logger
  #<End>;
  Logger.debug "#run"

let banner runtime server_info =
  let name = String.capitalize runtime.HSC.rt_server.HSC.rt_server_name in
  let version = HSC.server_name in
  let url = server_info.HST.server_url in
  #<If:TESTING> () #<Else>
  Logger.notice "%s (%s) serving on %s" name version url
  #<End>
