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

(** This is a module for handling smtp mail sending.
    It is NOT really RFC compliant. *)

module SCC = SmtpClientCore
module List = Base.List
module String = Base.String
let (<|) f a = f a
let (|>) a f = f a
let ( @* ) g f x = g(f(x))

let mailer_name = Printf.sprintf "Opa-mailclient/%d" BuildInfos.git_version_counter

let sprintf = Printf.sprintf

let _log sep code reason = Logger.warning "%d%s%s" code sep reason

exception Bad_address of string
exception Too_much_try
exception Unknown_address of string

let split_email s =
  try let _, (_, user_domain) = Email.parse_email_email s in Some user_domain
  with Trx_runtime.SyntaxError _ -> None

let valid_email s =
  try ignore (Email.parse_email_email s); true
  with Trx_runtime.SyntaxError _ -> false

let simple_mail s =
  try
    let _, (_, (user, domain)) = Email.parse_email_email s in
    sprintf "%s@%s" user domain
  with Trx_runtime.SyntaxError _ -> raise (Bad_address s)

let mail_content ?(charset="ISO-8859-1") ?(cte="7bit") body =
  sprintf "Content-Type: text/plain; charset=%s\r\n\
Content-Transfer-Encoding: %s\r\n\
Content-Disposition: inline\r\n\
\r\n%s\r\n" charset cte body

(*
  FIXME:
  - we should produce quoted-printable content for html.
  - we should handle Content-ID for inline content.
*)
let mail_content_html ?(charset="ISO-8859-1") ?(cte="7bit") ~ascii_part body =
  let ascii_part = sprintf
    "Content-Type: text/plain; charset=%s\r\n\
Content-Transfer-Encoding: %s\r\n\
Content-Disposition: inline\r\n\
\r\n%s\r\n" charset cte ascii_part in
  let html_part = sprintf
    "Content-Type: text/html; charset=%s\r\n\
Content-Transfer-Encoding: %s\r\n\
Content-Disposition: inline\r\n\
\r\n%s\r\n" charset cte body in
  let boundary = String.random 30 in
  sprintf "Content-Type: multipart/alternative;\
boundary=%s\r\n\r\n\
--%s\r\n\
%s\r\n\
--%s\r\n\
%s\r\n\
--%s--\r\n"
    boundary boundary ascii_part boundary html_part boundary

let split_encode str n sep =
  let len = String.length str in
  if len <= n
  then str
  else
    let seplen = String.length sep in
    let newlen = len + seplen * ((len-1) / n) in
    let newstr = String.create newlen in
    let rec aux i j =
      if len - i <= n
      then (String.blit str i newstr j (len-i); newstr)
      else (String.blit str i newstr j n;
            String.blit sep 0 newstr (j+n) seplen;
            aux (i+n) (j+n+seplen))
    in
    aux 0 0

(* Most of this is just guesswork and it's incomplete, feel free to modify, add more etc. *)
let get_cte mime_type content =
  match mime_type with
  | "text/plain"
  | "text/html" -> "8bit", content
  | "application/octet-stream" | "application/postscript" | "application/pdf"
  | "image/x-xbitmap" | "image/x-xpixmap" | "image/x-xwindowdump" | "image/x-cmu-raster" | "image/x-portable-anymap"
  | "image/x-portable-bitmap" | "image/x-portable-graymap" | "image/x-rgb" | "image/gif" | "image/jpeg" | "image/tiff"
  | "audio/basic" | "audio/x-wav"
  | "video/mpeg" | "video/quicktime" | "video/x-sgi-movie"
  | "application/java" | "application/x-csh" | "application/x-sh" | "application/x-tcl" | "application/x-tex"
  | "application/x-latex" | "application/x-texinfo" | "application/zip" | "application/x-bcpio" | "application/x-cpio"
  | "application/x-shar" | "application/x-tar" | "application/x-dvi" | "application/x-hdf" | "application/x-x509-ca-cert"
  | "multipart/x-zip" | "application/xml"
  | "application/wsdl+xml" -> "base64", (String.base64encode content)
  | _ -> (Logger.warning "SmtpClient.get_cte: Unknown mime type \"%s\"" mime_type;
          "base64", (String.base64encode content))

let attach_one_file boundary content_type filename charset cte content =
  let xid = String.random 30 in
  sprintf "--%s\r\n\
Content-Type: %s; name=\"%s\"; charset=%s\r\n\
Content-Disposition: attachment; filename=\"%s\"\r\n\
Content-Transfer-Encoding: %s\r\n\
X-Attachment-Id: %s\r\n\
\r\n%s\r\n" boundary content_type filename
    charset filename cte xid (
      if cte = "base64"
      then split_encode content 76 "\r\n"
      else content)

let attach_content boundary mdata fs =
  let ct = sprintf "Content-Type: multipart/mixed; boundary=%s\r\n\r\n" boundary in
  let md = sprintf "--%s\r\n%s\r\n" boundary mdata in
  let eb = sprintf "--%s--" boundary in
  ct^md^(String.concat "" fs)^eb

let attach_files files mdata ?(charset="UTF-8") () =
  let boundary = String.random 30 in
  let fs =
    List.map (fun (filename,mime,cte,content) ->
      attach_one_file boundary mime filename charset cte content) files in
  attach_content boundary mdata fs

let attach_custom_headers(custom_headers) =
  List.fold_left (fun acc (name, value) -> sprintf "%s%s: %s\r\n" acc name value) "" custom_headers

let full_email ?(subject="") mfrom mto mdata ?return_path ?html ?(files=[]) ?(custom_headers=[]) ?cte ?charset () =
  let mdata = match html with
    | Some html -> mail_content_html ?charset ?cte ~ascii_part:mdata html
    | None -> mail_content ?charset ?cte mdata
  in
  let return_path =
    match return_path with
    | Some return_path -> return_path
    | None -> mfrom
  in sprintf
"From: %s\r\n\
To: %s\r\n\
Return-Path:<%s>\r\n\
Message-ID: <%s.%s>\r\n\
X-Mailer: %s\r\n\
Date: %s\r\n\
Mime-Version: 1.0\r\n\
%s\
%s\
%s"
mfrom mto return_path (String.random 10) return_path mailer_name (Date.rfc1123 (Time.gmtime (Time.now())))
(if subject = "" then "" else sprintf "Subject: %s\r\n" subject)
(attach_custom_headers custom_headers)
(if files = []
 then mdata
 else attach_files files mdata ?charset ())

let resolve_UNIX name =
  try
    (Unix.gethostbyname name).Unix.h_addr_list.(0)
     |> Unix.string_of_inet_addr
     |> String.slice '.'
     |> List.map int_of_string
     |> function [a;b;c;d] -> Some (a,b,c,d) | _ -> None
  with Not_found | Failure _ | Unix.Unix_error _ -> None

let resolve_additional r n =
  let rec aux = function
    | hd :: tl ->
        if hd.Dig.domain = n then
          match hd.Dig.dst with
          | Dig.Ip i -> Some i
          | _ -> aux tl
        else aux tl
    | _ -> resolve_UNIX (List.fold_left (fun acc x -> sprintf "%s.%s" acc x) (List.hd n) (List.tl n))
  in
  aux (List.assoc "ADDITIONAL" r)

external get_mx_dns : string -> (string * int) array = "get_mx_dns"

let get_mx name : string list =
  let arr = get_mx_dns name in
  Array.sort (fun x y -> compare (snd x) (snd y)) arr;
  arr
   |> Array.to_list
   |> List.map fst

(* FIXME:
   - we should use native mx query, instead of calling a command line !!!
   - il faut en sortie une iterateur IntMapSort d'IP, triée par priorité
     ensuite, on doit tenter les IP une à une... *)
let resolve_mx name =
  let output = File.process_output (sprintf "dig %s MX" name) in
  try
    let _pos, r = Dig.parse_dig_dig output in
      List.assoc "ANSWER" r
      |> List.filter_map (fun x ->
          match x.Dig.category with
          | Dig.Mx pri -> Some (pri, x.Dig.dst)
          | _ -> None)
      |> List.sort (fun (pri1, _) (pri2, _) -> compare pri1 pri2)
      |> List.filter_map (function
          | (_, Dig.Ip i) -> Some i
          | (_, Dig.Name n) ->
              Logger.info "resolve_mx: name=%s" (List.to_string Base.identity n);
              if List.mem_assoc "ADDITIONAL" r then resolve_additional r n
              else resolve_UNIX (String.concat "." n)
          )
  with Not_found | Failure _ ->
    Logger.error "resolve_mx: parsing failed!" ; []

let read_code s =
  let get i = int_of_char (String.unsafe_get s i) - 48 in
  let l = String.length s in
  if l > 3 then 100 * get 0 + 10 * get 1 + get 2, String.sub s 4 (4 - 3)
  else 0, "unknown server answer"

let analyze_error = Mailerror.parse_mailerror_error

let mail_send_aux ?client_certificate ?verify_params ?(secure=false) sched
    ?subject mfrom mdst ?mto mdata ?return_path ?html ?files ?custom_headers ?cte ?charset nb_attempt ?(port=25)
    ?via ?addr ?auth ?user ?pass cont () =
  let mto =
    match mto with
    | Some tos -> tos
    | None -> mdst in
  let wait_and_retry x k = ignore(Scheduler.sleep sched x k) in
  let mdata = full_email ?subject mfrom mto mdata ?return_path ?html ?files ?custom_headers ?cte ?charset () in
  #<If:PROTOCOL_DEBUG$minlevel 10>Logger.debug "mdata='%s'" mdata#<End>;
  let from = split_email mfrom
  and dst = split_email mdst in
  match from, dst with
  | None,_ -> cont SCC.Bad_Sender
  | _,None -> cont SCC.Bad_Recipient
  | (Some (_,domain_from)),(Some (_,dst)) ->
      let mail = { SCC.from = simple_mail mfrom;
                   dests = [mdst];
                   body = mdata;
                   auth = Option.default "" auth; user = Option.default "" user; pass = Option.default "" pass;
                 }
      in
      let rec try_mx mail attempt ?ip_list cont =
        let ip_list =
          match ip_list with
          | Some list -> list
          | None ->
              (match addr with
               | Some dst ->
                   (match resolve_UNIX dst with
                    | Some ip -> [ip]
                    | None -> [])
               | None -> resolve_mx dst)
        in
        match ip_list with
        | [] ->
            if attempt < 0 then
              let _ = Logger.warning "No working MX server found - can't send mail to %s" mdst in
              cont SCC.Error_MX
            else
              wait_and_retry (Time.seconds 60) (fun () -> try_mx mail (pred attempt) cont)
        | _ when attempt < 0 -> Logger.error "Too many failures" ; cont SCC.Error_MX
        | dst_ip :: mx_servers as ips ->
            let tools = {
              SCC.log = _log " " ;
              SCC.elog = _log "-" ;
              SCC.k = (function
                | SCC.Error_MX -> try_mx mail (pred attempt) ~ip_list:mx_servers cont
                | SCC.Error msg ->
                    ( prerr_endline ("ERROR: " ^ msg) ;
                      try
                        let _pos, res = analyze_error msg in
                        match res with
                        | Mailerror.GreylistedSec x ->
                            let x = if x < 90 then 90 else x in
                            Logger.debug "::: greylisted (%d secs)" x;
                            wait_and_retry (Time.seconds x) (fun () -> try_mx mail (pred attempt) ~ip_list:ips cont)
                        | Mailerror.GreylistedMin x ->
                            Logger.debug "::: greylisted (%d mins)" x;
                            let x = x * 60 in
                            wait_and_retry (Time.seconds x) (fun () -> try_mx mail (pred attempt) ~ip_list:ips cont)
                        | Mailerror.Add_cc s ->
                            let new_mail = { mail with SCC.body = sprintf "Cc: %s\r\n%s" s mail.SCC.body } in
                            wait_and_retry (Time.seconds 1) (fun () -> try_mx new_mail (pred attempt) ~ip_list:ips cont)
                        | _ when fst (read_code msg) = 451 ->
                            let x = 60 * attempt * attempt in
                            Logger.debug "::: waiting (%d sec)" x;
                            wait_and_retry (Time.seconds x) (fun () -> try_mx mail (pred attempt) ~ip_list:ips cont)
                        | _ -> cont (SCC.Error msg)
                      with Not_found | Failure _ | Trx_runtime.SyntaxError _ -> cont (SCC.Error msg))
                | res -> cont res) ;
            } in
            let client = {
              SCC.runtime = {
                SCC.rt_plim = 128;
                SCC.rt_proto = {
                  SCC.rt_name = "";
                  rt_addr = "";
                  rt_port = 0;
                  rt_secure_mode = Network.Unsecured;
                  rt_block_size = 4096;
                  rt_backtrace = true;
                  rt_server_write_timeout = Time.hours 2;
                  rt_payload = ();
                };
              };
              SCC.err_cont = None;
              SCC.extra_params = (mail,domain_from,tools)
            } in
            let dst_string = match via with
              | Some str_server -> str_server
              | None -> Network.string_of_ipv4 dst_ip in
            let secure_mode =
              if secure
              then Network.Secured (client_certificate, verify_params)
              else Network.Unsecured
            in SCC.connect client ~secure_mode sched dst_string port
      in try_mx mail nb_attempt cont

let mail_send ?client_certificate ?verify_params ?secure sched
    ?subject mfrom mdst ?mto mdata ?return_path ?html ?files ?cte ?charset nb_attempt
    ?port ?via ?addr ?auth ?user ?pass cont () =
  let files = match files with
    | Some l ->
        let res =
          List.map (fun (file,filename) ->
                      let content_type = HttpServer.mime_type file in
                      let base, content = get_cte content_type (File.content file) in
                      (filename,content_type,base,content)) l in
        Some res
    | None -> None in
  mail_send_aux ?client_certificate ?verify_params ?secure sched
    ?subject mfrom mdst ?mto mdata ?return_path ?html ?files ?cte ?charset nb_attempt
    ?port ?via ?addr ?auth ?user ?pass cont ()

