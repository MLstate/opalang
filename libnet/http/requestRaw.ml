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
open Printf
open Base
open Option
open Rp_typ
open Rp_hdr
open Requestdef
open RequestType

module S = String
module L = List

(*let req =
  "GET /_internal_/code/all.js HTTP/1.1\r\n\
If-Modified-Since: Thu, 18 Nov 2010 17:12:02 GMT\r\n\
User-Agent: Mozilla/5.0 (X11; U; Linux i686 (x86_64); fr; rv:1.9.2.12) Gecko/20101026 Firefox/3.6.12\r\n\
Connection: keep-alive\r\n\
Cookie: 8d53aab25d7975f78b310cee5b9a158b\r\n\
Accept-Language: fr,fr-fr;q=0.8,en-us;q=0.5,en;q=0.3\r\n\
NewCookie: 8d53aab25d7975f78b310cee5b9a158b\r\n\
Host: localhost:8080\r\n\
Referer: http://localhost:8080/\r\n\
Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\n\
Accept: */*\r\n\r\n"*)

exception ParseFail of int

let ug = S.unsafe_get

let pos_crlf str len n =
  let rec pc0 pos =
    if pos >= len
    then len, len
    else
      let c1 = ug str pos in
      if c1 = '\r' || c1 = '\n'
      then
        if pos + 1 >= len
        then pos, pos+1
        else
          let c2 = ug str (pos+1) in
          if (c1 = '\r' && c2 = '\n') || (c1 = '\n' && c2 = '\r')
          then pos, pos+2
          else
            if (c1 = '\r' && c2 = '\r') || (c1 = '\n' && c2 = '\n')
            then pos, pos+1
            else pc0 (pos+1)
      else pc0 (pos+1) in
  pc0 n

let upto_crlf str len n =
  let pos, p2 = pos_crlf str len n in
  let l = pos - n in
  (p2,l,S.sub str n l)

let pos_HTTP str len n =
  (*printf "pos_HTTP: str=%s len=%d n=%d\n" str len n;*)
  let rec pH pos =
    if pos >= len - 4
    then raise (ParseFail pos)
    else
      if ug str pos = 'H' && ug str (pos+1) = 'T' && ug str (pos+2) = 'T' && ug str (pos+3) = 'P'
      then pos
      else pH (pos+1) in
  pH n

let skip_lws str len n =
  let rec sl pos =
    if pos >= len
    then len
    else
      if ug str pos = ' ' || ug str pos = '\t' || ug str pos = '\r' || ug str pos = '\n'
      then sl (pos+1)
      else pos in
  sl n

(* field value can span multiple lines with newline + space *)
let upto_header_crlf str len n =
  let pos = skip_lws str len n in
  let pos0 = pos in
  let pos_, pos = pos_crlf str len pos in
  let len0 = pos_ - pos0 in
  let str0 = S.sub str pos0 len0 in
  if pos >= len
  then (len,len0,str0)
  else if ug str pos <> ' ' && ug str pos <> '\t'
  then (pos,len0,str0)
  else
    let rec uhc str1 len1 pos =
      let pos = skip_lws str len pos in
      let pos0 = pos in
      let pos_, pos = pos_crlf str len pos in
      let l = pos_ - pos0 in
      let len1 = len1 + l + 1 in
      let str1 = (S.sub str pos0 l)::str1 in
      if pos >= len
      then (len,len1,S.concat " " (L.rev str1))
      else if ug str pos <> ' ' && ug str pos <> '\t'
      then (pos,len1,S.concat " " (L.rev str1))
      else uhc str1 len1 pos
    in
    uhc [str0] len0 pos

let get_rh hdr hdrlen nxt rh start rqst =
  (* header field with no colon is a bad request *)
  if ug hdr start <> ':' then raise (ParseFail nxt);
  (* any number of spaces between colon and field value *)
  let cpos = skip_lws hdr hdrlen (start + 1) in
  let v = S.sub hdr cpos (hdrlen-cpos) in
  (*let att = S.sub hdr 0 (cpos-2) in printf "Att: %s Val: %s\n" att v;*)
  (* FIXME why this try with block ??? *)
  try RequestHeader.add rqst (`string v) rh with _ -> raise (ParseFail nxt)

let rec get_hd rl req reqlen nxt rh =
  let nxt2,hdrlen,hdr = upto_header_crlf req reqlen nxt in
  (*printf "Hdr: %s len: %d\n" hdr hdrlen;*)
  if hdr = ""
  then nxt2,Complete {request_line=rl; request_header=rh; request_message_body=""; server_info=None}
  else
    (* Unknown fieldname must be skipped *)
    let rh = try hdr_call get_rh hdr hdrlen nxt2 rh with ParseFail_hdr _ -> rh in
    get_hd rl req reqlen nxt2 rh

let get_rl typ typlen nxt req reqlen start _method =
  (*printf "get_rl:\nlet typ=\"%s\";;\nlet typlen=%d;;\nlet nxt=%d;;\nlet start=%d;;\n" typ typlen nxt start;*)
  let start = start+1 in
  let hpos = pos_HTTP typ typlen start in
  let uri = S.sub typ start (hpos-(start+1)) in
  let vstart = hpos in
  let vlen = typlen-vstart in
  if vlen <> 8 then raise (ParseFail nxt);
  let ver = S.sub typ vstart vlen in
  (*printf "Type: %s\n" typ;*)
  get_hd { _method = _method; request_uri = uri ; http_version = ver } req reqlen nxt RequestHeader.empty

let parse_request req =
  let reqlen = S.length req in
  try
    (* Crlf before request are allowed... *)
    let rec loop pos =
      match upto_crlf req reqlen pos with
      | nxt,0,"" -> loop nxt
      | nxt,typlen,typ  -> typ_call get_rl typ typlen nxt req reqlen
    in
    loop 0
  with
    ParseFail nxt -> nxt,Incomplete
  | ParseFail_typ nxt -> nxt,Incomplete
  | ParseFail_hdr nxt -> nxt,Incomplete
  | _ -> reqlen,Incomplete

(*let _ = parse_request req*)
