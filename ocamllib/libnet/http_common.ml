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
(*
    @author Laurent Le Brun
**)

open Requestdef

let version = string_of_int BuildInfos.git_version_counter
let crlf = "\r\n"
let double_crlf = crlf ^ crlf

(** {6 Request} *)

let content_length req =
  match RequestHeader.get_string `Content_Length req.request_header with
  | None -> 0
  | Some v -> int_of_string v

let string_of_request_line r =
  Printf.sprintf "%s %s %s%s" (string_of_method r._method) r.request_uri r.http_version crlf

let string_of_request r =
  Printf.sprintf "%s%s%s%s"
    (string_of_request_line r.request_line)
    (RequestHeader.to_string string_of_request_header r.request_header)
    crlf
    (String.sub r.request_message_body 0 (content_length r))

(* for debug *)
let print_request_header req =
  match req with
  | Some req -> RequestHeader.iter (
      fun _k _v ->
        Logger.log ~color:`cyan "%s" (RequestHeader.to_string string_of_request_header req.request_header)
    ) req.request_header
  | _ -> ()

(** {6 Response} *)

type status_line =
    { status_http_version : string
    ; status : status }

(* e.g. "HTTP/0.9 302 Found" *)
let string_of_status_line sl =
  let code = status_code sl.status in
  let phrase = reason_phrase code in
  Printf.sprintf "%s %d %s%s" sl.status_http_version code phrase crlf

type res_body =
  | Result of string
  | PartialResult of int * string * (int -> res_body)
  | AuthenticationRequest of res_body
(*   | AuthenticationRequest of string (\* (string option) * requestDescription * (unit -> ret)  *\) *)

type response =
    { status_line : status_line
    ; response_header : ResponseHeader.header
    ; response_message_body : res_body }

let reponse_content_length req =
  match ResponseHeader.get_string `Content_Length req.response_header with
  | None -> 0
  | Some v -> int_of_string v

let rec string_of_body body =
  match body with
  | Result s -> s
  | PartialResult (_, s, _) -> Printf.sprintf "partial\n%s" s
  | AuthenticationRequest body ->
      let s = string_of_body body in
      Printf.sprintf "authentification\n%s" s

let string_of_response_header r =
  Printf.sprintf "%s%s%s"
    (string_of_status_line r.status_line)
    (ResponseHeader.to_string string_of_response_header r.response_header)
    crlf

let string_of_response ?(body_limit=1024) r =
  Printf.sprintf "%s%s"
    (string_of_response_header r)
    (String.sub (string_of_body r.response_message_body) 0 (min (reponse_content_length r) body_limit))

(* let string_of_response r = *)
(*   Printf.sprintf "%s%s%s%s" *)
(*     (string_of_status_line r.status_line) *)
(*     (ResponseHeader.to_string string_of_response_header r.response_header) *)
(*     crlf *)
(*     (r.response_message_body) *)

let parse_response response =
  let str = FBuffer.contents response in
  try
    let pos, req = Request.parse_request_response str in
    req, FBuffer.sub response pos (FBuffer.length response - pos)
  with
  | Trx_runtime.SyntaxError (loc, err) ->
      failwith (Printf.sprintf "Failed to parse http response: %s --> %s" str (Trx_runtime.show_error str loc err))

let print_response_header resp =
  ResponseHeader.iter (
    fun _k _v ->
      Logger.log ~color:`green "%s" (ResponseHeader.to_string (
        fun k -> Requestdef.string_of_response_header k) resp.response_header
      )
  ) resp.response_header
