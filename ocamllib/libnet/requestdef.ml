(*
    Copyright Â© 2011 MLstate

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

(**)

exception Parsing of string

type request_header =
  [ `Cache_Control
  | `Connection
  | `Date
  | `Pragma
  | `Trailer
  | `Transfer_Encoding
  | `Upgrade
  | `Via
  | `Warning
  | `Allow
  | `Content_Encoding
  | `Content_Language
  | `Content_Length
  | `Content_Location
  | `Content_MD5
  | `Content_Range
  | `Content_Type
  | `Content_Disposition
  | `Expires
  | `Last_Modified
  | `Accept
  | `Accept_Charset
  | `Accept_Encoding
  | `Accept_Language
  | `Authorization
  | `Expect
  | `From
  | `Host
  | `If_Match
  | `If_Modified_Since
  | `If_None_Match
  | `If_Range
  | `If_Unmodified_Since
  | `Max_Forwards
  | `Proxy_Authorization
  | `ReqRange
  | `Referer
  | `TE
  | `User_Agent
  | `Cookie
  | `NewCookie (* inserted by read (only on first connection) *)
  ]


let string_of_request_header = function
  | `Cache_Control -> "Cache-Control"
  | `Connection -> "Connection"
  | `Date -> "Date"
  | `Pragma -> "Pragma"
  | `Trailer -> "Trailer"
  | `Transfer_Encoding -> "Transfer-Encoding"
  | `Upgrade -> "Upgrade"
  | `Via -> "Via"
  | `Warning -> "Warning"
  | `Allow -> "Allow"
  | `Content_Encoding -> "Content-Encoding"
  | `Content_Language -> "Content-Language"
  | `Content_Length -> "Content-Length"
  | `Content_Location -> "Content-Location"
  | `Content_MD5 -> "Content-MD5"
  | `Content_Range -> "Content-Range"
  | `Content_Type -> "Content-Type"
  | `Content_Disposition -> "Content-Disposition"
  | `Expires -> "Expires"
  | `Last_Modified -> "Last-Modified"
  | `Accept -> "Accept"
  | `Accept_Charset -> "Accept-Charset"
  | `Accept_Encoding -> "Accept-Encoding"
  | `Accept_Language -> "Accept-Language"
  | `Authorization -> "Authorization"
  | `Expect -> "Expect"
  | `From -> "From"
  | `Host -> "Host"
  | `If_Match -> "If-Match"
  | `If_Modified_Since -> "If-Modified-Since"
  | `If_None_Match -> "If-None-Match"
  | `If_Range -> "If-Range"
  | `If_Unmodified_Since -> "If-Unmodified-Since"
  | `Max_Forwards -> "Max-Forwards"
  | `Proxy_Authorization -> "Proxy-Authorization"
  | `ReqRange -> "ReqRange"
  | `Referer -> "Referer"
  | `TE -> "TE"
  | `User_Agent -> "User-Agent"
  | `Cookie -> "Cookie"
  | `NewCookie -> "NewCookie"

let request_header_of_string = function
  | "Cache-Control" -> `Cache_Control
  | "Connection" -> `Connection
  | "Date" -> `Date
  | "Pragma" -> `Pragma
  | "Trailer" -> `Trailer
  | "Transfer-Encoding" -> `Transfer_Encoding
  | "Upgrade" -> `Upgrade
  | "Via" -> `Via
  | "Warning" -> `Warning
  | "Allow" -> `Allow
  | "Content-Encoding" -> `Content_Encoding
  | "Content-Language" -> `Content_Language
  | "Content-Length" -> `Content_Length
  | "Content-Location" -> `Content_Location
  | "Content-MD5" -> `Content_MD5
  | "Content-Range" -> `Content_Range
  | "Content-Type" -> `Content_Type
  | "Content-Disposition" -> `Content_Disposition
  | "Expires" -> `Expires
  | "Last-Modified" -> `Last_Modified
  | "Accept" -> `Accept
  | "Accept-Charset" -> `Accept_Charset
  | "Accept-Encoding" -> `Accept_Encoding
  | "Accept-Language" -> `Accept_Language
  | "Authorization" -> `Authorization
  | "Expect" -> `Expect
  | "From" -> `From
  | "Host" -> `Host
  | "If-Match" -> `If_Match
  | "If-Modified-Since" -> `If_Modified_Since
  | "If-None-Match" -> `If_None_Match
  | "If-Range" -> `If_Range
  | "If-Unmodified-Since" -> `If_Unmodified_Since
  | "Max-Forwards" -> `Max_Forwards
  | "Proxy-Authorization" -> `Proxy_Authorization
  | "ReqRange" -> `ReqRange
  | "Referer" -> `Referer
  | "TE" -> `TE
  | "User-Agent" -> `User_Agent
  | "Cookie" -> `Cookie
  | "NewCookie" -> `NewCookie
  | _ -> raise (Parsing "request_header")

let request_header_of_string_safe s =
  try Some (request_header_of_string s) with
  | Parsing _ -> None

type response_header =
  [ `Cache_Control
  | `Connection
  | `Date
  | `Pragma
  | `Trailer
  | `Transfer_Encoding
  | `Upgrade
  | `Via
  | `Warning
  | `Allow
  | `Content_Encoding
  | `Content_Language
  | `Content_Length
  | `Content_Location
  | `Content_MD5
  | `Content_Range
  | `Content_Type
  | `Content_Disposition
  | `Expires
  | `Last_Modified
  | `Accept_Ranges
  | `Age
  | `ETag
  | `Location
  | `Proxy_Authenticate
  | `Retry_After
  | `Server
  | `Vary
  | `WWW_Authenticate
  | `Set_Cookie
  | `Set_Cookie_External
  | `Set_Cookie_Internal
  ]

let string_of_response_header = function
  | `Cache_Control -> "Cache-Control"
  | `Connection -> "Connection"
  | `Date -> "Date"
  | `Pragma -> "Pragma"
  | `Trailer -> "Trailer"
  | `Transfer_Encoding -> "Transfer-Encoding"
  | `Upgrade -> "Upgrade"
  | `Via -> "Via"
  | `Warning -> "Warning"
  | `Allow -> "Allow"
  | `Content_Encoding -> "Content-Encoding"
  | `Content_Language -> "Content-Language"
  | `Content_Length -> "Content-Length"
  | `Content_Location -> "Content-Location"
  | `Content_MD5 -> "Content-MD5"
  | `Content_Range -> "Content-Range"
  | `Content_Type -> "Content-Type"
  | `Content_Disposition -> "Content-Disposition"
  | `Expires -> "Expires"
  | `Last_Modified -> "Last-Modified"
  | `Accept_Ranges -> "Accept-Ranges"
  | `Age -> "Age"
  | `ETag -> "ETag"
  | `Location -> "Location"
  | `Proxy_Authenticate -> "Proxy-Authenticate"
  | `Retry_After -> "Retry-After"
  | `Server -> "Server"
  | `Vary -> "Vary"
  | `WWW_Authenticate -> "WWW-Authenticate"
  | `Set_Cookie -> "Set-Cookie"
  | `Set_Cookie_External -> "Set-Cookie"
  | `Set_Cookie_Internal -> "Set-Cookie"

let response_header_of_string = function
  | "Cache-Control" -> `Cache_Control
  | "Connection" -> `Connection
  | "Date" -> `Date
  | "Pragma" -> `Pragma
  | "Trailer" -> `Trailer
  | "Transfer-Encoding" -> `Transfer_Encoding
  | "Upgrade" -> `Upgrade
  | "Via" -> `Via
  | "Warning" -> `Warning
  | "Allow" -> `Allow
  | "Content-Encoding" -> `Content_Encoding
  | "Content-Language" -> `Content_Language
  | "Content-Length" -> `Content_Length
  | "Content-Location" -> `Content_Location
  | "Content-MD5" -> `Content_MD5
  | "Content-Range" -> `Content_Range
  | "Content-Type" -> `Content_Type
  | "Content-Disposition" -> `Content_Disposition
  | "Expires" -> `Expires
  | "Last-Modified" -> `Last_Modified
  | "Accept-Ranges" -> `Accept_Ranges
  | "Age" -> `Age
  | "ETag" | "Etag" -> `ETag
  | "Location" -> `Location
  | "Proxy-Authenticate" -> `Proxy_Authenticate
  | "Retry-After" -> `Retry_After
  | "Server" -> `Server
  | "Vary" -> `Vary
  | "WWW-Authenticate" -> `WWW_Authenticate
  | "Set-Cookie" -> `Set_Cookie
  | _ -> raise (Parsing "response_header")

let response_header_of_string_safe s =
  try Some (response_header_of_string s) with
  | Parsing _ -> None

type _method =
  | Options
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Trace
  | Connect
let string_of_method = function
  | Options -> "OPTIONS"
  | Get -> "GET"
  | Head -> "HEAD"
  | Post -> "POST"
  | Put -> "PUT"
  | Delete -> "DELETE"
  | Trace -> "TRACE"
  | Connect -> "CONNECT"
let method_of_string s = match String.uppercase s with
  | "OPTIONS" -> Options
  | "GET" -> Get
  | "HEAD" -> Head
  | "POST" -> Post
  | "PUT" -> Put
  | "DELETE" -> Delete
  | "TRACE" -> Trace
  | "CONNECT" -> Connect
  | _ -> failwith "method_of_string"


(* FIXME: générer automatiquement... type classes ;) *)
module Order_request_header : (Map.OrderedType with type t = request_header) =
struct type t = request_header let compare (a:request_header) b = Pervasives.compare a b end
module Order_response_header : (Map.OrderedType with type t = response_header) =
struct type t = response_header let compare (a:response_header) b = Pervasives.compare a b end

module Value =
struct
  (* FIXME: type 'a parsed = Unparsed of string | Parse of 'a *)
  type value =
    [ `string of string
    | `value of (string * string option) list
    ]
  let sprint_value (a, b) = Printf.sprintf "%s%s" a (match b with Some s -> Printf.sprintf ";%s" s | _ -> "")
  let to_string = function
    | `string s -> s
    | `value [] -> ""
    | `value (hd::tl) ->
        List.fold_left (
          fun acc ab -> Printf.sprintf "%s,%s" acc (sprint_value ab)
        ) (sprint_value hd) tl

(* let first_value = function
   | `string s -> s
   | `value v -> fst (List.hd v) *)
end

module MakeHeader (Order : Map.OrderedType) =
struct
  module HMap = Map.Make (Order)
  include HMap
  type header = Value.value t
  let add_string k v = add k (`string v)
  let get k t = if mem k t then Some (find k t) else None
  let get_string k t = if mem k t then Some (Value.to_string (find k t)) else None
  let remove k t = remove k t
  let to_string f t = (* FIXME: Tune 128... de manière globale sur l'ensemble de l'application... *)
    FBuffer.contents (
      fold (fun k v b -> FBuffer.add b (Printf.sprintf "%s: %s%s" (f k) (Value.to_string v) Base.crlf))
        t (FBuffer.create ~name:"MakeHeader.to_string" 128)
    )
  let keys t =
    HMap.fold (fun k _ acc -> k::acc) t []

  (** parse une ligne de request header *)
  let parse f header l =
    let rec aux header remainder = function
      | [] -> header, remainder
      | ((x, y) as hd)::tl ->
          begin try (* ajoute le champ reconnu par f au header *)
            aux (add (f x) y header) remainder tl
          with (Parsing _) -> (* champ non reconnu par f *)
            aux header (hd::remainder) tl end
    in
    aux header [] l
end

module RequestHeader = MakeHeader (Order_request_header)
module ResponseHeader = MakeHeader (Order_response_header)

(* --------------------- types de webserve ---------------------- *)

type request_line =
  { _method : _method
  ; request_uri : string
  ; http_version : string }

type server_info =
    { server_url : string
    ; server_id : int }

type request =
  { request_line : request_line
  ; request_header : RequestHeader.header
  ; request_message_body : string       (* use FBuffer ? *)
  ; server_info : server_info option }

type status =
  | SC_Continue
  | SC_SwitchingProtocols
  | SC_OK
  | SC_Created
  | SC_Accepted
  | SC_Non_AuthoritativeInformation
  | SC_NoContent
  | SC_ResetContent
  | SC_PartialContent
  | SC_MultipleChoices
  | SC_MovedPermanently
  | SC_Found
  | SC_SeeOther
  | SC_NotModified
  | SC_UseProxy
  | SC_TemporaryRedirect
  | SC_BadRequest
  | SC_Unauthorized
  | SC_PaymentRequired
  | SC_Forbidden of string option
  | SC_NotFound
  | SC_MethodNotAllowed
  | SC_NotAcceptable
  | SC_ProxyAuthenticationRequired
  | SC_RequestTime_out
  | SC_Conflict
  | SC_Gone
  | SC_LengthRequired
  | SC_PreconditionFailed
  | SC_RequestEntityTooLarge
  | SC_Request_URITooLarge
  | SC_UnsupportedMediaType
  | SC_RequestedRangeNotSatisfiable
  | SC_ExpectationFailed
  | SC_InternalServerError
  | SC_NotImplemented
  | SC_BadGateway
  | SC_ServiceUnavailable
  | SC_GatewayTime_out
  | SC_HTTPVersionNotSupported
  (* | extension-code, extension-code = 3DIGIT *)

let status_code = function
  | SC_Continue -> 100
  | SC_SwitchingProtocols -> 101
  | SC_OK -> 200
  | SC_Created -> 201
  | SC_Accepted -> 202
  | SC_Non_AuthoritativeInformation -> 203
  | SC_NoContent -> 204
  | SC_ResetContent -> 205
  | SC_PartialContent -> 206
  | SC_MultipleChoices -> 300
  | SC_MovedPermanently -> 301
  | SC_Found -> 302
  | SC_SeeOther -> 303
  | SC_NotModified -> 304
  | SC_UseProxy -> 305
  | SC_TemporaryRedirect -> 307
  | SC_BadRequest -> 400
  | SC_Unauthorized -> 401
  | SC_PaymentRequired -> 402
  | SC_Forbidden _ -> 403
  | SC_NotFound -> 404
  | SC_MethodNotAllowed -> 405
  | SC_NotAcceptable -> 406
  | SC_ProxyAuthenticationRequired -> 407
  | SC_RequestTime_out -> 408
  | SC_Conflict -> 409
  | SC_Gone -> 410
  | SC_LengthRequired -> 411
  | SC_PreconditionFailed -> 412
  | SC_RequestEntityTooLarge -> 413
  | SC_Request_URITooLarge -> 414
  | SC_UnsupportedMediaType -> 415
  | SC_RequestedRangeNotSatisfiable -> 416
  | SC_ExpectationFailed -> 417
  | SC_InternalServerError -> 500
  | SC_NotImplemented -> 501
  | SC_BadGateway -> 502
  | SC_ServiceUnavailable -> 503
  | SC_GatewayTime_out -> 504
  | SC_HTTPVersionNotSupported -> 505

let reason_phrase = function (* *<TEXT, excluding CR, LF> *)
  | 100 -> "Continue"
  | 101 -> "Switching Protocols"
  | 200 -> "OK"
  | 201 -> "Created"
  | 202 -> "Accepted"
  | 203 -> "Non-Authoritative Information"
  | 204 -> "No Content"
  | 205 -> "Reset Content"
  | 206 -> "Partial Content"
  | 300 -> "Multiple Choices"
  | 301 -> "Moved Permanently"
  | 302 -> "Found"
  | 303 -> "See Other"
  | 304 -> "Not Modified"
  | 305 -> "Use Proxy"
  | 307 -> "Temporary Redirect"
  | 400 -> "Bad Request"
  | 401 -> "Unauthorized"
  | 402 -> "Payment Required"
  | 403 -> "Forbidden"
  | 404 -> "Not Found"
  | 405 -> "Method Not Allowed"
  | 406 -> "Not Acceptable  "
  | 407 -> "Proxy Authentication Required"
  | 408 -> "Request Time-out"
  | 409 -> "Conflict"
  | 410 -> "Gone"
  | 411 -> "Length Required"
  | 412 -> "Precondition Failed"
  | 413 -> "Request Entity Too Large"
  | 414 -> "Request-URI Too Large"
  | 415 -> "Unsupported Media Type"
  | 416 -> "Requested range not satisfiable"
  | 417 -> "Expectation Failed"
  | 500 -> "Internal Server Error"
  | 501 -> "Not Implemented"
  | 502 -> "Bad Gateway"
  | 503 -> "Service Unavailable"
  | 504 -> "Gateway Time-out"
  | 505 -> "HTTP Version not supported"
  | _ -> failwith "reason_phrase"

(** V2: WIP *)

type req_body =
  | Buffer of string
  | TmpFile of string

type request2 =
  { request_line2 : request_line
  ; request_header2 : RequestHeader.header
  ; request_message_body2 : req_body
  ; server_info2 : server_info option }
