(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
(*
   @author Frederic Ye
**)

(**)

(** The HTTP client name (with version) *)
val client_name : string

(** An exception triggered when the client can reach the server but the server has failed to
    deliver a complete and parseable reply after a given number of attempts*)
exception Timeout

(** The HTTP client is HTTP/1.0 compatible only
    Tries a request two times *)

(** GET function
    @param async the asynchronous object (from the connection)
    @param hostname the destination name or ip (warning: sometime
    destination request with host param. set to their ip instead of their name e.g. www.google.fr)
    @param port the destination port
    @param path the destination path (absolute)
    @param client_certificate the client certificate
    @param verify_params the verification parameters
    @param secure use a secure connection (https)
    @param auth UNDOCUMENTED
    @param more_headers concatenate more headers in the request
    @param cont the continuation (fun request_header_map body -> unit)

    @see "Net.ssl_certificate" text
    @see "Net.ssl_verify_params" text
*)
val get :
  Scheduler.t ->
  string ->
  int ->
  string ->
  ?client_certificate:SslAS.ssl_certificate ->
  ?verify_params:SslAS.ssl_verify_params ->
  ?secure:bool ->
  ?auth:string ->
  ?more_headers:string ->
  ?err_cont:(exn -> unit) ->
  ?failure:([ `Cannot_parse_response of string | `Timeout | `Unknown_machine of string ] -> unit) ->
  (Requestdef.Value.value Requestdef.ResponseHeader.t * string -> unit) ->
  unit

(** As [get], but with more options*)
val place_request :
  Scheduler.t ->
  hostname:string ->
  port:int ->
  path:string ->
  ?client_certificate:SslAS.ssl_certificate ->
  ?verify_params:SslAS.ssl_verify_params ->
  ?secure:bool ->
  request_kind:string ->
  ?auth:string ->
  ?more_headers:string ->
  ?data:string ->
  ?client_name:string ->
  ?timeout:Time.t ->
  ?err_cont:(exn -> unit) ->
  success:(int * Requestdef.Value.value Requestdef.ResponseHeader.t * string -> unit) ->
  failure:([`Cannot_parse_response of string | `Unknown_machine of string | `Timeout] -> unit) ->
  unit ->
  unit

(** POST function (uses the get method)
    @param async the asynchronous object (from the connection)
    @param hostname the destination name or ip (warning: sometime
    destination request with host param. set to their ip instead of their name e.g. www.google.fr)
    @param port the destination port
    @param path the destination path (absolute)
    @param client_certificate the client certificate
    @param verify_params the verification parameters
    @param secure use a secure connection (https)
    @param auth UNDOCUMENTED
    @param mime_type the content type of the request
    @param length the length of the content. If -1 then will be computed
    @param data the request body
    @param cont the continuation (fun request_header_map body -> unit)

    @see "Net.ssl_certificate" text
    @see "Net.ssl_verify_params" text
*)
val post :
  Scheduler.t ->
  string ->
  int ->
  string ->
  ?client_certificate:SslAS.ssl_certificate ->
  ?verify_params:SslAS.ssl_verify_params ->
  ?secure:bool ->
  ?auth:string ->
  string ->
  ?length:int ->
  ?err_cont:(exn -> unit) ->
  ?failure:([ `Cannot_parse_response of string | `Timeout | `Unknown_machine of string ] -> unit) ->
  string ->
  (Requestdef.Value.value Requestdef.ResponseHeader.t * string -> unit) ->
  unit
