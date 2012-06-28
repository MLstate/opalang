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
  ?more_headers:string list ->
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
  ?more_headers:string list ->
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
