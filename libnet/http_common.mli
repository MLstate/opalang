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
(*
   @author Frederic Ye
**)

(**)

(** Weblib version *)
val version : string

(** {6 Request}
    @see "requestdef.ml" text *)

(** Utility functions: self explanatory *)
val content_length : Requestdef.request -> int
val string_of_request_line : Requestdef.request_line -> string
val string_of_request : Requestdef.request -> string
val print_request_header : Requestdef.request option -> unit

(** {6 Response} *)

(** An HTTP status line: e.g. "HTTP/0.9 302 Found" *)
type status_line = {
  status_http_version : string;
  status : Requestdef.status;
}

val string_of_status_line : status_line -> string

(** The type of a response body, it can be :
    - a string (whole response)
    - a partial result, with a continuation to call when this partial result has been send
    the continuation takes the number of bytes already sent
    - an authentication request before doing the normal behaviour *)
type res_body =
    Result of string
  | PartialResult of int * string * (int -> res_body)
  | AuthenticationRequest of res_body

(** An HTTP response *)
type response = {
  status_line : status_line;
  response_header : Requestdef.ResponseHeader.header;
  response_message_body : res_body;
}

(** Does not include the body *)
val string_of_response_header : response -> string
val print_response_header : response -> unit

(** Include the body *)
val string_of_response : ?body_limit:int -> response -> string
