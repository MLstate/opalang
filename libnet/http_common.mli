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
