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
(** Encodings:
    Some string transformation functions, mostly to do with HTTP/HTML/etc.
*)

(** A hand optimised decode for "%30" -> "0" etc. *)
val http_unencode : string -> string

(** Generic string encoding functions. *)
val encode_chars : ?hint:(int -> int) -> (char -> string) -> string -> string
val encode_list_to_map : (char * string) list -> string array
val encode_uri_component : string -> string
val decode_uri_component : string -> string
val encode_aws_uri : string -> string
val http_encode : string -> string
val revert_http_encode : string -> string

(** Hand-optimised HTTP rewrite.
    Extracts "<name> = <value> <&> ..." pairs from strings with HTTP decode (%uAAAA -> etc.)
*)
exception HttpBodyRewriteError of int * string
val http_body_rewrite : string -> int * (string * string) list

(* End of file encodings.mli *)

