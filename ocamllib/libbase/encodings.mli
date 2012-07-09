(*
    Copyright © 2011, 2012 MLstate

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
(** Encodings:
    Some string transformation functions, mostly to do with HTTP/HTML/etc.
*)

(** A hand optimised decode for "%30" -> "0" etc. *)
val http_unencode : string -> string

(** Generic string encoding functions. *)
val encode_chars : ?hint:(int -> int) -> (char -> string) -> string -> string
val pc_encode_string : (char -> bool) -> string -> string

val pc_decode_string : string -> string

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

