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
(** HttpTools:

    Just some support routines for handling HTTP requests and responses.

*)

val http : NetAddr.protocol

(** Possible exceptions *)

exception CallbackAbort

(** Timing functions. *)

val rpt : int -> ('a -> 'b) -> 'a -> unit
val timefn : int -> ('a -> 'b) -> 'a -> unit
val verifyfn : ('a -> 'b) -> ('a -> string) -> ('b -> string) -> ('a * 'b) list -> bool

(** Basic string functions *)

val strcom : string -> int -> string * string option
val rmldtrsp0 : string -> int -> string
val rmldtrsp : string -> string
val rmldtrsp2 : string * string -> string * string
val rmldtrspl : string list -> string list

(** String based support for raw parser *)

val pos_mark : ('a -> int -> 'b) -> 'a -> int -> 'a -> int -> int -> (int * int) option
val upto_mark : string -> int -> string -> int -> int -> int * int * string
val upto_mark_ci : string -> int -> string -> int -> int -> int * int * string

(** Streaming parsers *)

val buf_clean : (Buffer.t * int ref) -> unit

val upto_mark_stream : ('a -> char) -> 'a -> string -> string

val get_char_cps : ('a -> ('b * string -> 'c) -> 'c) -> string ref * int ref * 'a -> (char -> 'c) -> 'c

val upto_mark_stream_cps : ?inclusive:bool -> Buffer.t -> ('a -> (char -> 'b) -> 'b) -> 'a -> string -> (string -> 'b) -> 'b
val upto_mark_stream_cps2 :
  ?inclusive:bool -> (*?oc_opt:out_channel option ->*) Scheduler.t -> Scheduler.connection_info -> (Buffer.t * int ref) -> string ->
  ?callback:('a -> int -> Buffer.t -> bool) -> 'a -> ?blocksize:int ->
  ?err_cont:(exn -> unit) -> ?timeout:Time.t -> (Buffer.t * int * int -> unit) -> unit
val upto_mark_stream_cps3 :
  ?inclusive:bool -> (*?oc_opt:out_channel option ->*) Scheduler.t -> Scheduler.connection_info -> (Buffer.t * int ref) -> string ->
  ?callback:('a -> int -> Buffer.t -> bool) -> 'a -> ?blocksize:int ->
  ?err_cont:(exn -> unit) -> ?timeout:Time.t -> (string -> unit) -> unit

val upto_stream_cps :
  ?inclusive:bool ->
  Buffer.t ->
  ('a -> ('b * string -> 'c) -> 'c) ->
  string ref * int ref * 'a -> string -> (string -> 'c) -> 'c

val read_upto_stream_cps :
  ?inclusive:bool ->
  Buffer.t ->
  string ref * int ref * Scheduler.connection_info ->
  string ->
  Scheduler.t ->
  ?err_cont:(exn -> unit) -> ?timeout:Time.t -> (string -> unit) -> unit

val fixed_stream_cps :
  Buffer.t ->
  ('a -> ('b * string -> 'c) -> 'c) ->
  string ref * int ref * 'a -> int -> (string -> 'c) -> 'c

val read_fixed_stream_cps :
  Buffer.t ->
  string ref * int ref * Scheduler.connection_info ->
  int ->
  Scheduler.t ->
  ?err_cont:(exn -> unit) -> ?timeout:Time.t -> (string -> unit) -> unit

val fixed_stream_cps2_buf :
  (*?oc_opt:out_channel option ->*) Scheduler.t -> Scheduler.connection_info -> (Buf.t * int ref) -> int ->
  ?callback:('a -> int -> Buf.t -> bool) -> 'a -> ?blocksize:int ->
  ?err_cont:(exn -> unit) -> ?timeout:Time.t -> (Buf.t * int * int -> unit) -> unit
val fixed_stream_cps2 :
  (*?oc_opt:out_channel option ->*) Scheduler.t -> Scheduler.connection_info -> (Buffer.t * int ref) -> int ->
  ?callback:('a -> int -> Buffer.t -> bool) -> 'a -> ?blocksize:int ->
  ?err_cont:(exn -> unit) -> ?timeout:Time.t -> (Buffer.t * int * int -> unit) -> unit
val fixed_stream_cps3 :
  (*?oc_opt:out_channel option ->*) Scheduler.t -> Scheduler.connection_info -> (Buffer.t * int ref) -> int ->
  ?callback:('a -> int -> Buffer.t -> bool) -> 'a -> ?blocksize:int ->
  ?err_cont:(exn -> unit) -> ?timeout:Time.t -> (string -> unit) -> unit

val upto : string -> ('a -> ('b * string -> 'c) -> 'c) -> string ref * int ref * 'a -> (string -> 'c) -> 'c

(** putback string onto protocol input *)

val putback : string -> string ref * int ref * 'a -> unit
val putback2 : string -> Buffer.t * int ref -> unit

(** Fast-forward functions *)

val skip_sptab : string -> int -> int -> int
val skip_lws : string -> int -> int -> int

(** More streaming functions *)

val upto_mark_lws : string -> int -> string -> int -> int -> int * int * string
val upto_mark_lws_ci : string -> int -> string -> int -> int -> int * int * string

(** Overflow from rcontent *)

(** content_compress gzip deflate compression_level cache_response content content_len:
    Compress file content according to gzip/deflate flags (deflate has priority).
    Does not read file in if file content, writes from/to disc.
*)
val content_compress : Scheduler.t -> bool -> bool -> int -> bool -> Rcontent.content -> int ->
  ((bool * Rcontent.content) -> unit) -> unit

(** Simple buffer management *)
val get_buf : ?hint:int -> unit -> Buffer.t
val free_buf : Buffer.t -> unit

(** SSL certificate generation *)

val make_ssl_cert : string -> string -> string -> SslAS.ssl_certificate option
val make_ssl_verify : string -> string -> string -> string -> (Ssl.certificate -> bool) -> bool -> SslAS.ssl_verify_params option
