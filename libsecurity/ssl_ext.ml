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

open Base
open Ssl
open Cryptokit

(* SSL *)

exception Renegotiation_error
exception Handshake_error of Ssl.ssl_error

let _ =
  Callback.register_exception "ssl_ext_exn_renegotiation_error" Renegotiation_error;
  Callback.register_exception "ssl_ext_exn_handshake_error" (Handshake_error Ssl.Error_none);

external init : unit -> unit = "ocaml_ssl_ext_init"

external set_session_id_context : context -> unit = "ocaml_ssl_ext_ctx_set_session_id_context"

external set_ctx_options : context -> int = "ocaml_ssl_ext_ctx_set_options"

external get_no_client_verify_callback_ptr : unit -> verify_callback = "ocaml_ssl_ext_get_no_client_verify_callback_ptr"

external get_signature : certificate -> string = "ocaml_ssl_ext_get_signature"

external get_hash_ptr : certificate -> string = "ocaml_ssl_ext_get_hash"

external compute_digest_ptr : certificate -> string -> string = "ocaml_ssl_ext_compute_digest"

external renegotiate : socket -> unit = "ocaml_ssl_ext_renegotiate"

external do_handshake : socket -> unit = "ocaml_ssl_ext_do_handshake"

external set_verify : socket -> verify_mode list -> verify_callback option -> unit = "ocaml_ssl_ext_set_verify"

external set_verify_depth : socket -> int -> unit = "ocaml_ssl_ext_set_verify_depth"

external set_accept_state : socket -> unit = "ocaml_ssl_ext_set_accept_state"

external get_verify_mode : socket -> int = "ocaml_ssl_ext_get_verify_mode"

external get_state : socket -> int = "ocaml_ssl_ext_get_state"

external set_state : socket -> int -> unit = "ocaml_ssl_ext_set_state"

external clear : socket -> int = "ocaml_ssl_ext_clear"

external get_num_renegotiations : socket -> int = "ocaml_ssl_ext_num_renegotiations"

external verify_chain : certificate -> string -> int = "ocaml_ssl_ext_verify_chain"

let no_client_verify_callback = get_no_client_verify_callback_ptr ()

let check_chain certificate cafile = verify_chain certificate cafile > 0

(* We return an hexa string for simplicity *)
let get_hash certificate =
  let hash = String.to_hex (get_hash_ptr certificate) in
  let len = String.length hash in
  if len >= 40 (*sha1*) then
    String.sub hash (len-40) 40
  else hash

(* We return an hexa string for simplicity *)
let compute_digest certificate dname dsize =
  let digest = String.to_hex (compute_digest_ptr certificate dname) in
  let len = String.length digest in
  if len >= dsize then
    String.sub digest 0 dsize
  else digest

let certificate_to_string certificate dname dsize =
  let issuer = get_issuer certificate
  and subject = get_subject certificate
  and signature = String.to_hex (get_signature certificate)
  and hash = get_hash certificate
  and digest = compute_digest certificate dname dsize in
  Printf.sprintf "\tissuer=%s\n\tsubject=%s\n\tsignature=%s\n\thash=%s\n\tdigest=%s" issuer subject signature hash digest

(* RSA *)
(* FIXME: do not put in weblib ? *)

exception RSA_error

let _ =
  Callback.register_exception "ssl_ext_exn_rsa_error" RSA_error;


type rsa_key

external rsa_read_privkey : string -> rsa_key = "ocaml_ssl_ext_rsa_read_privkey"
external rsa_read_pubkey : string -> rsa_key = "ocaml_ssl_ext_rsa_read_pubkey"

external rsa_get_size : rsa_key -> int = "ocaml_ssl_ext_rsa_get_size"
external rsa_get_n : rsa_key -> string = "ocaml_ssl_ext_rsa_get_n"
external rsa_get_e : rsa_key -> string = "ocaml_ssl_ext_rsa_get_e"
external rsa_get_d : rsa_key -> string = "ocaml_ssl_ext_rsa_get_d"
external rsa_get_p : rsa_key -> string = "ocaml_ssl_ext_rsa_get_p"
external rsa_get_q : rsa_key -> string = "ocaml_ssl_ext_rsa_get_q"
external rsa_get_dp : rsa_key -> string = "ocaml_ssl_ext_rsa_get_dp"
external rsa_get_dq : rsa_key -> string = "ocaml_ssl_ext_rsa_get_dq"
external rsa_get_qinv : rsa_key -> string = "ocaml_ssl_ext_rsa_get_qinv"

let rsa_key_to_cryptokit_hex_rsa rsa_key = {
  RSA.size = rsa_get_size(rsa_key) * 8; (* the result is in bytes whereas Cryptokit.RSA uses bits *)
  RSA.n = String.from_hex (rsa_get_n(rsa_key));
  RSA.e = String.from_hex (rsa_get_e(rsa_key));
  RSA.d = String.from_hex (rsa_get_d(rsa_key));
  RSA.p = String.from_hex (rsa_get_p(rsa_key));
  RSA.q = String.from_hex (rsa_get_q(rsa_key));
  RSA.dp = String.from_hex (rsa_get_dp(rsa_key));
  RSA.dq = String.from_hex (rsa_get_dq(rsa_key));
  RSA.qinv = String.from_hex (rsa_get_qinv(rsa_key));
}


let error_to_string = function
  | Ssl.Error_none -> "none"
  | Ssl.Error_ssl -> "ssl"
  | Ssl.Error_want_read -> "want_read"
  | Ssl.Error_want_write -> "want_write"
  | Ssl.Error_want_x509_lookup -> "want_x509_lookup"
  | Ssl.Error_syscall -> "syscall"
  | Ssl.Error_zero_return -> "zero_return"
  | Ssl.Error_want_connect -> "want_connect"
  | Ssl.Error_want_accept -> "want_accept"
