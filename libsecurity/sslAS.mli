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

exception InvalidCertificate
(** Exception raised when the certificate provided is invalid *)

(** Certificate to provide.
    When an entity (client or server) asks for a certificate,
    provide this certificate.
    @see <http://www.openssl.org/docs/ssl/SSL_CTX_use_certificate.html> for certificate
    The password field is only used if the private key file is password protected,
    and if it's not an empty string
    If it asks for intermediate CAs, give those in certfile then in certpath
    @see <http://www.openssl.org/docs/ssl/SSL_CTX_load_verify_locations.html> for CA *)
type ssl_certificate

(** Certificates verifications rules.
    When an entity (client or server) provides a certificate,
    verify the certificate is valid :
    - cafile checks if the certificate is signed by this ca
    - capath checks if the certificate is signed by one of the ca in the ca path
    - certpath checks if the certificate is contained in the cert path
    - accept_fun the function to call if the certificate is unknown/invalid
    The verifications are made in this order :
    - ca check (see http://www.openssl.org/docs/ssl/SSL_CTX_load_verify_locations.html and http://www.openssl.org/docs/ssl/SSL_load_client_CA_file.html)
    - cert check, if it's in the cert path directory
    - accept_fun if a cert path is defined and the certificate is not in this directory *)
type ssl_verify_params

type secure_type = ssl_certificate option * ssl_verify_params option

type is_valid_cert = bool

type secure_response =
  | UnsecuredRes
  | SecuredRes of is_valid_cert * (Ssl.certificate option * ssl_verify_params option)

(**
   Construct a SSL certificate, i.e. something that will be sent to
   a third party to ensure confidence.

   @param cafile The name of the file containing the server CA certificate
   @param capath The name of a directory containing more CAs
   @param certfile Complete path to the certificate file, in PEM format
   @param privkey The name of the file containing the private key
   @param password The password to use if private key protected
*)
val make_ssl_certificate :
  ?cafile:string ->
  ?capath:string ->
  string -> string -> string ->
  ssl_certificate

(**
   Construct a SSL verifier, i.e. something that will decide whether
   to accept a third-party certificate

   @param client_ca_file A list of CAs sent to the client when requesting a client certificate
   @param accept_fun A fallback function, called when a certificate cannot be checked automatically (e.g. to prompt the user to check the certificate manually)
   @param always Always verify the presence of a certificate
   @param cafile A file containing CA certificates in PEM format, used for verification
   @param capath A directory containing CA certificates in PEM format, used for verification
   @param certpath A directory containing client certificates in PEM format
*)
val make_ssl_verify_params:
  ?client_ca_file:string ->
  ?accept_fun:(Ssl.certificate -> bool) ->
  ?always:bool ->
  string -> string -> string ->
  ssl_verify_params

val get_listen_callback :
  Scheduler.t ->
  secure_type ->
  (secure_response -> Scheduler.connection_info -> unit) ->
  (Scheduler.connection_info -> unit)
(**
   @return a callback to handle a new client over a secure connection.
*)

val connect :
  Scheduler.t ->
  Scheduler.connection_info ->
  ssl_certificate option * ssl_verify_params option ->
  ?err_cont:(exn -> unit) ->
  (Scheduler.connection_info -> unit) ->
  unit
(**
   Secured connect on a socket. Once it is done, your callback is called with a [Scheduler.connection_info] containing a secured socket.
   The default error handler continuation logs any exception as a warning and returns.
*)


(** Renegotiate a connection from the server side,
    basically it does two handshakes again with the client.
    If you need to change the connection options, first call set_verify for example *)
val renegotiate :
  Scheduler.t ->
  Scheduler.connection_info ->
  ?timeout:Time.t ->
  ?retry:int ->
  Ssl.socket ->
  ?err_cont:(exn -> unit) ->
  (unit -> unit) ->
  unit

(** Renegotiate a connection from the client side,
    basically it does one handshake with the server.
    If you need to change the connection options, first call set_verify for example *)
val renegotiate_client :
  Scheduler.t ->
  Scheduler.connection_info ->
  ?timeout:Time.t ->
  ?retry:int ->
  Ssl.socket ->
  ?err_cont:(exn -> unit) ->
  (unit -> unit) ->
  unit

(**  Try to get a valid certificate and verify its validity
     If there are no certificate available, try to renegotiate with the client
     to get one.
     The certificate's validity (boolean) is then passed to the continuation *)
val get_valid_certificate :
  Scheduler.t ->
  Scheduler.connection_info ->
  ?timeout:Time.t ->
  ?retry:int ->
  Ssl.socket ->
  ssl_verify_params ->
  ?err_cont:(exn -> unit) ->
  (bool -> unit) ->
  unit

(** Reload all authorized certificates into the certs ref stringmap.
    By default, only read ".pem" files.
    The certificates must be in PEM format.
    Does not invalidate current connections.
    @return true if everything went OK
    (the failure of some certificate reading is not considered as real errors) *)
val reload_certs :
  ?extensions:string list ->
  ssl_verify_params ->
  bool

(** Compute the fingerprint of a certificate (SHA256) *)
val compute_fingerprint :
  Ssl.certificate -> string
