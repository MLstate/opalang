(*
    Copyright © 2011 MLstate

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
module C = Cryptokit

let rng () = C.Random.device_rng "/dev/random"

let from_hex s = (* if s = "" then "" else *) C.transform_string (C.Hexa.decode()) s
let to_hex s = (* if s = "" then "" else *) C.transform_string (C.Hexa.encode()) s

let from_base64 s = C.transform_string (C.Base64.decode()) s
let to_base64 s = C.transform_string (C.Base64.encode_compact()) s
let to_base64_m s = C.transform_string (C.Base64.encode_multiline()) s

let hash s = C.hash_string (C.Hash.sha256()) s

let symetric direction key s =
  let cipher = C.Cipher.aes ~pad:C.Padding._8000 key direction in
  C.transform_string cipher s

let symetric_encrypt = symetric C.Cipher.Encrypt
let symetric_decrypt = symetric C.Cipher.Decrypt

let new_rsa_empty_key () = {
  C.RSA.size = 0;
  C.RSA.n = "";
  C.RSA.e = "";
  C.RSA.d = "";
  C.RSA.p = "";
  C.RSA.q = "";
  C.RSA.dp = "";
  C.RSA.dq = "";
  C.RSA.qinv = "";
}

let print_rsa_key rsa_key =
  print_endline (Printf.sprintf " \n\
let rsa_key = { \n\
RSA.size = %d; \n\
RSA.n = \"%s\"; \n\
RSA.e = \"%s\"; \n\
RSA.d = \"%s\"; \n\
RSA.p = \"%s\"; \n\
RSA.q = \"%s\"; \n\
RSA.dp = \"%s\"; \n\
RSA.dq = \"%s\"; \n\
RSA.qinv = \"%s\"; \n\
}" rsa_key.C.RSA.size (to_hex rsa_key.C.RSA.n) (to_hex rsa_key.C.RSA.e) (to_hex rsa_key.C.RSA.d)
    (to_hex rsa_key.C.RSA.p) (to_hex rsa_key.C.RSA.q) (to_hex rsa_key.C.RSA.dp) (to_hex rsa_key.C.RSA.dq) (to_hex rsa_key.C.RSA.qinv))

let sign rsa_sig_privkey s =
  C.RSA.sign_CRT rsa_sig_privkey s

let unsign rsa_sig_pubkey s =
  C.RSA.unwrap_signature rsa_sig_pubkey s

let random_key () =
  C.Random.string (C.Random.secure_rng) 32

(* Read an RSA private key from a file.
   If the file is password protected, a password will be asked in the console *)
let read_rsa_privkey file = try
  let rsa = Ssl_ext.rsa_read_privkey file in
  Ssl_ext.rsa_key_to_cryptokit_hex_rsa rsa
with Ssl_ext.RSA_error -> failwith "Read RSA private key failure"

(* Read an RSA public key from a file.
   If the file is password protected, a password will be asked in the console *)
let read_rsa_pubkey file = try
  let rsa = Ssl_ext.rsa_read_pubkey file in
  Ssl_ext.rsa_key_to_cryptokit_hex_rsa rsa
with Ssl_ext.RSA_error -> failwith "Read RSA public key failure"
