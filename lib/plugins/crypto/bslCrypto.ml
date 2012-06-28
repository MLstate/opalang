(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

exception BslCrypto of string

##register md5 : string -> string
let md5 = (fun x -> Digest.to_hex (Digest.string x))

##register base64_encode : string -> string
let base64_encode str =
  BaseString.base64encode str

##register base64_encode_compact : string -> string
let base64_encode_compact str =
  Cryptokit.transform_string (Cryptokit.Base64.encode_compact ()) str

##register base64_encode_multiline : string -> string
let base64_encode_multiline str =
  Cryptokit.transform_string (Cryptokit.Base64.encode_multiline ()) str

##register base64_decode : string -> string
let base64_decode str =
  BaseString.base64decode str

##register base64_decode2 : string -> string
let base64_decode2 str =
   Cryptokit.transform_string (Cryptokit.Base64.decode ()) str

##register hmac_sha1 : string, string -> string
let hmac_sha1 key text =
  Cryptokit.hash_string (Cryptokit.MAC.hmac_sha1 key) text

##register hmac_sha256 : string, string -> string
let hmac_sha256 key text =
  Cryptokit.hash_string (Cryptokit.MAC.hmac_sha256 key) text

##register hmac : string, string, string, string -> string
let hmac algo encoding key text =
  let hash =
    match algo with
    | "md5" -> Cryptokit.MAC.hmac_md5 key
    | "sha1" -> Cryptokit.MAC.hmac_sha1 key
    | "sha256" -> Cryptokit.MAC.hmac_sha256 key
    | "ripemd160" -> Cryptokit.MAC.hmac_ripemd160 key
    | _ -> raise (BslCrypto ("Unknown algorithm "^algo))
  in
  let str = Cryptokit.hash_string hash text in
  match encoding with
  | "binary" -> str
  | "hex" -> BaseString.to_hex(str)
  | "base64" -> BaseString.base64encode(str)
  | _ -> raise (BslCrypto ("Unknown output encoding"^encoding))

##register sha2 : string -> string
let sha2 s =
  let hashobj = Cryptokit.Hash.sha256 () in
  begin
    hashobj#add_string s;
    hashobj#result
  end

##register hash : string, string, string -> string
let hash algo encoding s =
  let hashobj =
    match algo with
    | "md5" -> Cryptokit.Hash.md5 ()
    | "sha1" -> Cryptokit.Hash.sha1 ()
    | "sha256" -> Cryptokit.Hash.sha256 ()
    | "ripemd160" -> Cryptokit.Hash.ripemd160 ()
    | _ -> raise (BslCrypto ("Unknown algorithm "^algo))
  in
  begin
    hashobj#add_string s;
    match encoding with
    | "binary" -> hashobj#result
    | "hex" -> BaseString.to_hex(hashobj#result)
    | "base64" -> BaseString.base64encode(hashobj#result)
    | _ -> raise (BslCrypto ("Unknown output encoding"^encoding))
  end


##extern-type Crypto.RSA.key = Cryptokit.RSA.key

##register rsa_new_key : int -> Crypto.RSA.key
let rsa_new_key size = Cryptokit.RSA.new_key ~rng:Cryptokit.Random.secure_rng size

##register rsa_encrypt : Crypto.RSA.key, string -> option(string)
let rsa_encrypt key msg =
  try
    Some (Cryptokit.RSA.encrypt key msg)
  with Cryptokit.Error _ -> None

##register rsa_decrypt : Crypto.RSA.key, string -> option(string)
let rsa_decrypt key msg =
  try
    Some (Cryptokit.RSA.decrypt key msg)
  with Cryptokit.Error _ -> None
