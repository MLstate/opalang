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

##register sha2 : string -> string
let sha2 s =
  let hashobj = Cryptokit.Hash.sha256 () in
  begin
    hashobj#add_string s;
    hashobj#result
  end
