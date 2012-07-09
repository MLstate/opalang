/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * {1 About this module}
 *
 * This module contains a few encoding functions.
 *
 * {1 Where should I start ?}
 *
 * The documentation of this module is not complete. In particular, it could be nice
 * to get informations about in what condition such encoding are used, by who, for what reason, etc.
 *
 * {1 What if I need more ?}
 *
**/

/**
 * {1 Interface}
 */

import-plugin crypto

type Crypto.RSA.key = external

Crypto = {{

  Base64 = {{

    encode =  %% BslCrypto.base64_encode %% : string -> string

    encode_compact =  %% BslCrypto.base64_encode_compact %% : string -> string

    encode_multiline =  %% BslCrypto.base64_encode_multiline %% : string -> string

    decode =  %% BslCrypto.base64_decode %% : string -> string

    decode2 =  %% BslCrypto.base64_decode2 %% : string -> string

  }}

  Hash = {{

    /**
     * Produces a HMAC_SHA1 for the given key and message. The first argument
     * is the key, the second is the message.
     */
    hmac_sha1 = %% BslCrypto.hmac_sha1 %% : string, string -> string

    /**
     * Produces a HMAC_SHA256 for the given key and message. The first argument
     * is the key, the second is the message.
     */
    hmac_sha256 = %% BslCrypto.hmac_sha256 %% : string, string -> string

    md5 = %% BslCrypto.md5 %% : string -> string

    sha2 = %%BslCrypto.sha2%% : string -> string

  }}

  #<Ifstatic:OPA_BACKEND_QMLJS>
  #<Else>
  RSA = {{

    /**
     * Generate a new RSA key of size [size].
     */
    new_key = %% BslCrypto.rsa_new_key %% : int -> Crypto.RSA.key

    /**
     * Encrypt a string with a certain RSA key.
     */
    encrypt = %% BslCrypto.rsa_encrypt %% : Crypto.RSA.key, string -> option(string)

    /**
     * Decrypt an RSA encrypted message.
     * /!\ You might need to trim the result in order to retrieve the origin message.
     */
    decrypt = %% BslCrypto.rsa_decrypt %% : Crypto.RSA.key, string -> option(string)

  }}
  #<End>

}}

/**
 * {1 Deprecated API}
 *
 * function used to be exported to the global namespace, which is bad
**/

@deprecated({use="Crypto.Base64.encode"}) base64_encode = Crypto.Base64.encode
@deprecated({use="Crypto.Base64.decode"}) base64_decode = Crypto.Base64.decode
@deprecated({use="Crypto.Hash.hmac_sha1"}) hmac_sha1 = Crypto.Hash.hmac_sha1
@deprecated({use="Crypto.Hash.md5"}) md5 = Crypto.Hash.md5
@deprecated({use="Crypto.Hash.md5"}) Hash = Crypto.Hash
