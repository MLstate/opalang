/*
    Copyright © 2011, 2012, 2013 MLstate

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

type Crypto.salted = {
  data : binary;
  salt : binary;
}

/**
 * Type of a running hash computation.
 */
type Crypto.hash = external

Crypto = {{

  /**
   * Returns a cryptographically strong pseudo-random binary data.
   * @param length The length of the binary data to generate
   * @return A cryptographically strong pseudo-random binary data.
   */
  random(length) = %%BslCrypto.secure_random%%(length)

  /**
   * Adds [data] to the running hash computation.
   * @param hash The hash to update.
   * @param data
   */
  add(hash, data) = %%BslCrypto.add%%(hash, data)

  /**
   * Terminates the hash computation and returns the digest.
   */
  digest(hash) = %%BslCrypto.digest%%(hash)

  /**
   * This module supports the encoding and decoding of
   * binary data in base 64 format, using only alphanumeric
   * characters that can safely be transmitted over e-mail or
   * in URLs.
   *
   * Based on OCaml Cryptokit interface.
   */
  Base64 = {{

    /**
     * Returns a base 64 encoded string.
     * The output is divided in lines of length 76 characters,
     * and final [=] characters are used to pad the output,
     * as specified in the MIME standard.
     * The output is approximately [4/3] longer than the input.
     */
    encode_multiline =  %% BslCrypto.base64_encode_multiline %% : binary -> string

    /**
     * Same as {!Crypto.Base64.encode_multiline}, but the output is not
     * split into lines, and no final padding is added.
     * This is adequate for encoding short strings for
     * transmission as part of URLs, for instance.
     */
    encode_compact =  %% BslCrypto.base64_encode_compact %% : binary -> string

    /**
     * Same as {!Crypto.Base64.encode_compact}, but the output is
     * padded with [=] characters at the end (if necessary).
     */
    encode =  %% BslCrypto.base64_encode %% : binary -> string

    /**
     * Returns a base 64 decoded binary.
     * The input must consist of valid base 64 string;
     * blanks are ignored.
     */
    decode =  %% BslCrypto.base64_decode %% : string -> binary

  }}

  /**
   * The [HMAC] module implements keyed-hash functions (see the RFC2104).
   */
  HMAC = {{

    /**
     * A generic HMAC digest of the message [m].
     * @param H is the name of cryptographic hash function
     * @param K is a secret key
     * @param m is a message
     * @return the digest of [m]
     */
    @private
    digest(H, K, m) = %% BslCrypto.hmac_digest %%(H, K, m)

    /**
     * Calculates the HMAC-MD5 digest of [data] with the secret [key].
     * @param key is a secret key
     * @param data is a message
     * @return the calculated digest
     */
    md5(key, data) = digest("md5", key, data)

    /**
     * Calculates the HMAC-SHA1 digest of [data] with the secret [key].
     * @param key is a secret key
     * @param data is a message
     * @return the calculated digest
     */
    sha1(key, data) = digest("sha1", key, data)

    /**
     * Calculates the HMAC-SHA256 digest of [data] with the secret [key].
     * @param key is a secret key
     * @param data is a message
     * @return the calculated digest
     */
    sha256(key, data) = digest("sha256", key, data)

    /**
     * Calculates the HMAC-SHA512 digest of [data] with the secret [key].
     * @param key is a secret key
     * @param data is a message
     * @return the calculated digest
     */
    sha512(key, data) = digest("sha512", key, data)

    /**
     * Calculates the HMAC-SHA1 digest of [data] with the secret [key].
     * @param key is a secret key
     * @param data is a message
     * @return the calculated digest
     */
    ripemd160(key, data) = digest("ripemd160", key, data)

    /**
     * Creates a HMAC computation
     * @param H is the name of cryptographic hash function
     * @param K is a secret key
     * @return A new hash computation
     */
    @private
    create(H, K) = %% BslCrypto.create_hmac %%(H, K)

    /**
     * Creates a new HMAC-MD5 computation.
     * @param key is a secret key
     * @return A new computation
     */
    create_md5(key) = create("md5", key)

    /**
     * Creates a new HMAC-SHA1 computation.
     * @param key is a secret key
     * @return A new computation
     */
    create_sha1(key) = create("sha1", key)

    /**
     * Creates a new HMAC-SHA256 computation.
     * @param key is a secret key
     * @return A new computation
     */
    create_sha256(key) = create("sha256", key)

    /**
     * Creates a new HMAC-SHA512 computation.
     * @param key is a secret key
     * @return A new computation
     */
    create_sha512(key) = create("sha512", key)

    /**
     * Creates a new HMAC-RIPEM160 computation.
     * @param key is a secret key
     * @return A new computation
     */
    create_ripemd160(key) = create("ripemd160", key)

  }}

  /**
   * The [Hash] module implements (unkeyed) hash functions.
   */
  Hash = {{

    /**
     * A generic digest of the message [m].
     * @param H is the name of cryptographic hash function
     * @param m is a message
     * @return the digest of [m]
     */
    @private
    digest(H, m) = %% BslCrypto.hash_digest %%(H, m)

    /**
     * Compute the MD5 signature of a string.
     *
     * @param data A text of arbitrary length.
     * @return A 32 digit long hexadecimal string
     */
    md5(data) = %%BslCrypto.md5%%(data)

    /**
     * Calculates the MD5 digest of [data].
     * @param data is a message
     * @return the calculated digest
     */
    md5_bin(data) = digest("md5", data)

    /**
     * Calculates the SHA1 digest of [data].
     * @param data is a message
     * @return the calculated digest
     */
    sha1(data) = digest("sha1", data)

    /**
     * Calculates the SHA256 digest of [data].
     * @param data is a message
     * @return the calculated digest
     */
    sha256(data) = digest("sha256", data)

    /**
     * Calculates the SHA512 digest of [data].
     * @param data is a message
     * @return the calculated digest
     */
    sha512(data) = digest("sha512", data)

    /**
     * Calculates the HMAC-RIPEM160 digest of [data].
     * @param data is a message
     * @return the calculated digest
     */
    ripemd160(data) = digest("ripemd160", data)

    /**
     * Creates a Hash computation
     * @param H is the name of cryptographic hash function
     * @return A new hash computation
     */
    @private
    create(H) = %% BslCrypto.create_hash %%(H)

    /**
     * Creates a new MD5 computation.
     * @return A new computation
     */
    create_md5() = create("md5")

    /**
     * Creates a new SHA1 computation.
     * @return A new computation
     */
    create_sha1() = create("sha1")

    /**
     * Creates a new SHA256 computation.
     * @return A new computation
     */
    create_sha256() = create("sha256")

    /**
     * Creates a new SHA512 computation.
     * @return A new computation
     */
    create_sha512() = create("sha512")

    /**
     * Creates a new RIPEM160 computation.
     * @return A new computation
     */
    create_ripemd160() = create("ripemd160")

  }}

  Salt = {{

    /**
     * A generic salt + hash function.
     */
    @private salt0(data, salt, hash):Crypto.salted =
      bin = Binary.create(Binary.length(data) + Binary.length(salt))
      do Binary.add_binary(bin, data)
      do Binary.add_binary(bin, salt)
      {~salt data = hash(bin)}

    /**
     * A generic salt + hash function.
     */
    @private salt(data, length, hash):Crypto.salted =
      salt0(data, Crypto.random(length), hash)

    /**
     * Returns a salted and md5 hashed binary data from [data]
     * @param data The data to salt and hash
     * @param length The length of the random salt
     */
    md5(data, length) = salt(data, length, Hash.md5_bin)

    /**
     * Returns a salted and sha1 hashed binary data from [data]
     * @param data The data to salt and hash
     * @param length The length of the random salt
     */
    sha1(data, length) = salt(data, length, Hash.sha1)

    /**
     * Returns a salted and sha256 hashed binary data from [data]
     * @param data The data to salt and hash
     * @param length The length of the random salt
     */
    sha256(data, length) = salt(data, length, Hash.sha256)

    /**
     * Returns a salted and sha512 hashed binary data from [data]
     * @param data The data to salt and hash
     * @param length The length of the random salt
     */
    sha512(data, length) = salt(data, length, Hash.sha512)

    /**
     * Returns a salted and ripemd160 hashed binary data from [data]
     * @param data The data to salt and hash
     * @param length The length of the random salt
     */
    ripemd160(data, length) = salt(data, length, Hash.ripemd160)

    /**
     * A generic check function.
     */
    @private check(data, salted, hash):bool =
      result = salt0(data, salted.salt, hash)
      Binary.equals(result.data, salted.data)

    /**
     * Checks if the given [data] matches with the [salted] data, using the md5
     * hash algorithm.
     * @param data The data to check
     * @param salted The salted data used for comparison.
     * @return A boolean that indicates if [data] matches with the [salted] data
     */
    check_md5(data, salted) = check(data, salted, Hash.md5_bin)

    /**
     * Checks if the given [data] matches with the [salted] data, using the sha1
     * hash algorithm.
     * @param data The data to check
     * @param salted The salted data used for comparison.
     * @return A boolean that indicates if [data] matches with the [salted] data
     */
    check_sha1(data, salted) = check(data, salted, Hash.sha1)

    /**
     * Checks if the given [data] matches with the [salted] data, using the sha256
     * hash algorithm.
     * @param data The data to check
     * @param salted The salted data used for comparison.
     * @return A boolean that indicates if [data] matches with the [salted] data
     */
    check_sha256(data, salted) = check(data, salted, Hash.sha256)

    /**
     * Checks if the given [data] matches with the [salted] data, using the sha512
     * hash algorithm.
     * @param data The data to check
     * @param salted The salted data used for comparison.
     * @return A boolean that indicates if [data] matches with the [salted] data
     */
    check_sha512(data, salted) = check(data, salted, Hash.sha512)

    /**
     * Checks if the given [data] matches with the [salted] data, using the ripemd160
     * hash algorithm.
     * @param data The data to check
     * @param salted The salted data used for comparison.
     * @return A boolean that indicates if [data] matches with the [salted] data
     */
    check_ripemd160(data, salted) = check(data, salted, Hash.ripemd160)

  }}

  RSA = {{

    /**
     * Generate a new RSA key of size [size].
     */
    new_key = %% BslCrypto.rsa_new_key %% : int -> Crypto.RSA.key

    /**
     * Encrypt a binary data with a certain RSA key.
     */
    encrypt = %% BslCrypto.rsa_encrypt %% : Crypto.RSA.key, binary -> option(binary)

    /**
     * Decrypt an RSA encrypted binary message.
     * /!\ You might need to trim the result in order to retrieve the origin message.
     */
    decrypt = %% BslCrypto.rsa_decrypt %% : Crypto.RSA.key, binary -> option(binary)

  }}

}}
