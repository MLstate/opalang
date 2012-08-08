/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Compute the MD5 signature of a string.
 *
 * @param {!string} str A text of arbitrary length.
 * @return {!string} A 32 digit long hexadecimal string
 */
/**
 * @register {string -> string} md5 MD5.digest
 */
//Implementation in md5.js -- files kept separate to simplify legalese

/**
 * Encode a string as Base 64
 *
 * @param {!string} str A text of arbitrary length.
 * @return {!string} A (longer) representation, encoded as base 64
 */
/**
 * @register {string -> string} base64_encode base64.encode
 */
//Implementation in base64.js -- files kept separate to simplify legalese

/**
 * @param {!string} str A base64-encoded text
 * @return {!string} A decoded representation
 */
/**
 * @register {string -> string} base64_decode base64.decode
 */
//Implementation in base64.js -- files kept separate to simplify legalese
