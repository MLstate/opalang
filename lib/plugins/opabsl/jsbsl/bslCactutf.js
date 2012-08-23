/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Client-side implementation of UTF-8 strings.
 */

/**
 * @register {string -> int} length
 */
function cactuft_length(s) {
    return s.length;
}

/**
 * In Javascript the indexation is done with char position, not in byte,
 * so this function is the identity on the client side
**/
/**
 * @register {string, int -> int}
 */
function nth(_, pos) {
    return pos
}

/**
 * Given the index used in Cactutf strings, returns the number of Unicode characters
 * In javascript, the indexation uses directly the number of Unicode characters, this function is the identity
**/
/**
 * @register {string, int -> int}
 */
function length_until(_, pos) {
    return pos
}

/**
 * @register {string, int -> int}
 */
function next(s, i) {
    return i + 1;
}

/**
 * @register {string, int, int -> string} sub
 */
function cactuft_sub(s, i, j) {
        return s.substring(i, i + j);
}


/**
 * @register {string, int, int -> opa[option(string)]}
 */
function sub_opt(s, i, j) {
    if( ( i>= 0) || i+j < s.length) return js_some(s.substring(i, i + j))
    else return js_none
}

/**
 * @register {string, int -> int} get
 */
function cactuft_get(s, i) {
    return s.charCodeAt(i);
}

/**
 * @register {string, int -> int}
 */
function look(s, i) {
    return s.charCodeAt(i);
}

/**
 * @register {int -> string} cons
 */
function cactuft_cons(i) {
    return String.fromCharCode(i);
}

/**
 * @register {string -> string} uppercase
 */
function cactuft_uppercase(s) {
    return s.toUpperCase();
}

/**
 * @register {string -> string} lowercase
 */
function cactuft_lowercase(s) {
    return s.toLowerCase();
}

// All the following are copied from libqml/libqml/cactutf.ml
/**
 * @register {int -> opa[option(int)]}
 * @pure
 */
function lenbytes(i) {
    if (i < 128) {
        return js_some(1);
    } else if (i < 192) {
        return js_none;
    } else if (i < 224) {
        return js_some(2);
    } else if (i < 240) {
        return js_some(3);
    } else
        return js_some(4)
}

/**
 * @register {int -> int}
 * @pure
 */
function one_byte(b1) {
    return b1;
}

/**
 * @register {int, int -> int}
 * @pure
 */
function two_bytes(b1,b2) {
    return (((b1 - 192) * 64) + (b2 - 128));
}

/**
 * @register {int, int, int -> int}
 * @pure
 */
function three_bytes(b1,b2,b3) {
    return (((b1 - 224) * 4096) + ((b2 - 128) * 64) + (b3 - 128));
}

/**
 * @register {int, int, int, int -> int}
 * @pure
 */
function four_bytes(b1,b2,b3,b4) {
    return (((b1 - 240) * 262144) + ((b2 - 128) * 4096) + ((b3 - 128) * 64) + (b4 - 128));
}
