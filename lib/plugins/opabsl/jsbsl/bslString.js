/*
    Copyright © 2011-2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Low-level string operations
 * @side{both}
 *
 * To access these operations at OPA-level, use module [String].
 */

//////////////////////////////////////////////////
// CHARACTER /////////////////////////////////////
//////////////////////////////////////////////////

/**
 * @register {string, int -> int} char_at
 */
function char_at(str, i){
  return str.charCodeAt(i);
}

/**
 * @register {int -> string} of_char String.fromCharCode
 */

/**
 * @register {list(int) -> string} of_chars
 */
function of_chars(l){
    return String.fromCharCode.apply(null, list2js(l));
}

/**
 * @register {string, string -> string} concat
 * @pure
 */
function string_concat(a, b) {
    return a + b;
}

/**
 * Replace all occurrences of a substring in a given string
 *
 * @param search
 * @param replacement
 * @param source
 * @register {string, string, string -> string} replace
 */
function string_replace(search, replacement, source) {
  var left= "", right= source, pos;
  while (true)
  {
    pos= right.indexOf(search);
    if (pos < 0) return left+right;
    left += right.substr(0, pos)+replacement;
    right= right.substr(pos + search.length);
  }
}

/**
 * @register {string, int -> string}
 * @pure
 */
function repeat(s, n) {
    for (var str= "", i= 0; i < n; i++)
        str += s ;
    return str;
  }

/**
 * @register {string, int -> string} get
 * @pure
 */
function string_get(s, i) {
    return s[i];
  }

/**
 * @register {string -> string}
 * @pure
 */
function reverse(s) {
    return s.split('').reverse().join('');
  }

/**
 * @register {string -> int} length
 * @pure
 */
function string_length(s) {
    return s.length;
  }

/**
 * @register {int, int, string -> string} sub
 * @pure
 */
function string_sub(offset, len, src) {
    return src.substr(offset, len);
  }

/**
 * @register {string, string -> opa[option(int)]} index
 * @pure
 */
function string_index(pattern, source) {
    var pos = source.indexOf(pattern);
    return (pos > -1) ? js_some(pos) : js_none;
  }


/**
 * @register {int -> string} of_int
 * @pure
 */
function string_of_int(n) {
  return ""+n;
}

/**
 * @register {int -> string}
 * @pure
 */
function to_character(i) {
    /* [String.fromCharCode] doesn't support characters beyond 65535.
     * The following workaround comes from:
     * https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Global_Objects/String/fromCharCode
     */

    if (i > 0xFFFF) {
      i -= 0x10000;
      return String.fromCharCode(0xD800 + (i >> 10), 0xDC00 +
                                 (i & 0x3FF));
    }
    else {
      return String.fromCharCode(i);
    }
  }

   // special function for TRX
/**
 * @register {string, int, string -> bool}
 * @pure
 */
function check_match_literal(input, pos, literal) {
    var i = 0;
    for (i = 0; i < literal.length; i++)
        if (input[pos+i] != literal[i])
            return false;
    return true;
  }


function string_escaped(s){ return s; } // what's wrong ?
function string_encode(str){ return escape(str); }
function string_decode(str){ return unescape(str); }


/**
 * @register {string, string -> bool} leq
 * @pure
 */
function string_leq(c1,c2) {
   return c1 <= c2
}

/**
 * @register {string, string -> bool} lt
 * @pure
 */
function string_lt(c1,c2) {
   return c1 < c2
}

/**
 * @register {string, string -> bool} eq
 * @pure
 */
function string_eq(c1,c2) {
   return c1 == c2
}

/**
 * @register {string, string -> bool} geq
 * @pure
 */
function string_geq(c1,c2) {
   return c1 >= c2
}

/**
 * @register {string, string -> bool} gt
 * @pure
 */
function string_gt(c1,c2) {
   return c1 > c2
}

/**
 * @register {string, string -> bool} neq
 * @pure
 */
function string_neq(c1,c2) {
   return c1 != c2
}

/**
 * @register {string,string -> opa[Order.ordering]} ordering
 * @pure
 */
function string_ordering(c1,c2) {
    if(c1<c2) return result_lt
    if(c1==c2) return result_eq
    return result_gt
}

/**
 * @register {string -> string} encode_uri_component encodeURIComponent
 * @pure
 */
/**
 * @register {string -> string} decode_uri_component decodeURIComponent
 * @pure
 */
