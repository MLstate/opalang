/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

//////////////////////////////////////////////////////////////////////
// BEWARE THIS FILE IS SHARING BEETWEEN THE JAVASCRIPT AND NODE BSL //
//////////////////////////////////////////////////////////////////////
##extern-type binary
/**
 * Low-level string operations
 *
 * To access these operations at OPA-level, use module [String].
 */
##register [pure] concat: string, string -> string
##args(a, b)
{
    return a + b;
}

/**
 * Replace all occurrences of a substring in a given string
 *
 * @param search
 * @param replacement
 * @param source
 */
##register replace : string, string, string -> string
  ##args(search, replacement, source)
   {
    var left= "", right= source, pos;
    while (true)
    {
      pos= right.indexOf(search);
      if (pos < 0) return left+right;
      left += right.substr(0, pos)+replacement;
      right= right.substr(pos + search.length);
    }
   }

##register [pure] repeat : string, int -> string
##args(s, n)
{
    for (var str= "", i= 0; i < n; i++)
        str += s ;
    return str;
  }

##register [pure] get : string, int -> string
  ##args(s, i)
  {
    return s[i];
  }

##register [pure] reverse : string -> string
  ##args(s)
  {
    return s.split('').reverse().join('');
  }

##register [pure] length : string -> int
  ##args(s)
  {
    return s.length;
  }

##register [pure] sub : int, int, string -> string
  ##args(offset, len, src)
  {
    return src.substr(offset, len);
  }

##register [pure] index : string, string -> option(int)
  ##args(pattern, source)
  {
    var pos = source.indexOf(pattern);
    return (pos > -1) ? js_some(pos) : js_none;
  }

##register [pure] uppercase : string -> string
  ##args(str)
  {
    return str.toUpperCase();
  }

##register [pure] lowercase : string -> string
##args(str)
{
    return str.toLowerCase();
}

##register remove_accents: string -> string
##args(str)
{
    var result = str.replace(/[ÀÁÂÃÄÅ]/g, "A")
        .replace(/[àáâãäå]/g, "a")
        .replace(/[ÒÓÔÕÖØ]/g, "O")
        .replace(/[òóôõöø]/g, "o")
        .replace(/[ÈÉÊË]/g,   "E")
        .replace(/[èéêë]/g,   "e")
        .replace(/Ç/g,        "C")
        .replace(/ç/g,        "c")
        .replace(/ÌÍÎÏ/g,     "I")
        .replace(/ìíîï/g,     "i")
        .replace(/ÙÚÛÜ/g,     "u")
        .replace(/ùúûü/g,     "u")
        .replace(/ÿ/g,        "y")
        .replace(/Ñ/g,        "N")
        .replace(/ñ/g,        "n")
    return result;
}

##register escapeHTML : bool, string -> string
  ##args(_,someText)
  {
    var div = document.createElement('div');
    var text = document.createTextNode(someText);
    div.appendChild(text);
    return div.innerHTML;
  }

##register [pure] of_int : int -> string
  ##args(n)
  {
    return ""+n;
  }

##register [pure] to_character : int -> string
  ##args(i)
  {
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

##register [pure] of_byte_unsafe : int -> string
##args(i)
{
  return String.fromCharCode(i);
}

##register [pure] of_byte_val : int -> string
##args(i)
{
    return %% BslString.of_byte_unsafe %%(i)
}

##register [pure] byte_at_unsafe : int, string -> int
  ##args(n, s)
  {
    return s.charCodeAt(n);
  }

   // special function for TRX
##register [pure] check_match_literal : string, int, string -> bool
  ##args(input, pos, literal)
  {
    var i = 0;
    for (i = 0; i < literal.length; i++)
        if (input[pos+i] != literal[i])
            return false;
    return true;
  }


function string_escaped(s){ return s; } // what's wrong ?
function string_encode(str){ return escape(str); }
function string_decode(str){ return unescape(str); }


##register [pure] leq:string, string -> bool
##args(c1,c2)
{
   return c1 <= c2
}

##register [pure] lt:string, string -> bool
##args(c1,c2)
{
   return c1 < c2
}

##register [pure] eq:string, string -> bool
##args(c1,c2)
{
   return c1 == c2
}

##register [pure] geq:string, string -> bool
##args(c1,c2)
{
   return c1 >= c2
}

##register [pure] gt:string, string -> bool
##args(c1,c2)
{
   return c1 > c2
}

##register [pure] neq:string, string -> bool
##args(c1,c2)
{
   return c1 != c2
}

##register [pure] ordering: string,string -> opa[Order.ordering]
##args(c1,c2)
{
    if(c1<c2) return result_lt
    if(c1==c2) return result_eq
    return result_gt
}

##register [pure] encode_uri_component\ encodeURIComponent: string -> string
##register [pure] decode_uri_component\ decodeURIComponent: string -> string

