/*
    Copyright © 2011, 2012 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

/**
 * Low-level string operations
 *
 * To access these operations at OPA-level, use module [String].
 */
##register concat: string, string -> string
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

##register repeat : string, int -> string
##args(s, n)
{
    for (var str= "", i= 0; i < n; i++)
        str += s ;
    return str;
  }

##register get : string, int -> string
  ##args(s, i)
  {
    return s[i];
  }

##register reverse : string -> string
  ##args(s)
  {
    return s.split('').reverse().join('');
  }

##register length : string -> int
  ##args(s)
  {
    return s.length;
  }

##register sub : int, int, string -> string
  ##args(offset, len, src)
  {
    return src.substr(offset, len);
  }

##register index : string, string -> option(int)
  ##args(pattern, source)
  {
    var pos = source.indexOf(pattern);
    return (pos > -1) ? js_some(pos) : js_none;
  }

##register uppercase : string -> string
  ##args(str)
  {
    return str.toUpperCase();
  }

##register lowercase : string -> string
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

##register of_int : int -> string
  ##args(n)
  {
    return ""+n;
  }

##register to_character : int -> string
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

##register of_byte_unsafe : int -> string
  ##args(i)
  {
    return String.fromCharCode(i);
  }

##register byte_at_unsafe : int, string -> int
  ##args(n, s)
  {
    return s.charCodeAt(n);
  }

   // special function for TRX
##register check_match_literal : string, int, string -> bool
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


##register leq:string, string -> bool
##args(c1,c2)
{
   return c1 <= c2
}

##register lt:string, string -> bool
##args(c1,c2)
{
   return c1 < c2
}

##register eq:string, string -> bool
##args(c1,c2)
{
   return c1 == c2
}

##register geq:string, string -> bool
##args(c1,c2)
{
   return c1 >= c2
}

##register gt:string, string -> bool
##args(c1,c2)
{
   return c1 > c2
}

##register neq:string, string -> bool
##args(c1,c2)
{
   return c1 != c2
}

##register ordering: string,string -> opa[Order.ordering]
##args(c1,c2)
{
    if(c1<c2) return %%BslPervasives.order_lt%%
    if(c1==c2) return %%BslPervasives.order_eq%%
    return %%BslPervasives.order_gt%%
}

##register encode_uri_component\ encodeURIComponent: string -> string

