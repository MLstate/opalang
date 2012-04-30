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

//////////////////////////////////////////////////////////////////////
// BEWARE THIS FILE IS SHARING BEETWEEN THE JAVASCRIPT AND NODE BSL //
//////////////////////////////////////////////////////////////////////

##module Int \ bsl_int

##register of_string : string -> int
  ##args(str)
  {
      if (str.length < 2)
          return parseInt(str,10);
      else {
          var hex = /^0x/;
          var is_hex = str.match(hex);
          if (is_hex != null)
              return parseInt(str,10);
          else {
              var oct = /^0o/;
              var is_oct = str.match(oct);
              if (is_oct != null) {
                  return parseInt(str,8);
                }
              else
                  return parseInt(str,10)
          }
      }
  }

##register of_string_opt : string -> option(int)
    ##args(str)
{
    try {
        js_some(%%BslNumber.Int.of_string%%(str))
    } catch(e) {
        return js_none;
    }
}

##register of_float : float -> int
  ##args(a)
  {
    if (a<0)
      return Math.ceil(a); // because Math.floor(-3.1) = 4 and not 3
    else
      return Math.floor(a);
  }

##register op_land : int, int -> int
##args(n, m)
{
    return n & m;
}

##register op_lor : int, int -> int
##args(n, m)
{
    return n | m;
}

##register op_lxor : int, int -> int
##args(n, m)
{
    return n ^ m;
}

##register op_lnot : int -> int
##args(n)
{
    return ~n;
}

##register op_lsl : int, int -> int
##args(n, m)
{
    return n << m;
}

##register op_lsr : int, int -> int
##args(n, m)
{
    return n >>> m;
}

##register op_asr : int, int -> int
##args(n, m)
{
    return n >> m;
}

##register leq:int, int -> bool
##args(c1,c2)
{
   return c1 <= c2
}

##register geq:int, int -> bool
##args(c1,c2)
{
   return c1 >= c2
}

##register ordering: int,int -> opa[Order.ordering]
##args(c1,c2)
{
    if(c1<c2) return result_lt
    if(c1==c2) return result_eq
    return result_gt
}


##endmodule

##module Float \ bsl_float

##register of_int : int -> float
  ##args(a)
  {
    return a;
  }

##register of_string : string -> float
  ##args(v)
  {
    return parseFloat(v)
  }

##register of_string_opt : string -> option(float)
    ##args(str)
{
    try {
        js_some(%%BslNumber.Float.of_string%%(str))
    } catch(e) {
        return js_none;
    }
}

  // transforms the string so that it is compatible with the mlbsl
  // (see the comment there)
##register to_string : float -> string
  ##args(v)
{
    var str = ""+v;
    if (str.indexOf('.') >= 0 || str.indexOf('e') >= 0 || str[0] == 'N' || str[0] == 'I' || str[1] == 'I') {
        return str; //Printing corresponds to server-side printing
    } else {
        return str + ".0";//Printing needs to be adjusted
    }
}

  // should also be compatible with mlbsl
##register to_formatted_string : bool, option(int), float -> string
  ##args(always_dot,decimals_option,f)
  {
      var str = ""+f;
      if ('none' in decimals_option) {
          if (!always_dot || (str.indexOf('.') >= 0) || str.indexOf('e') >= 0 || str[0] == 'N' || str[0] == 'I' || str[1] == 'I') {
              // either js stringification already doesn't print the dot when not necessary
              // or if there is already one, nothing to do
              // or we have a '1e-88' kind of float and we don't add any point
              // or we have NaN or Infinity or -Infinity, and we can't add a decimal point
              return str;
          } else {
              // we want a dot, and didn't find one in str, so we add it ourselves
              return str + ".0";
          }
      } else {
          if (always_dot || str.indexOf('.') >= 0) {
              // either, we want a dot, we will have it when calling toFixed
              // or we have a real float (there is a dot), keep the dot, but truncate the string
              return f.toFixed(decimals_option.some);
          } else {
              // int or special cases, or scientific notation
              return str;
          }
      }
  }

##register round    \ `Math.round`   : float -> int
##register ceil     \ `Math.ceil`    : float -> float
##register floor    \ `Math.floor`   : float -> float

##register leq:float, float -> bool
##args(c1,c2)
{
   return c1 <= c2
}

##register lt:float, float -> bool
##args(c1,c2)
{
   return c1 < c2
}

##register eq:float, float -> bool
##args(c1,c2)
{
   return c1 == c2
}

##register geq:float, float -> bool
##args(c1,c2)
{
   return c1 >= c2
}

##register gt:float, float -> bool
##args(c1,c2)
{
   return c1 > c2
}

##register neq:float, float -> bool
##args(c1,c2)
{
   return c1 != c2
}

##register comparison: float,float -> opa[Order.comparison]
##args(c1,c2)
{
    if(isNaN(c1) || isNaN(c2)) return result_neq
    if(c1<c2) return result_lt
    if(c1==c2) return result_eq
    return result_gt
}

##endmodule

##module Math \ bsl_math

##register sqrt_i : int -> int
##args(n)
{
    return Math.floor(Math.sqrt(n));
}

##register log     \ `Math.log`     : float -> float
##register sqrt_f  \ `Math.sqrt`    : float -> float
##register exp     \ `Math.exp`     : float -> float

##register abs_i   \ `Math.abs`     : int -> int
##register abs_f   \ `Math.abs`     : float -> float

##register ceil    \ `Math.ceil`    : float -> float
##register floor   \ `Math.floor`   : float -> float

##register sin     \ `Math.sin`     : float -> float
##register cos     \ `Math.cos`     : float -> float
##register tan     \ `Math.tan`     : float -> float

##register asin    \ `Math.asin`    : float -> float
##register acos    \ `Math.acos`    : float -> float
##register atan    \ `Math.atan`    : float -> float

##register isNaN   \ `isNaN`        : float -> bool

##register is_infinite : float -> bool
##args(n)
{
    return !(isFinite(n) || isNaN(n));
}

##register is_normal \ `isFinite` : float -> bool

##endmodule

##module Random
##register int : int -> int
  ##args(n)
  {
    return Math.floor(Math.random() * n)
  }
##register float : float -> float
  ##args(n)
  {
    return Math.random() * n
  }

##register random_init : -> void
  ##args()
  {
      return ;
  }

function makeStringFromChars(chars, len) {
    var s = "";
    for (var i = 0; i < len; ++i)
	s += chars.charAt(Math.floor(Math.random() * chars.length));
    return s;
}

##register generic_string : string, int -> string
    ##args(chars, len)
{
    return (makeStringFromChars(chars, len));
}

##register string : int -> string
    ##args(len)
{
    var chars = "abcdefghijklmnopqrstuvwxyz";
    return makeStringFromChars(chars, len);
}

##register base64 : int -> string
    ##args(len)
{
    var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    return makeStringFromChars(chars, len)
  }

##register base64_url : int -> string
    ##args(len)
{
    var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
    return makeStringFromChars(chars, len)
  }

##endmodule
