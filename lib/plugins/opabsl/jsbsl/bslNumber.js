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

##module Int \ bsl_int

##register max_int \ max_int: int
var max_int = Math.pow(2, 53);

##register [pure] of_string : string -> int
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

##register [pure] of_string_opt : string -> opa[option(int)]
    ##args(str)
{
    try {
        js_some(%%BslNumber.Int.of_string%%(str))
    } catch(e) {
        return js_none;
    }
}

##register [pure] of_float : float -> int
  ##args(a)
  {
    if (a<0)
      return Math.ceil(a); // because Math.floor(-3.1) = 4 and not 3
    else
      return Math.floor(a);
  }

##register [pure] op_land : int, int -> int
##args(n, m)
{
    return n & m;
}

##register [pure] op_lor : int, int -> int
##args(n, m)
{
    return n | m;
}

##register [pure] op_lxor : int, int -> int
##args(n, m)
{
    return n ^ m;
}

##register [pure] op_lnot : int -> int
##args(n)
{
    return ~n;
}

##register [pure] op_lsl : int, int -> int
##args(n, m)
{
    return n << m;
}

##register [pure] op_lsr : int, int -> int
##args(n, m)
{
    return n >>> m;
}

##register [pure] op_asr : int, int -> int
##args(n, m)
{
    return n >> m;
}

##register [pure] leq:int, int -> bool
##args(c1,c2)
{
   return c1 <= c2
}

##register [pure] geq:int, int -> bool
##args(c1,c2)
{
   return c1 >= c2
}

##register [pure] ordering: int,int -> opa[Order.ordering]
##args(c1,c2)
{
    if(c1<c2) return result_lt
    if(c1==c2) return result_eq
    return result_gt
}


##endmodule

##module Float \ bsl_float

##register [pure] of_int : int -> float
  ##args(a)
  {
    return a;
  }

##register [pure] of_string : string -> float
  ##args(v)
  {
    return parseFloat(v)
  }

##register [pure] of_string_opt : string -> opa[option(float)]
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
##register [pure] to_string : float -> string
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
##register [pure] to_formatted_string : bool, opa[option(int)], float -> string
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

##register [pure] round    \ `Math.round`   : float -> int
##register [pure] ceil     \ `Math.ceil`    : float -> float
##register [pure] floor    \ `Math.floor`   : float -> float

##register [pure] leq:float, float -> bool
##args(c1,c2)
{
   return c1 <= c2
}

##register [pure] lt:float, float -> bool
##args(c1,c2)
{
   return c1 < c2
}

##register [pure] eq:float, float -> bool
##args(c1,c2)
{
   return c1 == c2
}

##register [pure] geq:float, float -> bool
##args(c1,c2)
{
   return c1 >= c2
}

##register [pure] gt:float, float -> bool
##args(c1,c2)
{
   return c1 > c2
}

##register [pure] neq:float, float -> bool
##args(c1,c2)
{
   return c1 != c2
}

##register [pure] comparison: float,float -> opa[Order.comparison]
##args(c1,c2)
{
    if(isNaN(c1) || isNaN(c2)) return result_neq
    if(c1<c2) return result_lt
    if(c1==c2) return result_eq
    return result_gt
}

##endmodule

##module Math \ bsl_math

##register [pure] sqrt_i : int -> int
##args(n)
{
    return Math.floor(Math.sqrt(n));
}

##register [pure] log     \ `Math.log`     : float -> float
##register [pure] sqrt_f  \ `Math.sqrt`    : float -> float
##register [pure] exp     \ `Math.exp`     : float -> float

##register [pure] abs_i   \ `Math.abs`     : int -> int
##register [pure] abs_f   \ `Math.abs`     : float -> float

##register [pure] ceil    \ `Math.ceil`    : float -> float
##register [pure] floor   \ `Math.floor`   : float -> float

##register [pure] sin     \ `Math.sin`     : float -> float
##register [pure] cos     \ `Math.cos`     : float -> float
##register [pure] tan     \ `Math.tan`     : float -> float

##register [pure] asin    \ `Math.asin`    : float -> float
##register [pure] acos    \ `Math.acos`    : float -> float
##register [pure] atan    \ `Math.atan`    : float -> float

##register [pure] isNaN   \ `isNaN`        : float -> bool

##register [pure] is_infinite : float -> bool
##args(n)
{
    return !(isFinite(n) || isNaN(n));
}

##register [pure] is_normal \ `isFinite` : float -> bool

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
