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

/** @module Int */

/** @register {int} max_int max_int */
var max_int = Math.pow(2, 53);

  /**
   * @register {string -> int} of_string
   * @pure
   */
  function int_of_string(str) {
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

/**
 * @register {string -> opa[option(int)]} of_string_opt
 * @pure
 */
function int_of_string_opt(str) {
    try {
        js_some(BslNumber_Int_of_string(str));
    } catch(e) {
        return js_none;
    }
}

/**
 * @register {float -> int}
 * @pure
 */
function of_float(a) {
    if (a<0)
      return Math.ceil(a); // because Math.floor(-3.1) = 4 and not 3
    else
      return Math.floor(a);
  }

/**
 * @register {int, int -> int}
 * @pure
 */
function op_land(n, m) {
    return n & m;
}

/**
 * @register {int, int -> int}
 * @pure
 */
function op_lor(n, m) {
    return n | m;
}

/**
 * @register {int, int -> int}
 * @pure
 */
function op_lxor(n, m) {
    return n ^ m;
}

/**
 * @register {int -> int}
 * @pure
 */
function op_lnot(n) {
    return ~n;
}

/**
 * @register {int, int -> int}
 * @pure
 */
function op_lsl(n, m) {
    return n << m;
}

/**
 * @register {int, int -> int}
 * @pure
 */
function op_lsr(n, m) {
    return n >>> m;
}

/**
 * @register {int, int -> int}
 * @pure
 */
function op_asr(n, m) {
    return n >> m;
}

/**
 * @register {int, int -> bool} leq
 * @pure
 */
function int_leq(c1,c2) {
   return c1 <= c2
}

/**
 * @register {int, int -> bool} geq
 * @pure
 */
function int_geq(c1,c2) {
   return c1 >= c2
}

/**
 * @register {int,int -> opa[Order.ordering]} ordering
 * @pure
 */
function int_ordering(c1,c2) {
    if(c1<c2) return result_lt
    if(c1==c2) return result_eq
    return result_gt
}


/** @endModule */

/** @module Float */

/**
 * @register {int -> float} of_int
 * @pure
 */
function float_of_int(a) {
    return a;
  }

/**
 * @register {string -> float}
 * @pure
 */
function of_string(v) {
    return parseFloat(v)
  }

/**
 * @register {string -> opa[option(float)]}
 * @pure
 */
function of_string_opt(str) {
    try {
        js_some(BslNumber_Float_of_string(str));
    } catch(e) {
        return js_none;
    }
}

  // transforms the string so that it is compatible with the mlbsl
  // (see the comment there)
/**
 * @register {float -> string}
 * @pure
 */
function to_string(v) {
    var str = ""+v;
    if (str.indexOf('.') >= 0 || str.indexOf('e') >= 0 || str[0] == 'N' || str[0] == 'I' || str[1] == 'I') {
        return str; //Printing corresponds to server-side printing
    } else {
        return str + ".0";//Printing needs to be adjusted
    }
}

  // should also be compatible with mlbsl
/**
 * @register {bool, opa[option(int)], float -> string}
 * @pure
 */
function to_formatted_string(always_dot,decimals_option,f) {
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

/**
 * @register {float -> int} round    Math.round
 * @pure
 */
/**
 * @register {float -> float} ceil     Math.ceil
 * @pure
 */
/**
 * @register {float -> float} floor    Math.floor
 * @pure
 */

/**
 * @register {float, float -> bool} leq
 * @pure
 */
function float_leq(c1,c2) {
   return c1 <= c2
}

/**
 * @register {float, float -> bool} lt
 * @pure
 */
function float_lt(c1,c2) {
   return c1 < c2
}

/**
 * @register {float, float -> bool} eq
 * @pure
 */
function float_eq(c1,c2) {
   return c1 == c2
}

/**
 * @register {float, float -> bool} geq
 * @pure
 */
function float_geq(c1,c2) {
   return c1 >= c2
}

/**
 * @register {float, float -> bool} gt
 * @pure
 */
function float_gt(c1,c2) {
   return c1 > c2
}

/**
 * @register {float, float -> bool} neq
 * @pure
 */
function float_neq(c1,c2) {
   return c1 != c2
}

/**
 * @register {float,float -> opa[Order.comparison]}
 * @pure
 */
function comparison(c1,c2) {
    if(isNaN(c1) || isNaN(c2)) return result_neq
    if(c1<c2) return result_lt
    if(c1==c2) return result_eq
    return result_gt
}

/** @endModule */

/** @module Math */

/**
 * @register {int -> int}
 * @pure
 */
function sqrt_i(n) {
    return Math.floor(Math.sqrt(n));
}

/**
 * @register {float -> float} log     Math.log
 * @pure
 */
/**
 * @register {float -> float} sqrt_f  Math.sqrt
 * @pure
 */
/**
 * @register {float -> float} exp     Math.exp
 * @pure
 */

/**
 * @register {int -> int} abs_i   Math.abs
 * @pure
 */
/**
 * @register {float -> float} abs_f   Math.abs
 * @pure
 */

/**
 * @register {float -> float} ceil    Math.ceil
 * @pure
 */
/**
 * @register {float -> float} floor   Math.floor
 * @pure
 */

/**
 * @register {float -> float} sin     Math.sin
 * @pure
 */
/**
 * @register {float -> float} cos     Math.cos
 * @pure
 */
/**
 * @register {float -> float} tan     Math.tan
 * @pure
 */

/**
 * @register {float -> float} asin    Math.asin
 * @pure
 */
/**
 * @register {float -> float} acos    Math.acos
 * @pure
 */
/**
 * @register {float -> float} atan    Math.atan
 * @pure
 */

/**
 * @register {float -> bool} isNaN   isNaN
 * @pure
 */

/**
 * @register {float -> bool}
 * @pure
 */
function is_infinite(n) {
    return !(isFinite(n) || isNaN(n));
}

/**
 * @register {float -> bool} is_normal isFinite
 * @pure
 */

/** @endModule */

/** @module Random */
/**
 * @register {int -> int}
 */
function int(n) {
  return Math.floor(Math.random() * n)
}

/**
 * @register {float -> float}
 */
function float(n) {
  return Math.random() * n
}

/**
 * @register { -> void}
 */
function random_init() {
    return ;
}

function makeStringFromChars(chars, len) {
    var s = "";
    for (var i = 0; i < len; ++i)
	s += chars.charAt(Math.floor(Math.random() * chars.length));
    return s;
}

/**
 * @register {string, int -> string}
 */
function generic_string(chars, len) {
    return (makeStringFromChars(chars, len));
}

/**
 * @register {int -> string}
 */
function string(len) {
    var chars = "abcdefghijklmnopqrstuvwxyz";
    return makeStringFromChars(chars, len);
}

/**
 * @register {int -> string}
 */
function base64(len) {
    var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    return makeStringFromChars(chars, len)
}

/**
 * @register {int -> string}
 */
function base64_url(len) {
    var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
    return makeStringFromChars(chars, len)
}

/** @endModule */
