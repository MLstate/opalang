/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * @side{both}
 */

// We should really use "const" here but it's not cross-browser compatible.
// Is it ok to use const in server-side only code?
// What about function myconst() { return "const"; }
var POW2_32 = 0x0000000100000000; //  2^32
var IMIN32 = -0x0000000100000000; // -2^32
var IMAX32 =  0x00000000ffffffff; //  2^32-1
var POW2_52 = 0x0010000000000000; //  2^52
var IMIN52 = -0x0010000000000000; // -2^52
var IMAX52 =  0x000fffffffffffff; //  2^52-1
var POW2_53 = 0x0020000000000000; //  2^53
var IMIN53 = -0x0020000000000000; // -2^53
var IMAX53 =  0x001fffffffffffff; //  2^53-1
var qNaNstr_be = "\x7f\xc0\x00\x00\x00\x00\x00\x00"; // quiet NaN with no payload
var qNaNstr_le = "\x00\x00\x00\x00\x00\x00\xc0\x7f";
function qNaNstr(le) { return (le) ? qNaNstr_le : qNaNstr_be; }
var qNaNarray_be = [0x7f, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]; // quiet NaN with no payload as an array
var qNaNarray_le = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x7f];
function qNaNarray(le) { return (le) ? qNaNarray_le : qNaNarray_be; }

/** @module Int */

// Note: for bitwise ops you only get 32 bits - and they're sign extended
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
              return parseInt(str,16);
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
        var res = int_of_string(str);
        if (isNaN(res)) {
            return js_none;
        } else {
            return js_some(res);
        }
    } catch(e) {
        return js_none;
    }
}

/**
 * @register {float -> int} of_float
 * @pure
 */
function int_of_float(a) {
    if (a<0)
      return Math.ceil(a); // because Math.floor(-3.1) = 4 and not 3
    else
      return Math.floor(a);
  }

// For bitwise you get: floats -> 32-bit ints -> op -> 32-bit ints -> float
// If you try bitwise ops outside of the 32-bit range the results just don't make sense, or are imprecise
// I think it would be safer to just return NaN for values outside this range (or undefined?)

/**
 * @register {int, int -> int} op_land
 * @pure
 */
function int_op_land(n, m) {
  if (n < IMIN32 || n > IMAX32 || m < IMIN32 || m > IMAX32) return NaN;
  return n & m;
}

/**
 * @register {int, int -> int} op_lor
 * @pure
 */
function int_op_lor(n, m) {
  if (n < IMIN32 || n > IMAX32 || m < IMIN32 || m > IMAX32) return NaN;
  return n | m;
}

/**
 * @register {int, int -> int} op_lxor
 * @pure
 */
function int_op_lxor(n, m) {
  if (n < IMIN32 || n > IMAX32 || m < IMIN32 || m > IMAX32) return NaN;
  return n ^ m;
}

/**
 * @register {int -> int} op_lnot
 * @pure
 */
function int_op_lnot(n) {
  if (n < IMIN32 || n > IMAX32) return NaN;
  return ~n;
}

/**
 * @register {int, int -> int} op_lsl
 * @pure
 */
function int_op_lsl(n, m) {
  if (m >= 32 || n < IMIN32 || n > IMAX32) return NaN;
  return n << m;
}

/**
 * @register {int, int -> int} op_lsr
 * @pure
 */
function int_op_lsr(n, m) {
  if (m >= 32) return 0;
  if (n < IMIN32 || n > IMAX32) return NaN;
  return n >>> m;
}

/**
 * @register {int, int -> int} op_asr
 * @pure
 */
function int_op_asr(n, m) {
  if (n < IMIN32 || n > IMAX32) return NaN;
  if (m >= 32) return (n & 0x80000000) ? -1 : 0;
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

function i64_of_int_signed(i)
{
  var msk = 0;
  if (i >= 0 && i <= IMAX32) { return {h:0, l:i}; };
  if (i < IMIN53 || i > IMAX53) { return NAN(); };
  if (i < 0) { i = 0x20000000000000 + i; msk = 0xffe00000};
  var h = Math.floor(i / POW2_32);
  var l = i % POW2_32;
  return {h:h+msk, l:l};
}

function i64_to_int_signed(i64)
{
  if (is_nan(i64)) return NaN;
  var sign = land(i64.h,0xffe00000);
  if (sign != 0) {
    if (sign != 0xffe00000) throw "Int64.to_int_signed: int64 too big for signed int";
    return -(0x20000000000000 - (((i64.h & 0x001fffff) * 0x100000000) + i64.l));
  } else
    return (i64.h * 0x100000000) + i64.l;
}

/** @module BslInt64 */
// Note these are *unsigned* int64 routines.

/* Explanation:  Why do we have all these strange routines here?
 * Basically because JS ints are 53 bits in accuracy but that
 * bitwise operations are restricted to 32-bit *sign-extended*
 * ints.  That is, ~0 === -1.  Since we implement int64 as:
 * {h:<high 32 bits>, l:<low 32 bits>} we can't tolerate any of
 * the sign extension which goes on, we need {h:0x80000000, ...}
 * and not {h:-0x80000000, ...}.  To prevent JS from sign extending
 * we have to do our bitwise ops as:
 * if (op x) has bit 31 set then ((op x) resulting in bit 31 not set)+0x80000000.
 * This is greatly inconvenient and slow but seems to work.
 */

// These get passed around an awful lot...
// ... so we'll prevent them from being accidentally modified.
function NAN() { return {h:NaN, l:NaN}; }
function ZERO() { return {h:0, l:0}; }
function ONE() { return {h:0, l:1}; }
function INFINITY() { return {h:Infinity, l:Infinity}; }
var MAX_INT = {h:0xffffffff, l:0xffffffff};

function is_nan(i) { return isNaN(i.h) || isNaN(i.l); }
function is_zero(i) { return i.h === 0 && i.l === 0; }
function is_one(i) { return i.h === 0 && i.l === 1; }
function is_finite(i) { return isFinite(i.h) && isFinite(i.l); }

// For debugging...
/**
 * @register {( -> 'a) -> opa[outcome('a,string)]} catch
 */
function bslnumber_catch(f) {
  try {
    return {success:f()};
  } catch (err) {
    return {failure:err};
  }
}

/**
 * @register {int64} max_int MAX_INT
 */

/**
 * @register {int64 -> bool} is_NaN
 */
function int64_is_NaN(i) {
  return is_nan(i);
}

/**
 * @register {int -> int64} of_int
 * @pure
 */
function int64_of_int(i) {
  if (i >= 0 && i <= IMAX32) { return {h:0, l:i}; };
  if (i < 0 || i < IMIN53 || i > IMAX53) { return NAN(); };
  var h = Math.floor(i / POW2_32);
  var l = i % POW2_32;
  return {h:h, l:l};
}

/**
 * @register {int -> int64} of_int_signed i64_of_int_signed
 */

/**
 * @register {int64 -> int} to_int
 */
function int64_to_int(i64) {
  if (is_nan(i64)) return NaN;
  if (i64.h > 0x1fffff) throw "Int64.to_int: int64 too big for int";
  return (i64.h * 0x100000000) + i64.l;
}

/**
 * @register {int64 -> int} to_int_signed i64_to_int_signed
 */

/**
 * @register {int64 -> option(int)} to_int_signed_opt
 */
function int64_to_int_signed_opt(i64){
  var i = i64_to_int_signed(i64)
  if(isNaN(i)) return null;
  else return i;
}

/**
 * @register {int64 -> int64} succ
 */
function int64_succ(i) {
  if (i.l === 0xffffffff)
    if (i.h === 0xffffffff)
      throw "Int64.succ: overflow";
    else
      return {h:i.h+1, l:0};
  else
    return {h:i.h, l:i.l+1};
}

/**
 * @register {int64 -> int64} pred
 */
function int64_pred(i) {
  if (i.l === 0)
    if (i.h === 0)
      throw "Int64.pred: underflow";
    else
      return {h:i.h-1, l:0xffffffff};
  else
    return {h:i.h, l:i.l-1};
}

/**
 * @register {int64, int64 -> int64} add
 */
function int64_add(i1,i2) {
  return adde(i1, i2, "Int64.add: overflow");
}

function adde(i1, i2, err)
{
  if (is_nan(i1) || is_nan(i2)) return NAN();
  if (is_zero(i1)) return i2;
  if (is_zero(i2)) return i1;
  var ll = i1.l + i2.l;
  var c = 0;
  if (ll > 0xffffffff) { c = 1; ll -= 0x100000000; };
  var hh = i1.h + i2.h + c;
  if (hh > 0xffffffff) throw err;
  return {h:hh, l:ll};
}

/**
 * @register {int64, int64 -> int64} sub
 */
function int64_sub(i1,i2) {
  if (is_nan(i1) || is_nan(i2)) return NAN();
  if (is_zero(i2)) return i1;
  var ll = i1.l - i2.l;
  var b = 0;
  if (ll < 0) { b = 1; ll += 0x100000000; };
  var hh = i1.h - i2.h - b;
  if (hh < 0) throw "Int64.sub: underflow";
  return {h:hh, l:ll};
}

function mul16n(i32hh, i32hl, i32lh, i32ll, i16, n, err) {
  var tmp = i32ll * i16;
  var ll = tmp & 0xffff;
  tmp = i32lh * i16 + (tmp >>> 16);
  var lh = tmp & 0xffff;
  tmp = i32hl * i16 + (tmp >>> 16);
  var hl = tmp & 0xffff;
  var hh = i32hh * i16 + (tmp >>> 16);
  switch (n) {
  case 0:
    if (hh > 0xffff) throw err;
    return {h:hh*0x10000+hl, l:lh*0x10000+ll};
  case 16:
    if (hh > 0 || hl > 0xffff) throw err;
    return {h:hl*0x10000+lh, l:ll*0x10000};
  case 32:
    if (hh > 0 || hl > 0 || lh > 0xffff) throw err;
    return {h:lh*0x10000+ll, l:0};
  case 48:
    if (hh > 0 || hl > 0 || lh > 0 || ll > 0xffff) throw err;
    return {h:ll*0x10000, l:0};
  default:
    throw "Int64.mul: bad shift";
  }
}

function mule(i1, i2, err) {
  if (is_nan(i1) || is_nan(i2)) return NAN();
  if (is_zero(i1) || is_zero(i2)) return ZERO();
  if (is_one(i1)) return i2;
  if (is_one(i2)) return i1;
  var i1hh = i1.h >>> 16, i1hl = i1.h & 0xffff, i1lh = i1.l >>> 16, i1ll = i1.l & 0xffff;
  var i2hh = i2.h >>> 16, i2hl = i2.h & 0xffff, i2lh = i2.l >>> 16, i2ll = i2.l & 0xffff;
  var a = mul16n(i1hh, i1hl, i1lh, i1ll, i2ll,  0, err);
  var b = mul16n(i1hh, i1hl, i1lh, i1ll, i2lh, 16, err);
  var c = mul16n(i1hh, i1hl, i1lh, i1ll, i2hl, 32, err);
  var d = mul16n(i1hh, i1hl, i1lh, i1ll, i2hh, 48, err);
  function add(x,y) { return adde(x,y,err); }
  return add(add(a,b), add(c,d));
}

/**
 * @register {int64, int64 -> int64} mul
 */
function int64_mul(i1,i2) {
  return mule(i1,i2,"Int64.mul: overflow");
}

var btab = [0x00000001, 0x00000002, 0x00000004, 0x00000008,
            0x00000010, 0x00000020, 0x00000040, 0x00000080,
            0x00000100, 0x00000200, 0x00000400, 0x00000800,
            0x00001000, 0x00002000, 0x00004000, 0x00008000,
            0x00010000, 0x00020000, 0x00040000, 0x00080000,
            0x00100000, 0x00200000, 0x00400000, 0x00800000,
            0x01000000, 0x02000000, 0x04000000, 0x08000000,
            0x10000000, 0x20000000, 0x40000000, 0x80000000];

function setb(x, msk) {
  if (msk === 0x80000000) {
    if (x & 0x80000000) return x;
    return x + 0x80000000; // <-- this seems to be the only way of preventing JS from sign-extending bit 31
  };
  if (x & 0x80000000) {
    return ((x & 0x7fffffff) | msk) + 0x80000000; // <-- !!!!! you've got to be kidding
  };
  return x | msk;
}

function shftleft(x) {
  if (x & 0x40000000) return ((x & 0xbfffffff) << 1) + 0x80000000; // <-- and again
  return x << 1;
}

function setbit(v, bn) {
  if (bn > 63) return v;
  if (bn > 31)
    return {h:setb(v.h,btab[bn-32]), l:v.l};
  else
    return {h:v.h, l:setb(v.l,btab[bn])};
}

function shft128(x) {
  var y = [0,0,0,0];
  y[0] = shftleft(x[0]);
  if (x[1] & 0x80000000) y[0] = setb(y[0],1);
  y[1] = shftleft(x[1]);
  if (x[2] & 0x80000000) y[1] = setb(y[1],1);
  y[2] = shftleft(x[2]);
  if (x[3] & 0x80000000) y[2] = setb(y[2],1);
  y[3] = shftleft(x[3]);
  return y;
}

function sub128(i1, i2)
{
  var l3 = i1[3] - i2[3];
  var b = 0;
  if (l3 < 0) { b = 1; l3 += 0x100000000; };
  var l2 = i1[2] - i2[2] - b;
  b = 0;
  if (l2 < 0) { b = 1; l2 += 0x100000000; };
  var l1 = i1[1] - i2[1] - b;
  b = 0;
  if (l1 < 0) { b = 1; l1 += 0x100000000; };
  var l0 = i1[0] - i2[0] - b;
  if (l0 < 0) throw "Int64.sub128: underflow";
  return [l0,l1,l2,l3];
}

// Naive, slow but accurate.
// Try using 53-bit division and get it to divide 64-bits without sign-extending or truncating the results.
function qr64(N, D) {
  if (is_nan(N) || is_nan(D)) return {q:NAN(), r:NAN()};
  if (is_zero(D)) return {q:INFINITY(), r:NAN()};
  if (is_one(D)) return {q:N, r:ZERO()};
  if (BslNumber_BslInt64_op_gt(D,N)) return {q:ZERO(), r:N};
  var P = [0,0,N.h,N.l];
  var D = [D.h,D.l,0,0];
  var Q = ZERO();
  for (var i = 63; i >= 0; i--) {
    P = shft128(P);
    if ((P[0] === D[0]) ? ((P[1] === D[1]) ? ((P[2] === D[2]) ? P[3] >= D[3] : P[2] >= D[2]) : P[1] >= D[1]) : P[0] >= D[0]) {
      Q = setbit(Q, i);
      P = sub128(P, D);
    };
  }
  return {q:Q, r:{h:P[0], l:P[1]}};
}

/**
 * @register {int64, int64 -> int64} div
 */
function int64_div(N, D) {
  return qr64(N, D).q;
}

/**
 * @register {int64, int64 -> int64} rem
 */
function int64_rem(N, D) {
  return qr64(N, D).r;
}

/**
 * @register {int64, int64 -> bool} op_eq
 */
function int64_op_eq(i1,i2) {
  return (i1.h === i2.h) && (i1.l === i2.l);
}

/**
 * @register {int64, int64 -> bool} op_ne
 */
function int64_op_ne(i1,i2) {
  return (i1.h !== i2.h) || (i1.l !== i2.l);
}

/**
 * @register {int64, int64 -> bool}
 */
function op_gt(i1,i2) {
  return (i1.h === i2.h) ? i1.l > i2.l : i1.h > i2.h;
}

/**
 * @register {int64, int64 -> bool}
 */
function op_ge(i1,i2) {
  return (i1.h === i2.h) ? i1.l >= i2.l : i1.h >= i2.h;
}

/**
 * @register {int64, int64 -> bool}
 */
function op_lt(i1,i2) {
  return (i1.h === i2.h) ? i1.l < i2.l : i1.h < i2.h;
}

/**
 * @register {int64, int64 -> bool}
 */
function op_le(i1,i2) {
  return (i1.h === i2.h) ? i1.l <= i2.l : i1.h <= i2.h;
}

/**
 * @register {int64, int -> string}
 */
function to_string_radix(i, radix) {
  if (radix < 2 || radix > 16) throw ("Int64.to_string: bad radix "+radix);
  if (is_nan(i)) return "NaN";
  if (is_zero(i)) return "0";
  if (is_one(i)) return "1";
  if (!is_finite(i)) return "Infinity";
  var strs = "0123456789abcdef";
  var s = [], R={h:0, l:radix};
  while (!is_zero(i)) {
    qr = qr64(i, R);
    if (qr.r.l >= radix) throw "Int64.to_string: bad number";
    s.push(strs[qr.r.l]);
    i = qr.q;
  }
  return s.reverse().join("");
}

/**
 * @register {int64 -> string} to_string
 */
function int64_to_string(i) {
  return BslNumber_BslInt64_to_string_radix(i, 10);
}

/**
 * @register {string, int -> int64}
 */
function of_string_radix(s, radix) {
  var tab = { '0': 0, '1': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, '8': 8, '9': 9,
              'a': 10, 'b': 11, 'c': 12, 'd': 13, 'e': 14, 'f': 15,
              'A': 10, 'B': 11, 'C': 12, 'D': 13, 'E': 14, 'F': 15 };
  if (radix < 2 || radix > 16) throw ("Int64.of_string: bad radix "+radix);
  var x = ZERO();
  var err = "Int64.of_string: number too large for int64";
  for (var i = 0; i < s.length; i++) {
    var dig = tab[s[i]];
    if (typeof dig == 'undefined' || dig >= radix)
      throw ("Int64.of_string: bad digit "+dig);
    x = adde(mule(x, {h:0, l:radix}, err), {h:0, l:dig}, err);
  }
  return x;
}

/**
 * @register {string -> int64} of_string
 */
function int64_of_string(s) {
  return BslNumber_BslInt64_of_string_radix(s, 10);
}

function land(i1,i2) {
  if ((i1 & 0x80000000) && (i2 & 0x80000000)) {
    return ((i1 & 0x7fffffff) & (i2 & 0x7fffffff)) + 0x80000000;
  }
  return i1 & i2;
}

/**
 * @register {int64, int64 -> int64}
 */
function logand(i1,i2) {
  return {h:land(i1.h,i2.h), l:land(i1.l,i2.l)};
}

function lor(i1,i2) {
  if ((i1 & 0x80000000) || (i2 & 0x80000000)) {
    return ((i1 & 0x7fffffff) | (i2 & 0x7fffffff)) + 0x80000000;
  }
  return i1 | i2;
}

/**
 * @register {int64, int64 -> int64}
 */
function logor(i1,i2) {
  return {h:lor(i1.h,i2.h), l:lor(i1.l,i2.l)};
}

function lxor(i1,i2) {
  if (((i1 & 0x80000000) && !(i2 & 0x80000000)) || (!(i1 & 0x80000000) && (i2 & 0x80000000))) {
    return ((i1 & 0x7fffffff) ^ (i2 & 0x7fffffff)) + 0x80000000;
  }
  return i1 ^ i2;
}

/**
 * @register {int64, int64 -> int64}
 */
function logxor(i1,i2) {
  return {h:lxor(i1.h,i2.h), l:lxor(i1.l,i2.l)};
}

function lnot(i) {
  if (!(i & 0x80000000)) {
    return (~(i | 0x80000000)) + 0x80000000;
  }
  return ~i;
}

/**
 * @register {int64 -> int64}
 */
function lognot(i) {
  return {h:lnot(i.h), l:lnot(i.l)};
}

function lsl(i,n) {
  if (n <= 0) return {h:0, l:i};
  if (n > 63) return ZERO();
  if (n === 32) return {h:i, l:0};
  if (n > 32) {
    n = n - 32;
    if (i & btab[31-n]) {
      return {h:((i & lnot(btab[31-n])) << n) + 0x80000000, l:0};
    }
    return {h:i<<n, l:0};
  }
  if (i & btab[31-n]) {
    return {h:i>>>(32-n), l:((i & lnot(btab[31-n])) << n) + 0x80000000};
  }
  return {h:i>>>(32-n), l:i<<n};
}

/**
 * @register {int64, int -> int64}
 */
function shift_left(i,n) {
  if (n <= 0) return i;
  if (n > 63) return ZERO();
  if (n === 32) return {h:i.l, l:0};
  var s1 = lsl(i.h,n);
  var s2 = lsl(i.l,n);
  return {h:lor(s1.l,s2.h), l:s2.l};
}

function asr_(i, n) {
  if (n === 0) return i;
  if (n >= 32) return (i & 0x80000000) ? 0xffffffff : 0;
  var msks = [0x80000000, 0xc0000000, 0xe0000000, 0xf0000000,
              0xf8000000, 0xfc000000, 0xfe000000, 0xff000000,
              0xff800000, 0xffc00000, 0xffe00000, 0xfff00000,
              0xfff80000, 0xfffc0000, 0xfffe0000, 0xffff0000,
              0xffff8000, 0xffffc000, 0xffffe000, 0xfffff000,
              0xfffff800, 0xfffffc00, 0xfffffe00, 0xffffff00,
              0xffffff80, 0xffffffc0, 0xffffffe0, 0xfffffff0,
              0xfffffff8, 0xfffffffc, 0xfffffffe, 0xffffffff];
  if (i & 0x80000000)
    return (i >>> n) + msks[n-1];
  else
    return i >>> n;
}

function asr(i,n) {
  if (!(i & 0x80000000)) return lsr(i, n);
  if (n <= 0) return {h:i, l:0};
  if (n > 63) return (i & 0x80000000) ? MAX_INT : ZERO();
  if (n === 32) return {h:(i & 0x80000000) ? 0xffffffff : 0, l:i};
  if (n > 32) return {h:(i & 0x80000000) ? 0xffffffff : 0, l:asr_(i,n-32)};
  if (i & btab[n-1]) {
    return {h:asr_(i,n), l:((i & lnot(btab[n-1])) << (32-n)) + 0x80000000};
  }
  return {h:asr_(i,n), l:i<<(32-n)};
}

/**
 * @register {int64, int -> int64}
 */
function shift_right(i,n) {
  if (!(i.h & 0x80000000)) return shift_right_logical_(i,n);
  if (n <= 0) return i;
  if (n > 63) return MAX_INT;
  if (n === 32) return {h:0xffffffff, l:i.h};
  var s1 = asr(i.h,n);
  var s2 = asr(i.l,n);
  return {h:s1.h, l:lor(s1.l,s2.h)};
}

function lsr(i,n) {
  if (n <= 0) return {h:i, l:0};
  if (n > 63) return ZERO();
  if (n === 32) return {h:0, l:i};
  if (n > 32) return {h:0, l:i>>>(n-32)};
  if (i & btab[n-1]) {
    return {h:i>>>n, l:((i & lnot(btab[n-1])) << (32-n)) + 0x80000000};
  }
  return {h:i>>>n, l:i<<(32-n)};
}

/**
 * @register {int64, int -> int64}
 */
function shift_right_logical(i,n) {
  return shift_right_logical_(i,n);
}

function shift_right_logical_(i,n)
{
  if (n <= 0) return i;
  if (n > 63) return ZERO();
  if (n === 32) return {h:0, l:i.h};
  var s1 = lsr(i.h,n);
  var s2 = lsr(i.l,n);
  return {h:s1.h, l:lor(s1.l,s2.h)};
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
 * @register {string -> float} of_string
 * @pure
 */
function float_of_string(v) {
    return parseFloat(v)
  }

/**
 * @register {string -> opa[option(float)]} of_string_opt
 * @pure
 */
function float_of_string_opt(str) {
    try {
        js_some(BslNumber_Float_of_string(str));
    } catch(e) {
        return js_none;
    }
}

  // transforms the string so that it is compatible with the mlbsl
  // (see the comment there)
/**
 * @register {float -> string} to_string
 * @pure
 */
function float_to_string(v) {
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
 * @register {float,float -> opa[Order.comparison]} comparison
 * @pure
 */
function float_comparison(c1,c2) {
    if(isNaN(c1) || isNaN(c2)) return result_neq
    if(c1<c2) return result_lt
    if(c1==c2) return result_eq
    return result_gt
}

/**
 * @register {int -> bool} is_int_NaN  isNaN
 */

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
 * @register {-> void}
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
 * @register {int -> string} base64
 */
function bslnumber_base64(len) {
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
