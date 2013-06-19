/*
    Copyright © 2011-2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Primitives for manipulates binary containers.
 * @side{server}
 * @externType binary
 * @externType int32
 * @externType int64
 * @opaType outcome('a, 'b)
 */

/**
 * @constructor
 */
function Binary(hint) {
  this.contents = new Buffer(hint);
  this.length   = 0;
}

function binary_of_buffer(contents, length){
  return {contents : contents, length : length == null ? contents.length : length};
}

function buffer_of_binary(bin){
  trim(bin);
  return bin.contents;
}

/**
 * @register {int -> binary} create
 */
function binary_create(hint) {
  return new Binary(hint);
}

/**
 * @register {binary -> int} length
 */
function binary_length(b) {
  return b.length;
}

/*
 * We do actually need this, at least for the mongo driver.
 * The routine below is for 7-bit characters *only* it actually
 * gives nonsense values for 8-bit characters (they're always fffd).
 * This routine has been renamed string_of_binary8 to indicate
 * that it correctly handles 8-bit characters ignoring, eg. unicode
 * pairs.  It is much slower than the simpler routine below.
 */
function string_of_binary8(b)
{
  switch (typeof(b)) {
  case 'object': // Assume binary
    var s = "";
    // Slow but guarantees no encoding problems, sign extension etc.
    for (var i = 0; i < b.length; i++) { s += code(b.contents[i]) };
    return s;
  case 'string':
    return b;
  default:
    return undefined;
  };
}

/**
 * @register {string, string -> binary} of_encoding
 */
function binary_of_string(d, e){
  return binary_of_buffer(new Buffer(d, e));
}

/**
 * @register {binary, string -> string} to_encoding
 */
function string_of_binary(d, e){
  return d.contents.toString(e, 0, d.length);
}

/**
 * @register {binary, int -> void}
 */
function resize(b, size) {
  if (b.contents.length == size) return;
  var rb = new Buffer(size);
  var length = (b.length > size) ? size : b.length;
  b.contents.copy(rb, 0, 0, length);
  if (b.length > length) b.length = length;
  b.contents = rb;
}

/**
 * @register {binary, int -> void}
 */
function seek(b, pos) {
    b.length = pos;
    if (b.contents.length <= pos) resize(b, pos);
}

function grow(b) {
  resize(b, (b.contents.length < 8) ? 8 : (b.contents.length * 3) / 2);
}

function expand(b, min) {
  var g = (b.contents.length < min) ? min : b.contents.length;
  var g2 = g >>> 1;
  var size = g + g2;
  if (size < 8) size = 8;
  resize(b,size);
}

/**
 * @register {binary -> void} clear
 */
function binary_clear(b) {
  b.contents = new Buffer(0);
  b.length = 0;
  return js_void;
}

/**
 * @register {binary -> void}
 */
function trim(b) {
  resize(b, b.length);
  return js_void;
}

/**
 * @register {binary -> void}
 */
function reset(b) {
  b.length = 0;
  return js_void;
}

/**
 * @register {binary, string -> void}
 */
function add_string(b, s) {
  var rlen = Buffer.byteLength(s);
  if (b.length + rlen > b.contents.length) expand(b, b.length + rlen);
  var b2 = new Buffer(s);
  b2.copy(b.contents, b.length, 0, rlen);
  b.length += rlen;
  return js_void;
}

/**
 * @register {binary, int -> void}
 */
function add_unicode(b, i) {
    if (i < 0x80) {
    if (b.length + 1 > b.contents.length) expand(b, b.length + 1);
        b.contents.writeUInt8(i, b.length++);
    } else if (i < 0x800) {
        if (b.length + 2 > b.contents.length) expand(b, b.length + 2);
        b.contents.writeUInt8(0xc0 | (i >> 6), b.length++),
        b.contents.writeUInt8(0x80 | (i & 0x3f), b.length++);
    } else if (i < 0xd800 || i >= 0xe000) {
        if (b.length + 3 > b.contents.length) expand(b, b.length + 3);
        b.contents.writeUInt8(0xe0 | (i >> 12), b.length++);
        b.contents.writeUInt8(0x80 | ((i >> 6) & 0x3f), b.length++),
        b.contents.writeUInt8(0x80 | (i & 0x3f), b.length++);
    } else {
        if (b.length + 4 > b.contents.length) expand(b, b.length + 4);
        b.contents.writeUInt8(0xf0 | (i >> 18), b.length++);
        b.contents.writeUInt8(0x80 | ((i >> 12) & 0x3f), b.length++);
        b.contents.writeUInt8(0x80 | ((i >> 6) & 0x3f), b.length++),
        b.contents.writeUInt8(0x80 | (i & 0x3f), b.length++);
    }
}

/**
 * @register {binary, string -> opa[outcome(void,string)]}
 */
function add_stringr(b, s){
  var rlen = Buffer.byteLength(s);
  if (b.length + rlen > b.contents.length) expand(b, b.length + rlen);
  var b2 = new Buffer(s);
  b2.copy(b.contents, b.length, 0, rlen);
  b.length += rlen;
  return {success:js_void};
}

/**
 * @register {binary, int, int -> void}
 */
function add_fill(b, len, v){
  for (i = 0; i < len; i++)
    b.contents.writeInt8(v, b.length+i, true);
  b.length += len;
  return js_void;
}

/**
 * @register {binary, binary -> void}
 */
function add_binary(b, nb) {
  if (b.length + nb.length > b.contents.length) expand(b, b.length + nb.length);
  nb.contents.copy(b.contents,b.length,0,nb.length)
  b.length += nb.length;
  return js_void;
}

/**
 * @register {binary, binary -> opa[outcome(void,string)]}
 */
function add_binaryr(b, nb){
  if (b.length + nb.length > b.contents.length) expand(b, b.length + nb.length);
  nb.contents.copy(b.contents,b.length,0,nb.length)
  b.length += nb.length;
  return {success:js_void};
}

/**
 * @register {binary, int -> void}
 */
function add_int8(b, i) {
  if (i < -0x80 || i > 0x7f) throw("BslBinary.add_int8: out of range int "+i);
  if (b.length >= b.contents.length) grow(b);
  b.contents.writeInt8(i, b.length, true);
  b.length++;
  return js_void;
}

/**
 * @register {binary, int -> opa[outcome(void,string)]}
 */
function add_int8r(b, i) {
  if (i < -0x80 || i > 0x7f) return({failure:"BslBinary.add_int8r: out of range int "+i});
  if (b.length >= b.contents.length) grow(b);
  b.contents.writeInt8(i, b.length, true);
  b.length++;
  return {success:js_void};
}

/**
 * @register {binary, int -> void}
 */
function add_uint8(b, i) {
  if (i < 0 || i > 0xff) throw("BslBinary.add_uint8: out of range int "+i);
  if (b.length >= b.contents.length) grow(b);
  b.contents.writeUInt8(i, b.length, true);
  b.length++;
  return js_void;
}

/**
 * @register {binary, int -> opa[outcome(void,string)]}
 */
function add_uint8r(b, i) {
  if (i < 0 || i > 0xff) return({failure:"BslBinary.add_uint8r: out of range int "+i});
  if (b.length >= b.contents.length) grow(b);
  b.contents.writeUInt8(i, b.length, true);
  b.length++;
  return {success:js_void};
}

/**
 * @register {binary, int -> void}
 */
function add_int16_le(b, i) {
  if (i < -0x8000 || i > 0x7fff) throw("BslBinary.add_int16_le: out of range int "+i);
  if (b.length + 2 >= b.contents.length) grow(b);
  b.contents.writeInt16LE(i, b.length, true);
  b.length += 2;
  return js_void;
}

/**
 * @register {binary, int -> opa[outcome(void,string)]}
 */
function add_int16_ler(b, i) {
  if (i < -0x8000 || i > 0x7fff) return({failure:"BslBinary.add_int16_le: out of range int "+i});
  if (b.length + 2 >= b.contents.length) grow(b);
  b.contents.writeInt16LE(i, b.length, true);
  b.length += 2;
  return {success:js_void};
}

/**
 * @register {binary, int -> void}
 */
function add_uint16_le(b, i) {
  if (i < 0 || i > 0xffff) throw("BslBinary.add_uint16_le: out of range int "+i);
  if (b.length + 2 >= b.contents.length) grow(b);
  b.contents.writeUInt16LE(i, b.length, true);
  b.length += 2;
  return js_void;
}

/**
 * @register {binary, int -> opa[outcome(void,string)]}
 */
function add_uint16_ler(b, i) {
  if (i < 0 || i > 0xffff) return({failure:"BslBinary.add_uint16_le: out of range int "+i});
  if (b.length + 2 >= b.contents.length) grow(b);
  b.contents.writeUInt16LE(i, b.length, true);
  b.length += 2;
  return {success:js_void};
}

/**
 * @register {binary, int -> void}
 */
function add_int16_be(b, i) {
  if (i < -0x8000 || i > 0x7fff) throw("BslBinary.add_int16_be: out of range int "+i);
  if (b.length + 2 >= b.contents.length) grow(b);
  b.contents.writeInt16BE(i, b.length, true);
  b.length += 2;
  return js_void;
}

/**
 * @register {binary, int -> opa[outcome(void,string)]}
 */
function add_int16_ber(b, i) {
  if (i < -0x8000 || i > 0x7fff) return({failure:"BslBinary.add_int16_be: out of range int "+i});
  if (b.length + 2 >= b.contents.length) grow(b);
  b.contents.writeInt16BE(i, b.length, true);
  b.length += 2;
  return {success:js_void};
}

/**
 * @register {binary, int -> void}
 */
function add_uint16_be(b, i) {
  if (i < 0 || i > 0xffff) throw("BslBinary.add_uint16_be: out of range int "+i);
  if (b.length + 2 >= b.contents.length) grow(b);
  b.contents.writeUInt16BE(i, b.length, true);
  b.length += 2;
  return js_void;
}

/**
 * @register {binary, int -> opa[outcome(void,string)]}
 */
function add_uint16_ber(b, i) {
  if (i < 0 || i > 0xffff) return({failure:"BslBinary.add_uint16_be: out of range int "+i});
  if (b.length + 2 >= b.contents.length) grow(b);
  b.contents.writeUInt16BE(i, b.length, true);
  b.length += 2;
  return {success:js_void};
}

/**
 * @register {binary, int -> void}
 */
function add_int32_le(b, i) {
  if (i < -0x80000000 || i > 0x7fffffff) throw("BslBinary.add_int32_le: out of range int "+i);
  if (b.length + 4 >= b.contents.length) grow(b);
  b.contents.writeInt32LE(i, b.length, true);
  b.length += 4;
  return js_void;
}

/**
 * @register {binary, int -> opa[outcome(void,string)]}
 */
function add_int32_ler(b, i) {
  if (i < -0x80000000 || i > 0x7fffffff) return({failure:"BslBinary.add_int32_le: out of range int "+i});
  if (b.length + 4 >= b.contents.length) grow(b);
  b.contents.writeInt32LE(i, b.length, true);
  b.length += 4;
  return {success:js_void};
}

/**
 * @register {binary, int -> void}
 */
function add_uint32_le(b, i) {
  if (i < 0 || i > 0xffffffff) throw("BslBinary.add_uint32_le: out of range int "+i);
  if (b.length + 4 >= b.contents.length) grow(b);
  b.contents.writeUInt32LE(i, b.length, true);
  b.length += 4;
  return js_void;
}

/**
 * @register {binary, int -> opa[outcome(void,string)]}
 */
function add_uint32_ler(b, i) {
  if (i < 0 || i > 0xffffffff) return({failure:"BslBinary.add_uint32_le: out of range int "+i});
  if (b.length + 4 >= b.contents.length) grow(b);
  b.contents.writeUInt32LE(i, b.length, true);
  b.length += 4;
  return {success:js_void};
}

/**
 * @register {binary, int -> void}
 */
function add_int32_be(b, i) {
  if (i < -0x80000000 || i > 0x7fffffff) throw("BslBinary.add_int32_be: out of range int "+i);
  if (b.length + 4 >= b.contents.length) grow(b);
  b.contents.writeInt32BE(i, b.length, true);
  b.length += 4;
  return js_void;
}

/**
 * @register {binary, int -> opa[outcome(void,string)]}
 */
function add_int32_ber(b, i) {
  if (i < -0x80000000 || i > 0x7fffffff) return({failure:"BslBinary.add_int32_be: out of range int "+i});
  if (b.length + 4 >= b.contents.length) grow(b);
  b.contents.writeInt32BE(i, b.length, true);
  b.length += 4;
  return {success:js_void};
}

/**
 * @register {binary, int -> void}
 */
function add_uint32_be(b, i) {
  if (i < 0 || i > 0xffffffff) throw("BslBinary.add_uint32_be: out of range int "+i);
  if (b.length + 4 >= b.contents.length) grow(b);
  b.contents.writeUInt32BE(i, b.length, true);
  b.length += 4;
  return js_void;
}

/**
 * @register {binary, int -> opa[outcome(void,string)]}
 */
function add_uint32_ber(b, i) {
  if (i < 0 || i > 0xffffffff) return({failure:"BslBinary.add_uint32_be: out of range int "+i});
  if (b.length + 4 >= b.contents.length) grow(b);
  b.contents.writeUInt32BE(i, b.length, true);
  b.length += 4;
  return {success:js_void};
}

/**
 * @register {binary, int -> void}
 */
function add_int53_le(b, i) {
  var i64 = i64_of_int_signed(i);
  if (is_nan(i64)) throw("BslBinary.add_int53_be: out of range int "+i);
  if (b.length + 8 > b.contents.length) grow(b);
  b.contents.writeUInt32LE(i64.l, b.length, true);
  b.contents.writeUInt32LE(i64.h, b.length+4, true);
  b.length += 8;
  return js_void;
}

/**
 * @register {binary, int -> opa[outcome(void,string)]}
 */
function add_int53_ler(b, i) {
  var i64 = i64_of_int_signed(i);
  if (is_nan(i64)) return({failure:"BslBinary.add_int53_be: out of range int "+i});
  if (b.length + 8 > b.contents.length) grow(b);
  b.contents.writeUInt32LE(i64.l, b.length, true);
  b.contents.writeUInt32LE(i64.h, b.length+4, true);
  b.length += 8;
  return {success:js_void};
}

/**
 * @register {binary, int -> void}
 */
function add_int53_be(b, i) {
  var i64 = i64_of_int_signed(i);
  if (is_nan(i64)) throw("BslBinary.add_int53_be: out of range int "+i);
  if (b.length + 8 > b.contents.length) grow(b);
  b.contents.writeUInt32BE(i64.h, b.length, true);
  b.contents.writeUInt32BE(i64.l, b.length+4, true);
  b.length += 8;
  return js_void;
}

/**
 * @register {binary, int -> opa[outcome(void,string)]}
 */
function add_int53_ber(b, i) {
  var i64 = i64_of_int_signed(i);
  if (is_nan(i64)) return({failure:"BslBinary.add_int53_be: out of range int "+i});
  if (b.length + 8 > b.contents.length) grow(b);
  b.contents.writeUInt32BE(i64.h, b.length, true);
  b.contents.writeUInt32BE(i64.l, b.length+4, true);
  b.length += 8;
  return {success:js_void};
}

/**
 * @register {binary, int64 -> void}
 */
function add_uint64_le(b, i) {
  if (b.length + 8 >= b.contents.length) grow(b);
  b.contents.writeUInt32LE(i.l, b.length, true);
  b.contents.writeUInt32LE(i.h, b.length+4, true);
  b.length += 8;
  return js_void;
}

/**
 * @register {binary, int64 -> opa[outcome(void,string)]}
 */
function add_uint64_ler(b, i) {
  if (b.length + 8 >= b.contents.length) grow(b);
  b.contents.writeUInt32LE(i.l, b.length, true);
  b.contents.writeUInt32LE(i.h, b.length+4, true);
  b.length += 8;
  return {success:js_void};
}

/**
 * @register {binary, int64 -> void}
 */
function add_uint64_be(b, i) {
  if (b.length + 8 >= b.contents.length) grow(b);
  b.contents.writeUInt32BE(i.h, b.length, true);
  b.contents.writeUInt32BE(i.l, b.length+4, true);
  b.length += 8;
  return js_void;
}

/**
 * @register {binary, int64 -> opa[outcome(void,string)]}
 */
function add_uint64_ber(b, i) {
  if (b.length + 8 >= b.contents.length) grow(b);
  b.contents.writeUInt32BE(i.h, b.length, true);
  b.contents.writeUInt32BE(i.l, b.length+4, true);
  b.length += 8;
  return {success:js_void};
}

// Might not be accurate to the last bit...
var FLOATMIN32 = 1.175494351e-38;
var FLOATMAX32 = 3.402823466e38;

/**
 * @register {binary, float -> void}
 */
function add_float_le(b, f) {
  if (f < FLOATMIN32 || f > FLOATMAX32) throw("BslBinary.add_float_le: out of range float "+f);
  if (b.length + 4 >= b.contents.length) grow(b);
  b.contents.writeFloatLE(f, b.length, false); // <-- this probably also traps NaN, Inf, -Inf etc.
  b.length += 4;
  return js_void;
}

/**
 * @register {binary, float -> opa[outcome(void,string)]}
 */
function add_float_ler(b, f) {
  if (f < FLOATMIN32 || f > FLOATMAX32) return({failure:"BslBinary.add_float_le: out of range float "+f});
  if (b.length + 4 >= b.contents.length) grow(b);
  b.contents.writeFloatLE(f, b.length, false); // <-- this probably also traps NaN, Inf, -Inf etc.
  b.length += 4;
  return {success:js_void};
}

/**
 * @register {binary, float -> void}
 */
function add_float_be(b, f) {
  if (f < FLOATMIN32 || f > FLOATMAX32) throw("BslBinary.add_float_be: out of range float "+f);
  if (b.length + 4 >= b.contents.length) grow(b);
  b.contents.writeFloatBE(f, b.length, false); // <-- this probably also traps NaN, Inf, -Inf etc.
  b.length += 4;
  return js_void;
}

/**
 * @register {binary, float -> opa[outcome(void,string)]}
 */
function add_float_ber(b, f) {
  if (f < FLOATMIN32 || f > FLOATMAX32) return({failure:"BslBinary.add_float_be: out of range float "+f});
  if (b.length + 4 >= b.contents.length) grow(b);
  b.contents.writeFloatBE(f, b.length, false); // <-- this probably also traps NaN, Inf, -Inf etc.
  b.length += 4;
  return {success:js_void};
}

/**
 * @register {binary, float -> void}
 */
function add_double_le(b, f) {
  if (b.length + 8 >= b.contents.length) grow(b);
  b.contents.writeDoubleLE(f, b.length, false);
  b.length += 8;
  return js_void;
}

/**
 * @register {binary, float -> opa[outcome(void,string)]}
 */
function add_double_ler(b, f) {
  if (b.length + 8 >= b.contents.length) grow(b);
  b.contents.writeDoubleLE(f, b.length, false);
  b.length += 8;
  return {success:js_void};
}

/**
 * @register {binary, float -> void}
 */
function add_double_be(b, f) {
  if (b.length + 8 >= b.contents.length) grow(b);
  b.contents.writeDoubleBE(f, b.length, false);
  b.length += 8;
  return js_void;
}

/**
 * @register {binary, float -> opa[outcome(void,string)]}
 */
function add_double_ber(b, f) {
  if (b.length + 8 >= b.contents.length) grow(b);
  b.contents.writeDoubleBE(f, b.length, false);
  b.length += 8;
  return {success:js_void};
}

/**
 * @register {binary, int, int -> string}
 */
function get_string(b, start, length) {
  return b.contents.toString('utf8', start, start + length);
}

/**
 * @register {binary, int, int -> opa[outcome(string,string)]}
 */
function get_stringr(b, start, length) {
  return {success : get_string(b, start, length)};
}

/**
 * @register {binary, int, int -> binary}
 */
function get_binary(b, start, length) {
  if (typeof start == 'undefined') start = 0;
  if (typeof length == 'undefined') length = b.length;
  if (start < 0 || start >= b.length || length < 0) return {contents:new Buffer(0), length:0};
  var end = (b.length < start + length) ? b.length : start + length;
  var len = end - start;
  var nb = new Buffer(len);
  for (var i = start, j = 0; i < end; i++,j++) { nb[j] = b.contents[i]; };
  return {contents:nb, length:len};
}

/**
 * @register {binary, int, int -> opa[outcome(binary,string)]}
 */
function get_binaryr(b, start, length) {
  if (typeof start == 'undefined') start = 0;
  if (typeof length == 'undefined') length = b.length;
  if (start < 0 || start >= b.length || length < 0) return {contents:new Buffer(0), length:0};
  var end = (b.length < start + length) ? b.length : start + length;
  var len = end - start;
  var nb = new Buffer(len);
  for (var i = start, j = 0; i < end; i++,j++) { nb[j] = b.contents[i]; };
  return {success:{contents:nb, length:len}};
}

/**
 * @register {binary, int -> int}
 */
function get_int8(b, start) {
  if (start >= b.length) throw("BslBinary.get_int8: insufficient buffer data");
  return b.contents.readInt8(start, true);
}

/**
 * @register {binary, int -> opa[outcome(int,string)]}
 */
function get_int8r(b, start) {
  if (start >= b.length) return({failure:"BslBinary.get_int8: insufficient buffer data"});
  return {success:b.contents.readInt8(start, true)};
}

/**
 * @register {binary, int -> int}
 */
function get_uint8(b, start) {
  if (start >= b.length) throw("BslBinary.get_uint8: insufficient buffer data");
  return b.contents.readUInt8(start, true);
}

/**
 * @register {binary, int -> opa[outcome(int,string)]}
 */
function get_uint8r(b, start) {
  if (start >= b.length) return({failure:"BslBinary.get_uint8: insufficient buffer data"});
  return {success:b.contents.readUInt8(start, true)};
}

/**
 * @register {binary, int -> int}
 */
function get_int16_le(b, start) {
  if (start > b.length - 2) throw("BslBinary.get_int16_le: insufficient buffer data");
  return b.contents.readInt16LE(start, true);
}

/**
 * @register {binary, int -> opa[outcome(int,string)]}
 */
function get_int16_ler(b, start) {
  if (start > b.length - 2) return({failure:"BslBinary.get_int16_le: insufficient buffer data"});
  return {success:b.contents.readInt16LE(start, true)};
}

/**
 * @register {binary, int -> int}
 */
function get_uint16_le(b, start) {
  if (start > b.length - 2) throw("BslBinary.get_uint16_le: insufficient buffer data");
  return b.contents.readUInt16LE(start, true);
}

/**
 * @register {binary, int -> opa[outcome(int,string)]}
 */
function get_uint16_ler(b, start) {
  if (start > b.length - 2) return({failure:"BslBinary.get_uint16_le: insufficient buffer data"});
  return {success:b.contents.readUInt16LE(start, true)};
}

/**
 * @register {binary, int -> int}
 */
function get_int16_be(b, start) {
  if (start > b.length - 2) throw("BslBinary.get_int16_be: insufficient buffer data");
  return b.contents.readInt16BE(start, true);
}

/**
 * @register {binary, int -> opa[outcome(int,string)]}
 */
function get_int16_ber(b, start) {
  if (start > b.length - 2) return({failure:"BslBinary.get_int16_be: insufficient buffer data"});
  return {success:b.contents.readInt16BE(start, true)};
}

/**
 * @register {binary, int -> int}
 */
function get_uint16_be(b, start) {
  if (start > b.length - 2) throw("BslBinary.get_uint16_be: insufficient buffer data");
  return b.contents.readUInt16BE(start, true);
}

/**
 * @register {binary, int -> opa[outcome(int,string)]}
 */
function get_uint16_ber(b, start) {
  if (start > b.length - 2) return({failure:"BslBinary.get_uint16_be: insufficient buffer data"});
  return {success:b.contents.readUInt16BE(start, true)};
}

/**
 * @register {binary, int -> int}
 */
function get_int32_le(b, start) {
  if (start > b.length - 4) throw("BslBinary.get_int32_le: insufficient buffer data");
  return b.contents.readInt32LE(start, true);
}

/**
 * @register {binary, int -> opa[outcome(int,string)]}
 */
function get_int32_ler(b, start) {
  if (start > b.length - 4) return({failure:"BslBinary.get_int32_le: insufficient buffer data"});
  return {success:b.contents.readInt32LE(start, true)};
}

/**
 * @register {binary, int -> int}
 */
function get_uint32_le(b, start) {
  if (start > b.length - 4) throw("BslBinary.get_uint32_le: insufficient buffer data");
  return b.contents.readUInt32LE(start, true);
}

/**
 * @register {binary, int -> opa[outcome(int,string)]}
 */
function get_uint32_ler(b, start) {
  if (start > b.length - 4) return({failure:"BslBinary.get_uint32_le: insufficient buffer data"});
  return {success:b.contents.readUInt32LE(start, true)};
}

/**
 * @register {binary, int -> int}
 */
function get_int32_be(b, start) {
  if (start > b.length - 4) throw("BslBinary.get_int32_be: insufficient buffer data");
  return b.contents.readInt32BE(start, true);
}

/**
 * @register {binary, int -> opa[outcome(int,string)]}
 */
function get_int32_ber(b, start) {
  if (start > b.length - 4) return({failure:"BslBinary.get_int32_be: insufficient buffer data"});
  return {success:b.contents.readInt32BE(start, true)};
}

/**
 * @register {binary, int -> int}
 */
function get_uint32_be(b, start) {
  if (start > b.length - 4) throw("BslBinary.get_uint32_be: insufficient buffer data");
  return b.contents.readUInt32BE(start, true);
}

/**
 * @register {binary, int -> opa[outcome(int,string)]}
 */
function get_uint32_ber(b, start) {
  if (start > b.length - 4) return({failure:"BslBinary.get_uint32_be: insufficient buffer data"});
  return {success:b.contents.readUInt32BE(start, true)};
}

/**
 * @register {binary, int -> int}
 */
function get_int53_le(b, start) {
  if (start > b.length - 8) throw("BslBinary.get_int53_le: insufficient buffer data");
  return i64_to_int_signed({h:b.contents.readUInt32LE(start+4, true),
                                           l:b.contents.readUInt32LE(start, true)});
}

/**
 * @register {binary, int -> opa[outcome(int,string)]}
 */
function get_int53_ler(b, start) {
  if (start > b.length - 8) return({failure:"BslBinary.get_int53_le: insufficient buffer data"});
  return {success:i64_to_int_signed({h:b.contents.readUInt32LE(start+4, true),
                                        l:b.contents.readUInt32LE(start, true)})};
}

/**
 * @register {binary, int -> int}
 */
function get_int53_be(b, start) {
  if (start > b.length - 8) throw("BslBinary.get_int53_be: insufficient buffer data");
  return i64_to_int_signed({h:b.contents.readUInt32BE(start, true),
                                           l:b.contents.readUInt32BE(start+4, true)});
}

/**
 * @register {binary, int -> opa[outcome(int,string)]}
 */
function get_int53_ber(b, start) {
  if (start > b.length - 8) return({failure:"BslBinary.get_int53_be: insufficient buffer data"});
  return {success:i64_to_int_signed({h:b.contents.readUInt32BE(start, true),
                                        l:b.contents.readUInt32BE(start+4, true)})};
}

/**
 * @register {binary, int -> int64}
 */
function get_uint64_le(b, start) {
  if (start > b.length - 8) throw("BslBinary.get_uint64_le: insufficient buffer data");
  return {h:b.contents.readUInt32LE(start+4, true),
          l:b.contents.readUInt32LE(start, true)};
}

/**
 * @register {binary, int -> opa[outcome(int64,string)]}
 */
function get_uint64_ler(b, start) {
  if (start > b.length - 8) return({failure:"BslBinary.get_uint64_le: insufficient buffer data"});
  return {success:{h:b.contents.readUInt32LE(start+4, true),
                   l:b.contents.readUInt32LE(start, true)}};
}

/**
 * @register {binary, int -> int64}
 */
function get_uint64_be(b, start) {
  if (start > b.length - 8) throw("BslBinary.get_uint64_be: insufficient buffer data");
  return {h:b.contents.readUInt32BE(start, true),
          l:b.contents.readUInt32BE(start+4, true)};
}

/**
 * @register {binary, int -> opa[outcome(int64,string)]}
 */
function get_uint64_ber(b, start) {
  if (start > b.length - 8) return({failure:"BslBinary.get_uint64_be: insufficient buffer data"});
  return {success:{h:b.contents.readUInt32BE(start, true),
                   l:b.contents.readUInt32BE(start+4, true)}};
}

/**
 * @register {binary, int -> float}
 */
function get_float_le(b, start) {
  if (start > b.length - 4) throw("BslBinary.get_float_le: insufficient buffer data");
  return b.contents.readFloatLE(start, true);
}

/**
 * @register {binary, int -> opa[outcome(float,string)]}
 */
function get_float_ler(b, start) {
  if (start > b.length - 4) return({failure:"BslBinary.get_float_le: insufficient buffer data"});
  return {success:b.contents.readFloatLE(start, true)};
}

/**
 * @register {binary, int -> float}
 */
function get_float_be(b, start) {
  if (start > b.length - 4) throw("BslBinary.get_float_be: insufficient buffer data");
  return b.contents.readFloatBE(start, true);
}

/**
 * @register {binary, int -> opa[outcome(float,string)]}
 */
function get_float_ber(b, start) {
  if (start > b.length - 4) return({failure:"BslBinary.get_float_be: insufficient buffer data"});
  return {success:b.contents.readFloatBE(start, true)};
}

/**
 * @register {binary, int -> float}
 */
function get_double_le(b, start) {
  if (start > b.length - 8) throw("BslBinary.get_double_le: insufficient buffer data");
  return b.contents.readDoubleLE(start, true);
}

/**
 * @register {binary, int -> opa[outcome(float,string)]}
 */
function get_double_ler(b, start) {
  if (start > b.length - 8) return({failure:"BslBinary.get_double_le: insufficient buffer data"});
  return {success:b.contents.readDoubleLE(start, true)};
}

/**
 * @register {binary, int -> float}
 */
function get_double_be(b, start) {
  if (start > b.length - 8) throw("BslBinary.get_double_be: insufficient buffer data");
  return b.contents.readDoubleBE(start, true);
}

/**
 * @register {binary, int -> opa[outcome(float,string)]}
 */
function get_double_ber(b, start) {
  if (start > b.length - 8) return({failure:"BslBinary.get_double_be: insufficient buffer data"});
  return {success:b.contents.readDoubleBE(start, true)};
}
