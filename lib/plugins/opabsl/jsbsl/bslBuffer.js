/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * A low-level module of mutable string buffers.
 * @side{both}
 *
 * Values of type [buffer] are not and should not be serialized, so any use of buffer must be strictly controlled
 *
 *
 * @author David Rajchenbach-Teller
 */

/** @externType Buffer.t */
   //a JS array

/**
 * @constructor
 */
function LLBuffer() {
    this.contents = [];
    this.length   = 0;
}

LLBuffer.prototype = {
    opa_do_not_inspect: true
}

/**
 * @register {int -> Buffer.t} create
 * @pure
 */
function buffer_create(_) {
    return new LLBuffer();
}

/**
 * @register {Buffer.t, string -> void} append
 */
function buffer_append(buf, value) {
    buf.contents.push(value);
    buf.length = buf.length + value.length
    return js_void;
}

/**
 * @register {Buffer.t -> int} length
 */
function buffer_length(buf) {
    return buf.length;
}

/**
 * @register {Buffer.t -> string}
 */
function contents(buf) {
    var contents = buf.contents;
    var result   = contents.join("");
    buf.contents = [result];//Cache result
    return result;
}

/**
 * @register {Buffer.t -> void} clear
 */
function buffer_clear(b) {
    b.contents = [];
    b.length = 0;
    return js_void;
}
