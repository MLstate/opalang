/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * @type boolean
 */
var command_line_execution

/**
 * @param {Boolean} b
 * @param {String} s
 * @return {!*}
 */
var js_assert = function(b, s) {}

/**
 * @param {!string} s
 * @return {!*}
 */
var js_debug = function(s) {}

/**
 * @param {!string} s
 */
function error(s) {}

/**
 * @param {*} x
 * @return {Boolean}
 */
var is_native_object = function(x) {}

// Nodejs module exports

/**
 * @type Object
 */
var exports;

/**
 * @type Object
 */
var global;

/**
 * @type Function
 */
var require;
