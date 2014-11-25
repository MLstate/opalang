/*
    Copyright © 2014 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


/** Session storage. */

/**
 * @register {string, string -> void}
 */
function session_setItem(x, val) {
  sessionStorage.setItem(x, val);
}

/**
 * @register {string -> opa[option(string)]}
 */
function session_getItem(x) {
	var val = sessionStorage.getItem(x);
	if (val) return js_some(val);
	else return js_none;
}

/**
 * @register {string -> void}
 */
function session_removeItem(x) {
	sessionStorage.removeItem(x);
}

/**
 * @register { -> void}
 */
function session_clear() {
	sessionStorage.clear();
}

/**
 * @register { -> int}
 */
function session_length() {
	return sessionStorage.length();
}

/**
 * @register {int -> opa[option(string)]}
 */
function session_key(i) {
	var key = sessionStorage.key(i);
	if (key) return js_some(key);
	else return js_none;
}


/** Local storage. */

/**
 * @register {string -> bool}
 */
function local_keyExists(x) {
  return localStorage.getItem(x)===null;
}

/**
 * @register {string, string -> void}
 */
function local_setItem(x, val) {
  localStorage.setItem(x, val);
}

/**
 * @register {string -> opa[option(string)]}
 */
function local_getItem(x) {
	var val = localStorage.getItem(x);
	if (val) return js_some(val);
	else return js_none;
}

/**
 * @register {string -> void}
 */
function local_removeItem(x) {
	localStorage.removeItem(x);
}

/**
 * @register { -> void}
 */
function local_clear() {
	localStorage.clear();
}

/**
 * @register { -> int}
 */
function local_length() {
	return localStorage.length();
}

/**
 * @register {int -> opa[option(string)]}
 */
function local_key(i) {
	var key = localStorage.key(i);
	if (key) return js_some(key);
	else return js_none;
}
