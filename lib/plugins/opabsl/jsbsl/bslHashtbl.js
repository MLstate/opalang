/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * @side{both}
 *
 * @opaType Hashtbl.binding('key, 'value)
 * @externType Hashtbl.t('key, 'value)
 */

/**
 * @register {('key -> string), ('key, 'key -> bool), int -> Hashtbl.t('key, 'value)} make
 */
function hashtable_make(hash, equals, _init) {
    return new Hashtable(hash, equals);
}

/**
 * @register {int -> Hashtbl.t('key, 'value)} create
 */
function hashtable_create(_init) {
    return new SimpleTable();
}

/**
 * @register {Hashtbl.t('key, 'value) -> void} clear
 */
function hashtable_clear(table) {
    table.clear();
    return js_void;
}

/**
 * @register {Hashtbl.t('key, 'value), opa['key], opa['value] -> void} add
 */
function hashtable_add(table, key, value) {
    table.put(key, value);
    return js_void;
}

/**
 * @register {Hashtbl.t('key, 'value), opa['key], opa['value] -> void} replace
 */
function hashtable_replace(table, key, value) {
    // TODO - Should have the same semantics as ML bsl
    table.put(key, value);
    return js_void;
}

/**
 * @register {Hashtbl.t('key, 'value), opa['key] -> opa[option('value)]} try_find
 */
function hashtable_try_find(table, key) {
    var r = table.get(key);
    return r != null ? js_some(r) : js_none;
}

/**
 * @register {Hashtbl.t('key, 'value), opa['key] -> void} remove
 */
function hashtable_remove(table, key) {
    table.remove(key);
    return js_void;
}

/**
 * @register {Hashtbl.t('key, 'value) -> int} size
 */
function hashtable_size(table) {
    return table.size();
}

/**
 * @register {Hashtbl.t('key, 'value), opa['key] -> bool} mem
 */
function hashtable_mem(table, key) {
    return table.containsKey(key)
}

/**
 * @register {Hashtbl.t('key, 'value) -> llarray(opa[Hashtbl.binding('key, 'value)])} bindings
 */
function hashtable_bindings(table) {
    return table.values();
}
