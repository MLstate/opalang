/*
    Copyright © 2011 MLstate

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
 * The signature of the client lib, used for verification of the BSL
 *
 * @author David Rajchenbach-Teller, 2010
 */

/**
 * @type {!*}
 */
var js_void

/**
 * @type {!*}
 */
var _true
/**
 * @type {!*}
 */
var _false
/**
 * @type {!Object}
 */
var js_none

/**
 * @param {!*} s
 * @return {!Object}
 */
var js_some

/**
 * @param {!*} e
 * @return {!*}
 */
var bool2obj = function(e) {}

/**
 * @param {!*} r
 * @param {!string} f
 * @return {!*}
 */
var unsafe_dot = function(r, f) {}

/**
 * @param {!*} r
 * @param {!string} f
 * @return {!*}
 */
var dot = function(r, f) {}

/**
 * @param {!Object} r
 * @param {!string} f
 * @return {?*}
 */
var udot = function(r, f) {}

/**
 * @param {!Function} f
 * @param {!*} r
 * @param {!*} a
 * @return {!*}
 */
function fold_record(f, r, a) {}

/**
 * @param {!Function} f
 * @param {!*} r1
 * @param {!*} r2
 * @param {!*} a
 * @return {!*}
 */
var fold_2_record = function(f, r1, r2, a) {}

/**
 * @param {!string} field
 * @return {!*}
 */
var name_of_field = function(field) {}

/**
 * @param {!string} name
 * @return {!*}
 */
var field_of_name = function(name) {}

/**
 * @param {!string} name
 * @return {!string}
 */
var static_field_of_name = function(name) {}

/**
 * @return {!*}
 */
var empty_constructor = function() {}

/**
 * @param {!*} cons
 * @param {!*} field
 * @param {!*} value
 * @return {!*}
 */
var add_field = function(cons, field, value) {}

/**
 * @param {!*} cons
 * @return {!*}
 */
var make_record = function(cons) {}

/**
 * @param {!string} field
 * @return {!*}
 */
var make_simple_record = function(field) {}

/**
 * @param {!string} field
 * @param {!*} value
 * @return {!*}
 */
var make_onefield_record = function(field, value) {}

/**
 * @param {!*} e
 * @return {!*}
 */
var obj2bool = function(e) {}

/**
 * @param {!*} x
 * @return {!*}
 */
function normalize_obj(x) {}

/**
 * @param {?number} x
 * @return {!number}
 */
function normalize_num(x) {}

/**
 * @param {!*} x
 * @return {!*}
 */
function record2obj(x) {}

/**
 * @param {!*} e
 * @param {!Array} fields
 * @return {!*}
 */
function extendrecord(e, fields) {}

/**
 * @param {!Array.<!string>} fs
 * @return {!Array.<!string>}
 */
function fields_indexes(fs) {}

/**
 * @param {!string} f
 * @return {!string}
 */
function field_index(f) {}

/**
 * @param {!*} r
 * @param {!string} f
 * @return {!*}
 */
function dot_with_field_index(r,fi) {}

/**
 * @param {!Array.<!Array.<!string>>} patterns
 * @return {!Array.<!Array.<!string>>}
 */
function patterns_indexes(patterns) {}

/**
 * @param {!Array.<!Array.<!string>>} patterns
 * @param {!*} r1
 * @param {!*} r2
 * @return {!*}
 */
function compare_structure(patterns,r1,r2) {}



/**
 * @param {!*} l
 * @return {!Array.<!*>}
 */
function list2js(l) {}

/**
 * @param {!Array.<!*>} l
 * @return {!*}
 */
function js2list(l) {}

/**
 * @param {!*} o
 * @return {?*}
 */
function option2js(o) {}

/**
 * @param {!*} o
 * @return {?*}
 */
function option2jsu(o) {}

/**
 * @param {?*} o
 * @return {!*}
 */
function js2option(o) {}

/**
 *
 * @param {!*} a
 * @param {!*} b
 * @return {!number}
 */
function compare_raw(a,b) {}

/**
 * @param {function(*): *} fun
 * @param {!*} arg
 * @return {*}
 */
function tailcall_manager_call(fun, arg) {}

/**
 * @param {function(*): *} fun
 * @param {!Array.<*>} args
 * @return {*}
 */
function tailcall_manager_apply(fun, args) {}
