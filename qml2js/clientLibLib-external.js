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
 * @type bool
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
 * return {Boolean}
 */
var is_native_object = function(x) {}

/**
 * Nodejs module exports
 */
var exports, globals, require;
