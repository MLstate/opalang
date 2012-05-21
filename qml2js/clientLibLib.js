/*
    Copyright © 2011, 2012 MLstate

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

var print = console.log
// command_line_execution should be deprecated (use preprocessing instead)
// keep for the moment because some code still use
#<Ifstatic:OPABSL_NODE>
var command_line_execution = true;
#<Else>
var command_line_execution = false;
#<End>

/**
 * Browser-specific adaptations
 */
#<Ifstatic:OPABSL_NODE>
#<Else>
var is_native_object;

var userAgent = navigator.userAgent.toLowerCase();
if(/msie/.test( userAgent ) && !/opera/.test( userAgent )) //MSIE compatibility mode
{
    is_native_object = function(x)
    {
        return (x instanceof ActiveXObject);
    }
} else if (userAgent=="no browser") { //Command-line compatibility mode
    is_native_object = function(x)
    {
        return false;
    }
} else {
    is_native_object = function(x) {
        return (x instanceof Node || x instanceof Event);
    }
}
#<End>

function error(s)
{
    console.error(s);
    throw new Error(s);
}

/**
 * An assertion function provided for convenience when debugging the compiler.
 * Unless you're debugging the JS compiler, you shouldn't need this function.
 *
 * Check a condition. If the condition is [false], raise an [Error] with a given debugging message.
 *
 * @param {Boolean} b A boolean condition. If it is [false], this is a fatal error, execution will stop.
 * @param {String} s A debugging message.
 */
function js_assert(b, s){
    if (!b) error(s);
    return js_void;
}

/**
 * This function does nothing.
 * Replace it (dynamically) with something else to make it do something.
 */
var js_debug = function(s)
{
}
