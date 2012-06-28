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

/**
 * Configure the JS environment for browser-less execution if necessary.
 *
 * To detect whether we are in a browser, we check whether [window] is defined and has type "object".
 * This detection may change. The public API is to use global variable [command_line_execution],
 * which is set to [true] if we are executed out of a browser, [false] otherwise.
 */
var print = console.log
if (typeof window != "object") {
    alert = function (x) { print(x) };
    var placeholder = function (name) {return function() { print("function ["+name+"] requires a browser")} };
    var element = {
        innerHTML: "",
        style: {},
        insertBefore: placeholder("insertBefore"),
        removeChild : placeholder("removeChild"),
        createComment: placeholder("createComment"),
        appendChild: placeholder("appendChild"),
        getElementById: placeholder("getElementById")
    };
    var command_line_execution = true;

    element.documentElement = element;

    var element_fun = function () {
        return element;
    };

    element.createElement = element_fun;
    element.getElementsByTagName = element_fun;

    /* not define with "var" keyword because MSIE is little ... (what
     * U want)*/

    setTimeout    = placeholder("setTimeout");
    setInterval   = placeholder("setInterval");
    clearTimeout  = placeholder("clearTimeout");
    clearInterval = placeholder("clearInterval");
    clearTimeout  = placeholder("clearTimeout");
    location = {};
    document = element;
    document.cookie = "OK";
    window = { addEventListener: placeholder("addEventListener") };
    navigator = { userAgent : "no browser" };
} else {
    command_line_execution = false;
}


/**
 * Browser-specific adaptations
 */
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

function error(s)
{
    throw new Error(s)
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
