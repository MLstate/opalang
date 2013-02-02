/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

// Try to detect whether running client or server-side by checking whether
// window is defined, i.e.
//
//   if (typeof window == 'undefined')
//     do_something();
//

// command_line_execution should be deprecated (use preprocessing instead)
// keep for the moment because some code still use

var command_line_execution;

if (typeof window == 'undefined') {
  command_line_execution = true;
} else {
  command_line_execution = false;
}

/**
 * Browser-specific adaptations
 */

var is_native_object;

if (typeof window != 'undefined') {
  var userAgent = navigator.userAgent.toLowerCase();
  if(/msie/.test( userAgent ) && !/opera/.test( userAgent )) //MSIE compatibility mode
  {
      is_native_object = function(x)
      {
          return (x instanceof ActiveXObject);
      };
  } else if (userAgent=="no browser") { //Command-line compatibility mode
      is_native_object = function(x)
      {
          return false;
      };
  } else {
      is_native_object = function(x) {
          return (x instanceof Node || x instanceof Event);
      };
  }
}

function error(s)
{
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

/**
 * FIXME: should be put elsewhere
 */

function check_opa_deps(dependencies){
  var deps = dependencies.filter(function(dependency, index, array) {
    // console.log('Checking', dependency, '...');
    try {
        require(dependency);
        return false;
    } catch(e) {
        if (process.version < "v0.8.0") return true;
        return (e.code === 'MODULE_NOT_FOUND');
    }
  });

  if (deps.length > 0) {
      console.error(deps.length+' modules are missing.', 'Please run: sudo npm install -g', deps.join(' '));
      process.exit(1);
  }
}
