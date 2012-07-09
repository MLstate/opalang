/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Extensions to JS standard prototypes
 *
 * @author David Rajchenbach-Teller
 */

/**
 * {1 Array}
 */

//The following extracts come from the specification of ECMASCRIPT 262

if (!Array.prototype.map)
{
  /**
   * This would be the place for the documentation of Array.map
   * @param fun
  **/
  Array.prototype.map = function(fun /*, thisp */)
  {
    "use strict";

    if (this === void 0 || this === null)
      throw new TypeError();

    var t = Object(this);
    var len = t.length >>> 0;
    if (typeof fun !== "function")
      throw new TypeError();

    var res = new Array(len);
    var thisp = arguments[1];
    for (var i = 0; i < len; i++)
    {
      if (i in t)
        res[i] = fun.call(thisp, t[i], i, t);
    }

    return res;
  };
}

if (!Array.prototype.forEach)
{
  /**
   * This would be the place for the documentation of Array.forEach
   * @param fun
  **/
  Array.prototype.forEach = function(fun /*, thisp */)
  {
    "use strict";

    if (this === void 0 || this === null)
      throw new TypeError();

    var t = Object(this);
    var len = t.length >>> 0;
    if (typeof fun !== "function")
      throw new TypeError();

    var thisp = arguments[1];
    for (var i = 0; i < len; i++)
    {
      if (i in t)
        fun.call(thisp, t[i], i, t);
    }
  };
}

if (!Array.prototype.reduce)
{
  /**
   * This would be the place for the documentation of Array.reduce
   * @param fun
  **/
  Array.prototype.reduce = function(fun /*, initialValue */)
  {
    "use strict";

    if (this === void 0 || this === null)
      throw new TypeError();

    var t = Object(this);
    var len = t.length >>> 0;
    if (typeof fun !== "function")
      throw new TypeError();

    // no value to return if no initial value and an empty array
    if (len == 0 && arguments.length == 1)
      throw new TypeError();

    var k = 0;
    var accumulator;
    if (arguments.length >= 2)
    {
      accumulator = arguments[1];
    }
    else
    {
      do
      {
        if (k in t)
        {
          accumulator = t[k++];
          break;
        }

        // if array contains no values, no initial value to return
        if (++k >= len)
          throw new TypeError();
      }
      while (true);
    }

    while (k < len)
    {
      if (k in t)
        accumulator = fun.call(undefined, accumulator, t[k], k, t);
      k++;
    }

    return accumulator;
  };
}
//End of extracts

/**
 * @param i
 * @param b
**/
Array.prototype.append_sub = function(i, b)
{
    return this.concat(b.slice(i));
}

/**
 * @param i
**/
Array.prototype.sub = function(i)
{
    return this.slice(0, i);
}

/**
 * @param value
**/
Array.prototype.inArray = function (value)
{
  for (var i = 0, l = this.length; i < l; i++)
    if (value == this[i]) return true;
  return false;
}

/**
 * {1 String}
 */

/**
 * Remove blank char at left and right.
 * @return {String|string} trimed string
**/
String.prototype.trim = function ()
{
  var chr = " \t\n\r\v\0", n = this;
  while (n.length > 0 && chr.indexOf(n.substr(0, 1)) != -1) n = n.substr(1);
  while (n.length > 0 && chr.indexOf(n.substr(-1)) != -1) n = n.substr(0, n.length - 1);
  return n;
}
