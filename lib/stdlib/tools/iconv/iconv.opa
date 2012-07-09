/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Opa interface for iconv
 *
 * @author Frederic Ye, 2012
 * @category TOOL
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.1
 */

import-plugin iconv
package stdlib.tools.iconv

type Iconv.t = external

Iconv = {{

  /**
   * Open a descriptor for character set conversion
   */
  open(~{from to}) = %%bslIconv.iconv_open%%(to, from)

  /**
   * Perform a character set conversion
   * from a binary encoded data into another encoded binary data
   */
  convert = %%bslIconv.iconv%%

  /**
   * Perform a character set conversion
   * from a binary encoded data into an utf-8 compatible string
   */
  convert_to_utf8(from:string, s:binary) : option(string) =
    iconv = open({~from to="utf-8"})
    Option.map(string_of_binary, convert(iconv, s))

}}
