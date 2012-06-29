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
