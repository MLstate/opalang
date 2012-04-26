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
   *
   */
  open(~{from to}) = %%bslIconv.iconv_open%%(to, from)

  /**
   * Convert a binary encoded data into another encoded binary data
   */
  convert = %%bslIconv.iconv%%

  /**
   * Convert a binary binary data into an utf-8 compatible string
   */
  convert_to_utf8(from:string, s:binary) : string =
    iconv = open({~from to="utf-8"})
    convert(iconv, s)

}}
