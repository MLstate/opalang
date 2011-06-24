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

import stdlib.core.{parser}

/**
 * {1 About this module}
 *
 * Type URL ; savagely extracted from css.opa
 */
// ***** TODO: this should be either merged or replaced by uri.opa *****

/**
 * {1 Types defined in this module}
 */

@abstract type url = string

/**
 * {1 Interface}
 */

Url = {{
/*
  decode(url) =
  hexa = parser [0-9] -> Char.to_int(__1) - Char.to_int('0')
               | [a-f] -> 10 + Char.to_int(__1) - Char.to_int('a')
               | [A-F] -> 10 + Char.to_int(__1) - Char.to_int('A')
   p2 = parser "%" hexa hexa -> String.of_char(Int.to_char_unsafe(16*__2+__3))
            | [-%]+ -> String.string_of_chars(__1)
   p = parser p2* -> String.concat(__1)
   parse(p,url)
*/

/**
 * Creates an url from a string. No encoding is done, the text of the string
 * passed as argument is straight used as the url.
**/
  make(str : string) : url =
    str

  encode(url : url) =
    with_modulo(str) =
      rec aux(str, accu) =
        if String.length(str) == 0 then accu
        else aux(String.drop_left(2, str), accu^"%"^String.sub(0, 2, str))
      aux(if mod(String.length(str), 2) != 0 then "0"^str else str, "")
    rec p = parser
      | c=[a-zA-Z0-9~!@$^&*()_|\\=\-\[\]}{;:?/.,] p=p -> Int.to_string_value(c)^p
      | c=. p=p -> (Int.to_hex(c) |> with_modulo)^p
      | !. -> ""
    Parser.parse(p,url)

}}
