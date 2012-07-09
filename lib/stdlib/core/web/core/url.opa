/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
    c = parser
      | c=([a-zA-Z0-9~!@$^&*()_|\\=\-\[\]}{;:?/.,]) -> Text.to_string(c)
      | c=. -> (Int.to_hex(c) |> with_modulo)
    p = parser
      | l=c* -> String.flatten(l)
    Parser.parse(p,url)

}}
