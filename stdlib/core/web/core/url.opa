/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
