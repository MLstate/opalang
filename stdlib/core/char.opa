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

@abstract type Char.order = void

/**
 * {1 About this module}
 *
 * Here are provided some basic functionality on characters
 * Characters only represent the ASCII set
 *
 * @category data
 * @destination public
 * @author Valentin Gatien-Baron, 2010 (documentation)
 * @author Sarah Maarek, 2010 (review)
 * @stability stable
**/

Char = {{

  /**
   * Create a string made of a single character
   */
  to_string = %% Bslpervasives.string_of_char %%

  /**
   * Gives the ASCII code of the given character
   */
  to_int    = %% Bslpervasives.int_of_char %%

  /**
   * Convert letters to lowercase, does nothing otherwise
   */
  lowercase = %% BslChar.lowercase %%

  /**
   * Convert letters to uppercase, does nothing otherwise
   */
  uppercase = %% BslChar.uppercase %%

  of_string_aux = %% Bslpervasives.string_to_char %%

  /**
   * [Char.of_string(string)] returns the first char of [string]
   * Exits with an error when [String.length(source) <= 0]
   */
  of_string(source: string)=
    if String.length(source) > 0 then of_string_aux(source)
    else error("Char" ^ __POSITION__)
  : char

  /**
   * Creates the character with the given ASCII code
   *
   * If the given integer is not in the interval [0,127], this function exits with an error
   */
  unsafe_chr(c:int) = Option.get_msg(-> "Char.unsafe_chr: Can't convert {string_of_int(c)} in char", Int.to_char(c))

  ordering = %%BslChar.ordering%%
  `lt` = %%BslChar.lt%%
  `gt` = %%BslChar.gt%%
  `le` = %%BslChar.leq%%
  `ge` = %%BslChar.geq%%
  `ne` = %%BslChar.neq%%
  `eq` = %%BslChar.eq%%

  /**
   * Returns [true] when the given character is a spacing character, i.e. either a
   * space, a newline, a carriage return or a tabulation.
   */
  is_space(c) = match c with
    | ' ' | '\n' | '\t' | '\r' -> true
    | _ -> false

  /**
   * Returns [true] when the given character is a lower-case letter.
   */
  is_lower_case(c) = c >= 'a' && c <= 'z'

  /**
   * Returns [true] when the given character is an upper-case letter.
   */
  is_upper_case(c) = c >= 'A' && c <= 'Z'

  /**
   * Returns [true] when the given character is a letter (upper- or lower-case)
   */
  is_letter(c) = is_upper_case(c) || is_lower_case(c)

  compare(a,b): Order.comparison =
    @opensums(ordering(a,b))

  compare_raw =  %% Bslpervasives.compare_char %%

  order: order(char, Char.order) = Order.make(ordering)

}}
