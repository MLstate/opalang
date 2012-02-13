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

/**
 * @author Corentin Gallet, October 2009 (now using text iterators)
 * @author Adam Koprowski, February 2010 (clean-up, extensions, documentation, ...)
 * @author Nicolas Pelletier, February 2010 (extensions)
 *
 * @category parsing
 * @destination public
 */

/**
 * {1 About this module}
 *
 * This module contains a high-level interface for TRX: OPA's engine for parsing.
 * If you want to parse some text, then you have come to the right place.
 *
 *
 * {1 Where should I start?}
 *
 * See module for a number of functions for parsing. The ones you are likely to
 * use most often are:
 *  - {!Parser.parse}
 *  - {!Parser.try_parse}
 *  - {!Parser.partial_parse}
 *  - {!Parser.partial_try_parse}
 * The prefix [try] indicates that parsing may fail, in which case a [none] will
 * be returned; functions without this prefix should only be used if parsing
 * should not fail, regardless of the input, as they indicate an error when that
 * happens. Similarly parsing with functions without the [partial] prefix will only
 * succeed if the whole input is consumed, whereas their counter-parts with this
 * prefix allow incomplete (partial) parses.
 *
 * The {!Parser} module also contains a number of standard parsers.
 *
 *
 * {1 What if I need more?}
 *
 * If you are looking for a more general parsing front-end start by exploring
 * the {!Parser.parse_generic} function.
 *
 * If you need to write a custom parser, you can easily do so: see the {!Rule}
 * module for some examples of parsing rules (you may find some of them useful
 * for your own parsers) and consult the OPA manual for more information on how
 * to write OPA parsers.
 */

/**
 * {1 Parser module interface}
 */

/**
 * A module with parsing related functions and some common parsers
 */
Parser =
{{

  /**
   * {2 Front-end functions for parsing}
   */

  /**
   * Generic function for parsing.
   *
   * @param partial indicates whether partial parsing is allowed
   * (if not then parsing which does not consume the whole input
   * is considered erroneous)
   * @param rule parser to be used
   * @param s input string to be parsed
   * @param on_failure if parsing fails then the result will be
   * [on_failure()]
   * @param on_success if parsing succeeds with [res] then the
   * result of this function will be [on_success(res)].
   * @return the result of parsing, as outlined above.
   */
  parse_generic(partial, rule: Parser.general_parser, s, on_failure,
                on_success) =
    match rule(partial, Text.itstart(Text.cons(s))) with
    | {none} -> on_failure()
    | {some=(it, res)} -> on_success((it, res))

  parse_error(input, pos, fn) =
    txt =
      if String.length(input) < 20 then
        input
      else
        String.substring(0, 20, input) ^ "..."
    error("Parse error: function {fn} on string: <{txt}> {pos}")

  /**
   * Generic function for non-partial parsing that returns the
   * result of parsing, or signals an error if parsing fails.
   *
   * @param rule parser to be used
   * @param s input string to be parsed
   * @return the result of parsing
   */
  parse(rule, s) =
    on_failure() = parse_error(s, __POSITION__, "parse")
    on_success((_it, res)) = res
    parse_generic(false, rule, s, on_failure, on_success)

  /**
   * Generic function for non-partial parsing that returns
   * option type: either the result of parsing or [none] if
   * parsing fails.
   *
   * @param rule parser to be used
   * @param s input string to be parsed
   * @return either [some(v)] where [v] is the result of parsing
   * or [none] on parsing failure.
   */
  try_parse(rule, s) =
    on_failure() = none
    on_success((_it, res)) = some(res)
    parse_generic(false, rule, s, on_failure, on_success)

  /**
   * [try_parse_opt(f,s)] behaves like [try_parse(f,s) ? {none}]
   */
  try_parse_opt(rule,s) =
    on_failure() = none
    on_success((_it, res)) = res
    parse_generic(false, rule, s, on_failure, on_success)

  /**
   * Generic function for partial parsing that returns the
   * result of parsing, or signals an error if parsing fails.
   *
   * @param rule parser to be used
   * @param s input string to be parsed
   * @return the result of parsing
   */
  partial_parse(rule, s) =
    on_failure() = parse_error(s, __POSITION__, "partial_parse")
    on_success((_it, res)) = res
    parse_generic(true, rule, s, on_failure, on_success)

  /**
   * Generic function for partial parsing that returns
   * option type: either the result of parsing or [none] if
   * parsing fails.
   *
   * @param rule parser to be used
   * @param s input string to be parsed
   * @return either [some(v)] where [v] is the result of parsing
   * or [none] on parsing failure.
   */
  partial_try_parse(rule, s) =
    on_failure() = none
    on_success((_it, res)) = some(res)
    parse_generic(true, rule, s, on_failure, on_success)

  /**
   * Non-failing (i.e. failure results in an error), non-partial
   * parsing returning the result of parsing and a text iterator
   * for the remainder of the input.
   *
   * @param rule parser to be used
   * @param s input string to be parsed
   * @return a pair [(v, it)] where [v] is the result of parsing and
   * [it] is a text iterator for the remainder of [s].
   */
  parse_and_it(rule, s) =
    on_failure() = parse_error(s, __POSITION__, "parse_and_it")
    on_success(it_res) = some(it_res)
    parse_generic(false, rule, s, on_failure, on_success)

  /**
   * {2 Parsers for some common types}
   */

  /**
   * A parsing function for integer values.
   *
   * @param an input string
   *
   * @return [some(i)] with [i : int] being the numerical value of
   * the input, or [none] if the source does not contain a valid
   * representation of an integer.
   */
  int = try_parse(Rule.integer, _)

  /**
   * A parsing function for floating point numbers.
   *
   * @param an input string
   *
   * @return [some(f)] with [f : float] begin the float point number
   * represented by the input or [none] if the source does not contain
   * a valid representation of a float point number.
   */
  float = try_parse(Rule.float, _)

  /**
   * {2 Parsing related functions}
   */

  /**
   * Calculates the number of leading white space characters in a
   * given string.
   *
   * @param s Input string
   *
   * @return integer value corresponding to the number of leading white
   * space characters in [s].
   */
  ws_length =
    p = parser
      | l=Rule.white_space* -> List.length(l)
    partial_parse(p, _)

  of_string = Rule.of_string

  of_string_case_insensitive = Rule.of_string_case_insensitive

}}

@opacapi
Parser_of_string = Parser.of_string
