/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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

/**erents parse
 * {1 Parser module interface}
 */

/**
 * [genparse] is the function used by the [Parser.Generic] module to build the
 * differents parsing function. For more description of the genparse expected
 * behavior see [Parser.parse_generic]
 *
 * Note : The module is just used for forall
 */
@private type Parser.Gen('input) = {{
  genparse : (bool, Parser.general_parser('a), 'input, (-> 'b), ((itextrator, 'a) -> 'b) -> 'b)
  to_string: 'input -> string
}}

/**
 * A module with parsing related functions and some common parsers
 */
@workable
Parser =
{{

  /**
   * As [Parser.parse_string_generic] but on a [text].
   */
  @private
  StringGen = {{
    genparse(partial, rule:Parser.general_parser, s, on_failure, on_success) =
      TextGen.genparse(partial, rule, @toplevel.Text.cons(s), on_failure, on_success)

    to_string(x:string) = x
  }}

  /**
   * As [Parser.parse_string_generic] but on a [text].
   */
  @private
  TextGen = {{
    genparse(partial, rule:Parser.general_parser, text, on_failure, on_success) =
      ItextGen.genparse(partial, rule, @toplevel.Text.itstart(text), on_failure, on_success)

    to_string = @toplevel.Text.to_string
  }} : Parser.Gen(text)

  /**
   * As [Parser.parse_string_generic] but on a [itextrator].
   */
  @private
  ItextGen = {{
    genparse(partial, rule:Parser.general_parser, iterator, on_failure, on_success) =
      match rule(partial, iterator) with
      | {none} -> on_failure()
      | {some=(it, res)} -> on_success((it, res))

    to_string = @toplevel.Itextrator.to_string
  }} : Parser.Gen(itextrator)

  @private
  parse_error(input:string, pos:string, fn:string) =
    txt =
      if @toplevel.String.length(input) < 20 then
        input
      else
        @toplevel.String.substring(0, 20, input) ^ "..."
    error("Parse error: function {fn} on string: <{txt}> {pos}")

  /**
   * A generic functor to create parse functions.
   */
  @private
  Generic(F:Parser.Gen('input)) = {{

    parse_generic = F.genparse

    /**
     * As [Parser.parse] but the input depends of [genparse]
     */
    parse(rule:Parser.general_parser('a), s:'input):'a =
      on_failure() = parse_error(F.to_string(s), __POSITION__, "parse")
      on_success((_it, res)) = res
      F.genparse(false, rule, s, on_failure, on_success)

    /**
     * As [Parser.try_parse] but the input depends of [genparse]
     */
    try_parse(rule:Parser.general_parser('a), s:'input):option('a) =
      on_failure() = none
      on_success((_it, res)) = some(res)
      F.genparse(false, rule, s, on_failure, on_success)

    /**
     * As [Parser.try_parse_opt] but the input depends of [genparse]
     */
    try_parse_opt(rule:Parser.general_parser(option('a)), s:'input):option('a) =
      on_failure() = none
      on_success((_it, res)) = res
      F.genparse(false, rule, s, on_failure, on_success)

    /**
     * As [Parser.partial_parse] but the input depends of [genparse]
     */
    partial_parse(rule:Parser.general_parser('a), s:'input):'a =
      on_failure() = parse_error(F.to_string(s), __POSITION__, "partial_parse")
      on_success((_it, res)) = res
      F.genparse(true, rule, s, on_failure, on_success)

    /**
     * As [Parser.partial_try_parse] but the input depends of [genparse]
     */
    partial_try_parse(rule:Parser.general_parser('a), s:'input):option('a) =
      on_failure() = none
      on_success((_it, res)) = some(res)
      F.genparse(true, rule, s, on_failure, on_success)

    /**
     * As [Parser.parse_and_it] but the input depends of [genparse]
     */
    parse_and_it(_rule:Parser.general_parser('a), _s):option((itextrator, 'a)) =
      _on_failure() = @fail // parse_error(s, __POSITION__, "parse_and_it")
      _on_success(_it_res):option((itextrator, 'a)) = @fail
      @fail
  }}

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
  parse_generic(partial, rule, s, on_failure, on_success) =
    String.parse_generic(partial, rule, s, on_failure, on_success)

  /**
   * Generic function for non-partial parsing that returns the
   * result of parsing, or signals an error if parsing fails.
   *
   * @param rule parser to be used
   * @param s input string to be parsed
   * @return the result of parsing
   */
  parse(rule, s) =
    String.parse(rule, s)

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
    String.try_parse(rule, s)

  /**
   * [Parser.try_parse_opt(rule, s)] behaves like [try_parse(f,s) ? {none}]
   */
  try_parse_opt(rule, s) =
    String.try_parse_opt(rule, s)

  /**
   * Generic function for partial parsing that returns the
   * result of parsing, or signals an error if parsing fails.
   *
   * @param rule parser to be used
   * @param s input string to be parsed
   * @return the result of parsing
   */
  partial_parse(rule, s) =
    String.partial_parse(rule, s)

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
    String.partial_try_parse(rule, s)

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
    String.parse_and_it(rule, s)


  /**
   * {2 Parsers for differents type of input}
   *
   * These sub-modules defined same functions as above but on differents types.
   */

  /**
   * Parse functions on [string] inputs.
   */
  @both_implem
  String = Generic(StringGen)

  /**
   * Parse functions on [text].
   */
  @both_implem
  Text = Generic(TextGen)

  /**
   * Parse functions on [itextrator].
   */
  @both_implem
  Itextrator = Generic(ItextGen)

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
   * @return [some(f)] with [f : float] being the float point number
   * represented by the input or [none] if the source does not contain
   * a valid representation of a float point number.
   */
  float = try_parse(Rule.float, _)

  /**
   * A parsing function for alphanumerical strings, i.e a non empty
   * a sequence of letters (a-z and A-Z) and digits (0-9):
   *
   * @param an input string
   *
   * @return [some(f)] with [f : string]
   * or [none] if the source is not contain
   * a valid representation of an alphanum sequence
   */
  alphanum = try_parse(Rule.alphanum_string, _)

  /**
   * A parsing function for idents, ie a non empty
   * sequence of letters (a-z and A-Z), digits (0-9) and underscores.
   *
   * @param an input string
   *
   * @return [some(f)] with [f : string]
   * or [none] if the source is not contain
   * a valid representation of an ident
   */
  ident = try_parse(Rule.ident, _)

  /**
   * A parsing function for boolean string
   * ("false", "False", "true" or "True")
   *
   * @param an input string
   *
   * @return [some(f)] with [f : bool]
   * or [none] if the source is not contain
   * a valid representation of boolean
   */
  bool = try_parse(Rule.bool, _)

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

  of_string(x) = Rule.of_string(x)

  of_string_case_insensitive = Rule.of_string_case_insensitive

}}

@opacapi
Parser_of_string = Parser.of_string
