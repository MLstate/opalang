/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * @author ...?
 * @author Adam Koprowski, February-March 2010
 *
 * @category parsing
 * @destination public
 */

import stdlib.core.map

/**
 * {1 About this module}
 *
 * This module provides a number of generic parser combinators as well as some
 * ready-made parsers for common data-types.
 *
 *
 * {1 Where should I start?}
 *
 * If you need a parser, you can start by browsing the {!Rule} module to see whether
 * there is one that suits your needs. Often it will also be located in the module
 * of the appropriate data-type, so for instance parsers for dates can be found
 * in the {!Date} module.
 *
 *
 * {1 What if I need more?}
 *
 * If you need a parser that is not available here, you can easily write one,
 * following the instructions in the OPA manual. You may find some parser
 * combinators from the {!Rule} module useful for that. You can also browse
 * existing parsers in this module, for inspiration and design patterns.
 */

/**
 * {1 Interface}
 */

/**
 * Some useful parsers and parser combinators
 */
Rule =
{{

  /**
   * {2 Parser combinators}
   */

  /**
   * A parser combinator that will run a given parser [rule], [n] times.
   *
   * @param n the required number of repetitions of the base parser.
   * @param rule a base parser to be used by this combinator.
   * @return a parser that runs [rule] [n] times and produces a list of
   * results given by the [rule].
   */
  rep(n : int, rule : Parser.general_parser('a)) : Parser.general_parser(list('a)) =
    if n > 0 then parser x=rule xs={rep(n - 1, rule)} -> x +> xs
    else parser succeed -> []

  /**
   * A parser combinator that will run a given parser [rule], [n] times.
   * Similar to {!rep}, but the [rule] parser can be parametrized by the
   * iteration counter.
   *
   * @param n the required number of repetitions of the base parser.
   * @param rule a function from the iteration counter (in range [0] -- [n-1])
   *        to a base parser to be used by this combinator.
   * @return a parser that runs [rule] [n] times and produces a list of
   *         results given by the [rule].
   */
  repi(n : int, rule : int -> Parser.general_parser('a)) : Parser.general_parser(list('a)) =
    rec aux(i) =
      if i == n then
        parser succeed -> []
      else
        parser x={rule(i)} xs={aux(i + 1)} -> x +> xs
    aux(0)

  /**
   * A parser combinator that parses a list of elements with some
   * separator, so of the form:
   *
   * {[[sep] elem sep ... sep elem]}
   *
   * where the first separator is required only if [sep_at_beg=true].
   *
   * In particular:
   *   - if [sep_at_beg=false] then the function will first try to
   *     parse [elem]. If that fails it results in [[]]; beware when
   *     [elem] accepts empty string though, as then even application
   *     on empty string will result in one element list.
   *   - if [sep_at_beg=true] then the function will first try to
   *     parse [sep] and only then [elem]. If [sep] fails the overall
   *     result is [[]]; ditto if [sep] succeeds but [elem] fails.
   *     Again: beware if [sep] succeeds on empty string.
   *
   * @param sep_at_beg whether separator is required at the beginning
   * @param sep a parser for the separator between elements of the list
   * @param elem a parser for the elements of the list
   * @return a parser that recognizes input in the form presented above
   * and returns a list of results parsed with [elem] (parsing results
   * of [sep] are ignored).
   */
  parse_list_sep(sep_at_beg, elem, sep) =
    parse_no_sep_at_beg = parser
      | x=elem xs=(sep v=elem -> v)* -> x +> xs
      | succeed -> []
    parse_sep_at_beg = parser
      | sep res={parse_no_sep_at_beg} -> res
      | succeed -> []
    if sep_at_beg then
      parse_sep_at_beg
    else
      parse_no_sep_at_beg


  /**
   * A parser combinator that parses a list of elements with some
   * separator, so of the form:
   *
   * {[sep* elem sep+ ... sep+ elem sep*]}
   *
   * Compared to [parse_list_sep], two consecutive separators are
   * allowed and the empty text in-between them will not be parsed
   * with [elem].
   *
   * @param sep a parser for the separator between elements of the list
   * @param elem a parser for the elements of the list
   * @return a parser that recognizes input in the form presented above
   * and returns a list of results parsed with [elem] (parsing results
   * of [sep] are ignored).
   */
  parse_list_sep_non_empty(elem, sep) =
    parser sep* l=(x=elem sep+ -> x)* last=elem? -> match last | {none} -> l | ~{some} -> l ++ [some]

  /**
   * As {!parse_list_sep} but lists of length shorter than [n] are rejected.
   *
   * @param n the minimal number of elements required in the parsed list
   */
  parse_list_sep_min_length(n, sep_at_beg, elem, sep) =
    parse_list = parse_list_sep(sep_at_beg, elem, sep)
    parser res={parse_list} {if List.length(res) < n then fail else succeed} -> res

  /**
   * List parsing with a given separator.
   *
   * As {!parse_list_sep} with [sep_at_beg=false].
   */
  parse_list(elem, sep) =
    parse_list_sep(false, elem, sep)

  /**
   * Parsing of a non-empty list with a given separator.
   *
   * As {!parse_list_sep_min_length} with [sep_at_beg=false] and [n=1].
   */
  parse_list_non_empty(elem, sep) =
    parse_list_sep_min_length(1, false, elem, sep)


  /**
   * {2 Parsers for basic data types}
   */

  /**
   * This is a void parser that has no effect on parsing but as a side
   * effect calls [f] with a single parameter being the input to be
   * parsed at that point. This can be useful for debugging parsers.
   *
   * @param f a function accepting a single string parameter, which
   * will be called with the input to be parsed.
   */
  debug_parse_string : (string -> 'a) -> Parser.general_parser('a) =
    f -> parser res=&(s=(.*) -> f(Text.to_string(s))) -> res

  /**
   * A parser for a single digit.
   */
  digit = parser val=([0-9]) -> Int.of_string(Text.to_string(val))

  /**
   * A parser for an unsigned integer (i.e. a non-empty list of digits).
   */
  natural = parser uint=([0-9]+) -> Int.of_string(Text.to_string(uint))

  /**
   * A parser for an unsigned integer of a fixed length, of [n] digits.
   * Parsing of numbers shorter than [n] digits will fail.
   * Parsing of numbers longer than [n] digits will only consume the
   * first [n] digits.
   */
  fixed_length_natural(n) = parser uint=({rep(n, digit)}) -> Int.of_string(Text.to_string(uint))

  /**
   * A parser for an integer (i.e. a list of digits, optionally prefixed
   * with the minus sign '-'; no spaces allowed between the '-' and the
   * digits).
   */
  integer = parser sign="-"? val=natural -> Option.switch(_ -> -1, 1, sign) * val

  /**
   * Same as Rule.integer except for 64-bit integers.
   */
  int64 = parser sign="-"? val=([0-9]+) -> Int64.of_string(Option.switch(_ -> "-", "", sign) ^ Text.to_string(val))

  /**
   * A parser for byte (integer between 0 and 255
   */
  byte = parser
    x=( | ("25" [0-5])
        | ("2" [0-4] Rule.digit)
        | ("1" Rule.digit Rule.digit)
        | ([1-9] Rule.digit)
        | Rule.digit ) -> Int.of_string(Text.to_string(x))

  /**
   * A parser for a float (i.e. a list of digits, optionally prefixed
   * with the minus sign '-'; no spaces allowed between the '-' and the
   * digits).
   */
  float = parser
    | t=(integer "."? digit*) -> Float.of_string(Text.to_string(t))
    | t=("-"? "." natural) -> Float.of_string(Text.to_string(t))

  /**
   * A parser for a float between 0 and 1
   */
  float_0_1 = parser
    | "1" ("." "0"*) ? -> 1.
    | t=("0"? "." natural) -> Float.of_string(Text.to_string(t))

  /**
   * A parser for a single white space character.
   */
  white_space = parser
    | " "
    | "\t"
    | "\r"
    | "\n"
    | "\011"
    | "\000";

  /**
   * A parser for an arbitrary number of white space character.
   *
   * @return parser of type [Parser.general_parser(void)]
   */
  ws = parser white_space* -> void;

  /**
   * @return A parser recognizing end of a line
   */
  eol = parser [\n] -> void

  /**
   * A parser for a non zero number of white space character.
   *
   * @return parser of type [Parser.general_parser(void)]
   */
  strict_ws = parser white_space+ -> void;

  /**
   * A parser for a hexadecimal character, i.e. a digit or a letter
   * in range A-Z (case insensitive). The result of parsing is a
   * decimal value associated with the given hexadecimal character.
   */
  hexadecimal = parser
    | val=digit -> val
    | ("a" | "A") -> 10
    | ("b" | "B") -> 11
    | ("c" | "C") -> 12
    | ("d" | "D") -> 13
    | ("e" | "E") -> 14
    | ("f" | "F") -> 15

  /**
   * A parser for a hexadecimal numbers, i.e. a sequence of
   * hexadecimal characters as above. The result of parsing is a
   * decimal value associated with the given hexadecimal number.
   */

  hexadecimal_number = parser
    | l=hexadecimal+ -> List.fold((i, acc -> acc * 16 + i), l, 0)

  /**
   * A parser that checks for end of input. It succeeds only on
   * empty input, giving {!void} as result.
   */
  eos = parser !. -> void

  /**
   * A parser that always fails (it can be useful in some parsing
   * combinators).
   */
  fail = parser eos [.] -> error("[Rule.fail] Internal parsing error.")

  /**
   * A parser that always succeeds, without consuming any input.
   */
  succeed = parser "" -> void

  /**
   * A parser that always succeeds, and consume all its input and returns it
  **/
  consume = parser v=(.*) -> Text.to_string(v)
    // FIXME: inefficient, see low level implementation of opa parser

  /**
   * @return A parser that consumes (and returns) the rest of the line
   */
  full_line = parser txt=((!eol .)*) eol? -> Text.to_string(txt)

  /**
   * A parser combinator that succeed without consuming any input
   * when given [{true}], or fails when given false
   */
  succeed_if(b) = if b then Rule.succeed else Rule.fail

  succeed_opt(o) =
    match o with
    | {none} -> Rule.fail
    | {some = v} -> parser Rule.succeed -> v

  /**
   * A parser for a single alphanumerical characters, i.e. a
   * parser that accepts a single letter (small or capital) or
   * digit and returns it as a string.
   */
  alphanum_char = parser v=([a-zA-Z0-9]) -> Text.to_string(v)

  /**
   * As {!alphanum_char} but a non-empty sequence of alphanumerical
   * characters is accepted.
   */
  alphanum_string = parser v=(Rule.alphanum_char+) -> Text.to_string(v)

  /**
   * A parser for identifiers, i.e. a non-empty list of
   * alpha-numerical characters and underscores.
   */
  ident = parser v=((alphanum_char | [_])+) -> Text.to_string(v)

  /**
   * A parser for boolean values. Accepts strings "false" and "true" and
   * gives respective boolean values.
   */
  bool = parser
    | [Tt] "rue" -> true
    | [Ff] "alse" -> false

  /**
   * A parser that checks whether the suffix of the input succeeds
   * with respect to some given parser [p].
   *
   * @param p some parser
   * @return This function returns a parser which succeeds only if
   * the input has a suffix that parses correctly with [p]. The result
   * of this parser is a pair [(text, res)] where [text] is the text
   * consumed before parsing with [p] and [res] is the result of parsing
   * the suffix. If there are many suffixes on which [p] succeeds then
   * the longest one will be chosen.
   */
  has_suffix(p : Parser.general_parser('res)) : Parser.general_parser((text, 'res)) =
    parser prefix=((!({p} eos) .)*) suffix={p} eos -> (prefix, suffix)

  /**
   * Wrap a standard function to obtain a parser.
   *
   * @param s a function with type bool, parser_input -> 'a
   *
   * @return a parser of type Parser.general_parser('a).
   */
  function_to_parser(f) = f : Parser.general_parser('a)

  /**
   * Produces a parser for a given string literal.
   *
   * @param s string literal to be turned into a parser.
   *
   * @return a parser for string literal [s].
   */
  of_string(s) =
    function_to_parser(partial, input ->
      Parser_private.check_partial(partial, Parser_private.parse_literal(input, s))
    )

  /**
   * Produces a parser for a given text literal.
   *
   * @param t text literal to be turned into a parser.
   *
   * @return a parser for text literal [t].
   */
  of_text(t) =
    of_string(Text.to_string(t))

  /**
   * Produces a case-insensitive parser for a given string literal.
   *
   * @param s string literal to be turned into a parser.
   *
   * @return a case-insensitive parser for string literal [s].
   */
  of_string_case_insensitive(s) =
    function_to_parser(partial, input ->
      Parser_private.check_partial(partial, Parser_private.parse_literal_case_insensitive(input, s))
    )

  of_parsers(parsers:list(Parser.general_parser('a))): Parser.general_parser('a) =
  (
     init: Parser.general_parser('a) = fail
     f(p: Parser.general_parser('a), acc: Parser.general_parser('a)): Parser.general_parser('a) =
        parser x={p}   -> (x:'a)
            |  y={acc} -> (y:'a)
     List.fold(f, List.rev(parsers), init)
  )

  of_map(map:stringmap('a)):Parser.general_parser('a) =
  (
     f(key, result, acc) =  [(parser {Rule.of_string(key)} -> result) | acc]
     of_parsers(StringMap.fold(f,map,[]))
  )

  /**
   * Transform an ['a] parser to an ['b] parser using the map function [f].
   */
  map(p:Parser.general_parser('a), f):Parser.general_parser('b) = i, b ->
    match p(i, b) with
    | {none} -> none
    | {some = (i, r)} -> {some = (i, f(r))}

  /**
   * Pipe parser [pi] to the [p] parser.
   * @param pi The parser which translate the input
   * @param p The main parser
   * @return A parser which inputs are decoded by [pi] then parsed by [p].
   */
  pipe(pi:Parser.general_parser(itextrator), p:Parser.general_parser('a))
    :Parser.general_parser('a) = b, i ->
    match pi(b, i) with
    | {none} as e -> e
    | {some = (_, r)} -> p(b, r)



}}

/*
 * FIXME: explain why this is exported to the global namespace.
 * If it is part of @opacapi, use the directive, if not, remove.
 */
rule_of_string = Rule.of_string
