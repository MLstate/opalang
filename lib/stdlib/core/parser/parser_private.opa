/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * @author Corentin Gallet, October 2009 (now using text iterators)
 * @author Adam Koprowski, February 2010 (clean-up, extensions, documentation, ...)
 * @category parsing
 * @destination private
 */

/**
 * {1 About this module}
 *
 * This module contains some low-level functions for parsing, as used by TRX.
 * We probably do not want to expose this interface to end-users.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

/**
 * [Parser.general_parser('a)] is a type of a parser that returns a value of type ['a].
 * Such a parser needs to be given a boolean (indicating whether we want a partial
 * parsing, where partial parsing means that there may be some input remaining after
 * parsing, whereas with non-partial parsing all input must be consumed) and a text
 * iterator and it returns either [none] if parsing failed or a pair [some((i, v))]
 * where [i] is a new text iterator (at the new position in the input) and [v] is
 * the value associated with the parsed construct.
 */
type Parser.private_general_parser('a) = bool, itextrator -> option((itextrator, 'a))
@opacapi
type Parser.general_parser('a) = Parser.private_general_parser('a)

/**
 * [Parser_private.range] represents a range of characters.
 */
type Parser_private.range = { one : Unicode.character }
                              /** a single character [one] */
                          / { from : Unicode.character ; to : Unicode.character }
                              /** a range of characters between [from] and [to] */
                          / { any }
                              /** an arbitrary character */

/**
 * {1 Interface}
 */

Parser_private =
{{

  check_partial_lazy(partial, it, res) =
    if partial then
      some((it, res()))
    else
      match Itextrator.next(it) with
      | {none} -> some((it, res()))
      | {some=_} -> none

  check_partial(partial, result) =
    if partial then
      result
    else
      match result : option with
      | {none} -> none
      | {some=(it, _res)} ->
        match Itextrator.next(it) with
        | {none} -> result
         /* parsing is non-partial and there is some input left, so parsing fails */
        | {some=_} -> none

  /**
   * [char_equal(case_sensitive, c1, c2)] is true iff characters [c1] and [c2] are
   * equal. If [case_sensitive=true] then the comparison should be case-sensitive.
   */
  char_equal(case_sensitive, c1, c2) =
    c1 == c2 ||
    (not(case_sensitive) &&
       Unicode.lowercase(c1) ==
       Unicode.lowercase(c2))

  /**
   * [match_char(c, r)] returns true iff character [c] matches
   * any of the ranges in [r].
   */
  match_char(c, l)=
    rec aux(range) = match range : list(Parser_private.range) with
      | [] -> false
      | [{any} | _] -> true
      | [{~one} | tl] -> one == c || aux(tl)
      | [{~from ~to} | tl] -> (c >= from && c <= to) || aux(tl)
    aux(l)

  @private
  no_progress(it1,it2) =
    Itextrator.pos(it1):int == Itextrator.pos(it2)

  /**
   * [primary_list] repeatedly applies a given parsing function on the input
   * for as long as it succeeds. The result of [primary_list] is a list of
   * results produced by the parsing function. If [is_plus] then the function
   * must succeed at least once, or else [primary_list] fails.
   */
  primary_list(is_plus, f, init_pos) =
    rec aux(acc, pos) =
      match f(pos) : option with
      | {some = (newpos, res)} ->
          if no_progress(pos,newpos) then (pos, List.rev(acc))
          else aux(res +> acc, newpos)
      | _ ->
          (pos, List.rev(acc))
    res = aux([], init_pos);
    if is_plus && List.is_empty(res.f2) then
      none
    else
      some(res)

  /**
   * As [primary_list] but we do not care about the result.
   */
  primary_list_no_res(is_plus, f, init_pos) =
    rec aux(pos) =
      match f(pos) : option with
      | {some = (newpos, _)} ->
          if no_progress(pos,newpos) then pos
          else aux(newpos)
      | _ -> pos
    pos = aux(init_pos)
    if is_plus && no_progress(init_pos,pos) then none
    else some((pos,void))

  /*
   * [parse_literal(it, literal)] checks whether the text contained by
   * the iterator [it] corresponds to a string [literal]. If this is the
   * case then the function returns [some(it', literal)], where [it'] is [it]
   * after consuming its [literal] prefix; otherwise the function returns [none].
   */
  parse_literal(it, literal) =
    itpos = Itextrator.pos(it)
    ittxt = Itextrator.txt(it)
    if itpos + String.length(literal) > String.length(ittxt) then
      none
    else
      if String.check_substring(ittxt, itpos, literal) then
        new_it = Itextrator.forward(it, String.length(literal))
        some((new_it, literal))
      else
        none

  /**
   * As [parse_literal_case_sensitive] but the comparison of characters is
   * *not* case-sensitive.
   */
  parse_literal_case_insensitive(it, literal) =
    rec aux(input : itextrator, literal : itextrator) =
      match Itextrator.next(literal) with
      | {none} -> some(input)
      | {some = (new_literal, c1)} ->
        match Itextrator.next(input) with
        | {none} -> none
        | {some = (new_input, c2)} ->
          if char_equal(false, c1, c2) then
            aux(new_input, new_literal)
          else
            none
    literal_it = Itextrator.make(literal)
    match aux(it, literal_it) with
    | {none} -> none
    | {some = new_it} -> some((new_it, literal))

  /**
   * [parse_range(it, l)] takes a text iterator [it] and a list of ranges [l]
   * and returns [none] if none of the elements represented in [l] conforms
   * to the first character in [it], and otherwise it returns [some(it',c)]
   * where [it'] is [it.next()] and [c] is the first character of [it].
   */
  parse_range(it : itextrator, l) =
    res = Itextrator.next(it) : option
    match res with
    | {none} ->
        none
    | {some = (_, c)} ->
        if match_char(c, l) then
          res
        else
          none

  /**
   * Generic function for testing that parsing and pretty-printing functions
   * are inverses of each other. We cannot simply check for:
   *
   *   [to_string(of_string(s)) == s]
   *
   * as the input string will typically have some stuff that will be thrown
   * away in its data representation types (think: white space). But instead
   * we perform this test with its normalized version i.e. with:
   *
   *   [s_normalized = to_string(of_string(s))]
   *
   * we check that:
   *
   *   [to_string(of_string(s_normalized)) == s_normalized]
   *
   * So in a sense we check for the fixed point of the initial condition.
   *
   * @param to_string a function converting from this data type to string
   * @param of_string a function converting from string to some data type
   * @param s a string on which to test whether parsing and pretty printing
   * are inverses in the above sense.
   * @return a boolean value indicating whether the test was successful
   */
  test_parsing(to_string, of_string, s) =
    process = compose(to_string,of_string)
    s_normalized = process(s)
    process(s_normalized) == s_normalized

  /**
   * As {!test_parsing} only now the [of_string] function gives an
   * option type (i.e. parsing may fail). Though, the test succeeds
   * only if it does not (on given inputs).
   * @param correct indicates whether the given input string is correct,
   * if not then the test succeeds if the parsing fails.
   */
  test_try_parsing(to_string, of_string, s, correct) =
    process(s) = Option.map(to_string, of_string(s))
    if correct then
      match process(s) with
      | {none} -> false
      | {some = v1} ->
        match process(v1) with
        | {none} -> false
        | {some = v2} -> v1 == v2
    else
      process(s) == none

  /**
   * As {!test_try_parsing} but we start from a data element and not from
   * a string and check that the value after printing and parsing is the same
   * one that we started with.
   */
  test_try_parsing_with_data(to_string, of_string, data, correct) =
    dp = of_string(to_string(data))
    if correct then
      match dp with
      | {none} -> false
      | {some = new_data} -> data == new_data
    else
      dp == none

}}
