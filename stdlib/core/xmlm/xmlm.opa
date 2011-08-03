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

/**
 * Binding for Xmlm
 *
 * Server-side only, fast utilities for manipulating XML.
 *
 * @stability UNSTABLE
 */


import stdlib.core.{xhtml}
/* NB: we depend on stdlib.xhtml essentially because the translation to xmlns is here
   (and also because of the type string_assoc, but it could be put elsewhere).
   In brief, we can easily inverse the dependencies if we want.
*/


/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

type Xmlm.signal =
    { Data : string }
  / { Dtd : option(string) }
  / { El_end }
  / { El_start; namespace : string; tag : string; args : list(string_assoc(string)) }

/**
 * {1 Interface}
 */

Xmlm =
{{
  /**
   * Produces a [Xmlm.signal] imperative iterator
   *
   * Note: as more capabilities of Xmlm are bound to OPA, this function could take more options as arguments
   */
  make_scanner = %%bslXmlm.make_scanner%% : string -> (-> option(Xmlm.signal))

  parse_tree = %%bslXmlm.parse_tree%% : string -> option(xmlns)

  /**
   * Same as make_scanner but with default options
   *
   * Note: provided for compatibility while make_scanner may evolve; don't change its interface
   */
  make_default_scanner : string -> (-> option(Xmlm.signal)) = make_scanner

  /**
   * Pops the imperative iterator so as to recover a valid piece of xml
   *
   * @param allow_fragments a boolean flag to allow the result to be a
   * fragment; setting it to false will pop the iterator more lazily,
   * recovering only one well-formed xml at a time
   * @param scanner a [Xmlm.signal] imperative iterator, such as returned by [make_scanner]
   */
  to_xmlns(allow_fragments : bool, tolerant:bool, scanner : (-> option(Xmlm.signal))) : option(xmlns) =
    rec aux(allow_fragments : bool, acc : list(xmlns)) : option(list(xmlns)) =
      return(elt) =
        if allow_fragments
          then aux(allow_fragments, [elt | acc])
          else some([elt]) ;
      match scanner() with
      | {none} ->
        if tolerant
          then some([])
          else none
      | {some = signal} ->
        match signal with
        | { Dtd = _ } ->
          aux(allow_fragments, acc)
        | { Data = d } ->
          return({ text = d })
        | { El_start; ~namespace; ~tag; ~args } ->
          match aux(true, []) with
          | {none} -> none
          | {some = r} ->
            content = List.rev(r)
            elt = { ~namespace; ~tag; ~args; ~content; specific_attributes = none }
            return(elt)
          end
        | { El_end } ->
          some(acc)
        end
      end
    match aux(allow_fragments, []) with
    | {none} | {some = []} -> none
    | {some = [x]} -> some(x)
    | {some = xs} -> if allow_fragments then some({ fragment = List.rev(xs) })
            else error("Internal error: Xmlm.to_xmlns returned a fragment, but that was not allowed")
  ;

  @private text_node_empty(node) = match node with
  | { ~text } -> String.is_blank(text)
  | _ -> false

  @private strip_side(input, strip_fun, add_fun):string =
    stripped = strip_fun(input)
    if String.length(stripped) != String.length(input)
      then add_fun(stripped)
      else stripped

  @private strip_to_one_space(node:xmlns):xmlns = match node with
  | { ~text } ->
    left_stripped = strip_side(text, String.strip_left, (a -> " {a}") )
    stripped = strip_side(left_stripped, String.strip_right, (a -> "{a} ") )
    { text = stripped }
  | other -> other

  @private remove_blank_node(xmlns:xmlns) : xmlns =
    process_children(children:list(xmlns)):list(xmlns) =
      filtered_children = List.filter((node -> not(text_node_empty(node))), children)
      filtered_children = List.map(strip_to_one_space, filtered_children)
      List.map(remove_blank_node, filtered_children )
    match xmlns with
    | { ~fragment } ->
      { fragment = process_children(fragment)  }
    | { ~content; ~namespace; ~tag; ~args; ~specific_attributes } ->
      new_content= if tag == "pre"
        then content
        else process_children(content)
      { ~namespace; ~tag; ~args; ~specific_attributes; content=new_content }
    | other -> other
    end

  /**
   * A all-in-one try_parse, as in Xmlns
   */
  try_parse(s : string) : option(xmlns) =
    Option.map(remove_blank_node, parse_tree(s))
}}
