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
 * An abstraction mechanism of a XHTML tag style
 *
 * @author Guillem Rieu, 2010
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */


/**
 * {1 About this module}
 *
 * This module provides an abstraction mechanism of a XHTML tag style: styles
 * handling is made consistent whether the actual underlying style is made of
 * classes or embedded CSS.
 *
 * {1 Where should I start?}
 *
 * Constructors {!WStyler.empty_styler}, {!WStyler.style}, {!WStyler.class} and
 * {!WStyler.styler} can be used to create records of type [WStyler.styler].
 *
 * A styler can be attached to a XHTML element with either {!WStyler.add_styler}
 * or {!WStyler.set_styler} (the first function complements the current style,
 * latter replaces it).
 *
 * To apply a styler to a DOM element, the function {!WStyler.set_styler_dom}
 * should be used.
 *
 * {1 What if I need more?}
 *
 * You can take a look at the {Css} and {Xhtml} modules to learn more about
 * styles in OPA.
 *
 */

type WStyler.styler =
    { class: list(string) }
  / { style: Css.properties }
  / { class: list(string) style: Css.properties }

WStyler =

  tuple_of_styler(styler: WStyler.styler): (list(string), Css.properties) =
    match styler with
      { ~class ~style } -> (class, style)
      { ~class }        -> (class, [])
      { ~style }        -> ([], style)

  styler_of_tuple(tuple: (list(string), Css.properties)): WStyler.styler =
    match tuple with
      | ([], [])   -> WStyler.empty
      | (cls, [])  -> WStyler.make_class(cls)
      | ([], stl)  -> WStyler.make_style(stl)
      | (cls, stl) -> WStyler.make(cls, stl)

{{

  /**
   * {1 High-level interface
   */

  /**
   * {2 Constructors}
   */

  /**
   * The empty styler.
   */
  empty: WStyler.styler = { style = [] }

  /**
   * Constructs a styler from Css properties.
   */
  make_style(css: Css.properties): WStyler.styler = { style = css }

  /**
   * Constructs a styler from a list of classes.
   */
  make_class(names: list(string)): WStyler.styler = { class = names }

  make(names: list(string), css: Css.properties): WStyler.styler =
    { class = names style = css }

  /**
   * {2 XHTML handling functions}
   */

  /**
   * Update the current element style with a given styler.
   *
   * @param styler The styler to attach
   * @param elt The XHTML element on which to apply the styler
   * @return The resulting XHTML element with new styling information
   */
  add(styler: WStyler.styler, elt: xhtml): xhtml =
    current_styler = get(elt)
    new_styler = merge([current_styler, styler])
    set(new_styler, elt)

  /**
   * Attach a styler (classes, a style, or both) to a XHTML element, possibly
   * replacing the current style.
   *
   * @param styler The styler to attach
   * @param elt The XHTML element on which to apply the styler
   * @return The resulting XHTML element with new styling information
   */
  set(styler: WStyler.styler, elt: xhtml): xhtml =
    match elt with
      | { specific_attributes=attrs_opt ~namespace ~tag ~args ~content } ->
          attrs = default(Xhtml.default_attributes, attrs_opt)
        (cls, stl) = tuple_of_styler(styler)
        { specific_attributes = some({ attrs with
              class = cls
              style = stl
            })
          namespace=namespace tag=tag args=args content=content }
      | _ -> elt

  /**
   * Retrieve the current style of a XHTML element
   *
   * @param elt The XHTML element
   * @return The styler corresponding to the XHTML element style
   */
  get(elt: xhtml): WStyler.styler =
    match elt with
      | { specific_attributes=attrs_opt namespace=_ tag=_ args=_ content=_ } ->
        attrs = default(Xhtml.default_attributes, attrs_opt)
        styler_of_tuple((attrs.class, attrs.style))
      | _ -> empty

  /**
   * Merge a list of stylers into a single one
   *
   * @param stylers A list of stylers
   * @return The resulting styler
   */
  merge(stylers: list(WStyler.styler)): WStyler.styler =
    rec aux(styler, (cls, stl)) = (match styler with
      | { ~class ~style } -> (cls ++ class, stl ++ style)
      | { ~class } -> (cls ++ class, stl)
      | { ~style } -> (cls, stl ++ style))
    styler_of_tuple(List.fold(aux, stylers, ([],[])))


  /**
   * {1 DOM handling functions}
   */

  /**
   * Attach a styler to an existing DOM element
   *
   * @param styler The styler to attach
   * @param id The HTML id of the DOM element
   */
  set_dom(styler: WStyler.styler, id: string): void =
    dom = #{id}
    action_class(cls) = List.iter(Dom.add_class(dom, _), cls)
    action_style(stl) = Dom.set_style(dom, stl)
    do Dom.void_class(dom)
    match styler with
      { ~class ~style } -> do action_class(class); action_style(style)
      { ~class }        -> action_class(class)
      { ~style }        -> action_style(style)

  /**
   * Remove all styling attributes from an existing DOM element
   *
   * @param id The HTML id of the DOM element
   */
  clear_dom(id: string): void =
    dom = #{id}
    do Dom.void_style(dom)
    do Dom.void_class(dom)
    void

}}
