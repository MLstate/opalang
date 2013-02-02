/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Elvis
 *
 * @category UI
 * @author David Rajchenbach-Teller, 2011
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */

/**
 * An elvis ("element of vision") is a data structure that can be displayed in the browser screen.
 * Elvises are abstract data structures, characterized by the events they can post (the "sources")
 * and their accessors.
 *
 * ** Relationships between Elvises and Xhtml
 *   - elvises are not xhtml (although they are certainly implemented as such)
 *   - elvises are not referenced by their ID
 *   - for display, elvises are inserted into xhtml
 *   - styling, positioning, etc. are done with CSS and selectors, through the indirection of the [Styler] widget
 *   - DOM events are used to implement Elvis, but they are low-level informations, not meant to be seen outside the Elvis
 *
 * ** Events
 *   - each Elvis publishes a number of [source]s, corresponding the events it can send
 *   - a channel or a function can be registered to listen on a [source], and possibly unregistered later
 *   - an Elvis MUST NOT listen to its own [source]s
 *
 * ** Implementation guidelines
 *   - Each xhtml node in an Elvis should have a class name
 */

/**
 * {1 Standard event names}
 *
 * - [{chosen:       void}] -- a button was pressed, an item was selected in a list, etc.
 * - [{value_changed: {old: 'a new:'a}}]
 * - [{value_rejected:{old: 'a rejected:'b}}]
 */


@abstract type Elvis.elvis('sources, 'implem) = {
   sources: 'sources /**Sources of events that the elvis can send (e.g. "selected", "value changed", etc.)
                        Use this to register event observers (or, possibly, to trigger artificial events).

                        Note: By design, an elvis MUST NOT register with its own sources.*/
   implem:  'implem  /**Anything that may be needed to access the functions of this elvis, e.g. to set content*/
   display:  Elvis.theme ->/**Anything required to (re)display the elvis.*/
   {
     xhtml: xhtml  /**The xhtml code for the display part of this elvis.*/
     dom:   dom    /**A {e concrete} [dom] corresponding to [xhtml] page.*/
   }
}
@abstract type Elvis.masked = {}
@abstract type Elvis.theme  = list(string)

Elvis = {{
  /**
   * Convert an elvis into something that can be injected on a page
   *
   * Note: Called automatically by magic_to_xhtml
   */
  for_display(elvis: Elvis.elvis(_, _)): xhtml =
  (
     for_display_in_theme(elvis, (["mlstate_default"]))
  )

  /**
   * Convert an elvis into something that can be injected on a page
   */
  for_display_in_theme(elvis: Elvis.elvis(_, _), theme: Elvis.theme): xhtml =
  (
     (elvis).display(theme).xhtml
  )

  /**
   * Access the event sources of the elvis
   */
  sources(elvis: Elvis.elvis('sources, 'implem)): 'sources =
  (
    (elvis).sources
  )

  implem(elvis: Elvis.elvis('sources, 'implem)): 'implem =
  (
    (elvis).implem
  )

  /**
   * Construct an Elvis
   */
  make(sources: 'sources, implem: 'implem, display: Elvis.theme -> {xhtml:xhtml; dom:dom}): Elvis.elvis('sources, 'implem) =
  (
     ({~sources ~implem ~display})
  )

  /**
   * Existential stuff
   */
  pack(elvis: Elvis.elvis(_, _)):          Elvis.elvis(Elvis.masked, Elvis.masked) =
  (
    ({sources=masked implem=masked display=(elvis).display})
  )

  pack_sources(elvis: Elvis.elvis(_, 'b)): Elvis.elvis(Elvis.masked, 'b) =
  (
    ({sources=masked implem=(elvis).implem display=(elvis).display})
  )

  pack_implem(elvis: Elvis.elvis('a, _)):  Elvis.elvis('a, Elvis.masked) =
  (
    ({sources=(elvis).sources  implem=masked display=(elvis).display})
  )

  masked: Elvis.masked = ({})

  Theme =
  {{
     of_classes(classes: list(string)): Elvis.theme =
     (
        (classes)
     )

     get_classes(theme: Elvis.theme): string =
     (
        List.to_string_using("", "", " ", (theme))
     )

     add_class(class:string, theme: Elvis.theme): Elvis.theme =
     (
        ([class | (theme)])
     )
  }}
}}
