/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Common functions related to widgets
 *
 * @author Guillem Rieu, 2010
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */

// TODO: high-level API
// TODO: merge install and make?


WCore =

  /*
   * Construct a list of handle_assoc(xhtml_event) ready to be added to a
   * tag event list.
   *
   * @param handles Association list (handle, action)
   * @return A [handle_assoc(xhtml_event)] list
   */
  cons_binds(handles)
      : list(handle_assoc(xhtml_event)) =
    List.rev_map((evt, action) -> // Faster than List.map?
      hassoc(evt, {expr= e -> action(e)}), handles)

{{

  /**
   * Function taking a selector and a pair of (xhtml, f), where f is
   * a function having a side effect on the given selector, necessary to
   * initialize the widget whitin the page. f is called and the xhtml
   * chunk is returned.
   *
   * @param sel The selector of the DOM element on which to apply side effects
   * @param widget A pair of a XHTML chunk and a function conducting side effects
   */
  install(sel: Dom.private.element, (chunk: xhtml, fun_install: Dom.private.element -> void)): xhtml =
    do fun_install(sel)
    chunk

  /**
   * A shortcut to {!WCore.install} with the whole document as selector
   * on which to install the handles.
   */
  install_document(widget: (xhtml, (Dom.private.element -> void))): xhtml =
    install(Dom.of_selection(Dom.select_document()), widget)

  /**
   * A shortcut to {!WCore.install} with the window as selector
   * on which to install the handles.
   */
  install_window(widget: (xhtml, (Dom.private.element -> void))): xhtml =
    install(Dom.of_selection(Dom.select_window()), widget)

  /**
   * Outer container initialization function
   *
   * The default {!WCore.make} encapsulates the given [content] in a div,
   * and is thus, suited for widgets appearing as an inpependent block.
   *
   * @param classes A list of classes to associate to the div
   * @param style The CSS style to associate to the div
   * @param id The main ID designating the widget
   * @param content The content of the widget, as returned by widget functions
   */
  make(classes: list(string), style: Css.properties,
       id: string, content: xhtml): xhtml =
    <div class={classes} id=#{id} style={style}>
      {content}
    </div>

  /**
   * A shortcut to {!WCore.make} with default classes and a default style.
   */
  make_default(id: string, content: xhtml): xhtml =
    make(["widget"], [], id, content)

  /**
   * {!WCore.make_inline} has the same role as make, but produces a span
   * instead of a div.
   *
   * @param classes A list of classes to associate to the div
   * @param style The CSS style to associate to the div
   * @param id The main ID designating the widget
   * @param content The content of the widget, as returned by widget functions
   */
  make_inline(classes: list(string), style: Css.properties,
              id: string, content: xhtml): xhtml =
    <span class={classes} id=#{id} style={style}>
      {content}
    </span>

  /**
   * A shortcut to {!WCore.make} with default classes and a default style.
   */
  make_inline_default(id: string, content: xhtml): xhtml =
    make_inline(["widget"], [], id, content)

  /**
   * Bind actions to events on the tag contained in [xhtml]
   *
   * @param handles An association list (event, handle) of handles
   * @param xhtml The XHTML chunk on which install the handles
   */
  add_binds(handles, xhtml: xhtml): xhtml =
    Xhtml.add_binds(cons_binds(handles), xhtml)

  /**
   * Compute the prefix_class from a config containing a prefix_class field of type option(string)
   * If no prefix_class is found, returns the prefid_id in parameter
   *
   * @param config the config to use
   * @param prefix_id the prefix_id to use in case no prefix_class was defined
   */
  compute_prefix_class(config:'config, prefix_id:string) =
    match config.prefix_class with
    | ~{some} -> some
    | {none} -> prefix_id

}}
