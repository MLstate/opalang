/*
    Copyright © 2011, 2012 MLstate

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
 * Low-level interaction with the user-interface
 *
 * @author David Rajchenbach-Teller, 2010
 * @target PUBLIC
 * @stability EXPERIMENTAL
 */

import stdlib.core.{web.core}

/**
 * {1 About this module}
 *
 * This module defines low-level primitives to manipulate the contents of the page currently displayed
 * by the browser. You can use it to add or remove from the page, to get the contents of a form, to
 * apply styles or special effects.
 *
 * {1 Where should I start?}
 *
 * Most functions in this module require as argument a {!dom}, that is the selection of some elements
 * on the page, so you'll first need to get this {!dom}. If the content you wish to manipulate is already
 * on the page, you will need to {e select} it using one of the functions [Dom.select_...], for instance
 * {! Dom.select_id}. If the content is not in the page yet, you will need to {e insert} it using function
 * {! Dom.insert}.
 *
 * To move items on the page, take a look at {!Dom.move_after}
 *
 * {1 what if i need more?}
 */

/**
 * {1 Types defined in this module.}
 */

/**
 * The type of a selection in a document.
 *
 * Note for advanced users: By default, selections are browser-independent and lazy. In other
 * words, you may create selection [#foo] even on the server, and even if there is no client connected.

Selections are resolved lazily. In other words, you may create selection [#foo]
 * even if your document doesn't contain any element with id [foo] yet. Whenever you
 * apply functions of module {!Dom}, the document will be scanned for an
 * element matching your selection.
 *
 * You can invoke function {!Dom.resolve} to resolve the selection at a given time.
 */
@opacapi
type @abstract dom = Dom.private.selection

/**
 * The type of an animation in the Dom
 *
 * Animations are special effects such as sliding in, fading in, fading our or fading to, etc.
 * Animations can be composed, customized and extended.
 */
type @abstract Dom.animation = Dom.private.animation

type Dom.Effect.duration = {immediate} / {slow} / {fast} / {default} / {millisec: int}

/**
 * An actual Dom element, as represented by the browser
 */
type Dom.private.element = dom_element//TODO: external

type Dom.Dimension.t = Dom.dimensions//TODO: remove
type Dom.event_handler = external

/**
 * The current on-screen selection
 */
type Dom.selected =
{
   start_in:    dom;
   start_at:    int;
   finish_in:   dom;
   finish_at:   int
   is_collapsed:bool;
}

/**
 * The current on-screen selection
 *
 * Manipulated internally
 */
type Dom.private.selected =
{
   start_in:    dom_element;
   start_at:    int;
   finish_in:   dom_element;
   finish_at:   int
   is_collapsed:bool;
}

/*
 * Note: We use this algebraic type and not directly [Dom.private.element] to
 * allow preparing requests from the server.
 */
type Dom.private.selection =
   { all }             /**Select everything, in depth*/
 / { document }
 / { window }
 / { id:string }
 / { class:string }
 / { inside: Dom.private.selection; select: Dom.private.selection }
 / { selector: string } /**A literal CSS selector*/
 / { shallow }          /**Select everything at this level*/
 / { contents }          /** Select contents */
 / { concrete: Dom.private.element }/**The concrete client-side representation of a selection.*/
//Note: in the future, we will certainly add new selectors


/**
 * Private type of an animation.
 */
type Dom.private.animation =
     {apply: Dom.private.element, option(/*duration_ms*/int), option(string), option(Dom.private.element -> void) -> void}
     /**In [{apply = f}], [f(dom,cb,dur)] will be called to progressively apply transformations
        to element [dom]. If [cb] is not [{none}], the contained function will be called once the
        transformation is complete. If [{dur}] is not [{none}], the contained number will contain
        a suggested duration for the transformation.
        Transformations take place asynchronously.*/
    / {with_duration: int/*ms*/; embedded: Dom.private.animation}
     /**Apply some transformation with a given duration (given in ms)*/
    / {with_easing: string; embedded: Dom.private.animation}
     /**Apply some transformation with a given easing (given as a name)*/
    / {sequence: list(Dom.private.animation)}
     /**Compose a sequence of transitions. Each transition will be called as soon as the previous
        one has been terminated.*/

type @abstract Dom.transition = Dom.private.element

`$` = Dom.select_id

/**
 * {1 Interface}
 */

Dom = {{

  /**
   * {2 Fresh}
   */
  fresh_id() = Random.string(32)


  /**
   * {2 Selecting elements}
   */

  /**
   * Select the element with a given id [{name}], if it exists.
   *
   * @param name The name of the element.
   * @return An empty selection if the element doesn't exist
   *
   * Note: Elements which haven't been inserted in the page cannot be selected yet.
   */
  select_id(name: string): dom =
  (
    {id = name}
  )

  /**
   * Select everything in the document
   */
  select_all(): dom =
  (
   {all}
  )

  /**
   * Select the document itself
   */
  select_document(): dom =
  (
    {document}
  )

  /**
   * Select the body of the document
   */
  select_body(): dom =
  (
    {id = "Body"}
  )

  /**
   * Select the browser's window
   */
  select_window(): dom =
  (
    {window}
  )

  /**
   * Select all the items belonging to a class
   */
  select_class(name: string): dom =
  (
    {class = name}
  )

  /**
   * Combine two selections.
   *
   * Note: selecting the [{window}] inside another selection is pointless
   * @param container The outer selection
   * @param selector The inner selection
   * @return The elements of [selector] appearing inside [container]
   */
  select_inside(container: dom, selector: dom): dom =
  (
    {inside = container
     select = selector}
  )

  /**
   * Select all the children of a selection
   *
   * Note: This does {e not} select text nodes.
   */
  select_children(container: dom): dom =
  (
     {inside = container
      select = {shallow}}
  )

  /**
   * Select content of a selection
   * Use this to access the content of an iframe
   */
  select_contents(container: dom): dom =
  (
     {inside = container
      select = {contents}}
  )

  /**
   * Perform a selection using CSS syntax
   */
  @deprecated({use = "select_raw_unsafe"}) select_raw(selector: string): dom = (
      ~{selector}
  )

  /**
   * Perform a selection using CSS syntax. This function is unsafe
   * because doesn't control [selector] argument. You can use [escape_selector] to escape
   *
   * {[xss = ", #toto"
   *   select_raw_unsafe(".class_{xss}")
   * ]}
   */
  select_raw_unsafe(selector: string): dom = (
      ~{selector}
  )


  /**
   * Compile a [dom] into its fastest form.
   *
   * By default, [dom] items use a side-agnostic representation, which allows fast manipulation on both the server and the client.
   * This function compiles a [dom] into a client-only representation, further optimized, but which cannot be manipulated by the
   * server anymore.
   */
  resolve(dom:dom): dom =
  (
        {concrete = of_selection(dom)}
  )

  select_concrete(dom_elt) : dom =
  (
        {concrete = dom_elt}
  )

  select_previous_one(dom: dom): dom =
  (
      position = of_selection(dom)
      {concrete = %% BslDom.select_previous_one %%(position)}
  )

  select_next_one(dom: dom): dom =
  (
      position = of_selection(dom)
      {concrete = %% BslDom.select_next_one %%(position)}
  )

  select_last_one(dom : dom) : dom =
  (
      position = of_selection(dom)
      {concrete = %% BslDom.select_last_one %%(position)}
  )

  select_first_one(dom : dom) : dom =
  (
      position = of_selection(dom)
      {concrete = %% BslDom.select_first_one %%(position)}
  )

  select_previous_several(dom: dom): dom =
  (
      position = of_selection(dom)
      {concrete = %% BslDom.select_previous_several %%(position)}
  )

  select_next_several(dom: dom): dom =
  (
      position = of_selection(dom)
      {concrete = %% BslDom.select_next_several %%(position)}
  )

  select_parent_one(dom:dom): dom =
  (
      position = of_selection(dom)
      {concrete = %% BslDom.select_parent_one %%(position)}
  )

  select_siblings(dom:dom):dom =
  (
      position = of_selection(dom)
      {concrete = %% BslDom.select_siblings %%(position)}
  )

  select_parent_several(dom:dom): dom =
  (
      position = of_selection(dom)
      {concrete = %% BslDom.select_parent_several %%(position)}
  )

  select(dom:dom): void =
  (
      position = of_selection(dom)
      %% BslDom.select %%(position)
  )

  /**
   * {2 Inserting new content}
   */

  of_xhtml(xhtml:xhtml): dom =
  (
     {concrete = from_xhtml(xhtml)}
  )

  /**
   * {2 Dom as a data structure}
   */

  get(x:dom, index:int): dom =
  (
        concrete = of_selection(x)
        {concrete = %% BslDom.at %%(concrete, index)}
  )

  sub(x:dom, start:int, stop:int): dom =
  (
        concrete = of_selection(x)
        {concrete = %% BslDom.sub %%(concrete, start, stop)}
  )

  contains_selector(x:dom, css:string): bool =
  (
        concrete = of_selection(x)
        %% BslDom.contains_selector %%(concrete, css)
  )

  /**
   * For a dom element composed by selecting several nodes, return the corresponding nodes.
   * Otherwise, do nothing interesting.
   */
  split(x:dom): list(dom) =
  (
        concrete = of_selection(x)
        result   = %% BslDom.decompose %%(concrete)
        List.map(x -> {concrete = x}, result)
  )

  /**
   * For a dom element composed by selecting several nodes, return the corresponding nodes.
   * Otherwise, do nothing interesting.
   */
  unsplit(l:list(dom)): dom =
  (
        result = %% BslDom.compose %%(List.map(of_selection, l))
        {concrete = result}
  )


  /**
   * @return the number of elements in a selection. This will typically be 0 or 1 if the
   * Dom was obtained by a [select_id], 1 if it was obtained by a [make_xhtml] or a
   * [make_element], or the number of nodes with class "c" if it was obtained by
   * [select_class("c")].
   */
  length(dom:dom): int =
  (
        %% BslDom.length %%(of_selection(dom))
  )

  /**
   * @return [true] if this Dom is an empty selection, [false] otherwise
   */
  is_empty(dom: dom): bool =
  (
        %% BslDom.is_empty %%(of_selection(dom))
  )

   /**
    * Iterate on the selection
    */
  iter(f:dom -> void, on: dom): void =
  (
        g(x:Dom.private.element) = f({concrete = x})
        %% BslDom.iter %%(g, of_selection(on))
  )

  fold(f:(dom,'a -> 'a), init:'a, on: dom): 'a =
  (
        g(x:Dom.private.element, acc) = f({concrete = x}, acc)
        %% BslDom.fold %%(g, init, of_selection(on))
  )

   /**
    * Iterate on whole forests
    */
  iter_deep(f:dom -> void, on: dom): void =
  (
        g(x:Dom.private.element) = f({concrete = x})
        %% BslDom.iter_deep %%(g, of_selection(on))
  )

  fold_deep(f:(dom,'a -> 'a), init:'a, on: dom): 'a =
  (
        g(x:Dom.private.element, acc) = f({concrete = x}, acc)
        %% BslDom.fold_deep %%(g, init, of_selection(on))
  )

  find(item:dom, inside:dom): option(int) =
  (
       %% BslDom.find %%(of_selection(item), of_selection(inside))
  )

  index(item:dom): option(int) =
  (
       %% BslDom.index %%(of_selection(item))
  )

  to_string(dom:dom): string =
    Rec = {{
     aux(aux_dom : Dom.private.selection) =
       match aux_dom with
       | {document}      -> "document"
       | {all}            -> "*"
       | ~{id}            -> "#{id}"
       | ~{class}         -> ".{class}"
       | {concrete = _}   -> "(dom element)"
       | {window}         -> "window"
       | ~{selector}      -> selector
       | ~{inside select} -> "{aux(inside)} {aux(select)}"
       |  {shallow}       -> ":parent:children"
       | {contents}       -> ":children"
     }}
     Rec.aux(dom)


  /**
   * {2 Actions on the web page}
   */

   /**
    * Move an element after another
    *
    * @param position The dom giving the position. If it contains several positions, the item will be cloned.
    */
  put_after(position:  dom, item:dom): dom =
  (
        to_selection(%% BslDom.after %%(of_selection(position), of_selection(item)))
        //Internally performs finalization
  )

   /**
    * Move an element to the end of another
    *
    * @param position The dom giving the position. If it contains several positions, the item will be cloned.
    */
  put_at_end(position:  dom, item:dom): dom =
  (
        to_selection(%% BslDom.append %%(of_selection(position), of_selection(item)))
        //Internally performs finalization
  )

  /**
   * Move an element to the start of another
   *
   * @param position The dom giving the position. If it contains several positions, the item will be cloned.
   */
  put_at_start(position:  dom, item:dom): dom =
  (
        to_selection(%% BslDom.prepend %%(of_selection(position), of_selection(item)))
        //Internally performs finalization
  )

  /**
   * Move an element before another
   *
   * @param position The dom giving the position. If it contains several positions, the item will be cloned.
   */
  put_before(position:  dom, item:dom): dom =
  (
        to_selection(%% BslDom.before %%(of_selection(position), of_selection(item)))
        //Internally performs finalization
  )

  /**
   * Move an element to replace another
   *
   * @param position The dom giving the position. If it contains several positions, the item will be cloned. If it is empty,
   * the item will simply disappear.
   */
  put_replace(position:dom, item:dom): dom =
  (
        to_selection(%% BslDom.replace %%(of_selection(position), of_selection(item)))
        //Internally performs finalization
  )

  /**
   * Move an element to replace another
   *
   * @param position The dom giving the position. If it contains several positions, the item will be cloned. If it is empty,
   * the item will simply disappear.
   */
  put_inside(position:dom, item:dom): dom =
  (
        to_selection(%% BslDom.replace_contents %%(of_selection(position), of_selection(item)))
        //Internally performs finalization
  )

  /**
   * Remove an element from the document, as well as all its properties.
   */
  remove(item:dom): void =
  (
        %% BslDom.remove %%(of_selection(item))
  )

  /**
   * Remove an element from the document, but retain its properties for later reinsertion.
   */
  detach(item: dom): void =
  (
        %% BslDom.detach %%(of_selection(item))
  )

  /**
   * Apply an animation to a dom item.
   *
   * This function is meaningless if the dom item hasn't been inserted on the page.
   */
  transition(item:dom, effect: Dom.animation): Dom.transition =
  (
        concrete = of_selection(item)
        Rec = {{ //Probably not performance-critical. If this changes, we may need to reimplement in JS.
          apply_many(l:list(Dom.private.animation), duration, easing, callback:option(Dom.private.element -> void)) = match l with
            | []        -> match callback with {none} -> void | ~{some} -> some(concrete) end//We're done
            | [hd | tl] -> apply_one(hd, duration, easing, {some = _ -> apply_many(tl, duration, easing, callback)})
          end
          apply_one(elementary_effect: Dom.private.animation,  duration: option(int), easing: option(string), callback: option(Dom.private.element -> void)) =
             match elementary_effect with
            | ~{with_duration embedded} ->
              duration = some(with_duration)//Compute new duration (overwrite any previous duration)
              apply_one(embedded, duration, easing, callback)
            | ~{with_easing embedded}   ->
              easing = some(with_easing)    //Compute new easing (overwrite any previous duration)
              apply_one(embedded, duration, easing, callback)
            | ~{apply} ->
              apply(concrete, duration, easing, callback)
            | ~{sequence} ->
              // TODO BUGGY HERE DURATION SHOULD BE SPLITTED
              apply_many(sequence, duration, easing, callback)
          end
        }}
        do Rec.apply_one(effect, {none}:option, {none}, {none}:option)
        concrete
  )

  stop_transition(transition: Dom.transition): void =
  (
       %% BslDom.stop %%(transition)
  )

  give_focus(dom:dom): void =
  (
        _ = %% BslDom.give_focus %%(of_selection(dom))
        void
  )

  give_blur(dom:dom): void =
  (
        _ = %% BslDom.give_blur %%(of_selection(dom))
        void
  )

  /**
   * Get the [id] of an item.
   *
   * If the item doesn't have an id, a new one is generated.
   * If there are several items, behavior is unspecified.
   */
  get_id(dom:dom): string =
  (
      %% BslDom.get_id %%(of_selection(dom))
  )

  /**
   * Form manipulation
   *
   * Get the value from a form element.
   * Note: Do not confuse [get_value] and [get_text]. The first one will return the content entered by the user
   * (e.g. from an input, a menu, etc.), while the second one will return content inserted in the Dom.
   */
  get_value(dom): string =
  (
        %% BslDom.get_value %%(of_selection(dom))
  )

  set_value(dom:dom, value:string): void =
  (
        %% BslDom.set_value %%(of_selection(dom), value)
  )

  clear_value(dom: dom): void =
  (
        set_value(dom, "")
  )

  get_text(dom:dom): string =
  (
        %% BslDom.get_text %%(of_selection(dom))
  )

  set_text(dom:dom, text:string): void =
  (
        %% BslDom.set_text %%(of_selection(dom), text)
  )

  /**
   * Behave as {!Dom.get_value} if the item is a form element, otherwise as {!Dom.get_text}
   */
  get_content(dom:dom): string =
  (
        %% BslDom.get_content %%(of_selection(dom))
  )

  is_checked(dom:dom): bool =
  (
        %% BslDom.is_checked %%(of_selection(dom))
  )

  set_checked(dom:dom, v:bool): void =
  (
        %% BslDom.set_checked %%(of_selection(dom), v)
  )

  is_enabled(dom:dom): bool =
  (
        %% BslDom.get_enabled %%(of_selection(dom))
  )

  set_enabled(dom:dom, v:bool): void =
  (
        %% BslDom.set_enabled %%(of_selection(dom), v)
  )

  set_html_unsafe(dom:dom, text:string): void =
  (
        %% BslDom.set_html %%(of_selection(dom), text)
  )

  remove_content(dom:dom): void =
  (
        %% BslDom.remove_content %%(of_selection(dom))
  )

  /**
   * {2 Visuals }
   */

  /**
   * {3 Offset}
   */

  get_offset(dom:dom): Dom.dimensions =
  (
        %% BslDom.get_offset %%(of_selection(dom))
  )

  set_offset(dom:dom, offset:Dom.dimensions): void =
  (
        do %% BslDom.set_offset %%(of_selection(dom), offset)
        void
  )

  /**
   * {3 Position}
   */

  get_position(dom:dom): Dom.dimensions =
  (
        %% BslDom.get_position %%(of_selection(dom))
  )

  set_position(dom:dom, position:Dom.dimensions): void =
  (
        do %% BslDom.set_position %%(of_selection(dom), position)
        void
  )

  /**
   * {3 Size}
   */

  get_width(dom): int =
  (
        get_size(dom).x_px
  )

  get_height(dom): int =
  (
        get_size(dom).y_px
  )

  get_inner_height(dom): int =
  (
        get_inner_size(dom).y_px
  )

  get_outer_height(dom): int =
  (
        get_outer_size(dom).y_px
  )

  get_inner_width(dom): int =
  (
        get_inner_size(dom).x_px
  )

  get_outer_width(dom): int =
  (
        get_outer_size(dom).x_px
  )

  set_width(dom, x_px): void =
  (
        do %% BslDom.set_width %%(of_selection(dom), x_px)
        void
  )

  set_height(dom, y_px): void =
  (
        do %% BslDom.set_height %%(of_selection(dom), y_px)
        void
  )

  set_size(dom, dimensions): void =
  (
        do %% BslDom.set_size %%(of_selection(dom), dimensions)
        void
  )

  get_size(dom):   Dom.dimensions =
  (
        %% BslDom.get_size %%(of_selection(dom))
  )

  get_inner_size(dom):   Dom.dimensions =
  (
        %% BslDom.get_inner_size %%(of_selection(dom))
  )

  get_outer_size(dom):   Dom.dimensions =
  (
        %% BslDom.get_outer_size %%(of_selection(dom))
  )

  get_scrollable_size(dom):   Dom.dimensions =
  (
        %% BslDom.get_scrollable_size %%(of_selection(dom))
  )


  get_scroll(dom): Dom.dimensions =
  (
        %% BslDom.get_scroll %%(of_selection(dom))
  )

  set_scroll(dom:dom, dim:Dom.dimensions): void =
  (
        do %% BslDom.set_scroll %%(of_selection(dom), dim)
        void
  )

  set_scroll_left(dom:dom, px:int): void =
  (
        set_scroll(dom, {y_px = 0 x_px = px})
  )

  get_scroll_left(dom:dom): int =
  (
        get_scroll(dom).x_px
  )

  set_scroll_top(dom:dom, px:int): void =
  (
        set_scroll(dom, {x_px = 0 y_px = px})
  )

  get_scroll_top(dom:dom): int =
  (
        get_scroll(dom).y_px
  )

  scroll_to_top(dom: dom): void =
  (
        set_scroll_top(dom, 0)
  )

  scroll_to_leftmost(dom: dom): void =
  (
        set_scroll_left(dom, 0)
  )

  scroll_to_rightmost(dom: dom): void =
  (
        set_scroll_left(dom, get_scrollable_size(dom).x_px)
  )

  scroll_to_bottom(dom: dom): void =
  (
        set_scroll_top(dom,  get_scrollable_size(dom).y_px)
  )

  /**
   * If an item is not visible, scroll the window to make it visible
   */
  scroll_into_view(dom: dom): void =
  (
        offset = get_offset(dom)
        size   = get_size(dom)
        //Compute coordinate of NW and SE
        nw     = offset
        se     = {x_px = offset.x_px + size.x_px   y_px = offset.y_px + size.y_px}

        //Compute viewport
        win    = select_window()
        wscroll= get_scroll(win)
        wsize  = get_size(win)

        //If we can have both NW and SE, try and get them both on screen
        (scroll_x, scroll_y) = if wsize.x_px >= size.x_px + 5 && wsize.y_px >= size.y_px + 5 then
        (
           left_x   = wscroll.x_px <= nw.x_px
           right_x  = se.x_px <= wscroll.x_px + wsize.x_px
           scroll_x = match(left_x, right_x) with
             | ({true}, {true}) -> {none}//Don't scroll horizontally at all
             | ({true}, {false})-> {some = nw.x_px - 5}              //Scroll right
             | ({false},{true}) -> {some = se.x_px - wsize.x_px + 5} //Scroll left
             | _                -> do Log.error("Scrolling", "Internal error: inconsistent horizontal on-screen position") {none}
           down_y    = wscroll.x_px <= nw.y_px
           up_y      = se.y_px <= wscroll.y_px + wsize.y_px
           scroll_y = match(up_y, down_y) with
             | ({true}, {true}) -> {none}//Don't scroll vertically at all
             | ({true}, {false})-> {some = nw.y_px - 5}              //Scroll down
             | ({false},{true}) -> {some = se.y_px - wsize.y_px + 5} //Scroll up
             | _                -> do Log.error("Scrolling", "Internal error: inconsistent vertical on-screen position") {none}
           (scroll_x, scroll_y)
        )
        else //Otherwise, favor NW
        (
           //TODO: Determine if we're scrolling down or up
           scroll_x = if wscroll.x_px <= nw.x_px && nw.x_px <= wscroll.x_px + wsize.x_px then {none}
                      else {some = nw.x_px - 5}
           scroll_y = if wscroll.y_px <= nw.y_px && nw.y_px <= wscroll.y_px + wsize.y_px then {none}
                      else {some = nw.y_px - 5}
           (scroll_x, scroll_y)
        )

        _ = Dom.transition(select_raw_unsafe("html, body")/*For some reason, it doesn't work with the window itself*/,
              Effect.scroll_to_xy(scroll_x, scroll_y))
        void
  )

  @private @client
  transform_on_client(l:list(Dom.transformation)): void =
    List.iter(transform_1_on_client, l)

  @private
  transform_1_on_client(a:Dom.transformation): void =
    match a with
      | {~jq ~subject ~verb} ->
        sel = resolve(jq)
       /*
       when we do something in an empty selection, it will break the javascript code.
       to do fail in an action in the dom is not so important, so I write a Log.info.
       */
        if length(sel) != 0 then
        match subject with
          | {~property ~value_p} ->
            _ = set_property_unsafe(sel, property, value_p)
            void
          | {~value} ->
            _ = match verb with
              | {append} -> set_value(sel, get_value(sel) ^ value)
              | {prepend} -> set_value(sel, value ^ get_value(sel))
              | {set} -> set_value(sel, value)
              end
            void
          | {~content} ->
            x = of_xhtml(content)
            _ = match verb with
              | {append}  -> put_at_end(sel, x)
              | {prepend} -> put_at_start(sel, x)
              | {set}     -> put_inside(sel, x)
              end
            void
          | {~css} -> match verb with
            | {set} -> set_style(sel, `css`)
            | _ -> error("Style properties can only be set.")
            end
        end
       else
        Log.info("Dom.transform", "empty DOM selection {to_string(jq)}")


  /**
   * Exec a list of actions
   *
   * Note: this function exists both on the server and on the client. The server version optimizes the message before calling the client version
   */
  transform(l:list(Dom.transformation)):void =
    @sliced_expr(
    {client = transform_on_client(l)
     server =
       optimize(action:Dom.transformation): Dom.transformation = match action with
        | {~jq ~subject ~verb} ->
           match subject with
             | {~content} -> {~jq ~verb subject={content = Xhtml.precompile(content)}}
             | _          -> action //TODO: Precompile CSS?
           end
        | _ -> action
        transform_on_client(List.map(optimize, l))
    })

  /**
   * {2 Dimension module}
   */

  Dimension = {{

    /**
     * The distance between 2 coordinates
     */
    distance({x_px=x1 y_px=y1} : Dom.Dimension.t,{x_px=x2 y_px=y2} : Dom.Dimension.t) : int =
      Math.sqrt_i(Math.square_i(x1-x2) + Math.square_i(y1-y2))

    /**
     * Add 2 dimension
     */
    add({x_px=x1 y_px=y1} : Dom.Dimension.t,{x_px=x2 y_px=y2} : Dom.Dimension.t) : Dom.Dimension.t=
      {x_px=(x1+x2)  y_px=(y1+y2)}

    /**
     * Subtract 2nd dimension to the 1st one
     */
    sub({x_px=x1 y_px=y1} : Dom.Dimension.t,{x_px=x2 y_px=y2} : Dom.Dimension.t) : Dom.Dimension.t=
      {x_px=(x1-x2)  y_px=(y1-y2)}

    /**
     * Compute a normal vector
     */
    normal({x_px=x y_px=y} : Dom.Dimension.t) : Dom.Dimension.t =
      {x_px=y y_px= - x}

    /**
     * Normalize a vector
     */
    normalize({x_px=x y_px=y} : Dom.Dimension.t) : Dom.Dimension.t =
      d = size({x_px=x y_px=y})
      {x_px=x/d y_px=y/d}

    /**
     * Size of the vector
     */
    size({x_px=x y_px=y} : Dom.Dimension.t) : int =
      Math.sqrt_i(Math.square_i(x) + Math.square_i(y))
  }}



  /**
   * {2 Event handling}
   */

  /**
   * Bind an event handler to an event
   *
   * @param event The name of the event, e.g. "click", "dblclick", etc. You are not limited to standard browser events.
   * To trigger an event, use function {!Dom.trigger}.
   */
  bind(dom:dom, event:Dom.event.kind, handler:(Dom.event -> void)): Dom.event_handler =
  (
        llbind(of_selection(dom), Event.get_name(event), handler)
  )

  /**
   * Bind an event handler to an event
   *
   * @param dom Where to bind the event.
   * @param event The name of the event, e.g. "click", "dblclick", etc. You are not limited to standard browser events.
   * @param handler The event handler
   * @param options the list of bind options
   *        - {stop_propagation} always stops propagation of the event after handling
   *        - {prevent_default} always prevents browser default behaviour of the event after handling
   *        - {propagation_handler} [Client only] provide a custom propagation handler, allowing to stop/prevent the event or not, given the event.
   */
  bind_with_options(dom:dom, event:Dom.event.kind, handler:(Dom.event -> void), options:list(Dom.event_option) ): Dom.event_handler =
  (
        //warning: names "stop_propagation" and "prevent_default" are hardcoded in JS
        llbind_with_options(of_selection(dom), Event.get_name(event), handler, options)
  )

  unbind(dom:dom, handler: Dom.event_handler): void =
  (
        do %% BslDom.unbind %%(of_selection(dom), handler)
        void
  )

  unbind_event(dom:dom, event:Dom.event.kind): void =
  (
        do %% BslDom.unbind_event %%(of_selection(dom), Event.get_name(event))
        void
  )

  /**
   * A special event sent when the user attempts to navigate away from a page
   *
   * @param cb a function returning either [{none}] if there is no objection to navigating away or [{some = s}], where [s]
   * is a message which the browser will display. It is not possible to cancel this action.
   *
   * Compatibility note: [Dom.bind_unload_confirmation] has no effect on Opera <= 11
   */
  bind_unload_confirmation(cb:Dom.event -> option(string)): void =
  (
        do %% BslDom.bind_unload_confirmation%%(cb)
        void
  )

  /**
   * A special event sent when the user attempts to navigate away from a page
   *
   * @param cb a function returning either [{none}] if there is no objection to navigating away or [{some = s}], where [s]
   * is a message which the browser will display prompting the user to reconsider their choice.
   *
   */
  bind_beforeunload_confirmation(cb:Dom.event -> option(string)): void =
  (
        do %% BslDom.bind_beforeunload_confirmation%%(cb)
        void
  )

  /**
   * Trigger a default event.
   *
   * @param dom Where to trigger the event. Once triggered, an event will climb the tree until it either reaches the root
   * or is stopped by an event handler connected with option [{stop_propagation}].
   *
   * @see bind_with_options
   */
  trigger(dom:dom, event_kind:Dom.event.kind): void =
  (
        do %% BslDom.trigger %%(of_selection(dom), Event.get_name(event_kind))
        void
  )

  /**
   * Trigger an event
   *
   * @param dom Where to trigger the event. Once triggered, an event will climb the tree until it either reaches the root
   * or is stopped by an event handler connected with option [{stop_propagation}].
   *
   * @see bind_with_options
   */
  trigger_event(dom:dom, event:Dom.event): void =
  (
        do %% BslDom.trigger_event %%(of_selection(dom), event, Event.get_name(event.kind))
        void
  )

  /**
   * {2 Event module}
   */

  Event =
  {{
    get_name(handle:Dom.event.kind) = match handle with
      | { click }      -> "click"
      | { mouseup }    -> "mouseup"
      | { mousedown }  -> "mousedown"
      | { mouseover }  -> "mouseover"
      | { mouseout }   -> "mouseout"
      | { mousemove }  -> "mousemove"
      | { mouseenter}  -> "mouseenter"
      | { mouseleave } -> "mouseleave"
      | { resize }     -> "resize"
      | { dblclick }   -> "dblclick"
      | { keypress }   -> "keypress"
      | { keydown }    -> "keydown"
      | { keyup }      -> "keyup"
      | { load }       -> "load"
      | { unload }     -> "unload"
      | { error }      -> "error"
      | { submit }     -> "submit"
      | { focus }      -> "focus"
      | { focusin }    -> "focusin"
      | { focusout }   -> "focusout"
      | { blur }       -> "blur"
      | { mousewheel } -> "mousewheel"
      | { change }     -> "change"
      | { select }     -> "select"
      | { scroll }     -> "scroll"
      | { newline }    -> "keydown.newline"
      | { keyesc}      -> "keydown.keyesc"
      | { ready }      -> "ready"
      | { input }      -> "input"
      | { paste }      -> "paste"
      | ~{custom}      -> custom

    of_string(handle:string):Dom.event.kind = match handle with
      | "click"      -> {click}
      | "mouseup"    -> {mouseup}
      | "mousedown"  -> {mousedown}
      | "mouseover"  -> {mouseover}
      | "mouseout"   -> {mouseout}
      | "mousemove"  -> {mousemove}
      | "mouseenter" -> {mouseenter}
      | "mouseleave" -> {mouseleave}
      | "dblclick"   -> {dblclick}
      | "keypress"   -> {keypress}
      | "keydown"    -> {keydown}
      | "keyup"      -> {keyup}
      | "ready"      -> {ready}
      | "load"       -> {load}
      | "unload"     -> {unload}
      | "error"      -> {error}
      | "select"     -> {select}
      | "submit"     -> {submit}
      | "focus"      -> {focus}
      | "focusin"    -> {focusin}
      | "focusout"   -> {focusout}
      | "blur"       -> {blur}
      | "mousewheel" -> {mousewheel}
      | "scroll"     -> {scroll}
      | "change"     -> {change}
      | "resize"     -> {resize}
      | "input"      -> {input}
      | "paste"      -> {paste}
      | "newline"  | "keydown.newline"    -> {newline}
      | "keyesc"   | "keydown.keyesc"     -> {keyesc}
      | _            -> {custom = handle}

    to_string = get_name
    to_js_attribute_name(handle:Dom.event.kind) =
      "on" ^ get_name(handle)

    default_event:Dom.event =
    {
      kind         = {custom = "none"}
      mouse_position_on_page = {x_px= 0 y_px=0}
      key_code     = {none}
      mouse_button = {none}
      key_modifiers= []
      value_change = {none}
    }
  }}

  get_currently_selected(): Dom.selected =
  (
     ~{start_in start_at finish_in finish_at is_collapsed} = %% BslDom.get_selection_range %%()
     {start_in = to_selection(start_in)
      finish_in= to_selection(finish_in)
      ~start_at
      ~finish_at
      ~is_collapsed}
  )

  /**
   * Direct manipulation of style properties
   */
  set_style(dom:dom, properties:Css.properties): void =
  (
        set_style_unsafe(dom, Css_printer.to_xhtml_style(properties))
  )

  /**
   * Remove all styles from an element
   */
  void_style(dom): void =
  (
     %% BslDom.void_style %%(of_selection(dom))
  )

  /**
   * As [set_style], but perform no conversion on CSS property names.
   *
   * In other words, if you wish to set CSS property [float], you're responsible for ensuring that
   * the property is called [cssFloat] under Mozilla and [styleFloat] under IE.
   */
  set_style_unsafe(dom:dom, list:list(Css.compiled_property)): void =
  (
        set_one = %% BslDom.set_style_property_unsafe %%: Dom.private.element, string, string -> void
        dom_concrete = of_selection(dom)
        do List.iter((~{name value} -> set_one(dom_concrete, name, value)), list)
        void
  )

  /**
   * Get the value of an attribute for the first element in the set of matched elements.
   * /!\ Different from get_attribute
   *
   * @see http://api.jquery.com/prop/
   */
  get_property(dom: dom, string: string): option(string) =
  (
        %% BslDom.get_property %%(of_selection(dom), string)
  )

  get_property_unsafe(dom:dom, string:string): string = //return "" when the property is undefined
  (
        %% BslDom.get_property_unsafe %%(of_selection(dom), string)
  )

  set_property_unsafe(dom:dom, name:string, value:string): void =
  (
        do %% BslDom.set_property_unsafe %%(of_selection(dom), name, value)
        void
  )

  /**
   * Get the value of an attribute for the first element in the set of matched elements.
   * /!\ Different from get_property
   *
   * @see http://api.jquery.com/attr/
   */
  get_attribute(dom: dom, string: string): option(string) =
  (
        %% BslDom.get_attribute %%(of_selection(dom), string)
  )

  get_attribute_unsafe(dom:dom, string:string): string = //return "" when the attribute is undefined
  (
        %% BslDom.get_attribute_unsafe %%(of_selection(dom), string)
  )

  set_attribute_unsafe(dom:dom, name:string, value:string): void =
  (
        do %% BslDom.set_attribute_unsafe %%(of_selection(dom), name, value)
        void
  )

  set_style_property_unsafe(dom:dom, name:string, value:string): void =
  (
        do %% BslDom.set_style_property_unsafe %%(of_selection(dom), name, value)
        void
  )

  set_class(dom:dom, class:string): void =
  (
        do %% BslDom.set_class %%(of_selection(dom), class)
        void
  )

  has_class(dom:dom, class:string): bool =
  (
        %% BslDom.has_class %%(of_selection(dom), class)
  )

  add_class(dom:dom, class:string): void =
  (
        %% BslDom.add_class %%(of_selection(dom), class)
  )

  remove_class(dom:dom, class:string): void =
  (
        %% BslDom.remove_class %%(of_selection(dom), class)
  )

  /**
   * Remove all classes from an element
   */
  void_class(dom): void =
  (
     %% BslDom.void_class %%(of_selection(dom))
  )

  toggle_class(dom, class): void=
  (
     %% BslDom.toggle_class %%(of_selection(dom), class)
  )

  /**
   * Show a hidden Dom element.
   *
   * For more powerful animations, see module [Effects]
   */
  show(dom): void =
  (
    _ = transition(dom, Effect.with_duration({millisec = 0}, Effect.show()))
    void
  )

  /**
   * Hide a visible Dom element.
   *
   * For more powerful animations, see module [Effects]
   */
  hide(dom): void =
  (
    _ = transition(dom, Effect.with_duration({millisec = 0}, Effect.hide()))
    void
  )

  toggle(dom): void =
  (
    _ = transition(dom, Effect.with_duration({millisec = 0}, Effect.toggle()))
    void
  )


  /**
   * {2 Effect module}
   */

  /**
   * Manipulation of special effects.
   * Use function [transition] to perform actual special effects.
   */
  Effect =
  {{
     /**
      * An animation that does nothing
      */
     empty: Dom.animation = {sequence = []}

     /**
      * An animation that makes a dom disappear at once
      */
     hide(): Dom.animation    = {apply = %% BslDom.do_hide %% }

     /**
      * An animation that makes a dom appear at once
      */
     show(): Dom.animation    = {apply = %% BslDom.do_show %% }

     toggle(): Dom.animation = {apply = %% BslDom.do_toggle %% }

     /**
      * Progressively change update current css properties with the given set of properties
      * Current css properties are not erased but modified according to the given set
      */
     change_to(properties: Css.properties): Dom.animation =
     (
        css_list = Css_printer.to_xhtml_style(properties)
        f = %% BslDom.do_change_to %%(css_list)
        {apply = f}
     )

     scroll_to(dim: Dom.dimensions): Dom.animation =
     (
        scroll_to_xy({some = dim.x_px}, {some = dim.y_px})
     )

     scroll_to_top(): Dom.animation =
     (
        scroll_to_xy({none}, {some = 0})
     )

     scroll_to_bottom(): Dom.animation =
     (
        {apply = %% BslDom.do_scroll_to_bottom %%}
     )

     scroll_to_leftmost(): Dom.animation =
     (
        scroll_to_xy({some = 0}, {none})
     )

     scroll_to_rightmost(): Dom.animation =
     (
        {apply = %% BslDom.do_scroll_to_bottom %%}
     )

     scroll_to_xy(x_px: option(int), y_px:option(int)): Dom.animation =
     (
        scroll_x = match x_px with {none} -> [] | {some = x} -> [{name="scrollLeft" value="{x}px"}]
        scroll_y = match y_px with {none} -> [] | {some = y} -> [{name="scrollTop"  value="{y}px"}]
        css_list = scroll_x ++ scroll_y
        f = %% BslDom.do_change_to %%(css_list)
        {apply = f}
     )



     /**
      * An animation that makes a dom disappear progressively, by transparency -- requires jQuery 1.4.x
      */
     fade_out(): Dom.animation= {apply = %% BslDom.do_fade_out %% }

     /**
      * An animation that makes a dom appear progressively, by transparency -- requires jQuery 1.4.x
      */
     fade_in(): Dom.animation = {apply = %% BslDom.do_fade_in %% }

     fade_toggle(): Dom.animation = {apply = %% BslDom.do_fade_toggle %% }

     /**
      * Progressively take a set of properties
      */
     fade_to(properties: Css.properties): Dom.animation =
     (
        css_list = Css_printer.to_xhtml_style(properties)
        f = %% BslDom.do_fade_to %%(css_list)
        {apply = f}
     )

     /**
      * An animation that makes a dom appear progressively, by sliding in
      */
     slide_in():  Dom.animation= {apply = %% BslDom.do_slide_in %% }

     /**
      * An animation that makes a dom disappear progressively, by sliding away
      */
     slide_out(): Dom.animation= {apply = %% BslDom.do_slide_out %% }

     slide_toggle(): Dom.animation= {apply = %% BslDom.do_slide_toggle %% }

     /**
      * Customize the duration of an animation, if the animation accepts it.
      *
      * Note: some animations ignore such customization.
      */
     with_duration(duration: Dom.Effect.duration, anim: Dom.animation): Dom.animation =
        duration_ms = match duration with
          | {immediate} -> 0
          | {slow}      -> 600
          | {default}   -> 400
          | {fast}      -> 200
          | ~{millisec} -> millisec
        {with_duration = duration_ms; embedded = anim}


     with_easing(easing: {linear}/{swing}, anim: Dom.animation): Dom.animation =
         easing_name = match easing with
            | {linear} -> "linear"
            | {swing}  -> "swing"
         {with_easing = easing_name; embedded = anim}


     /**
      * An empty animation whose sole effect is to wait.
      * Use this to make a pause in a sequence of animations.
      */
     wait(): Dom.animation = {apply = %% BslDom.do_wait %% }

     /**
      * An animation defined from a function.
      *
      * Use this to introduce new kinds of animations or to trigger a callback at some
      * point during a sequence of animations.
      */
     of_simple_fun(f:dom,option(int) -> void): Dom.animation =
     (
        g(dom_concrete:Dom.private.element, maybe_duration: option(int), _: option(string), maybe_cb_concrete:option(Dom.private.element -> void)): void =
        (
           do f(to_selection(dom_concrete), maybe_duration)
           match maybe_cb_concrete with
             | {none} -> void
             |~{some} -> some(dom_concrete)
        )
        {apply = g}
     )

     of_fun_with_cb(f:dom,option(int),option(string),option(dom -> void) -> void): Dom.animation =
     (
        g(dom_concrete:Dom.private.element, maybe_duration: option(int), maybe_easing: option(string), maybe_cb_concrete:option(Dom.private.element -> void)): void =
        (
           wrapped_maybe_cb: option(dom -> void) = match maybe_cb_concrete with
              | {none}      -> {none}
              | {some = cb_concrete} -> {some = concrete -> cb_concrete(of_selection(concrete))}
           f(to_selection(dom_concrete), maybe_duration, maybe_easing, wrapped_maybe_cb)
        )
        {apply = g}
     )

     /**
      * Combine several animations as one
      *
      * Note: This function does not offer full support for [with_duration] yet.
      */
     sequence(l: list(Dom.animation)): Dom.animation = {sequence = List.map(x -> x, l)}
     /* TODO add with_duration support */
  }}

  /**
   * {2 Key module}
   */

  /**
   * Constants for usual keypresses.
   */
  Key =
  {{
  BACKSPACE: Dom.key_code = 8
  TAB:       Dom.key_code = 9
  RETURN:    Dom.key_code = 13
  ESC:       Dom.key_code = 27
  LEFT:      Dom.key_code = 37
  UP:        Dom.key_code = 38
  RIGHT:     Dom.key_code = 39
  DOWN:      Dom.key_code = 40
  DELETE:    Dom.key_code = 46
  HOME:      Dom.key_code = 36
  END:       Dom.key_code = 35
  PAGEUP:    Dom.key_code = 33
  PAGEDOWN:  Dom.key_code = 34
  INSERT:    Dom.key_code = 45
  }}

  /**
   * Escaping for jQuery selectors !"#$%&'()*+,./:;<=>?@[\]^`{|}~.
   * To use for raw selection.
   * @see http://api.jquery.com/category/selectors/
   */
  escape_selector(str) =
    String.map(c ->
        match c with
        | "!" | "\"" | "#" | "$" | "%" | "&" | "'" | "(" | ")" | "*"
        | "+" | ","  | "." | "/" | ":" | ";" | "<" | "=" | ">" | "?"
        | "@" | "[" | "\\" | "]" | "^" | "`" | "\{" | "|"| "}" | "~" | " " -> "\\\\" ^ c
        | _ -> c
      , str)

  /*
   * {2 Select module}
   */
  @private Select =
  {{

   /**
    * The browser window
    */
  window(): Dom.private.element =
  (
        %% BslDom.select_window %%()
  )

   /**
    * The full document
    */
  document(): Dom.private.element =
  (
        %% BslDom.select_document %%()
  )

   /**
    * The body of the document
    */
  body() : Dom.private.element =
  (
        %% BslDom.select_body %%()
  )

  empty(): Dom.private.element =
  (
        %% BslDom.select_nothing %%()
  )


  /**
   * Select everything in the document
   */
  select_all(): Dom.private.element =
  (
        %% BslDom.select_all %%()
  )

  /**
   * Select the element with a given id [{name}], if it exists.
   *
   * @param name The name of the element.
   * @return An empty selection if the element doesn't exist
   *
   * Note: Elements which haven't been inserted in the page cannot be selected yet.
   */
  select_id(name: string): Dom.private.element =
  (
        %% BslDom.select_id %%(escape_selector(name))
  )

  /**
   * Select all the elements of the page with class [{class}], if any
   * Note: Elements which haven't been inserted in the page cannot be selected yet.
   */
  select_class(class: string): Dom.private.element =
  (
        %% BslDom.select_class %%(escape_selector(class))
  )

  /**
   * Select all the elements of the page matching CSS selector [{selector}], if any
   * Note: Elements which haven't been inserted in the page cannot be selected yet.
   */
  select_css(selector: string): Dom.private.element =
  (
        %% BslDom.select_css %%(selector)
  )

  /**
   * Select all the elements of the page with tag [{tag}], if any.
   * Note: Elements which haven't been inserted in the page cannot be selected yet.
   */
  select_tag(tag: string): Dom.private.element =
  (
        %% BslDom.select_tag %%(escape_selector(tag))
  )

  /**
   * Select all the children of a given [{dom}]
   */
  select_children(parent:Dom.private.element): Dom.private.element =
  (
        %% BslDom.select_children %%(parent)
  )

  /**
   * Select content of a selected [{dom}]
   * Use this to access the content of an iframe
   */
  select_contents(parent:Dom.private.element): Dom.private.element =
  (
        %% BslDom.select_contents %%(parent)
  )

  /**
   * Select all the descendants of a given [{dom}]
   */
  select_all_in(parent: Dom.private.element): Dom.private.element =
  (
        %% BslDom.select_all_in %%(parent)
  )

  /**
   * As [select_id], but among the children of a dom element
   */
  select_id_in(name: string, parent: Dom.private.element) =
  (
        %% BslDom.select_id_in %%(escape_selector(name), parent)
  )

  /**
   * As [select_class], but among the children of a dom element
   */
  select_class_in(class: string, parent: Dom.private.element) =
  (
        %% BslDom.select_class_in %%(escape_selector(class), parent)
  )

  /**
   * As [select_tag], but among the children of a dom element
   */
  select_tag_in(tag: string, parent: Dom.private.element) =
  (
        %% BslDom.select_tag_in %%(escape_selector(tag), parent)
  )

  /**
   * As [select_css], but among the children of a dom element
   */
  select_css_in(selector: string, parent:Dom.private.element): Dom.private.element =
  (
        %% BslDom.select_css_in %%(selector, parent)
  )

  select_parent(dom: Dom.private.element): Dom.private.element =
  (
        %% BslDom.select_parent %%(dom)
  )

  }}

  to_selection(x: Dom.private.element): dom =
  (
     {concrete = x}
  )

  /**
   * Find the element(s) corresponding to a selection
   */
  of_selection(selection: dom): Dom.private.element =
  (
      Rec = {{
       top(selection:Dom.private.selection): Dom.private.element =  match selection with
         | { all }      -> Select.select_all()
         | { document } -> Select.document()
         | { window   } -> Select.window()
         | ~{id}        -> Select.select_id(id)
         | ~{class}     -> Select.select_class(class)
         | ~{selector}  -> Select.select_css(selector)
         | ~{inside select} ->
            container = top(inside)
            depth(container, select)
         | ~{concrete}  -> concrete
         | {contents}   -> Select.document()
         | {shallow}    -> Select.document()
        depth(container:Dom.private.element, selection:Dom.private.selection):Dom.private.element = match selection with
         | { document } -> Select.empty()
         | { window   } -> Select.empty()
         | {concrete = _}  -> Select.empty()//Note: This case is meaningless
         | ~{id}        -> Select.select_id_in(id, container)
         | ~{class}     -> Select.select_class_in(class, container)
         | {all}        -> Select.select_all_in(container)
         | ~{selector}  -> Select.select_css_in(selector, container)
         | ~{inside select} ->
               container = depth(container, inside)
               depth(container, select)
         | {contents}   -> Select.select_contents(container)
         | {shallow}    -> Select.select_children(container)
      }}
      Rec.top(selection)
  )


 /**
  * {2 Low-level interface}
  */

 /**
   * Bind an event handler to an event (low-level version)
   *
   * @param event The name of the event, e.g. "click", "dblclick", etc. You are not limited to standard browser events.
   * To trigger an event, use function [trigger].
   */
  @private llbind(dom:Dom.private.element, event:string, handler:(Dom.event -> void)): Dom.event_handler =
  (
        %% BslDom.bind %%(dom, event, handler)
  )

  @client @private llbind_with_options(dom:Dom.private.element, event:string, handler:(Dom.event -> void), options:list(Dom.event_option)): Dom.event_handler =
  (
        stop_propagation = List.mem({stop_propagation}, options)
        prevent_default = List.mem({prevent_default}, options)
        propagation_handler =
          match List.find((e->match e {propagation_handler=_} -> true _ -> false), options)
          {some={propagation_handler=ph}} -> some(ph)
          _ -> none
        %% BslDom.bind_with_options %%(dom, event, handler, propagation_handler, stop_propagation, prevent_default)
  )


  /**
   * {2 Dom node constructors}
   */
  /**
   * Create a dom text node from a string
   */
  @private create_text_node = %% BslClientOnly.Dom.create_text_node %%
    : string -> Dom.private.element

  /**
   * Create a void dom fragment
   */
  @private create_fragment = %% BslClientOnly.Dom.create_fragment %%
    : -> Dom.private.element

  /**
   * Create a node from a string that should contains a XHTML.
   * That function is very unsafe and can be source of code injection
   */
  @private create_inner_node = %% BslClientOnly.Dom.create_inner_html_unsafe %%
    : string -> Dom.private.element

  /**
   * Create a dom element from a [namespace] and a [tag]
   */
  @private create_element = %% BslClientOnly.Dom.create_element %%
    : string -> Dom.private.element

  /**
   * Create a dom element from a [namespace] and a [tag]
   * [create_element(namespace,tag)]
   */
  @private create_element_ns = %% BslClientOnly.Dom.create_element_ns %%
    : string, string -> Dom.private.element

  /**
   * {2 Dom node manipulations}
   */
  /**
   * Set the [attribute] with a string [value] of the given dom
   * [node].
   * [set_attribute(element,name,value)]
   */
  @private set_attribute = %% BslClientOnly.Dom.set_attribute %%
    : Dom.private.element, string, string -> void

  /**
   * Set the [attribute] with a string [value] of the given dom
   * [node].
   * [set_attribute_ns(element,namespace,name,value)]
   */
  @private set_attribute_ns = %% BslClientOnly.Dom.set_attribute_ns %%
    : Dom.private.element, string, string, string -> void


  /**
   * As [bind] but for a pre-compiled JavaScript string.
   */
  opabind_value_unsafe =  %% BslClientOnly.Dom.opabind_value_unsafe %%
    : dom_element, string, string -> void

  bind_value_unsafe(dom: dom_element, event: string, handler:string) =
  (
     opabind_value_unsafe(dom, event, handler)
  )

  bind_stop_propagation : dom_element, string -> void = %% BslClientOnly.Dom.bind_stop_propagation %%
  bind_prevent_default : dom_element, string -> void = %% BslClientOnly.Dom.bind_prevent_default %%

  default_opa_event = %% BslClientOnly.default_opa_event %% : -> Dom.event

  /**
   * Like [set_attribute] but the style attribute of the given [node].
   */
  @private set_style_attribute = %% BslClientOnly.Dom.set_style_attribute %%
    : dom_element, string, string -> void

  /**
   * [append_child parent child] Append to the children list of [parent] the
   * given [child] dom node.
   */
  @private append_child = %% BslClientOnly.Dom.append_child %%
    : dom_element, dom_element -> void

  /**
   * Add a css class name to the given dom node
   */
  @private add_class_name = %% BslClientOnly.Dom.add_class_name %%
    : dom_element, string -> void

  /**
   * {2 Style constructions}
   */

  /**
   * Create an empty style constructor
   */
  @private create_style_constructor = %% BslClientOnly.Dom.create_style_constructor %%
    : -> style_constructor

  /**
   * Add a style application to a style constructor. That has the same
   * effect as [set_style_attribute] but it's applied only when you
   * call [flush_style_constructor].
   */
  @private add_style_application = %% BslClientOnly.Dom.add_style_application %%
    : style_constructor, dom_element, string, string -> void

  /**
   * Store a [style_constructor] on a [dom_element]. This
   * [style_constructor] will be flushed when the given
   * [style_constructor] will be inserted on Dom.
   */
  @private store_style_constructor = %% BslClientOnly.Dom.store_style_constructor %%
    : dom_element, style_constructor -> void

  /**
   * Flush the given style constructor. Apply all applications added
   * with [add_style_application]
   */
  @private flush_style_constructor = %% BslClientOnly.Dom.flush_style_constructor %%
    : style_constructor -> void

  /**
   * {2 Insertion handlers}
   */

  /**
   * Create an empty [insertion_handlers].
   */
  @private create_insertion_handlers = %% BslClientOnly.Dom.create_insertion_handlers %%
    : -> insertion_handlers

  /**
   * Add a handler to an [insertion_handlers].
   */
  @private add_insertion_handler = %% BslClientOnly.Dom.add_insertion_handler %%
    : insertion_handlers, (-> void) -> void

  /**
   * Add a handler to an [insertion_handlers] from a unsafe (JavaScript)
   * string.
   */
  @private add_insertion_handler_unsafe = %% BslClientOnly.Dom.add_insertion_handler_unsafe %%
    : insertion_handlers, string -> void

  /**
   * Store an [insertion_handlers] on a [dom_element].
   *
   * Note : That handlers will be executed when the [dom_element] will
   * be inserted on the document
   */
  @private store_insertion_handlers = %% BslClientOnly.Dom.store_insertion_handlers %%
    : dom_element, insertion_handlers -> void

  /**
   * {2 Converting}
   */
  /**
   * Generic conversion from any [xml] value. Get two function for
   * converting the parameters of xml type :
   * - [handle_dialect] : Should create a dom element from xml extensions
   * - [handle_attribute] : Should handle the specific attributes. It
   *   take also the the dom node that own this specific attributes.
   */
  from_xml(xml : xml('attribute, 'extensions),
           default_namespace:string,
           handle_dialect : 'extensions -> dom_element,
           handle_attribute : (XmlNsEnv.t, string/*tag*/, list(string_assoc(string)), dom_element, 'attribute -> void)) =
    rec aux(aux_xml : xml('attribute, 'extensions), nsenv) =
    match aux_xml with
      | ~{text} -> create_text_node(text)
      | ~{content_unsafe} -> create_inner_node(content_unsafe)
      | ~{fragment} ->
        match fragment with
          | []  -> create_fragment()
          | [e] -> aux(e, nsenv)
          | _   ->
            element = create_fragment()
            do List.iter(
                 x -> append_child(element, aux(x, nsenv)),
                 fragment)
            element
        end
      | ~{namespace tag args content specific_attributes xmlns} ->
        nsenv = XmlNsEnv.add(nsenv, xmlns)
        element =
          match XmlNsEnv.try_get_uri(namespace, nsenv)
          | {none} -> create_element(tag)
          | {some = uri} -> create_element_ns(uri, tag)
        do List.iter(
          | ~{default}  -> set_attribute(element, "xmlns", default)
          | ~{name uri} -> set_attribute(element, "xmlns:{name}", uri)
          , xmlns)
        do List.iter(
             (~{namespace name value} ->
               match XmlNsEnv.try_get_uri(namespace, nsenv)
               | {none} -> set_attribute(element, name, value)
               | {some = uri} -> set_attribute_ns(element, uri, name, value)
             ), args)
        do List.iter(
             (child -> append_child(element, aux(child, nsenv))),
             content)
        do Option.iter(attributes -> handle_attribute(nsenv, tag, args, element, attributes),
                       specific_attributes)
        element : dom_element
      | ~{xml_dialect} ->
        match xml_dialect with
        | {none} -> create_fragment()
        | ~{some} -> handle_dialect(some)
        end
    aux(xml, XmlNsEnv.empty) : dom_element

  /**
   * Conversion from [xhtml] to a [dom_element].
   */
  from_xhtml(xhtml : xhtml): dom_element =
    opabind = %% BslClientOnly.Dom.bind %%: dom_element, string, ('a -> 'b) -> void
    wrap_as_dom = %% BslDom.import %%
    cons = create_style_constructor()
    ins = create_insertion_handlers()
    dom = from_xml(xhtml,
                   Xhtml.ns_uri,
                 (~{html_code_unsafe js_code_unsafe} ->
                     do add_insertion_handler_unsafe(ins, js_code_unsafe)
                     create_inner_node(html_code_unsafe)
                 ),
               (_nsenv, tag, args, element, attribute ->
                 match attribute with
                 | ~{class style events events_options href} ->
                   //Handle classes
                   do List.iter(
                        class -> add_class_name(element, class),
                        class)
                   //Handle non-class typed styles
                   do List.iter(
                        (| ~{name value} ->
                           add_style_application(cons, element, name, value)),
                           Css_printer.to_xhtml_style(style))
                   //Handle events
                   do List.iter(
                        (|~{name value} ->
                           //At this stage, we know that [value] is an event.
                           //We need to bind it. Depending on its memory
                           //representation, we either perform regular [bind]
                           //(if it's a function already) or [bind_value_unsafe]
                           //(if it's a special-purpose closure-as-string).
                           match value with
                           | ~{value} ->
                             match name with
                             | {ready} | {unload} ->
                               //At this stage, we insert a default event manually, as insertion handlers don't post any event
                               add_insertion_handler_unsafe(ins, "var event = {JsInterface.default_opa_event};"^value)
                             | _ ->
                               bind_value_unsafe(element, Event.get_name(name), value)
                             end
                           | ~{expr} ->
                             match name with
                             | {ready} | {unload} ->
                               f = -> expr(default_opa_event())
                               add_insertion_handler(ins, f)
                             | _ ->
                               f = e -> expr(e)
                               opabind(element, Event.get_name(name), f)
                             end
                           end
                        ), events)
                       //Handle events options
                       do List.iter(
                           (|{name=handle value=options} ->
                           match handle with
                            | {ready} -> void //Do nothing for [onready]-style events
                            | _ ->
                              name = Event.get_name(handle)
                              stop_propagation = List.exists(_ == {stop_propagation}, options)
                              prevent_default  = List.exists(_ == {prevent_default},  options)
                              do if stop_propagation then bind_stop_propagation(element, name) else void
                              do if prevent_default  then bind_prevent_default(element, name) else void
                              void
                       ), events_options)
                   //Handle hyperlinks
                   href = match tag with
                      | "a" -> //By definition, we want each [<a>] to have a href, even if there's none
                            match href with
                               | {none} ->
                                  match List.find({~name namespace=_ value=_} -> name == "href", args)
                                   //TODO: Hack around incorrect [Xml.to_xhtml]
                                   //TODO: We should rather implement a new, safe Xml.to_xhtml
                                      | {none} -> {constant= "javascript:void(0)"}//TODO: Magic constant. Should be moved somewhere else
                                      |~{some} -> {constant=some.value}
                                  end
                               | _      -> href
                            end
                      | _   -> href
                   match href with
                      | {none} -> void
                      | ~{constant} -> set_attribute(element, "href", constant)
                      | ~{untyped}  -> //Untyped URIs go through URI.of_string's whitelisting
                               sanitized = if Uri.is_secure(untyped) then untyped
                                           else "javascript:void(0)/*Sanitized URI*/"
                               set_attribute(element, "href", sanitized)
                      | ~{typed}    -> set_attribute(element, "href", Uri.to_string(typed))
                )
              )
    do store_style_constructor(dom, cons)
    do store_insertion_handlers(dom, ins)
    wrap_as_dom(dom)

  /**
   * Check functionality supported by the client's browser.
   */
  Support = {{
    placeholder() : bool =
       %% BslDomSupport.support_placeholder %%()

    notification():bool =
      %% BslDomSupport.support_notification %%()
  }}


  /**
   * Allow to use Html5 notification system.
   */
  Notification = {{
    /**
     *Displays a notification on the clients screen
     *
     *@param img Url of the notification icon, put empty string for no icon
     *@param title Title of the notification
     *@param body Text of the notification
     */
    show(img : string, title : string, body : string):void =(
      if Support.notification()
      then %% BslDom.notification %%(img,title,body)
      else Log.warning("Notification","Your browser doesn't support Html5 notification")
    )
  }}

}}



/**
 * {1 Functions exported to the global namespace}
 */

/* TODO: move with html ?? */
type xhtml_attr('a)   = { namespace:string name:string value:'a }//TODO: Always used with [string], we can certainly normalize
type string_assoc('a) = { namespace:string name:string value:'a }//TODO: [string_assoc] is used for CSS, which doesn't have [namespace]
@opacapi type handle_assoc('a) = { name: Dom.event.kind value: 'a }
@opacapi type event_handler = handle_assoc(xhtml_event)
sassoc_full(namespace, name, value) = ~{ namespace name value }: string_assoc
sassoc(name, value) = sassoc_full("",name,value)
sassoc_insert(sassoc : string_assoc('a), list : list(string_assoc('a))) =
  /* used to avoid duplicate (leading to invalid XHTML) */
  match list with
  | [] -> [sassoc]
  | [hd|tl] -> if hd.name == sassoc.name
    then [sassoc | tl]
    else [hd | sassoc_insert(sassoc, tl)] ;
hassoc(a, b) = { name = a; value = b }: handle_assoc


/**
 * Abstract type that represent a style constructor
 */
//TODO: Restore type style_constructor = external

/**
 * Abstract type that represent a collection of insertion handlers.
 *
 * Some special handlers (e.g. [onload] and [onready]) have a non-standard behavior, i.e. if they
 * are attached to a xhtml tree, even after loading, they need to be triggered immediately when
 * the tree is inserted in the page.
 *
 * This client-only data structure holds handlers that have been set aside to be executed upon
 * insertion.
 */
//TODO: Restore type insertion_handlers = external


/**
 * Abstract type that represent a Dom element
 */
type dom_element = external

/**
 * Abstract type that represent a style constructor
 */
type style_constructor = external

/**
 * Abstract type that represent a collection of insertion handlers.
 * That handlers is executed when a [dom_element] is inserted on the Dom.
 */
type insertion_handlers = external


/* Used by the parser, in [actions.trx]. */
@opacapi Dom_select_id = Dom.select_id
@opacapi Dom_select_class = Dom.select_class



type Dom.Transformation.verb = { append } / {prepend} / {set}
@opacapi type Dom.Transformation.subject = { value: string} / {content:xhtml} / {css:`Css.properties`} / {property : string ; value_p : string}
@opacapi type Dom.transformation = { jq : dom ; subject : Dom.Transformation.subject ; verb : Dom.Transformation.verb} // TODO: turn into @abstract.

@opacapi Dom_transform = Dom.transform


/**
 * @deprecated
 */
type Client.action = Dom.transformation
@deprecated({hint = "use Dom.transform"}) exec(l:list(Dom.transformation)) = Dom.transform(l)
@deprecated({hint = "use Dom.transform"}) exec_actions = Dom.transform
