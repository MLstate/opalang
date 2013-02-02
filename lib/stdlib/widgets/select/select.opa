/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * A configurable select widget.
 *
 * @author Frederic Ye, 2010
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.1
 */

import stdlib.web.client
import stdlib.widgets.core
import stdlib.widgets.deprecated.hlist

/*
 * {1 About this module}
 *
 * Select menu widget
 * Inspired by https://github.com/fnagel/jquery-ui
 *
 * {1 TODO}
 *
 */

/**
 * An item content
 */
type WSelect.item_content('key) = {
  key: 'key
  title: string
  value: xhtml
}

/**
 * An item state
 */
type WSelect.item_state = {
  selected: bool
}

/**
 * An item
 */
type WSelect.item('key) = {
  content: WSelect.item_content('key)
  state: WSelect.item_state
  styler: WStyler.styler
}

type WSelect.stylers = {
  select: WStyler.styler
  icon: WStyler.styler
  ul: WStyler.styler
}

/**
 * The widget configuration
 */
type WSelect.config('key) = {
  prefix_class: option(string)
  on_select: 'key -> bool // what is this bool ?
  stringify: 'key -> string
  menu_height: option(int)
  menu_width: option(int) // not used for the moment
  z_index: int
  show_duration: int
  hide_duration: int
  stylers: WSelect.stylers
}

WSelect = {{

  /**
  * {1 ID/Class shortcuts}
  */

  @private
  gen_id(prefix_id:string, suffix:string) =
    "{prefix_id}_select{suffix}"

  @private
  select_id(prefix_id:string) =
    gen_id(prefix_id, "")

  @private
  current_id(prefix_id:string) =
    gen_id(prefix_id, "_current")

  @private
  focus_id(prefix_id:string) =
    gen_id(prefix_id, "_focus")

  /**
  * {1 Class shortcuts}
  */

  @private
  gen_class(prefix_class:string, suffix:string) =
    "{prefix_class}_select{suffix}"

  @private
  current_class(prefix_class:string) =
    gen_class(prefix_class, "_current")

  @private
  select_icon_class(prefix_class:string) =
    gen_class(prefix_class, "_icon")

  /**
  * {1 Constructors}
  */

  make_item(content:WSelect.item_content,
           state:WSelect.item_state) = {
    content = content
    state = state
    styler = WStyler.empty
  } : WSelect.item

  /**
   * {1 Configuration}
   */

  default_config = {
    prefix_class = none
    on_select(_) = true
    stringify = OpaSerialize.serialize // FIXME: not usable as DOM id !
    menu_height = none
    menu_width = none
    z_index = 9999
    show_duration = 0
    hide_duration = 0
    stylers = {
      select = WStyler.empty
      icon = WStyler.empty
      ul = WStyler.empty
    }
  } : WSelect.config

  default_config_with_css(css_prefix:string) = { default_config with
    prefix_class = some(css_prefix)
  } : WSelect.config

  to_hlist_item(item:WSelect.item) =
    value = item.content.value
    WHList.make_item(
      {title=item.content.title value=value},
      {selected=item.state.selected checked=false},
      {selectable=true checkable=false no_sons=true}
    )

  to_hlist_config(config:WSelect.config, prefix_id:string, items:list(WSelect.item), (cancel_cb: -> void)) = {
    WHList.default_config with
    prefix_class = config.prefix_class
    helper = {
      stringify = item -> "{config.stringify(item.content.key)}"
      father = _key -> none
    } : WHList.key_helper
    stylers.hlist = match config.menu_height with
                    | {~some} -> WStyler.make_style(css {
                                   height: {some}px;
                                   overflow: auto;
                                 })
                    | {none} -> WStyler.empty
                    end
    stylers.ul = config.stylers.ul
    on_select = internal_on_select(config, prefix_id, items, _)
    select_on_hover = some(item -> rebind_current(config, prefix_id, item, items, cancel_cb))
  }

  to_hlist_items(items:list(WSelect.item)) =
    if List.is_empty(items)
    then none
    else
      f(item, (acc, selected)) =
        ((item, to_hlist_item(item)) +> acc,
        if item.state.selected then item else selected)
      (list, selected) = List.fold(f, items, ([], List.head(items)))
      Option.some((List.rev(list), selected))

  /**
  * {1 Actions}
  */

  /**
   * Action to do when the user clicks on the widget entry point (selected element)
   */
  @private
  internal_on_choose(prefix_id:string) =
    // Focus on focus_id if the hlist is not visible
    if not(Dom.contains_selector(#{WHList.hlist_id(prefix_id)}, ":visible")) then
      _ = Dom.give_focus(#{focus_id(prefix_id)})
      void

  @private
  rebind_current(config:WSelect.config, prefix_id:string, item:WSelect.item, items:list(WSelect.item), (cancel_cb: ->void)) =
    _ = Dom.unbind_event(#{focus_id(prefix_id)}, {keyup})
    _ = Dom.bind(#{focus_id(prefix_id)}, {keyup}, key_listener(config, prefix_id, item, items, cancel_cb, _))
    void

  /**
   * Find an item in a list, and apply an operation to find the "next" item
   * In our case, apply succ or pred
   */
  @private
  find_item(config:WSelect.config, prefix_id:string, item:WSelect.item, items:list(WSelect.item), (op:int->int), def_el:WSelect.item) =
    (elt, height, _found) = List.foldi(
      index, elt, (next, height, found) ->
        h = Dom.get_outer_height(#{WHList.item_id(prefix_id, config.stringify(elt.content.key))})
        if elt == item then
          idx = op(index)
          nh = if idx < index then height-h else height+h
          (List.get(idx, items), nh, true)
        else if found then (next, height, found)
        else (next, height+h, found)
    , items, (none, 0, false))
    list_height = Dom.get_inner_height(#{WHList.hlist_id(prefix_id)})
    match elt with
    | {~some} ->
      height = height - list_height / 2
      do Dom.set_scroll_top(#{WHList.hlist_id(prefix_id)}, height)
      some
    | {none} ->
      height = if height < 0 then list_height else 0
      do Dom.set_scroll_top(#{WHList.hlist_id(prefix_id)}, height)
      def_el

  @private
  sel_next(config:WSelect.config, prefix_id:string, item:WSelect.item, items:list(WSelect.item), (cancel_cb : ->void)) =
    if List.is_empty(items)
    then void
    else
    hlist_config = to_hlist_config(config, prefix_id, items, cancel_cb)
    def = List.head(items)
    next = find_item(config, prefix_id, item, items, succ, def)
    _ = WHList.select_item(hlist_config, prefix_id, next, false)
    do rebind_current(config, prefix_id, next, items, cancel_cb)
    void

  @private
  sel_prev(config:WSelect.config, prefix_id:string, item:WSelect.item, items:list(WSelect.item), (cancel_cb : ->void)) =
    if List.is_empty(items)
    then void
    else
    hlist_config = to_hlist_config(config, prefix_id, items, cancel_cb)
    def = List.head(List.rev(items))
    prev = find_item(config, prefix_id, item, items, pred, def)
    _ = WHList.select_item(hlist_config, prefix_id, prev, false)
    do rebind_current(config, prefix_id, prev, items, cancel_cb)
    void

  @private
  validate_current(config:WSelect.config, prefix_id:string) =
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    // Careful with the selector here, very sensitive to change in hlist...
    _ = Dom.trigger(#{"#{WHList.hlist_id(prefix_id)} > ul > li.{WHList.item_selected_class(prefix_class)} > ul > li > a"}, {click})
    // Explicitely give blur because the blur is not triggered with "click"
    // FIXME: commented for now since the new [Dom] module breaks [giveBlur]
    // _ = jQuery.giveBlur(#{focus_id(prefix_id)})
    _ = Dom.trigger(#{focus_id(prefix_id)}, {blur})
    void

  @private
  key_listener(config:WSelect.config, prefix_id:string, item:WSelect.item, items:list(WSelect.item), (cancel_cb : ->void), evt:Dom.event) =
    match Client.get_keycode(evt) with
    | 38 -> // event key up
      sel_prev(config, prefix_id, item, items, cancel_cb)
    | 40 -> // event key down
      sel_next(config, prefix_id, item, items, cancel_cb)
    | 9 | 27 -> // TAB, ESC
      cancel_cb()
    | 13 -> // event key enter
      validate_current(config, prefix_id)
    | _ ->
      // TODO: maybe we should select the first element beginning with this letter? (if it's a string list?)
      void

  @private
  on_focus(config:WSelect.config, prefix_id:string, item:WSelect.item, items:list(WSelect.item), _evt:Dom.event) =
    cancel_cb() = _ = Dom.bind(#{focus_id(prefix_id)}, {blur}, (_->void)) void
    _ = Dom.transition(#{WHList.hlist_id(prefix_id)}, Dom.Effect.with_duration({millisec = config.show_duration}, Dom.Effect.show()))
    if List.length(items) > 1 then
      _ = Dom.bind(#{focus_id(prefix_id)}, {keyup}, key_listener(config, prefix_id, item, items, cancel_cb, _))
      void

  @private
  on_blur(config:WSelect.config, prefix_id:string, evt:Dom.event) =
    do Log.debug("Event",evt)
    if (evt.kind == {mousedown})
    then _ = Dom.give_focus(#{focus_id(prefix_id)}) void
    else
        _ = Dom.unbind_event(#{current_id(prefix_id)}, {keyup})
    sleep_fun = ->
      _ = Dom.transition(#{WHList.hlist_id(prefix_id)}, Dom.Effect.with_duration({millisec = config.hide_duration}, Dom.Effect.hide()))
      void
    // We need a delay here because the selection is not detected otherwise
    // The delay has been arbitrary chosen, should be tested more
    sleep(100, sleep_fun)

  /**
   * Generate the current selected item HTML
   */
  @private
  gen_current_item(config:WSelect.config, prefix_id:string, item:WSelect.item, items:list(WSelect.item)) =
    if List.is_empty(items)
    then <></>
    else
    cancel_cb() = _ = Dom.bind(#{focus_id(prefix_id)}, {blur}, (_->void)) void
    hlist_config = to_hlist_config(config, prefix_id, items, cancel_cb)
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    <a title={item.content.title} class={[current_class(prefix_class)]} id={current_id(prefix_id)} onclick={_ ->
      do internal_on_choose(prefix_id)
      _ = find_item(config, prefix_id, item, items, (i->i), List.head(items))
      _ = WHList.select_item(hlist_config, prefix_id, item, false)
      void
    }>
      {item.content.value}
      {<span class={[select_icon_class(prefix_class)]}></span> |> WStyler.add(config.stylers.icon, _)}
    </a>

  /**
   * Action to do on hlist select
   */
  @private
  internal_on_select(config:WSelect.config, prefix_id:string, items:list(WSelect.item), item:WSelect.item) =
    _ = Dom.hide(#{WHList.hlist_id(prefix_id)})
    html = gen_current_item(config, prefix_id, item, items)
    _ = Dom.put_replace(#{current_id(prefix_id)}, Dom.of_xhtml(html))
    config.on_select(item.content.key)


  disable_blur(prefix_id,_config)= _ ->
    Dom.unbind_event(#{focus_id(prefix_id)},{blur})

  enable_blur(prefix_id,config)= _ ->
    _ = Dom.bind(#{focus_id(prefix_id)},{blur}, on_blur(config, prefix_id, _))
    void

  /**
   * Entry point
   */
  html(config:WSelect.config, prefix_id:string, items:list(WSelect.item)) =
    cancel_cb() = _ = Dom.bind(#{focus_id(prefix_id)}, {blur}, (_->void)) void
    hlist_config = to_hlist_config(config, prefix_id, items, cancel_cb)
    hlist_opt = to_hlist_items(items)
    match hlist_opt with
      | {some=(hlist_items, selected)} ->
          <div>
            {gen_current_item(config, prefix_id, selected, items)}
            {<a id={focus_id(prefix_id)}>{
              WHList.html(hlist_config, prefix_id, hlist_items)
              |> WStyler.add(WStyler.make_style(css {
                   position : absolute;
                   z-index : {some(config.z_index)}
                 }), _)
              |> WStyler.add(WStyler.make_style(css {display:none}), _)
              |> WCore.add_binds([
                 ({mouseover}, disable_blur(prefix_id,config)),
                 ({mouseout}, enable_blur(prefix_id,config))], _)
            }</a> |> WCore.add_binds([
                       ({focus}, on_focus(config, prefix_id, selected, items, _)),
                       ({blur}, on_blur(config, prefix_id, _))], _)}
          </div> |> WStyler.add(config.stylers.select, _)
      | {none} -> <></>

}}

SimpleSelect = {{

  /**
    * This function is a thin wrapper over <select><option .../>...</>
    * @param options the options to be displayed, as a list of pairs:
    *                xhtml to be displayed and a value
    * @param callback the callback to be invoked when a new value is selected
    * @return the xhtml of the selection component
    */
  xhtml(options : list((xhtml, 'a)), callback : 'a -> void, selected : option('a)) : xhtml =
    id = Dom.fresh_id()
    getval() = List.unsafe_get(String.to_int(Dom.get_value(#{id})), options).f2;
    mk_opt(i, (x, v)) =
      opt = <option value={i} selected=selected>{x}</>
      if some(v) == selected then
         Xhtml.add_attribute_unsafe("selected", "selected", opt)
      else
        opt
    <select class=span2 id={id} onchange={ _ -> callback(getval())}>
      {List.mapi(mk_opt, options)}
    </select>

}}
