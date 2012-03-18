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
 * A configurable hierarchical list widget.
 *
 * @author Frederic Ye, 2010
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.3
 */

/*
 * {1 About this module}
 *
 * Hierarchical list widget, aka WHList widget
 *
 * An item is an element of the hierarchical list
 *
 * {1 Where should I start?}
 *
 * If you simply want an hlist widget, use [WHList.html]
 *
 * {1 Recommandations}
 *
 * {1 What if I need more?}
 *
 * {1 TODO (ideas) }
 *
 * - Multi-select
 * - DnD
 */

import stdlib.web.client
import stdlib.widgets.core

/**
 * {1 Types defined in this module}
 */

/**
 * A meaningful item position (used for insertion, deletion)
 *
 * [at_begin] means the first item position aka list start
 * [at_select] means the selected item position
 * [after_select] means just after the selected item position
 * [at_end] means the last item position aka list end
 * -- [fixed : int] means an exact item position (hence fixed) [NOT HANDLED FOR THE MOMENT] --
 */
type WHList.pos = { at_begin }
                / { at_select }
                / { after_select }
                / { at_end }
                / { fixed: int }

/**
 * An item content
 *
 * [title] the title to use for the link (mouseover usually)
 * [value] the content XHTML value
 */
type WHList.item_content = { title: string; value: xhtml }

/**
 * An item state
 */
type WHList.item_state = {
  selected: bool
  checked: bool
}

/**
 * An item properties
 */
type WHList.item_properties = {
  selectable: bool // can be selected (with a click)
  checkable: bool // can be checked (with a checkbox)
  no_sons: bool // can't have sons
}

/**
 * A helper for adressing items key
 */
type WHList.key_helper('key) = { // data structure for addressing items (NOTE: fathers are hard-coded in the addresses)
  stringify: 'key -> string // should be usable as DOM ID suffix (TODO: use magic.serialize and a suitable hash function by default)
  father: 'key -> option('key)
}

/**
 * An item
 */
type WHList.item = {
  content: WHList.item_content
  state: WHList.item_state
  properties: WHList.item_properties
  styler: WStyler.styler
}

type WHList.stylers = {
  hlist: WStyler.styler
  ul: WStyler.styler
  item: WStyler.styler
  item_content: WStyler.styler
  item_sons: WStyler.styler
  item_selectable: WStyler.styler
  item_checkable: WStyler.styler
  item_no_sons: WStyler.styler
}

/**
 * The widget configuration
 */
type WHList.config('key) = {
  prefix_class: option(string)
  helper: WHList.key_helper('key) // the key helper
  on_add: option('key), WHList.item -> option(('key, WHList.item)) // item added
  on_select: 'key -> bool // item selected (clicked)
  on_remove: 'key -> bool // item removed
  insert_pos: WHList.pos // item insert position
  delete_pos: WHList.pos // item to select on delete
  select_on_hover: option('key -> void) // action to do when hover - maybe we should let the user handle the  mouseover?
  stylers: WHList.stylers
}

WHList = {{

  /*
  * {1 ID shortcuts}
  */

  // Generate an ID
  gen_id(prefix_id:string, suffix:string) =
    "{prefix_id}_hlist{suffix}"

  hlist_id(prefix_id:string) =
    gen_id(prefix_id, "")

  item_id(prefix_id, s_key) = // s_key = stringified key
    gen_id(prefix_id, "_item_{s_key}")

  /*
  * {1 Class shortcuts}
  */

  // Generate a class
  gen_class(prefix_class:string, suffix:string) =
    "{prefix_class}_hlist{suffix}"

  hlist_class(prefix_class:string) =
    gen_class(prefix_class, "")

  item_class(prefix_class:string) =
    gen_class(prefix_class, "_item")

  item_content_class(prefix_class:string) =
    gen_class(prefix_class, "_item_content")

  item_sons_class(prefix_class:string) =
    gen_class(prefix_class, "_item_sons")

  item_selected_class(prefix_class:string) =
    gen_class(prefix_class, "_item_selected")

  item_selectable_class(prefix_class:string) =
    gen_class(prefix_class, "_item_selectable")

  item_checkable_class(prefix_class:string) =
    gen_class(prefix_class, "_item_checkable")

  item_no_sons_class(prefix_class:string) =
    gen_class(prefix_class, "_item_no_sons")

  /**
   * {1 Configuration}
   */

  default_config = {
    prefix_class = none
    helper = {
      stringify = OpaSerialize.serialize
      father(_key) = none
    } : WHList.key_helper
    on_add = _key, _item -> none
    on_select = _key -> true
    on_remove = _key -> true
    insert_pos = { at_end }
    delete_pos = { after_select }
    select_on_hover = none
    stylers = {
      hlist = WStyler.empty
      ul = WStyler.empty
      item = WStyler.empty
      item_content = WStyler.empty
      item_sons = WStyler.empty
      item_selectable = WStyler.empty
      item_checkable = WStyler.empty
      item_no_sons = WStyler.empty
    }
  } : WHList.config

  default_config_with_css(css_prefix:string) = { default_config with
    prefix_class = some(css_prefix)
  } : WHList.config

  int_config() = { default_config with
    helper = {
      stringify = key -> "{key}"
      father = _key -> none
    } : WHList.key_helper
  } : WHList.config

  /**
   * {1 High-level interface}
   */

  /*
  * {1 Constructors}
  */

  // Item Content //

  /**
  * Quick constructor for making a simple item content
  *
  * @param s the title and content to use
  * @return the corresponding item_content
  */
  make_simple_content(s:string) = { title=s; value=Xhtml.of_string(s) }

  // Item //

  /**
   * Constuctor for making an item
   */
  make_item(content:WHList.item_content,
           state:WHList.item_state,
           properties:WHList.item_properties) = {
    content = content
    state = state
    properties = properties
    styler = WStyler.empty
  } : WHList.item

  // @private //

  item_selectable_class_option(prefix_class, item) =
    if item.properties.selectable then some(item_selectable_class(prefix_class))
    else none

  is_selectable(prefix_class, id) =
    Dom.has_class(#{id}, item_selectable_class(prefix_class))

  item_checkable_class_option(prefix_class, item) =
    if item.properties.checkable then some(item_checkable_class(prefix_class))
    else none

  is_checkable(prefix_class, id) =
    Dom.has_class(#{id}, item_checkable_class(prefix_class))

  item_no_sons_class_option(prefix_class, item) =
    if item.properties.no_sons then some(item_no_sons_class(prefix_class))
    else none

  has_no_sons(prefix_class, id) =
    Dom.has_class(#{id}, item_no_sons_class(prefix_class))

  insert_if_some(elt, list) =
    match elt with
    | ~{some} -> some +> list
    | {none} -> list

  build_item_classes(prefix_class:string, item:WHList.item) =
    list = [item_class(prefix_class)]
    list = insert_if_some(item_selectable_class_option(prefix_class, item), list)
    list = insert_if_some(item_checkable_class_option(prefix_class, item), list)
    list = insert_if_some(item_no_sons_class_option(prefix_class, item), list)
    list

  apply_item_styles(config: WHList.config, item:WHList.item, elt: xhtml) =
    res = elt |> WStyler.add(config.stylers.item, _)
    res = if item.properties.selectable then res |> WStyler.add(config.stylers.item_selectable, _)
          else res
    res = if item.properties.checkable then res |> WStyler.add(config.stylers.item_checkable, _)
          else res
    res = if item.properties.no_sons then res |> WStyler.add(config.stylers.item_no_sons, _)
          else res
    res

  /**
   * Get the current position of an item
   *
   * @param current_id the item XHTML id
   * @return the optional current position of the item
   */
  current_pos(current_id:string) =
    if Dom.length(#{current_id}) > 0 then
      some(Dom.length(Dom.select_inside(Dom.select_previous_several(#{current_id}), Dom.select_raw_unsafe("li"))) + 1)
    else none

  /*
  * {1 Actions}
  */

  // CREATION //

  /**
   * Create the "content" of an item
   *
   * @param config the widget configuration
   * @param prefix_id the prefix id to use
   * @param key the item key
   * @param item the item to use for content creation
   * @return The XHTML corresponding to an item content
   */
  @private
  create_item_content(config:WHList.config, prefix_id:string, key:'key, item:WHList.item) =
    ocs = _event -> if item.properties.selectable then _ = select_item(config, prefix_id, key, true); void else void
    html = <a title={item.content.title} onclick={ocs}>{item.content.value}</a>
    omo = action, _ ->
      _ = internal_select_item(config, prefix_id, config.helper.stringify(key))
      _ = action(key)
      void
    match config.select_on_hover with
    | {~some} ->
      html |> WCore.add_binds([({mouseover}, omo(some, _))], _)
    | {none} -> html

  /**
    * Create the item node, in our implementaion, it's a <li>
    *
    * @param config the widget configuration
    * @param prefix_id the prefix id to use
    * @param key the item key
    * @param item the item to use for content creation
    * @return The XHTML corresponding to an item
    */
  @private
  create_item_node(config:WHList.config, prefix_id:string, key:'key, item:WHList.item) =
    s_key = config.helper.stringify(key)
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    <li class={build_item_classes(prefix_class, item)} id=#{item_id(prefix_id, s_key)}>
     {<ul>
        {<li class={[item_content_class(prefix_class)]}>{create_item_content(config, prefix_id, key, item)}</li>
         |> WStyler.add(config.stylers.item_content, _)}
        {if item.properties.no_sons then <></>
         else <li class={[item_sons_class(prefix_class)]}>{<ul/> |> WStyler.add(config.stylers.ul, _)}</li>
              |> WStyler.add(config.stylers.item_sons, _)}
      </ul> |> WStyler.add(config.stylers.ul, _)}
    </li> |> apply_item_styles(config, item, _)

  /**
   * Extract the stringified key of an id
   *
   * N.B: suppose the stringified key didn't contain '_'
   *
   * @param id the complete item XHTML id
   * @return the optional stringified key extracted
   */
  extract_item_s_key(id:string) =
    list = String.explode("_", id)
    if List.length(list) > 0 then some(List.max(list))
    else none

  // SELECTION //

  /**
   * Unselect the previous selected item(s)
   *
   * @param prefix_id the prefix id to use
   * @return true if successful
   */
  @private
  unselect_all(config:WHList.config, prefix_id:string) =
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    _ = Dom.remove_class(#{"{hlist_id(prefix_id)} .{item_class(prefix_class)}"}, item_selected_class(prefix_class))
    Dom.length(#{"{hlist_id(prefix_id)} .{item_selected_class(prefix_class)}"}) == 0

  @private
  internal_select_item(config:WHList.config, prefix_id:string, s_key:string) =
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    current_id = item_id(prefix_id, s_key)
    _ = unselect_all(config, prefix_id)
    if Dom.length(#{current_id}) > 0 then
      _ = Dom.add_class(#{current_id}, item_selected_class(prefix_class))
      current_pos(current_id)
    else none

  /**
   * Select an item by its key
   *
   * @param config the widget configuration
   * @param prefix_id the prefix id to use
   * @param key the item key
   * @param notify notify the user by calling on_select // (TOOD: useful ? should always be true ?)
   * @return true if successful
   */
  select_item(config:WHList.config('key), prefix_id:string, key:'key, notify:bool) =
    ok = not(notify) || config.on_select(key)
    if ok then
      internal_select_item(config, prefix_id, config.helper.stringify(key))
    else none

  // INSERTION //

  /**
   * Insert an item at a meaningful position
   *
   * @param config the widget configuration
   * @param prefix_id the prefix id to use
   * @param key the inserted item key
   * @param item the item to insert
   * @param father_sel the father selector, ie the parent node
   * @param pos the position to insert the item
   * @param sel_key consider this optional key as selected
   * @param notify notify the user by calling on_select
   * @return the optional position of the inserted item
   */
  @private
  internal_insert_item(config:WHList.config, prefix_id:string, key:'key, item:WHList.item, father_sel:string, pos:WHList.pos, sel_key:option('key), notify:bool) =
    current_id = item_id(prefix_id, config.helper.stringify(key))
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    // If the item (it's key) already exists, do not insert_item
    // TODO: configurable? replace item?
    if Dom.length(#{current_id}) > 0 || Dom.length(Dom.select_raw_unsafe(father_sel)) == 0 then none
    else
      xhtml_item = create_item_node(config, prefix_id, key, item)
      selected = match sel_key with
      | ~{some} -> item_id(prefix_id, config.helper.stringify(some))
      | {none} -> "{father_sel} > li.{item_selected_class(prefix_class)}:last"
      do match pos with
      | {at_begin} ->
        _ = Dom.put_at_start(Dom.select_raw_unsafe(father_sel),Dom.of_xhtml(xhtml_item))
        void
      | {at_select} ->
        if Dom.length(Dom.select_raw_unsafe(selected)) == 0 then
          _ = Dom.put_at_start(Dom.select_raw_unsafe(father_sel),Dom.of_xhtml(xhtml_item))
          void
        else
          jq = Dom.of_xhtml(xhtml_item) : dom
          _ = Dom.put_before(Dom.select_raw_unsafe(selected), jq)
          void
      | {after_select} ->
        if Dom.length(Dom.select_raw_unsafe(selected)) == 0 then
          _ = Dom.put_at_end(Dom.select_raw_unsafe(father_sel), Dom.of_xhtml(xhtml_item))
          void
        else
          jq = Dom.of_xhtml(xhtml_item) : dom
          _ = Dom.put_after(Dom.select_raw_unsafe(selected), jq)
          void
      | {at_end} ->
        _ = Dom.put_at_end(Dom.select_raw_unsafe(father_sel), Dom.of_xhtml(xhtml_item))
        void
      | { fixed=_ } -> void // TO IMPLEMENT
      do if item.state.selected then
        _ = select_item(config, prefix_id, key, notify)
        void
      current_pos(current_id)

  /**
   * Insert an item with the behaviour defined in the configuration
   *
   * @param config the widget configuration
   * @param prefix_id the prefix id to use
   * @param key the inserted item key
   * @param item the item to insert
   * @param sel_key consider this optional key as selected
   * @param notify notify the user by calling on_add and on_select
   * @return the optional position of the inserted item
   */
  insert_item(config:WHList.config, prefix_id:string, key:'key, item:WHList.item, sel_key:option('key), notify:bool) =
    father = config.helper.father(key)
    father_sel = match father with
      | ~{some} ->
        f_key = config.helper.stringify(some)
        father_id = item_id(prefix_id, f_key)
        if has_no_sons(prefix_id, "{father_id}") then Option.none
        else Option.some("#{father_id} > ul > li > ul")
      | {none} ->
        Option.some("#{hlist_id(prefix_id)} > ul")
    match father_sel with
    | ~{some=father_sel} ->
      if notify then
        match config.on_add(Option.some(key), item) with
        | ~{some=(key, item)} ->
          internal_insert_item(config, prefix_id, key, item, father_sel, config.insert_pos, sel_key, notify)
        | {none} -> none
        end
      else internal_insert_item(config, prefix_id, key, item, father_sel, config.insert_pos, sel_key, notify)
    | {none} -> none

  // REPLACEMENT //

  /**
  * Replace an item content with a new one with possible new properties (stays at the same position)
  *
  * @param config the widget configuration
  * @param prefix_id the prefix id to use
  * @param key the replaced item key
  * @param item the item to use for replacement
  * @return optional position of the replaced item
  */
  replace_item_content(config:WHList.config, prefix_id:string, key:'key, item:WHList.item) =
    s_key = config.helper.stringify(key)
    current_id = item_id(prefix_id, s_key)
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    if Dom.length(#{current_id}) > 0 then
      new_item_content = create_item_content(config, prefix_id, key, item)
      do Dom.transform([#{"{current_id} > ul > li.{item_content_class(prefix_class)}"} <- new_item_content])
      current_pos(current_id)
    else none

  // DELETION //

  @private
  internal_remove_item(config:WHList.config, prefix_id:string, s_key:string) =
    current_id = item_id(prefix_id, s_key)
    current_pos = current_pos(current_id)
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    // Does the deleted item (current) have the selected item?
    selected_is_currents_son = Dom.length(#{"{item_id(prefix_id, s_key)} li.{item_sons_class(prefix_class)} li.{item_selected_class(prefix_class)}"}) > 0
    selected = if selected_is_currents_son then current_id
               else "{hlist_id(prefix_id)} .{item_selected_class(prefix_class)}:last"
    selected_id = Dom.get_property_unsafe(#{selected}, "id")
    // We also could use Dom.get_parent_one then select the first/last li
    parent = Dom.select_parent_one(#{selected_id})
    next = Dom.select_next_one(#{selected_id})
    prev = Dom.select_previous_one(#{selected_id})
    _ = Dom.remove(#{current_id})
    all =  Dom.select_children(parent)
    first= Dom.select_first_one(all)
    last = Dom.select_last_one(all)
    first = if Dom.is_empty(first) then "" else Dom.get_id(first)
    last =  if Dom.is_empty(last) then "" else Dom.get_id(last)
    if current_id == selected_id
    then
      id = match config.delete_pos with
        | {at_begin} -> first
        | {at_select} -> if Dom.is_empty(prev) then first else Dom.get_id(prev)
        | {after_select} -> if Dom.is_empty(next) then last else Dom.get_id(next)
        | {at_end} -> last
        | {fixed=_} -> ""
      if (id != "") && (Dom.length(#{id}) > 0)
      then
        _ = Dom.trigger(Dom.select_raw_unsafe("#{id} > ul > li.{item_content_class(prefix_class)} > a"), {click})
        current_pos
      else
        _ = unselect_all(config, prefix_id)
        none
    else current_pos

  /**
  * Remove an item by its key
  *
  * @param config the widget configuration
  * @param prefix_id the prefix id to use
  * @param key the removed item key
  * @param notify if want to be notified by calling on_remove
  * @return the optional position of the removed item
  */
  remove_item(config:WHList.config, prefix_id:string, key:'key, notify:bool) =
    if not(notify) || config.on_remove(key) then
      internal_remove_item(config, prefix_id, config.helper.stringify(key))
    else none

  // HTML //

  @private
  on_html_ready(config:WHList.config, prefix_id:string, init_items:list(('key, WHList.item)), _event:Dom.event) =
    // Empty the "fake" html used for "loading time" / "bots"
    do Dom.remove_content(#{"{hlist_id(prefix_id)} > ul"})
    // FIXME: the widget shouldn't find a way to check the uniqueness of init_items keys ?
    f((key, item), acc) =
      _ = insert_item(config, prefix_id, key, item, none, false) // No selection notification
      if item.state.selected then some(key)
      else acc
    to_sel = List.fold(f, init_items, none)
    // Select the last item indicated
    match to_sel with
    | ~{some} -> _ = select_item(config, prefix_id, some, true); void
    | {none} -> void

  /**
  * The main function to be called for creating an hlist widget, with some default items
  * The init_items is not "hierarchical" because the hierarchy is given by the <father> function in config.helper
  *
  * @param config the widget configuration
  * @param prefix_id the prefix id to use
  * @param init_items the list of items (and their key) to show at initilialization
  * @return The XHTML corresponding to the widget
  */
  html(config:WHList.config, prefix_id:string, init_items:list(('key, WHList.item))) =
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    <div id=#{hlist_id(prefix_id)} class={[hlist_class(prefix_class)]} onready={on_html_ready(config, prefix_id, init_items, _) }>
      {<ul>{
        // FIXME: we just display a flat list here,
        // we should keep the hierarchy that was meant to be...
        // we should generate the proper HTML right away, without using an onready
        xhtml_list = List.fold_backwards((_key, item), acc ->
          <li><a title={item.content.title}>{item.content.value}</a></li> +> acc
        , init_items, [])
        Xhtml.createFragment(xhtml_list)
      }</ul> |> WStyler.add(config.stylers.ul, _)}
    </div> |> WStyler.add(config.stylers.hlist, _)

}}
