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
 * @author Frederic Ye 2010
 * @author Nicolas Glondu 2011
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.4
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
  selected : bool
  checked  : bool
}

/**
 * An item properties
 */
type WHList.item_properties = {
  selectable : bool // can be selected (with a click)
  checkable  : bool // can be checked (with a checkbox)
  no_sons    : bool // can't have sons
}

/**
 * A helper for adressing items key
 */
type WHList.key_helper('key) = { // data structure for addressing items (NOTE: fathers are hard-coded in the addresses)
  stringify : 'key -> string // should be usable as DOM ID suffix (TODO: use magic.serialize and a suitable hash function by default)
  father    : 'key -> option('key)
}

/**
 * An item
 */
type WHList.item = {
  content    : WHList.item_content
  state      : WHList.item_state
  properties : WHList.item_properties
  styler     : WStyler.styler
}


type WHList.tree('key) =
   { key  : 'key
     item : WHList.item
     sons : list(WHList.tree('key)) }

type WHList.stylers = {
  hlist           : WStyler.styler
  ul              : WStyler.styler
  item            : WStyler.styler
  item_content    : WStyler.styler
  item_sons       : WStyler.styler
  item_selectable : WStyler.styler
  item_checkable  : WStyler.styler
  item_no_sons    : WStyler.styler
}

/**
 * The widget configuration
 */
type WHList.config('key) = {
  prefix_id       : string
  prefix_class    : option(string)
  helper          : WHList.key_helper('key) // the key helper
  on_add          : option('key), WHList.item -> option(('key, WHList.item)) // item added
  on_select       : 'key -> bool // item selected (clicked)
  on_remove       : 'key -> bool // item removed
  insert_pos      : WHList.pos // item insert position
  delete_pos      : WHList.pos // item to select on delete
  select_on_hover : option('key -> void) // action to do when hover - maybe we should let the user handle the  mouseover?
  stylers         : WHList.stylers
}

WHList = {{

  /**
   * Compute the prefix_class from a config containing a prefix_class field of type option(string)
   * If no prefix_class is found, returns the prefid_id in parameter
   *
   * @param config the config to use
   * @param prefix_id the prefix_id to use in case no prefix_class was defined
   */
  compute_prefix_class(config:WHList.config) =
    match config.prefix_class with
    | ~{some} -> some
    | {none} -> config.prefix_id

  /*
  * {1 ID shortcuts}
  */

  // Generate an ID
  gen_id(prefix_id:string, suffix:string) =
    "{prefix_id}_hlist{suffix}"

  hlist_id(prefix_id) = gen_id(prefix_id, "")

  // s_key = stringified key
  item_id(prefix_id, s_key) = gen_id(prefix_id, "_item_{s_key}")

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

  default_config(prefix_id) = {
    ~prefix_id
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

  default_config_with_css(prefix_id, css_prefix:string) =
    { default_config(prefix_id) with
        prefix_class = some(css_prefix)
    } : WHList.config

  int_config(prefix_id) =
    { default_config(prefix_id) with
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
   * @param key the item key
   * @param item the item to use for content creation
   * @return The XHTML corresponding to an item content
   */
  @private
  create_item_content(config:WHList.config, key:'key, item:WHList.item) =
    prefix_class = compute_prefix_class(config)
    xhtml =
      if item.properties.selectable then
        if item.content.title != "" then
          <a class="{item_content_class(prefix_class)}"
             title={item.content.title}>{item.content.value}</a>
        else <a class="{item_content_class(prefix_class)}">{item.content.value}</a>
      else
        if item.content.title != "" then
          <div class="{item_content_class(prefix_class)}"
             title={item.content.title}>{item.content.value}</div>
        else <div class="{item_content_class(prefix_class)}">{item.content.value}</div>
    events =
      if not(item.properties.selectable) then []
      else
        ocs = _ev ->
          _ = select_item(config, key, true)
          void
        [{name={click} value={expr=ocs}}]
    events =
      match config.select_on_hover with
      | {none} -> events
      | {some=action} ->
          omo = _ev ->
            _ = internal_select_item(config, config.helper.stringify(key))
            _ = action(key)
            void
          [{name={mouseover} value={expr=omo}}|events]
    sons_style = config.stylers.item_sons
    Xhtml.add_binds(events, xhtml) |> WStyler.add(sons_style, _)

  /**
    * Create the item node, in our implementaion, it's a <li>
    *
    * @param config the widget configuration
    * @param key the item key
    * @param item the item to use for content creation
    * @return The XHTML corresponding to an item
    */
  @private
  rec create_tree_node(config:WHList.config, tree:WHList.tree('key)) =
    prefix_class = compute_prefix_class(config)
    s_key = config.helper.stringify(tree.key)
    <li class={build_item_classes(prefix_class, tree.item)} id=#{item_id(config.prefix_id, s_key)}>
      {create_item_content(config, tree.key, tree.item)}
      {if tree.item.properties.no_sons then <></>
       else
         <ul class="{item_sons_class(prefix_class)}">
           {List.fold(
             subtree, acc -> create_tree_node(config, subtree) <+> acc,
             tree.sons, <></>)}
         </ul> |> WStyler.add(config.stylers.ul, _)
      }
    </li> |> apply_item_styles(config, tree.item, _)

  @private create_item_node(config, key, item) =
    create_tree_node(config, ~{key item sons=[]})

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
   * @return true if successful
   */
  @private @client
  unselect_all(config:WHList.config) =
    prefix_class = compute_prefix_class(config)
    _ = Dom.remove_class(#{"{hlist_id(config.prefix_id)} .{item_class(prefix_class)}"}, item_selected_class(prefix_class))
    Dom.length(#{"{hlist_id(config.prefix_id)} .{item_selected_class(prefix_class)}"}) == 0

  @private @client
  internal_select_item(config:WHList.config, s_key:string) =
    prefix_class = compute_prefix_class(config)
    current_id = item_id(config.prefix_id, s_key)
    _ = unselect_all(config)
    if Dom.length(#{current_id}) > 0 then
      _ = Dom.add_class(#{current_id}, item_selected_class(prefix_class))
      current_pos(current_id)
    else none

  /**
   * Select an item by its key
   *
   * @param config the widget configuration
   * @param key the item key
   * @param notify notify the user by calling on_select // (TOOD: useful ? should always be true ?)
   * @return true if successful
   */
  @client select_item(config:WHList.config('key), key:'key, notify:bool) =
    ok = not(notify) || config.on_select(key)
    if ok then
      internal_select_item(config, config.helper.stringify(key))
    else none

  // INSERTION //

  /**
   * Insert an item at a meaningful position
   *
   * @param config the widget configuration
   * @param key the inserted item key
   * @param item the item to insert
   * @param father_sel the father selector, ie the parent node
   * @param pos the position to insert the item
   * @param sel_key consider this optional key as selected
   * @param notify notify the user by calling on_select
   * @return the optional position of the inserted item
   */
  @private @client
  internal_insert_item(config:WHList.config, key:'key, item:WHList.item, father_sel:string, sel_key:option('key), notify:bool) =
    current_id = item_id(config.prefix_id, config.helper.stringify(key))
    prefix_class = compute_prefix_class(config)
    // If the item (it's key) already exists, do not insert_item
    // TODO: configurable? replace item?
    if Dom.length(#{current_id}) > 0 || Dom.length(Dom.select_raw_unsafe(father_sel)) == 0 then none
    else
      xhtml_item = create_item_node(config, key, item)
      dom_item = Dom.of_xhtml(xhtml_item) : dom
      selected = match sel_key with
        | ~{some} -> item_id(config.prefix_id, config.helper.stringify(some))
        | {none} -> "{father_sel} > li.{item_selected_class(prefix_class)}:last"
      do match config.insert_pos with
        | {at_begin} ->
          _ = Dom.put_at_start(Dom.select_raw_unsafe(father_sel), dom_item)
          void
        | {at_select} ->
          if Dom.length(Dom.select_raw_unsafe(selected)) == 0 then
            _ = Dom.put_at_start(Dom.select_raw_unsafe(father_sel), dom_item)
            void
          else
            _ = Dom.put_before(Dom.select_raw_unsafe(selected), dom_item)
            void
        | {after_select} ->
          if Dom.length(Dom.select_raw_unsafe(selected)) == 0 then
            _ = Dom.put_at_end(Dom.select_raw_unsafe(father_sel), dom_item)
            void
          else
            _ = Dom.put_after(Dom.select_raw_unsafe(selected), dom_item)
            void
        | {at_end} ->
          _ = Dom.put_at_end(Dom.select_raw_unsafe(father_sel), dom_item)
          void
        | { fixed=_ } -> void // TO IMPLEMENT
      do if item.state.selected then
        _ = select_item(config, key, notify)
        void
      current_pos(current_id)

  /**
   * Insert an item with the behaviour defined in the configuration
   *
   * @param config the widget configuration
   * @param key the inserted item key
   * @param item the item to insert
   * @param sel_key consider this optional key as selected
   * @param notify notify the user by calling on_add and on_select
   * @return the optional position of the inserted item
   */
  @client insert_item(config:WHList.config, key:'key, item:WHList.item, sel_key:option('key), notify:bool) =
    // do jlog("Inserting some item at {key}")
    prefix_class = compute_prefix_class(config)
    father = config.helper.father(key)
    father_sel = match father with
      | ~{some} ->
        // do jlog("This element has a father")
        f_key = config.helper.stringify(some)
        father_id = item_id(config.prefix_id, f_key)
        if has_no_sons(prefix_class, "{father_id}") then Option.none
        else Option.some("#{father_id} > ul.{item_sons_class(prefix_class)}")
      | {none} ->
        // do jlog("This element has no father")
        Option.some("#{hlist_id(config.prefix_id)}")
    match father_sel with
    | ~{some=father_sel} ->
      // do jlog("Father selector exists")
      if notify then
        // do jlog("It asked for notifications")
        match config.on_add(Option.some(key), item) with
        | ~{some=(key, item)} ->
          // do jlog("on_add data found: {key}")
          internal_insert_item(config, key, item, father_sel, sel_key, notify)
        | {none} ->
          // do jlog("no on_add data found")
          none
        end
      else internal_insert_item(config, key, item, father_sel, sel_key, notify)
    | {none} -> none

  // REPLACEMENT //

  /**
  * Replace an item content with a new one with possible new properties (stays at the same position)
  *
  * @param config the widget configuration
  * @param key the replaced item key
  * @param item the item to use for replacement
  * @return optional position of the replaced item
  */
  @client replace_item_content(config:WHList.config, key:'key, item:WHList.item) =
    s_key = config.helper.stringify(key)
    current_id = item_id(config.prefix_id, s_key)
    prefix_class = compute_prefix_class(config)
    if Dom.length(#{current_id}) > 0 then
      new_item_content = create_item_content(config, key, item)
      do Dom.transform([#{"{current_id} > div.{item_content_class(prefix_class)}"} <- new_item_content])
      current_pos(current_id)
    else none

  // DELETION //

  @private @client
  internal_remove_item(config:WHList.config, s_key:string) =
    current_id = item_id(config.prefix_id, s_key)
    current_pos = current_pos(current_id)
    prefix_class = compute_prefix_class(config)
    // Does the deleted item (current) have the selected item?
    selected_is_currents_son = Dom.length(#{"{item_id(config.prefix_id, s_key)} li.{item_sons_class(prefix_class)} li.{item_selected_class(prefix_class)}"}) > 0
    selected = if selected_is_currents_son then current_id
               else "{hlist_id(config.prefix_id)} .{item_selected_class(prefix_class)}:last"
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
        _ = unselect_all(config)
        none
    else current_pos

  /**
  * Remove an item by its key
  *
  * @param config the widget configuration
  * @param key the removed item key
  * @param notify if want to be notified by calling on_remove
  * @return the optional position of the removed item
  */
  @client remove_item(config:WHList.config, key:'key, notify:bool) =
    if not(notify) || config.on_remove(key) then
      internal_remove_item(config, config.helper.stringify(key))
    else none

  // HTML //

  @private build_initial_content(config:WHList.config, init_items:list(('key, WHList.item))) =
    rec add_in_tree((key,item), searched, tree:list(WHList.tree('key))) =
      new_elt = {~key ~item sons=[]}:WHList.tree('key)
      match searched:option('key) with
      | {none} -> [new_elt|tree]
      | {some=father} ->
        List.map(
          {key=k item=i sons=s} ->
            s =
              if k == father then [new_elt|s]
              else add_in_tree((key,item), searched, s)
            {key=k item=i sons=s},
          tree)
    List.fold(
      (key,item), tree ->
        father = config.helper.father(key)
        add_in_tree((key,item), father, tree),
      init_items, [])
    |> List.fold(
         tree, acc -> create_tree_node(config, tree) <+> acc,
         _, <></>)

  @private @client
  on_html_ready(config:WHList.config, to_sel:option('key)) =
    // Select the last item indicated
    match to_sel with
    | ~{some} -> _ = select_item(config, some, true); void
    | {none} -> void

  /**
  * The main function to be called for creating an hlist widget, with some default items
  * The init_items is not "hierarchical" because the hierarchy is given by the <father> function in config.helper
  *
  * @param config the widget configuration
  * @param init_items the list of items (and their key) to show at initilialization
  * @return The XHTML corresponding to the widget
  */
  html(config:WHList.config, init_items:list(('key, WHList.item))) =
    prefix_class = compute_prefix_class(config)
    f((key, item), acc) =
      if item.state.selected then some(key)
      else acc
    to_sel = List.fold(f, init_items, none)
    main_style = WStyler.merge([config.stylers.ul, config.stylers.hlist])
    <ul onready={_ -> on_html_ready(config, to_sel)}
        id=#{hlist_id(config.prefix_id)} class="{hlist_class(prefix_class)}">
       {build_initial_content(config, init_items)}
    </ul> |> WStyler.add(main_style, _)

}}
