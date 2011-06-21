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
 * A configurable tabs widget.
 *
 * @author Frederic Ye, 2010
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.7
 */

/*
 * {1 About this module}
 *
 * WTabs widget
 * Using WHList widget
 * WTabs are closable and duplicatable compared to basic hlists
 *
 * {1 Where should I start?}
 *
 * If you simply want a tabs widgets, use [WTabs.html]
 *
 * {1 Recommandations}
 *
 * Once a tab is added, you can add a custom_data to recognize the added tab
 * This custom data is a string for the moment
 *
 * {1 What if I need more?}
 *
 * {1 TODO (ideas) }
 *
 * - Handle recursive tabs list (list(list(...(tab)))) for hierarchical tabs initilialization
 *   @see Opera 11 / Firefox Tab Kit
 * - Choose vertical / horizontal styler
 * - Drag&Drop
 * - Content handling (components/tabs_ext.opa ?)
 *
 * {1 Developers Notes}
 *
 * v0.5 was richer and not using hlist, so if you want to take a look, you can find it in GIT
 * stop_propagation is usefull because the tab will be selected otherwise...
 */

import stdlib.web.client
import stdlib.widgets.hlist
import stdlib.widgets.core

DEBUG = false // sould be replaced by something usable at runtime for debugging...

type WTabs.pos = WHList.pos

/**
 * A tab content
 *
 * [title] the title to use for the link (mouseover usually)
 * [value] the content value
 */
type WTabs.content = WHList.item_content

/**
 * A tab state
 */
type WTabs.state = WHList.item_state

/**
 * A tab properties
 */
type WTabs.properties = {
  base_properties: WHList.item_properties
  closable: bool
  duplicatable: bool
}

/**
 * A tab
 */
type WTabs.tab = {
  content: int -> WTabs.content
  state: WTabs.state
  properties: WTabs.properties
  custom_data: option(string)
  remove_text: option((string, xhtml))
  duplicate_text: option((string, xhtml))
  styler: WStyler.styler
}

type WTabs.stylers = {
  tabs: WStyler.styler
  tab: WStyler.styler
  tab_content: WStyler.styler
  tab_sons: WStyler.styler
  tab_selectable: WStyler.styler
  tab_checkable: WStyler.styler
  tab_no_sons: WStyler.styler
  tab_closable: WStyler.styler
  tab_duplicatable: WStyler.styler
  tab_close: WStyler.styler
  tab_duplicate: WStyler.styler
  tab_add: WStyler.styler
}

/**
* The widget configuration
*/
type WTabs.config = {
  prefix_class: option(string);
  on_select: int, WTabs.tab -> bool // tab clicked
  on_add: int, WTabs.tab -> option(WTabs.tab) // tab added
  on_remove: int, WTabs.tab -> bool // tab removed
  on_duplicate: int, WTabs.tab, int, WTabs.tab -> option(WTabs.tab) // tab duplicated
  add_text: option((string, xhtml)) // if none, does not display the button
  remove_text: option((string, xhtml)) // if none, does not display the button
  duplicate_text: option((string, xhtml)) // if none, does not display the button
  new_tab_content: int -> WTabs.content // content to use for a new tab
  select_new_tab: bool // select the new tab at creation
  insert_pos: WTabs.pos // tab insert position
  delete_pos: WTabs.pos // tab to select on delete
  duplicate_pos: WTabs.pos // tab duplicate position
  new_tab_closable: bool // is new tab closable by default
  new_tab_duplicatable: bool // is new tab duplicatable by default
  max_content_chars: option(int) // max chars in a content (dynamic)
  max_tabs_number: option(int) // max number of tabs (in one level)
  stylers: WTabs.stylers
}

WTabs = {{

  /*
  * {1 ID shortcuts}
  */

  gen_id(prefix_id:string, suffix:string) =
    "{prefix_id}_tabs{suffix}"

  tabs_id(prefix_id:string) =
    gen_id(prefix_id, "")

  tabs_list_id(prefix_id:string) =
    gen_id(prefix_id, "_list")

  counter_id(prefix_id:string) =
    gen_id(prefix_id, "_counter")

  number_id(prefix_id:string) =
    gen_id(prefix_id, "_number")

  selected_id(prefix_id:string) =
    gen_id(prefix_id, "_selected")

  tab_id(prefix_id, num) =
    gen_id(prefix_id, "_tab_{num}")

  edit_id(prefix_id, num) =
    gen_id(prefix_id, "_edit_{num}")

  /*
  * {1 Class shortcuts}
  */

  gen_class(prefix_class:string, suffix:string) =
    "{prefix_class}_tabs{suffix}"

//   tab_closable_class(prefix_id:string) =
//     gen_class(prefix_id, "_tab_closable")

//   tab_duplicatable_class(prefix_id:string) =
//     gen_class(prefix_id, "_tab_duplicatable")

//   tab_custom_class_data(prefix_id:string) =
//     gen_class(prefix_id, "_tab_custom_data")

   tab_close_class(prefix_class:string) =
     gen_class(prefix_class, "_tab_close")

   tab_duplicate_class(prefix_class:string) =
     gen_class(prefix_class, "_tab_duplicate")

  /*
  * {1 Config}
  */

  to_hlist_config(config:WTabs.config) = {
    WHList.default_config with
      prefix_class = config.prefix_class
      helper = {
        stringify = (num, _tab) -> "{num}"
        father = _ -> none
      }
      on_add = key, item ->
        match key with
        | ~{some=(num, tab)} ->
          match config.on_add(num, tab) with
          | ~{some} -> Option.some(((num, some), item))
          | {none} -> Option.none
          end
        | {none} -> Option.none
      on_select = (num, tab) -> config.on_select(num, tab)
      on_remove = (num, tab) -> config.on_remove(num, tab)
      insert_pos = config.insert_pos
      delete_pos = config.delete_pos
      stylers = {
        hlist = config.stylers.tabs
        ul = WStyler.make_style(css {list-style:{{style=none style_position=none}};})
        item = config.stylers.tab
        item_content = config.stylers.tab_content
        item_sons = config.stylers.tab_sons
        item_selectable = config.stylers.tab_selectable
        item_checkable = config.stylers.tab_checkable
        item_no_sons = config.stylers.tab_no_sons
      }
    }

  print_pos(pos:option(int)) =
    if DEBUG then
      match pos with
      | ~{some} -> jlog("{some}")
      | {none} -> void
    else void

  /*
  * {1 Constructors}
  */

  // Content //

  make_constant_content(s:string) = { title=s value=Xhtml.of_string(s) }

  // Tab //

  make_tab(content:(int->WTabs.content),
           selected:bool, closable:bool, duplicatable:bool,
           custom_data:option(string),
           remove_text:option((string, xhtml)), duplicate_text:option((string, xhtml))) = {
    content = content
    state = {
      selected = selected
      checked = false
    }
    properties = {
      base_properties = {
        selectable = true
        checkable = false
        no_sons = true
      } : WHList.item_properties
      closable = closable
      duplicatable = duplicatable
    }
    custom_data = custom_data
    remove_text = remove_text
    duplicate_text = duplicate_text
    styler = WStyler.empty
  } : WTabs.tab

  no_tab() = make_tab((_->make_constant_content("")), false, false, false, none, none, none)

  // Config

  /* Lambda-lifted default actions */
  default_action = _num, _tab -> true
  default_action_option = _num, tab -> some(tab)
  default_action_duplicate = _num, _tab, _new_num, new_tab -> some(new_tab)
  default_action_new_tab = _ -> {title="Tab" value=Xhtml.of_string("Tab")}

  default_config = {
    prefix_class = none
    on_select = default_action
    on_add = default_action_option
    on_remove = default_action
    on_duplicate = default_action_duplicate
    add_text = some(("Add", Xhtml.of_string("Add")))
    remove_text = some(("Remove", Xhtml.of_string("X")))
    duplicate_text = some(("Duplicate", Xhtml.of_string("D")))
    new_tab_content = default_action_new_tab
    select_new_tab = true
    insert_pos = {at_end}
    delete_pos = {after_select}
    duplicate_pos = {after_select}
    new_tab_closable = true
    new_tab_duplicatable = true
    max_content_chars = none
    max_tabs_number = none
    stylers = {
      tabs = WStyler.empty
      tab = WStyler.empty
      tab_content = WStyler.empty
      tab_sons = WStyler.empty
      tab_selectable = WStyler.empty
      tab_checkable = WStyler.empty
      tab_no_sons = WStyler.empty
      tab_closable = WStyler.empty
      tab_duplicatable = WStyler.empty
      tab_close = WStyler.empty
      tab_duplicate = WStyler.empty
      tab_add = WStyler.empty
    }
  } : WTabs.config

  default_config_with_css(css_prefix:string) = { default_config with
    prefix_class = some(css_prefix)
  }

  /*
  * {1 XHTML reading / writing}
  */

  // Global tabs infos // @private //

  extract_counter(prefix_id:string) =
    match Parser.int(Dom.get_text(#{counter_id(prefix_id)})) with
    | ~{some} -> some
    | {none} -> 0

  set_counter(prefix_id:string, nb:int) =
    Dom.transform([#{counter_id(prefix_id)} <- nb])

  extract_number(prefix_id:string) =
    match Parser.int(Dom.get_text(#{number_id(prefix_id)})) with
    | ~{some} -> some
    | {none} -> 0

  set_number(prefix_id:string, nb:int) =
    Dom.transform([#{number_id(prefix_id)} <- nb])

  // Tab // @private //

  extract_tab_num(s:string) =
    list = String.explode("_", s)
    if List.length(list) > 0 then
      Parser.int(List.max(list))
    else none

//   tab_closable_class_option(prefix_id, tab) =
//     if tab.properties.closable then some(tab_closable_class(prefix_id))
//     else none

//   is_closable(prefix_id, id) =
//     Dom.has_class(#{id}, tab_class_closable(prefix_id))

//   tab_class_duplicatable_option(prefix_id, tab) =
//     if tab.properties.duplicatable then some(tab_class_duplicatable(prefix_id))
//     else none

//   is_duplicatable(prefix_id, id) =
//     Dom.has_class(#{id}, tab_class_duplicatable(prefix_id))

//   extract_custom_data(prefix_id, num) =
//     id = tab_id(prefix_id, num)
//     jq = #{"{id} > ul > li.{tab_class_custom_data(prefix_id)}:first"}
//     if Dom.length(jq) > 0 then
//       some(Dom.get_text(jq))
//     else none

  /*
  * {1 Actions}
  */

  /**
  * Create the "content" of a tab, that means the content and close button
  *
  * @param config the widget configuration
  * @param prefix_id the prefix id to use
  * @param tab the tab to use for content creation
  * @return The XHTML corresponding to a tab content
  */
  @private
  create_tab_content(config:WTabs.config, prefix_id:string, num:int, tab:WTabs.tab) =
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    ocr = _event -> if tab.properties.closable then remove_tab(config, prefix_id, num, tab) else void
    ocd = _event -> if tab.properties.duplicatable then duplicate_tab(config, prefix_id, num, tab) else void
    <>
      <span>{tab.content(num).value}</span>
      {if tab.properties.duplicatable then
        match tab.duplicate_text with
        | ~{some=(title, duplicate)} ->
          <a class={[tab_duplicate_class(prefix_class)]} title={title} onclick={ocd} options:onclick="stop_propagation">{duplicate}</a>
          |> WStyler.add(config.stylers.tab_duplicate, _)
        | {none} -> <></>
      else <></>}
      {if tab.properties.closable then
        match tab.remove_text with
        | ~{some=(title, remove)} ->
          <a class={[tab_close_class(prefix_class)]} title={title} onclick={ocr} options:onclick="stop_propagation">{remove}</a>
          |> WStyler.add(config.stylers.tab_close, _)
        | {none} -> <></>
      else <></>}
    </>
//       {if Option.is_some(tab.custom_data) then
//          <li style="display:none" class={[tab_class_custom_data(prefix_id)]}>{Option.get(tab.custom_data)}</li>
//        else <></>}

  /**
  * Select a tab
  * if tab.num = 0 then unselect only
  *
  * @param config the widget configuration
  * @param prefix_id the prefix id to use
  * @param tab the tab to select
  */
  select_tab(config:WTabs.config, prefix_id:string, num:int, tab:WTabs.tab, notify:bool) =
    hlist_config = to_hlist_config(config)
    _ = WHList.select_item(hlist_config, prefix_id, (num, tab), notify)
    void

  /**
  * Insert a tab at a meaningful position
  * It will use default values if content or position are given
  *
  * @param config the widget configuration
  * @param prefix_id the prefix id to use
  * @param num the tab numerical identifier
  * @param tab the tab to insert
  * @param sel_key consider this optional (num, tab) as selected
  * @param notify
  */
  @private
  internal_insert_tab(config:WTabs.config, prefix_id:string, num:int, tab:WTabs.tab, pos:WTabs.pos, _sel_key:option((int, WTabs.tab)), notify:bool) =
    tab_content = create_tab_content(config, prefix_id, num, tab)
    hlist_config = { to_hlist_config(config) with insert_pos = pos }
    item = WHList.make_item({tab.content(num) with value=tab_content},
             {selected=tab.state.selected; checked=tab.state.checked},
             {selectable=tab.properties.base_properties.selectable;
             no_sons=tab.properties.base_properties.no_sons;
             checkable=tab.properties.base_properties.checkable})
    _ = WHList.insert_item(hlist_config, prefix_id, (num, tab), item, none, notify)
    // do set_number(prefix_id, extract_number(prefix_id) + 1)
    do set_counter(prefix_id, extract_counter(prefix_id) + 1)
    void

  /**
  * Insert a tab with the behaviour defined in the configuration
  * Will compute the id
  *
  * @param config the widget configuration
  * @param prefix_id the prefix id to use
  * @param tab the tab to insert
  */
  insert_tab(config:WTabs.config, prefix_id:string, tab:WTabs.tab) =
    id = extract_counter(prefix_id) + 1
    internal_insert_tab(config, prefix_id, id, tab, config.insert_pos, none, true)

  /**
  * Add a tab with the behaviour defined in the configuration
  *
  * @param config the widget configuration
  * @param prefix_id the prefix id to use
  */
  add_tab(config:WTabs.config, prefix_id:string) =
    tab = make_tab(config.new_tab_content, config.select_new_tab, config.new_tab_closable, config.new_tab_duplicatable, none, config.remove_text, config.duplicate_text)
    insert_tab(config, prefix_id, tab)

  /**
  * Duplicate a tab with the behaviour defined in the configuration
  *
  * @param config the widget configuration
  * @param prefix_id the prefix id to use
  * @param num the tab numerical identifier
  * @param tab the tab to duplicate
  */
  duplicate_tab(config:WTabs.config, prefix_id:string, num:int, tab:WTabs.tab) =
    id = extract_counter(prefix_id) + 1
    new_tab = make_tab(tab.content, config.select_new_tab, tab.properties.closable, tab.properties.duplicatable, none, tab.remove_text, tab.duplicate_text)
    match config.on_duplicate(num, tab, id, new_tab) with
    | ~{some} -> internal_insert_tab(config, prefix_id, id, some, config.duplicate_pos, none, true)
    | {none} -> void

  /**
  * Replace a tab content with a new one with possible new properties (stays at the same position)
  *
  * @param config the widget configuration
  * @param prefix_id the prefix id to use
  * @param num the tab numerical identifier
  * @param tab the tab to use for replacement
  */
  replace_tab_content(config:WTabs.config, prefix_id:string, num:int, tab:WTabs.tab) =
    tab_content = create_tab_content(config, prefix_id, num, tab)
    hlist_config = to_hlist_config(config)
    item = WHList.make_item({tab.content(num) with value=tab_content},
             {selected=tab.state.selected; checked=tab.state.checked},
             {selectable=tab.properties.base_properties.selectable;
             no_sons=tab.properties.base_properties.no_sons;
             checkable=tab.properties.base_properties.checkable})
    _ = WHList.replace_item_content(hlist_config, prefix_id, (num, tab), item)
    void

  /**
  * Remove a tab
  * {at_select} or {after_select} do not change the selected tab
  * otherwise, we simulate a selection change
  *
  * @param config the widget configuration
  * @param prefix_id the prefix id to use
  * @param num the tab numerical identifier
  * @param tab the tab to remove
  */
  remove_tab(config:WTabs.config, prefix_id:string, num:int, tab:WTabs.tab) =
    hlist_config = to_hlist_config(config)
    _ = WHList.remove_item(hlist_config, prefix_id, (num, tab), true)
    // do set_number(prefix_id, extract_number(prefix_id) - 1)
    void

  /**
  * The main function to be called for creating a tabs widget, with some default tabs
  *
  * @param config the widget configuration
  * @param prefix_id the prefix id to use
  * @param init_tabs the list of tabs to show at initilialization
  * @return The XHTML corresponding to the widget
  */
  html(config:WTabs.config, prefix_id:string, init_tabs:list((int, WTabs.tab))) =
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    hlist_config = to_hlist_config(config)
    (init_items, max_id) = List.fold_backwards((num, tab), (acc, max_id) ->
      (((num, tab), WHList.make_item(tab.content(num),
                  {selected=tab.state.selected; checked=tab.state.checked},
                  {selectable=tab.properties.base_properties.selectable;
                  no_sons=tab.properties.base_properties.no_sons;
                  checkable=tab.properties.base_properties.checkable})) +> acc,
      max(num, max_id))
    , init_tabs, ([], List.length(init_tabs)))
    core_html = WHList.html(hlist_config, prefix_id, init_items)
    style = if DEBUG then "" else "display:none"
    <>
      {core_html}
      {<div class={[gen_class(prefix_class, "_add")]} id=#{gen_id(prefix_id, "_add")}>
        {match config.add_text with
        | ~{some=(title, add)} ->
          <a title={title} onclick={_event -> add_tab(config, prefix_id)}>{add}</a>
        | {none} -> <></>}
      </div> |> WStyler.add(config.stylers.tab_add, _)}
      <div style="{style}" id=#{gen_id(prefix_id, "_status")}>
        {if DEBUG then <span>WTabs counter: </span> else <></>}
        <span id=#{counter_id(prefix_id)}>{max_id}</span>
      </div>
      <div style="clear: both"></div>
    </>
//         <!--<span>Number of tabs: </span>-->
//         <!--<span id=#{number_id(prefix_id)}>{List.length(init_tabs)}</span>-->
//         <!--<span>Selected tab: </span>-->
//         <!--<span id=#{selected_id(prefix_id)}>0</span>-->

}}

// private_default_css =

// div._hlist {
//   padding: 5px;
//   padding-bottom: 0;
//   display: block;
// }
// div._hlist a {
//   text-decoration: none;
//   color: black;
// }
// div._hlist a:link {
//   text-decoration: none;
//   color: black;
// }
// div._hlist a:visited {
//   text-decoration: none;
//   color: black;
// }
// div._hlist a:hover {
//   text-decoration: none;
//   color: black;
// }
// div._hlist a:active {
//   text-decoration: none;
//   color: black;
// }
// div._hlist ul {
//   list-style: none;
//   padding: 0;
//   margin: 0;
// }
// div._hlist li._hlist_item {
//   float: left;
//   margin: 0 1px;
// }
// div._hlist li._hlist_item > ul > li._hlist_item_content {
//   border: 1px solid #aaaaaa;
//   border-bottom-width: 0;
//   padding: 0 10px;
//   background: #eeeeee;
// }
// div._hlist li._hlist_item > ul > li._hlist_item_content > a:hover > span { text-decoration: underline; }
// div._hlist a._tabs_tab_duplicate { color: green; }
// div._hlist a._tabs_tab_close { color: red; }
// div._hlist ul > li._hlist_item_selected {
//   font-weight: bold;
//   border: 1px solid #000000;
//   border-bottom: 0;
// }
// div._hlist ul > li._hlist_item_selected > ul > li._hlist_item_content {
//   background: #ffffff;
//   position: relative;
//   top: 1px;
//   border: 0;
// }
// div._tabs_add {
//   float: right;
//   margin-right: 5px;
// }
