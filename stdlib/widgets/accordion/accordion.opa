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
 * A configurable accordion widget.
 *
 * @author Jessica Castejon, 2011
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.1
 */

package stdlib.widgets.accordion

import stdlib.widgets.core

/*
 * {1 About this module}
 *
 * Accordion widget
 * Inspired by http://jqueryui.com/themeroller/
 *
 */


/**
 *The configuration of a widget.
 */
type WAccordion.config = {
  close_others : bool /** Defines if at the opening of one tab the other open tabs should be closed */
  global_style : WStyler.styler /** Global CSS style of the widget */
  tab_open_style : WStyler.styler /** CSS style of the tab when it is open*/
  tab_close_style : WStyler.styler /** CSS style of the tab when it is closed */
  open_style : WStyler.styler /** CSS style of the content of a tab when it is open*/
  close_style : WStyler.styler /** CSS style of the content of a tab when it is closed */
  tab_content_style : WStyler.styler /** CSS style of the content of a tab */
}
@private
type WAccordion.private.mode = {open} / {close}


WAccordion = {{
  @private
  internal_opa_close = "internal_opa_close"

  @private
  internal_opa_open = "internal_opa_open"

/**
 *Default configuration of a widget
 */
  default_config : WAccordion.config = {
      close_others = true
      global_style = WStyler.make_style(css {
                                            font-family: Verdana;
                                            font-size: 12px;
                                            color: #696969;
                                            border: 1px solid #000000;})
      tab_open_style = WStyler.empty
      tab_close_style = WStyler.empty
      open_style = WStyler.empty
      close_style = WStyler.empty
      tab_content_style = WStyler.empty
  }

/**
 *Main display function of the accordion.
 *
 *@param config The widget configuration
 *@param id The widget identifier
 *@param tab_content The content of the tabs of the widget. This list contains
 *the identifier of a tab container, the title of the tab and the content of
 *the tab.
 *@return The HTML corresponding to the widget
 */
  html(config: WAccordion.config, id: string, tab_content: list((string, string, string))): xhtml =
      widget = <div id=#{id}>{get_tabs(config,tab_content,id)}</div>
      widget = WStyler.add(config.global_style,widget)
      <>{widget}</>

/**
 *Function that opens a closed tab
 *
 *@param id_widget The widget identifier
 *@param id The tab container identifier
 *@param config The widget configuration
 */
  do_open(id_widget: string, id: string, config: WAccordion.config): void =
    dom_container = Dom.select_id("{id}")
    dom_content = Dom.select_id("{id}_content")
    is_close = Dom.has_class(dom_container,internal_opa_close)
    if is_close
    then (
         do close_all(config, id_widget)
         apply_change(config, dom_content, dom_container, id, {open})
    )
/*
 *Function that generates the HTML corresponding to a tabs list
 *
 *@param config The widget configuration
 *@param tab_content The content of the tabs of the widget. This list contains
 *the identifier of a tab container, the title of the tab and the content of
 *the tab.
 *@param id The widget identifier
 *@return A list containing the HTML of each tab
 */
  @private
  get_tabs(config: WAccordion.config, tab_content: list((string,string,string)), id: string): list(xhtml) =
    add_tab(i,(num,title,content))=
      (class,style,tab_style) = if i==0
                       then (internal_opa_open,WStyler.merge([config.tab_content_style,config.open_style,WStyler.make_style([Css_build.display_block])]),config.tab_open_style)
                       else (internal_opa_close,WStyler.merge([config.tab_content_style,config.close_style,WStyler.make_style([Css_build.display_none])]),config.tab_close_style)
      block_content = <div id="{num}_content">{content}</div>
      block_content = WStyler.add(style,block_content)
      block_title = <div id="{num}_title" onclick={_ -> clicked_title(id,num,config)}>{title}</div>
      block_title = WStyler.add(tab_style,block_title)
      <div id=#{num} class="{class}">
        {block_title}
        {block_content}
      </div>
    List.mapi(add_tab,tab_content)

/*
 *Function called when the user clicks on the title of a tab and applies the
 *changes needed.
 *
 *@param id_widget The widget identifier
 *@param id The tab container identifier
 *@param config The widget configuration
 */
  @private
  clicked_title(id_widget: string, id: string, config: WAccordion.config): void =
    dom_container = Dom.select_id("{id}")
    dom_content = Dom.select_id("{id}_content")
    is_open = Dom.has_class(dom_container,internal_opa_open)
    mode =
      if is_open
      then {close}
      else (
         do close_all(config, id_widget)
         {open})
    apply_change(config, dom_content, dom_container, id, mode)

  @private
  close_all(config: WAccordion.config, id_widget: string)=
    if config.close_others
    then (
      dom_open = Dom.select_inside(Dom.select_id("{id_widget}"),Dom.select_class(internal_opa_open))
      Dom.iter(hide_open(config,_), dom_open)
    )
/*
 *Function that closes an open tab
 *
 *@param dom_open The container of the open tab
 */
  @private
  hide_open(config: WAccordion.config, dom_open: dom): void =
    id = Dom.get_id(dom_open)
    dom_content = Dom.select_id("{id}_content")
    apply_change(config, dom_content, dom_open, id, {close})

  @private
  apply_change(config: WAccordion.config, dom_content: dom, dom_container: dom, id: string, action: WAccordion.private.mode): void =
    (remove_class,add_class,style,tab_style,action)=
      if action=={close}
      then (internal_opa_open,internal_opa_close,config.close_style,config.tab_close_style,Dom.transition(_,Dom.Effect.hide()))
      else (internal_opa_close,internal_opa_open,config.open_style,config.tab_open_style,Dom.transition(_,Dom.Effect.slide_in()))
    do Dom.remove_class(dom_container,remove_class)
    do Dom.add_class(dom_container,add_class)
    do WStyler.set_dom(style,"{id}_content")
    do WStyler.set_dom(tab_style,"{id}_title")
    _ = action(dom_content)
    void
}}
