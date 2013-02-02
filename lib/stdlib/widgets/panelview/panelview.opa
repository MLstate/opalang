/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * A layout manager for applications organized in multiple panels.
 *
 * @author Guillem Rieu, 2010
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */

/**
 * {1 About this module}
 *
 * This widget is a thin layer over [WSidepanel], adding automated management 
 * of multiple panels at the same time. A set of multiple panels in a given 
 * state (opened, closed...) forms a 'layout'. Layouts can in turn be grouped 
 * in 'views', which aim at being consistent organisations of panels for a 
 * given usage (e.g. for an initial display using three columns, we could 
 * define alternative views to display only two columns at a time, or even a 
 * single one -- think PDAs, iPhone...).
 *
 * Once these views are defined, the widget handles resizing of the different 
 * panels in order to keep their display consistent.
 *
 * {1 Where should I start?}
 *
 * The first step to use [WPanelview] is to define the inner panels and the main 
 * content. If you wish to change them later, or can't initialize them right 
 * away, you can very well set empty containers with a known ID, and update 
 * them later. For each panel you need, create an associated record of type 
 * {!WPanelview.panel}.
 *
 * Once the panels are defined, you must think to the different ways you will 
 * display them (three at a time? two? a single one?). Each of these ways, the 
 * 'views', will be associated to multiple layouts (e.g. in the case of a view 
 * displaying two panels at a time, but with three panels initially defined, at 
 * least two layouts will be needed: the first panel and the second one 
 * visible, or the first panel and the content; you could as well add the 
 * second panel and the content to cover up all combinations). You will later 
 * be able to handle these layouts yourself using {!WPanelview.set_layout} or 
 * let the widget switch between them depending on the current view.
 *
 * To get the widget to do all the layout switching work, you can wrap views 
 * and their associated layouts using the function 
 * {!WPanelview.wrap_set_layout}.
 *
 * Finally, to retrieve the HTML corresponding to the widget, use the function 
 * {!WPanelview.html} with the previously defined values.
 */

import stdlib.widgets.core
import stdlib.widgets.sidepanel

/**
 * A type representing a single panel of the [WPanelview]. See the module 
 * {!WSidepanel} for more information on individual types.
 */
type WPanelview.panel = {
  /** The configuration of a {!WSidepanel} */
  config: WSidepanel.config
  /** The HTML id of the panel */
  id: string
  /** The content of the panel */
  content: xhtml
}

/**
 * A set of panel states defining a distinct layout. The name identifies the 
 * layout; it should be unique. The list of active panels is necessary to check 
 * if the layout should be changed when 
 */
type WPanelview.layout = {
  /** Name of the layout */
  name: string
  /** IDs of active panels in this layout */
  panels: list(string)
  /** The function giving the state of the different panels */
  get_display: WSidepanel.config, string -> WSidepanel.display
}

type WPanelview.config = {
  /** The lowest z-index to start from when creating the panels */
  init_zindex: int
  /** Get the zindex for the next panel */
  next_zindex: int -> int
  /** Style of the main DIV container */
  style: WStyler.styler
}

WPanelview =
  get_body_id(id: string): string   = "{id}_body"
  get_view_id(id: string): string = "{id}_layout"
  get_layout_id(id: string): string   = "{id}_view"

  get_body_info(id: string): WSidepanel.content = {
    id              = get_body_id(id)
    size_opt        = _ -> none
    margin_size_opt = _ -> none
    parent_size_opt = _ -> none
  }

{{

  /**
   * {1 High-level interface}
   */

  /**
   * A default {!WSidepanel.config} better suited to [WPanelview] than the one 
   * provided in the original {!WSidepanel} module.
   *
   * @param id The HTML id of the {!WSidepanel}
   */
  default_panel_config(id: string): WSidepanel.config =
    {WSidepanel.default_config(some(get_body_info(id))) with
      get_handle = WSidepanel.no_handles
      opening_delay = 0
    }

  /**
   * A default {!WPanelview.config} with reasonable values. The z-index of the 
   * different panels starts at 70 (for the most 'inner' panel) and is 
   * incremented by one for each panel. No style is applied to the widget.
   */
  default_config: WPanelview.config = {
    init_zindex = 70
    next_zindex = i -> i + 1
    style = WStyler.empty
  }

  /**
   * Default function to create a [WPanelview]
   *
   * @param config The config of the [WPanelview]
   * @param id The HTML id of the [WPanelview]
   * @param panels A list of {!WPanelview.panel} composing the widget
   * @param init_view The initial view identifier
   * @param init_layout The initial layout to display
   * @param init_body The initial main content of the [WPanelview]
   * @return The HTML corresponding to the widget
   */
  html(config: WPanelview.config, id: string, panels: list(WPanelview.panel),
      init_view: string, init_layout: WPanelview.layout, init_body: xhtml)
      : xhtml =
    /* Auxiliary function creating a single panel and its associated DIV */
    show_aux((zindex: int, acc: xhtml), panel: WPanelview.panel): (int, xhtml) =
      body_id = get_body_id(panel.id)
      panel_config = {panel.config with
        get_zindex = _ -> zindex

      on_change_state =
        WSidepanel.change_content_size(get_body_info(panel.id), _, _, _)
      }
      (config.next_zindex(zindex),
      <>
        {WSidepanel.html(panel_config, panel.id, panel.content, false)}
        <div id={body_id}
            style={css {width: 100%; height: 100%;
              position: absolute; top: 0;}}>
          {acc}
        </div>
      </>)
    /* Build the panels */
    <div id={id}
        style={css {width: 100%; height: 100%; padding: 0; margin: 0;}}
        onready={_ ->
            List.map((panel -> (panel.config, panel.id)), panels)
              |> set_layout(id, _, init_layout)
        }>
      {(List.fold_right(show_aux, panels,
          (config.init_zindex, init_body))).f2}
      <input id="{get_view_id(id)}" type="hidden" value="{init_view}" />
      <input id="{get_layout_id(id)}" type="hidden" value="undefined" />
    </div>
      |> WStyler.add(config.style, _)

  /**
   *  {2 Predefined layouts}
   */

  /**
   * Layout constructor
   *
   * @param name The name of the layout
   * @param panels The list of panel HTML ids composing the layout
   * @return The resulting layout
   */
  mk_layout(name: string, panels: list(string),
      get_display: WSidepanel.config, string -> WSidepanel.display)
      : WPanelview.layout =
    ~{name panels get_display}

  /**
   * Fullpage layout: set the given panel to full size, and hide others.
   *
   * @param name The name of the layout
   * @param active_panel The active panel in this layout
   * @return The layout corresponding to a fullpage [active_panel]
   */
  fullpage_panel(name: string, active_panel: string): WPanelview.layout =
    get_display(_: WSidepanel.config, id: string): WSidepanel.display =
      if id == active_panel then
        {fullpage}
      else
        {closed}
    mk_layout(name, [active_panel], get_display)

  /**
   * Fixed size layout: let a list of panels be opened with given sizes.
   *
   * @param name The name of the layout
   * @param panels A list of pairs (panel_id, size) defining opened panels
   * @return The layout corresponding to the given list of panels
   */
  panels_with_size(name: string, panels: list((string, Css.size_or_normal)))
      : WPanelview.layout =
    get_display(_: WSidepanel.config, id: string): WSidepanel.display =
      Option.switch(size -> {opened_with_size=size}, {closed},
          List.assoc(id, panels))
    mk_layout(name, List.map((p, _) -> p, panels), get_display)

  /**
   * Close all panels (the content takes all the page)
   *
   * @param name The name of the layout
   * @param active_panels A list of 'active' panels in this layout
   * @return A layout with all panels closed
   */
  fullpage_content(name: string, active_panels: list(string))
      : WPanelview.layout =
    mk_layout(name, active_panels, (_, _ -> {closed}))

  /**
   * Open all panels to their default sizes
   *
   * @param name The name of the layout
   * @param active_panels A list of 'active' panels in this layout
   * @return A layout with all panels opened
   */
  fixed_layout(name: string, active_panels: list(string))
      : WPanelview.layout =
    mk_layout(name, active_panels, (_, _ -> {opened}))

  /**
   *   {2 Imperative interface}
   */

  /**
   *    {3 Views handling}
   */

  /**
   * Change the currently selected view
   *
   * @param id The HTML id of the [WPanelview]
   */
  set_view(id: string, layout: string): void =
    _ = Dom.set_value(#{get_view_id(id)}, layout)
    void

  /**
   *    {3 Layouts handling}
   */

  /**
   * Modify the current display of the panels with the given layout
   *
   * @param id The HTML id of the [WPanelview]
   * @param panels A list of pairs (sidepanel_config, sidepanel_id)
   * @param layout The layout to set
   */
  // TODO : switching to fullpage content
  set_layout(id: string, panels: list((WSidepanel.config, string)),
      layout: WPanelview.layout)
      : void =
    _ = Dom.set_value(#{get_layout_id(id)}, layout.name)
    set_layout_aux((panel_config: WSidepanel.config, panel_id: string)) =
      layout.get_display(panel_config, panel_id)
        |> WSidepanel.set_state(panel_config, panel_id, _, false)
    List.iter(set_layout_aux, panels)

  /**
   * Wrap multiple views and automatically switch between them
   *
   * @param id The HTML id of the [WPanelview]
   * @param layouts A list of pairs (
   */
  wrap_set_layout(id: string, panels: list((WSidepanel.config, string)),
      views: list((string, list(WPanelview.layout))))
      : string -> void = panel_id ->
    current_layout = Dom.get_value(#{get_layout_id(id)})
    current_view   = Dom.get_value(#{get_view_id(id)})
    match List.assoc(current_view, views) with
    | {none}  -> void
    | {~some} ->
      match List.find((v -> List.mem(panel_id, v.panels)), some) with
      | {none}  -> void
      | {~some} ->
        if not(current_layout == some.name) then
          set_layout(id, panels, some)

}}
