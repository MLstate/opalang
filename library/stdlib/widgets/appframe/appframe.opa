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
 * An application skeleton with common configurable features.
 *
 * @category WIDGET
 * @author Guillem Rieu, Hugo Heuzard 2011
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */

import stdlib.widgets.{core,button,sidepanel,loginbox,notification}

type WAppFrame.sidepanel_conf = {
  style: WStyler.styler
  size: int
}

type WAppFrame.config = {
  topbar : option(WAppFrame.sidepanel_conf) /** none means no topbar */
  sidebar : option(WAppFrame.sidepanel_conf) /** none means no sidebar */
  statusbar : option(WAppFrame.sidepanel_conf) /** none means no statusbar */
  notification : WNotification.config /** notification widget configuration */
  loginbox: option(WStyler.styler) /** none means no loginbox */
  searchbox: option(WStyler.styler) /** none means no searcgbox */
}

type WAppFrame.content = {
  topbar    : xhtml /** Top elements */
  main      : xhtml /** Main content */
  statusbar : xhtml /** Status bar content */
  loginbox  : xhtml
}

type WAppFrame.action = {
  on_search : (option(string) -> void)
}


WAppFrame =

  searchbar_config(some : WAppFrame.sidepanel_conf): WSidepanel.config =
    {WSidepanel.default_config_noresize with
      get_handle = WSidepanel.no_handles
      get_size   = _ -> {size={px=some.size}}
      place      = {right}
      position   = {fixed}
      get_zindex = _ -> 25
    }

{{

  /**
   * {1 High-level interface}
   */

  /**
   * A default configuration for [WAppFrame]. It should be used as base to
   * customize the component.
   */
  default_config : WAppFrame.config = {
    topbar = {some={size = 40 style=WStyler.make_style(css {background: #336699;})}}
    sidebar= {some={size = 200 style=WStyler.make_style(css {background: #225588;border-left: 1px solid #114477})}}
    statusbar = {some={size = 30 style=WStyler.make_style(css {background: #336699})}}
    notification = WNotification.default_config
    loginbox= {some=WStyler.make_style(css {display:inline-block;})}
    searchbox= {some=WStyler.make_style(css {display:inline-block;})}
  }

  /** A configuration with all optional features disabled */
  minimal_config : WAppFrame.config = {
    topbar = none
    sidebar = none
    statusbar = none
    notification = WNotification.default_config
    loginbox = none
    searchbox = none
  }

  // full_config /* A configuration with all optional features enabled */
  // mlstate_config /* A base configuration for MLstate applications */


  /**
   * Main [WAppFrame] building function
   */
  html(config : WAppFrame.config, id : string, action : WAppFrame.action, content : WAppFrame.content) : xhtml =
    /* Search box */
    /* Callback on a search event */
    on_change_search() : void =
      search_value = Dom.get_value(#{get_searchfield_id(id)})
      if search_value == "" then
        action.on_search(none)
      else
        action.on_search(some(search_value))
    searchbox_xhtml =
      match config.searchbox with
      | {~some} ->
        <div>
          <input type="text" id="{get_searchfield_id(id)}" />
          <a onclick={_ -> on_change_search()}>Search</a>
        </div> |> WStyler.set(some,_)
      | {none} -> <></>

    sidebar_xhtml =
      match config.sidebar with
        | {~some} ->
          /**
           * A [WSidePanel] configuration for the search bar
           */
          searchbar_config: WSidepanel.config = searchbar_config(some)
          econtent =
            <div style="height:100%;width:{some.size}px">
              <a onclick={_ -> WSidepanel.do_close(searchbar_config, get_left_sidepanel_id(id), true)} > close </a>
              <div id="{get_searchres_content_id(id)}">
              </div>
            </div>
            |> WStyler.add(some.style,_)
          WSidepanel.html(searchbar_config, get_left_sidepanel_id(id),econtent,false)
        | {none} -> <></>


    loginbox_xhtml =
      match config.loginbox with
         | {~some} -> <div>{content.loginbox}</div> |> WStyler.add(some,_)
         | {none} -> <></>

    /* Top bar content */
    (topbar_xhtml,topbar_size) =
      match config.topbar with
        | {~some} ->
           /**
            * A [WSidePanel] configuration for the top bar
            */
            topbar_config: WSidepanel.config =
              {WSidepanel.default_config_noresize with
                get_handle = WSidepanel.no_handles
                get_size   = _ -> {size={px=some.size }}
                place      = {top}
                position   = {fixed}
              }
            econtent =
              <div  style="width:100%;height:100%">
                <div style="float:right;height:100%">
                  {searchbox_xhtml}
                </div>
                <div style="float:right;height:100%">
                  {loginbox_xhtml}
                </div>
                <div id="{get_header_id(id)}">
                  {content.topbar}
                </div>
              </div>
              |> WStyler.add(some.style,_)
            (WSidepanel.html(topbar_config, get_top_sidepanel_id(id), econtent, true),
            some.size)
         | {none} -> (<></>,0)
    /* Status bar content */
    (statusbar_xhtml,statusbar_size) =
      match config.statusbar with
         | {~some} ->
             /**
              * A [WSidePanel] configuration for the status bar
              */
             statusbar_config: WSidepanel.config =
               {WSidepanel.default_config_noresize with
                 get_handle = WSidepanel.no_handles
                 get_size   = _ -> {size={px=some.size}}
                 //get_size   = _ -> {normal}
                 place      = {bottom}
                 position   = {fixed}
               }
             econtent = <div id="{get_bottom_sidepanel_id(id)}" style="width:100%;height:100%">
                            {content.statusbar}
                       </div>
                       |> WStyler.add(some.style,_)
             (WSidepanel.html(statusbar_config, "{id}_statusbar",econtent,true),
             some.size)
         | {none} -> (<></>,0)

    /** [WAppFrame] xhtml building */
    notif = WNotification.html(WNotification.default_config,get_notification_id(id))
    appframe_xhtml =
      <div id="{get_main_id(id)}">
        {topbar_xhtml}
        <div style={css {margin: {topbar_size}px 0px {statusbar_size}px 0px;padding:0px}}>
          {sidebar_xhtml}
          <div id="{get_content_id(id)}">
            {content.main}
          </div>
        </div>
        {statusbar_xhtml}
        {notif}
      </div>

    appframe_xhtml

 /**
  * Set the sidebar content
  */
  do_set_sidebar_content(id : string, config : WAppFrame.config, xhtml : xhtml) : void =
    match config.sidebar with
      | ~{some} ->
         searchbar_config: WSidepanel.config = searchbar_config(some)
         do Dom.transform([#{get_searchres_content_id(id)} +<- xhtml])
         do WSidepanel.do_open(searchbar_config, get_searchbar_id(id), true)
         void
      | _ -> void

 /**
  * Open the sidebar
  */
  do_open_sidebar(id : string, config) : void =
    match config.sidebar with
      | ~{some} ->
         searchbar_config: WSidepanel.config = searchbar_config(some)
         do WSidepanel.do_open(searchbar_config, get_left_sidepanel_id(id), true)
         void
      | _ -> void

 /**
  * Close the sidebar
  */
  do_close_sidebar(id : string, config) : void =
    match config.sidebar with
      | ~{some} ->
         searchbar_config: WSidepanel.config = searchbar_config(some)
         do WSidepanel.do_close(searchbar_config, get_left_sidepanel_id(id), true)
         void
      | _ -> void

 /**
  * Set the content of the topbar
  */
  do_set_topbar(id : string, xhtml : xhtml ) : void =
    Dom.transform([#{get_header_id(id)} <- xhtml])

 /**
  * Set the content of the status bar
  */
  do_set_status(id : string, xhtml : xhtml ) : void =
    Dom.transform([#{get_bottom_sidepanel_id(id)} <- xhtml])

 /**
  * Set the main content
  */
  do_set_content(id : string, xhtml : xhtml) : void =
    Dom.transform([#{get_content_id(id)} <- xhtml])

 /**
  * Display a notification box
  */
  do_notify(id : string, config : WAppFrame.config, notification : WNotification.box) =
     WNotification.notify(config.notification, WNotification.box_id(get_notification_id(id)), notification)

  /**
   * {1 Private functions aimed at internal use}
   *
   * Do not use them outside of the module.
   */
  @private get_notification_id(id) = id ^ "_notification"
  @private get_main_id(id)         = id ^ "_main"
  @private get_content_id(id)      = id ^ "_content"

  @private get_searchbar_id(id)    = id ^ "_searchbar"
  @private get_searchres_id(id)    = id ^ "_searchres"

  @private get_header_id(id)       = id ^ "_header"
  @private get_searchres_content_id(id)    = id ^ "_searchrescontent"
  @private get_searchfield_id(id)  = id ^ "_search_field"



  @private get_top_sidepanel_id(id)   = id ^ "_side_topbar"
  @private get_bottom_sidepanel_id(id)= id ^ "_side_statusbar"
  @private get_left_sidepanel_id(id)  = id ^ "_side_left"
}}



