/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * A togglable side-panel
 *
 * @author Guillem Rieu, 2010
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */

/**
 * {1 About this module}
 *
 * This module provides togglable side-panels. Basically, it displays any kind
 * of XHTML in panels attached to a side of the parent element and can be
 * opened or closed on demand (whether by the user or the application).
 *
 * {1 Where should I start?}
 *
 * A side-panel is a XHTML chunk made of one main part, the actual content, and
 * an optional handle aimed at letting the user open or close the panel using
 * the mouse.
 *
 * Therefore, in order to create a panel, you need at least the XHTML part
 * which will be its content. Additionnaly, you need to decide which events
 * will open or close it. The easier (and default behaviour) way to proceed is
 * to provide the user with a “handle”: a bar with the same length as the panel
 * which enables the user to switch between opened and closed states.
 *
 * The most basic default config [WSidepanel.default_config_noresize] can be
 * used to obtain a left side-panel with no resizing of the main content. If
 * such a resizing is needed, you can either write your own dedicated function,
 * or use the one provided (it's called [WSidepanel.change_content_size]). In
 * the latter case, you may as well use directly default configs named
 * [WSidepanel.default_*_config], replacing the '*' with either 'left', 'right',
 * 'top' or 'bottom' depending on which side you want the panel to be. It takes
 * the HTML ID of your main content as sole argument.
 *
 * The function [WSidepanel.html] can then be used to create a side-panel with
 * the config.
 *
 * {1 What if I need more?}
 *
 */

// FIXME: automated resizing and margin setting when opening top panel in
// [change_content_size] doesn't work properly

import stdlib.widgets.core

/**
 * {1 Types defined in this module}
 */

/**
 * Place of a panel relatively to its parent element
 */
type WSidepanel.place =
  { left } / { right } / { top } / { bottom }

/**
 * Different possible panel positioning relatively to the rest of the page
 */
type WSidepanel.position =
    /** Scrolling the side-panel along with the page */
    { absolute }
    /** Scrolling the page doesn't affect the side-panel */
  / { fixed }

/**
 * Type of a main content element (e.g. a DIV side-by-side with a panel.
 * Information in such a record is used to resize the said content when a
 * change of a side-panel size occurs.
 */
type WSidepanel.content = {
  /** Main ID */
  id              : string
  /** Functions yielding sizes given the content ID */
  size_opt        : string -> option(Css.size)
  margin_size_opt : string -> option(Css.unary)
  parent_size_opt : string -> option(Css.size)
}

/**
 * The different display states a panel can be in.
 */
type WSidepanel.display =
  /** The panel is closed */
    { closed }
  /** The panel is opened with the current size */
  / { opened }
  /** The panel is opened with a given (and possibly new) size */
  / { opened_with_size: Css.size_or_normal }
  /** The panel covers the whole page */
  / { fullpage }
  /** Let the user handle the display of the panel with the given ID */
  / { custom: string -> option(Css.size_or_normal) }

/**
 * Possible panel handle types
 */
type WSidepanel.handle =
    /** No handle */
    { none }
    /** Static XHTML handle */
  / { content: xhtml size: Css.size }

/**
 * The type of a panel configuration.
 */
type WSidepanel.config = {
  /**
   * Take an ID and the state of a panel ('true' if opened, 'false' if closed)
   * and return the corresponding handle
   */
  get_handle: WSidepanel.config, string, WSidepanel.display ->
      WSidepanel.handle

  /** Placement of the panel */
  place: WSidepanel.place

  /** Positioning of the panel */
  position: WSidepanel.position

  /** Size of the panel, given its ID */
  get_size: string -> Css.size_or_normal
  /** z-index of the panel, given its ID */
  get_zindex: string -> int

  /** Closing / opening animation duration in ms */
  opening_delay : int

  /** Callback event when the panel is changed to a new state */
  on_change_state :
    WSidepanel.place, Css.size_or_normal, Css.size_or_normal -> void
}

/**
 * {1 Private types}
 *
 * The following types are used internally by the module. They shouldn't be
 * used anywhere else.
 */

/**
 * A more vague type than [WSidepanel.place] giving the orientation of a panel
 * (left and right panels are oriented horizontally, whether top and bottom
 * ones are oriented vertically.
 *
 * @private
 */
type WSidepanel.private.orientation =
  { horizontal } / { vertical }

/**
 * A type describing the different styles needed for a side-panel positioning
 *
 * @private
 */
type WSidepanel.private.place_css = {
  container : WStyler.styler
  content   : WStyler.styler
  handle    : WStyler.styler
}

WSidepanel =

  get_opened_class(id : string) : string         = "{id}_opened"
  get_handle_id(id : string) : string            = "{id}_handle"
  get_handle_char_id(id : string) : string       = "{id}_handle_char"
  get_handle_background_id(id : string) : string = "{id}_handle_background"
  get_content_id(id: string) : string            = "{id}_content"

{{
  /**
   * {1 Configuration}
   */

  default_config_noresize: WSidepanel.config = {
    get_handle = open_close_handles(<>→</>, <>←</>)

    place = {left}
    position = {absolute}

    get_size = _ -> {size={percent=21.}}
    get_zindex = _ -> 20

    opening_delay = 200

    on_change_state = (_, _, _ -> void)
  }

  /**
   * A default configuration of a side panel.
   * It's on the left side and takes 25% of the page. It can be opened or
   * closed by the user using handles.
   */
  default_config(content_element_opt: option(WSidepanel.content)): WSidepanel.config =
    Option.switch(content_element -> {default_config_noresize with
            on_change_state =
              change_content_size(content_element, _, _, _)
        }, default_config_noresize, content_element_opt)

  default_left_config = default_config

  default_right_config(content_element: option(WSidepanel.content))
      : WSidepanel.config =
    {default_config(content_element) with
      get_handle = open_close_handles((<>←</>), (<>→</>))
      place = {right}
    }

  default_top_config(content_element: option(WSidepanel.content))
      : WSidepanel.config =
    {default_config(content_element) with
      get_handle = open_close_handles((<>↓</>), (<>↑</>))
      place = {top}
    }

  default_bottom_config(content_element: option(WSidepanel.content))
      : WSidepanel.config =
    {default_config(content_element) with
      get_handle = open_close_handles((<>↑</>), (<>↓</>))
      place = {bottom}
    }


  /**
   * {1 High-level interface}
   */

  /**
   * The main function used to create a side-panel widget.
   *
   * @param config The configuration of the side-panel
   * @param id The HTML identifier surrounding the side-panel
   * @param init_content The HTML content to display in the side-panel
   * @param start_opened Whether or not to display the side-panel by default
   * @return The HTML representing the side-panel
   */
  html(config: WSidepanel.config, id: string, init_content: xhtml,
      start_opened: bool)
      : xhtml =
    pos_css = css_of_place(config.place)
    /* Build the XHTML structure of the panel */
    <div id={id}
        onready={_ ->
          if start_opened then
            do_open(config, id, false)
          else
            _ = set_handle_size(config, id, {closed})
            void
        }
        style="z-index: {some(config.get_zindex(id))};
            position: {css_of_position(config.position)};">
      {<div id={get_content_id(id)}
          style="display: none; position: relative;">
        {init_content}
      </div>
        |> WStyler.add(pos_css.content, _)}
      {<div id={get_handle_id(id)}></div>
        |> WStyler.add(pos_css.handle, _)}
    </div>
      |> WStyler.add(pos_css.container, _)

  /**
   * {2 Imperative interface}
   */

  /**
   * Open the side-panel
   * @param config Configuration of the side-panel
   * @param id HTML id of the side-panel
   * @param animate Whether or not to animate the side-panel when opening
   */
  @client do_open(config: WSidepanel.config, id: string, animate: bool)
      : void =
    set_state(config, id, {opened}, animate)

  /**
   * Close the side-panel
   * @param config Configuration of the side-panel
   * @param id HTML id of the side-panel
   * @param animate Whether or not to animate the side-panel when closing
   */
  @client do_close(config: WSidepanel.config, id: string, animate: bool)
      : void =
    set_state(config, id, {closed}, animate)

  /**
   * Switch the panel state (open it if it’s closed, close it if it’s opened)
   * @param config Configuration of the side-panel
   * @param id HTML id of the side-panel
   * @param animate Whether or not to animate the side-panel change
   */
  @client toggle(config: WSidepanel.config, id: string, animate: bool): void =
    if Dom.has_class(#{id}, get_opened_class(id)) then
      do_close(config, id, animate)
    else
      do_open(config, id, animate)

  /**
   * {1 Lower-level interface}
   */

  /**
   * A helper function building an empty [get_handle]. It can be used to
   * disable handles of the side-panel.
   */
  no_handles(_: WSidepanel.config, _: string, _: WSidepanel.display)
      : WSidepanel.handle =
    {none}

  /**
   * Auxiliary function to change the handle colors
   */
  change_handle_colors(id: string, bg_color: color, fg_color: color)
      : Dom.event -> void = _ ->
    Dom.transform([#{get_handle_background_id(id)} -> css <-
        css {background: {Css_build.background_color(bg_color)};},
      #{get_handle_char_id(id)} -> css <- [Css_build.color(fg_color)]])

  /**
   * A helper function building a default [get_handle] given two XHTML chunks
   * (corresponding to opening and closing values
   */
  open_close_handles(close_elt: xhtml, open_elt: xhtml)
      : (WSidepanel.config, string, WSidepanel.display -> WSidepanel.handle) =
    /* Auxiliary function to build the handle given a character to display */
    handle_aux(id: string, char_elt: xhtml, place: WSidepanel.place,
        event_callback: (Dom.event -> void)): xhtml =
      css_border_attr = match place with
        | {left}   -> "border-right"
        | {right}  -> "border-left"
        | {top}    -> "border-bottom"
        | {bottom} -> "border-top"
      <a title = "Click to open the side-panel"
          onmouseover =
            {change_handle_colors(id, Color.darkgray, Color.lightgrey)}
          onmouseout =
            {change_handle_colors(id, Color.lightgrey, Color.darkgray)}
          onclick = {event_callback}>
        <div id={get_handle_background_id(id)}
            style="position: absolute; width: 100%; height: 100%;
                {css_border_attr}: 1px solid #696969;
                top: 0; right: 0; background: #D2D2D2;">
          <span id={get_handle_char_id(id)}
              style="font-size: 20px; font-weight: bold; color: #878787;
                  position: absolute; text-align: center; top: 50%;
                  margin-top: -12px; width: 100%;">
            {char_elt}
          </span>
        </div>
      </a>
    /* Handle building functions */
    (config, id, display ->
        if is_opened(id, display) then
          {content=handle_aux(id, open_elt, config.place,
              (_ -> do_close(config, id, true)))
            size={px=20}}
        else
          {content=handle_aux(id, close_elt, config.place,
              (_ -> do_open(config, id, true)))
            size={px=20}})

  /**
   * {2 Lower-level imperative interface}
   */

  /**
   * Modify the current side-panel state (opened or closed)
   */
  @client set_state(config: WSidepanel.config, id: string,
      display: WSidepanel.display, animate: bool)
      : void =
    orientation = orientation_of_place(config.place)
    delay       = if animate then config.opening_delay else 0
    is_opened   = Dom.has_class(#{id}, get_opened_class(id))
    config_size = config.get_size(id)
    fullsize    = {size = {percent = 100.}}
    /* Auxiliary function opening the panel to the given size */
    open_aux(open_size: Css.size_or_normal): Css.size_or_normal =
      size_delta = set_size(orientation, id, open_size)
      _ = Dom.transition(#{get_content_id(id)}, Dom.Effect.with_duration({millisec = delay}, Dom.Effect.show()))
      do Dom.add_class(#{id}, get_opened_class(id))
      size_delta
    /* Auxiliary function closing the panel to the given size */
    close_aux(): Css.size_or_normal =
      _ = Dom.transition(#{get_content_id(id)}, Dom.Effect.with_duration({millisec = delay}, Dom.Effect.hide()))
      size_delta = set_size(orientation, id, {size={em=0.}})
      do Dom.remove_class(#{id}, get_opened_class(id))
      size_delta
    /* Determine action to take given the wanted and current state */
    size_delta = match (display : WSidepanel.display, is_opened) with
      | ({closed}, {true}) ->
        close_aux()
      | ({opened}, {true}) -> /* Only reset the size to config one */
        set_size(orientation, id, config_size)
      | ({opened}, {false}) ->
        open_aux(config_size)
      | ({opened_with_size=size}, {true}) -> /* Only change the size */
        set_size(orientation, id, size)
      | ({opened_with_size=size}, {false}) ->
        open_aux(size)
      | ({fullpage}, {true}) ->
        set_size(orientation, id, fullsize)
      | ({fullpage}, {false}) ->
        open_aux(fullsize)
      | ({~custom}, {true}) ->
        Option.lazy_switch(set_size(orientation, id, _), close_aux, custom(id))
      | ({~custom}, {false}) ->
        Option.switch(open_aux, {normal}, custom(id))
      | _ -> @fail
    /* Update the handle */
    handle_size_delta = set_handle_size(config, id, display)
    /* Trigger callback function */
    config.on_change_state(config.place, size_delta, handle_size_delta)

  /**
   * Update the handle content to match the new state of the panel
   */
  @client set_handle_size(config: WSidepanel.config, id: string,
      display: WSidepanel.display): Css.size_or_normal =
    orientation = orientation_of_place(config.place)
    /* Save the size of the panel before any resizement */
    init_size = get_oriented_size(orientation, #{id})
    /* Resize the handle */
    handle_id = get_handle_id(id)
    (min_size_attr, size_attr, css_build_margin) =
      css_of_handle(config.place)
    (handle_xhtml, handle_size) = config.get_handle(config, id, display)
      |> values_of_handle(_)
    // TODO: simplify and factorize funactions
    do Dom.transform([#{handle_id} <- handle_xhtml,
        #{handle_id} -> css <- [{not_typed=(size_attr,
          string_of_size(handle_size))}],
        #{get_content_id(id)} -> css <- [css_build_margin(handle_size)],
        #{id} -> css <- [{not_typed=(min_size_attr,
            string_of_size(handle_size))}]])
    /* Retrieve the new size of the panel */
    new_size = get_oriented_size(orientation, #{id})
    {size={px=(new_size - init_size)}}

  /**
   * Change the size of an element, and return the actual change in size
   */
  @client set_size(orientation: WSidepanel.private.orientation, id: string,
      size: Css.size_or_normal): Css.size_or_normal =
    match size with {normal} -> {normal} | {~size} ->
      init_size = get_oriented_size(orientation, #{id})
      elt_css   = switch_orientation(
          (-> Css_build.width(size)),
          (-> Css_build.height(size)), orientation)
      do Dom.transform([#{id} -> css <- [elt_css]])
      new_size      = get_oriented_size(orientation, #{id})
      size_delta_px = new_size - init_size
      match size with
      | {percent=_} ->
        size_delta_percent =
          percent_of_px(orientation, Dom.select_parent_one(#{id}),
              size_delta_px)
        {size={percent=size_delta_percent}}
      | _ -> {size={px=size_delta_px}}

  /**
   * Update the size of the main content according to side-panel info. Only
   * pixel and percentage sizes are handled for now.
   */
  @client change_content_size(content_element: WSidepanel.content,
      place: WSidepanel.place, panel_size_delta: Css.size_or_normal,
      handle_size_delta: Css.size_or_normal)
      : void =
    orientation = orientation_of_place(place)
    /* Gather content and content's parent sizes in pixels */
    parent = Dom.select_parent_one(#{content_element.id})
    init_content_px = Option.lazy_switch(
        px_of_size(orientation, parent, _),
        (-> get_oriented_size(orientation,
            #{content_element.id})),
        content_element.size_opt(content_element.id))
    size_delta =
      add_size(some((orientation, parent)),
          option_of_size(panel_size_delta),
          option_of_size(handle_size_delta))
        |> size_of_option(_)
    use_percent = match size_delta with {size={percent=_}} -> true | _ -> false
    delta_px = match size_delta with
      | {normal} -> 0
      | {~size} -> px_of_size(orientation, parent, size)
    /* Resize the content */
    new_content_px = init_content_px - delta_px
    new_content_size = if use_percent then
        {percent=percent_of_px(orientation, parent,
            new_content_px)}
      else
        {px=new_content_px}
    new_content_css = [switch_orientation(
        (-> Css_build.width(new_content_size)),
        (-> Css_build.height(new_content_size)), orientation)]
    /* Hide the panel if the new size is null */
    new_content_css =
      if new_content_px <= 1 then Css_build.display_none +> new_content_css
      else Css_build.display_block +> new_content_css
    /* Auxiliary function retrieving current margin of the panel */
    init_margin() = Option.lazy_default(->
        null_size = if use_percent then {percent=0.} else {px=0}
        null_margin = Css_build.margin_all(null_size)
        init_margin_str_opt =
          Dom.get_property_unsafe(#{content_element.id}, "style")
          |> try_parse_style
          |> Option.default([], _)
          |> List.assoc("margin", _)
        Option.switch((init_margin_str ->
            try_parse_margin(use_percent, init_margin_str) ?
                null_margin),
          null_margin, init_margin_str_opt),
      content_element.margin_size_opt(content_element.id))
    /* Shift the content if necessary */
    new_content_css = match (place, size_delta) with
      | ({left}, {~size}) ->
        add_margin(some((orientation, parent)),
            Css_build.margin_left(size),
            init_margin()) +> new_content_css
      | ({top}, {~size}) ->
        add_margin(some((orientation, parent)),
            Css_build.margin_top(size),
            init_margin()) +> new_content_css
      | _ -> new_content_css
    /* Actually apply CSS changes */
    Dom.transform([#{content_element.id} -> css <- new_content_css])

  /**
   * {1 Private functions aimed at internal use}
   *
   * Do not use them outside of the module.
   *
   *  {2 Panel styles}
   */

  @private
  left_place_style: WSidepanel.private.place_css = {
    container = WStyler.make_style(css {
      top: 0px;
      left: 0px;
      height: 100%;
    })
    content = WStyler.make_style(css {
      top: 0px;
      left: 0px;
      width: 100%;
      height: 100%;
      padding: 0;
    })
    handle = WStyler.make_style(css {
      position: absolute;
      right: 0;
      top: 0;
      width: 20px;
      height: 100%;
    })
  }

  @private
  right_place_style: WSidepanel.private.place_css = {
    container = WStyler.make_style(css {
      top: 0px;
      right: 0px;
      height: 100%;
    })
    content = WStyler.make_style(css {
      top: 0px;
      left: 0px;
      width: 100%;
      height: 100%;
      padding: 0;
    })
    handle = WStyler.make_style(css {
      position: absolute;
      left: 0;
      top: 0;
      width: 20px;
      height: 100%;
    })
  }

  @private
  top_place_style: WSidepanel.private.place_css = {
    container = WStyler.make_style(css {
      top: 0px;
      left: 0px;
      width: 100%;
    })
    content = WStyler.make_style(css {
      top: 0px;
      left: 0px;
      width: 100%;
      height: 100%;
      padding: 0;
    })
    handle = WStyler.make_style(css {
      position: absolute;
      bottom: 0px;
      width: 100%;
      height: 20px;
    })
  }

  @private
  bottom_place_style: WSidepanel.private.place_css = {
    container = WStyler.make_style(css {
      bottom: 0px;
      left: 0px;
      width: 100%;
    })
    content = WStyler.make_style(css {
      top: 0px;
      left: 0px;
      width: 100%;
      height: 100%;
      padding: 0;
    })
    handle = WStyler.make_style(css {
      position: absolute;
      top: 0px;
      width: 100%;
      height: 20px;
    })
  }

  /**
   *  {2 Panel state related functions}
   */

  @private
  @client is_opened(id: string, display: WSidepanel.display): bool =
    match display with
    | {closed} -> false
    | {opened} | {opened_with_size=_} | {fullpage} -> true
    | {~custom} -> Option.is_some(custom(id))

  /**
   *  {2 Conversion functions}
   */

  @private
  @client string_of_size(size: Css.size): string = match size with
    | { ~percent } -> "{percent}%"
    | { ~px }      -> "{px}px"
    | _            -> error("string_of_size pattern matching: This kind of CSS size is not implemented.")

  @private
  css_of_place(pos: WSidepanel.place): WSidepanel.private.place_css =
    match pos with
      | {left} -> left_place_style
      | {right} -> right_place_style
      | {top} -> top_place_style
      | {bottom} -> bottom_place_style

  /**
   * Get CSS elements corresponding to the panel place
   */
  @private
  @client css_of_handle(place: WSidepanel.place)
      : (string, string, (Css.size -> Css.unary)) =
    match place with
    | {left}   -> ("min-width", "width", Css_build.margin_right)
    | {right}  -> ("min-width", "width", Css_build.margin_left)
    | {top}    -> ("min-height", "heigh", Css_build.margin_bottom)
    | {bottom} -> ("min-height", "heigh", Css_build.margin_top)

  /**
   * Get a handle value from an option, or a default empty one
   */
  @private
  @client values_of_handle(handle: WSidepanel.handle): (xhtml, Css.size) =
    match handle with
    | ~{content size} -> (content, size)
    | {none} -> (<></>, {px=0})

  @private
  @client switch_orientation(horizontal_action: -> 'a,
      vertical_action: -> 'a, orientation: WSidepanel.private.orientation)
      : 'a =
    match orientation with
      | {horizontal} -> horizontal_action()
      | {vertical}   -> vertical_action()

  @private
  @client orientation_of_place(place: WSidepanel.place)
      : WSidepanel.private.orientation =
    match place with
      | {left} | {right}  -> {horizontal}
      | {top}  | {bottom} -> {vertical}

  /**
   * Retrieve the width or height of an element given its orientation
   */
  @private
  @client get_oriented_size(orientation: WSidepanel.private.orientation,
      element: dom)
      : int =
    match orientation with
      | {horizontal} -> Dom.get_width(element)
      | {vertical}   -> Dom.get_height(element)

  /**
   * Convert a pixel value to a percent relative to an element
   */
  @private
  @client percent_of_px(orientation: WSidepanel.private.orientation, element: dom,
      px: int): float =
    element_px = get_oriented_size(orientation, element)
    Float.of_int(px) / Float.of_int(element_px) * 100.

  /**
   * Convert a percent (relative to an element) to a pixel value
   */
  @private
  @client px_of_percent(orientation: WSidepanel.private.orientation, element: dom,
      percent: float): int =
    element_px = get_oriented_size(orientation, element)
    Int.of_float(percent * Float.of_int(element_px) / 100.)

  /**
   * Try to convert the given [size] to pixels
   */
  @private
  @client px_of_size(orientation: WSidepanel.private.orientation, element: dom,
      size: Css.size): int =
    match size with
    | {~px} -> px
    | {~percent} -> px_of_percent(orientation, element, percent)
    | _ -> error("px_of_percent: Css.size unit not supported")

  @private
  @client option_of_size(css_size: Css.size_or_normal): option(Css.size) =
    match css_size with
    | {normal} -> none
    | {~size}  -> some(size)

  @private
  @client size_of_option(opt_size: option(Css.size)): Css.size_or_normal =
    match opt_size with
    | {none}  -> {normal}
    | {~some} -> {size=some}

  /**
   * Boring conversion of [Css.position] to a more restrictive type
   */
  @private
  css_of_position(sidepanel_pos: WSidepanel.position): Css.position =
    match sidepanel_pos with
    | {absolute} -> {absolute}
    | {fixed}    -> {fixed}

  /**
   *  {2 CSS operations}
   */

  /**
   * Get the null size corresponding to the given size unit.
   */
  @private @client
  null_size: Css.size -> Css.size = map_css_size((_: float -> 0.))

  /**
   * Make an operation on two [Css.size]
   */
  @private
  @client op_size(op_int: int, int -> int, op_float: float, float -> float,
      ref_element_opt: option((WSidepanel.private.orientation, dom)),
      size1_opt: option(Css.size), size2_opt: option(Css.size))
      : option(Css.size) =
    error() = error("op_size: incompatible CSS size operation")
    match (size1_opt, size2_opt) with
    | ({none}, {none}) -> none
    | ({none}, {some=css_size}) -> op_size(op_int, op_float, ref_element_opt,
      some(null_size(css_size)), size2_opt)
    | ({some=css_size}, {none}) -> op_size(op_int, op_float, ref_element_opt,
      size1_opt, some(null_size(css_size)))
    | ({some=size1}, {some=size2}) -> some(
      match (size1, size2) with
      | ({cm=s1}      , {cm=s2})      -> {cm=op_float(s1, s2)}
      | ({em=s1}      , {em=s2})      -> {em=op_float(s1, s2)}
      | ({ex=s1}      , {ex=s2})      -> {ex=op_float(s1, s2)}
      | ({inch=s1}    , {inch=s2})    -> {inch=op_float(s1, s2)}
      | ({mm=s1}      , {mm=s2})      -> {mm=op_float(s1, s2)}
      | ({percent=s1} , {percent=s2}) -> {percent=op_float(s1, s2)}
      | ({pc=s1}      , {pc=s2})      -> {pc=op_float(s1, s2)}
      | ({pt=s1}      , {pt=s2})      -> {pt=op_float(s1, s2)}
      | ({px=s1}      , {px=s2})      -> {px=op_int(s1, s2)}
      | ({percent=s1} , {px=s2})      ->
        Option.lazy_switch(((orientation, element) ->
            {percent=op_float(s1, percent_of_px(orientation, element, s2))}),
          error, ref_element_opt)
      | ({px=s1}, {percent=s2}) ->
        Option.lazy_switch(((orientation, element) ->
            {px=op_int(s1, px_of_percent(orientation, element, s2))}),
          error, ref_element_opt)
      | _ -> error())

  @private add_size = op_size(`+`, Float.`+`, _, _, _)
  @private sub_size = op_size(`-`, Float.`-`, _, _, _)

  @private
  @client add_block_size(ref_elt, b1: Css.block_size, b2: Css.block_size)
      : Css.block_size =
    add_size_aux = add_size(ref_elt, _, _)
    {
      t = add_size_aux(b1.t, b2.t)
      r = add_size_aux(b1.r, b2.r)
      l = add_size_aux(b1.l, b2.l)
      b = add_size_aux(b1.b, b2.b)
    }

  @private
  @client add_margin(ref_elt, margin1: Css.unary, margin2: Css.unary)
      : Css.unary =
    match (margin1, margin2) with
    | ({margin=m1}, {margin=m2}) -> {margin=add_block_size(ref_elt, m1, m2)}
    | _ -> error("add_margin: arguments are not margins")

  /**
   *  {2 CSS parsing}
   */

  /**
   *    {3 Parsing rules}
   */

  @private
  @client size_rule = parser value=Rule.float
      unit=("%"|"px"|"cm"|"em"|"ex"|"inch"|"mm"|"pc"|"pt") ->
    match Text.to_string(unit) with
    | "%"    -> {percent=value}
    | "px"   -> {px=Int.of_float(value)}
    | "cm"   -> {cm=value}
    | "em"   -> {em=value}
    | "ex"   -> {ex=value}
    | "inch" -> {inch=value}
    | "mm"   -> {mm=value}
    | "pc"   -> {pc=value}
    | "pt"   -> {pt=value}
    | _ -> @fail

  @private
  @client attr_rule =
    parser attr=([a-zA-Z\-]+)":" Rule.ws value=(([a-zA-Z0-9%# \-]|".")+) ->
      (Text.to_string(attr), Text.to_string(value))

  @private
  @client style_rule =
    parser css_list={Rule.parse_list(attr_rule, parser ";" Rule.ws)} ";"? ->
      css_list

  /**
   *    {3 Parsing functions}
   */

  /**
   * Parse the content of a 'style' attribute and return a corresponding list
   * of pairs (css_attribute, value)
   */
  @private
  @client try_parse_style(style_str: string): option(list((string, string))) =
    Parser.try_parse(style_rule, style_str)

  @private
  @client try_parse_size(size_str: string): option(Css.size) =
    Parser.try_parse(size_rule, size_str)

  @private
  @client try_parse_margin(use_percent: bool, margin_str: string)
      : option(Css.unary) =
    margin_rule     = Rule.parse_list(size_rule, Rule.strict_ws)
    margin_list_opt = Parser.try_parse(margin_rule, margin_str)
    null_size       = if use_percent then {percent= 0.} else {px = 0}
    Option.map(margin_list ->
        List.foldi(idx, size, margin_acc ->
            size_opt = some(size)
            block = (match margin_acc with {~margin} -> margin
              | _ -> error("try_parse_margin: Css.unary should be margin"))
            (match idx with
            | 0 -> Css_build.margin_all(size)
            | 1 -> {margin={block with l=size_opt r=size_opt}}
            | 2 -> {margin={block with b=size_opt}}
            | 3 -> {margin={block with l=size_opt}}
            | _ -> @fail),
          margin_list, Css_build.margin_all(null_size)),
      margin_list_opt)
}}
