/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

/**
 * A configurable autocompletion widget.
 *
 * @category WIDGET
 * @author Tt Team & Guillem Rieu, 2010
 * @destination PUBLIC
 * @stability TRANSITIONAL
 */

import stdlib.web.client
import stdlib.widgets.core

// FIXME: keep changing the selection while an arrow key is down (use keydown instead of keyup?)
// FIXME: solve the switching from keyboard selection to mouse selection by resetting the classes of all suggestions
// FIXME: Opera bug with arrow up key (disable keyboard navigation in opera or with an option?)
// TODO: enable the user to use a styler with suffixes

/**
 * {1 About this module}
 *
 * Input field with autocomplete capabilities
 *
 * {1 Where should I start?}
 *
 * Use the function [WCompletion.html] to construct the widget.
 */

/**
 * {1 Types defined in this module}
 */

type WCompletion.suggestion('item) = {
  input   : string /** What to add to the input box when a selection is made */
  display : xhtml  /** The corresponding XHTML to display in the suggestion list */
  item    : 'item  /** The item associated with the suggestion */
}

type WCompletion.config('item) = {
     strict         : bool                         /** Whether or not to force the selection if a single element remains in the list */
     multiple       : option(string)               /** An optional separator for multiple selections */
     nstart         : int                          /** Start to suggest possible completions after [nstart] entered characters */
     clear_focus    : bool                         /** Whether to empty or not the input field on focus */
     auto_scroll    : bool                         /** Automatically scroll to follow the focus */
     suggest        :                              /** A function returning a list of suggestions for a given string */
       string -> list(WCompletion.suggestion('item))

     max_height_px  : option(int)                  /** Maximum height of the suggestion list (before scrolling), in pixels */
     input_style    : WStyler.styler               /** Style of the input field */
     list_style     : WStyler.styler               /** Style of the suggestion list */
     element_style  : WStyler.styler               /** Style of a the list elements */
     selected_style : WStyler.styler               /** Style of a selected element */

     on_open        : string -> void               /** Function called when showing the suggestion list */
     on_close       : string -> void               /** Function called when hiding the suggestion list */
     open_class     : string                       /** Name of the class used to mark the widget as open (when the date picker is shown) */
}

WCompletion =
{{

  /**
   * {1 Configuration}
   */

  /**
   * Default styles of the [WCompletion] widget
   */
  default_list_style = WStyler.make_style(css {
    position   : absolute;
    float      : left;
    display    : none;
    z-index    : 999;
    background : white;
    border     : 1px solid #DDDDDD;
    list-style : {{style=none style_position=none}};
  })

  default_element_style = WStyler.make_style(css {
    display: block;
    padding: 2px 4px 2px 10px;
  })

  default_selected_style = WStyler.make_style(css {
    background : #EEEEEE;
    font-decoration : bold;
  })

  /**
   * Default configuration of the [WCompletion] widget
   */
  default_config : WCompletion.config = {
    strict         = false
    multiple       = none
    nstart         = 1
    clear_focus    = true
    auto_scroll    = true
    suggest        = _ -> []

    max_height_px  = {some = 200}
    input_style    = WStyler.empty
    list_style     = default_list_style
    element_style  = default_element_style
    selected_style = default_selected_style

    on_open        = _ -> void
    on_close       = _ -> void
    open_class   = "wcompletion_open"
  }

  /**
   * {1 High-level interface}
   */

  /**
   * The main function to be called for creating a [WCompletion] widget.
   *
   * @param config The widget configuration
   * @param onselect Callback function triggered when a choice is made
   * @param id The main ID identifying the widget
   * @param init_value The initial value of the input field
   * @return The XHTML corresponding to the widget
   */
  html(config: WCompletion.config, onselect: 'a -> void, id: string,
      init_value: WCompletion.suggestion): xhtml =
    input_id = get_input_id(id)
    max_height_style = match config.max_height_px with
                       | {none} -> {none}
                       | {some=px} -> {size=~{px}}
    <div id="{id}_completion_wrapper"
        style="display: inline;">
      <div id="{id}_completion_input_wrapper"
          style="display: inline; padding: 0; margin: 0;">
      {<input id={input_id} value={init_value.input}
              onfocus={init_widget(config, id, onselect, init_value)}
              options:onclick="stop_propagation"
              options:onmouseup="stop_propagation" />
        |> WStyler.add(config.input_style, _)}
      </div>
      <div id="{id}_completion_list_wrapper"
          style="position: relative; display: inline; padding: 0; margin: 0;">
       {<div id={get_list_id(id)}
            style={css { display: none; overflow: auto; position: absolute
                       ; max-height: {max_height_style}}} />
        |> WStyler.add(config.list_style, _)}
      </div>
    </div> ;

  /**
   * Retrieve the list of selected values from the input field.
   *
   * @param config Configuration of the widget
   * @param id Identifier of the widget
   * @return The string read from the input field
   */
  parse(config: WCompletion.config, id: string): list(string) =
    selection_str = Dom.get_value(#{get_input_id(id)})
    Option.switch(sep ->
        string_explode_strip(sep, selection_str),
      [selection_str], config.multiple)

/**
 * {2 Imperative interface}
 */

  /**
   * Show the suggestion list
   *
   * @param id The identifier of the widget
   */
  do_open(open_class: string, id: string): void =
    show_list(open_class, id)

  /**
   * Hide the suggestion list
   *
   * @param id The identifier of the widget
   */
  do_close(open_class: string, id: string): void =
    hide_list(open_class, id)

  /**
   * Test whether or not the suggestion list is shown
   */
  is_open(open_class: string, id: string): bool =
    is_shown(open_class, id)


  /**
   * {1 Private functions aimed at internal use}
   *
   * Do not use them outside of the module.
   */

  /** {2 IDs} **/

  @private get_list_id(id : string)      : string = id ^ "_list"
  @private get_input_id(id : string)     : string = id ^ "_input"

  get_suggestion_id(id: string, index: int): string =
    "{id}_suggestion_{index}"

  /**
   *  {2 Various helper functions}
   */

  @private
  string_explode_strip(separator: string, source: string): list(string) =
    String.strip(separator)
      |> String.explode(_, source)
      |> List.map(String.strip, _)

  /**
   *  {2 DOM manipulation functions}
   */

  /**
   * Initialization of the suggestion list
   */
  @private
  make_active(config: WCompletion.config, id: string, callback: 'a -> void,
      init_value: WCompletion.suggestion): void =
    input_id = get_input_id(id)
    /* Input event */
    _ = Dom.unbind_event(#{input_id}, {keyup})
    _ = Dom.bind(#{input_id}, {keyup},
        key_listener(config, id, callback, init_value, 1, 0))
    void

  /**
   * Initialization function to call when the widget is focused
   */
  @private
  init_widget(config: WCompletion.config, id: string, onselect: 'a -> void,
      init_value: WCompletion.suggestion)
      : Dom.event -> void = _ ->
    input_id = get_input_id(id)
    do sleep(10, ( -> make_active(config, id, onselect, init_value)))
    if config.clear_focus &&
        String.equals(Dom.get_value(#{input_id}), init_value.input) then
      _ = Dom.set_value(#{input_id}, "")
    void

  /**
   * Display the suggestion list
   */
  @private
  show_list(open_class: string, id: string): void =
    input_id = get_input_id(id)
    wrapper_id = "{id}_completion_wrapper"
    do Dom.add_class(#{input_id}, open_class)
    wrapper_width = Dom.get_width(#{wrapper_id})
    wrapper_height = Dom.get_height(#{wrapper_id})
    input_width = Dom.get_width(#{input_id})
    Dom.transform([#{get_list_id(id)} -> style <-
      (css { display: inline;
             top: {wrapper_height + 2}px;
             left: {-wrapper_width + 1}px;
             width: {input_width}px;})
      ])

  /**
   * Hide the suggestion list
   */
  @private
  hide_list(open_class: string, id: string): void =
    do Dom.remove_class(#{get_input_id(id)}, open_class)
    Dom.transform([#{get_list_id(id)} -> style <- (css { display: none; })])

  /**
   * Is the suggestion list displayed?
   **/
  @private
  is_shown(open_class: string, id: string): bool =
    Dom.has_class(#{get_input_id(id)}, open_class)

  /**
   *  {2 Keyboard events manager}
   */

  /**
   * When moving the mouse over an element of the suggestion list
   */
  @private
  select_over(config: WCompletion.config, id: string, onselect: ('a -> void),
      auto_scroll: bool, init_value: WCompletion.suggestion, imax: int,
      new_pos: int, old_pos: int): void =
    input_id = get_input_id(id)
    list_id = get_list_id(id)
    _ = Dom.unbind_event(#{input_id}, {keyup})
    _ = Dom.bind(#{input_id}, {keyup},
        key_listener(config, id, onselect, init_value, imax, new_pos))
    /* Gather list layout info for scrolling */
    list_height = Dom.get_inner_height(#{list_id})
    elt_height = Dom.get_outer_height(#{get_suggestion_id(id, new_pos)})
    new_scroll = max(0, elt_height * new_pos - list_height / 2)
    /* Unstyle former selected element, and style the new one instead */
    do if new_pos != 0 then
      do WStyler.clear_dom(get_suggestion_id(id, old_pos))
      do WStyler.set_dom(config.element_style,
          get_suggestion_id(id, old_pos))
      WStyler.merge([config.element_style,
          config.selected_style])
        |> WStyler.set_dom(_, get_suggestion_id(id, new_pos))
    /* Automated scrolling */
    if new_pos > 0 && auto_scroll then
      do Dom.set_scroll_top(#{list_id}, new_scroll)
      void

  /**
   * A function installing a handle on the given widget, initializing
   * back the field to the previous value if nothing has been entered.
   */
  @private
  update_initval(id: string, _init_value: string): void =
    input_id = get_input_id(id)
    handle = (_ ->
      // FIXME: set the current value as the new init value
      // val =  Dom.get_value(#{input_id})
      // if val == "" then
      //   _ = Dom.set_value(#{input_id}, init_value)
      void)
    /* Bind again the 'onblur' event */
    _ = Dom.unbind_event(#{input_id}, {blur})
    _ = Dom.bind(#{input_id}, {blur}, handle)
    void

  /**
   * Get suggestion list
   */
  @private
  get_suggestions(config: WCompletion.config, id: string) =
    input_val =  Dom.get_value(#{get_input_id(id)}) |> String.strip(_)
    pat = Option.switch(sep ->
        if String.get_end(0, input_val) == String.strip(sep) then ""
        else
          String.explode(sep, input_val)
            |> List.rev(_) |> List.get(0, _) |> Option.default("", _),
      input_val, config.multiple)
    if String.length(pat) < config.nstart then [] else config.suggest(pat)

  /**
   * Close the suggestion list
   */
  @private
  close_list(config: WCompletion.config, id: string): void =
    if is_shown(config.open_class, id) then
      do hide_list(config.open_class, id)
      config.on_close(id)

  /**
   * Select an element of the suggestion list
   */
  @private
  select(config: WCompletion.config, id: string, onselect: 'a -> void,
      elt: WCompletion.suggestion)
      : void =
    input_id = get_input_id(id)
    new_selections =
      Option.lazy_switch(sep ->
          // retrieve already selected items, removing the one being edited
          selections = string_explode_strip(sep, Dom.get_value(#{input_id}))
          already_selected = List.take(List.length(selections) - 1, selections)
          if already_selected == [] then
            elt.input ^ sep
          else
            String.concat(sep, already_selected) ^ sep ^ elt.input ^ sep,
        (-> do update_initval(id, elt.input) elt.input), config.multiple)
    do Dom.set_value(#{input_id}, new_selections)
    do close_list(config, id)
    _ = Dom.give_focus(#{input_id})
    onselect(elt.item)

  /**
   * When the mouse leaves the suggestion list
   */
  @private
  select_out(config: WCompletion.config, id: string, index : int): void =
    // FIXME: use Dom.unbind to only remove arrow and enter key from being catched
    do WStyler.clear_dom(get_suggestion_id(id, index))
    WStyler.set_dom(config.element_style,
      get_suggestion_id(id, index))

  /**
   * Return a suggestion list XHTML element
   */
  @private
  suggestion(config: WCompletion.config, id: string, onselect: ('a -> void),
      element: WCompletion.suggestion, imax: int, index: int, pos: int)
      : xhtml =
    elt_style =
      if index == pos then
        WStyler.merge([config.element_style,
            config.selected_style])
      else
        config.element_style
    <li onclick={ _ -> select(config, id, onselect, element)}
        onmouseout={ _ -> select_out(config, id, index)}
        onmousemove={ _ -> select_over(config, id, onselect,
            false, element, imax, index, pos)}
        id={get_suggestion_id(id, index)}>
      {element.display}
    </li>
      |> WStyler.set(elt_style, _)

  /**
   * Build a XHTML list of the suggestions
   */
  @private
  suggestion_list(config: WCompletion.config, id: string, onselect: ('a -> void),
      suggestion_list: list(WCompletion.suggestion), _pos: int)
      : void =
    imax = List.length(suggestion_list)
    data = List.mapi((index, element ->
      // TODO: replace the [index] argument by the previously selected suggestion
      suggestion(config, id, onselect, element, imax, index + 1, index)),
        suggestion_list)
    do Dom.transform([#{get_list_id(id)} <-
        <ul style={css {
            padding: 0px; margin: 0px;}} style="top: -30px">
          {data}
        </ul>])
    do show_list(config.open_class, id)
    config.on_open(id)

  /**
   * Update the suggestion list according to the new entered key
   */
  @private
  entry_key_up(config: WCompletion.config, id: string, onselect: ('a -> void),
      pos: int, init_value: WCompletion.suggestion)
      : void =
    suggestions = get_suggestions(config, id)
    do match (suggestions, config.strict) with
      | ([], _) -> close_list(config, id)
      | ([singleton], {true}) -> select(config, id, onselect, singleton)
      | (_, _) -> suggestion_list(config, id, onselect, suggestions, pos)
    imax = List.length(suggestions)
    /* Initialize selected element */
    select_over(config, id, onselect, config.auto_scroll, init_value,
        imax, 1, pos)

  /**
   * The keyboard events handler
   */
  @private
  key_listener(config: WCompletion.config, id: string, onselect: 'a -> void,
      init_value: WCompletion.suggestion, imax: int, pos: int)
      : Dom.event -> void = evt ->
    match Client.get_keycode(evt) with
      | 38 -> // event key up
        if pos == 0 || pos == 1 then
          select_over(config, id, onselect, true, init_value, imax, imax, pos)
        else if pos > 1 then
          select_over(config, id, onselect, config.auto_scroll, init_value, imax, pos - 1, pos)
      | 40 -> // event key down
        if pos == 0 || pos == imax then
          select_over(config, id, onselect, true, init_value, imax, 1, pos)
        else if pos < imax then
          select_over(config, id, onselect, config.auto_scroll, init_value, imax, pos + 1, pos)
      | 9 | 27 -> // TAB, ESC
        close_list(config, id)
      | 13 -> // event key enter
        select(config, id, onselect,
            List.get(pos - 1, get_suggestions(config, id)) ? init_value)
      | _ ->
        entry_key_up(config, id, onselect, pos, init_value)

}}
