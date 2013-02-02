/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * A two state widget.
 *
 * @category WIDGET
 * @author Guillem Rieu, 2011
 * @destination PUBLIC
 * @stability TRANSITIONAL
 */

// TODO: ensure we remain client-side as long as possible
// TODO: iphone style

/**
 * {1 About this module}
 *
 *
 * {1 Where should I start?}
 *
 */

import stdlib.widgets.core

/**
 * {1 Types defined in this module}
 */

type WSwitch.value = bool

type WSwitch.update_action =
    { update_xhtml: xhtml }
  / { update_style: WStyler.styler }

type WSwitch.config = {
  get_switch_on: string, (Dom.event -> void), bool -> WSwitch.update_action
  get_switch_off: string, (Dom.event -> void), bool -> WSwitch.update_action
}

WSwitch = {{

  /**
   * {1 Configuration}
   */

  simple_style_on = WStyler.make_style(css {
    background: greenyellow;
    border: 1px solid gray;
    font-decoration: bold;
    padding: 3px;
  })

  simple_style_off = WStyler.make_style(css {
    background: #FFC1C1;
    border: 1px solid gray;
    color: gray;
    font-decoration: bold;
    padding: 3px;
  })

  /*
   * Default configuration
   */

  /**
   * Lambda-lifted default config functions
   */
  default_switch_on(label_on: string, style_on: WStyler.styler)( _: string,
      switch_action: (Dom.event -> void), _: bool)
      : WSwitch.update_action =
    {update_xhtml=
      <span onclick={switch_action}>
        {label_on}
      </span>
        |> WStyler.set(style_on, _)}

  default_switch_off(label_off: string, style_off: WStyler.styler)( _: string,
      switch_action: (Dom.event -> void), _: bool)
      : WSwitch.update_action =
    {update_xhtml=
      <span onclick={switch_action}>
        {label_off}
      </span>
        |> WStyler.set(style_off, _)}
  /**
   * A default configuration constructor with modifiable labels
   *
   * @param label_on The text to display in the active state
   * @param label_off The text to display in the disabled state
   * @param style_on The style in the active state
   * @param style_off The style in the disabled state
   * @return A [WSwitch] configuration
   */

  default_config_with_settings(label_on: string, label_off: string,
      style_on: WStyler.styler, style_off: WStyler.styler)
      : WSwitch.config = {
    get_switch_on  = default_switch_on(label_on, style_on)
    get_switch_off = default_switch_off(label_off, style_off)
  }

  /**
   * The default configuration of [WSwitch]
   */
  default_config =
    default_config_with_settings("ON", "OFF",
        simple_style_on, simple_style_off)

  /*iphone_config =*/
  /*  default_config_with_settings("ON", "OFF",*/
  /*      iphone_style_on, iphone_style_off)*/

  /**
   * {1 High-level interface}
   */

  /**
   * Editable switch: the user can alternate between the two states of the 
   * switch.
   */
  edit(config: WSwitch.config, id: string, on_change: (WSwitch.value -> void),
      init_value: WSwitch.value): xhtml =
    switch_id = get_switch_id(id)
    /*checkbox_id = get_checkbox_id(id)*/
    /*checkbox_config: WCheckbox.config = {*/
    /*  prefix_class = none*/
    /*  stylers = IEditable.default_config.stylers*/
    /*}*/
    /*<>*/
    /*  {WCheckbox.show(checkbox_config, checkbox_id, init_value: WCheckbox.value)*/
    /*    |> WCore.make_inline_default(checkbox_id, _)}*/
    /*</>*/
    /*  |> WStyler.add(config.style, _)*/
    <div id=#{switch_id}
        onready={_ ->
          if init_value then
            update_widget(id, config, on_change,
                config.get_switch_on, switch_off_action, true)
          else
            update_widget(id, config, on_change,
                config.get_switch_off, switch_on_action, true)}>
    </div>

  /**
   * {2 Imperative interface}
   */

  update_widget(id: string, config: WSwitch.config, on_change,
      make_switch, make_action, init: bool)
      : void =
    switch_id = get_switch_id(id)
    update_action = make_switch(id, make_action(config, id, on_change, _), init)
    match update_action with
      | {~update_xhtml} -> Dom.transform([#{switch_id} <- update_xhtml])
      | {~update_style} -> WStyler.set_dom(update_style, switch_id)

  do_switch_on(config: WSwitch.config, id: string, on_change): void =
    update_widget(id, config, on_change, config.get_switch_on, switch_off_action, false)

  do_switch_off(config: WSwitch.config, id: string, on_change): void =
    update_widget(id, config, on_change, config.get_switch_off, switch_on_action, false)

  switch_on_action(config, id, on_change, _: Dom.event): void =
    do do_switch_on(config, id, on_change)
    on_change(true)

  switch_off_action(config, id, on_change, _: Dom.event): void =
    do do_switch_off(config, id, on_change)
    on_change(false)

  /**
   * Display-only switch: the widget is disabled and the user can't change
   * its state.
   */
  /*show(config: WSwitch.config, id: string, init_value: WSwitch.value): xhtml 
   * =*/
  /*  <></>*/

  /**
   * {1 Private functions}
   */

  @private get_switch_id(id: string): string = "{id}_switch"
  @private get_checkbox_id(id: string): string = "{id}_checkbox"

}}
