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
 * A configurable checkbox widget.
 *
 * @author Frederic Ye, 2010
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.1
 */

/**
 * {1 About this module}
 *
 * This widget is a checkbox with two states: checked or not.
 *
 * {1 Where should I start?}
 *
 * The simpler way to obtain a checkbox is to use the [WCheckbox.html] 
 * function:
 *
 * {[
 * change_cb(is_checked) =
 *   state_str = if is_checked then "checked" else "unchecked"
 *   Dom.transform([#feedback <- <>Checkbox {state_str}</>])
 *
 * params = {
 *   editable = true
 *   on_change = change_checkbox
 * }
 *
 * <div id=#feedback></div>
 * <div id=#sample_cb>
 *   {WCheckbox.html(WCheckbox.default_config_with_css, "sample_cb", params, 
 *   {value = false})
 * </div>
 * ]}
 *
 * {1 What if I need more?}
 *
 * The default configuration can be modified to customize the checkbox 
 * appearance:
 *
 * {[
 * my_config = {WCheckbox.default_config with
 *   ...
 * }
 * ]}
 */

import stdlib.widgets.core
import stdlib.interactions.editable
import stdlib.components.table

/**
 * {1 Types defined in this module}
 */

/**
 * State of a checkbox.
 */
type WCheckbox.value = bool

/**
 * Parameters of a checkbox.
 */
type WCheckbox.parameters = {
  editable: bool
  on_change: WCheckbox.value -> void
}

/**
 * Configuration of a checkbox.
 */
type WCheckbox.config = {
  prefix_class: option(string)
  stylers: IEditable.stylers
}

WCheckbox = {{

  /*
  * {1 ID shortcuts}
  */

  gen_id(prefix_id:string, suffix:string) =
    "{prefix_id}_checkbox{suffix}"

  checkbox_id(prefix_id:string) =
    gen_id(prefix_id, "")

  /**
   * {1 Configuration}
   */

  /**
   * A default checkbox configuration
   */
  default_config = {
    prefix_class = none
    stylers = IEditable.default_config.stylers
  } : WCheckbox.config

  default_config_with_css(css_prefix:string) = { default_config with
    prefix_class = some(css_prefix)
  } : WCheckbox.config

  /**
   * {1 High-level interface}
   */

  /**
   * Constructs a non-editable checkbox.
   *
   * @param config The configuration of the checkbox.
   * @param prefix_id The unique ID associated to the checkbox.
   * @param init_value The state of the checkbox.
   * @return The checkbox instance, represented by a XHTML value.
   */
  show(config:WCheckbox.config, prefix_id:string, init_value:WCheckbox.value) : xhtml =
    // FIXME: do not copy code: using Xhtml.to_dom ? ...
    // Maybe use another display ?
    html = if init_value then
             <input type="checkbox" readonly="readonly" disabled="disabled" id=#{checkbox_id(prefix_id)} checked="checked"/>
           else
             <input type="checkbox" readonly="readonly" disabled="disabled" id=#{checkbox_id(prefix_id)}/>
    html |> WStyler.add(config.stylers.show, _)

  /**
   * Constructs a checkbox.
   *
   * @param config The configuration of the checkbox.
   * @param prefix_id The unique ID associated to the checkbox.
   * @param parameters Initial parameters of the checkbox
   * @param init_value Initial state of the checkbox
   * @return The checkbox instance, represented by a XHTML value.
   */
  edit(config:WCheckbox.config, prefix_id:string, parameters:WCheckbox.parameters, init_value:WCheckbox.value) : xhtml =
    html = if init_value then
             <input onchange={change_action(config, prefix_id, parameters, _)} type="checkbox" id=#{checkbox_id(prefix_id)} checked="checked"/>
           else
             <input onchange={change_action(config, prefix_id, parameters, _)} type="checkbox" id=#{checkbox_id(prefix_id)}/>
    html |> WStyler.add(config.stylers.edit, _) // |> WCore.add_binds([({change}, change_action)], _) // works only in client mode

  /**
   * Constructs a checkbox, editable or not depending on the given parameters.
   *
   * @param config The configuration of the checkbox.
   * @param prefix_id The unique ID associated to the checkbox.
   * @param parameters Initial parameters of the checkbox.
   * @param init_value Initial state of the checkbox
   * @return The checkbox instance, represented by a XHTML value.
   */
  html(config:WCheckbox.config, prefix_id:string, parameters:WCheckbox.parameters, init_value:IEditable.content(WCheckbox.value)) : xhtml =
    val = match init_value with
          | {~value} -> value
          | _ -> false
    if parameters.editable then
      edit(config, prefix_id, parameters, val)
    else
      show(config, prefix_id, val)

  /**
   * Returns the state of the checkbox (checked or not).
   *
   * @param _config The configuration of the checkbox
   * @param prefix_id The unique ID associated to the checkbox
   * @return A boolean value corresponding to the checkbox state
   */
  parse(_config:WCheckbox.config, prefix_id:string) =
    Dom.is_checked(#{checkbox_id(prefix_id)})

  @private
  change_action(config:WCheckbox.config, prefix_id:string, parameters:WCheckbox.parameters, _event:Dom.event) =
    checked = parse(config, prefix_id)
    parameters.on_change(checked)

}}

WCheckCell(box : WCheckbox.value -> 'a, unbox : option('a) -> WCheckbox.value) : CTable.Cell.widget('a) =

  internal_parameters(config:CTable.Cell.config, id:string, editable:bool) = {
    editable = editable
    on_change = val -> config.on_change(id, box(val))
  }

  internal_html(config:CTable.Cell.config, id:string, val:IEditable.content(WCheckbox.value), editable:bool) =
    internal_config = { WCheckbox.default_config with
                        stylers.editable = config.style }
    WCheckbox.html(internal_config, id, internal_parameters(config, id, editable), val)
    |> WStyler.add(config.style, _)

  internal_set_value(config:CTable.Cell.config, id:string, val:IEditable.content(WCheckbox.value)) =
    Dom.transform([#{id} <- internal_html(config, id, val, config.openable)])

  internal_parse(_config:CTable.Cell.config, id:string) =
    parse_fun = id -> some(WCheckbox.parse(WCheckbox.default_config, id))
    IEditable.parse_content(id, parse_fun(_))

{

  html(config:CTable.Cell.config, id:string, val:IEditable.content('a)) : xhtml =
    val = match val with
          | {~value} -> {value=unbox(Option.some(value))}
          | {missing_value} -> {missing_value}
          | {invalid_value=opt_val} ->
            match opt_val with
            | {~some} -> {invalid_value=Option.some(unbox(Option.some(some)))}
            | {none} -> {invalid_value=Option.none}
            end
          end
    internal_html(config, id, val, config.openable)

  do_open(config:CTable.Cell.config, id:string) : void =
    Dom.transform([#{id} <- internal_html(config, id, internal_parse(config, id), true)])

  do_close(config:CTable.Cell.config, id:string) : void =
    internal_set_value(config, id, internal_parse(config, id))

  set_value(config:CTable.Cell.config, id:string, val:'a) : void =
    internal_set_value(config, id, {value=unbox(Option.some(val))})

  set_invalid_value(config:CTable.Cell.config, id:string, val:option('a)) =
    match val with
    | {~some} -> internal_set_value(config, id, {invalid_value=Option.some(unbox(Option.some(some)))})
    | {none} -> internal_set_value(config, id, {invalid_value=Option.none})

  clear_value(config:CTable.Cell.config, id:string) : void =
     internal_set_value(config, id, {missing_value})

  parse(config:CTable.Cell.config, id:string) : IEditable.content('a) =
    match internal_parse(config, id) with
    | {~value} -> {value=box(value)}
    | {missing_value} -> {missing_value}
    | {invalid_value=opt_val} ->
      match opt_val with
      | {~some} -> {invalid_value=Option.some(box(some))}
      | {none} -> {invalid_value=Option.none}
      end
    end

}
