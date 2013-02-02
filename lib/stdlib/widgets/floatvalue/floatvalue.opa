/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * A configurable float value widget.
 * Using string value widget.
 *
 * @author Frederic Ye, 2010-2011
 * @author François-Régis Sinot, 2011 (refactoring)
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.3
 */

import stdlib.web.client
import stdlib.widgets.{core, anyvalue}
import stdlib.interactions.editable
import stdlib.components.table

type WFloatValue.value = float

type WFloatValue.parameters = WAnyValue.parameters(float)

type WFloatValue.stylers = WAnyValue.stylers

type WFloatValue.config = {
  prefix_class: option(string);
  range: (option(float), option(float))
  step: float
  incr_text: option((string, xhtml))
  decr_text: option((string, xhtml))
  stylers: WFloatValue.stylers
  show_buttons: bool
}

WFloatValue = {{

  /*
   * {1 Configuration}
   */

  default_config = {
    prefix_class = none
    range = (none, none)
    step = 1.
    incr_text = some(("Increase", Xhtml.of_string("+")))
    decr_text = some(("Decrease", Xhtml.of_string("-")))
    stylers = {
      base_stylers = IEditable.default_config.stylers
      incr = WStyler.empty
      decr = WStyler.empty
    }
    show_buttons = true
  } : WFloatValue.config

  default_config_with_css(css_prefix:string) = { default_config with
    prefix_class = some(css_prefix)
  } : WFloatValue.config

  @private float_incr(x) = x + 1.
  @private float_decr(x) = x - 1.

  @private
  to_anyvalue_config(config:WFloatValue.config) : WAnyValue.config(float) = {
    WAnyValue.default_config(0.) with
    prefix_class = config.prefix_class
    parse = Parser.float
    show = string_of_float
    validator = float_validator(config, _)
    incr = Option.map((s, x) -> (s, x, float_incr), config.incr_text)
    decr = Option.map((s, x) -> (s, x, float_decr), config.decr_text)
    stylers = config.stylers
    show_buttons = config.show_buttons
  } : WAnyValue.config(float)

  // Validator

  @private
  float_validator(config:WFloatValue.config, value:WFloatValue.value) =
      (lower, upper) = config.range
      Option.switch(_ <= value, true, lower) && Option.switch(_ >= value, true, upper)

  // Parse function

  parse(config:WFloatValue.config, prefix_id:string) : option(WFloatValue.value) =
    WAnyValue.parse(to_anyvalue_config(config), prefix_id)

  // Display function

  show(config:WFloatValue.config, prefix_id:string, init_value:WFloatValue.value) : xhtml =
    WAnyValue.show(to_anyvalue_config(config), prefix_id, init_value)

  // Edit function

  edit(config:WFloatValue.config, prefix_id:string, init_value:WFloatValue.value) =
    WAnyValue.edit(to_anyvalue_config(config), prefix_id, init_value)

  html(config:WFloatValue.config, prefix_id:string, parameters:WFloatValue.parameters, value:IEditable.content(WFloatValue.value), empty_text:string) =
    WAnyValue.html(to_anyvalue_config(config), prefix_id, parameters, value, empty_text)

}}

WFloatCell(box : WFloatValue.value -> 'a, unbox : option('a) -> WFloatValue.value) : CTable.Cell.widget('a) =
  anyvalue_config : WAnyValue.config(float) =
    { WAnyValue.default_config(0.) with
      parse = Parser.float
      show = string_of_float
      validator(_) = true
      incr = none
      decr = none
    } : WAnyValue.config(float)
  WAnyCell(anyvalue_config, box, unbox)
