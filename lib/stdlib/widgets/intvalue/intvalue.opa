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
 * A configurable int value widget.
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

/**
 * {1 Types defined in this module}
 */

type WIntValue.value = int

type WIntValue.parameters = WAnyValue.parameters(int)

type WIntValue.stylers = WAnyValue.stylers

type WIntValue.config = {
  prefix_class: option(string);
  range: (option(int), option(int))
  step: int
  incr_text: option((string, xhtml))
  decr_text: option((string, xhtml))
  stylers: WIntValue.stylers
  show_buttons: bool
}

WIntValue = {{

  /**
   * {1 Configuration}
   */

  default_config = {
    prefix_class = none
    range = (none, none)
    step = 1
    incr_text = some(("Increase", Xhtml.of_string("+")))
    decr_text = some(("Decrease", Xhtml.of_string("-")))
    stylers = {
      base_stylers = IEditable.default_config.stylers
      incr = WStyler.empty
      decr = WStyler.empty
    }
    show_buttons = true
  } : WIntValue.config

  default_config_with_css(css_prefix:string) = { default_config with
    prefix_class = some(css_prefix)
  } : WIntValue.config

  /**
   * {1 High-level interface}
   */

  html(config:WIntValue.config, prefix_id:string, parameters:WIntValue.parameters, value:IEditable.content(WIntValue.value), empty_text:string) =
    WAnyValue.html(to_anyvalue_config(config), prefix_id, parameters, value, empty_text)

  // Display function

  show(config:WIntValue.config, prefix_id:string, init_value:WIntValue.value) : xhtml =
    WAnyValue.show(to_anyvalue_config(config), prefix_id, init_value)

  // Edit function

  edit(config:WIntValue.config, prefix_id:string, init_value:WIntValue.value) =
    WAnyValue.edit(to_anyvalue_config(config), prefix_id, init_value)

  parse(config:WIntValue.config, prefix_id:string) : option(WIntValue.value) =
    WAnyValue.parse(to_anyvalue_config(config), prefix_id)


  /**
   * {1 Private functions}
   */

  @private int_incr(x) = x + 1
  @private int_decr(x) = x - 1

  @private
  to_anyvalue_config(config:WIntValue.config) : WAnyValue.config(int) = {
    WAnyValue.default_config(0) with
    prefix_class = config.prefix_class
    parse = Parser.int
    show = string_of_int
    validator = int_validator(config, _)
    incr = Option.map((s, x) -> (s, x, int_incr), config.incr_text)
    decr = Option.map((s, x) -> (s, x, int_decr), config.decr_text)
    stylers = config.stylers
    show_buttons = config.show_buttons
  } : WAnyValue.config(int)

  // Validator

  @private
  int_validator(config:WIntValue.config, value:WIntValue.value) =
      (lower, upper) = config.range
      Option.switch(_ <= value, true, lower) && Option.switch(_ >= value, true, upper)

}}

WIntCell(box : WIntValue.value -> 'a, unbox : option('a) -> WIntValue.value) : CTable.Cell.widget('a) =
  anyvalue_config : WAnyValue.config(int) =
    { WAnyValue.default_config(0) with
      parse = Parser.int
      show = string_of_int
      validator(_) = true
      incr = none
      decr = none
    }
  WAnyCell(anyvalue_config, box, unbox)
