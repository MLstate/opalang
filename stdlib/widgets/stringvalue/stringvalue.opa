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
 * A configurable string value widget.
 *
 * @author Frederic Ye, 2010
 * @author François-Régis Sinot, 2011 (refactoring)
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.3
 */

/*
 * {1 TODO (ideas) }
 *
 * - Handle placeholders
 */

import stdlib.widgets.{core, anyvalue}
import stdlib.interactions
import stdlib.components.table

/**
 * {1 Types defined in this module}
 */

type WStringValue.value = string

type WStringValue.parameters = WAnyValue.parameters(string)

type WStringValue.config = {
  prefix_class: option(string)
  validator: WStringValue.value -> bool // validator to use to validate the string value
  stylers: IEditable.stylers
}

WStringValue = {{

  /**
   * {1 Configuration}
   */

  default_config = {
    prefix_class = none
    validator = _value -> true
    stylers = IEditable.default_config.stylers
  } : WStringValue.config

  default_config_with_css(css_prefix:string) = { default_config with
    prefix_class = some(css_prefix)
  } : WStringValue.config

  /**
   * {1 High-level interface}
   */

  // Interactive display

  html(config:WStringValue.config, prefix_id:string, parameters:WStringValue.parameters, value:IEditable.content(WStringValue.value), empty_text:string) =
    WAnyValue.html(to_anyvalue_config(config), prefix_id, parameters, value, empty_text)

  show(config:WStringValue.config, prefix_id:string, init_value:WStringValue.value) : xhtml =
    WAnyValue.show(to_anyvalue_config(config), prefix_id, init_value)

  // Edit function

  edit(config:WStringValue.config, prefix_id:string, init_value:WStringValue.value) : xhtml =
    WAnyValue.edit(to_anyvalue_config(config), prefix_id, init_value)

  // Parse function

  parse(config:WStringValue.config, prefix_id:string) : option(WStringValue.value) =
    WAnyValue.parse(to_anyvalue_config(config), prefix_id)

/**
 * {1 Private functions}
 */

  @private
  to_anyvalue_config(config:WStringValue.config) : WAnyValue.config(string) = {
    WAnyValue.default_config("") with
    prefix_class = config.prefix_class
    parse(s : string) = some(s)
    show(s : string) = s
    validator = config.validator
    incr = none
    decr = none
    stylers = { base_stylers = config.stylers; incr = WStyler.empty; decr = WStyler.empty }
  } : WAnyValue.config(string) 

}}

WStringCell(box : WStringValue.value -> 'a, unbox : option('a) -> WStringValue.value) : CTable.Cell.widget('a) =
  anyvalue_config : WAnyValue.config(string) =
    { WAnyValue.default_config("") with
      parse(s : string) = some(s)
      show(s : string) = s
      validator(_) = true
      incr = none
      decr = none
    }
  WAnyCell(anyvalue_config, box, unbox)
