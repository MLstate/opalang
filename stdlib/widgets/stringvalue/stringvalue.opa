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
import stdlib.interactions.editable
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
