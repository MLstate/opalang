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
 * A configurable any value widget.
 * Using string value widget.
 *
 * @author Frederic Ye, 2010-2011
 * @author François-Régis Sinot, 2011 (refactoring)
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.3
 */

import stdlib.widgets.core
import stdlib.interactions
import stdlib.components.table

/**
 * {1 About this module}
 *
 * A configurable any value widget.
 *
 */

/**
 * {1 Types defined in this module}
 */

type WAnyValue.value('any) = 'any

type WAnyValue.parameters('any) = {
  init_state: IEditable.state
  manually_editable: bool
  on_change: WAnyValue.value('any) -> void
  on_edit: -> void
  on_validate: -> void
}

type WAnyValue.stylers = {
  base_stylers: IEditable.stylers
  incr: WStyler.styler
  decr: WStyler.styler
}

type WAnyValue.config('any) = {
  prefix_class: option(string);
  default_value : WAnyValue.value('any);
  parse : string -> option(WAnyValue.value('any));
  show : WAnyValue.value('any) -> string;
  non_editable_show : WAnyValue.value('any) -> option(xhtml);
  validator: WAnyValue.value('any) -> bool;
  incr: option((string, xhtml, (WAnyValue.value('any) -> WAnyValue.value('any))))
  decr: option((string, xhtml, (WAnyValue.value('any) -> WAnyValue.value('any))))
  stylers: WAnyValue.stylers
  show_buttons: bool // show_buttons is only used by +html
}

WAnyValue = {{

  /**
   * {1 Configuration}
   */

  default_config(default_value) = {
    prefix_class = none
    default_value = default_value
    parse(_) = none
    show(x) = "{x}"
    non_editable_show(_) = none
    validator(_) = true
    incr = none
    decr = none
    stylers = {
      base_stylers = IEditable.default_config.stylers
      incr = WStyler.empty
      decr = WStyler.empty
    }
    show_buttons = false
  } : WAnyValue.config('any)

  /**
   * {1 High-level interface}
   */

  /**
   * Constructs an anyvalue widget
   */
  html(config:WAnyValue.config('any), prefix_id:string, parameters:WAnyValue.parameters('any), value:IEditable.content(WAnyValue.value('any)), empty_text:string) =
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    config = { config with prefix_class = some(prefix_class) }
    editable_config = { IEditable.default_config with
      prefix_class = config.prefix_class
      validate_handles = [{newline}] // put in parameters
      check_handles = [{keyup}]
      cancel_handles = [{keyesc}] // put in parameters
      missing_value_text = empty_text
      stylers = config.stylers.base_stylers
    }
    editable_parameters = {
      show = show(config, prefix_id, _)
      edit = internal_edit(config, prefix_id, parameters.manually_editable, _)
      on_change = parameters.on_change(_)
      on_edit = parameters.on_edit
      on_validate = parameters.on_validate
      parse = _ -> match parse(config, prefix_id) with
                   | {~some} -> {value=some}
                   | {none} -> {invalid_value=Option.none}
      default_value = config.default_value
      manually_editable = parameters.manually_editable
    }
    <>
      {IEditable.html(editable_config, prefix_id, editable_parameters, parameters.init_state, value)}
      {if config.show_buttons then
         mk_a = Option.switch(
           (title, inc, what) -> <a title={title} onclick={_ -> oper(config, prefix_id, parameters, what)}>{inc}</a>,
           <></>, _)
         plus = mk_a(config.incr)
         minus = mk_a(config.decr)
         <>{plus}{minus}</>
       else <></>}
    </>

  /**
   * Display a non-editable value of the widget.
   */
  show(config:WAnyValue.config('any), prefix_id:string, init_value:WAnyValue.value('any)) : xhtml =
    <span id=#{anyvalue_id(prefix_id)}>
      {config.non_editable_show(init_value) ? <>{ config.show(init_value) }</> }
    </span> |> WStyler.add(config.stylers.base_stylers.show, _)

  /**
   * Returns the current value of the widget.
   */
  parse(config:WAnyValue.config('any), prefix_id:string) : option(WAnyValue.value('any)) =
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    str = if IEditable.is_editing(prefix_class, IEditable.editable_id(prefix_id))
      then Dom.get_value(#{anyvalue_id(prefix_id)})
      else Dom.get_text(#{anyvalue_id(prefix_id)})
    config.parse(str)

  /**
   * {1 Private functions}
   */

  /*
  * {2 ID/Class shortcuts}
  */

  @private
  gen_id(prefix_id:string, suffix:string) =
    "{prefix_id}_anyvalue{suffix}"

  @private
  anyvalue_id(prefix_id:string) =
    gen_id(prefix_id, "")

  /* Edit function */

  @private
  internal_edit(config:WAnyValue.config('any), prefix_id:string, manually_editable:bool, init_value:WAnyValue.value('any)) : xhtml =
    html = if manually_editable 
      then <input type="text" id=#{anyvalue_id(prefix_id)} value={config.show(init_value)}/>
      else <span id=#{anyvalue_id(prefix_id)}>{config.non_editable_show(init_value) ? <>{ config.show(init_value) }</>}</span>
    html |> WStyler.add(config.stylers.base_stylers.edit, _)

  edit(config:WAnyValue.config('any), prefix_id:string, init_value:WAnyValue.value('any)) : xhtml =
    internal_edit(config, prefix_id, true, init_value)

  @private
  oper(config:WAnyValue.config('any), prefix_id:string, parameters:WAnyValue.parameters('any), op:WAnyValue.value('any)->WAnyValue.value('any)) =
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    match parse(config, prefix_id) with
    | ~{some} ->
      new_val = op(some)
      if config.validator(new_val) then
        do if IEditable.is_editing(prefix_class, IEditable.editable_id(prefix_id)) then
             _ = Dom.set_value(#{anyvalue_id(prefix_id)}, config.show(new_val))
             void
           else
             Dom.transform([#{anyvalue_id(prefix_id)} <- config.non_editable_show(new_val) ? <>{config.show(new_val)}</>])
        parameters.on_change(new_val)
      else void
    | {none} -> void

}}

WAnyCell(anyvalue_config : WAnyValue.config('any), box : WAnyValue.value('any) -> 'a, unbox : option('a) -> WAnyValue.value('any)) : CTable.Cell.widget('a) =

  internal_parameters(config:CTable.Cell.config, id:string, state:IEditable.state) = {
    init_state = state
    manually_editable = config.openable
    on_change = val -> config.on_change(id, box(val))
    on_edit = -> config.on_open(id)
    on_validate = -> config.on_close(id)
  }

  internal_html(config:CTable.Cell.config, id:string, val:IEditable.content(WAnyValue.value('any)), state:IEditable.state) =
    internal_config = { 
      anyvalue_config with
      stylers.base_stylers.editable = config.style 
    }
    WAnyValue.html(internal_config, id, internal_parameters(config, id, state), val, "{unbox(none)}")
    |> WStyler.add(config.style, _)

  internal_set_value(config:CTable.Cell.config, id:string, val:IEditable.content(WAnyValue.value('any))) =
    Dom.transform([#{id} <- internal_html(config, id, val, {show})])

  internal_parse(_config:CTable.Cell.config, id:string) =
    IEditable.parse_content(id, WAnyValue.parse(anyvalue_config, _))

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
    internal_html(config, id, val, {show})

  do_open(config:CTable.Cell.config, id:string) : void =
    internal_set_value(config, id, internal_parse(config, id))

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
