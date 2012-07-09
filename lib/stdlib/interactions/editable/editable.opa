/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * A configurable editable interaction.
 *
 * @author Frederic Ye, 2010-2011
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.2
 */

/*
 * {1 Notes}
 *
 * <valid> and <invalid> stylers are unused for the moment,
 * those may be used later with WStyler.set_dom
 */

import stdlib.widgets.core

type IEditable.state = { show } / { edit }

type IEditable.content('a) =
    { value : 'a }
  / { invalid_value : option('a) }
  / { missing_value }

type IEditable.stylers = {
  editable: WStyler.styler // Global widget styler
  show: WStyler.styler // Styler to appply in show mode
  edit: WStyler.styler // Styler to apply in edit mode
  valid: WStyler.styler // Styler to apply for a valid value
  invalid: WStyler.styler // Styler to apply for an invalid value
  missing: WStyler.styler // Styler to apply for a missing value
}

// Less likely to me modified
type IEditable.parameters('a) = {
  show: 'a -> xhtml
  edit: 'a -> xhtml
  on_change: 'a -> void
  on_edit: -> void
  on_validate: -> void
  parse: string -> IEditable.content('a)
  default_value: 'a
  manually_editable: bool
}

// More likely to be modified
type IEditable.config = {
  prefix_class: option(string)
  edit_handles: list(Dom.event.kind)
  validate_handles: list(Dom.event.kind)
  check_handles: list(Dom.event.kind)
  cancel_handles: list(Dom.event.kind)
  missing_value_text: string
  stylers: IEditable.stylers
}

IEditable = {{

  /*
  * {1 ID shortcuts}
  */

  gen_id(prefix_id:string, suffix:string) =
    "{prefix_id}_editable{suffix}"

  editable_id(prefix_id:string) =
    gen_id(prefix_id, "")

  /*
  * {1 Class shortcuts}
  */

  gen_class(prefix_class:string, suffix:string) =
    "{prefix_class}_editable{suffix}"

  editing_class(prefix_class) =
    gen_class(prefix_class, "_editing")

  is_editing(prefix_class, id) =
    Dom.has_class(#{id}, editing_class(prefix_class))

  valid_class(prefix_class) =
    gen_class(prefix_class, "_valid")

  is_valid(prefix_class, id) =
    Dom.has_class(#{id}, valid_class(prefix_class))

  invalid_class(prefix_class) =
    gen_class(prefix_class, "_invalid")

  is_invalid(prefix_class, id) =
    Dom.has_class(#{id}, invalid_class(prefix_class))

  missing_class(prefix_class:string) =
    gen_class(prefix_class, "_missing")

  is_missing(prefix_class, id) =
    Dom.has_class(#{id}, missing_class(prefix_class))

  // Config

  default_config = {
    prefix_class = none
    edit_handles = [{click}, {dblclick}]
    validate_handles = [{newline}] // {blur}
    check_handles = []
    cancel_handles = [{keyesc}]
    missing_value_text = ""
    stylers = {
      editable = WStyler.empty
      show = WStyler.empty
      edit = WStyler.empty
      valid = WStyler.empty
      invalid = WStyler.empty
      missing = WStyler.empty
    }
  } : IEditable.config

  default_config_with_css(css_prefix:string) = { default_config with
    prefix_class = some(css_prefix)
  } : IEditable.config

  display_value(config:IEditable.config, prefix_id:string, parameters:IEditable.parameters, content:IEditable.content) =
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    edit_handles = List.fold(handle, acc ->
                     (handle, edit_action(config, prefix_id, parameters, content, content, _)) +> acc
                     , config.edit_handles, [])
    <span id=#{editable_id(prefix_id)}>{
      match content with
      | {~value} -> parameters.show(value)
      | {~invalid_value} ->
        match invalid_value with
        | {~some} -> parameters.show(some)
        | {none} -> <></>
        end
      | { missing_value } -> <>{config.missing_value_text}</>
      end |> WStyler.add(config.stylers.show, _)
    }</span> |> WStyler.add(config.stylers.editable, _)
    |> (if parameters.manually_editable then WCore.add_binds(edit_handles, _) else Xhtml.to_xhtml(_))
    |> (match content with
        | {missing_value} -> WStyler.add(WStyler.make_class([missing_class(prefix_class)]), _)
        | {invalid_value=_}  -> WStyler.add(WStyler.make_class([invalid_class(prefix_class)]), _)
        | _ -> WStyler.add(WStyler.make_class([valid_class(prefix_class)]), _))

  edit_value(config:IEditable.config, prefix_id:string, parameters:IEditable.parameters, content:IEditable.content, orig_content:IEditable.content) =
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    validate_handles = List.fold(handle, acc ->
      (handle, validate_action(config, prefix_id, parameters, true, _)) +> acc
    , config.validate_handles, [])
    check_handles = List.fold(handle, acc ->
      (handle, validate_action(config, prefix_id, parameters, false, _)) +> acc
    , config.check_handles, [])
    cancel_handles = List.fold(handle, acc ->
      (handle, cancel_action(config, prefix_id, parameters, orig_content, _)) +> acc
    , config.cancel_handles, [])
    binds = ({ready}, focus_action(prefix_id, _)) +> validate_handles ++ check_handles ++ cancel_handles
    <span class={[editing_class(prefix_class)]} id=#{editable_id(prefix_id)}>{
      match content with
      | {~value} -> parameters.edit(value)
      | {~invalid_value} ->
        match invalid_value with
        | {~some} -> parameters.edit(some)
        | {none} -> parameters.edit(parameters.default_value)
        end
      | {missing_value} -> parameters.edit(parameters.default_value)
      end |> WStyler.add(config.stylers.edit, _) |> WCore.add_binds(binds, _)
    }</span> |> WStyler.add(config.stylers.editable, _)
    |> (match content with
        | {missing_value} -> WStyler.add(WStyler.make_class([missing_class(prefix_class)]), _) // FIXME do this if not keyup : Xhtml.to_xhtml(_) //
        | {invalid_value=_}  -> WStyler.add(WStyler.make_class([invalid_class(prefix_class)]), _)
        | _ -> WStyler.add(WStyler.make_class([valid_class(prefix_class)]), _))

  edit_action(config:IEditable.config, prefix_id:string, parameters:IEditable.parameters, content:IEditable.content, orig_content:IEditable.content, _) =
    (val, orig_val) = if is_missing(prefix_id, editable_id(prefix_id)) then (content, orig_content)
      else match parameters.parse(prefix_id) with
      | {~value} -> ({value=value}, {value=value})
      | _ -> (content, orig_content)
    _ = Dom.put_replace(#{editable_id(prefix_id)}, Dom.of_xhtml(edit_value(config, prefix_id, parameters, val, orig_val)))
    parameters.on_edit()

  replace_value(config:IEditable.config, prefix_id:string, parameters:IEditable.parameters, content:IEditable.content) =
    xhtml = display_value(config, prefix_id, parameters, content)
    // Add styler <valid> ? maybe in display_value ?
    _ = Dom.put_replace(#{editable_id(prefix_id)}, Dom.of_xhtml(xhtml))
    void

  validate_value(config:IEditable.config, prefix_id:string, parameters:IEditable.parameters, replace:bool) =
    prefix_class = WCore.compute_prefix_class(config, prefix_id)
    dom = #{editable_id(prefix_id)}
    match parameters.parse(prefix_id) with
    | {~value} ->
      do Dom.remove_class(dom, missing_class(prefix_class))
      do Dom.remove_class(dom, invalid_class(prefix_class))
      do Dom.add_class(dom, valid_class(prefix_class))
      if replace then
        do replace_value(config, prefix_id, parameters, {value=value})
        parameters.on_change(value)
    | {invalid_value=_} ->
      do Dom.remove_class(dom, missing_class(prefix_class))
      do Dom.remove_class(dom, valid_class(prefix_class))
      do Dom.add_class(dom, invalid_class(prefix_class))
      // Re-call edit_value with styler <invalid> ?
      // Find a way to apply a styler without calling edit_value...
      void
    | _ -> void

  validate_action(config:IEditable.config, prefix_id:string, parameters:IEditable.parameters, replace:bool, _) =
    do validate_value(config, prefix_id, parameters, replace)
    if replace then
      parameters.on_validate()

  cancel_action(config:IEditable.config, prefix_id:string, parameters:IEditable.parameters, content:IEditable.content, _) =
    do replace_value(config, prefix_id, parameters, content)
    parameters.on_validate()

  focus_action(prefix_id:string, _) =
    // FIXME: we suppose the son is an input...
    _ = Dom.give_focus(Dom.select_raw_unsafe("#{editable_id(prefix_id)} > input:first"))
    void

  parse_content(prefix_id:string, parse_fun:string->option('a)) =
    if is_missing(prefix_id, editable_id(prefix_id)) then
      {missing_value}
    else if is_invalid(prefix_id, editable_id(prefix_id)) then
      {invalid_value=Option.none}
    else
      match parse_fun(prefix_id) with
      | {~some} -> {value=some}
      | {none} -> {invalid_value=Option.none}

  html(config:IEditable.config, prefix_id:string, parameters:IEditable.parameters, state:IEditable.state, content:IEditable.content) =
    match state with
    | {show} -> display_value(config, prefix_id, parameters, content)
    | {edit} -> edit_value(config, prefix_id, parameters, content, content)

}}
