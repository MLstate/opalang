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

package stdlib.widgets.formbuilder

import stdlib.web.mail
import stdlib.upload
import stdlib.widgets.core

/* FIXME For now form fields should have unique names within a page. We could
         try to use selection within a form, but then we have to figure out
         how to transfer form id to equals_validator */
// FIXME Change form field id -> name

/**
 * @author Adam Koprowski
 * @category widget
 * @stability slowly stabilizing, the API may still change slightly
 *
 *
 * {1 About this module}
 *
 * This module allows easy creation of web forms, including support
 * for validation.
**/

type WFormBuilder.passwd_validator_spec =
  { min_length : int
  ; force_uppercase : bool
  ; force_digit : bool
  ; force_special_char : bool

  ; error_msg_shows_all_reqs : bool
  ; error_msg_emph : xhtml -> xhtml
  ; error_formatter : list(xhtml) -> xhtml
  ; min_length_err_msg : int -> xhtml
  ; force_uppercase_err_msg : xhtml
  ; force_digit_err_msg : xhtml
  ; force_special_char_err_msg : xhtml
  }

type WFormBuilder.field_value('ty) =
  { no_value } / { conversion_error : xhtml } / { value : 'ty }

type WFormBuilder.field_accessor('ty) =
  WFormBuilder.field_data -> WFormBuilder.field_value('ty)

type WFormBuilder.field_renderer('ty) =
  { data : WFormBuilder.field_rendering_data; initial_value : option('ty) } -> xhtml

type WFormBuilder.field_validator('ty) =
  'ty -> outcome('ty, xhtml)

type WFormBuilder.field_converter('ty) =
  { render : WFormBuilder.field_renderer('ty)
  ; accessor : WFormBuilder.field_accessor('ty)
  }

type WFormBuilder.field_data =
  { id : string
  ; label : string
  ; hint : option(xhtml)
  ; optionality : {optional} / {required : xhtml}
  }

type WFormBuilder.field('ty) =
  { data : WFormBuilder.field_data
  ; initial_value : option('ty)
  ; converter : WFormBuilder.field_converter('ty)
  ; validator : WFormBuilder.field_validator('ty)
  }

type WFormBuilder.style =
  { field_style : bool -> WStyler.styler
  ; label_style : bool -> WStyler.styler
  ; input_style : bool -> WStyler.styler
  ; hint_style : WStyler.styler
  ; error_style : WStyler.styler
  ; non_required_style : WStyler.styler
  ; required_style : WStyler.styler
  ; hint_elt_type : {inline} / {block}
  ; error_elt_type : {inline} / {block}
  ; fade_duration : Dom.Effect.duration
  }

@abstract
type WFormBuilder.form_data =
    { SimpleForm }
  / { FileUploadForm : Upload.form_data }

type WFormBuilder.field_ids =
  { field_id : string
  ; input_id : string
  ; label_id : string
  ; hint_id : string
  ; error_id : string
  }

type WFormBuilder.field_rendering_data =
  { ids : WFormBuilder.field_ids
  ; style : WFormBuilder.style
  ; data : WFormBuilder.field_data
  }

type WFormBuilder.field_builder =
  { mk_label : WFormBuilder.field_rendering_data -> xhtml
  ; mk_input : xhtml, WFormBuilder.field_rendering_data -> xhtml
  ; mk_hint : WFormBuilder.field_rendering_data -> xhtml
  ; mk_err : WFormBuilder.field_rendering_data -> xhtml
  ; mk_field : {label:xhtml input:xhtml hint:xhtml err:xhtml data:WFormBuilder.field_rendering_data} -> xhtml
  }

WFormBuilder =
{{

  /** {1 Styling} */

  empty_style : WFormBuilder.style =
    { field_style(_) = WStyler.empty
    ; label_style(_) = WStyler.empty
    ; input_style(_) = WStyler.empty
    ; hint_style = WStyler.empty
    ; error_style = WStyler.empty
    ; non_required_style = WStyler.empty
    ; required_style = WStyler.empty
    ; fade_duration = {default}
    ; error_elt_type = {block}
    ; hint_elt_type = {block}
    }

  /** {1 Validators} */

  empty_validator : WFormBuilder.field_validator('ty) =
    input -> {success=input}

  empty_passwd_validator_spec : WFormBuilder.passwd_validator_spec =
    { min_length = 6
    ; force_uppercase = false
    ; force_digit = true
    ; force_special_char = false

    ; error_msg_shows_all_reqs = false
    ; error_msg_emph(error) = <b>{error}</>
    ; error_formatter(errors) =
        errors_xml = XmlConvert.of_list_using(<></>, <></>, <>, </>, errors)
        <>Your password should contain at least: {errors_xml}</>
    ; min_length_err_msg(i) = <>{i} characters</>
    ; force_uppercase_err_msg = <>one upper-case letter</>
    ; force_digit_err_msg = <>one digit</>
    ; force_special_char_err_msg = <>one special character</>
    }

  password_validator(spec : WFormBuilder.passwd_validator_spec)
    : WFormBuilder.field_validator(string) =
    input ->
      length_ok = String.length(input) >= spec.min_length
      length_msg = spec.min_length_err_msg(spec.min_length)
      uppercase_ok = if spec.force_uppercase then String.to_lower(input) != input else true
      uppercase_msg = spec.force_uppercase_err_msg
      rec find_digit = parser
      | [0-9] .* -> true
      | . v=find_digit -> v
      | {Rule.succeed} -> false
      digit_ok = if spec.force_digit then Parser.parse(find_digit, input) else true
      digit_msg = spec.force_digit_err_msg
      rec find_special = parser
      | [a-zA-Z0-9] v=find_special -> v
      | .* -> true
      | {Rule.succeed} -> false
      special_ok = if spec.force_special_char then Parser.parse(find_special, input) else true
      special_msg = spec.force_special_char_err_msg
      rec aux(l, ok, errs) =
        match l with
        | [] ->
            if ok then
              {success=input}
            else
              {failure=spec.error_formatter(errs)}
        | [(this_ok, err_msg) | xs] ->
            if this_ok then
              new_errs =
                if spec.error_msg_shows_all_reqs then
                  [err_msg | errs]
                else
                  errs
              aux(xs, ok, new_errs)
            else
              new_errs = [spec.error_msg_emph(err_msg) | errs]
              aux(xs, false, new_errs)
      aux([ (uppercase_ok, uppercase_msg)
          , (digit_ok, digit_msg)
          , (special_ok, special_msg)
          , (length_ok, length_msg)
          ], true, [])

  equals_validator(fld : WFormBuilder.field('ty), error_msg : xhtml) : WFormBuilder.field_validator('ty) =
    input ->
      if {value=input} == get_field_value(fld) then
        {success = input}
      else
        {failure = error_msg}

  merge_validators(vs : list(WFormBuilder.field_validator)) : WFormBuilder.field_validator =
    rec aux(input) =
      | [] -> { success=input }
      | [v | vs] ->
          match v(input) with
          | {success=input} -> aux(input)(vs)
          | res -> res
    aux(_)(vs)

  /** {1 Fields interfaces} */

  string_accessor(data) =
    v = get_val_string("{data.id}_input")
    if String.is_empty(v) then
      {no_value}
    else
      {value=v}

  string_converter(data, wrong_data, conv) =
    match string_accessor(data) with
    | {no_value} as e
    | {conversion_error=_} as e -> e
    | {value=input} ->
        match conv(input) with
        | {none} -> {conversion_error = wrong_data}
        | {some=v} -> {value = v}

  selection_accessor(data, options, to_id, none_selected) =
    match string_accessor(data) with
    | {no_value} as e
    | {conversion_error=_} as e -> e
    | {value=input} ->
         check_opt(o) = to_id(o) == input
         match List.find(check_opt, options) with
         | {none} -> {conversion_error=none_selected}
         | {some=opt} -> {value=opt}

  email_accessor = string_converter(_, _, Email.of_string_opt)

  uri_accessor = string_converter(_, _, Uri.of_string)

  render_text_input(~{data initial_value}, f) =
    add_initial_value(
      <input type="text" id={data.ids.input_id} />,
      "value", Option.map(f, initial_value)
    )

  render_text_area(~{data initial_value}, size) =
    // FIXME, resize:none should be in css{...}
    do Log.error("FB", "render_text_area")
    <textarea onclick={_ -> void} style="resize: none;" type="text" id={data.ids.input_id}
      rows={size.rows} cols={size.cols}>
      {initial_value}
    </>

  render_passwd_input(~{data ...}) =
    <input type="password" id={data.ids.input_id} />

  render_combobox(~{data initial_value}, options, to_id, to_label) =
    mk_option(opt) =
      o =
        <option value={to_id(opt)} id={data.ids.input_id}>
          {to_label(opt)}
        </>
      if initial_value == some(opt) then
        Xhtml.add_attribute("selected", "true", o)
      else
        o
    <select>
      {List.map(mk_option, options)}
    </>

  /** {1 Field converters} */

  text_field : WFormBuilder.field_converter(string) =
    { render = render_text_input(_, identity)
    ; accessor = string_accessor
    }

  passwd_field : WFormBuilder.field_converter(string) =
    { render = render_passwd_input
    ; accessor = string_accessor
    }

  email_field(wrong_email : xhtml) : WFormBuilder.field_converter(Email.email) =
    { render = render_text_input(_, Email.to_string)
    ; accessor = email_accessor(_, wrong_email)
    }

  desc_field_with(size : {rows:int cols:int}) : WFormBuilder.field_converter(string) =
    { render = render_text_area(_, size)
    ; accessor = string_accessor
    }

  desc_field : WFormBuilder.field_converter(string) =
    desc_field_with({rows=3 cols=40})

  uri_field(wrong_uri : xhtml) : WFormBuilder.field_converter(Uri.uri) =
    { render = render_text_input(_, Uri.to_string)
    ; accessor = uri_accessor(_, wrong_uri)
    }

  select_combobox_field(options : list('a), to_id : 'a -> string,
    to_label : 'a -> xhtml, none_selected : xhtml) : WFormBuilder.field_converter('a) =
    { render = render_combobox(_, options, to_id, to_label)
    ; accessor = selection_accessor(_, options, to_id, none_selected)
    }

  /** {1 Fields/form construction} */

  field_data(label) =
    id = Dom.fresh_id()
    {~label
     ~id
     optionality={optional}
     hint=none
    }

  mk_field(label, converter) : WFormBuilder.field =
    { data=field_data(label)
    ; ~converter
    ; initial_value=none
    ; validator=empty_validator
    }

  /** {1 Extending fields} */

  add_validator(field : WFormBuilder.field, v : WFormBuilder.field_validator)
    : WFormBuilder.field =
    { field with
        validator=merge_validators([field.validator, v])
    }

  add_hint(field : WFormBuilder.field, hint : xhtml) : WFormBuilder.field =
    { field with data.hint=some(hint) }

  set_id(field : WFormBuilder.field, id : string) : WFormBuilder.field =
    {field with data.id=id}

  set_initial_value(field : WFormBuilder.field('ty), initial_value : 'ty) : WFormBuilder.field('ty) =
    {field with initial_value=some(initial_value)}

  make_required(field : WFormBuilder.field, err_msg : xhtml) : WFormBuilder.field =
    {field with data.optionality={required = err_msg}}

  make_required_with_default_msg(field : WFormBuilder.field) : WFormBuilder.field =
    make_required(field, <>Please provide a value</>)

  /** {1 Form/fields rendering} */

  default_field_builder : WFormBuilder.field_builder =
    {
      mk_label({data=~{optionality label ...} ids=~{label_id input_id ...} style=s}) =
        req =
          match optionality with
          | {optional} -> <span></> |> add_style(s.non_required_style)
          | {required=_} -> <span>*</> |> add_style(s.required_style)
        <label id={label_id} for={input_id}>
          {label}
          {req}
        </> |> add_style(s.label_style(true))

      mk_input(input_tag, {style=s ...}) =
        input_tag |> add_style(s.input_style(true))

      mk_hint(~{data ids style=s}) =
        match data.hint with
        | {none} -> <></>
        | {some=hint} ->
            <span id={ids.hint_id}>{hint}</>
            |> add_style(s.hint_style)

      mk_err(~{ids style=s ...}) =
        <span id={ids.error_id} />
        |> add_style(s.error_style)

      mk_field(~{label input hint err data}) =
        <div id={data.ids.field_id}>
          {label}
          {input}
          {hint}
          {err}
        </>
        |> add_style(data.style.field_style(true))

     }

  field_html(field : WFormBuilder.field('ty), builder : WFormBuilder.field_builder, style : WFormBuilder.style) : xhtml =
    hide(tag) = WStyler.add({style=css{display: none}}, tag)
    ~{data initial_value converter ...} = field
    ids = build_ids(data.id)
    rdata = ~{data ids style}
    label_xhtml = builder.mk_label(rdata)
    onblur(_) =
      _ = do_validate(field, style, ids)
      do hide_hint(style, ids)
      void
    onfocus(_) =
      do show_hint(style, ids)
      void
    bindings =
      [ ({blur}, onblur)
      , ({focus}, onfocus)
      ]
    input_xhtml = converter.render(~{data=rdata initial_value})
               |> WCore.add_binds(bindings, _)
               |> builder.mk_input(_, rdata)
    hint_xhtml = builder.mk_hint(rdata) |> hide
    err_xhtml = builder.mk_err(rdata) |> hide
    builder.mk_field({label=label_xhtml input=input_xhtml hint=hint_xhtml err=err_xhtml data=rdata})

  form_html( form_id : string
           , form_type : {Basic} / {Normal}
           , form_body : xhtml
           , process_form : WFormBuilder.form_data -> void
           ) : xhtml =
    match form_type
    | {Basic} ->
        <form id={form_id} action="#" onsubmit={_ -> process_form({SimpleForm})}
          method="get" options:onsubmit="prevent_default">
          {form_body}
        </form>
    | {Normal} ->
        process(data) =
          do Scheduler.push( -> process_form({FileUploadForm=data}))
          void
        config = {Upload.default_config() with
                    ~form_body ~process form_id=form_id}
        Upload.html(config)

  focus_on(field : WFormBuilder.field) : void =
    Dom.give_focus(#{build_ids(field.data.id).input_id})

  submit_action(form_id : string) : (Dom.event -> void) =
    _ ->
       // submit the form
      Dom.trigger(#{form_id}, {submit})

  get_field_value(field) =
    field.converter.accessor(field.data)

  get_field_label(field) =
    field.data.label

  /* ---------- Private functions ---------- */

  @private add_style(style) = WStyler.add(style, _)

  @private animate(style, dom, e) =
    _ = Dom.Effect.with_duration(style.fade_duration, e)
     |> Dom.transition(dom, _)
    void

  @private set_block_type(fld, style) =
    do Dom.set_style(fld,
          match style with
          | {inline} -> css {display: inline}
          | {block} -> css {display: block}
       )
    void

  @private do_validate(field : WFormBuilder.field('ty),
    style : WFormBuilder.style, ids) : bool =
    err_fld = #{ids.error_id}
    set_styles(ok) =
      go(id, style) = WStyler.set_dom(style(ok), id)
      do go(ids.label_id, style.label_style)
      do go(ids.field_id, style.field_style)
      do go(ids.input_id, style.input_style)
      void
    input = get_field_value(field)
    val_outcome =
      match (input, field.data.optionality) with
      | ({conversion_error=err}, _) -> {failure=err}
      | ({no_value}, {required=req_msg}) -> {failure=req_msg}
      | ({no_value}, {optional}) -> {success=void}
      | ({value=v}, _) ->
          match field.validator(v) with
          | {success=_} -> {success=void}
          | {failure=err} -> {failure=err}
    match val_outcome with
    | {success=_} ->
        do animate(style, err_fld, Dom.Effect.fade_out())
        do set_styles(true)
        true
    | {failure=err} ->
        do Dom.transform([{err_fld} <- err])
        do animate(style, err_fld, Dom.Effect.fade_in())
        do set_styles(false)
        do set_block_type(err_fld, style.error_elt_type)
        false

  @private show_hint(style, ids) : void =
    hint_fld = #{ids.hint_id}
    do animate(style, hint_fld, Dom.Effect.fade_in())
    do set_block_type(hint_fld, style.hint_elt_type)
    void

  @private hide_hint(style, ids) : void =
    animate(style, #{ids.hint_id}, Dom.Effect.fade_out())

  @private get_val_string(id : string) =
    Dom.get_value(#{id})

  @private get_form_val_string(id : string, data : WFormBuilder.form_data) : string =
    match data with
    | { SimpleForm } -> get_val_string(id)
    | { FileUploadForm=data } -> Map.get(id, data.form_fields) ? ""

  @private get_file_upload_value(id : string, data : WFormBuilder.form_data) : option(Upload.file) =
    match data with
    | { SimpleForm } -> none
    | { FileUploadForm=data } -> Map.get(id, data.uploaded_files)

  @private build_ids(id) : WFormBuilder.field_ids =
    { field_id = "{id}_field"
    ; label_id = "{id}_label"
    ; input_id = "{id}_input"
    ; hint_id = "{id}_hint"
    ; error_id = "{id}_err"
    }

  @private add_initial_value(tag, attr, initial_value) =
    match initial_value with
    | {none} -> tag
    | {some=iv} -> Xhtml.add_attribute(attr, iv, tag)

}}
