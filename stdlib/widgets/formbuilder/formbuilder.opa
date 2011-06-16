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


import stdlib.web.mail
import stdlib.widgets.{core, upload}

/* FIXME For now form fields should have unique names within a page. We could
         try to use selection within a form, but then we have to figure out
         how to transfer form id to equals_validator */
// FIXME Change form field id -> name

/**
 * @author Adam Koprowski
 * @category widget
 * @stability experimental [WIP, waiting for feature requests & bug reports]
 *
 *
 * {1 About this module}
 *
 * This module allows easy creation of web forms, including support
 * for validation.
**/

type WFormBuilder.validator =
  string -> outcome(string, xhtml)

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

type WFormBuilder.field =
  { id : string
  ; label : string
  ; optionality : { required } / { optional }
  ; field_type : { email } / { text } / { passwd } / { desc : { cols: int rows: int} } / { upload }
  ; initial_value : string
  ; validator : WFormBuilder.validator
  ; hint : option(xhtml)
  }

type WFormBuilder.element = { field : WFormBuilder.field } / { fragment : xhtml }

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

type WFormBuilder.specification =
  { form_id : string
  ; elts : list(WFormBuilder.element)
  ; style : WFormBuilder.style
  }

@abstract
type WFormBuilder.form_data =
    { SimpleForm }
  / { FileUploadForm : Upload.form_data }

WFormBuilder =

  field_id(id) = "{id}_field"
  label_id(id) = "{id}_label"
  input_id(id) = "{id}_input"
  hint_id(id) = "{id}_hint"
  error_id(id) = "{id}_err"

{{

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

  create_specification( form_id : string
                      , elts : list(WFormBuilder.element)
                      ) : WFormBuilder.specification =
    {~form_id ~elts style=empty_style}

  empty_validator : WFormBuilder.validator =
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

  required_validator(error_msg : xhtml) : WFormBuilder.validator =
    input ->
      if String.is_empty(input) then
        {failure=error_msg}
      else
        {success=input}

  email_validator(error_msg : xhtml) : WFormBuilder.validator =
    input ->
      match Email.of_string_opt(input) with
      | {none} -> {failure=error_msg}
      | {some=_} -> {success=input}

  password_validator(spec : WFormBuilder.passwd_validator_spec)
    : WFormBuilder.validator =
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

  equals_validator(fld_id : string, error_msg : xhtml) : WFormBuilder.validator =
    input ->
      if input == get_text_value_of(fld_id) then
        {success = input}
      else
        {failure = error_msg}

  url_validator(error_msg : xhtml) : WFormBuilder.validator =
    input ->
      match Uri.of_string(input) with
      | {none} -> {failure = error_msg}
      | {some=uri} ->
          if Uri.is_valid_http(uri) then
            {success = input}
          else
            {failure = error_msg}

  merge_validators(vs : list(WFormBuilder.validator)) : WFormBuilder.validator =
    rec aux(input) =
      | [] -> { success=input }
      | [v | vs] ->
          match v(input) with
          | {success=input} -> aux(input)(vs)
          | res -> res
    aux(_)(vs)

  @private
  mk_field(label, field_type) : WFormBuilder.field =
    {~label ~field_type
     id=Dom.fresh_id()
     optionality={optional}
     initial_value=""
     validator=empty_validator
     hint=none
    }

  mk_fragment(xhtml : xhtml) : WFormBuilder.element =
    {fragment=xhtml}

  mk_text_field(label) : WFormBuilder.field =
    mk_field(label, {text})

  mk_email_field(label) : WFormBuilder.field =
    mk_field(label, {email})

  mk_passwd_field(label) : WFormBuilder.field =
    mk_field(label, {passwd})

  mk_desc_field_with(label, size : {rows : int; cols : int})
    : WFormBuilder.field =
    mk_field(label, {desc=size})

  mk_desc_field(label) : WFormBuilder.field =
    mk_desc_field_with(label, {rows=3 cols=40})

  mk_upload_field(label) : WFormBuilder.field =
    mk_field(label, {upload})

  add_validator(field : WFormBuilder.field, v : WFormBuilder.validator)
    : WFormBuilder.field =
    { field with
        validator=merge_validators([field.validator, v])
    }

  add_hint(field : WFormBuilder.field, hint : xhtml) : WFormBuilder.field =
    { field with hint=some(hint) }

  add_email_validator(field : WFormBuilder.field, err_msg : xhtml)
    : WFormBuilder.field =
    add_validator(field, email_validator(err_msg))

  add_default_email_validator(field : WFormBuilder.field) : WFormBuilder.field =
    add_email_validator(field, <>This is not a valid email address.</>)

  add_url_validator(field : WFormBuilder.field, err_msg : xhtml) : WFormBuilder.field =
    add_validator(field, url_validator(err_msg))

  add_default_url_validator(field : WFormBuilder.field) : WFormBuilder.field =
    add_url_validator(field, <>This does not seem to be a valid URL.</>)

  set_id(field : WFormBuilder.field, id : string) : WFormBuilder.field =
    {field with ~id}

  set_initial_value(field : WFormBuilder.field, initial_value : string) : WFormBuilder.field =
    {field with ~initial_value}

  make_required(field : WFormBuilder.field, err_msg : xhtml)
    : WFormBuilder.field =
    v = required_validator(err_msg)
    { field with
        optionality={required}
        validator=merge_validators([field.validator, v])
    }

  make_required_with_default_msg(field : WFormBuilder.field) : WFormBuilder.field =
    make_required(field, <>Please provide a value</>)

  @private
  animate(style, dom, e) =
    _ = Dom.Effect.with_duration(style.fade_duration, e)
     |> Dom.transition(dom, _)
    void

  @private
  set_block_type(fld, style) =
    do Dom.set_style(fld,
          match style with
          | {inline} -> css {display: inline}
          | {block} -> css {display: block}
       )
    void

  @private
  do_validate(style : WFormBuilder.style, id : string,
              validator : WFormBuilder.validator) : void =
    input = Dom.get_value(#{input_id(id)})
    err_fld = #{error_id(id)}
    set_styles(ok) =
      go(id, style) = WStyler.set_dom(style(ok), id)
      do go(label_id(id), style.label_style)
      do go(field_id(id), style.field_style)
      do go(input_id(id), style.input_style)
      void
    match validator(input) with
    | {success=_} ->
        do animate(style, err_fld, Dom.Effect.fade_out())
        do set_styles(true)
        void
    | {failure=err} ->
        do Dom.transform([{err_fld} <- err])
        do animate(style, err_fld, Dom.Effect.fade_in())
        do set_styles(false)
        do set_block_type(err_fld, style.error_elt_type)
        void

  @private
  show_hint(style, id) : void =
    hint_fld = #{hint_id(id)}
    do animate(style, hint_fld, Dom.Effect.fade_in())
    do set_block_type(hint_fld, style.hint_elt_type)
    void

  @private
  hide_hint(style, id) : void =
    animate(style, #{hint_id(id)}, Dom.Effect.fade_out())

  @private
  upload_fields_no(spec : WFormBuilder.specification) : int =
    is_upload_field =
    | ~{ field } -> field.field_type == {upload}
    | _ -> false
    List.filter(is_upload_field, spec.elts) |> List.length

  html( spec : WFormBuilder.specification
      , process : WFormBuilder.form_data -> void
      ) : xhtml =
    s = spec.style
    style(style) = WStyler.add(style, _)
    mk_field(~{label validator optionality initial_value field_type id hint}) =
      req =
        match optionality with
        | {optional} -> <span></> |> style(s.non_required_style)
        | {required} -> <span>*</> |> style(s.required_style)
      label_xhtml =
        <label id={label_id(id)} for={input_id(id)}>
          {label}
          {req}
        </> |> style(s.label_style(true))
      input(input_type) =
        <input type={input_type} name={input_id(id)} id={input_id(id)}
          value={initial_value} />
      input_tag =
        match field_type with
        | {email} -> input("email")
        | {text} -> input("text")
        | {passwd} -> input("password")
        | {upload} -> input("file")
        | {desc=~{cols rows}} ->
            // FIXME, resize:none should be in css{...}
            <textarea style="resize: none;" type="text"
              name={input_id(id)} id={input_id(id)}
              rows={rows} cols={cols} />
      stl_input_tag = input_tag |> style(s.input_style(true))
      onblur(_) =
        do do_validate(spec.style, id, validator)
        do hide_hint(spec.style, id)
        void
      onfocus(_) =
        do show_hint(spec.style, id)
        void
      bindings =
        [ ({blur}, onblur)
        , ({focus}, onfocus)
        ]
      input_xhtml = WCore.add_binds(bindings, stl_input_tag)
      hint_xhtml =
        match hint with
        | {none} -> <></>
        | {some=hint} ->
            <span id={hint_id(id)} style={css {display: none}}>{hint}</>
            |> style(s.hint_style)
      err_xhtml =
        <span id={error_id(id)} style={css {display: none}} />
        |> style(s.error_style)
      <div id={field_id(id)}>
        {label_xhtml}
        {input_xhtml}
        {hint_xhtml}
        {err_xhtml}
      </>
      |> style(s.field_style(true))
    mk_element =
    | ~{ fragment } -> fragment
    | ~{ field } -> mk_field(field)
    body_form = <>{List.map(mk_element, spec.elts)}</>
    if upload_fields_no(spec) == 0 then
      <form id={spec.form_id} action="#" onsubmit={_ -> process({SimpleForm})}
        method="get" options:onsubmit="prevent_default">
        {body_form}
      </form>
    else
      process(data) =
        do Scheduler.push( -> process({FileUploadForm=data}))
        void
      config = {Upload.default_config() with
                  ~body_form ~process form_id=spec.form_id}
      Upload.html(config)

  start(spec : WFormBuilder.specification) : void =
    is_field =
    | {field=_} -> true
    | _ -> false
    match List.find(is_field, spec.elts) with
    | {some=~{field}} -> Dom.give_focus(#{input_id(field.id)})
    | _ -> void

  submit_action(form_id : string) : (Dom.event -> void) =
    _ ->
       // submit the form
      Dom.trigger(#{form_id}, {submit})

  @private get_text_value_of(id : string) =
    Dom.get_value(#{input_id(id)})

  get_field_text_value( id : string
                      , data : WFormBuilder.form_data
                      ) : string =
    match data with
    | { SimpleForm } -> get_text_value_of(id)
    | { FileUploadForm=data } -> Map.get(input_id(id), data.form_fields) ? ""

  get_file_upload_value( id : string
                       , data : WFormBuilder.form_data
                       ) : option(Upload.file) =
    match data with
    | { SimpleForm } -> none
    | { FileUploadForm=data } -> Map.get(input_id(id), data.uploaded_files)

  // TODO do we want to keep field ids as strings? Maybe introduce abstraction for them...
  get_all_fields(spec : WFormBuilder.specification) : list(WFormBuilder.field) =
    get_field =
    | ~{field} -> some(field)
    | _ -> none
    List.filter_map(get_field, spec.elts)

}}
