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

@abstract
type WFormBuilder.field('ty) =
  { id : string
  ; label : string
  ; optionality : { required } / { optional }
  ; field_type : { email } / { text } / { passwd } / { desc : { cols: int rows: int} } / { upload }
  ; initial_value : string
  ; validator : WFormBuilder.validator
  ; hint : option(xhtml)
  ; field_accessor : WFormBuilder.form_data -> 'ty
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

type WFormBuilder.field_builder =
  { mk_label : {optionality:{optional}/{required} label:string label_id:string input_id:string style:WFormBuilder.style} -> xhtml
  ; mk_input : {input_tag:xhtml style:WFormBuilder.style} -> xhtml
  ; mk_hint : {hint:option(xhtml) hint_id:string style:WFormBuilder.style} -> xhtml
  ; mk_err : {error_id:string style:WFormBuilder.style} -> xhtml
  ; mk_field : {label:xhtml input:xhtml hint:xhtml err:xhtml field_id:string style:WFormBuilder.style} -> xhtml
  }

WFormBuilder =

  field_id(id) = "{id}_field"
  label_id(id) = "{id}_label"
  input_id(id) = "{id}_input"
  hint_id(id) = "{id}_hint"
  error_id(id) = "{id}_err"

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

  /** {1 Fields/form querying} */

  get_field_label(f : WFormBuilder.field) =
    f.label

  get_field_value(f : WFormBuilder.field('a), data : WFormBuilder.form_data) : 'a =
    f.field_accessor(data)

  /** {1 Fields/form construction} */

  @private
  mk_field(label, field_type, mk_accessor) : WFormBuilder.field =
    id = Dom.fresh_id()
    {~label ~field_type ~id
     optionality={optional}
     initial_value=""
     validator=empty_validator
     hint=none
     field_accessor=mk_accessor(id)
    }

  @private
  mk_string_field =
    mk_field(_, _, (id -> get_field_text_value(id, _)))

  mk_text_field(label) : WFormBuilder.field(string) =
    mk_string_field(label, {text})

  mk_email_field(label) : WFormBuilder.field(string) =
    mk_string_field(label, {email})

  mk_passwd_field(label) : WFormBuilder.field(string) =
    mk_string_field(label, {passwd})

  mk_desc_field_with(label, size : {rows : int; cols : int})
    : WFormBuilder.field(string) =
    mk_string_field(label, {desc=size})

  mk_desc_field(label) : WFormBuilder.field(string) =
    mk_desc_field_with(label, {rows=3 cols=40})

  mk_upload_field(label) : WFormBuilder.field(option(Upload.file)) =
    mk_field(label, {upload}, (id -> get_file_upload_value(id, _)))

  /** {1 Extending fields} */

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

  /** {1 Form/fields rendering} */

  @private add_style(style) = WStyler.add(style, _)

  default_field_builder : WFormBuilder.field_builder =
    {
      mk_label(~{optionality label label_id input_id style=s}) =
        req =
          match optionality with
          | {optional} -> <span></> |> add_style(s.non_required_style)
          | {required} -> <span>*</> |> add_style(s.required_style)
        <label id={label_id} for={input_id}>
          {label}
          {req}
        </> |> add_style(s.label_style(true))

      mk_input(~{input_tag ...}) = input_tag

      mk_hint(~{hint hint_id style=s}) =
        match hint with
        | {none} -> <></>
        | {some=hint} ->
            <span id={hint_id}>{hint}</>
            |> add_style(s.hint_style)

      mk_err(~{error_id style=s}) =
        <span id={error_id} />
        |> add_style(s.error_style)

      mk_field(~{label input hint err field_id style=s}) =
        <div id={field_id}>
          {label}
          {input}
          {hint}
          {err}
        </>
        |> add_style(s.field_style(true))

     }

  field_html(field : WFormBuilder.field('ty), builder : WFormBuilder.field_builder, style : WFormBuilder.style) : xhtml =
    hide(tag) = WStyler.add({style=css{display: none}}, tag)
    mk_field(~{label validator optionality initial_value field_type id hint ...}) =
      label_xhtml = builder.mk_label(~{optionality label label_id=label_id(id) input_id=input_id(id) style})
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
      stl_input_tag = input_tag |> add_style(style.input_style(true))
      onblur(_) =
        do do_validate(style, id, validator)
        do hide_hint(style, id)
        void
      onfocus(_) =
        do show_hint(style, id)
        void
      bindings =
        [ ({blur}, onblur)
        , ({focus}, onfocus)
        ]
      input_tag = WCore.add_binds(bindings, stl_input_tag)
      input_xhtml = builder.mk_input(~{input_tag style})
      hint_xhtml = builder.mk_hint(~{hint hint_id=hint_id(id) style}) |> hide
      err_xhtml = builder.mk_err({error_id=error_id(id) ~style}) |> hide
      builder.mk_field({label=label_xhtml input=input_xhtml hint=hint_xhtml err=err_xhtml field_id=field_id(id) ~style})
    <>{mk_field(field)}</>

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
    Dom.give_focus(#{input_id(field.id)})

  submit_action(form_id : string) : (Dom.event -> void) =
    _ ->
       // submit the form
      Dom.trigger(#{form_id}, {submit})

  /* ---------- Private functions ---------- */

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

  @private get_text_value_of(id : string) =
    Dom.get_value(#{input_id(id)})

  @private
  get_field_text_value( id : string
                      , data : WFormBuilder.form_data
                      ) : string =
    match data with
    | { SimpleForm } -> get_text_value_of(id)
    | { FileUploadForm=data } -> Map.get(input_id(id), data.form_fields) ? ""

  @private
  get_file_upload_value( id : string
                       , data : WFormBuilder.form_data
                       ) : option(Upload.file) =
    match data with
    | { SimpleForm } -> none
    | { FileUploadForm=data } -> Map.get(input_id(id), data.uploaded_files)

}}
