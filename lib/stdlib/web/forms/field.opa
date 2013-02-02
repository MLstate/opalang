/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import stdlib.web.mail

type Field.type = {text} / {email} / {passwd}

type Field.required = {no} / {yes} / {with_msg: xhtml}

type Field.field('ty) = FormBuilder.field('ty)

type Field.passwd_validator = FormBuilder.passwd_validator_spec

type Field.validator('ty) =
  { no_validator } /
  { equals : Field.field('ty); err_msg : xhtml } /
  { custom: FormBuilder.field_validator('ty) } /
  { passwd: Field.passwd_validator }

type Field.options('ty) =
  { label: string
  ; required: Field.required
  ; hint: xhtml
  ; validator: Field.validator('ty)
  }

Field = {{

  @private FB = FormBuilder

  @private process_options(field, options) =
    add_required(fld) =
      match options.required
      | {no} -> fld
      | {yes} -> FB.make_required(fld, <>Please provide a value.</>)
      | {~with_msg} -> FB.make_required(fld, with_msg)
    add_hint(fld) =
      if options.hint == empty_xhtml then
        fld
      else
        FB.add_hint(fld, options.hint)
    add_validator(fld) =
      match options.validator
       // generic validators
      | {no_validator} -> fld
      | {equals=fld2 ~err_msg} ->
          FB.add_validator(fld, FB.equals_validator(fld2, err_msg))
      | {custom=validator} ->
          FB.add_validator(fld, validator)
       // specific validators: ignore here
      | {passwd=_} -> fld
    field |> add_required |> add_hint |> add_validator

  new : Field.options =
    { label="" required={no} hint=empty_xhtml validator={no_validator} }

  text_field(opts : Field.options) : Field.field(string) =
    FB.mk_field(opts.label, FB.text_field)
    |> process_options(_, opts)

  email_field(opts : Field.options) : Field.field(Email.email) =
    error_msg = <>This is not a correct email</>
    FB.mk_field(opts.label, FB.email_field(error_msg))
    |> process_options(_, opts)

  default_passwd_validator : Field.passwd_validator =
    FB.empty_passwd_validator_spec

  passwd_field(opts : Field.options) : Field.field(string) =
    add_passwd_validator(fld) =
      match opts.validator with
      | {passwd=spec} -> FB.add_validator(fld, FB.password_validator(spec))
      | _ -> fld
    FB.mk_field(opts.label, FB.passwd_field)
    |> process_options(_, opts)
    |> add_passwd_validator

  render(form: Form.form, field: Field.field) : xhtml =
    FB.render_field(form, field)

  get_value(field : Field.field('ty)) : option('ty) =
    FB.get_field_value(field)

}}
