/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

package widgets.formbuilder

import stdlib.*
import widgets.core

/**
 * @author Adam Koprowski
 * @category widget
 * @stability experimental [WIP]
 *
 *
 * {1 About this module}
 *
 * This module defines a Template engine, allowing to easily build forms
 * with templating machinery.
**/

type FormBuilderTemplate.tag = { form }

FormBuilderTemplate =

  my_ns : Uri.uri =
    Uri.of_string("http://opalang.org/template/FormBuilder")
    |> Option.get

  FB = WFormBuilder

{{

  @private
  parse(_conf, {~ns; ~tag; args=_; children=_})
    : outcome(Template.content(either(FormBuilderTemplate.tag, 'b)), Template.failure) =
    if ns != my_ns then
      { failure = { namespace_not_supported_by_engine = "Got {ns}, expected {my_ns}" }}
    else
      match tag with
      | "form" -> { success = Template.to_extension({form}) }
      | _ -> { failure = { unsupported_tag; ~ns; ~tag } }

  @private
  export(content, _) =
    match Template.from_extension(content) with
    | {none} -> { failure = { unknown_tag = "Expected extension" } }
    | {some=fb_tag} ->
        match fb_tag with
        | {form} ->
          fields = []
           // FIXME, plug-in form processing
          process({get_field_value=_}) = void
          spec = FB.create_specification(fields)
          form = FB.html(spec, process)
          {success = form}

  engine : Template.engine =
    { Template.empty with ~parse ~export }

}}
