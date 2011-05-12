package opages
/** -- begin LICENCE
    (c) 2006-2011 MLstate
    All rights reserved.
    This file is confidential and intended solely for the addressee(s).
    Any unauthorized use or dissemination is prohibited.
    end LICENCE --
**/

/**
 * Opa template engine
 *
 * @author Anthonin Bonnefoy, 2011-2011
 * @destination private
 * @category web
 * @stability experimental
 */

import stdlib.{core, xhtml}
import stdlib.rpc.core
import stdlib.web.core
import stdlib.web.template
import stdlib.date

type TemplateDemo.tags('a) =
  { random }
/ { date_format:string }
/ { scope : TemplateDemo.content('a)}

type TemplateDemo.content('a) = Template.content(either(TemplateDemo.tags('a), 'a))

TemplateDemo = {{

  @private TDate = {{

    @private default_date_format = "%d/%m/%y"

    build({~args children=_}) =
      match List.find( { namespace=_; ~name; value=_ } -> name == "format", args) with
      | {some = { ~value ... } } -> { date_format=value }
      | { none } -> { date_format=default_date_format }

    export(~{date_format}, _child) = match Date.try_generate_printer(date_format) with
      | { success=date_printer } -> <>{Date.to_formatted_string(date_printer, Date.now())}</>
      | { ~failure } -> <>Incorrect date format {failure}</>
  }}

  @private TRandom = {{

    build({args=_ children=_}) = {random}

    export({random}, _child) = <>Random : {Random.int(515)}</>

  }}

  namespace = Uri.of_string("http://opalang.org/schema/demo.xsd") |> Option.get

  @private parse(_config, ~{ns tag args children }:Template.import_arg(TemplateDemo.tags('a), 'b)) : outcome(Template.content(either(TemplateDemo.tags('a), 'b)), Template.failure) =
    if ns == namespace then
      build = match tag
        | "random"      -> some(TRandom.build)
        | "date"        -> some(TDate.build)
        | "scope"       -> some({args=_ ~children} -> {scope = children} )
        | _             -> none
      Option.switch((build -> {success = Template.to_extension(build(~{args children}))}),
                    {failure = {unsupported_tag ~ns ~tag}},
                    build)
    else {failure = {namespace_not_supported_by_engine =
                "Engine({namespace}) vs Namespace({ns})"}}

  @private export(content, child) =
    e = Template.from_extension(content)
    match e
    | {none} -> {failure = {unknown_tag="Expected extension"}}
    | {some = e} ->
      {success = match e
            | {random} as e         -> TRandom.export(e, child)
            | {date_format=_} as e  -> TDate.export(e, child)
            | {scope=_}              -> child}

  @private source(content, child, xmlns_binding, printer) =
    binding = StringMap.get(Uri.to_string(namespace), xmlns_binding)
              |> Option.default("opa", _)
    create_tag(tag_name, may_attribute, autoclose) =
      begin, after ->
        attr = Option.default("", may_attribute)
        if autoclose
          then "{begin}<{binding}:{tag_name}{attr} />"
          else "{begin}<{binding}:{tag_name}{attr}> {child} {after}</{binding}:{tag_name}>"
      match Template.from_extension(content) with
      | { none } -> { failure = { unknown_tag = "Expected extension" } }
      | { some=tag } -> { success = printer(
          match tag
          | { random } -> create_tag("version", none, true)
          | ~{ date_format } -> create_tag("date", some(" format=\"{date_format}\""), true)
          | { scope=_ }       -> create_tag("scope", none, false)
        )}

  @private extract_children(content) = match Template.from_extension(content)
    | {some = ~{scope}} -> [scope]
    | _ -> []

  engine = ~{Template.empty with parse export source extract_children}

}}

