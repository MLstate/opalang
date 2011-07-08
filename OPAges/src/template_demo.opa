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

import stdlib.web.template

type TemplateDemo.tags('a) =
  { random }
/ { date_format:string }
/ { scope : TemplateDemo.content('a)}

type TemplateDemo.content('a) = Template.content(either(TemplateDemo.tags('a), 'a))

TemplateDemo = {{

  namespace = "http://opalang.org/schema/demo.xsd" 

  dom_err(msg) = {failure = {dom_error = msg}}

  @private TDate = {{

    @private default_date_format = "%d/%m/%y"

    build({~args children=_}) =
      match List.find( { namespace=_; ~name; value=_ } -> name == "format", args) with
      | {some = { ~value ... } } -> { date_format=value }
      | { none } -> { date_format=default_date_format }

    export(~{date_format}, _exporter) = match Date.try_generate_printer(date_format) with
      | { success=date_printer } -> <>{Date.to_formatted_string(date_printer, Date.now())}</>
      | { ~failure } -> <>Incorrect date format {failure}</>
  }}

  @private TRandom = {{

    build({args=_ children=_}) = {random}

    export({random}, _exporter) = <>Random : {Random.int(515)}</>

  }}

  @private parse(_config, ~{xmlns xmlns_parser}:Template.import_arg(TemplateDemo.tags('a), 'b)) : outcome(Template.content(either(TemplateDemo.tags('a), 'b)), Template.failure) =
    match xmlns with
    | { ~tag; namespace="http://opalang.org/schema/demo.xsd"; ~args; specific_attributes=_; ~content } -> 
      children = Outcome.get(Template.parse_list_xmlns(content, xmlns_parser))
      build = match tag
        | "random"      -> some(TRandom.build)
        | "date"        -> some(TDate.build)
        | "scope"       -> some({args=_ ~children} -> {scope = children} )
        | _             -> none
      Option.switch((build -> {success = Template.to_extension(build(~{args children}))}),
                    {failure = {unsupported_tag ns=namespace ~tag}},
                    build)
    | _ -> { failure = { unsupported_node=xmlns } }

  @private export(content, exporter) =
    match Template.from_extension(content)
    | {none} -> {failure = {unknown_tag="Expected extension"}}
    | {some = e} ->
      {success = match e
            | {random} as e         -> TRandom.export(e, exporter)
            | {date_format=_} as e  -> TDate.export(e, exporter)
            | {~scope}              -> Outcome.get(exporter(scope) ) }

  @private source(content, exporter, xmlns_binding, printer, depth) =
    binding = StringMap.get(namespace, xmlns_binding)
              |> Option.default("opa", _)
    create_tag(tag_name, may_attribute, autoclose, may_child) =
      may_child = Option.map(child -> Outcome.get(exporter(child) ), may_child)
      Template.print_tag(tag_name, some(binding), Option.default("", may_attribute), autoclose, true, may_child)

      match Template.from_extension(content) with
      | { none } -> { failure = { unknown_tag = "Expected extension" } }
      | { some=tag } -> { success = printer(depth)(
          match tag
          | { random } -> create_tag("version", none, true, none)
          | ~{ date_format } -> create_tag("date", some(" format=\"{date_format}\""), true, none)
          | { ~scope }       -> create_tag("scope", none, false, some(scope))
        )}

  engine = ~{Template.empty with parse export source }

}}
