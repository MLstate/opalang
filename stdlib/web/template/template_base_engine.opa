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
 * Define the base engine which is able to parse an html tree.
 *
 * @author Anthonin Bonnefoy, 2011-2011
 * @destination public
 * @category web
 * @stability experimental
 */

import stdlib.web.client

@private TemplateBaseEngine =
{{
  @private xmlns_namespace = "http://www.w3.org/2000/xmlns/"

  @private sassoc_opt(name:string, may_value:option(string)):option(string_assoc(string))=
    Option.map(value -> sassoc(name,value), may_value)

  @private sassoc_opt_tuples((str,value):(string, option(string))):option(string_assoc(string))=
    sassoc_opt(str,value)

  @private sassoc_local_binding(list_local_binding:list(Template.local_binding)) : list(option(string_assoc(string))) =
    List.map( local_binding ->
      { ~name; uri=value } = local_binding
      { some = { namespace = "xmlns"; ~name; ~value } }
    , list_local_binding )

  @private get_local_binding(list_string_assoc) : list(Template.local_binding)=
    List.filter( { ~namespace; name=_; value=_ } -> namespace == xmlns_namespace, list_string_assoc)
    |> List.map( { ~name; ~value ... } -> { ~name; uri=value }  , _ )

  @private get_attribute(attribute_name:string, list_string_assoc:Template.args ) : option(string) =
    List.find( { ~namespace; ~name; value=_ } -> namespace == "" && attribute_name == name, list_string_assoc)
    |> Option.map( { ~value ... } -> value, _ )

  @private get_map_attribute( attribute_name:string, list_string_assoc:Template.args, map_fun : (string -> option('a)), error_message:(string -> string) ) : outcome(option('a), Template.failure) =
    match get_attribute(attribute_name, list_string_assoc) with
    | { ~some } -> (
      match map_fun(some) with
      | {~some} -> { success = { ~some } }
      | { none } -> { failure={ attribute_tag_error=error_message(some) } }
    )
    | { none } -> { success = none }

  @private get_attribute_int( attribute_name:string, list_string_assoc:Template.args) : outcome(option(int), Template.failure) =
    get_map_attribute(attribute_name, list_string_assoc, Parser.int, (val -> "{attribute_name} takes an integer, got {val}") )

  @private get_attribute_url( attribute_name:string, list_string_assoc:Template.args) : outcome(option(Uri.uri), Template.failure) =
    get_map_attribute(attribute_name, list_string_assoc, (a -> Uri.of_string(a)), (val -> "{attribute_name} attribute should take a valid url, {val} is not a valid url") )

  /**
   *  Parser and exporter for the standard attributes
   */
  @private std_attr : Template.attribute(Template.standard_attribute) = {{

    known_attributes = ["title", "class", "dir", "xmlns", "id"]

    remove_known_attributes(args:Template.args):Template.args =
    List.filter(
      { ~namespace; ~name; value=_ } ->
        namespace != "http://www.w3.org/2000/xmlns/"
        && not(namespace == "" && List.mem(name, known_attributes) )
      , args
    )

    parse(conf:Template.conf, args:Template.args) : outcome(Template.standard_attribute, Template.failure)  =
      id = get_attribute( "id", args)
      class = get_attribute( "class", args)
      local_binding = get_local_binding(args)
      outcome_dir = get_map_attribute( "dir", args, Template.parse_dir,
        (val -> "dir attribute should take only 'ltr' and 'rtl' values, got {val} instead" ) )
      title = get_attribute( "title", args)
      free_attributes = remove_known_attributes(args)
      match  outcome_dir with
      | {~success} ->
        if (conf.strict && List.length(free_attributes) > 0)
          then { failure = { unknown_attribute = "{free_attributes}" } }
          else {success = { ~class; dir=success ; ~title; ~local_binding ; ~free_attributes; ~id } }
      | {~failure} -> {~failure}

    export(standard_attribute:Template.standard_attribute) : list(option(string_assoc(string))) =
      ~{ title; class; dir; local_binding; free_attributes; id } = standard_attribute
      dir = Option.map(Template.string_of_dir, dir)
      List.append(
        List.map(some, free_attributes),
        List.append(
          sassoc_local_binding(local_binding)
          ,[ sassoc_opt("title", title), sassoc_opt("class", class), sassoc_opt("dir", dir), sassoc_opt("id", id) ]
        )
      )
  }}

  @private base_remove_attributes(args:Template.args, known_attributes:list(string)):Template.args = List.filter(
      {~name; ~namespace; value=_ } -> not(namespace=="" && List.mem(name, known_attributes))
      , args
    )

  /**
   *  Parser and exporter for the form attributes
   */
  @private form_attr : Template.attribute(Template.form_attribute) = {{
    known_attributes = [ "accept", "name", "action", "method", "enctype"]

    remove_known_attributes(args) = base_remove_attributes(args, known_attributes)

    parse(conf:Template.conf, list_string_assoc:Template.args) : outcome(Template.form_attribute, Template.failure) =
      accept = get_attribute( "accept", list_string_assoc)
      name = get_attribute( "name", list_string_assoc)
      outcome_std_attr = std_attr.parse(conf, remove_known_attributes(list_string_assoc) )
      outcome_action : outcome(option(Uri.uri), Template.failure) = get_attribute_url( "action", list_string_assoc)
      outcome_method = get_map_attribute( "method", list_string_assoc, Template.parse_method,
        (val -> "method attribute should take get or post value, got {val} instead"))
      outcome_enctype = get_map_attribute( "enctype", list_string_assoc, Template.parse_enctype,
        (val -> "enctype attribute should take application/x-www-form-urlencoded, multipart/form-data or text/plain, got {val} instead"))
      match (outcome_action, outcome_method, outcome_enctype, outcome_std_attr) with
      | ( {~failure}, _,_, _) | ( _, {~failure}, _, _) | (_, _, {~failure}, _) | (_, _, _, {~failure}) -> { ~failure }
      | ( {success={none}}, _, _, _ ) -> { failure = { attribute_required = "action attribute is required on form" } }
      | _ -> { success = { ~accept; ~name; method=Outcome.get(outcome_method); action=Option.get(Outcome.get(outcome_action));
              enctype=Outcome.get(outcome_enctype); standard_attribute=Outcome.get(outcome_std_attr) } }

    export(form_attribute:Template.form_attribute) : list(option(string_assoc(string))) =
      ~{ accept; name; method; action; enctype; standard_attribute } = form_attribute
      action = some(Uri.to_string(action))
      method = Option.map(Template.string_of_method, method)
      enctype = Option.map(Template.string_of_enctype, enctype)
      zipped_attributes = List.zip(
        known_attributes ,
        [ accept, name, action, method, enctype ]
      )
      List.append( std_attr.export(standard_attribute), List.map( sassoc_opt_tuples, zipped_attributes))
  }}

  input_attrs : Template.attribute(Template.input_attribute) = {{
    known_attributes = [ "src", "maxlength", "size", "type", "value", "readonly", "name", "disabled", "checked", "alt", "accept" ]

    remove_known_attributes(args:Template.args) = base_remove_attributes(args, known_attributes)

    parse(conf:Template.conf, list_string_assoc:Template.args) : outcome(Template.input_attribute, Template.failure) =
      accept = get_attribute( "accept", list_string_assoc)
      alt = get_attribute( "alt", list_string_assoc)
      checked = get_attribute( "checked", list_string_assoc)
      disabled = get_attribute( "disabled", list_string_assoc)
      name = get_attribute( "name", list_string_assoc)
      readonly = get_attribute( "readonly", list_string_assoc)
      value = get_attribute( "value", list_string_assoc)

      standard_attribute = std_attr.parse(conf, remove_known_attributes(list_string_assoc) )
      maxlength = get_attribute_int( "maxlength", list_string_assoc)
      size = get_attribute_int( "size", list_string_assoc)
      src = get_attribute_url( "src", list_string_assoc)
      form_type = get_map_attribute( "type", list_string_assoc, Template.parse_input_type, (val -> "type takes button/checkbox/file/hidden/image/password/radio/reset/submit/text, got {val}"))

      match (maxlength, size, src, form_type, standard_attribute) with
      | ( {~failure}, _,_, _, _) | ( _, {~failure}, _, _, _) | (_, _, {~failure}, _, _) | (_, _, _, {~failure}, _) | (_, _, _, _, {~failure}) -> { ~failure }
      | _ ->
        (maxlength, size, src, form_type, standard_attribute) = (Outcome.get(maxlength), Outcome.get(size), Outcome.get(src), Outcome.get(form_type), Outcome.get(standard_attribute))
        { success = ~{ accept; alt; checked; disabled; name; readonly; value; maxlength; size; src; form_type; standard_attribute } }

    export(input_attribute:Template.input_attribute) : list(option(string_assoc(string))) =
      ~{ accept; alt; checked; disabled; name; readonly; value; maxlength; size; src; form_type; standard_attribute } = input_attribute
      src = Option.map( Uri.to_string, src)
      maxlength = Option.map(Int.to_string, maxlength)
      size = Option.map(Int.to_string, size)
      form_type = Option.map(Template.string_of_input_type,form_type)
      zipped_attributes = List.zip(
        known_attributes,
        [ src, maxlength, size, form_type, value, readonly, name, disabled, checked, alt, accept ] )
      List.append( std_attr.export(standard_attribute), List.map( sassoc_opt_tuples, zipped_attributes))

  }}

  @private textarea_attrs : Template.attribute(Template.textarea_attribute) = {{
    known_attributes = [ "cols", "rows", "disabled", "name", "readonly" ]

    remove_known_attributes(args) = base_remove_attributes(args, known_attributes)

    parse(conf:Template.conf, list_string_assoc:Template.args) : outcome(Template.textarea_attribute, Template.failure) =
      cols = get_attribute_int( "cols", list_string_assoc)
      rows = get_attribute_int( "rows", list_string_assoc)
      disabled = get_attribute( "disabled", list_string_assoc)
      name = get_attribute( "name", list_string_assoc)
      readonly = get_attribute( "readonly", list_string_assoc)
      standard_attribute = std_attr.parse(conf, remove_known_attributes(list_string_assoc) )

      match (cols, rows, standard_attribute) with
      | ( {~failure}, _,_) | ( _, {~failure}, _) | (_, _, {~failure}) -> { ~failure }
      | ( {success={none}}, _,_)
      | ( _, {success={none}}, _) -> { failure = { attribute_required = "Textarea : rows and cols attributes are required" } }
      | _ ->
        (cols, rows, standard_attribute) = (Option.get(Outcome.get(cols)), Option.get(Outcome.get(rows)), Outcome.get(standard_attribute))
        { success = ~{ cols; rows; disabled; name; readonly; standard_attribute } }

    export(textarea_attribute:Template.textarea_attribute) : list(option(string_assoc(string))) =
      ~{ cols; rows; disabled; name; readonly; standard_attribute } = textarea_attribute
      cols = some(Int.to_string(cols))
      rows = some(Int.to_string(rows))
      zipped_attributes = List.zip(
        known_attributes ,
        [ cols, rows, disabled, name, readonly ] )
      List.append( std_attr.export(standard_attribute), List.map( sassoc_opt_tuples, zipped_attributes))
  }}

  @private select_attrs : Template.attribute(Template.select_attribute) = {{
    known_attributes = [ "disabled", "multiple", "name", "size" ]

    remove_known_attributes(args) = base_remove_attributes(args, known_attributes)

    parse(conf:Template.conf, list_string_assoc:Template.args) : outcome(Template.select_attribute, Template.failure) =
      disabled = get_attribute( "disabled", list_string_assoc)
      multiple = get_attribute( "multiple", list_string_assoc)
      name = get_attribute( "name", list_string_assoc)
      size = get_attribute_int( "size", list_string_assoc)
      standard_attribute = std_attr.parse(conf, remove_known_attributes(list_string_assoc) )
      match (size, standard_attribute) with
      | ( {~failure}, _) | ( _, {~failure}) -> { ~failure }
      | _ ->
        (size, standard_attribute) = (Outcome.get(size), Outcome.get(standard_attribute))
        { success = ~{ disabled; multiple; name; size; standard_attribute } }

    export(select_attribute:Template.select_attribute) : list(option(string_assoc(string))) =
      ~{ disabled; multiple; name; size; standard_attribute } = select_attribute
      size = Option.map(Int.to_string,size)
      zipped_attributes = List.zip(
        known_attributes ,
        [ disabled, multiple, name, size ] )
      List.append( std_attr.export(standard_attribute), List.map( sassoc_opt_tuples, zipped_attributes))
  }}

  @private option_attrs : Template.attribute(Template.option_attribute) = {{
    known_attributes = [ "disabled", "label", "selected", "value" ]

    remove_known_attributes(args) = base_remove_attributes(args, known_attributes)

    parse(conf:Template.conf, list_string_assoc:Template.args) : outcome(Template.option_attribute, Template.failure) =
      disabled = get_attribute( "disabled", list_string_assoc)
      label = get_attribute( "label", list_string_assoc)
      selected = get_attribute( "selected", list_string_assoc)
      value = get_attribute( "value", list_string_assoc)
      standard_attribute = std_attr.parse(conf, remove_known_attributes(list_string_assoc) )
      match standard_attribute with
      | { ~failure } -> { ~failure }
      | { success = standard_attribute } ->
        { success = ~{ disabled; label; selected; value; standard_attribute } }

    export(option_attribute:Template.option_attribute) : list(option(string_assoc(string))) =
      ~{ disabled; label; selected; value; standard_attribute } = option_attribute
      zipped_attributes = List.zip(
        known_attributes ,
        [ disabled, label, selected, value ] )
      List.append( std_attr.export(standard_attribute), List.map( sassoc_opt_tuples, zipped_attributes))
  }}

  @private optgroup_attrs : Template.attribute(Template.optgroup_attribute) = {{
    known_attributes = [ "disabled", "label" ]

    remove_known_attributes(args) = base_remove_attributes(args, known_attributes)

    parse(conf:Template.conf, list_string_assoc:Template.args) : outcome(Template.optgroup_attribute, Template.failure) =
      disabled = get_attribute( "disabled", list_string_assoc)
      label = get_attribute( "label", list_string_assoc)
      standard_attribute = std_attr.parse(conf, remove_known_attributes(list_string_assoc) )
      match (label, standard_attribute) with
      | (_ , { ~failure }) -> { ~failure }
      | ({none} , _) -> { failure = { attribute_required = "Optgroup : label attribute is required" } }
      | (_, _)   ->
        (label, standard_attribute) = (Option.get(label), Outcome.get(standard_attribute))
        { success = ~{ disabled; label; standard_attribute } }

    export(optgroup_attribute:Template.optgroup_attribute) : list(option(string_assoc(string))) =
      ~{ disabled; label; standard_attribute } = optgroup_attribute
      zipped_attributes = List.zip(
        known_attributes,
        [ disabled, some(label) ] )
      List.append( std_attr.export(standard_attribute), List.map( sassoc_opt_tuples, zipped_attributes))
  }}

  @private td_attrs : Template.attribute(Template.td_attribute) = {{
    known_attributes = [ "abbr", "colspan", "rowspan" ]

    remove_known_attributes(args) = base_remove_attributes(args, known_attributes)

    parse(conf:Template.conf, list_string_assoc:Template.args) : outcome(Template.td_attribute, Template.failure) =
      abbr = get_attribute( "abbr", list_string_assoc)
      colspan = get_attribute_int( "colspan", list_string_assoc)
      rowspan = get_attribute_int( "rowspan", list_string_assoc)
      standard_attribute = std_attr.parse(conf, remove_known_attributes(list_string_assoc) )
      match (colspan, rowspan, standard_attribute) with
      | ({~failure}, _ , _)
      | (_, _ , {~failure} )
      | (_, {~failure} , _) -> { ~failure }
      | (_, _, _)   ->
        (colspan, rowspan,standard_attribute) = (Outcome.get(colspan), Outcome.get(rowspan), Outcome.get(standard_attribute))
        { success = ~{ abbr; rowspan; colspan; standard_attribute } }

    export(td_attribute:Template.td_attribute) : list(option(string_assoc(string))) =
      ~{ abbr; rowspan; colspan; standard_attribute } = td_attribute
      zipped_attributes = List.zip(
        known_attributes ,
        [ abbr, Option.map(Int.to_string, colspan), Option.map(Int.to_string, rowspan) ] )
      List.append( std_attr.export(standard_attribute), List.map( sassoc_opt_tuples, zipped_attributes))
  }}

  @private link_attrs : Template.attribute(Template.link_attribute) = {{
    known_attributes = [ "media", "rev", "rel", "href", "mime" ]

    remove_known_attributes(args) = base_remove_attributes(args, known_attributes)

    parse(conf:Template.conf, list_string_assoc:Template.args) : outcome(Template.link_attribute, Template.failure) =
      media_type = get_map_attribute( "media", list_string_assoc, Template.parse_media, (val -> "media takes screen/tty/tv/projection/handheld/print/braille/aural/all, got {val}"))
      rel_type = get_map_attribute( "rel", list_string_assoc, Template.parse_rel, (val -> "rel takes alternate/appendix/bookmark/chapter/contents/copyright/glossary/help/home/index/next/prev/section/start/stylesheet/subsection, got {val}"))
      rev_type = get_map_attribute( "rev", list_string_assoc, Template.parse_rev, (val -> "rev takes alternate/appendix/bookmark/chapter/contents/copyright/glossary/help/home/index/next/prev/section/start/stylesheet/subsection, got {val}"))
      href = get_attribute_url( "href", list_string_assoc)
      standard_attribute = std_attr.parse(conf, remove_known_attributes(list_string_assoc) )
      mime = get_attribute( "type", list_string_assoc)
      match (media_type, rev_type, rel_type, href, standard_attribute) with
      | ({~failure}, _, _, _, _) -> { ~failure }
      | (_, {~failure}, _, _, _) -> { ~failure }
      | (_, _, {~failure}, _, _) -> { ~failure }
      | (_, _, _, {~failure}, _) -> { ~failure }
      | (_, _, _, _, {~failure}) -> { ~failure }
      | (_, _, _, _, _)   ->
        (media, rev, rel, href, standard_attribute) = (Outcome.get(media_type), Outcome.get(rev_type), Outcome.get(rel_type), Outcome.get(href), Outcome.get(standard_attribute))
        { success = ~{ media; rev; rel; standard_attribute; href; mime } }

    export(link_attribute:Template.link_attribute) : list(option(string_assoc(string))) =
      ~{ media; mime; rev; rel; standard_attribute; href } = link_attribute
      zipped_attributes = List.zip(
        known_attributes ,
        [ Option.map(Template.string_of_media, media) , Option.map(Template.string_of_rev, rev) , Option.map(Template.string_of_rel, rel) , Option.map(Uri.to_string, href), mime ]
        )
      List.append( std_attr.export(standard_attribute), List.map( sassoc_opt_tuples, zipped_attributes))
  }}

  @private meta_attrs : Template.attribute(Template.meta_attribute) = {{
    known_attributes = [ "content", "name", "scheme" ]

    remove_known_attributes(args) = base_remove_attributes(args, known_attributes)

    parse(conf:Template.conf, list_string_assoc:Template.args) : outcome(Template.meta_attribute, Template.failure) =
      meta_content = get_attribute("content", list_string_assoc)
      name = get_map_attribute("name", list_string_assoc, Template.parse_name, (val -> "meta name takes author/description/keywords/generator/revised, got {val}") )
      scheme = get_attribute( "scheme", list_string_assoc)
      standard_attribute = std_attr.parse(conf, remove_known_attributes(list_string_assoc) )
      match (name, meta_content, standard_attribute) with
      | ({~failure}, _, _) -> { ~failure }
      | (_, {none}, _) -> { failure = { attribute_required = "meta : content attribute is required" } }
      | (_, _, {~failure} ) -> { ~failure }
      | (_, _, _) ->
        (name, meta_content, standard_attribute) = (Outcome.get(name), Option.get(meta_content), Outcome.get(standard_attribute) )
        { success = ~{ meta_content; name; scheme; standard_attribute } }

    export(meta_attribute:Template.meta_attribute) : list(option(string_assoc(string))) =
      ~{ meta_content; name; scheme; standard_attribute } = meta_attribute
      zipped_attributes = List.zip(
        known_attributes ,
        [ some(meta_content) , Option.map(Template.string_of_name, name) , scheme ]
        )
      List.append( std_attr.export(standard_attribute), List.map( sassoc_opt_tuples, zipped_attributes))

  }}

  @private anchor_attrs : Template.attribute(Template.anchor_attribute) = {{
    known_attributes = [ "href", "target" ]

    remove_known_attributes(args) = base_remove_attributes(args, known_attributes)

    parse(conf:Template.conf, list_string_assoc:Template.args) : outcome(Template.anchor_attribute, Template.failure) =
      href = get_attribute_url( "href", list_string_assoc)
      target = get_attribute( "target", list_string_assoc)
      standard_attribute = std_attr.parse(conf, remove_known_attributes(list_string_assoc) )
      match (href, standard_attribute) with
      | ({~failure}, _) -> { ~failure }
      | (_, {~failure} ) -> { ~failure }
      | ({ success = anchor_href }, {success = standard_attribute}) ->
        { success = ~{ anchor_href; ~standard_attribute; ~target } }

    export(anchor_attribute:Template.anchor_attribute) : list(option(string_assoc(string))) =
      ~{ anchor_href; ~target; ~standard_attribute } = anchor_attribute
      zipped_attributes = List.zip(
        known_attributes ,
        [ Option.map(Uri.to_string, anchor_href), target ]
        )
      List.append( std_attr.export(standard_attribute), List.map( sassoc_opt_tuples, zipped_attributes))
  }}

  @private quote_attrs : Template.attribute(Template.quote_attribute) = {{
    known_attributes = [ "cite" ]

    remove_known_attributes(args) = base_remove_attributes(args, known_attributes)

    parse(conf:Template.conf, list_string_assoc:Template.args) : outcome(Template.quote_attribute, Template.failure) =
      cite = get_attribute_url( "cite", list_string_assoc)
      standard_attribute = std_attr.parse(conf, remove_known_attributes(list_string_assoc) )
      match (cite, standard_attribute) with
      | ({~failure}, _) -> { ~failure }
      | (_, {~failure} ) -> { ~failure }
      | ({ success = cite }, {success = standard_attribute}) ->
        { success = ~{ cite; ~standard_attribute } }

    export(quote_attribute:Template.quote_attribute) : list(option(string_assoc(string))) =
      ~{ cite; ~standard_attribute } = quote_attribute
      zipped_attributes = [ ("cite" , Option.map(Uri.to_string,cite)) ]
      List.append( std_attr.export(standard_attribute), List.map( sassoc_opt_tuples, zipped_attributes))
  }}

  @private absolute_to_uri(uri:Uri.absolute):Uri.uri =
    match uri with
    | {~credentials; ~domain; ~fragment; ~path; ~port; ~query; ~schema; ~is_directory } -> {~credentials; ~domain; ~fragment; ~path; ~port; ~query; ~schema; ~is_directory }

  @private uri_to_absolute_uri(uri:Uri.uri) : option(Uri.absolute) =
    match uri with
    | {~credentials; ~domain; ~fragment; ~path; ~port; ~query; ~schema; ~is_directory } -> {some = {~credentials; ~domain; ~fragment; ~path; ~port; ~query; ~schema; ~is_directory } }
    | _ -> { none }

  @private base_attrs : Template.attribute(Template.base_attribute) = {{
    known_attributes = [ "href" ]

    remove_known_attributes(args) = base_remove_attributes(args, known_attributes)

    parse(conf:Template.conf, list_string_assoc:Template.args) : outcome(Template.base_attribute, Template.failure) =
      href = get_attribute_url("href", list_string_assoc)
      standard_attribute = std_attr.parse(conf, remove_known_attributes(list_string_assoc) )
      href = match href with
      | {~failure} -> { ~failure }
      | { ~success } -> (
        match success with
        | { ~some } ->
          Option.switch(
            (abs -> { success = abs } )
            , { failure = { attribute_tag_error= "Tag base : href must be an absolute uri, got {href}" } }
            , uri_to_absolute_uri(some)
          )
        | { none } -> { failure = { attribute_required = "Attribute href is required in base." } }
      )
      match (href, standard_attribute) with
      | ({ ~failure }, _) -> { ~failure }
      | (_ , {~failure} ) -> { ~failure }
      | (_, _) ->
        (base_href, standard_attribute) = (Outcome.get(href), Outcome.get(standard_attribute) )
        { success = { ~base_href ; ~standard_attribute }   }

    export(base_attribute:Template.base_attribute) : list(option(string_assoc(string))) =
      ~{ base_href; standard_attribute } = base_attribute
      zipped_attributes = [ ("href" , some(Uri.to_string(absolute_to_uri(base_href)))) ]
      List.append( std_attr.export(standard_attribute), List.map( sassoc_opt_tuples, zipped_attributes))
  }}

  @private img_attrs : Template.attribute(Template.img_attribute) = {{
    known_attributes = [ "src", "alt" ]

    remove_known_attributes(args) = base_remove_attributes(args, known_attributes)

    parse(conf:Template.conf, list_string_assoc:Template.args) : outcome(Template.img_attribute, Template.failure) =
      src = get_attribute_url( "src", list_string_assoc)
      alt = get_attribute( "alt", list_string_assoc)
      standard_attribute = std_attr.parse(conf, remove_known_attributes(list_string_assoc) )
      match (src, alt, standard_attribute) with
      | ({~failure},_, _) -> { ~failure }
      | (_, _, {~failure} ) -> { ~failure }
      | ({success={none}}, _, _) -> { failure = { attribute_required = "img : src attribute is required" } }
      | (_, {none}, _) -> { failure = { attribute_required = "img : alt attribute is required" } }
      | ({ success = {some = src} }, {some = alt}, {success = standard_attribute}) ->
        { success = ~{ standard_attribute; src; alt } }

    export(img_attribute:Template.img_attribute) : list(option(string_assoc(string))) =
      ~{ ~standard_attribute; ~src; ~alt } = img_attribute
      zipped_attributes = List.zip(
        known_attributes ,
        [ some(Uri.to_string(src)), some(alt) ]
        )
      List.append( std_attr.export(standard_attribute), List.map( sassoc_opt_tuples, zipped_attributes))
  }}

  @private label_attrs : Template.attribute(Template.label_attribute) = {{
    known_attributes = [ "for" ]

    remove_known_attributes(args) = base_remove_attributes(args, known_attributes)

    parse(conf:Template.conf, list_string_assoc:Template.args) : outcome(Template.label_attribute, Template.failure) =
      for = get_attribute( "for", list_string_assoc)
      standard_attribute = std_attr.parse(conf, remove_known_attributes(list_string_assoc) )
      match standard_attribute with
      | {~failure}  -> { ~failure }
      | ({success = standard_attribute}) -> { success = { ~standard_attribute; ~for } }

    export(label_attribute:Template.label_attribute) : list(option(string_assoc(string))) =
      ~{ ~standard_attribute; ~for } = label_attribute
      zipped_attributes = [( "for", for )]
      List.append( std_attr.export(standard_attribute), List.map( sassoc_opt_tuples, zipped_attributes))
  }}

  /**
   *  Add the list of attributes in the given xhtml. If the attributes already exist, it will be
   *  override
   */
  @private add_attributes_to_xhtml( list_attribute:list(option(string_assoc(string))), xhtml:xhtml) : outcome(xhtml, Template.export_failure)  =
    list_attribute = List.filter_map(identity, list_attribute)
    match xhtml with
    | { ~args ; ~content ; ~namespace ; ~specific_attributes ; ~tag } ->
      { success = { args=List.append(list_attribute, args); ~content ; ~namespace ; ~specific_attributes ; ~tag } }
    | _ -> error("add_attributes_to_xhtml called with a precompiled xhtml")

  @private parse_attributes(conf:Template.conf, args:Template.args, attr_fun:Template.attribute('a), fun:('a -> Template.content(either(void, 'b)))): outcome(Template.content(either(void, 'b)), Template.failure)=
    match attr_fun.parse(conf, args) with
    | { success=attr } -> {success = fun(attr) }
    | { ~failure } -> { ~failure }

  @private parse_standard_attributes(conf, args:Template.args, fun:(Template.standard_attribute -> Template.content(either(void, 'b)))): outcome(Template.content(either(void, 'b)), Template.failure)=
    parse_attributes(conf, args, std_attr, fun)

  @private parse_form_attributes(conf, args:Template.args, fun:(Template.form_attribute -> Template.content(either(void, 'b)))): outcome(Template.content(either(void, 'b)), Template.failure)=
    parse_attributes(conf, args, form_attr, fun)

  @private parse_title_tag(child : Template.content('a) ) : outcome(Template.content('a), Template.failure) =
    match child with
    | { ~text }
    | { checked_text=text } -> { success = { title=text } }
    | {fragment = [{checked_text=text}]}
    | {fragment = [{~text}]} -> { success = { title=text } }
    | _ -> { failure = { dom_error = "The title tag should contains only text, got {child}" } }

  @private base_extract_children(content:Template.content(either(void, 'b))):list(Template.content(either(void, 'b))) =
    match content with
    | { ~content  ... }   -> [content]
    | { ~fragment } -> fragment
    | _ -> []

  @private extract_xmlns_children(xmlns:xmlns) =
    match xmlns with
    | { ~content ... } -> content
    | _ -> []

  parse_node(conf, tag, args, children, _xmlns, xmlns_parser) =
    match Template.merge_outcome_content(List.map(xmlns_parser, children)) /* : outcome(Template.content(either(void, 'b)), Template.failure) */ with
    | { success = content } ->
      standard_tag(tag) =
        parse_standard_attributes(conf, args, (standard_attribute -> { standard_tag=tag; ~content; ~standard_attribute }))
      parse_with(attrs, f) =
        parse_attributes(conf, args, attrs, f)
      match tag with
      | "a" -> parse_with(anchor_attrs, (anchor_attribute -> { anchor; ~anchor_attribute; ~content }))
      | "div" -> standard_tag({div})
      | "blockquote" -> parse_with(quote_attrs, (quote_attribute -> { blockquote; ~content; ~quote_attribute }))
      | "q" -> parse_with(quote_attrs, (quote_attribute -> { quote; ~content; ~quote_attribute }))
      | "address" -> standard_tag({address})
      | "acronym" -> standard_tag({acronym})
      | "fieldset" -> standard_tag({fieldset})
      | "legend" -> standard_tag({legend})
      | "pre" -> standard_tag({pre})
      | "sub" -> standard_tag({sub})
      | "sup" -> standard_tag({sup})
      | "p" -> standard_tag({paragraph})
      | "hr" -> parse_standard_attributes(conf, args, (standard_attribute -> { hr; ~standard_attribute }))
      | "br" -> parse_standard_attributes(conf, args, (standard_attribute -> { br; ~standard_attribute }))
      | "h1" -> standard_tag({h1})
      | "h2" -> standard_tag({h2})
      | "h3" -> standard_tag({h3})
      | "h4" -> standard_tag({h4})
      | "h5" -> standard_tag({h5})
      | "h6" -> standard_tag({h6})
      | "abbr" -> standard_tag({abbr})
      | "del" -> standard_tag({del})
      | "ins" -> standard_tag({ins})
      | "dd" -> standard_tag({dd})
      | "dt" -> standard_tag({dt})
      | "dl" -> standard_tag({dl})
      | "label" -> parse_with(label_attrs, (label_attribute -> { label; ~content; ~label_attribute }))
      | "ul" -> standard_tag({ul})
      | "menu" -> standard_tag({menu})
      | "open" -> standard_tag({open})
      | "ol" -> standard_tag({ol})
      | "li" -> standard_tag({li})
      | "span" -> standard_tag({span})
      | "form" -> parse_form_attributes(conf, args, (form_attribute -> { form; ~content; ~form_attribute }) )
      | "input" -> parse_with(input_attrs, (input_attribute -> { input; ~input_attribute }))
      | "select" -> parse_with(select_attrs, (select_attribute -> { select; ~content; ~select_attribute }))
      | "option" -> parse_with(option_attrs, (option_attribute -> { option; ~content; ~option_attribute }))
      | "optgroup" -> parse_with(optgroup_attrs, (optgroup_attribute -> { optgroup; ~content; ~optgroup_attribute }))
      | "textarea" -> parse_with(textarea_attrs, (textarea_attribute -> { textarea; ~content; ~textarea_attribute }))
      | "table" -> standard_tag({table})
      | "caption" -> standard_tag({caption})
      | "thead" -> standard_tag({thead})
      | "tbody" -> standard_tag({tbody})
      | "tfoot" -> standard_tag({tfoot})
      | "tr" -> standard_tag({tr})
      | "td" -> parse_with(td_attrs, (td_attribute -> { td; ~content; ~td_attribute }))
      | "th" -> parse_with(td_attrs, (th_attribute -> { th; ~content; ~th_attribute }))
      | "img" -> parse_with(img_attrs, (img_attribute -> { img; ~img_attribute }))
      // Head
      | "head" -> {success = { head; ~content } }
      | "link" -> parse_with(link_attrs, (link_attribute -> { link; ~link_attribute }))
      | "title" -> parse_title_tag(content)
      | "meta" -> parse_with(meta_attrs, (meta_attribute -> { meta; ~meta_attribute }))
      | "base" -> parse_with(base_attrs, (base_attribute -> { base; ~base_attribute }))
      | "header" -> standard_tag({header})
      | "footer" -> standard_tag({footer})
      | "em" -> standard_tag({em})
      | "strong" -> standard_tag({strong})
      | tag -> { failure = {unsupported_tag; ~tag; ns="" } }
      end
    | { ~failure } -> { ~failure }
    end

  @private rec private_parse(conf :Template.conf, { ~xmlns_parser; ~xmlns }:Template.import_arg(void, 'b)): outcome(Template.content(either(void, 'b)), Template.failure) = (
    match xmlns with
    | { ~tag; ~args; ~content ... } -> parse_node(conf, tag, args, content, xmlns, xmlns_parser)
    | { ~fragment } -> Template.merge_outcome_content(List.map(xmlns_parser, fragment))
    | { ~text } -> {success = { checked_text=text } }
    | _ -> Template.error_unsupported_node(xmlns)
    end
  )

  @private standard_tag_export(tag : Template.standard_tag, children) =
    match tag with
    | {div} -> <div>{children}</>
    | {address} -> <address>{children}</>
    | {acronym} -> <acronym>{children}</>
    | {fieldset} -> <fieldset>{children}</>
    | {legend} -> <legend>{children}</>
    | {pre} -> <pre>{children}</>
    | {sub} -> <sub>{children}</>
    | {sup} -> <sup>{children}</>
    | {abbr} -> <abbr>{children}</>
    | {ins} -> <ins>{children}</>
    | {del} -> <del>{children}</>
    | {dd} -> <dd>{children}</>
    | {dt} -> <dt>{children}</>
    | {dl} -> <dl>{children}</>
    | {h1} -> <h1>{children}</>
    | {h2} -> <h2>{children}</>
    | {h3} -> <h3>{children}</>
    | {h4} -> <h4>{children}</>
    | {h5} -> <h5>{children}</>
    | {h6} -> <h6>{children}</>
    | {open} -> <open>{children}</>
    | {span} -> <span>{children}</>
    | {menu} -> <menu>{children}</>
    | {ul} -> <ul>{children}</>
    | {ol} -> <ol>{children}</>
    | {li} -> <li>{children}</>
    | {paragraph} -> <p>{children}</>
    | {caption} -> <caption>{children}</>
    | {table} -> <table>{children}</>
    | {thead} -> <thead>{children}</>
    | {tbody} -> <tbody>{children}</>
    | {tr} -> <tr>{children}</>
    | {tfoot} -> <tfoot>{children}</>
    | {header} -> <header>{children}</>
    | {footer} -> <footer>{children}</>
    | {em} -> <em>{children}</>
    | {strong} -> <strong>{children}</>

  @private standard_tag_to_string(tag : Template.standard_tag) =
    match tag with
    | {div} -> "div"
    | {address} -> "address"
    | {acronym} -> "acronym"
    | {fieldset} -> "fieldset"
    | {legend} -> "legend"
    | {pre} -> "pre"
    | {sub} -> "sub"
    | {sup} -> "sup"
    | {abbr} -> "abbr"
    | {ins} -> "ins"
    | {del} -> "del"
    | {dd} -> "dd"
    | {dt} -> "dt"
    | {dl} -> "dl"
    | {h1} -> "h1"
    | {h2} -> "h2"
    | {h3} -> "h3"
    | {h4} -> "h4"
    | {h5} -> "h5"
    | {h6} -> "h6"
    | {open} -> "open"
    | {span} -> "span"
    | {menu} -> "menu"
    | {ul} -> "ul"
    | {ol} -> "ol"
    | {li} -> "li"
    | {paragraph} -> "p"
    | {caption} -> "caption"
    | {table} -> "table"
    | {thead} -> "thead"
    | {tbody} -> "tbody"
    | {tr} -> "tr"
    | {tfoot} -> "tfoot"
    | {header} -> "header"
    | {footer} -> "footer"
    | {em} -> "em"
    | {strong} -> "strong"

  /**
  * The default engine : It can process default tags like div or span.
  */
  base_engine : Template.engine(void, 'b) = {
    parse(conf :Template.conf, import_args:Template.import_arg(void, 'b)): outcome(Template.content(either(void, 'b)), Template.failure) = private_parse(conf, import_args)
   ; export(content:Template.content('a), exporter):outcome(xhtml, Template.export_failure) = (
    content_children = base_extract_children(content)
    child = Template.export_list_content(content_children, exporter)
    do_export(export_f, attrs, tag) =
      add_attributes_to_xhtml(export_f.export(attrs), tag)
    match content with
    | {standard_tag={open}; content=_;
       standard_attribute=_}                     -> {success = <>{child}</>}
    | {~standard_tag; content=_;
       ~standard_attribute}                      -> do_export(std_attr, standard_attribute, standard_tag_export(standard_tag, child))
    | {anchor; content=_; ~anchor_attribute}     -> do_export(anchor_attrs, anchor_attribute, <a>{child}</> )
    | {blockquote; content=_; ~quote_attribute}  -> do_export(quote_attrs, quote_attribute, <blockquote>{child}</> )
    | {quote; content=_; ~quote_attribute}       -> do_export(quote_attrs, quote_attribute, <q>{child}</> )
    | {hr; ~standard_attribute}                  -> do_export(std_attr, standard_attribute, <hr/>)
    | {br; ~standard_attribute}                  -> do_export(std_attr, standard_attribute, <br/>)
    | {label; content=_; ~label_attribute}       -> do_export(label_attrs, label_attribute, <label>{child}</> )
    | {form; content=_; ~form_attribute}         -> do_export(form_attr, form_attribute, <form>{child}</>)
    | {input; ~input_attribute}                  -> do_export(input_attrs, input_attribute, <input/>)
    | {textarea; content=_; ~textarea_attribute} -> do_export(textarea_attrs, textarea_attribute, <textarea>{child}</>)
    | {option; content=_; ~option_attribute}     -> do_export(option_attrs, option_attribute, <option>{child}</>)
    | {select; content=_; ~select_attribute}     -> do_export(select_attrs, select_attribute, <select>{child}</>)
    | {optgroup; content=_; ~optgroup_attribute} -> do_export(optgroup_attrs, optgroup_attribute, <optgroup>{child}</>)
    | {td; content=_; ~td_attribute}             -> do_export(td_attrs, td_attribute, <td>{child}</>)
    | {th; content=_; ~th_attribute}             -> do_export(td_attrs, th_attribute, <th>{child}</>)
    | {img; ~img_attribute}                      -> do_export(img_attrs, img_attribute, <img />)
    | {meta; ~meta_attribute}                    -> do_export(meta_attrs, meta_attribute, <meta /> )
    | {base; ~base_attribute}                    -> do_export(base_attrs, base_attribute, <base /> )
    | {link; ~link_attribute}                    -> do_export(link_attrs, link_attribute, <link />)
    | {~text}                                    -> {success = <>{text}</>}
    | {~checked_text}                            -> {success = Xhtml.of_string_unsafe(checked_text) }
    | {fragment=_}                               -> {success = child }
    | {head; content=_}                          -> {success = <head>{child}</> }
    | {~title}                                   -> {success = <title>{title}</> }
    | {extension=_}                              -> {failure = {unknown_tag = "{content}" } }
    )

   ; source(content:Template.content('a), exporter, _xmlns_binding, printer, depth) : outcome(string, Template.export_failure) =
       children = base_extract_children(content)
       child=String.concat("", Outcome.get(Template.merge_outcome(List.map(exporter, children))) )

       print_string_assoc(list_attribute:Template.args) : string =
         List.map( {~name; ~value; ~namespace} ->
           namespace = if namespace == "" then " " else " {namespace}:"
           "{namespace}{name}=\"{value}\"" , list_attribute)
         |> String.concat(" ", _)

       rec should_print_after(node) = match base_extract_children(node) with
        | [] -> true
        | lst -> match List.head(List.rev(lst) ) with
          | { checked_text=text }
          | { ~text } -> String.ws_length(String.reverse(text)) != 0
          | { standard_tag={pre}; ... } -> false
          | other -> should_print_after(other)
          end
        end

       print_html_tag(tag_name, attrs, autoclose) =
        attr = print_string_assoc(List.filter_map(identity, attrs))
        print_after = should_print_after(content)
        { success = Template.print_tag(tag_name, none, attr, autoclose, print_after, some(child)) }

       print_text_node(before, text) =
         if String.ws_length(text) > 0
           then "{before}{text}"
           else text

       print_tag(name, attr_f, attrs) =
         print_html_tag(name, attr_f.export(attrs), false)
       print_tag_auto_close(name, attr_f, attrs) =
         print_html_tag(name, attr_f.export(attrs), true)

       res = match content with
       | {~standard_tag; content=_; ~standard_attribute} -> print_tag(standard_tag_to_string(standard_tag), std_attr, standard_attribute)
       | {anchor; content=_; ~anchor_attribute }         -> print_tag("a", anchor_attrs, anchor_attribute)
       | {blockquote; content=_; ~quote_attribute }      -> print_tag("blockquote", quote_attrs, quote_attribute)
       | {quote; content=_; ~quote_attribute }           -> print_tag("q", quote_attrs, quote_attribute)
       | {hr; ~standard_attribute}                       -> print_tag_auto_close("hr", std_attr, standard_attribute)
       | {br; ~standard_attribute}                       -> print_tag_auto_close("br", std_attr, standard_attribute)
       | {label; content=_; ~label_attribute }           -> print_tag("label", label_attrs, label_attribute)
       | {form; content=_; ~form_attribute }             -> print_tag("form", form_attr, form_attribute)
       | {input; ~input_attribute }                      -> print_tag_auto_close("input", input_attrs, input_attribute)
       | {textarea; content=_; ~textarea_attribute }     -> print_tag("textarea", textarea_attrs, textarea_attribute)
       | {option; content=_; ~option_attribute }         -> print_tag("option", option_attrs, option_attribute)
       | {select; content=_; ~select_attribute }         -> print_tag("select", select_attrs, select_attribute)
       | {optgroup; content=_; ~optgroup_attribute }     -> print_tag("optgroup", optgroup_attrs, optgroup_attribute)
       | {td; content=_; ~td_attribute }                 -> print_tag("td", td_attrs, td_attribute)
       | {th; content=_; ~th_attribute }                 -> print_tag("th", td_attrs, th_attribute)
       | {img; ~img_attribute }                          -> print_tag("img", img_attrs, img_attribute)
       | {meta; ~meta_attribute }                        -> print_tag_auto_close("meta", meta_attrs, meta_attribute)
       | {base; ~base_attribute }                        -> print_tag_auto_close("base", base_attrs, base_attribute)
       | {link; ~link_attribute }                        -> print_tag_auto_close("link", link_attrs, link_attribute)
       | {head; content=_ }                              -> print_html_tag("head", [], false)
       | {~title}                                        -> {success = Template.print_tag("title", none, "", false, true,some(title)) }
       | {~text}                                         -> {success = (before, _ -> print_text_node(before, text)) }
       | {~checked_text}                                 -> {success = (before, _ -> print_text_node(before, checked_text)) }
       | {fragment=_}                                    -> {success = (_, _  -> "{child}") }
       | {extension=_}                                   -> {failure = {unknown_tag = "{content}"}}
       match res with
       | { ~success }                                      -> { success = printer(depth)(success) }
       | { ~failure }                                      -> { ~failure }

   ; cachable = _ -> false
}
}}
