/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Define a template system.
 *
 * @author Anthonin Bonnefoy, 2011-2011
 * @destination public
 * @category web
 * @stability experimental
 */

/**
 * {1 About this module}
 *
 * This module contains a high-level interface for parsing xml with
 * normal tags (span, div) and exporting them in xhtml.
 * This template system is designed to be safe and extensible. You
 * can extend the system so that it can process your own tags.
 *
 * {1 Where should I start?}
 *
 * For simple html parse, you can use the Template.default engine with
 * Template.parse for obtaining a Template.content tree.
 * {[ content = Template.parse(Template.default, "<div></div>") }
 *
 * You can store this structure in a database and use the Template.to_xhtml function
 * to produce the corresponding xhtml.
 * {[ Template.to_xhtml( Template.default, content) }
 *
 * You can also retrieve the xml sources from a given content with Template.to_source.
 * {[ Template.to_source( Template.default, content) }
 *
 * {1 What if I need more?}
 * If the base tags are not sufficient, you can create your own engine to parse and export your own tags
 * and combine it with existing engine.
 *
 * {2 A simple extension}
 * An engine is responsible to parse and to export a set of tags.
 * For example, we will create an engine which will support the <ns:response /> tag and combine it with the default engine.
 * First, we need to create the type sum for the tag
 * {[ type MyEngine.tag = { response } }
 * Next, we implement the engine. We start from Template.empty which is an engine with default
 * functions and implement parse (conversion from xml to content) and export functions.
 * {[
 * my_namespace = "http://response.xsd"
 *
 * my_engine = { Template.empty with
 *   // Parse function. It takes the xmlns, and a xmlns parse which is able to process xmlns into Template.content.
 *   // If the current engine is able to process the current, it should create a Template.content. Otherwise, it should fail.
 *   parse(_conf, {xmlns_parser=_; ~xmlns }) : outcome(Template.content(either(MyEngine.tag, 'b)), Template.failure) =
 *   match xmlns with
 *   // We check if the node namespace is correct
 *   | { ~tag; namespace="http://response.xsd" ... } ->
 *      match tag with
 *      // We use Template.to_extension to encapsulate our type in the Template.content tree.
 *      | "response" -> { success = Template.to_extension({ response }) }
 *      | _ -> { failure = { unsupported_tag; ns=my_namespace; ~tag } }
 *      end
 *   | _ -> { failure = { unsupported_node = xmlns } }
 *  // Export function. It takes a Template.content tree and create the xhtml.
 *  export(content, _) =
 *     // We use Template.from_extension to extract the
 *     // extension tag if it exists
 *     match Template.from_extension(content) with
 *     | { none } -> { failure = { unknown_tag = "Expected extension" } }
 *     | { some=opa_tag } ->
 *       match opa_tag with
 *       | { response } -> { success = <>42</> }
 *       end
 * }
 * }
 *
 * This is enough to create an engine for our "response" tag. We will combine it
 * with the default engine in order to use our new tag in a html tree.
 *
 * {[
 * engine = Template.combine(Template.default, my_engine)
 * content = Template.parse(engine,
 *   "<div xmlns:my=\"http://response.xsd\">
 *     <my:response />
 *   </div>"
 * )
 * xhtml = Template.to_xhtml(engine, content)
 * }
 * The resulting xhtml should be something like
 * {[ <div>42</div> }
 *
 * {2 Manage attribute}
 * To see how manage attribute in our tags, we will create a "date" tag which will take
 * the date format as an attribute.
 *
 *  {[
 *
 *   // Our engine type
 *   type DateEngine.tag = { date; format:string }
 *
 *   default_date_format : string = "%d/%m/%y"
 *
 *   date_attribute = "format"
 *
 *   // Parse function for date tag. It seeks if a "format" attribute exists and fetch its value.
 *   parse_date_tag(args:Template.args) : DateEngine.tag =
 *     match List.find( { namespace=_; ~name; value=_ } -> name == date_attribute, args) with
 *     | {some = {value=format ... } } -> { date; ~format }
 *     | { none } -> { date; format=default_date_format }
 *
 *   // The export to xhtml function
 *   date_html(date_format)=
 *     match Date.try_generate_printer(date_format) with
 *     | { success=date_printer } -> <>{Date.to_formatted_string(date_printer, Date.now())}</>
 *     | { ~failure } -> <>Incorrect date format {failure}</>
 *
 *   // The engine implementation.
 *   date_engine = { Template.empty with
 *     parse(_conf, {~xmlns; xmlns_parser=_}) =
 *       match xmlns with
 *       | {tag="date"; ~args; namespace=_; content=_; specific_attributes=_ } -> { success = Template.to_extension(parse_date_tag(args)) }
 *       | _ -> { failure = { unsupported_node=xmlns } }
 *
 *    export(content, _) =
 *      match Template.from_extension(content) with
 *      | { none } -> { failure = { unknown_tag = "Expected extension" } }
 *      | { some=date_tag } ->
 *        match date_tag with
 *        | { date; ~format} -> { success = date_html(format) }
 *        end
 *   }
 * }
 * To use this engine, same thing
 * {[
 * html_date_engine = Template.combine(Template.default, date_engine)
 *
 * content = Template.parse(html_date_engine,
 *   "<div>
 *     <date format=\"%d %m\" />
 *   </div>")
 *
 * xhtml = Template.to_xhtml(html_date_engine, content)
 * }
 * The resulting xhtml should the date to the desired format.
 *
 * {2 Support children in tag}
 * To allow children in tags, you have to create a record with a field containing the children.
 * We will try to create a tag "headfoot" which wrap its' content between a <div>head</div> and a <div>foot</div>.
 * First, the type :
 * {[
 * type HeadFoot.content('a) = Template.content(either(head_foot_tag('a), 'a) )
 * type head_foot_tag('a) = { header_footer; children: HeadFoot.content('a) }
 * }
 *
 * {[
 *
 *    head_foot_engine : Template.engine(head_foot_tag('a), 'a) = { Template.empty with
 *      // Parse the tag. This time, we put the children passed in argument in the record.
 *      parse(_conf, {~xmlns; ~xmlns_parser} ) =
 *       match xmlns with
 *       | {tag="headfoot"; ~content ... } ->
 *          children = Template.parse_list_xmlns(content, xmlns_parser)
 *          {success = Template.to_extension( { header_footer; ~children } ) }
 *       | _ -> { failure = { unsupported_node=xmlns } }
 *
 *      // Export the tag in xhtml. The children is already transformed in xhtml and
 *      // provided as an argument. We just need to wrap our child in a header/footer
 *      export(content, child) =
 *        head_foot_html() =
 *          <><div>Header</div>{child}<div>Footer</div></>
 *        match Template.from_extension(content) with
 *        | { none } -> { failure = { unknown_tag = "Expected extension" } }
 *        | { some=head_foot_tag } ->
 *          match head_foot_tag with
 *          | { header_footer; children=_ } -> { success = head_foot_html() }
 *          | _ -> { failure = { unknown_tag = "" } }
 *          end
 *    }
 * }
 * Now, we can use our headfoot tag.
 * {[
 *   engine = Template.combine(Template.default, head_foot_engine)
 *   content = Template.parse(engine,
 *     "<headfoot>
 *       The body
 *     </headfoot>")
 *   xhtml = Template.to_xhtml(engine, content)
 * }
 * This will give the given xhtml : <><div>head</div>The body<div>foot</div></>
 *
 * {2 Multiple extension}
 * To combine two engines, use the Template.combine function. However, engine combined with this function
 * can't be combined with another engine.
 * If you wish to combine more than two engines, you need to use the Template.extend function with the
 * firsts engines and use Template.combine with the last two engines.
 * For example, we can combine the default engine, my_engine and date_engine this way :
 * {[
 *   big_engine = Template.combine(Template.extend(Template.default, my_engine), date_engine)
 * }
 * The new engine will be able to process input with date and response tag.
 * {2 Source export}
 * You need to implement the source function on your engine to be able to convert a Template.content to a xml source.
 * The source function of the engine takes the node, the already processed children of this node, the map of xmlns binding
 * and a printer for source indentation.
 * In our headfoot engine, we can to implement source as the following :
 * {[
 *  source(content, content_exporter, _xmlns_binding, printer, depth) =
 *   match Template.from_extension(content) with
 *   | { none } -> { failure = { unknown_tag = "Expected extension" } }
 *   | { some={header_footer; ~children } } ->
 *     child = Outcome.get(content_exporter(children) )
 *     res = Template.print_tag("headfoot", none, "", false, true, some(child) )
 *     { success = printer(depth)(res) }
 * }
 * With this, a {[ Template.to_source(head_foot_engine, content) } will be able to print the headfoot tags correctly.
 *
 */
Template =
{{

  default_conf = { strict = false ; fallback_to_text_node = true}

  @private map_content(translation:('a -> 'b), content:Template.content('a)) : Template.content('b) =
    match content with
    | { ~standard_tag; ~content; ~standard_attribute } -> { ~standard_tag; content=map_content(translation, content); ~standard_attribute}
    | { anchor; ~anchor_attribute; ~content } -> {anchor; ~anchor_attribute; content=map_content(translation, content) }
    | { blockquote; ~content; ~quote_attribute } -> { blockquote; content=map_content(translation, content); ~quote_attribute}
    | { quote; ~content; ~quote_attribute } -> { quote; content=map_content(translation, content); ~quote_attribute}
    | { ~fragment } -> { fragment = List.map(el -> map_content(translation, el), fragment) }
    | { ~text } -> { ~text }
    | { ~checked_text } -> { ~checked_text }
    | { ~extension } -> { extension = translation(extension) }
    | { br ; ~standard_attribute } -> { br ; ~standard_attribute }
    | { form ; ~content ; ~form_attribute } -> { form ; content=map_content(translation, content) ; ~form_attribute }
    | { hr ; ~standard_attribute } -> { hr ; ~standard_attribute }
    | { img ; ~img_attribute } -> { img ; ~img_attribute }
    | { input ; ~input_attribute } -> { input ; ~input_attribute }
    | { label ; ~content ; ~label_attribute } -> { label ; content=map_content(translation, content) ; ~label_attribute }
    | { optgroup ; ~content ; ~optgroup_attribute } -> { optgroup ; content=map_content(translation, content) ; ~optgroup_attribute }
    | { option ; ~content ; ~option_attribute } -> { option ; content=map_content(translation, content) ; ~option_attribute }
    | { select ; ~content ; ~select_attribute } -> { select ; content=map_content(translation, content) ; ~select_attribute }
    | { td ; ~content ; ~td_attribute } -> { td ; content=map_content(translation, content) ; ~td_attribute }
    | { textarea ; ~content ; ~textarea_attribute } -> { textarea ; content=map_content(translation, content) ; ~textarea_attribute }
    | { th ; ~content ; ~th_attribute } -> { th ; content=map_content(translation, content) ; ~th_attribute }
    | { link ; ~link_attribute } -> { link; ~link_attribute }
    | { head ; ~content } -> { head ; content=map_content(translation, content) }
    | { ~title } -> { ~title }
    | { meta; ~meta_attribute } -> { meta; ~meta_attribute }
    | { base; ~base_attribute } -> { base; ~base_attribute }

  abstract_map_xmlns_parser(fun:(xmlns -> outcome(Template.content('a), Template.failure)), mapper:(Template.content('a) -> Template.content('b) )):(xmlns -> outcome(Template.content('b), Template.failure)) =
    xmlns -> match fun(xmlns) with
      | { ~success} -> {success= mapper(success)}
      | { ~failure } -> { ~failure }


  @private map_content_right_shift(content:Template.content(either(either('a, 'b), 'c))):Template.content(either('a, either('b,'c))) =
    map_content(
      cont -> match cont with
      | { ~right } -> { right = { ~right } }
      | { left = { ~right } } -> { right = { left = right } }
      | { left = { ~left } } -> { ~left }
      , content)

  @private xmlns_parser_right_shift(fun) =
    abstract_map_xmlns_parser(fun, map_content_right_shift)

  @private map_content_left_swap(content:Template.content(either(either('a, 'b), 'c))):Template.content(either(either('b, 'a), 'c)) =
    map_content(
      cont -> match cont with
      | { left = { ~right } } -> { left = { left = right } }
      | { left = { ~left } } -> { left = { right = left } }
      | { ~right } -> { ~right }
      , content)

  @private xmlns_parser_left_swap(fun:(xmlns -> outcome(Template.content(either(either('a, 'b), 'c)), Template.failure ) )):(xmlns -> outcome(Template.content(either(either('b, 'a), 'c)), Template.failure) ) =
    abstract_map_xmlns_parser(fun, map_content_left_swap)

  @private map_content_left_shift(content:Template.content(either('a, either('b, 'c)))):Template.content(either(either('a, 'b), 'c)) =
    map_content(
      cont -> match cont with
      | { ~left } -> { left = { ~left } }
      | { right = { ~left } } -> { left = { right = left } }
      | { right = { ~right } } -> { ~right }
      , content)

  @private xmlns_parser_left_shift(fun) =
    abstract_map_xmlns_parser(fun, map_content_left_shift)

  @private map_content_swap(content:Template.content(either('a,'b))):Template.content(either('b,'a) ) = map_content(a ->
    match a with
    | { ~right } -> { left = right }
    | { ~left } -> { right = left }
    , content)

  @private xmlns_parser_swap(fun:(xmlns -> outcome(Template.content(either('a,'b) ), Template.failure ) ) ) : (xmlns -> outcome(Template.content(either('b, 'a)), Template.failure) ) =
    abstract_map_xmlns_parser(fun, map_content_swap)

  /**
    * {1 Import and export interface}
    */

  rec try_of_xmlns_with_conf(conf:Template.conf, engine:Template.engine('a, 'b), input:xmlns) : outcome(Template.content(either('a, 'b)), Template.failure ) =
    engine.parse(conf, {xmlns_parser=(xmlns -> try_of_xmlns_with_conf(conf, engine, xmlns) ); xmlns=input})

  /**
   * Convert a xmlns using the specified engine into a Template.content tree.
   * It may fails if a tag is not known from the engine
   */
  try_of_xmlns(engine:Template.engine('a, 'b), input:xmlns) : outcome(Template.content(either('a, 'b)), Template.failure ) =
    try_of_xmlns_with_conf(default_conf, engine, input)

  /**
   * Try to parse the input string into a template content.
   * If the xml is invalid, the input is stored as a text node
   * If the engine can't process the xml, it returns an error.
   */
  try_parse_with_conf(conf:Template.conf, engine:Template.engine('a, 'b), input:string) : outcome(Template.content(either('a, 'b)), Template.failure ) =
  (
    remove_span(content:Template.content(either('a, 'b))) =
      match content with
      | { standard_tag={span}; ~content  ... } -> content
      | _ -> @fail

    match Xmlns.try_parse(input) with
    | { ~some } -> try_of_xmlns_with_conf(conf, engine, some)
    | { none } -> (
      // TODO : Remove this hack when the xml parser is able to parse fragment at the root of document
      match Xmlns.try_parse("<span>{input}</span>") with
      | { ~some } -> (
        match try_of_xmlns_with_conf(conf, engine, some) with
        | { ~success } -> { success = remove_span(success) }
        | { ~failure } -> { ~failure }
      )
      | { none } ->
        if conf.fallback_to_text_node
          then { success = { text = input } }
          else { failure = {xml_parse_error = "The given input is not a correct xml." } }
    )
  )

  try_parse(engine:Template.engine('a, 'b), input:string) : outcome(Template.content(either('a, 'b)), Template.failure ) =
    try_parse_with_conf(default_conf, engine, input)

  /**
   * Parse the input string into a Template content
   * If the engine can't process the xml, the input is stored as a text node
   */
  parse_with_conf(conf, engine:Template.engine('a, 'b), input:string) : Template.content(either('a, 'b)) =
  (
    match try_parse_with_conf(conf, engine, input) with
    | { ~success } -> success
    | { failure=_ } -> { text = input }
  )

  parse(engine:Template.engine('a, 'b), input:string) : Template.content(either('a, 'b)) =
    parse_with_conf(default_conf, engine, input)

  /**
   * Convert a Template.content using the engine into a xhtml. It may fails if the engine is unable to
   * export an element inside the content tree
   */
  rec to_xhtml(engine:Template.engine('a, 'b), content:Template.content(either('a, 'b))) : xhtml =
    exporter(content) = { success = to_xhtml(engine, content) }
    Outcome.get(engine.export(content, exporter ))

  /**
   * Export the ast into a string representing the xml source
   */
  to_source(engine:Template.engine('a,'b), content:Template.content(either('a,'b)) ):string =
    to_source_with_printer(engine, content, default_printer)

  /**
   * Export the ast into a string representing the xml source with the specific printer.
   */
  to_source_with_printer(engine:Template.engine('a,'b), content:Template.content(either('a,'b)), printer:(int -> Template.printer) ):string =
    add_xmlns_binding(content:Template.content(either('a,'b)), xml_bindings:stringmap(string)) :stringmap(string) =
      match content with
      | { ~standard_attribute ... } ->
        List.fold_left( (acc, el ->
          StringMap.add(el.uri, el.name, acc)
          )
          , xml_bindings
          , standard_attribute.local_binding
        )
      | _ -> xml_bindings
      end
    rec process_source(content:Template.content(either('a,'b) ), xmlns_binding) =
      xmlns_binding = add_xmlns_binding(content, xmlns_binding)
      exporter(content) = { success = process_source(content, xmlns_binding) }
      Outcome.get(engine.source(content, exporter, xmlns_binding, printer, 0))

    process_source(content, StringMap_empty)

  /**
    * {1 Ast manipulation}
    */

  /**
   * Create a text node
   */
  text(txt:string):Template.content(either('a, 'b) ) = { text = txt }

  /**
   * Create a title node
   */
  title(txt:string):Template.content(either('a, 'b) ) = { title = txt }

  fragment(lst) : Template.content(either('a, 'b) ) = { fragment = lst }

  /**
    * {1 Extensibility interface}
    */

  @private rec private_combine_parse(engine_a, engine_b, conf, import_arg:Template.import_arg('a, 'b)):outcome(Template.content(either('a,'b)), Template.failure) = (
    { ~xmlns_parser ~xmlns } = import_arg
    parsing_a:(xmlns -> outcome(Template.content(either('a,'b)), Template.failure)) =
      xmlns ->
        private_combine_parse(engine_a, engine_b, conf, { ~xmlns_parser; ~xmlns } )

    parsing_b:(xmlns -> outcome(Template.content(either('b,'a)), Template.failure)) =
      xmlns_parser_swap(
        xmlns ->
        private_combine_parse(engine_a, engine_b, conf, { ~xmlns_parser; ~xmlns } )
      )

    match engine_a.parse(conf, { ~xmlns; xmlns_parser=parsing_a } ) with
    | { ~success } -> { ~success }
    | { failure=failure_a } ->
      match engine_b.parse(conf, { ~xmlns; xmlns_parser=parsing_b } ) with
        | { ~success } -> {success = map_content_swap(success) }
        | { failure=failure_b } ->
          { failure={ multiple_failure=[failure_a, failure_b] } }
      end
    )

  @private rec abstract_combine_map(processor_a, processor_b, content, exporter, depth)=
  a_exporter(content) = abstract_combine_map(processor_a, processor_b, content, exporter, depth+1)
  b_exporter(content) = abstract_combine_map(processor_a, processor_b, map_content_swap(content), exporter, depth+1)
  match processor_a(content, a_exporter, depth) with
  | { ~success } -> { ~success }
  | { failure=_ } -> processor_b(map_content_swap(content), b_exporter, depth)

  @private rec private_combine_export(engine_a, engine_b, content, exporter):outcome(xhtml, Template.export_failure)  =
    processor_a(content, exporter, _) = engine_a.export(content, exporter)
    processor_b(content, exporter, _) = engine_b.export(content, exporter)
    abstract_combine_map(processor_a, processor_b, content, exporter, 0)

  @private rec private_combine_source(engine_a, engine_b, content, exporter, xmlns_binding, printer, depth):outcome(string, Template.export_failure) =
    processor_a(content,exporter,depth) = engine_a.source(content, exporter, xmlns_binding, printer, depth)
    processor_b(content,exporter,depth) = engine_b.source(content, exporter, xmlns_binding, printer, depth)
    abstract_combine_map(processor_a, processor_b, content, exporter, depth)

  /**
   * Combine two engine into a new engine.
   * This new engine is capable of processing all tags from the first and the second engine.
   * Warning : Engine combined with this function can't be combined anymore. To combine multiple engines, use the extend function.
   */
  combine(engine_a:Template.engine('a, 'b), engine_b:Template.engine('b, 'a)): Template.engine('a, 'b) = {{

    parse(conf, import_arg:Template.import_arg('a, 'b)):outcome(Template.content(either('a,'b)), Template.failure) =
      private_combine_parse(engine_a, engine_b, conf, import_arg)

    export(content:Template.content(either('a, 'b)), exporter:(Template.content(either('a, 'b) ) -> outcome(xhtml, Template.export_failure) )):outcome(xhtml, Template.export_failure) =
     private_combine_export(engine_a, engine_b, content, exporter)

    cachable(content:Template.content(either('a, 'b) ) ) : bool =
      engine_a.cachable(content) || engine_b.cachable(map_content_swap(content))

    source(content : Template.content(either('a, 'b)), exporter:(Template.content(either('a,'b)) -> outcome(string, Template.export_failure) ), xmlns_binding:stringmap(string), printer:(int -> Template.printer), depth ) : outcome(string, Template.export_failure) =
      private_combine_source(engine_a, engine_b, content, exporter, xmlns_binding, printer, depth)
  }}

  @private rec private_extend_parse(engine_a, engine_b, conf, import_arg:Template.import_arg(either('a, 'b), 'c)) = (
    { ~xmlns_parser ~xmlns } = import_arg
    parsing_a:(xmlns -> outcome(Template.content(either('a, either('b, 'c))), Template.failure)) =
      xmlns_parser_right_shift(
        xmlns -> match xmlns_parser(xmlns) with
          | {~success } -> {~success}
          | {failure=failure_a} ->
            match private_extend_parse(engine_a, engine_b, conf, { ~xmlns_parser; ~xmlns } ) with
            | { ~success } -> { ~success }
            | { failure=failure_b } -> multiple_failure([failure_a, failure_b])
      )

    parsing_b:(xmlns -> outcome(Template.content(either('b, either('a, 'c))), Template.failure)) =
      xmlns_parser_right_shift(xmlns_parser_left_swap(
        xmlns -> match xmlns_parser(xmlns) with
          | {~success } -> {~success}
          | {failure=failing_a} -> match private_extend_parse(engine_a, engine_b, conf, { ~xmlns_parser; ~xmlns } ) with
            | { failure=failure_b } -> multiple_failure([failing_a, failure_b] )
            | { ~success } -> { ~success }
            end
      ))

    match engine_a.parse(conf, { ~xmlns; xmlns_parser=parsing_a } ) with
    | { ~success } -> { success=map_content_left_shift(success) }
    | { failure=failure_a } ->
      parsed_b : outcome(Template.content(either(either('a, 'b), 'c)), Template.failure) =
        match engine_b.parse(conf, { ~xmlns; xmlns_parser=parsing_b } ) with
        | { ~success } -> {success = map_content_left_swap(map_content_left_shift(success)) }
        | { failure=failure_b } -> multiple_failure([failure_a, failure_b])
      end
      parsed_b
    )

  @private rec abstract_extend_export(processor_a, processor_b, content, exporter, depth)=
    a_arg : Template.content(either('a, either('b, 'c) ) ) = map_content_right_shift( content )
    b_arg : Template.content(either('b, either('a, 'c) ) ) = map_content_right_shift( map_content_left_swap(content))
    a_exporter(content) = match exporter(map_content_left_shift(content)) with
    | { ~success } -> { ~success }
    | { failure=_ } -> abstract_extend_export(processor_a, processor_b, map_content_left_shift(content), exporter, depth+1)
    b_exporter(content) = match exporter(map_content_left_swap(map_content_left_shift(content))) with
    | { ~success } -> { ~success }
    | { failure=_ } -> abstract_extend_export(processor_a, processor_b, map_content_left_swap(map_content_left_shift(content)), exporter, depth+1)
    match processor_a(a_arg, a_exporter, depth) with
    | { ~success } -> { ~success }
    | { failure=_ } -> match processor_b(b_arg, b_exporter, depth) with
      | { ~success } -> { ~success }
      | { ~failure } -> { ~failure }

  @private rec private_extend_export(engine_a, engine_b, content:Template.content(either(either('a, 'b), 'c)), exporter:(Template.content(either(either('a, 'b), 'c)) -> outcome(xhtml, Template.export_failure) )) =
    processor_a(content, exporter, _)= engine_a.export(content, exporter)
    processor_b(content, exporter, _)= engine_b.export(content, exporter)
    abstract_extend_export(processor_a, processor_b, content, exporter, 0)

  @private rec private_extend_source(engine_a, engine_b, content:Template.content(either(either('a, 'b), 'c)), exporter:(Template.content(either(either('a, 'b), 'c)) -> outcome(string, Template.export_failure) ), xmlns_binding, printer, depth) =
    processor_a(content,exporter, depth) = engine_a.source(content, exporter, xmlns_binding, printer, depth)
    processor_b(content,exporter, depth) = engine_b.source(content, exporter, xmlns_binding, printer, depth)
    abstract_extend_export(processor_a, processor_b, content, exporter, depth)

  /**
   * Extend an engine with another one. The resulting engine can be extended with another engine using extend or combine.
   * Warning : An extended engine must be combined with another engine in order to be usable
   */
  extend(engine_a:Template.engine('a, either('b, 'c)), engine_b:Template.engine('b, either('a, 'c))): Template.engine(either('a, 'b), 'c) = {

    parse(conf, import_arg:Template.import_arg(either('a, 'b), 'c)) = private_extend_parse(engine_a, engine_b, conf, import_arg)

    export(content:Template.content(either(either('a, 'b), 'c)), exporter):outcome(xhtml, Template.export_failure) =
      private_extend_export(engine_a, engine_b, content, exporter)

    cachable(content:Template.content(either(either('a, 'b), 'c ) ) ) : bool =
      engine_a.cachable(map_content_right_shift(content)) || engine_b.cachable(map_content_right_shift(map_content_left_swap(content)))

    source(content : Template.content(either(either('a, 'b), 'c)), exporter, xmlns_binding:stringmap(string), printer:(int -> Template.printer), depth:int) : outcome(string, Template.export_failure) =
      private_extend_source(engine_a, engine_b, content, exporter, xmlns_binding, printer, depth)
  }

  /*to_cached_content : engine(a), content(a) -> content(either(a, unsafe_cached))*/

  /**
   * Encapsultate a type as a content extension
   */
  to_extension(el : 'a ) : Template.content(either('a, 'b) )  =
    { extension = { left = el } }

  /**
   * Extract the type from the extension
   */
  from_extension(content: Template.content(either('a, 'b) )) : option('a)  =
    match content with
    | { extension = { ~left } } -> some(left)
    | _ -> none

  /**
   * {1 Parse and string_of functions}
   */

  parse_dir(str : string) : option(Template.dir) = match str with
    | "ltr" -> some({ ltr })
    | "rtl" -> some({ rtl })
    | _ -> none

  parse_method(str:string) : option(Template.form_method) = match str with
    | "get" -> some({get})
    | "post" -> some({post})
    | _ -> none

  parse_enctype(str:string) : option(Template.form_enctype) = match str with
    | "application/x-www-form-urlencoded" -> some({application_x_www_form_urlencoded})
    | "multipart/form-data" -> some({multipart_form_data } )
    | "text/plain" -> some({text_plain})
    | _ -> none

  parse_input_type(str:string) : option(Template.input_type) = match str with
    | "button" ->   some({ button })
    | "checkbox" -> some({ checkbox })
    | "file" ->     some({ file })
    | "hidden" ->   some({ hidden })
    | "image" ->    some({ image })
    | "password" -> some({ password })
    | "radio" ->    some({ radio })
    | "reset" ->    some({ reset })
    | "submit" ->   some({ submit })
    | "text" ->     some({ text })
    | _ -> none

  parse_media(str:string) : option(Template.media) = match str with
    | "screen" -> some({ screen })
    | "tty" -> some({ tty })
    | "tv" -> some({ tv })
    | "projection" -> some({ projection })
    | "handheld" -> some({ handheld })
    | "print" -> some({ print })
    | "braille" -> some({ braille })
    | "aural" -> some({ aural })
    | "all" -> some({ all })
    | _ -> none

  parse_rel(str:string) : option(Template.rel) = match str with
    | "alternate" -> some({ alternate })
    | "appendix" -> some({ appendix })
    | "bookmark" -> some({ bookmark })
    | "chapter" -> some({ chapter })
    | "contents" -> some({ contents })
    | "copyright" -> some({ copyright })
    | "glossary" -> some({ glossary })
    | "help" -> some({ help })
    | "home" -> some({ home })
    | "index" -> some({ index })
    | "next" -> some({ next })
    | "prev" -> some({ prev })
    | "section" -> some({ section })
    | "start" -> some({ start })
    | "stylesheet" -> some({ stylesheet })
    | "subsection" -> some({ subsection })
    | _ -> none

  parse_rev(str:string) : option(Template.rev) = match str with
    | "alternate" -> some({ alternate })
    | "appendix" -> some({ appendix })
    | "bookmark" -> some({ bookmark })
    | "chapter" -> some({ chapter })
    | "contents" -> some({ contents })
    | "copyright" -> some({ copyright })
    | "glossary" -> some({ glossary })
    | "help" -> some({ help })
    | "home" -> some({ home })
    | "index" -> some({ index })
    | "next" -> some({ next })
    | "prev" -> some({ prev })
    | "section" -> some({ section })
    | "start" -> some({ start })
    | "stylesheet" -> some({ stylesheet })
    | "subsection" -> some({ subsection })
    | _ -> none

  parse_name(str:string) : option(Template.name) = match str with
    | "author" -> some({ author })
    | "description" -> some({ description })
    | "keywords" -> some({ keywords })
    | "generator" -> some({ generator })
    | "revised" -> some({ revised })
    | _ -> none

  string_of_dir(dir:Template.dir):string = match dir with
    | {rtl} -> "rtl"
    | {ltr} -> "ltr"

  string_of_method(method:Template.form_method):string = match method with
    | { get } -> "get"
    | { post } -> "post"

  string_of_enctype(enctype:Template.form_enctype):string = match enctype with
    | { application_x_www_form_urlencoded } -> "application/x-www-form-urlencoded"
    | { multipart_form_data } -> "multipart/form-data"
    | { text_plain } -> "text/plain"

  string_of_input_type(input_type:Template.input_type) : string = match input_type with
    | { button } -> "button"
    | { checkbox } -> "checkbox"
    | { file } -> "file"
    | { hidden } -> "hidden"
    | { image } -> "image"
    | { password } -> "password"
    | { radio } -> "radio"
    | { reset } -> "reset"
    | { submit } -> "submit"
    | { text } -> "text"

  rec string_of_template_failure(failure:Template.failure) : string = match failure with
    | { unsupported_tag; ~tag; ~ns } -> "Unsupported tag. Tag name={tag}, namespace={ns}"
    | { ~attribute_tag_error } -> attribute_tag_error
    | { ~unknown_attribute } -> "Unknown attribute : {unknown_attribute}. "
    | { ~attribute_required } -> attribute_required
    | { ~xml_parse_error } -> "Xml parse error : {xml_parse_error}"
    | { ~namespace_not_supported_by_engine } -> "Unsupported namespace : {namespace_not_supported_by_engine}"
    | { ~multiple_failure } ->
        list_errors = List.map(string_of_template_failure, multiple_failure)
        List.to_string_using("Multiple failure :\n", "", "\n", list_errors)
    | { ~dom_error } -> "Error in dom structure : {dom_error}"
    | { ~unsupported_node } -> "unsupported_node : {unsupported_node}"
    | { empty_engine } -> "empty_engine"

  rec xhtml_of_template_failure(failure:Template.failure) : xhtml = match failure with
    | { ~multiple_failure } ->
        if List.is_empty(multiple_failure)
          then <></>
          else
            list_errors = List.map(xhtml_of_template_failure, multiple_failure)
            List.foldl(el, acc ->
              <>{acc}<br />{el}</>
              ,list_errors
              , <>Multiple failure : </>
            )
    | other -> <>{string_of_template_failure(other)}</>

  string_of_media(media:Template.media) : string = match media with
    | { screen } -> "screen"
    | { tty } -> "tty"
    | { tv } -> "tv"
    | { projection } -> "projection"
    | { handheld } -> "handheld"
    | { print } -> "print"
    | { braille } -> "braille"
    | { aural } -> "aural"
    | { all } -> "all"

  string_of_rel(rel:Template.rel) : string = match rel with
    | { alternate } -> "alternate"
    | { appendix } -> "appendix"
    | { bookmark } -> "bookmark"
    | { chapter } -> "chapter"
    | { contents } -> "contents"
    | { copyright } -> "copyright"
    | { glossary } -> "glossary"
    | { help } -> "help"
    | { home } -> "home"
    | { index } -> "index"
    | { next } -> "next"
    | { prev } -> "prev"
    | { section } -> "section"
    | { start } -> "start"
    | { stylesheet } -> "stylesheet"
    | { subsection } -> "subsection"

  string_of_rev(rev:Template.rev) : string = match rev with
    | { alternate } -> "alternate"
    | { appendix } -> "appendix"
    | { bookmark } -> "bookmark"
    | { chapter } -> "chapter"
    | { contents } -> "contents"
    | { copyright } -> "copyright"
    | { glossary } -> "glossary"
    | { help } -> "help"
    | { home } -> "home"
    | { index } -> "index"
    | { next } -> "next"
    | { prev } -> "prev"
    | { section } -> "section"
    | { start } -> "start"
    | { stylesheet } -> "stylesheet"
    | { subsection } -> "subsection"

  string_of_name(name:Template.name) : string = match name with
    | { author } -> "author"
    | { description } -> "description"
    | { keywords } -> "keywords"
    | { generator } -> "generator"
    | { revised } -> "revised"

  /**
   * {1 Default values}
   */

  /**
   * An empty engine with default options and default operations
   */
  empty : Template.engine('a, 'b) = {

    parse(_conf, _) = { failure = { empty_engine } }
   ; export(_, _)= { failure = {unknown_tag = "" } }
   ; cachable = _ -> false
   ; source = _, _, _, _, _ -> { failure = { unknown_tag = "" } }
  }

  /**
   *  Default printer which add line break and spaces for source indentation
   */
  default_printer(depth:int):Template.printer = str ->
    if depth == 0
      then str("","\n")
      else spaces = String.repeat(depth, "  ")
           str("\n{spaces}", "\n{spaces}")

  /**
   * The default engine which is able to process html
   */
  default = TemplateBaseEngine.base_engine

  /**
   * Create a tag printer for a xml tag.
   * The tag printer have placeholder before the two concerned node for pretty printing
   * If autoclose is active, the child is ignored.
   * @param tag_name The name of the node to print
   * @param may_binding The xmlns binding to add to your tag name. For example, tag_name toto and may_binding some(opa) will print <opa:toto />
   * @param attr The list of attributes
   * @param autoclose Specifies if the tag has no end tag
   * @param may_child The node's children.
   */
  print_tag(tag_name:string, may_binding:option(string), attr:string, autoclose:bool, print_after:bool, may_child:option(string)) : (string, string -> string) =
    (before, after ->
    after = if print_after
      then after
      else ""
    binding = Option.switch(a -> "{a}:", "", may_binding)
    if autoclose
      then "{before}<{binding}{tag_name}{attr}/>"
      else
        match may_child with
        | { some=child } -> "{before}<{binding}{tag_name} {attr}>{child}{after}</{binding}{tag_name}>"
        | { none } -> "{before}<{binding}{tag_name} {attr}></{binding}{tag_name}>"
    )

  /**
   *  Make Template content structure public
   */
  export_to_public : Template.content('a) -> Template.public_content('a) = identity

  /**
   *  Make Template content structure private
   */
  export_to_private : Template.public_content('a) -> Template.content('a) = identity

  merge_outcome(outcomes:list(outcome('a,'b) ) ): outcome(list('a), list('b) ) =
    failures = List.filter(Outcome.is_failure, outcomes)
    if List.is_empty(failures)
      then {success = List.map(Outcome.get, outcomes) }
      else
        {failure =
         List.filter_map(outcome ->
          match outcome with
          | { success=_ } -> {none}
          | { ~failure } -> {some=failure}
          , outcomes
         )
        }

  merge_outcome_content(outcomes) =
    match merge_outcome(outcomes) with
      | { ~success } -> {success = { fragment = success } }
      | { ~failure } -> { failure= { multiple_failure = failure } }

  parse_list_xmlns(children:list(xmlns), xmlns_parser:(xmlns -> outcome(Template.content('a), Template.failure ))):outcome(Template.content('a), Template.failure) =
    merge_outcome_content(List.map(xmlns_parser, children))

  export_list_content(content:list(Template.content), exporter):xhtml =
    outcome = merge_outcome(List.map(exporter, content) )
    <>{Outcome.get(outcome)}</>

  error_unsupported_tag(tag:string):outcome('a, Template.failure) =
    { failure = { unsupported_tag; ~tag; ns="" } }

  error_unsupported_node(node:xmlns):outcome('a, Template.failure) =
    { failure = { unsupported_node=node } }

  multiple_failure(lst_failure : list(Template.failure ) ):outcome('a, Template.failure) =
    {failure = { multiple_failure = lst_failure } }

}}
