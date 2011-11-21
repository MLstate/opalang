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

/**
 * Define types, AST, parse functions and to_string
 * function used in template system
 *
 * @author Anthonin Bonnefoy, 2011-2011
 * @destination public
 * @category web
 * @stability experimental
 */


/*
  Type definition below not anymore used. If must become used again, then
  remove the "private" and turn this type into @type unsafe_cached = string
*/
// type unsafe_cached = private(string)

type Template.args = list(Xml.attribute )
type Template.conf = {
  strict : bool
  ; fallback_to_text_node : bool
}

type Template.dir =
  { rtl }
/ { ltr }

/**
 * Manage attribute parse and export
 */
type Template.attribute('a) =
{
  parse : Template.conf, Template.args -> outcome('a, Template.failure)
  ; known_attributes : list(string)
  ; remove_known_attributes : Template.args -> Template.args
  ; export : 'a -> list(option(string_assoc(string)))
}

type Template.local_binding =
{
  name : string
  ; uri : string
}

/**
 *  Standard attributes
 */
type Template.standard_attribute =
  {
    title:option(string)
    ; class:option(string)
    ; dir:option(Template.dir)
    ; local_binding : list(Template.local_binding)
    ; free_attributes : Template.args
    ; id:option(string)
  }

/**
 * Form attributes
 */
type Template.form_method =
  { get }
/ { post }

type Template.form_enctype =
    { application_x_www_form_urlencoded }
  / { multipart_form_data }
  / { text_plain }

type Template.form_attribute =
  {
      accept : option(string)
    ; action:Uri.uri
    ; name : option(string)
    ; method : option(Template.form_method)
    ; enctype : option(Template.form_enctype)
    ; standard_attribute : Template.standard_attribute
  }

type Template.input_type =
  { button }
/ { checkbox }
/ { file }
/ { hidden }
/ { image }
/ { password }
/ { radio }
/ { reset }
/ { submit }
/ { text }

type Template.textarea_attribute = {
  cols : int
  ; rows : int
  ; disabled : option(string)
  ; name : option(string)
  ; readonly : option(string)
  ; standard_attribute : Template.standard_attribute
}

@abstract type Template.input_attribute = {
    accept : option(string)
  ; alt : option(string)
  ; checked : option(string)
  ; disabled : option(string)
  ; maxlength : option(int)
  ; name : option(string)
  ; readonly : option(string)
  ; size : option(int)
  ; src : option(Uri.uri)
  ; form_type : option(Template.input_type)
  ; value : option(string)
  ; standard_attribute : Template.standard_attribute
}

@abstract type Template.select_attribute = {
  disabled : option(string)
  ; multiple : option(string)
  ; name : option(string)
  ; size : option(int)
  ; standard_attribute : Template.standard_attribute
}

@abstract type Template.option_attribute = {
  disabled : option(string)
  ; label : option(string)
  ; selected : option(string)
  ; value : option(string)
  ; standard_attribute : Template.standard_attribute
}

@abstract type Template.optgroup_attribute = {
  label : string
  ; disabled : option(string)
  ; standard_attribute : Template.standard_attribute
}

type Template.align =
  { left }
  / { right }
  / { center }
  / { justify }
  / { char }

@abstract type Template.td_attribute = {
  abbr : option(string)
  ; colspan : option(int)
  ; rowspan : option(int)
  ; standard_attribute : Template.standard_attribute
}

type Template.media =
    { screen }
  / { tty }
  / { tv }
  / { projection }
  / { handheld }
  / { print }
  / { braille }
  / { aural }
  / { all }

type Template.rel =
  { alternate }
/ { appendix }
/ { bookmark }
/ { chapter }
/ { contents }
/ { copyright }
/ { glossary }
/ { help }
/ { home }
/ { index }
/ { next }
/ { prev }
/ { section }
/ { start }
/ { stylesheet }
/ { subsection }

type Template.rev =
  { alternate }
/ { appendix }
/ { bookmark }
/ { chapter }
/ { contents }
/ { copyright }
/ { glossary }
/ { help }
/ { home }
/ { index }
/ { next }
/ { prev }
/ { section }
/ { start }
/ { stylesheet }
/ { subsection }

@abstract type Template.link_attribute = {
  href    : option(Uri.uri)
  ; media : option(Template.media)
  ; rev   : option(Template.rev)
  ; rel   : option(Template.rel)
  ; mime  : option(string)
  ; standard_attribute : Template.standard_attribute
}

type Template.http_equiv =
  { content_type }
/ { content_style_type }
/ { expires }
/ { set_cookie }

type Template.name =
  { author }
/ { description }
/ { keywords }
/ { generator }
/ { revised }

@abstract type Template.meta_attribute = {
  meta_content:string
  ; name:option(Template.name)
  ; scheme:option(string)
  ; standard_attribute : Template.standard_attribute
}

type Template.anchor_attribute = {
  anchor_href : option(Uri.uri)
  ; target : option(string)
  ; standard_attribute : Template.standard_attribute
}

@abstract type Template.base_attribute = {
  base_href : Uri.absolute
  ; standard_attribute : Template.standard_attribute
}

@abstract type Template.img_attribute = {
  alt : string
  ; src : Uri.uri
  ; standard_attribute : Template.standard_attribute
}

@abstract type Template.label_attribute = {
  for : option(string)
  ; standard_attribute : Template.standard_attribute
}

@abstract type Template.quote_attribute = {
  cite : option(Uri.uri)
  ; standard_attribute : Template.standard_attribute
}

type Template.standard_tag =
  {div} / {address} / {acronym} / {fieldset} / {legend} / {pre} / {sub} / {sup} /
  {abbr} / {ins} / {del} / {dd} / {dt} / {dl} / {h1} / {h2} / {h3} / {h4} / {h5} /
  {h6} / {open} / {span} / {menu} / {ul} / {ol} / {li} / {paragraph} / {caption} /
  {table} / {thead} / {tbody} / {tr} / {tfoot} / {header} / {footer}

/**
 * An AST which represents a typed tree of
 * a xml. It can be extended to support more tags.
 */
type Template.public_content('a) =
  { anchor; content:Template.public_content('a); anchor_attribute:Template.anchor_attribute }
/ { standard_tag:Template.standard_tag; content:Template.public_content('a); standard_attribute:Template.standard_attribute }
/ { blockquote; content:Template.public_content('a); quote_attribute:Template.quote_attribute }
/ { quote; content:Template.public_content('a); quote_attribute:Template.quote_attribute }
/ { hr; standard_attribute:Template.standard_attribute }
/ { br; standard_attribute:Template.standard_attribute }
/ { img; img_attribute:Template.img_attribute }
/ { label; content:Template.public_content('a); label_attribute:Template.label_attribute }
/ { form; content:Template.public_content('a); form_attribute:Template.form_attribute }
/ { input; input_attribute:Template.input_attribute }
/ { textarea; content:Template.public_content('a); textarea_attribute:Template.textarea_attribute }
/ { option; content:Template.public_content('a); option_attribute:Template.option_attribute }
/ { optgroup; content:Template.public_content('a); optgroup_attribute:Template.optgroup_attribute }
/ { select; content:Template.public_content('a); select_attribute:Template.select_attribute }
/ { fragment:list(Template.public_content('a)) }
/ { text : string }
/ { checked_text : string }
/ { th; content:Template.public_content('a); th_attribute:Template.td_attribute }
/ { td; content:Template.public_content('a); td_attribute:Template.td_attribute }
/ { extension: 'a }
/ { head; content:Template.public_content('a) }
/ { title:string }
/ { base; base_attribute:Template.base_attribute }
/ { link; link_attribute:Template.link_attribute }
/ { meta; meta_attribute:Template.meta_attribute }

@abstract type Template.content('a) = Template.public_content('a)

/**
 * Argument passed to the engine during the import processing.
 * It contains the xml node along with the children to be inserted in the AST
 */
type Template.import_arg('a, 'b) =
{{
  xmlns_parser : (xmlns -> outcome( Template.content(either('a, 'b)), Template.failure ) )
  ; xmlns: xmlns  /** The vanilla xml node **/
}}

/**
 * Type for the base engine
 */
type Template.default_content = Template.content(either(void, void) )

/**
 * Error thrown during the import process
 */
type Template.failure =
  { unsupported_tag; tag :string; ns:string }
/ { unsupported_node:xmlns  }
/ { empty_engine }
/ { unknown_attribute : string }
/ { attribute_tag_error : string }
/ { attribute_required : string }
/ { dom_error : string }
/ { xml_parse_error : string }
/ { namespace_not_supported_by_engine : string}
/ { multiple_failure:list(Template.failure)}

/**
 * Error thrown during the export process
 */
type Template.export_failure =
  { unknown_tag : string }

/**
 * Printer used to decorate and indent xml nodes
 */
type Template.printer = (string, string -> string) -> string

/**
 * The engine responsible to parse and export tags
 * @param The first polymorphic type is the type-sum supported by the current engine
 * @param The second polymorphic type is the type-sum of elements which can be inserted in the the Template.content tree
 */
type Template.engine('a, 'b) =
{{
  parse: Template.conf, Template.import_arg('a, 'b) -> outcome(Template.content(either('a, 'b)), Template.failure) /** The parse function which convert an xml node into a Template.content **/
  ; export : Template.content(either('a, 'b)), (Template.content(either('a, 'b) ) -> outcome(xhtml, Template.export_failure) ) -> outcome(xhtml, Template.export_failure) /** Convert a Template.content into a xhtml structure **/
  ; source : Template.content(either('a, 'b)), (Template.content(either('a, 'b) ) -> outcome(string, Template.export_failure)), stringmap(string), (int -> Template.printer), int -> outcome(string, Template.export_failure) /** Convert a Template.content into a string source **/
  ; cachable : Template.content(either('a, 'b)) -> bool
}}
