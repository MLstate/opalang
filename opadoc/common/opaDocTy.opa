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
 * AST of datas manipulated by opadoc (comments, types, path, etc.)
 *
 * @author Adrien Jonquet 2010
 * @author Mathieu Barbin 2011 (layout, documentation)
**/

/**
 * {1 About the types}
 *
 * Some of these AST are read from json files produced by opa (api) during the
 * compilation. So, this code is interacting with the compiler, and in particular
 * with the passes {b AddDocApiDirectives} & {b DocApiGeneration}
**/

/**
 * {1 Comments}
 *
 * As opposed to types and values, comments is all the informations contained
 * in the enriched text.
**/

/**
 * Formats are used for structuring the documentation. A format is a style
 * applied to a part of the documentation. The syntax for using formatted
 * text is to use {{}} and using a char to indicate the selected format.
**/
type OpaDocTy.format =
  {paragraph} /
  {title : int} /
  {bold} /
  {italic} /
  {link : string} /
  {link_elt} /
  {emphasize} /
  {superscript} /
  {subscript} /
  {center} /
  {left} /
  {right} /
  {latex} /
  {code_pre} /
  {verbatim} /
  {custom} /
  {list} /
  {enum}

/**
 * One author
**/
type OpaDocTy.authors_line = string

/**
 * Once parsed, the comments are split into small shunks called [txt_element].
 * This types is a disjonction regrouping the different cases.
 *
 * The type is parametrized probably for the dynamic version, using
 * a type with more dynamic support (xhtml) rather than static strings.
**/
type OpaDocTy.txt_element('a) =
   { Esc_accol }
 / { Esc_croch }
 / { Raw : 'a } // FIXME déplacer raw dans format
 / { License : (int, option(int)) }
 / { Authors : list(OpaDocTy.authors_line) }
 / { Param : (string, list(OpaDocTy.txt_element('a))) }
 / { Return : list(OpaDocTy.txt_element('a)) }
 / { Other_arobase : (
       string /* identifiant @id */,
       list(string) /* identifiants après @id */,
       list(OpaDocTy.txt_element('a))
     )
   }
 / { Newline }
 / { Code : string }
 / { Format : (OpaDocTy.format, OpaDocTy.txt('a)) }

type OpaDocTy.txt('a) = { l : list(OpaDocTy.txt_element('a)) }

/**
 * FIXME:UNDOCUMENTED
**/
type OpaDocTy.format2('a) =
  { newl } / /* new line */
  { newp } / /* new paragraph */
  { str : 'a }

/**
 * {2 Types describing one comment unit}
**/

/**
 * The filename where the comment was read
**/
type Comment.file = string

/**
 * Positions are used to join comments with compiler annotations (types).
 * Positions are absolute positions, exprimed in char
**/
type Comment.position = int

/**
 * Category of comment.
 * A comment may be global [{Glob}], meaning that it is not associated [{Assoc}]
 * to a particular value or type.
**/
type Comment.cat = { Glob } / { Assoc }

/**
 * A comment unit
**/
type Comment.comment = {
  content : OpaDocTy.txt(string)
  fname   : Comment.file
  cat     : Comment.cat
  pos     : Comment.position
}

/**
 * FIXME:UNDOCUMENTED
**/
type Comment.mode = { Doc } / { Raw }

/**
 * {1 Api Declarations}
 *
 * Theses declarations are produced by {b opa.exe --api}, using
 * informations from the type checker.
**/

/**
 * The types used by opadoc. Almost the runtime types, but with
 * the [TySumSugar] case, for treating cases like :
 * {[
 *  type foo = sugar / { bar }
 * }
**/
type Api.ty = OpaType.ty / {TySumSugar : list(OpaType.ty)}

/**
 * The name of the package containing the [Api.entry]
**/
type Api.pkg = string

/**
 * The module path leading to the entry.
**/
type Api.path = list(string)

/**
 * Visibility of values.
 * Correspond to [QmlAst.access_directive].
 *
 * For the point of vue of the documentation, private and package_ are
 * currentlyl equivalent
**/
type Api.value_visibility =
   { public }
 / { private }
 / { package_ }

/**
 * The infos attached to a documented value.
 * Currently only the type and the visibility of the value are exported
**/
type Api.value = {
  ty : Api.ty
  visibility : Api.value_visibility
}

/**
 * Type visibility.
 * This type correspond to the type [QmlAst.type_def_visibility]
**/
type Api.package_name = string

type Api.type_def_visibility =
   { TDV_public }
 / { TDV_abstract : Api.package_name }
 / { TDV_private : Api.package_name }

/**
 * The infos attached to a documented type definition
 * Type used for representing type definitions.
**/
type Api.type_def = {
  ty_def_visibility : Api.type_def_visibility
  ty_def_params : list(OpaType.typevar)
  ty_def_name : OpaType.ty_ident
  ty_def_body : Api.ty
}

/**
 * Toplevel [code_elt]
 * An [Api.type] can be a type definition, or a value definition.
**/
type Api.code_elt =
   { value    : Api.value }
 / { type_def : Api.type_def }

/**
 * Regrouping all infos about an Api entry.
**/
type Api.entry = {
  pkg      : Api.pkg
  path     : Api.path
  code_elt : Api.code_elt
  fname    : Comment.file
  pos      : Comment.position
}

/**
 * {1 Associations between comments and api}
**/

/**
 * A disjonction between the 2 types, e.g. for storing them in a common list.
**/
type Join.mix = {comment:Comment.comment} / {api:Api.entry}

/**
 * A grouped association.
 * Invariant: the case [{none}, {none}] is not present.
**/
type Join.group = {support:option(Api.entry) comment:option(Comment.comment)}

type Join.joined = list(Join.group)

/**
 * A indexation of associated groups.
 * The indexation in the map is the filenames.
**/
type Join.final = map(string , Join.joined)

/**
 * Hyperlink, from path to fname and path_html (anchors)
 *
 * the element definition is available in [{fname}.html#{path_html}]
**/
type Join.Html.link.elt = {
  fname : Comment.file ;
  path_html : string ;
}

/**
 * Indexed from the concatenation of path of values.
**/
type Join.Html.link.map = map(string, Join.Html.link.elt)

/**
 * {1 Associations between types and values}
**/

/**
 * This is a map indexed by type idents, indicating the list
 * of people (e.g. values entries) using this type
**/
type Join.use_type('use) = ordered_map(string, list('use), String.order)

/**
 * Element of generation for an entry
 * FIXME: use a record type instead of a tuple_3 for the snd
 *
 * [(path, (entry, path_html, basename))]
 * Example
 * [ "String.concat", (entry, "String.concat", "string.opa")]
 *
 * The path_html may be different of the path, in case of escaped char.
**/
type Entry.html = (string, (Api.entry, string, string))
