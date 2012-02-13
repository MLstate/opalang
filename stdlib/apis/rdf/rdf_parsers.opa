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
 * Parsers for rdf modules
 *
 * This file provides parsers for several rdf format.
 *
 * @author Anthonin Bonnefoy, 2011
 * @category api
 * @destination public
 */


/**
 *  Parser for the nt rdf format used for base dump
 */
NtParser = {{

  @private any = parser t=((!Rule.white_space .)+) -> Text.to_string(t)
  @private sep = parser Rule.ws -> void

  @private parse_nt = parser
    | sep res=any sep pre=any sep val=any sep "." sep -> (res, pre,val)

  @private parse_nts = parser
    | lst={Rule.parse_list(parse_nt, sep)} -> lst

  try_parse(dump):option(Rdf.base) = Parser.try_parse(parse_nts, dump)

}}

/**
 *  Parser for a rdf query
 */
QueryParser = {{

  @private any = parser t=((!Rule.white_space ![{}] .)+) -> Text.to_string(t)
  @private sep = Rule.ws

  @private uri_value= parser
    | "<" t=((!Rule.white_space !". " !"}" !">" .)+) ">" -> Text.to_string(t)

  @private parse_prefix = parser
    | "PREFIX" sep pref=any sep value=uri_value -> (pref, value)

  @private parse_variable = parser
    | sep "?" var=any sep -> var

  @private parse_select = parser
    | "SELECT" sep "*" sep -> { all }
    | "SELECT" sep vars={ Rule.parse_list_non_empty(parse_variable, sep) } sep -> { vars = vars }

  @private parse_where_value = parser
    | t=((!Rule.white_space !". " !"}" .)+) -> Text.to_string(t)

  @private parse_where_element = parser
    | sep var=parse_variable sep -> { var = var }
    | sep value=parse_where_value sep -> { value = value }

  @private parse_where_tuple = parser
    | t={Rule.rep(3, parse_where_element)} -> t

  @private where_element_separator = parser
    | sep "." sep -> void

  @private parse_where = parser
    | "WHERE" sep "\{" wheres={Rule.parse_list_non_empty(parse_where_tuple, where_element_separator)} where_element_separator? "}" sep -> wheres

  @private parse_query = parser
    | sep
      prefix={Rule.parse_list(parse_prefix, sep)} sep
      select=parse_select sep
      where=parse_where .*
      -> { ~prefix; ~select; ~where }

  try_parse(query):option(Rdf.query) = Parser.try_parse(parse_query, query)
}}
