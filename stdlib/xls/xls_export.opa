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
 * @author Grégoire Makridis, 2011
 * @destination public
 * @stability unknown
 * @category export
 */

/**
 * {1 About this module}
 *
 * Definition of the xls export library
 *
 * {1 Where should I start ?}
 *
 * {1 What if I need more?}
 */

import stdlib.core.web.resource

type XlsExport.config =
   {xls_header : list(string)
   ; xls_style : list(Css.compiled_property)
   ; xls_values : intmap((string -> string)) // column -> (comparaison function * classe name)
   ; xls_border : int // size in pixel of table border
   }

XlsExport =
{{

default_config =
  {xls_header = []
  ; xls_style = []
  ; xls_values = IntMap.empty
  ; xls_border = 1
  }

/**
* Returns a string with xls content from the given list of string list
* @param config configuration to be used
* @param content string list list
*/
to_string(config, content) =
  style = List.fold(elt, acc -> <>{acc}{elt.name} {elt.value}</>, config.xls_style, <></>)
  header = List.fold(
    elt , acc-> <>{acc}<td class="title">{String.strip(elt)}</td></>
    , config.xls_header, <></>)
      |> x -> <tr>{x}</tr>
  lines = List.fold(
    line, acc ->
      s = List.foldi(
        i, elt, acc ->
          cl = match IntMap.get(i, config.xls_values)
            | {some=foo} -> foo(elt)
            | _ -> ""
          <>{acc}<td class="{cl}">{String.strip(elt)}</td></>
      , line,  <></>)
      <>{acc}<tr>{s}</tr></>
      , content, <></>)
      html =
        <style>{style}</style>
        <table border={config.xls_border}>{header}{lines}</table>
      Xhtml.to_string(html)
  

/**
* Generate a new xls file from the given list of string list
* @param config configuration to be used
* @param content string list list
* @param filename name of the file to generate
*/
to_file(config, content, filename) =
    xls_s = to_string(config, content)
    %%BslFile.of_string%%(filename, xls_s)

/**
* Generate a new binary resource from the given list of string list
* @param config configuration to be used
* @param content string list list
*/
to_resource(config, content) =
  xls_s = to_string(config, content)
  Resource.binary(xls_s,"text/xls")

}}
