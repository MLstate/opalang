/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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

import-plugin unix

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
    %%BslFile.of_string%%(filename, binary_of_string(xls_s))

/**
* Generate a new binary resource from the given list of string list
* @param config configuration to be used
* @param content string list list
*/
to_resource(config, content) =
  xls_s = to_string(config, content)
  Resource.binary(binary_of_string(xls_s),"text/xls")

}}
