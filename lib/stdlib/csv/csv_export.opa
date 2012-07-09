/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
import-plugin unix
/**
 * @author Grégoire Makridis, 2011
 * @destination public
 * @stability unknown
 * @category export
 */

/**
 * {1 About this module}
 *
 * Definition of the csv export library
 *
 * {1 Where should I start ?}
 *
 * {1 What if I need more?}
 */

import stdlib.core.web.resource

type CsvExport.config =
   {csv_separator : string
   ; csv_header : list(string)
   }

CsvExport =
{{

default_config =
  {csv_separator = ","
  ; csv_header = []
  }

/**
* Returns a string with csv content from the given list of string list
* @param config configuration to be used
* @param content string list list
*/
to_string(config, content) =
  sep = config.csv_separator
  header = String.of_list(elt -> elt, sep,config.csv_header)
  lines = String.of_list(l -> String.of_list(elt -> elt, sep, l), "\n", content)
  header ^ "\n" ^ lines


/**
* Generate a new csv file from the given list of string list
* @param config configuration to be used
* @param content string list list
* @param filename name of the file to generate
*/
to_file(config, content, filename) =
    csv_s = to_string(config, content)
    %%BslFile.of_string%%(filename, binary_of_string(csv_s))

/**
* Generate a new binary resource from the given list of string list
* @param config configuration to be used
* @param content string list list
*/
to_resource(config, content) =
  csv_s = to_string(config, content)
    Resource.binary(binary_of_string(csv_s),"text/csv")

}}
