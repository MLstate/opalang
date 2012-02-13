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
    %%BslFile.of_string%%(filename, csv_s)

/**
* Generate a new binary resource from the given list of string list
* @param config configuration to be used
* @param content string list list
*/
to_resource(config, content) =
  csv_s = to_string(config, content)
  Resource.binary(csv_s,"text/csv")

}}
