/*
    Copyright © 2011, 2012 MLstate

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
