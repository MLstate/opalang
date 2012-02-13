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
 * Utils for calling dot (using external plugin)
 * Part of the fgraph lib
 *
 * @author Mathieu Barbin
 * @category algorithmic
 * @destination public
 * @stabilization Totally untested, beware by using this code
 */

/**
 * {1 About this module}
 *
 * An interface to use dot in OPA.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

/**
 * Several supported output images format, using external {it dot} command line tool
**/
type Dot.format =
    {png}
  / {svg}
  / {jpg}

/**
 * {1 Interface}
 */

Dot = {{

  /**
   * Convert the format into the option taken by the {it dot} application.
  **/
  format_to_string =
    | {png} -> "png"
    | {svg} -> "svg"
    | {jpg} -> "jpg"

  /**
   * [Dot.convert(format, dot_source)]
   * Convert a dot format source into a raw string, expressed
   * in the given format, using a sys command to {it dot}.
   * The given source should be written in the dot syntax
  **/
  convert(format : Dot.format, source) =
    exec = %%BslSys.Process.exec%%
    format = format_to_string(format)
    command = "dot -T{format}"
    do Log.info("Dot","calling external dot tool")
    exec(command, source)

  /**
   * Parse the svg output of {it dot}, and extract the svg part (hackish)
   * for building an xhtml opa representation of a graph.
  **/
  @private find_svg=parser
  | svg=("<svg" .*) -> Text.to_string(svg)
  | . rem=find_svg -> rem

  /**
   * Convert to xhtml using the unsafe import.
   * This function perform a sys call to {it dot}, on the server side.
  **/
  to_xhtml(source) =
    unsafe_svg = convert({svg}, source)
    unsafe_svg = Parser.parse(find_svg, unsafe_svg)
    Xhtml.of_string_unsafe(unsafe_svg)
}}
