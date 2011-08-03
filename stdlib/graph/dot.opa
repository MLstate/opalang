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
    do jlog("calling external dot tool")
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
