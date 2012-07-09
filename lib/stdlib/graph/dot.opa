/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
import-plugin unix
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
