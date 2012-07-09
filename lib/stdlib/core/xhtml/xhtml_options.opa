/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import stdlib.core.args

/**
  * Type defining the state of options
  * enable_inlined_event : Enable inlined event handler in xhtml when it is possible
  */
type Xhtml.serialization.options = {
  enable_inlined_event : bool
}

/**
 * {1 About this module}
 *
 * Command line options related to xhtml
 * the field options contains the record of options
 *
 */
@private
XhtmlOptions = {{

  /** default value for options */
  default = { enable_inlined_event = true } : Xhtml.serialization.options

  /** Contains command line options */
  @both
  options = p_options

  @private
  commandline : CommandLine.family(Xhtml.serialization.options) =
  {
    title = "Xhtml"
    init = default
    parsers = [
      CommandLine.switch(
        ["--xhtml-disable-inlined-event"],
        "Disable inlined event handler in xhtml"
       ){
         options -> {options with enable_inlined_event = false}
       }
    ]
    anonymous = []
  }

  @private @server @publish // i.e. published for options (see above)
  p_options = CommandLine.filter(commandline)

}}
