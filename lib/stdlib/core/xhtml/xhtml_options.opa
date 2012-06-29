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
