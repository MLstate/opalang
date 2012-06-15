/*
    Copyright Â© 2011, 2012 MLstate

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
 * {1 About this module}
 *
 * Be aware that this package access local file
 * and could be inaccessible or not working with some cloud configuration
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Interface}
 */

/**
  * A module for very basic file access
  */
File = {{
  exists = %% BslFile.exists %% : string -> bool
  content = %% BslFile.content %% : string -> binary
  content_opt = %% BslFile.content_opt %% : string -> option(binary)
  is_directory = %% BslFile.is_directory %% : string -> bool
  mimetype =
    #<Ifstatic:OPA_BACKEND_QMLJS>
    _ -> none
    #<Else>
    %% BslFile.mimetype_opt %% : string -> option(string)
    #<End>
  basename = %% BslFile.basename %% : string -> string
  dirname = %% BslFile.dirname %% : string -> string
}}
