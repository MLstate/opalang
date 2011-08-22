/*
    Copyright Â© 2011 MLstate

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
content = %% BslFile.content %% : string -> string
content_opt = %% BslFile.content_opt %% : string -> option(string)
is_directory = %% BslFile.is_directory %% : string -> bool
mimetype_opt = %% BslFile.mimetype_opt %% : string -> option(string)
}}


/**
 * {1 Functions exported to the global namespace}
 */

@deprecated({use="File.content"})
file_content = File.content

@deprecated({use="File.content_opt"})
file_content_opt = File.content_opt

/* Used only on --debug mode */
@deprecated({use="File.mimetype"})
file_mimetype_opt = File.mimetype_opt

@deprecated({use="File.is_directory"})
is_directory = File.is_directory
