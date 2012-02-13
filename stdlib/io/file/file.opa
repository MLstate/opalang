/*
    Copyright Â© 2011 MLstate

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
  content = %% BslFile.content %% : string -> string
  content_opt = %% BslFile.content_opt %% : string -> option(string)
  is_directory = %% BslFile.is_directory %% : string -> bool
  mimetype = %% BslFile.mimetype_opt %% : string -> option(string)
  @deprecated({use="File.mimetype"}) mimetype_opt = %% BslFile.mimetype_opt %% : string -> option(string)
  basename = %% BslFile.basename %% : string -> string
  dirname = %% BslFile.dirname %% : string -> string
  dir_sep = %% BslFile.dir_sep %% : string
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
