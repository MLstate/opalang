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
 * opa-create tool
 *
 * @category tools
 * @author Cedric Soulas
 * @destination PUBLIC
 * @stability experimental
 */

import stdlib.{system, io.file}

SRC_DIR = "tools/opa-create/template/mvc"
resources = @static_content_directory("tools/opa-create/template/mvc")

function CommandLine_ident(names,description,param_doc)(up) {
    {CommandLine.default_parser with
     ~names, ~description, ~param_doc,
     function on_param(o) { parser { case int=Rule.ident : {no_params:up(int,o)} } }
    }
}

list(CommandLine.parser({string name})) cmdline_parsers = [
	CommandLine_ident(["--name", "-n"],
		"Application name, without spaces, \"/\" or any other special characters",
		"app_name (no special characters)")({ function(name, r){ r with ~name}}
	)
]

options =
  cmdline = {
      init : { name : "wiki" },
      parsers : cmdline_parsers,
      anonymous : [],
      title : "Opa Application Generator"
    }
  CommandLine.filter(cmdline)

function write(file, content) {
	%%BslFile.of_string%%("./{file}", content)
}

function iter(file, f_content) {
	n = String.length(SRC_DIR)
	file = "{options.name}{String.sub(n, String.length(file) - n, file)}"
	if (File.exists(file)) { warning("File {file} already exists. \nPlease delete it and try again."); System.exit(1) }
	jlog("Generating {file}")
	content = String.replace("application_name", options.name, f_content())
	write(file, content)
}

StringMap.iter(iter, resources)

jlog("\nNow you can type:\n$ cd {options.name}\n$ make run")