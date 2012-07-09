/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * opa-create tool
 *
 * @category tools
 * @author Cedric Soulas
 * @destination PUBLIC
 * @stability experimental
 */
import-plugin unix
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
    %%BslFile.of_string%%("./{file}", binary_of_string(content))
}

function iter(file, f_content) {
	n = String.length(SRC_DIR)
	file = "{options.name}{String.sub(n, String.length(file) - n, file)}"
        if (File.exists(file)) { Log.warning("OpaCreate", "File {file} already exists. \nPlease delete it and try again."); System.exit(1) }
	jlog("Generating {file}")
    content = String.replace("application_name", options.name, string_of_binary(f_content()))
	write(file, content)
}

StringMap.iter(iter, resources)

jlog("\nNow you can type:\n$ cd {options.name}\n$ make run")
