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
 * @author Adam Koprowski (added multiple templates)
 * @destination PUBLIC
 * @stability experimental
 */
import-plugin unix
import stdlib.{system, io.file}

type Template.t = { string name, string dir, map(string, -> binary) resources }

function Template.t mk_template(name, dir, resources) {
  ~{ name, dir, ~resources }
}

mvc_template =
  mk_template("mvc", "tools/opa-create/template/mvc",
              @static_content_directory("tools/opa-create/template/mvc"))

list(Template.t) templates = [ mvc_template ]

function CommandLine_ident(names,description,param_doc)(up) {
    { CommandLine.default_parser with
      ~names, ~description, ~param_doc,
      function on_param(o) { parser { case int=Rule.ident : {no_params:up(int,o)} } }
    }
}

list(CommandLine.parser({oprion(string) name, Template.t template})) options_parsers =
  template_names = List.map(_.name, templates)
  [
    CommandLine_ident(["--name", "-n"],
      "Application name, without spaces, \"/\" or any other special characters",
      "app_name (no special characters)")({ function(name, r){ r with name:some(name)}}
    ),
    CommandLine_ident(["--template", "-t"],
      "Template to be used for the application",
      "template_name [{template_names}]")(
      function(template_name, r) {
        { r with template: List.find(function (t) {t.name == template_name}, templates) ?
          error("Wrong template name: {template_name} (known templates: {template_names})") }
      }
    )
  ]

options =
  cmdline = {
    init: { name: none, template: mvc_template },
    parsers: options_parsers,
    anonymous: [],
    title: "Opa Application Generator"
  }
  CommandLine.filter(cmdline)

function write(file, content) {
  %%BslFile.of_string%%("./{file}", binary_of_string(content))
}

function process_resource(file_name, file_content) {
  n = String.length(options.template.dir)
  file = "{options.name}{String.sub(n, String.length(file_name) - n, file_name)}"
  if (File.exists(file)) { warning("File {file_name} already exists. \nPlease delete it and try again."); System.exit(1) }
  jlog("Generating {file_name}")
  content = String.replace("application_name", options.name, string_of_binary(file_content()))
  write(file, content)
}

Scheduler.push(function () { // hack for node.js toplevel instruction
match (options.name) {
  case {none}: System.exit(0);
  case {some:name}:
    StringMap.iter(process_resource, options.template.resources)
    jlog("\nNow you can type:\n$ cd {options.name}\n$ make run")
  }
}
