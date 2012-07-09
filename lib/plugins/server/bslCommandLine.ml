(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
##extern-type CommandLine.private.native.state('a)  = 'a ServerArg.state
(*extern-type CommandLine.private.native.parser('a) = 'a -> ServerArg.arg_parser('a)*)
##extern-type CommandLine.private.native.args_parser('a) = 'a ServerArg.arg_parser
(*
extern-type [opaname] CommandLine.private.native.category_parser('a) = 'a ServerArg.param_parser
register make_arg_parser: caml_list(string),string,string, ('a, string -> option('a)) -> CommandLine.private.native.arg_parser('a)
let make_arg_parser names param_doc doc effect =
  let f' init = ServerArg.func_opt ServerArg.string effect init in
  let g' = ServerArg.fold f' in
  (names, g', param_doc, doc)
*)
##register filter: string, caml_list(CommandLine.private.native.args_parser('a)), 'a -> 'a
let filter topic args init =
  let my_parser = ServerArg.make_parser topic args in
  ServerArg.filter init my_parser

(*
  Used for anonymous arguments.
  If the function returns {none} the argument is kept
*)
##register anonymous_filter: ('a, string -> option('a)), 'a -> 'a
let anonymous_filter func init =
  let param_parser = ServerArg.func_opt ServerArg.anystring func in
  let param_parser = ServerArg.fold_all param_parser in
  ServerArg.filter init param_parser

##register no_more_params : 'a -> CommandLine.private.native.state('a)
##register more_params:     'a -> CommandLine.private.native.state('a)
##register maybe_params:    'a -> CommandLine.private.native.state('a)
let no_more_params x = ServerArg.No_more_params x
let more_params    x = ServerArg.More_params x
let maybe_params   x = ServerArg.Maybe_params x

##register make: \
    caml_list(string), \
    string, \
    string, \
    ('a -> CommandLine.private.native.state('a)), \
    ('a, string -> option(CommandLine.private.native.state('a))) -> \
    CommandLine.private.native.args_parser('a)
let make names param_doc doc initialize step =
  ServerArg.make_arg_parser ~names ~param_doc ~doc ~initialize ~step
