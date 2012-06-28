(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
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
