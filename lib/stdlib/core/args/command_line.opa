/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/**
 * Command line arguments
 * @author David Rajchenbach-Teller
 * @author Mathieu Barbin (layout, documentation, documentation back-port, anonymous arguments)
 * @destination public
**/
import-plugin {unix, server}
import stdlib.core.parser

/**
 * {1 About this module}
 *
 * The core idea is that of state. A family of arguments is defined by an initial state.
 * Encountering a command-line argument can change the state.
 * Some arguments take parameters, which can further change the state.
 * Once parsing of the family is complete, the application can consult the final state.
 *
 * {1 Where should I start ?}
 *
 * A simple example would be:
 *
 * {[
 * type custom_params = {
 *   name   : option(string)
 *   city   : option(string)
 *   others : list(string)
 * }
 *
 * init_params = {
 *   name   = {none}
 *   city   = {none}
 *   others = []
 * }
 *
 * my_params = CommandLine.filter({
 *   title   = "My parameters"
 *   init    = init_params
 *
 *   parsers = [{CommandLine.default_parser with
 *       names        = ["--name", "-n"]
 *       description  = "A name"
 *       on_param(st) = parser n=Rule.consume -> {no_params = {st with name = {some = n}}}
 *     },
 *
 *     {CommandLine.default_parser with
 *       names       = ["--city", "-c"]
 *       description = "A city"
 *       on_param(st) = parser c=Rule.consume -> {no_params = {st with city = {some = c}}}
 *     }]
 *
 *   anonymous = [{
 *       filter        = "*"
 *       description   = "Other anonymous parameters"
 *       parse(others) = parser result={Rule.consume} -> result +> others
 *     }]
 * })
 * }
 *
 * {1 What if I need more ?}
 *
**/

/**
 * {1 Types defined in this module}
**/

/**
 * A type to specify the expected parameters of a command-line flag.
 *
 * [{no_params: 'a}]
 * Don't parse params, or if we're parsing params,
 * stop parsing them and resume parsing args
 *
 * [{params: 'a}]
 * Parse params, not having a param is an error.
 *
 * [{opt_params: 'a}]
 * Parse params, but not having a param is not an error
**/
type CommandLine.change_state('a) =
   {no_params: 'a}
 / {params: 'a}
 / {opt_params: 'a}


/**
 * The type of a command-line flag
 *
 * [names] is the list of names recognized for this flag.
 * A typical example of name begins with one or two '-',
 * followed by a short keyword identifying the flag.
 * This is a list so that it is easy to define shorthands
 * like e.g [[ "--output-dir", "-o" ]]
 *
 * [param_doc] this is a short param keyword, which will be used
 * to print the help menu of the command-line flag.
 *
 * [description] used by the help menu as well.
 *
 * [on_encounter] the function is called with the current state as argument
 * when the flag is encountered. Depending on the returned {!CommandLine.change_state},
 * some more calls to [on_param] will be done.
 *
 * [on_param] called after an [on_encounter] in case of a expected parameter to
 * a command-line flag.
**/
type CommandLine.parser('state) = {
  names:        list(string) ;
  param_doc:    string ;
  description:  string ;
  on_encounter: 'state -> CommandLine.change_state('state) ;
  on_param:     'state -> Parser.general_parser(CommandLine.change_state('state)) ;
}

/**
 * The type for parsing anonymous arguments (meaning not associated to a param)
 *
 * [filter] is a string indicated the criteria used to see if this rule is applied.
 * it can be e.g. a regexp
 *
 * [description] used by the help menu
 *
 * [parse] try to parse the anonymous argument, and update the state.
 * If the parser fails to parse the argument, it is kept for an other CommandLine family
**/
type CommandLine.anonymous_parser('state) = {
  filter : string ;
  description : string ;
  parse : 'state -> Parser.general_parser('state) ;
}

/**
 * A type for a complete set of command line flags.
 *
 * [param] title The title of the family (e.g. "Debug options", "User options", etc.)
 *
 * [init] An initial state at the start of parsing of this family
 *
 * [parsers] A family of parsers manipulating the state
 *
 * [anonymous] Anonymous argument parsers
**/
type CommandLine.family('state) = {
  title : string ;
  init: 'state ;
  parsers: list(CommandLine.parser('state)) ;
  anonymous: list(CommandLine.anonymous_parser('state)) ;
}


/**
 * {1 Interface}
 */

@server_private CommandLine = {{

  /**
   * Parse a family of command-line options.
   *
   * Parsing takes place immediately.
   * Used arguments are removed from the set of arguments for further calls to [filter].
   *
   * @param family A family of options to parse
  **/
  filter(family: CommandLine.family('state)) : 'state =
    { ~title ~init ~parsers ~anonymous } = family

    parsers = List.map((p ->
                        l_on_param(state, text) =
                          do_parse(text) = Parser.try_parse(p.on_param(state), text)
                          if not(String.is_empty(text))
                          then
                            start = String.get(0, text)
                            if start == "-"
                            then {none} //By convention, reject params that start with '-'
                            else
                              if start == "\\"
                              then //By convention, remove initial '\\'
                                do_parse(String.drop_left(1, text))
                              else
                                do_parse(text)
                          else do_parse(text)
                        ServerArg.make_arg_parser(p.names, p.param_doc, p.description, p.on_encounter, l_on_param)), parsers)
    // 1. Parsing parametrized arguments
    state = ServerArg.filter(title, parsers, init)
    // 2. Parsing anonymous arguments
    fold(anonymous, state) =
      // TODO: bind the filter and the description in the ServerArg, for the --help
      fct = anonymous.parse
      func(state, anon_arg:string) = Parser.try_parse(fct(state), anon_arg)
      ServerArg.anonymous_filter(func, state)
    state = List.fold(fold, anonymous, state)
    //do jlog("CommandLine.filter: final state={state}")
    state

  /**
   * A common case for an parameter parser.
  **/
  default_parser : CommandLine.parser = {
    names         = []
    description   = "UNDOCUMENTED"
    param_doc     = ""
    on_encounter(x)= {params = x}
    on_param(x)    = no_params(x)
  }

  /**
   * A parser that never accepts any param.
   * on_param
  **/
  no_params(x) =
    parser {Rule.fail} ->
      //This should never happen
      {no_params = x} : CommandLine.change_state

  /**
   * A parser that accept exactly one argument
  **/
  singleton(p)(x) =
    parser
      result={p(x)} ->
        {no_params = result} : CommandLine.change_state

  /**
    * [switch(names,doc)(up)] creates a switch flag.
    * the [up] function is used to enrich the option configuration
    * e.g. [switch(["--myswitch","-mf"],"myswitch is my switch")(conf -> {conf with my_switch=true})]
    * will put the field [my_switch] of the option configuration to true when --myswitch or -ms is on the command line
    */
  switch(names,description)(up) =
    {CommandLine.default_parser with
     ~names ~description
     param_doc="" // no param
     on_encounter(o) = {no_params = up(o)}
    }:CommandLine.parser

  /**
    * [case(names,cases,doc,doc_param)(up)] creates a case flag.
    * such flag accept a keyword as parameter
    * @param cases associates each parameter's textual (string) representation with a high level (typed)representation (generally a sum type)
    * @param up is a function used to enrich the option configuration with the high level case representation
    * e.g. [case(["--mycase"],[("case1",{case1}),("case2",{case2})],"mycase accept keywords","either case1 or case2")(case,conf -> {conf with my_switch=case})]
    * will put the field [my_switch] of the option configuration to true when --myswitch or -ms is on the command line
    */
  case(names,cases,description,param_doc)(up) =
    param_case(p) = Rule.succeed_opt(List.assoc(Text.to_string(p),cases))
    {CommandLine.default_parser with
       ~names ~description ~param_doc
        on_param(o) =
//      parser p=(.*)
        parser p=(a=(.*) -> a) case={param_case(p)} -> {no_params=up(case,o)} // BUG-29
     }:CommandLine.parser

  /**
    * [text(names,doc,doc_param)(up)] create a flag with a text param
    */
  text(names,description,param_doc)(up) =
     {CommandLine.default_parser with
        ~names ~description ~param_doc
        on_param(o) =
        parser p=(.*) -> {no_params=up(p,o)}
    }:CommandLine.parser


  /**
     * [string(names,doc,doc_param)(up)] create a flag with a string param
     */
  string(names,description,param_doc)(up) =
     up(t,conf) = up(Text.to_string(t),conf)
     text(names,description,param_doc)(up)


  /**
    * [int(names,doc,doc_param)(up)] create a flag with a int param
    */
  int(names,description,param_doc)(up) =
       {CommandLine.default_parser with
        ~names ~description ~param_doc
        on_param(o) =
         parser int=Rule.integer -> {no_params=up(int,o)}
        }:CommandLine.parser

  /**
    * [bool(names,doc,doc_param)(up)] create a flag with a bool param
    */
  bool(names,description,param_doc)(up) =
       {CommandLine.default_parser with
        ~names ~description ~param_doc
        on_param(o) =
         parser bool=Rule.bool -> {no_params=up(bool,o)}
        }:CommandLine.parser

  /**
    * [float(names,doc,doc_param)(up)] create a flag with a float param
    */
  float(names,description,param_doc)(up) =
       {CommandLine.default_parser with
        ~names ~description ~param_doc
        on_param(o) =
         parser float=Rule.float -> {no_params=up(float,o)}
        }:CommandLine.parser

  /**
   * Get the name of the executable
  **/
  executable : -> string = %% BslSys.self_name %%

  /**
   * Select items from a list according to the backend
   */
  select_backend(l:list((list(string),'a))) : list('a) =
#<Ifstatic:OPA_BACKEND_QMLJS>
    be = "node.js"
#<Else>
    be = "caml"
#<End>
    List.map((e -> e.f2),List.filter(((bes, _a) -> List.mem(be,bes)),l))

  /**
   * {1 Deprecated}
  **/

  /**
   * The low level list of all command-line arguments.
   * Should not be used anymore.
  **/
  @deprecated({use="CommandLine.filter with anonymous arguments"})
  args : -> list(string) = %% BslSys.get_argv %%

}}

@private type CommandLine.private.native.state('a) = external
@private type CommandLine.private.native.args_parser('a) = external
