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
 * {1 Parameters for the generation}
 *
 * This module defines the parameters used for interacting with the generation
 * of the documentation, as well as a corresponding command line parser.
**/

/**
 * {2 Parameters definition}
**/

/**
 * Parameters for the generation.
 *
 * [private]
 * Enable the printing of internal abstract implementation of abstract types,
 * and printing of private functions and types definition.
 * This is used internally for developpement only, the default behavior
 * for building the html documentation is to hide these informations.
 *
 * Currently, there is no more parameters of generation.
**/
type OpaDocParameters.t = {
  files      : list(string)
  help       : bool
  output_dir : option(string)
  private    : bool
  long_uris  : bool
}

/**
 * {2 Parameters parsing}
**/

OpaDocParameters = {{

  /**
   * The default parameters
  **/
  @private default : OpaDocParameters.t = {
    files      = []
    help       = false
    output_dir = {none}
    private    = false
    long_uris  = true
  }

  /**
   * A distinct family for the '--help' option
   */
  @private help_family : CommandLine.family(bool) = {
    title = "Help"
    init = false
    anonymous = []
    parsers = [
      { CommandLine.default_parser with
        names = [ "--help", "-h"]
        description = "Help message on the usage of opadoc"
        on_encounter(_) = {no_params = true}
      },
    ]
  }

  /**
   * The commandLine family to use to parse command line options and
   * to build the parameters.
  **/
  @private parameters_family : CommandLine.family(OpaDocParameters.t) = {
    title = "Parameters"
    init = default
    anonymous = []
    parsers = [
      { CommandLine.default_parser with
        names = [ "--private" ]
        description = "Show private and abstract types/values"
        on_encounter(state) =
          state = { state with private = true }
          {no_params = state}
      },
      { CommandLine.default_parser with
        names = ["--output-dir", "-o"]
        description = "Output directory of generated HTML files"
        param_doc = "<dir>"
        on_param(state) = parser path=(.*) ->
          state = { state with output_dir = {some = Text.to_string(path)}}
          {no_params = state}
      },
      { CommandLine.default_parser with
        names = [ "--long-uris" ]
        description = "Use long uris of the form stdlib.core.rpc.core.network.opa instead of network.opa"
        on_encounter(state) =
          state = { state with long_uris = true }
          {no_params = state}
      },
    ]
  }

  /**
   * Another distinct family for anonymous arguments
   */
  @private anonymous_family : CommandLine.family(list(string)) = {
    title = "Anonymous"
    init = []
    anonymous = [
      {
        filter = "dirs, *.opa" ;
        description = "Give directories and/or some opa files" ;
        parse(files) =
          parser result={Rule.consume} -> result +> files
      },
    ]
    parsers = []
  }

  /**
   * For the simplicity of design, better than passing arround the parameters,
   * parameters are available as a global server side reference.
   * opadoc is a server side non concurrent application, this is really not
   * a problem.
  **/

  /**
   * Internal reference
  **/
  @private parameters = ServerReference.create(default)

  /**
   * Parse the command line, and store the parameters into the local reference.
   * Parameters are then available via the [OpaDocParameters.get] function
  **/
  filter_command_line() =
    params = CommandLine.filter(parameters_family)
    help   = CommandLine.filter(help_family)
    files  = CommandLine.filter(anonymous_family)
    params = {params with ~help ~files}
    ServerReference.set(parameters, params)

  /**
   * get the state of paramaters.
   * This function should be called once the command line has been parsed
   * with [OpaDocParameters.filter_command_line], if not, this returns
   * the default parameters.
  **/
  get() = ServerReference.get(parameters)
}}
