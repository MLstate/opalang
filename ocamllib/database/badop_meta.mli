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
(**
   A generic command-line parser using ServerArg to set up a database configuration.

   Uses the new features of ocaml 3.12 to build functor implementations as values :)
*)

val default_file: ?name:string -> unit -> string

val default_port: int

val default: (module Badop.S) * Badop.options

val options_parser: ((module Badop.S) * Badop.options) ServerArg.arg_parser list

val options_parser_with_default:
  ?name:string -> ((module Badop.S) * Badop.options)
  -> ((module Badop.S) * Badop.options) ServerArg.arg_parser list
