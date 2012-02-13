(*
    Copyright © 2011 MLstate

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
