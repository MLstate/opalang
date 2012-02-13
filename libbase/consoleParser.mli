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
   Console Parser for interpreters.

   @author Mathieu Barbin
*)

(**
   This module is a simple tool for accumulating an input of the user,
   until it parse a toplevel separator [";;"].

   This is used in [qmltop], [qmlcompilers].

   There is a support for compilers or interpreters directives,
   this is about any separated line beginning with a sharp,
   as in
   {[
   v = 5
   ;;
   #typer off;;
   etc...
   ]}

   Comment:

   + any thing in the same line than the semicolon separator is removed
   {[
   g = "toto" ;; this is a comment removed by the console parser
   ]}
*)

(** {6 Known bug} *)

(**
   This approch has some negative points :
   + it behave very badly if a string or a comment contains a toplevel separator [";;"]

*)

(** {6 Console Parser} *)
(**
   Some infos about the implementation :
   + Use an intern imperative Buffer

   + it performs a [String.rtrim] on the input.
*)

(**
   The type of a console parser. Statefull
*)
type t

(**
   Create a new console parser
*)
val create : unit -> t

(**
   The type of a parsed entity.

   In case a directive is parsed, the sharp char is keeped in the returned string.
*)
type input = Directive of string | Code of string

(** Reseting the state of the internal Buffer *)
val reset : t -> unit

(**
   Accumulate a line, a return possibly something, or just accumulate the input.

   The accumulate function will add a newline to the Buffer.
   <!> The new line char should not be given to the string.

   Typically, the correct use is :
   {[
   while true do
     match ConsoleParser.accumulate (input_line stdin) with
     | Some (ConsoleParser.Directive s) ->
     | Some (ConsoleParser.Code s) ->
     | None ->
   done
   ]}
*)
val accumulate : t -> string -> input option

(** Flush the last input. It is like [accumulate ";;"] *)
val flush : t -> input option


module Directive :
sig
  (** {6 Support for directives} *)

  (**
     Directives in an interpreter are a way to interact with the it,
     e.g. to modify a global property or an option once the loop is already started.

     The syntax for directives is defined by the [ConsoleParser], which is used
     in the interpreter for parsing the input.

     {[
     #directives [arguments] ;;
     ]}

     A directive can for example just modify the state of the interpreter, by modifying
     some option related to its behavior (like [#typer on/off]) or enrich the environment
     , as in [#load <file>].

     The interface of directives is : [extra-arguments -> env -> directives arguments -> env],
     so that we can handle all possible cases. (the env returned should be the same as the
     input env in case of just a switch of a property.)
  *)

  type arguments = string list
  type 'env action = 'env -> arguments -> 'env

  (**
     For parsing directive, we use a regexpr collection.
     All regexp are successively used to match the input.
     Then, we apply the first matching regexp action.
  *)

  type regexp = string
  type argument_number = int

  type 'env directive = regexp * argument_number * 'env action

  type 'env handler
  val empty : unit -> 'env handler
  val add : 'env handler -> 'env directive -> 'env handler
  val parse : 'env handler -> 'env -> string -> 'env option
end
