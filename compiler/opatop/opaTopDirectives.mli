(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)

(**
   OpaTop directives.
   @author Mathieu Barbin
*)

(**
   Directive are used for switching properties of the interpreter.
   CF documentation of directive handling in [ConsoleParser.Directive]
*)

(**
   The list of thing to do for adding a new directive:
   - add the documentation in this file
   - add the implementation in the ml
   - add the corresponding help note in the help directive
   - add the directive to the list [all_directives]
*)

type env = OpaTopEnv.env
type directive_action = env ConsoleParser.Directive.action
type directive = env ConsoleParser.Directive.directive

(**
   GUIDELINES:
   all directives should be documented :
   - precise the syntax of the directive
   - the role of its eventual arguments
   - its semantic, and some example of utilisation.
   - directive are defined in alphabetic order in this module (except for help).
   - the directive documentation should be added to the help directive.
   - the directive should be added to the directive handler at the end of the module.
*)

(** {6 Directives} *)

(**
   Syntax:
   {[
   #assert on / off ;;
   ]}

   Toggle assertion mode. Default is [on].
   {[
   #assert off ;;
   ]}
   is like [--no-assert] of the compiler. Assertions are ignored.
*)
val assert_ : directive

(**
   Syntax:
   {[
   #bypass ;;
   ]}

   Dump all available external primitives.
*)
val bypass : directive

(**
   Syntax:
   {[
   #dbgen on / off ;;
   ]}

   Toggle DbGen mode. Default is [on].
*)
(* val dbgen : directive *)

(**
   Syntax:
   {[
   #dddbgen on / off ;;
   ]}

   Toggle DbGen Debug Dump mode. Default is [off].
*)
val dddbgen : directive

(**
   Syntax:
   {[
   #env ;;
   ]}

   Dump the current environment.
*)
val env : directive

(**
   Syntax:
   {[
   #envgrep <regexp> ;;
   ]}

   Dump the current environment combined with a grep filter.
   Example:
   {[
   #envgrep int -> int ;;
   ]}
*)
val envgrep : directive

(**
   Syntax:
   {[
   #eval on / off ;;
   ]}

   Toggle the evaluation mode.
*)
(* val eval : directive *)

(**
   Syntax:
   {[
   #import-db <file> ;;
   ]}
   Example:
   {[
   #import-db "file.opa" ;;
   #import-db "complete/path/to/myfile.opa" ;;
   ]}

   Try to import an existing database with its definitions.
*)
val import_db : directive

(**
   Syntax:
   {[
   #load <file> ;;
   ]}
   Example:
   {[
   #load "file.opa" ;;
   #load "complete/path/to/myfile.opa" ;;
   ]}

   Load a file:
   - parse it
   - type it
   - evaluate it
*)
val load : directive

(**
   Syntax:
   {[
   #lookup <ident> ;;
   ]}
   Example
   {[
   #lookup x ;;
   ]}

   Lookup for an identifier in the environment.
   If found, print its type and its value, otherwise print ["Not-found"].
*)
val lookup : directive

(**
   Syntax:
   {[
   #quit ;;
   ]}

   Quit the interpreter.
*)
val quit : directive

(**
   Syntax:
   {[
   #reset ;;
   ]}

   Reset the environment of the interpreter.
   Restart the loop with an empty environment.
*)
val reset : directive

(**
   Syntax:
   {[
   #restricted-bypass on / off ;;
   ]}

   Toggle the checking about restricted bypass.
*)
val restricted_bypass : directive

(**
   Syntax:
   {[
   #schema <file.dot> ;;
   ]}
   Example :
   {[
   #schema "myfile.dot" ;;
   ]}

   Print the current db-schema in a dot file,
   and run display for seing it.
*)
val schema : directive

(**
   Syntax:
   {[
   #typer on / off ;;
   ]}

   Toggle the typeruation mode.
*)
(* val typer : directive *)

(**
   Syntax :
   {[
   #types ;;
   ]}

   Dump the types declarations of the current environment.
*)
(* val typers : directive *)

(** {9 Help} *)

(**
   Syntax:
   {[
   #help ;;
   ]}
   Print the help menu for all directives.
*)
val help : directive

(** {6 The handler} *)

(**
   The list of all directives defined in the previous section.
   Any new directive should be added to this list.
*)
val all_directives : directive list

(**
   The handler of all directives.
*)
val handler : env ConsoleParser.Directive.handler
