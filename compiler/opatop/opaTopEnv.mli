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
   OpaTop code_elt evaluation.
   @author Mathieu Barbin
*)

(** {6 Type alias} *)
type filename = string
type contents = string
type item_number = int


(** {6 Errors reporting} *)

(**
   Like the IDE, the interpreter should not crash with the first error.
   OManager expose a function [at_exit] which permit to replace the behavior
   of the compiler libs in case of errors.

   OpaTop define an exception, which is raised by OManager.
   This exception is caught internally in each function which can
   possibly enrich the environment. In case Fatal Mode is set,
   we kill the application.

   Fatal mode:

   In case we may want to use the interpreter in a fatal mode, we can
   toggle the corresponding property,
   - via an option of the command line tool,
   - via a call to the OpaTopProperties API
   - via a directives from the input of the intepreter.
*)

exception OManagerException

(** {6 Environment} *)
type env


(**
   Keep in cache the start-up environment (empty).
*)
val cache_initial : env option ref

(**
   Build a new start-up environment, and update the cached start-up environment.
   <!> Beware, this function calls [OpaTopBsl.bypass_map] to build the bsl map,
   and the contains of this map depends on the state of the BslPluginTable.
   Typically, building the start-up environment should not be done before
   the plugins have been stored into the BslPluginTable.

   The function also reset the console parser used by the interpreter.

   @see "OpaTopBsl.bypass_map" for more details about the plugin loading.
*)
val start : unit -> env

(**
   Lookup for a identifier in the environment.
*)
val find_opt :
  Ident.t -> env -> (QmlAst.ty * OpaTopValue.t) option

(**
   Iter on all identifiers contained in the environment.
   Based on values environment.
*)
val iter :
  (Ident.t -> QmlAst.ty -> OpaTopValue.t -> unit) -> env -> unit

val fold :
  (Ident.t -> QmlAst.ty -> OpaTopValue.t -> 'acc -> 'acc) ->
  env -> 'acc -> 'acc

(** {6 Initializer and Setters} *)

(**
   In practice, this is called with [OpaTopDirectives.handler].
*)
val set_directive_handler : env ConsoleParser.Directive.handler -> unit

val set_schema : env -> QmlDbGen.Schema.t -> env

(**
   Change the filename which is meant to be parsed.
   This is used only for error messages.
*)
val set_filename : env -> filename -> env

(**
   Change the item number which is meant to be parsed.
   This is used only for error messages.
   Combine this with the function [input_line].
*)
val set_item_number : env -> item_number -> env

(** {6 Accessors} *)

(**
   Get the console parser used by the interpreter.
   <!> Not for casual users.
*)
val console_parser : unit -> ConsoleParser.t

(**
   Get the db schema.
*)
val schema : env -> QmlDbGen.Schema.t

(**
   Get the typing environment.
*)
val types : env -> QmlTyper.env

(**
   Get the values environment.
*)
val values : env -> OpaTopEval.env

(**
   Get the bypass_map environment.
*)
val bypass_map : env -> OpaTopBsl.bypass_map

(** {6 Dump} *)

(**
   Dump the external primitives available on the std channel
   of the interpreter.
*)
(* val dump_bypass_map : env -> unit *)


(** {6 Input} *)

(**
   Enrich the environment by loading a code_elt
*)
val input_code_elt : env -> QmlAst.code_elt -> env

(**
   Enrich the environment by loading a file.
*)
val input_file : env -> filename -> env

(**
   Enrich the environment by loading a contents.
   The contents is a raw string in opa concrete syntax.
   The contents should not contain opatop directives.
   (this would be a syntax error)
*)
val input_contents : env -> contents -> env

(**
   Enrich the environment by loading a toplevel sentence from an input channel.
   For error messages, we use the filename and the item_number contained in
   the environment.
   Attention, the item number is not the line number since some items may span
   on several lines.

   The item_number is incremented.
   The line is read from the input channel using the
   function [String.unput_line_b].
*)
val input_line : env -> in_channel -> env

(**
   Loop with the function [input_line] until [End_of_file] is raised.
*)
val input_loop : env -> in_channel -> env
