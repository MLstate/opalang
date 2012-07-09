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
   Manage all messages of compiler.

   The way of enhancing outputs with colors is as follows:
   [OManager.printf "\@{<blue>blabla\@}"] which will print "blabla" in blue
   The available tags are
   - black, red, green, yellow, blue, magenta, cyan, white, default to set the foreground color
   - the same list but capitalized to set the background color
   - reset: to disable any colors
   - reverse: to switch foreground and background colors
   - cross: to cross the output
   - underline: to underline the output
   - dim, bright, normal: to set the brightness of the foreground

  All these tags can be nested arbitrarily (though nesting cross or underline tags is useless).

  You can set severals tags in one go by separating them with commas:
  [OManager.printf "\@{<blue,Red, bright}stuff\@}"] will set the foreground color
  to bright blue, and the background color to red.
*)

(** {6 Options} *)
(** Provides a specification list for parsing line command. *)
module Arg : sig
  (**
     Concatenation of all specs in interaction with this module.
     + [--no-color] to disable any color from the outputs
     + [--force-color] to enable colored output
     + [--verbose]
     + [--quiet]
  *)
  val options : (Base.Arg.key * Base.Arg.spec * Base.Arg.doc) list

  (**
     Given a tool name, it will return the triplet which print
     the name and the version of the tool.
  *)
  val version : string -> (Base.Arg.key * Base.Arg.spec * Base.Arg.doc)
end

(** Format of a compiler message.*)
type ('params,'output) oformat = ('params, Format.formatter, unit, 'output) format4

(** {6 Properties getter & setter}*)

(** Activate/Unactivate colors *)
val set_color : bool -> unit

(** Activate/Unactivate verbose mode.*)
val set_verbose : bool -> unit

(** Return the status of verbose mode. *)
val is_verbose : unit -> bool

(** Activate/Unactivate quiet mode.*)
val set_quiet : bool -> unit

(** Return the status of quiet mode. *)
val is_quiet : unit -> bool

(** {6 Standard output} *)
(**
   If you need to use the same formatter as OManager (stderr)
   <!> Not for casual user (e.g kfprintf, etc...)
*)
val oformatter : Format.formatter ref

(** Already print on OManager formatter. *)
val printf : ('a,unit) oformat -> 'a

(** Already print on OManager formatter. *)
val ifprintf : ('a,unit) oformat -> 'a

(** Print on OManager formatter only when verbose is activated. *)
val verbose : ('a,unit) oformat -> 'a

(** Print on OManager formatter unless quiet is activated. *)
val unquiet : ('a,unit) oformat -> 'a

(** {6 Errors} *)
(**
   Print an error and exit.
   See in NonFatal module for ide or interpreter.
   <!> You should rather try to use directly a module Error
   corresonding to the language your are manipulating.
   cf [QmlError], [OpaError]
*)
val error : ('a,'exit) oformat -> 'a

(**
   Add an error on OManager but doesn't exit immediatly.
   Exit when [flush_errors] is called.
*)
val serror : ('a,unit) oformat -> 'a

(**
   Like [error] but for internal.
   The message is prefixed with [Internal error. ]
*)
val i_error : ('a,'exit) oformat -> 'a

(**
   Like [serror] but for internal.
*)
val i_serror : ('a,unit) oformat -> 'a

(** Flush all errors and exit if OManager contains errors. *)
val flush_errors : unit -> unit

(** {6 Warning} *)
(** Print a warning, and it's behavior it's defined by [WarningClass]
    options. By default warning is [WarningClass.default]. *)
val warning : wclass:WarningClass.wclass -> ('a,unit) oformat -> 'a

(** At some point after a check for a cond, the [PassHandler] needs to know if a warn-error was produced by the checker.

    This function returns the list of all wclass which have
    already been produced in warn-error mode at this point.
*)
val warn_error_status : unit -> WarningClass.wclass list

(** {6 Using opa as a lib} *)

(**
   The function at_exit should have a specific type,
   like an exception, or an sys exit.
*)
type at_exit = { at_exit : 'a. int -> 'a }

(**
   To be used instead of Pervasives.exit, for users of the compiler
   as a lib
*)
val exit : int -> 'a

module CompilerAsLib :
sig
  (** {6 Cannal redirection} *)
  (**
     You can change the cannal used by the compiler.
     For wild code which does not respect guidelines, there is not guaranty.
  *)

  val set_stderr : Format.formatter -> unit

  (** TODO: stdout is not used by OManager *)
  val set_stdout : Format.formatter -> unit

  (**
     You can replace the function called in case of an fatal error.
     A typicall exemple is to define a exception in the ide to catch
     all errors of the compiler.

     Usability : the error messages have already been printed by the responsable
     of the error, you can just continue your jobs without taking care of
     reporting more messages about the error. :

     "Each module is responsable of the coherence and the clarty of its own errors"

     However, modules can take help from [LangError] modules.
  *)
  val at_exit : at_exit -> unit
end

(** {6 Tool welcome} *)

(**
   Given a tool name (e.g bslregister, qmlflat, etc...)
   this will produce on the oformatter :
   {[
   This is $tool version ... (c) MLstate 2010
   ]}

   [force] means if you want to print it, even if
   verbose is not set to [true].
   by default, the message is printed only if the
   verbose parameter of OManager is activated.
*)
val this_is_tool : ?force:bool -> string -> unit


(**
   Our apologize, internal error, invitation to bug report
*)
val apologies : unit -> unit
