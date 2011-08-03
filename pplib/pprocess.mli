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
(** Generic preprocessor *)

(**
   File syntax (quick summary):
#<Debug> / #<Else> / #<End>
   Filter according to PPDEBUG mode (-d switch)
#<If:VAR TEST> / #<Else> / #<End>
	Do a run-time check on environment variable VAR (applying function
	TEST on its value if TEST is present). Disabled in release mode (-r
	switch). If #<Else> is absent, unit is assumed.
	If TEST is used, the character \$ can be used as the ppdebug module
	prefix ($\{debugmodule\}.).
#<Ifstatic:VAR REGEXP> / #<Else> / #<End>
   Do a static check at compile-time on the value of environment
   variable VAR.
#<Debugvar:VAR>
	Define VAR as the default to test in following #<If>/#<Ifstatic> tests.

Invocation: $0 [-d] [-r] [file]
	-d	enable debug sections
	-r	release mode: comment out dynamic environment checks
	file	file to parse, by default stdin

The output is put to stdout
*)

(** {6 Environment} *)

(** Type of an preprocessor environment. *)
type env

(** Empty preprocessor environment. *)
val empty_env : env

(** [add_env name value] add binding [name] -> [value] on [env]. *)
val add_env : string -> string -> env -> env

(** Fill the preprocessor environment with the system environment. *)
val fill_with_sysenv : env -> env

(** {6 Options}
    Not stable...
*)
(** Type of preprocessor options. *)
type options = {
  env : env; (** Preprocessor environment *)
  output_suffix : string option (**If set, write to files, adding a suffix to each file name*);
  force_static : bool; (** Force all if to be static *)
}

(**
    Default options :
    - force_static = false
*)
val default_options : env -> options

(** {6 Preprocessing} *)

(** Some description of a langage (Not stable...) *)
type lang_description = {
  open_com : string; (** Open a comment *)
  close_com : string; (** Close a comment *)
  open_block : string; (** Open a block *)
  close_block : string; (** Close a block *)
  debug_module : string; (** The debug module *)
}

val process : lang_description -> options -> string -> string

(** {6 Executable} *)
(** A module for easy executable making. *)
module Exe :
sig
  (** Run a preprocessor according to the given language description. *)
  val run : lang_description -> unit
end
