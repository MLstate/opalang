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
   Configuration for plugins files dependencies
   @author Mathieu Barbin
*)

(** {6 Purpose} *)
(**
   With the separation of plugins/packages, we want to be able to load some
   plugins automatically if there are used by an imported package.

   For that we need a way to add automatically in the command line of
   the back-end compilation all the dependencies of the linked libraries.

   Before the separation, these dependencies was added in the command line
   of opa.
   Now, thanks to this module and conf files of plugins, dependencies
   can also be expressed in the opp directly, and automatically added
   by the compiler.
*)

(** {6 Configuration} *)
(**
   Any file called [*.conf] can be given in the command line of [opa-plugin-builder],
   following this syntax:

   {[
   # comments are welcome at the begining of lines
   [myproperty]
   argument argument argument
   argument argument

   [myproperty:platform]
   argument argument
   ]}

   Supported properties are:
   {[
   [mlcopt]
   # extra options to be passed to ocaml compiler

   [mllopt]
   # extra options to be passed to ocaml linker

   [mlinclude]
   # extra path to be passed to ocaml (absolute only)

   [cclib]
   # options to the C linker

   [ccopt]
   # options to the C compiler and linker
   ]}

   Supported platforms are:
   {[
   linux
   windows
   mac
   ]}
*)

(**
   The type of the accumulated properties
*)
type properties = {
  cclib : string list ;
  ccopt : string list ;
  mlcopt : string list ;
  mllopt : string list ;
  mlinclude : string list ;
  mllibs : string list ;
}

type conf = {
  all_platform: properties ;
  linux: properties option ;
  mac:properties option ;
  windows: properties option ;
  cygwin: properties option ;
}

type t

(**
   The default configuration.
*)
val default : t

(**
   <!> used by generated code
*)
val default_conf : conf

(**
   Tell if a conf is equal to the default configuration
*)
val is_default : conf -> bool

(**
   Fold confs using configration files
*)
val fold : filename:string -> t -> t

(**
   Export at the end of the fold the conf
*)
val export : t -> conf

(**
   Pretty print for debugging
*)
val pp : Format.formatter -> conf -> unit
