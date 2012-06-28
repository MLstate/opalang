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
   Dynamic loading of bypass plugin files.

   Bypass plugins are used to introduce custom primitives in an opa code.
   For the compiler {b opa.exe}, like types, implementation, etc..

   Bypass plugins are built with the application {b bslregister},
   and then used by any of the following applications :
   + bslregister (to link a plugin with a previous one)
   + bslbrowser (to browse the primitives of a plugin)
   + qmlcompilers (to link a qml file with some custom primitives)
   + opa.exe (to link a opa file with some custom primitives)

   @author Mathieu Barbin
*)

(** {6 Types for manipulating bypass plugins} *)

(** A bypass plugin file can be a shared object (cmo, cmxs) or a marshal structure (.bypass) *)
type bypass_plugin_file =
  | SharedObject of string (** the filename of an ocaml shared object *)
  | MarshalPlugin of string (** the filename of a marshaled structure *)

(** A sugar to get the filename from a bypass_plugin_file value *)
val file : bypass_plugin_file -> string

(** {6 Dynamic loading of bypass plugins} *)

(** This function does a side effect on the BslPluginTable,
    by registering all definitions of bypass of the given plugin.
    @raise Error if an error happens during loading *)
val load_bypass_plugin : bypass_plugin_file -> unit

(** an alias for [!BslDynlink.load_bypass_plugin] for being as close as possible of the function
    [Dynlink.load_bypass_plugin] from ocaml standard library
    @see "Dynlink" from ocaml standard library for more details *)
val loadfile_private : bypass_plugin_file -> unit

(**
   Same than [loadfile_private] but does nothing if the bypass_plugin_fille was already loaded.
   This is used for avoiding to reload, or to re deserialize plugins several times, for
   optimizing the time taken by the pass BslLoading
*)
val load_bypass_plugin_cache : bypass_plugin_file -> unit

(** {6 Error reporting} *)

(**
   This module uses [OManager] for its errrors.
*)
