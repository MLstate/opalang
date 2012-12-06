(*
    Copyright © 2011, 2012 MLstate

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
   Marshalling bypass maps.
   This module offers an alternative to cmxs ocaml shared object
   for the bypass plugin system.

   @author Mathieu Barbin
*)


(**
   Using this module :

   With bslregister :
   + create a new session of type [session]
   + use the register API to modify it by side-effect
   + [finalize] your session, and use I/O API to save your plugin.

   With Dynlink : use I/O facilty and Dynlink API, manipulating type [t] only.

   This module can be used instead if for any reason we have some problemes
   dealing with the native dynloading with ocaml,
   since this feature is not well ported (e.g windows), or can
   simply be not well supported in any further version of ocaml
   on some architecture.

   This does not work for the interpreters, just for compilers.
   For interpreters we should relink the interpreter, with the
   generated Loaders.
*)

(** {6 Type alias} *)
type filename = string
type pathname = string

(** {6 Error reporting} *)
(**
   This module uses [OManager] errors.
*)

(** {6 I/O facility} *)

(** the row type for a plugin bymap : serializable with marshal.
    a value of this type is not mutable *)
type t

(** [output_t filename r] outputs a value of type [t] to a file, using ocaml marshaling.
    @param filename the name of the file
    @raise Error in case of I/O or Marshal errors *)
val output_file : filename -> t -> unit

(** [intput_t filename] loads a value of type [t] from a file, using ocaml unmarshaling.
    @param filename the name of the file
    @raise Error in case of I/O or Marshal errors *)
val input_file : filename -> t

(**
   Same function, but with low level channel.
   The function does not open or close the channel.
   <!> Ocaml manual mention that channel should have be opened in binary mode
   under certain OS.

   The filename is used for error messages
*)
(** *)

val output : out_channel -> t -> unit
val input : filename:string -> in_channel -> t

(** {6 COMPATIBILITY with BslLib interface} *)

(** Build the same plugin structure as you would optained after the native dynloading of a Loader.*)
val plugin : t -> pathname option -> BslPluginInterface.plugin

(** {6 Dynlink API} *)

(** Marshaled structure Dynlinker.

    [loadfile_private filename] does :
    + loads the type [t] from a file
    + build the [BslLib.loader_introspection] using [loader_introspection]
    + register it in the loader table of libbsl using [BslLib.LoaderTable.register_bootstrap_loader]
    @param filename the name of the file containing the marshaled structure
    @raise Error in case of I/O error
    @see "BslDynlink" which uses this function
    @see "Dynlink" from ocaml standard library
*)
val loadfile_private : filename -> unit

(** an alias for [loadfile_private]
    @deprecated Please use [loadfile_private] *)
val loadfile : filename -> unit

(** {6 Registering API for bslregister} *)

(**
   The type to construct a [t] from BslLib register API.
   the type [session] is imperative, and register functions have
   a side effect on values of this type.
   Once finished registering things in session, use [finalize],
   to build a functionnal structure of type [t].
*)
type session

(** build a fresh new session *)
val create : unit -> session

(** EXPORT an session to a serializable and not mutable value *)
val finalize : session -> t

(** a) dynloader fields *)
(** *)
val unsafe_register_primitive       : session -> BslPluginInterface.register_primitive
val unsafe_register_type            : session -> BslPluginInterface.register_type

(** b) other fields *)

type contents    = string
type module_name = string
type uniq_id     = string

(** *)

val register_basename               : session -> BslPluginInterface.plugin_basename option -> unit
val register_module_name            : session -> module_name -> unit
val register_uniq_id                : session -> uniq_id -> unit
val register_conf                   : session -> BslConf.conf -> unit
val register_ml_runtime             : session -> module_name -> unit
val register_depends                : session -> module_name list -> unit

val register_opa_code               : session -> (filename * contents) list -> unit
val register_js_pack                : session -> JsPackage.t -> unit
val register_nodejs_pack            : session -> JsPackage.t -> unit

val register_has_ml_code            : session -> bool -> unit

val register_ocaml_env              : session -> BslPluginInterface.ocaml_env -> unit
val register_javascript_env         : session -> BslPluginInterface.javascript_env -> unit
