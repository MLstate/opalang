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
    Register Generator for the BSL.

    @author Mathieu Barbin
    @author Mehdi Bouaziz
*)

(**
   This module implement the lib for building and generating all plugins,
   laoder, runtime, etc... from bsl-sources files

   See also:
   + {b bslregister} The command line tool version of this lib
   + {b opa-plugin-builder} The installed wrapper
*)

(** {6 Alias for clarty} *)

type filename = string

(** {6 User Interruption} *)

(**
   Define a public exception for interrupting the lib.
   The command line tool should raise this exception to
   kill properly the lib.
*)
exception SigInt

(**
   Set the sigint signal to this exception.
   Usefull if you want to use this lib from a command line tool.
*)
val set_signal_sigint : unit -> unit

(**
   Get back to the signal handler before setting SigInt
   Add if needed (not implemented)
*)
val unset_signal_sigint : unit -> unit

(** {6 Warnings} *)
val warning_set : WarningClass.Set.t

(** {6 Registering Sessions} *)

(**
   The type of a work in progress register session.
   The type is not functionnal because it contains
   a marshal plugin session which is not functionnal,
   and a private mutable table.

   <!> Beware, the table used is [BslLib.BSL].
   If needed, it is possible to instanciate an other one,
   this is currently not a probleme because the compilers
   uses BSL too, but do not call BslRegisterLib at compile time.
   [bslregister] is done when we compile the plugins, not
   during the compilation of servers.
*)
type session

(** Building a new [session] for registering files *)
val create : options:BslInterface.options -> session

(** Set the session preprocessor *)
val set_pprocess : pprocess:(FilePos.filename -> string -> string) -> session -> session

(**
   Preprocessing on all languages, the extension tells what to do.
   Supported extensions are:
   -ml
   -js
   -jsconf
   -opa (deprecated)
*)
val preprocess_file : session -> filename -> session

(** {6 Finalization} *)

(**
   The finalized type, ready to be used for generated file production.
*)
type finalized_t

(**
   Finalize the session.

   build his own bsl, and checks  :
   - all registers
   - js checks
   - opa checks
*)
val finalize : session -> finalized_t

(** Construct directly the plugin value from the finalized
    session. Since the plugin wasn't loaded from disk, it doesn't have
    an associated path *)
val plugin : finalized_t -> BslPluginInterface.plugin

(** check *)
val js_validator : finalized_t -> unit

(** {6 Files Production} *)

(**
   TODO: rewrite using ASTs (not so easy, lot of static code...)
   maybe a mix would be better.
*)

type 'a output = out_channel -> 'a -> unit

type iterator = {
  output : 'a. filename -> (out_channel -> 'a -> unit) -> 'a -> unit
}

type 'a output_iterator = iterator -> 'a -> unit

(**
   JS runtime.
   This is the concatenation of all the js code.

   It is principally used for debugging, and having a trace
   of the resulting code.

   The code is spliten, and is in the plugin file by file.
   Opa compiler use plugins to get back the js code.
*)
val out_js_code              : finalized_t output
val out_nodejs_code          : finalized_t output
val write_nodejs_pack        : filename -> finalized_t -> unit

(**
   ML loader and plugin.

   This is almost the same file, but the loader contains more than the plugin,
   it contains also dynamic function pointer for passing it to the interpreter,
   so that its key resolution give him back a function which it can apply.

   We use 2 separated files, because since the loader contains functions pointer,
   it depends on the runtime, whereas the compiler does not.
   If the plugin depends also on the runtime, it wont be possible to dynlink
   a plugin from the compiler.

   So, functions are [Obj.t option]
   + in the loader : [Some (Obj.repr myfunction)]
   + in the plugin : [None]

   cf [BslPluginInterface]

   ML Runtime and Runtime header

   This is the concatenation of all the implementation code.
   The mli is generated from the [##register] directives, so
   that in this way we can check that the implementation has
   really the type that the implementer pretend in its register.

   JS keys

   This is a file containing all the key resolution in JS, so that
   we can use the checker for knowing statically if a resolution leads
   to an unbound value.

   node.js package

   This is a file that packages all the JS files in the plugin. In the
   node.js backend, it must be included in the final object file to
   use the plugin.
*)
(** *)
val out_js_keys              : finalized_t output
val out_ml_loader            : finalized_t output
val out_ml_plugin            : finalized_t output
val out_ml_runtime           : finalized_t output
val out_ml_runtime_mli       : finalized_t output

val need_makefile            : finalized_t -> bool

(**
   Marshal plugin.
   To avoid recompilation and dynamic loading, the infos from a plugin
   can also be marshalled.
   Our applications are supporting both mode, dynlink plugin and marshalled.
*)
val out_ml_marshal_plugin   : finalized_t output

(**
   Opa code after pre-processing.
   Directives [##include] and [##include-type] have been solved.
   The code is also in the loader, and plugin.
*)
val out_opa_code            : finalized_t output_iterator

(**
   Opa inferred types.

   Once preprocessed, opa files are regrouped and typed.
   For each opa file, an typed interface can be produced
   somewhere.
*)
val out_opa_interface       : finalized_t output_iterator


(*
(** {6 More export for sharing / debuging} *)

(**
   Used by browser for debuging
*)
val string_of_bypass_directive :
  lang:BslLanguage.t ->
  type_path_map:BslLib.typesmap ->
  current_path:(string list) ->
  directive_tags * bypasslang_directive -> (string * string)
*)
