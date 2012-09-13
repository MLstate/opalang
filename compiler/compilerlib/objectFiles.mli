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
(**)

(**
   Type alias for lisibility.
*)
type package_name = string (* foo *)
type package = package_name * FilePos.pos
type filename = string (* path/foo.opx *)
type basename = string (* foo.opx *)
type hash = string
type content = string

type 'code_elt parsed_code = (filename * content * 'code_elt list) list

module Package :
sig
  type t = package
  val compare : t -> t -> int
  val equal : t -> t -> bool

  (**
     Prints the name of the package
  *)
  val pp : Format.formatter -> package -> unit

  (**
     Prints the name of the package and its position
     (the position where the package was imported)
  *)
  val pp_full : Format.formatter -> package -> unit
end
module PackageTbl : Hashtbl.S with type key = Package.t

(** {6 Current package} *)
(**
   Get informations about the current packages being compiling.
   If linking mode, or not separated mode, returns informations
   of a dummy linking_packages, with an ["", nopos]
*)

val get_current_package : unit -> package
val get_current_package_name : unit -> package_name

(**
   These two functions returns are injections from package to string
   (ie there are no collisions) that 'remove' specials characters
   that may appears in package names but that are not valid in js/caml
*)
val get_current_package_name_as_a_valid_ident : ?will_be_prefixed:bool -> unit -> string
val get_package_name_as_a_valid_ident : ?will_be_prefixed:bool -> package_name -> string
val get_package_as_a_valid_ident : ?will_be_prefixed:bool -> package -> string

(**
   Not for casual user. Should be called only by the corresponding pass.
*)
val end_of_separate_compilation : unit -> unit
val global_compilation : unit -> bool (** true if we are not separated or if we are after the end of separate compilation *)
val last_pass : string
val no_init : bool ref (* used by qmlCompilers because it links but doesn't init *)

(**
   Get the opx directory corresponding to the compilation of the current package.
   If we are in linking mode, or if the separation mode is not activated, the
   function returns [None]
*)
val get_compilation_directory : unit -> filename option
val get_compilation_directory_or_current_dir : unit -> filename

(**
   Set extra paths, from the -I command line options
*)
val set_extrapaths : no_stdlib:bool -> filename list -> unit

val set_relative_stdlib : filename -> unit

(**
   Get all paths where the compiler should search its objects
*)
val get_paths : unit -> filename list

(**
   Set plugins version, from the loaded plugins
*)
val set_bsl_plugins : (basename * hash) list -> unit

(**
   test existence of a package given known or given extrapath
*)
val exists_package : ?extrapath:string list -> string -> bool

(**
   expand syntax of import directives, for package or plugins
*)
val expand_glob :
  ?mode:[`package|`plugin] -> Package.t list -> (package_name * FilePos.pos) -> Package.t list

(**
   [load_conf filename] load the configuration of packaging architecture
   for the current project. Used with --autobuild option, this may speed-up
   refactoring and packaging of big projects.
*)
val load_conf : filename -> unit

(**
   [conf_opa_files ()] returns all opa file present the loaded conf files
*)
val conf_opa_files : unit -> filename list

(**
   [load filename content code]
   check the validity of the 'Package' declarations from the code, and removes them
   loads the dependency of the given code, and update the current package
   It check the existence of .opx but doesn't load anything
 *)
val load :
  ?parallelism:int ->
  ?extrajs:string list ->
  no_stdlib:bool ->
  ('code_elt -> ([< `declaration | `import ] * package_name * FilePos.pos) option) ->
  ('code_elt list -> float option StringMap.t) ->
  'code_elt parsed_code ->
  ('code_elt parsed_code -> unit) ->
  unit

(**
   Fold on all the dependencies of the current package in the order of dependencies
   @param packages when [true], also fold on the packages that are linked
                   default is [false]
   @param deep when [true], fold on the transitive dependencies
                   default is [false]
   [filename] is the name of the directory containing all the information about the package
   (stdlib.core.opx for instance)
*)
val fold_dir : ?packages:bool -> ?deep:bool -> ('a -> filename -> 'a) -> 'a -> 'a
val fold_dir_name : ?packages:bool -> ?deep:bool -> ('a -> filename -> package -> 'a) -> 'a -> 'a
val fold_name : ?packages:bool -> ?deep:bool -> ('a -> package -> 'a) -> 'a -> 'a
val iter_dir : ?packages:bool -> ?deep:bool -> (filename -> unit) -> unit
val iter_dir_name : ?packages:bool -> ?deep:bool -> (filename -> package -> unit) -> unit
val iter_name : ?packages:bool -> ?deep:bool -> (package -> unit) -> unit

(**
   What the passes that need object files provide
   [t] is the type t to be stored
   [pass] is just the name of the pass
   The type t is going to be marshalled, so you may need to be refresh
   them when you load them
*)
module type S =
sig
  type t
  val pass : string (* don't use wierd chars in here, it will be a filename *)
  val pp : Format.formatter -> t -> unit
end

(**
   What the following functor provides:
   [save] saves the given object in the current package
   [fold] to get the content of the object files
*)
(* could avoid the functorization *)
(* actual loading happens lazily, not at functor application *)
module type R =
sig
  type 'a wrapper
  type t

  (**
     All the iteration functions take the parameter and iterate in the same order
     as [fold_dir].

     packages: packages linked in the command line only
     deep: transitive dependencies
  *)

  val iter_with_name : (?optional:bool -> ?packages:bool -> ?deep:bool -> (package -> t -> unit) -> unit) wrapper
  val fold_with_name : (?optional:bool -> ?packages:bool -> ?deep:bool -> (package -> 'acc -> t -> 'acc) -> 'acc -> 'acc) wrapper
  val iter_with_dir : (?optional:bool -> ?packages:bool -> ?deep:bool -> (filename -> t -> unit) -> unit) wrapper
  val fold_with_dir : (?optional:bool -> ?packages:bool -> ?deep:bool -> (filename -> 'a -> t -> 'a) -> 'a -> 'a) wrapper
  val iter : (?optional:bool -> ?packages:bool -> ?deep:bool -> (t -> unit) -> unit) wrapper
  val fold : (?optional:bool -> ?packages:bool -> ?deep:bool -> ('acc -> t -> 'acc) -> 'acc -> 'acc) wrapper

  (**
     Save the current information to the disk
     Only the information about the current package should be saved, not the
     whole environment that results from merging the environment of the dependencies with
     the current environment
  *)
  val save : ?overwrite:bool -> (t -> unit) wrapper
end

module Make : functor (S:S) -> R with type t = S.t and type 'a wrapper = 'a

(** same as Make but with Client/Server variant *)
module MakeClientServer : functor (S:S) -> R with type t = S.t and type 'a wrapper = side:[`client | `server] -> 'a

(**
   Compilation mode.
   Used as a global imperative state of the compiler.
   Set by the arg parser, and then, read by any pass which need to select its process
   according to the current mode.
*)
type compilation_mode = [
  | `prelude
  | `init
  | `linking
  | `compilation
]

(** Returns the current compilation mode. *)
val compilation_mode : unit -> compilation_mode

module Arg :
sig
  (**
     Concatenation of all specs in interaction with this module.

     + [-c] Compile the current package
     + [--package] Add package for linking.
  *)
  val public_options : (Base.Arg.key * Base.Arg.spec * Base.Arg.doc) list
  val private_options : (Base.Arg.key * Base.Arg.spec * Base.Arg.doc) list

  (**
     Since the anonfun of the compiler may add directly packages to link with,
     the function add_link_package is also exported.
     This is possible to call opa with a few opx files, for relinking.
  *)
  val add_link_package : filename -> unit

  (**
     Used by the pass_CheckOptions to know if there is nothing to do.
  *)
  val no_packages : unit -> bool

  (**
     Used by some passes in the transitional period,
     or for some tests, or qmljs in command line.
  *)
  val is_separated : unit -> bool

  (** same as above *)
  val is_fully_separated : unit -> bool

  (**
     Option used for producing some dependencies graphs.
  *)
  val is_opadep : unit -> bool
end

(**
   {5 Warnings}
*)
val warning_set : WarningClass.Set.t


(**
   The packages that contain values that the compiler can call
*)
val stdlib_packages : package -> bool
val stdlib_package_names : package_name -> bool


val compiler_package : package -> bool

val resave : unit -> unit

(**
   Loads the js extra lib with the given basename
*)
val find_js_file_content_digest : basename -> filename * content * hash

(**
   Used by the rest of the compiler to tells objectFiles
   that the current package has been compiled successfully
   and so must not be deleted
*)
val compilation_is_successfull : unit -> unit

(**
   Turn off separated compilation.
   This function should be used only by tools not using a separated compilation at all,
   and not using the standard Arg of this module.
*)
val turn_separated_off : unit -> unit

(**
   Use given package as a compiler package.
*)
val add_compiler_packages : package_name list -> unit
