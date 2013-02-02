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

(** This module is used to manipulates JavaScript package especially to produce
    valid node.js packages.

    @author Quentin Bourgerie
*)

(** Describe an element of a package. *)
type elt =
  | Verbatim of string
  | Code of JsAst.code

(** Describes a (Node) JavaScript package. *)
type t

(** Default package. *)
val default : name:string -> t

(** Set the filename of the main js file. *)
val set_main : t -> string -> t

(** Set the version of a package. *)
val set_version : t -> string -> t

(** Set the Unix permission of the main js file. *)
val set_perm : t -> Unix.file_perm -> t

(** Set the build directory, i.e. where the package will be written by
    [write]. *)
val set_build_dir : t -> string -> t

(** Add a list of dependencies. *)
val add_dependencies : t -> (string * string) list -> t

(** Get the list of dependencies. *)
val get_dependencies : t -> (string * string) list

(** Add an additional file to a package. (filename, content) *)
val add_file : t -> (string * string) -> t

(** Add verbatim code to a package. *)
val add_verbatim : t -> string -> t

(** Add structured code to a package. *)
val add_code : t -> JsAst.code -> t

(** Auto add dependencies (i.e look for require("const")). [miss] are called for
    each dependencies which not already present. *)
val auto_dependencies : ?miss:(string -> unit) -> t -> t

(** Change the content of a package by mapping its elements. The package
    elements are through in order of insertions. *)
val map : (elt -> elt) -> t -> t

(** Fold on the package elements. The package elements are through in order of
    insertions. *)
val fold : ('acc -> elt -> 'acc) -> 'acc -> t ->  'acc

(** Fold on the package elements. The package elements are through in reverse
    order of insertions. *)
val foldr : ('acc -> elt -> 'acc) -> 'acc -> t ->  'acc

(** Get the code contained on this package, it doesn't returns verbatim code. *)
val get_code : t -> JsAst.code

(** [merge p1 p2] Merge content of [p1] to the package [p2] (prepend) *)
val merge : t -> t -> t

(** Returns true if the package does not contains any code *)
val is_empty : t -> bool

(** Pretty printer of [package.json] configuration file *)
val pp_json : t BaseFormat.pprinter

(** Pretty printer of code contained in this package *)
val pp_code : t BaseFormat.pprinter

(** Write the package on the file system. *)
val write : t -> unit



