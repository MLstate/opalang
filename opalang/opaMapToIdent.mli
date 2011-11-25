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
(** Retrieve the current idents, after any renaming, from original
    names. *)
(** {6 How use it?}*)

(** This module link ident expressions with a string corresponding to
    the original name on the code after any renaming. So, once
    renaming is done, you can call [typ "list"] to get the ident of
    the type "list", or [val_ "myvar"] to get the ident of the myvar
    variable.

    All optional argument [side] is by default equals to
    [`server]. Before slicing, for all functions that take an optional
    argument [side:`client] should be throw an exception.

    With the introduction of opacapi in the compiler, you are not
    allowed to perform query to string which are not in opacapi.
*)

(** {6 Side definition} *)
(** Describe the side *)
type side = [`server | `client]

(** Get the other side *)
val other_side : side -> side

(** {6 Values}*)

val val_no_opacapi_check : ?side:side -> string -> QmlAst.ident
  (** only use for determinitic hashed naming schemes *)

val val_noerr : ?side:side -> string -> QmlAst.ident
  (** Get the ident of the value originally named by the given
      [string]. Throw [Not_found] if the name doesn't exists on the
      corresponding [side]. *)

val val_ : ?side:side -> string -> QmlAst.ident
  (**
     same as [val_noerr], excepts that is prints an error message and exits instead
     of raising an exception
  *)

(** Get the optional ident of the value originally named by the
    given [string]. *)
val val_opt : ?side:side -> string -> QmlAst.ident option

(** Add [ident] as the value originally named by the given
    [string]. Throw an exception if the linking already exists.*)
val val_add : ?side:side -> string -> QmlAst.ident

(** Same as [val_add] but doesn't throw any exception. *)
val val_unsafe_add : ?side:side -> string -> QmlAst.ident

(** {6 Starting server value}
    If we need other internal ident we can make this function :
    - val_internal : string -> QmlAst.ident option
    - val_internal_add : string -> QmlAst.ident
    ...
*)
(** Create a [start_server] ident.
    @raise Failure "start_server" If an start_server ident was
    already created.
*)
val val_start_server_add : unit -> QmlAst.ident

(** Getting the [start_server] ident if it was already created. *)
val val_start_server : unit -> QmlAst.ident option

(** {6 Types}*)
(** Get the ident of the value originally named by the given
    [string]. Throw [Not_found] if the name doesn't exists on the
    corresponding [side]. *)
val typ : string -> QmlAst.ident

(** {6 Map setter & getter}*)
(** Directly get the map of values *)
val get_val_map : ?side: side -> unit -> QmlAst.ident StringMap.t

(** Directly set the map of values *)
val set_val_map : ?side:side -> QmlAst.ident StringMap.t -> unit

(** Directly set the map of types *)
val set_typ_map : QmlAst.ident StringMap.t -> unit

(** {6 Iterators}*)
(** Iter on the map of value.*)
val iter_val_map : ?side:side -> (string -> QmlAst.ident -> unit) -> unit

(** Map on the map of value.*)
val map_val_map : ?side:side -> (QmlAst.ident -> 'a) -> 'a StringMap.t

(** Fold on the map of value.*)
val fold_val_map : ?side:side -> (string -> QmlAst.ident -> 'a -> 'a) -> 'a -> 'a

(** keep the maps only the identifiers satisfying the given predicates
    this function is applied to the two maps of identifiers (client and server) *)
val filter : (Ident.t -> bool) -> unit

(**
   Clears the state of this module
*)
val reset : unit -> unit

val pp : unit BaseFormat.pprinter
