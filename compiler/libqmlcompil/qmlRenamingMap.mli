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
(** Used when ident is renamed for bind the newest ident and the
    original ident. *)

(** {6 Defined types}*)
(** Type of renaming map *)
type t

(** {6 Construct map}*)
(** Empty renaming map. *)
val empty : t

(** [add t original_id new_id] Add a new binding. Consider
    [original_id] has renamed on [new_id].

    BEWARE : If a previous binding exists it doesn't erase, implies map is not
    bijective.
*)
val add : t -> QmlAst.ident -> QmlAst.ident -> t

(** [remove t original_id] Remove a binding with the [original_id]. *)
val remove_from_original : t -> QmlAst.ident -> t

(** the input map must be injective *)
val from_map : QmlAst.ident IdentMap.t -> t

(** [filter t f] Construct a new map from [t] where [f original_id
    new_id] returns [true] *)
val filter : t -> (QmlAst.ident -> QmlAst.ident -> bool) -> t

(** {6 Getter}*)
(** Get the original ident from a given.
    @throw [Not_found] if given ident has not added before.*)
val original_from_new : t -> QmlAst.ident -> QmlAst.ident

(** Like [original_from_new] but returns [None] instead of throw
    [Not_found]. *)
val original_from_new_opt : t -> QmlAst.ident -> QmlAst.ident option

(** Get the new ident from a original ident.
    @throw [Not_found] if given ident has not added before.*)
val new_from_original : t -> QmlAst.ident -> QmlAst.ident

(** Like [new_from_original] but returns [None] instead of throw
    [Not_found]. *)
val new_from_original_opt : t -> QmlAst.ident -> QmlAst.ident option

(** pretty printer for renaming maps *)
val pp : Format.formatter -> t -> unit
