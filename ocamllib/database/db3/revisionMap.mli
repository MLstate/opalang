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
type key = Revision.t
type 'a t

val empty: 'a t
val is_empty : 'a t -> bool
val add: key -> 'a -> 'a t -> 'a t
val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val iter : (key -> 'a -> unit) -> 'a t -> unit
val rev_iter : (key -> 'a -> unit) -> 'a t -> unit
val find : key -> 'a t -> 'a
val find_inf : key -> 'a t -> key * 'a
val size : 'a t -> int
val max : 'a t -> key * 'a
val remove_last : 'a t -> 'a t
val keys : 'a t -> key list
