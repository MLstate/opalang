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

module Make:
  functor (A : sig
             type key
             val value : key -> int
             val make : int -> key
           end) ->
sig
  type key = A.key
  type 'a t

  val empty : unit -> 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val mem : key -> 'a t -> bool

  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option

  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val filter_keys : (key -> bool) -> 'a t -> 'a t

  val keys : 'a t -> key list
  val max : 'a t -> key * 'a
  val size : 'a t -> int
  val resize : 'a t -> int -> unit
end
