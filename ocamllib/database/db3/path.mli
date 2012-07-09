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
   @author Henri Binsztok,
   @author Gregoire Makridis
*)

  type t

  val root : t
  val add : t -> Keys.t -> t
  val pop_last: t -> (Keys.t * t) option
  val last: t -> Keys.t (* Warning: raises an exception on root path! *)
  val fold : ('a -> Keys.t -> 'a) -> 'a -> t -> 'a
  val to_string : t -> string
  val size : t -> int

  val compare : t -> t -> int
  val remaining : t -> t -> Keys.t list option
  val remaining_prefix : t -> t -> t option
  val is_prefix : t -> t -> bool
  val concat : t -> t -> t
  val to_list : t -> Keys.t list
  val of_list : Keys.t list -> t

(** {6 IO} *)

(**
   Access the keys from a path in reverse order.
   Currently, keys in path are stored physically in reverse order.
*)
val write : t -> Keys.t list

(**
   Build a path from a physicall path. Keys are given in reverse order,
   since currently keys in path are stored physically in reverse order.
*)
val read : Keys.t list -> t

(** {6 HashCons} *)

(**
   The implementation needs rectypes, but the interface does not export it.
*)
module HashCons :
sig
  type ht
  val create : unit -> ht
  val clear : ht -> unit
  val find : ht -> Keys.t -> ht * Keys.t list
  val add : ht -> Keys.t -> ht * Keys.t list -> unit
end
