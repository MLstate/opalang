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
(** This module provides a mapping between database paths (Badop.path) and
    servers. It holds the [cluster_map], which guides this mapping and should be
    shared among all db nodes. *)
(**
   What you have here is a temporary place-holder until a real locator is
   implemented. Features wanted:

   - adding servers before completing initialisation (until a fixed number is
     reached -- like db4). This requires some network communication (directly or
     via a monitor ?)
   - dynamic cluster_map with update/add/remove...
   - cluster_map synchronisation
*)

type 'a t
type key

val create : flat_replication:int -> 'a list -> 'a t

(** Functions used to apply operations on the locator structure. They are all
    asynchronous (run in parallel using the scheduler) *)

val iter: 'a t -> ('a -> unit Cps.t) -> unit Cps.t

val sequential_iter : 'a t -> ('a -> unit Cps.t) -> unit

val map: 'a t -> ('a -> 'b Cps.t) -> 'b t Cps.t

val mapi: 'a t -> (key -> 'a -> 'b Cps.t) -> 'b t Cps.t

val reduce: 'a t -> ('acc -> 'b -> 'acc) -> 'acc -> ('a -> 'b Cps.t) -> 'acc Cps.t

val map_reduce: 'a t -> ('acc -> 'b -> 'c * 'acc) -> 'acc -> ('a -> 'b Cps.t) -> ('c t * 'acc) Cps.t

val at_path: 'a t -> Path.t -> (key -> 'a -> 'b Cps.t) -> 'b Cps.t

val mapi_path: 'a t -> Path.t -> (key -> 'a -> 'a Cps.t) -> 'a t Cps.t

(** Functions to manipulate the locator structure. Keys are obtained from
    [mapi], [mapi_path], etc. and can be used to get the corresponding
    element from another locator. *)

val get_key: 'a t -> key -> 'a

val set_key: 'a t -> key -> 'a -> 'a t

val push_key: 'a t -> key -> 'a -> unit (* imperative ! *)

val to_list: 'a t -> 'a list



(*
val map_path: 'a t -> Path.t -> ('a -> 'a Cps.t) -> 'a t Cps.t

val get: 'a t -> Path.t -> 'a

val geti: 'a t -> Path.t -> 'a
*)
