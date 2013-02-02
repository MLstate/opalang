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
   ListHashtbl.
   Module to store severals values for the same key in an imperative structure.
*)

type ('a, 'b) t
val create : int -> ('a, 'b) t
val clear : ('a, 'b) t -> unit
val copy : ('a, 'b) t -> ('a, 'b) t
val add : ('a, 'b) t -> 'a -> 'b -> unit
val remove : ('a, 'b) t -> 'a -> unit
val find : ('a, 'b) t -> 'a -> ('b, unit) Hashtbl.t
val find_list : ('a, 'b) t -> 'a -> 'b list
val to_list : ('a, 'b) t -> ('a * 'b list) list
val mem : ('a, 'b) t -> 'a -> bool
val mem_cp : ('a, 'b) t -> 'a * 'b -> bool
val iter : ('a -> ('b, unit) Hashtbl.t -> unit) -> ('a, 'b) t -> unit
val fold :
  ('a -> ('b, unit) Hashtbl.t -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val iter_list : ('a -> 'b list -> unit) -> ('a, 'b) t -> unit
val fold_list : ('a -> 'b list -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val length : ('a, 'b) t -> int
