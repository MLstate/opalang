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
   Hashtbl with sorted wrt their input order.
   @author Mathieu Barbin
*)

(**
   this structure comes when :
   + you don't need to remove elt, (just replace)
   + you want to find quickly elt from keys
   + you want to preserve input ordered in iter and fold
   + you need both rev and normal quick and tail
   + you dont care about the size of the table. (we use several redondant
   structures for keeping the tail recusive for folding in both direction)
*)

type ('a, 'b) t
val create : int -> ('a, 'b) t
val clear : ('a, 'b) t -> unit
val add : ('a, 'b) t -> 'a -> 'b -> unit
val replace : ('a, 'b) t -> 'a -> 'b -> unit

(**
   Same complexity than [Hashtbl.find]
*)
val find_opt : ('a, 'b) t -> 'a -> 'b option

(**
   tail rec, O(n)
*)
val to_list : ('a, 'b) t -> ('a * 'b) list

(**
   tail rec, O(n)
*)
val to_rev_list : ('a, 'b) t -> ('a * 'b) list

(**
   Same complexity than [Hashtbl.mem]
*)
val mem : ('a, 'b) t -> 'a -> bool

(**
   tail rec, O(n)
*)
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit

(**
   tail rec, O(n)
*)
val rev_iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit

(**
   tail rec, O(n)
*)
val fold_left : ('a -> 'b -> 'c -> 'a) -> 'a -> ('b, 'c) t -> 'a

(**
   tail rec, O(n)
*)
val fold_right : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

(**
   Same complexity than [Hashtbl.length]
*)
val length : ('a, 'b) t -> int
