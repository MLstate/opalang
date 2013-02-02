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
   Output signature of functors.
   Taking an aggregate of 'a element (lists, set, map..) to give an iterator.
*)
module type S =
sig

  (** The type of the aggregate of elements of type 'a*)
  type +'a structure

  (** The type of the elements of the aggregate*)
  type +'a element

  (** The type of the iterator*)
  type 'a t

  (** Create an iterator for the structure*)
  val make : 'a structure -> 'a t

  (** Taking an element of the iterator
     @raise IteratorEnd if there are no more elements*)
  val get : 'a t -> 'a element

  (** The iterator, one step further
     @raise IteratorEnd if there are no more elements*)
  val next : 'a t -> 'a t
(*   val prev : 'a t -> 'a t *)

  (** True if this iterator has no more element
     (get and next would raise IteartorEnd) *)
  val at_end : 'a t -> bool

  (** The number of remaining elements *)
  val remaining : 'a t -> int
end
