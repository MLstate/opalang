(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
