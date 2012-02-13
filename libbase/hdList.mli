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
   Manipulation of non empty lists
*)

type 'a t = 'a * 'a list

(** unwrap a value of type t *)
val unwrap : 'a t -> 'a list

(** wrap a list
   @raise Invalid_argument if the list is empty
*)
val wrap : 'a list -> 'a t

(**
   Build an new HdList from an element
*)
val singleton : 'a -> 'a t

val length : 'a t -> int
val hd : 'a t -> 'a
val tl : 'a t -> 'a list

val last : 'a t -> 'a

val nth : 'a t -> int -> 'a
val rev : 'a t -> 'a t
val iter : ('a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t

val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc

val reduce_left : ('a -> 'a -> 'a) -> 'a t -> 'a
val reduce_left : ('a -> 'a -> 'a) -> 'a t -> 'a

val fold_left_map : ('acc -> 'a -> 'acc * 'a) -> 'acc -> 'a t -> 'acc * 'a t
