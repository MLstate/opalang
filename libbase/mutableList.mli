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
   Manipulation of mutable list.
*)

(**
   Fee free to add any persistent function from [List] in this module,
   in the imperative version.
*)

type 'a t
val create : unit -> 'a t

(**
   O(n)
*)
val to_list : 'a t -> 'a list

(**
   O(1)
*)
val to_rev_list : 'a t -> 'a list

val clear : 'a t -> unit
val add : 'a t -> 'a -> unit

(**
   O(n)
*)
val mem : 'a -> 'a t -> bool

(**
   tail rec, O(n)
*)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(**
   tail rec, O(n)
*)
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

(**
   tail rec, O(n)
*)
val length : 'a t -> int

(**
   tail rec, O(n)
*)
val iter : ('a -> unit) -> 'a t -> unit

(**
   tail rec, O(n)
*)
val rev_iter : ('a -> unit) -> 'a t -> unit

val is_empty : 'a t -> bool

(**
   appends the reversed list at the end of the mutable list
*)
val rev_append : 'a t -> 'a list -> unit

(**
   appends the list at the end of the mutable list
*)
val append : 'a t -> 'a list -> unit

val from_list : 'a list -> 'a t
val from_rev_list : 'a list -> 'a t
val reset_from_list : 'a t -> 'a list -> unit
val reset_from_rev_list : 'a t -> 'a list -> unit
val update : ('a list -> 'a list) -> 'a t -> unit
