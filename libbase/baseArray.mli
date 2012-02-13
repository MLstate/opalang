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
external length : 'a array -> int = "%array_length"
external get : 'a array -> int -> 'a = "%array_safe_get"
external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
external make : int -> 'a -> 'a array = "caml_make_vect"
external create : int -> 'a -> 'a array = "caml_make_vect"
val is_empty: 'a array -> bool
val init : int -> (int -> 'a) -> 'a array
val make_matrix : int -> int -> 'a -> 'a array array
val create_matrix : int -> int -> 'a -> 'a array array
val append : 'a array -> 'a array -> 'a array

(** [unsafe_create len] returns an ['a array] of length [len] *)
val unsafe_create : int -> 'a array
  (**
     Append two memory buffers.
     By opposition to [append], this function does not attempt to be smart
     and to optimize memory representation of arrays.

     Do not use this on regular arrays.*)
val append_memory : 'a array -> 'a array -> 'a array
val concat : 'a array list -> 'a array
val sub : 'a array -> int -> int -> 'a array
val copy : 'a array -> 'a array

(** Return a copy of a memory buffer.
    By opposition to [copy], this function does not attempt to be smart
    and to optimize memory representation of arrays.

    Do not use this on regular arrays.*)
val copy_memory: 'a array -> 'a array
val fill : 'a array -> int -> int -> 'a -> unit
val blit : 'a array -> int -> 'a array -> int -> int -> unit
  (** Same as Array.blit but doesn't check anything on offsets or length *)
val unsafe_blit : 'a array -> int -> 'a array -> int -> int -> unit
val to_list : 'a array -> 'a list
val of_list : 'a list -> 'a array
val iter : ('a -> unit) -> 'a array -> unit
val map : ('a -> 'b) -> 'a array -> 'b array
val iteri : (int -> 'a -> unit) -> 'a array -> unit
val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a array -> 'b -> 'b
val sort : ('a -> 'a -> int) -> 'a array -> unit
val stable_sort : ('a -> 'a -> int) -> 'a array -> unit
val fast_sort : ('a -> 'a -> int) -> 'a array -> unit
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
val swap : 'a array -> int -> int -> unit

(**
   Insert an element at the right place in an array, preserving the array sorted.
   By default, there is no duplication, and in case of conflict, the returned array
   is exactly the one taken as argument (no replace)
*)
val insert_sorted : ?cmp:('a -> 'a -> int) -> ?dupl:(bool) -> 'a -> 'a array -> 'a array

val fold_left_i : ('a -> 'b -> int -> 'a) -> 'a -> 'b array -> 'a
val max : 'a array -> 'a
val min : 'a array -> 'a
val argmax : 'a array -> int
val argmin : 'a array -> int
val map_some : ('a -> 'b option) -> 'a array -> 'b array
val mapi_some : (int -> 'a -> 'b option) -> 'a array -> 'b array
val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
val fill_some : 'a option array -> 'a -> int -> int -> unit
val mem : 'a -> 'a array -> bool
val exists : ('a -> bool) -> 'a array -> bool
val split: ('a * 'b) array -> 'a array * 'b array
val find: 'a array -> 'a -> int (** May raise [Not_found] *)

(**
   the function given in argument should return exactly -1, 0 or 1
*)
val compare : ('a -> 'a -> int) -> 'a array -> 'a array -> int

val to_string : ('a -> string) -> 'a array -> string
(** Print on array format; i.e. [|a;b;c;...|] *)
val print : ('a -> string) -> 'a array -> string

(**
   [filter fct a]
   Reallocate a new array with only filtered fields of a
   such than [fct a = true].
   <!> if the array is empty, returns the same array,
   and the function [fct] is never called
*)
val filter : ('a -> bool) -> 'a array -> 'a array

(**
   Same than [filter] but give acces to the index of the filtered item.
*)
val filteri : (int -> 'a -> bool) -> 'a array -> 'a array
