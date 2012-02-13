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
(** The original Obj signature *)

type t = Obj.t
external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
external is_block : t -> bool = "caml_obj_is_block"
external is_int : t -> bool = "%obj_is_int"
external tag : t -> int = "caml_obj_tag"
external set_tag : t -> int -> unit = "caml_obj_set_tag"
external size : t -> int = "%obj_size"
external truncate : t -> int -> unit = "caml_obj_truncate"
external field : t -> int -> t = "%obj_field"
external set_field : t -> int -> t -> unit = "%obj_set_field"
external new_block : int -> int -> t = "caml_obj_block"
external dup : t -> t = "caml_obj_dup"

val lazy_tag : int
val closure_tag : int
val object_tag : int
val infix_tag : int
val forward_tag : int
val no_scan_tag : int
val abstract_tag : int
val string_tag : int
val double_tag : int
val double_array_tag : int
val custom_tag : int
val final_tag : int

val int_tag : int
val out_of_heap_tag : int
val unaligned_tag : int

val marshal : t -> string
val unmarshal : string -> int -> t * int

(** Additional functions *)

val dump : ?depth:int -> 'a -> string
  (** creates a string of the runtime representation of value
      This function is intented for low level debugging purpose
      You should know the internal representation of (at least) algebraic datatypes
      to understand the output of this function
  *)

val print : ?prefix:string -> 'a -> unit
  (** print the value to stdout, possibly prefixed by the given string *)

val native_runtime : bool
  (** [native_runtime = true] when the code currently
      executing is native code *)

val bytecode_runtime : bool
  (** the opposite of the previous flag *)
