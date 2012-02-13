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
   * Simple library, intended to have some of the properties of both
   * String and Buffer.  No automatic resize but it can be done manually.
**)

(** This type is concrete **)
type buf = { mutable str : string; mutable i : int; }

(** Global flag for resize **)
type resize_mode = RM_stdout | RM_stderr | RM_custom of (string -> unit) | RM_failwith | RM_exit | RM_noresize
val auto_resize : resize_mode ref

(** Common to String and Buffer **)
type t = buf
val length : buf -> int
val create : int -> buf
val sub : buf -> int -> int -> string

(** Compatibility with String *)
val make : int -> char -> buf
val get : buf -> int -> char
val unsafe_get : buf -> int -> char
val set : buf -> int -> char -> unit
val unsafe_set : buf -> int -> char -> unit
val copy : buf -> buf

(** Compatibility with Buffer **)
val nth : buf -> int -> char
val clear : buf -> unit
val reset : buf -> unit
val add_char : buf -> char -> unit
val add_string : buf -> string -> unit
val add_substring : buf -> string -> int -> int -> unit
val contents : buf -> string

(** Specifics **)
val empty : unit -> buf
val append : buf -> string -> int -> unit
val add_buf : buf -> buf -> unit
val of_string : string -> buf
val to_string : buf -> string
val resize : buf -> int -> unit
val extend : buf -> int -> unit
val real_length : buf -> int
val spare : buf -> int
