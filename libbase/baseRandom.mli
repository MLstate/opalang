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
(** {6 From Ocaml Standard Library} *)

val init : int -> unit
val full_init : int array -> unit
val self_init : unit -> unit
val bits : unit -> int
val int : int -> int
val int32 : Int32.t -> Int32.t
val nativeint : Nativeint.t -> Nativeint.t
val int64 : Int64.t -> Int64.t
val float : float -> float
val bool : unit -> bool
module State :
sig
  type t = Random.State.t
  val make : int array -> t
  val make_self_init : unit -> t
  val copy : t -> t
  val bits : t -> int
  val int : t -> int -> int
  val int32 : t -> Int32.t -> Int32.t
  val nativeint : t -> Nativeint.t -> Nativeint.t
  val int64 : t -> Int64.t -> Int64.t
  val float : t -> float -> float
  val bool : t -> bool
end
val get_state : unit -> State.t
val set_state : State.t -> unit

(** {6 Extra Function} *)

(**
   Ensure to call [Random.self_init] once.
   The function [Random.self_init] is called only in the first call
   to [ensure_init] (internal reference)
*)
val ensure_init : unit -> unit

val max_int : unit -> int
val max_int64 : unit -> Int64.t

(**
   [string len]
   Generates a random string of length [len]
*)
val string : int -> string

(**
   [alpha_string len]
   Generates a random string of letters of length [len]
*)
val alpha_string : int -> string
