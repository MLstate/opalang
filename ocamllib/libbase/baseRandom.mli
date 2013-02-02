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
