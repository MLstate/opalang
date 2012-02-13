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
    @author Henri Binsztok,
    @author Gregoire Makridis
*)

type t =
  | IntKey of int
  | StringKey of string
  | ListKey of t array
  | VariableKey of int
      (** Only valid within a transaction; resolved at commit to fresh keys *)

type real = t

val newkey : t
val to_string : t -> string
val unsafe_make : real -> t
val succ : t -> t option
val pred : t -> t option
val max : t -> t -> t
val min : t -> t -> t
val equal : t -> t -> bool
val compare : t -> t -> int
val value : t -> real

exception Unqualified of string
