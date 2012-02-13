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

  type t

  val root : t
  val add : t -> Keys.t -> t
  val pop_last: t -> (Keys.t * t) option
  val last: t -> Keys.t (* Warning: raises an exception on root path! *)
  val fold : ('a -> Keys.t -> 'a) -> 'a -> t -> 'a
  val to_string : t -> string
  val size : t -> int

  val compare : t -> t -> int
  val remaining : t -> t -> Keys.t list option
  val remaining_prefix : t -> t -> t option
  val is_prefix : t -> t -> bool
  val concat : t -> t -> t
  val to_list : t -> Keys.t list
  val of_list : Keys.t list -> t

(** {6 IO} *)

(**
   Access the keys from a path in reverse order.
   Currently, keys in path are stored physically in reverse order.
*)
val write : t -> Keys.t list

(**
   Build a path from a physicall path. Keys are given in reverse order,
   since currently keys in path are stored physically in reverse order.
*)
val read : Keys.t list -> t

(** {6 HashCons} *)

(**
   The implementation needs rectypes, but the interface does not export it.
*)
module HashCons :
sig
  type ht
  val create : unit -> ht
  val clear : ht -> unit
  val find : ht -> Keys.t -> ht * Keys.t list
  val add : ht -> Keys.t -> ht * Keys.t list -> unit
end
