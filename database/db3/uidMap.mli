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
type key = Uid.t
type 'a t

val empty : unit -> 'a t
val is_empty : 'a t -> bool
val add : key -> 'a -> 'a t -> 'a t
val mem : key -> 'a t -> bool

val find : key -> 'a t -> 'a
val find_opt : key -> 'a t -> 'a option

val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val iter : (key -> 'a -> unit) -> 'a t -> unit
val filter_keys : (key -> bool) -> 'a t -> 'a t

val keys : 'a t -> key list
val max : 'a t -> key * 'a
val size : 'a t -> int
val resize : 'a t -> int -> unit
