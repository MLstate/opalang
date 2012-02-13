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
exception MaxSize

type 'a t =
    {
      mutable array : 'a Weak.t;
      mutable length : int
    }
val make : ?size:int -> int -> 'a t
val create : ?size:int -> int -> 'a t
val get : 'a t -> int -> 'a option
val set : 'a t -> int -> 'a option -> unit
val remove : 'a t -> int -> unit
val length : 'a t -> int
val real_length : 'a t -> int
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_left_i : ('a -> 'b -> i:int -> 'a) -> 'a -> 'b t -> 'a
