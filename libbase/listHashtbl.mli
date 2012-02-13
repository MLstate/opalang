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
   ListHashtbl.
   Module to store severals values for the same key in an imperative structure.
*)

type ('a, 'b) t
val create : int -> ('a, 'b) t
val clear : ('a, 'b) t -> unit
val copy : ('a, 'b) t -> ('a, 'b) t
val add : ('a, 'b) t -> 'a -> 'b -> unit
val remove : ('a, 'b) t -> 'a -> unit
val find : ('a, 'b) t -> 'a -> ('b, unit) Hashtbl.t
val find_list : ('a, 'b) t -> 'a -> 'b list
val to_list : ('a, 'b) t -> ('a * 'b list) list
val mem : ('a, 'b) t -> 'a -> bool
val mem_cp : ('a, 'b) t -> 'a * 'b -> bool
val iter : ('a -> ('b, unit) Hashtbl.t -> unit) -> ('a, 'b) t -> unit
val fold :
  ('a -> ('b, unit) Hashtbl.t -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val iter_list : ('a -> 'b list -> unit) -> ('a, 'b) t -> unit
val fold_list : ('a -> 'b list -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val length : ('a, 'b) t -> int
