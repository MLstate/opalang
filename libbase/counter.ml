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
type t = {
  mutable keys:int list;
  mutable keys_size: int;
}

type key = int

(* Private function *)
let init_keys min max =
  let rec aux n l =
    if n < min then
      l
    else
      aux (n-1) (n::l)
  in
  aux max []

let make max = {
  keys=init_keys 0 max;
  keys_size=max;
}

let get_key counter =
  match counter.keys with
  | key::t ->
      counter.keys <- t;
      key
  | [] ->
      let s = counter.keys_size * 2 in
      let key = counter.keys_size + 1 in
      counter.keys <- init_keys (counter.keys_size + 2) s;
      counter.keys_size <- s;
      key

let release_key counter key =
  counter.keys <- key::(counter.keys)
