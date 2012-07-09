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
