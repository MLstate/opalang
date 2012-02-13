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
(* cf mli *)
(* guidlines: define step by step whenever you need, in the same order than in Base.List *)
module List = Base.List
type 'a t = 'a * 'a list
external unwrap : 'a t -> 'a list = "%identity"
let wrap = function
  | t::q -> t, q
  | _ -> invalid_arg "HdList.wrap"
let singleton a = a, []

let length (_, l) = succ (List.length l)
let hd = fst
let tl = snd

let last (a, l) =
  match l with
  | [] -> a
  | tl -> hd (wrap (List.rev tl))

let nth (hd, tl) i = if i = 0 then hd else List.nth tl (pred i)
let rev (hd, tl) = wrap (List.rev (hd::tl))

let iter f (hd, tl) = f hd; List.iter f tl
let map f (hd, tl) = f hd, List.map f tl

let fold_left f acc (hd, tl) = List.fold_left f (f acc hd) tl
let fold_right f acc (hd, tl) =
  let tl = List.rev tl in
  let acc = List.fold_left f acc tl in
  f acc hd
  (* List.fold_left f acc (rev hdl) with some more tuple cons/decons *)

let reduce_left f (hd, tl) = List.fold_left f hd tl
let reduce_right f hdl = reduce_left f (rev hdl)

let fold_left_map f acc (hd, tl) =
  let acc, hd = f acc hd in
  let acc, tl = List.fold_left_map f acc tl in
  acc, (hd, tl)
