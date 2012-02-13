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
(* CF mi *)

type ('a, 'b) t = ('a, ('b, unit) Hashtbl.t) Hashtbl.t
let create = Hashtbl.create
let clear = Hashtbl.clear
let copy = Hashtbl.copy
let remove = Hashtbl.remove
let mem = Hashtbl.mem
let iter = Hashtbl.iter
let fold = Hashtbl.fold
let length = Hashtbl.length
let add t k v =
  let all = try Hashtbl.find t k with | Not_found -> Hashtbl.create 10 in
  Hashtbl.replace all v (); Hashtbl.replace t k all
let find t k = try Hashtbl.find t k with | Not_found -> Hashtbl.create 10
let mem_cp t (k, v) = Hashtbl.mem (find t k) v
let gen_to_list f t =
  let fold k v acc = (f k v)::acc in
  Hashtbl.fold fold t []
let to_list t = gen_to_list (fun a b -> a, gen_to_list (fun a _ -> a) b) t
let find_list t k =
  try
    gen_to_list (fun a _ -> a) (Hashtbl.find t k)
  with
  | Not_found -> []
let iter_list f =
  let iter k v = f k (gen_to_list (fun a _ -> a) v) in
  Hashtbl.iter iter
let fold_list f =
  let fold k v ac = f k (gen_to_list (fun a _ -> a) v) ac in
  Hashtbl.fold fold
