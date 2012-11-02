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
(* CF mi *)

type ('a, 'b) t = ('a, ('b, unit) Hashtbl.t) Hashtbl.t
let create i = Hashtbl.create #<Ifstatic:OCAML_VERSION_MAJOR 4> ~random:false #<End> i
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
