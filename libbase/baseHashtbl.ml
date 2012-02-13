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
type ('a,'b) t = ('a,'b) Hashtbl.t
let create = Hashtbl.create
let clear = Hashtbl.clear
let add = Hashtbl.add
let copy = Hashtbl.copy
let find = Hashtbl.find
let find_all = Hashtbl.find_all
let mem = Hashtbl.mem
let remove = Hashtbl.remove
let replace = Hashtbl.replace
let iter = Hashtbl.iter
let fold = Hashtbl.fold
let length = Hashtbl.length
let hash = Hashtbl.hash
external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"
module type HashedType = Hashtbl.HashedType

(* could be done (with magic) more efficiently
 * by just assigning the two fields of the hashtbls
 * But then, we become dependent on the stdlib and if it changes,
 * since not everybody uses the same version of ocaml, it won't be
 * possible *)
let replace_content h1 h2 =
  clear h1;
  iter (fun k v -> add h1 k v) h2

let find_opt h v =
  try Some (find h v) with Not_found -> None

module type S =
sig
  include Hashtbl.S
  val replace_content :'a t -> 'a t -> unit
  val find_opt : 'a t -> key -> 'a option
end

module Make(H:HashedType) : S with type key = H.t =
struct
  include Hashtbl.Make(H)
  let replace_content h1 h2 =
    clear h1;
    iter (fun k v -> add h1 k v) h2

  let find_opt h v =
    try Some (find h v) with Not_found -> None
end

let combine a b = a * 19 + b

(*
  To extending the interface with function needing to access the implementation
  of the type [t], we use dark magie.
  This MUST be exactly the same type than [Hashtbl.t]
*)
type ('a, 'b) public_t = {
  mutable size: int ;
  mutable data: ('a, 'b) public_bucketlist array ;
}
and ('a, 'b) public_bucketlist =
  | Empty
  | Cons of 'a * 'b * ('a, 'b) public_bucketlist

external public_of_t : ('a, 'b) t -> ('a, 'b) public_t = "%identity"
external t_of_public : ('a, 'b) public_t -> ('a, 'b) t = "%identity"

let pick_remove t =
  let t = public_of_t t in
  let size = t.size in
  if size <> 0
  then (
    let data = t.data in
    let length = Array.length data in
    let rec aux index =
      if index = length then () else (
        match Array.unsafe_get data index with
        | Empty -> aux (succ index)
        | Cons (_, _, bucket) ->
            Array.unsafe_set data index bucket ;
            t.size <- pred size ;
      )
    in
    aux 0 ;
  )
