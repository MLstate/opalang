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
type ('a,'b) t = ('a,'b) Hashtbl.t
let create i = Hashtbl.create #<Ifstatic:OCAML_VERSION_MAJOR 4> ~random:false #<End> i
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
