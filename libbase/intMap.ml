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
exception IteratorEnd

(** Big-Endian Particia tree maps, from "Fast Mergeable Integer Maps", Chris Okasaki, Andrew Gill
    @author Louis Gesbert *)

(*In the comments, low means on the right, hence the bits whose value is
smaller. If a number is b_4b_3b_2b_1, then b_1 is the lowest bit and b_4 the highest
When we speak of "bit m" we will mean the bit k=log(m), and we assume
that m=2^k - hence m has only one 1 in his binary representation*)

(*Big-endian means that branches are shared for paths whose higher bits are
equal*)
type key = int

type 'a t =
  | Empty
  (*A tree with one element whose index is the int*)
  | Lf of int * 'a
  (*A tree with two subtree, we assume that the index of both subtree are
    equal on bit higher than mask, and the bit at position mask is 0 on the
    left subtree, and 1 on the right subtree.
    We require that the m lower bits are 01111...11*)
  | Br of int (* prefix *) * int (* branching bit (mask) *) * 'a t * 'a t

let empty = Empty

let is_empty t = Empty = t

let singleton k v = Lf (k,v)

(** Sets bit m to 0, and all lower bit to 1*)
let mask i m = if m > 0 then (i lor (m-1)(*one-ing the bit at post lower than m*)) land (lnot m) (*zeroing bit m*)else 0

(**is bit mth equal to 0*)
let zerobit i m = (0 = i land m)

(* Put all bits to 0 from bit m leftwards until we found the highest bit*)
(**The highest bit, assuming there is one at least at bit m, else 0*)
let highestbit i m =
  let rec aux i =
    let m = i land (-i) in (* lowest bit of i *)
    if i = m (* If i has only one bit, which is the lowest one, or i=0*)
    then i else aux (i - m (*i without its lowest bit*))
  in aux (i land (lnot (m - 1))
            (*bits at position strictly lower than m are zeroed*))

(** Highest bit where p1 and p2 differ at bit m or higher, else 0*)
let branchingbit p1 p2 m = highestbit (p1 lxor p2) m

(** A tree whith two subtrees, t1 and t2, whose indexes, p1 and p2.
    We assume that they differ only at position m or higher*)
let join p1 t1 p2 t2 m =
  (*the position of the highest difference at bit m or higher*)
  let m = branchingbit p1 p2 m in
  (*if the first different bit is 0 in p1*)
  if zerobit p1 m
  then Br (mask p1 m, m, t1, t2)
  else Br (mask p1 m, m, t2, t1)


let rec add k v t = match t with
  | Lf (k',_v') ->
      if k = k' then Lf (k, v)
      else join k (Lf (k, v)) k' t 1
  | Br (p,m,t1,t2) when mask k m = p ->
      (*if k and m are equal on bit strictly higher than m*)
      if zerobit k m
      then Br (p, m, add k v t1, t2)
      else Br (p, m, t1, add k v t2)
  | Br (p,m,_t1,_t2) ->
      (*There is a difference on bit m or an higher bit*)
      join k (Lf (k, v)) p t (m lsl 1)
  | Empty -> Lf (k, v)

let replace k rep t =
  let rec aux = function
    | Lf (k', v') ->
        if k = k' then Lf (k, rep (Some v'))
        else join k (Lf (k, rep None)) k' t 1
    | Br (p, m, t1, t2) when mask k m = p ->
        if zerobit k m
        then Br (p, m, aux t1, t2)
        else Br (p, m, t1, aux t2)
    | Br (p, m, _t1, _t2) ->
        join k (Lf (k, rep None)) p t (m lsl 1)
    | Empty -> Lf (k, rep None)
  in
  aux t

let rec add_fi f k v t = match t with
  | Lf (k',v') ->
      if k = k' then Lf (k, f k v v')
      else join k (Lf (k,v)) k' t 1
  | Br (p,m,t1,t2) when mask k m = p ->
      if zerobit k m
      then Br (p, m, add_fi f k v t1, t2)
      else Br (p, m, t1, add_fi f k v t2)
  | Br (p,m,_t1,_t2) ->
      join k (Lf (k, v)) p t (m lsl 1)
  | Empty -> Lf (k,v)

let rec safe_add k v t =
  add_fi (fun _ _ -> raise (Invalid_argument "Base.Map.safe_add")) k v t

let rec find_opt k t = match t with
  | Br (_,m,tl,tr) -> if zerobit k m then find_opt k tl else find_opt k tr
  | Lf (k',v) -> if k = k' then Some v else None
  | Empty -> None

let rec findi_opt k t = match find_opt k t with
  | Some v -> Some (k, v)
  | None -> None

let find k t = match find_opt k t with Some v -> v | None -> raise Not_found
let findi k t = match find_opt k t with Some v -> k,v | None -> raise Not_found

let rec remove k t = match t with
  | Br (_,_,Lf(k',_v),t') when k' = k -> t'
  | Br (_,_,t',Lf(k',_v)) when k' = k -> t'
  | Br (p,m,t1,t2) ->
      if zerobit k m
      then Br(p,m,remove k t1,t2)
      else Br(p,m,t1,remove k t2)
  | Lf (k',_v) when k = k' -> Empty
  | n -> n

(* on_miss specify what to do when the key is not found
   take the part of intmap that should contains the key *)
let rec update_gen k f t ~on_miss  = match t with
  | Br (p,m,tl,tr) ->
      if zerobit k m
      then Br (p,m,update_gen k f tl ~on_miss,tr)
      else Br (p,m,tl,update_gen k f tr ~on_miss)
  | Lf (k',v) when k=k' -> Lf (k,f v)
  | n -> on_miss n

let raise_Not_found _ = raise Not_found
let update k f t = update_gen k f t ~on_miss:raise_Not_found
let update_default k f default t = update_gen k f t ~on_miss:(fun m -> add k default m)

let rec mem k t = match t with
  | Br (_,m,tl,tr) -> if zerobit k m then mem k tl else mem k tr
  | Lf (k',_v) when k=k' -> true
  | _ -> false

(* We want some functions to be ordered. The patricia tree is ordered by
   lexicographical order of bits, that is 0 < 1 < max_int < min_int < -1.
   The only problem may be the first node when m = 0b1000..., ie m = min_int
   (<=> m < 0).
   So we embed the functions with the following hack, only modifying the
   top-level call (so that the cost is negligible) ; it breaks the tree, but
   returns a prefix suitable for integer comparison. *)
let ordertop f t = match t with
  | Br (_p,m,tl,tr) -> if m < 0 then f (Br (-1, m, tr, tl)) else f t
  | t -> f t

(* "repairs" the root of t' according to t, to call after ordertop for map-like functions *)
let ordertop_ret t t' = match t, t' with
  | Br (p,m,_,_), Br (_,_,tl,tr) ->
      if m < 0 then Br (p, m, tr, tl) else t'
  | _, _ -> t'

let ordertop_and_ret f t = ordertop_ret t (ordertop f t)

let rec iter f t = match t with
  | Br (_,_,tl,tr) -> iter f tl; iter f tr
  | Lf (k,v) -> f k v
  | Empty -> ()
let iter f = ordertop (iter f)

let rec rev_iter f t = match t with
  | Br (_,_,tl,tr) -> rev_iter f tr; rev_iter f tl
  | Lf (k,v) -> f k v
  | Empty -> ()
let rev_iter f = ordertop (rev_iter f)

let rec map f t = match t with
  | Br (p,m,tl,tr) -> Br (p, m, map f tl, map f tr)
  | Lf (k,v) -> Lf (k, f v)
  | Empty -> Empty
let map f = ordertop_and_ret (map f)

let rec mapi f t = match t with
  | Br (p,m,tl,tr) -> Br (p, m, mapi f tl, mapi f tr)
  | Lf (k,v) -> Lf (k, f k v)
  | Empty -> Empty
let mapi f = ordertop_and_ret (mapi f)

let rec fold f t acc = match t with
  | Br (_,_,tl,tr) -> fold f tr (fold f tl acc)
  | Lf (k,v) -> f k v acc
  | Empty -> acc
let fold f t acc = ordertop (fun t -> fold f t acc) t

let fold_range_compare _ = assert false

let rec fold_range f t k1 k2 acc = match t with
  | Br (p,_,tl,tr) ->
      let acc = if k1 <= p then fold_range f tl k1 k2 acc else acc in
      if p < k2 then fold_range f tr k1 k2 acc else acc
  | Lf (k,v) -> if k1 <= k && k <= k2 then f k v acc else acc
  | Empty -> acc
let fold_range f t k1 k2 acc = ordertop (fun t -> fold_range f t k1 k2 acc) t

let fold_length ~start:_ ~length:_ = assert false (* TODO as soon as needed *)

let rec fold_rev f t acc = match t with
  | Br (_,_,tl,tr) -> fold_rev f tl (fold_rev f tr acc)
  | Lf (k,v) -> f k v acc
  | Empty -> acc
let fold_rev f t acc = ordertop (fun t -> fold_rev f t acc) t

let rec fold_map f t acc = match t with
  | Br (p,m,tl,tr) ->
      let acc, tl = fold_map f tl acc in
      let acc, tr = fold_map f tr acc in
      acc, Br (p, m, tl, tr)
  | Lf (k,v) ->
      let acc, v = f k v acc in acc, Lf (k, v)
  | Empty -> acc, Empty
let fold_map f t acc =
  let acc, t' = ordertop (fun t -> fold_map f t acc) t in
  acc, ordertop_ret t t'


let filter_val f t =
  fold (fun k v acc -> if f v then add k v acc else acc) t empty

let filter_keys f t =
  fold (fun k v acc -> if f k then add k v acc else acc) t empty

let rec compare f t1 t2 = match t1, t2 with
  | Br (p1,m1,tl1,tr1), Br (p2,m2,tl2,tr2) ->
      let k = Pervasives.compare (p1,m1) (p2,m2) in
      if k <> 0 then k else
        let k = compare f tl1 tl2 in
        if k <> 0 then k
        else compare f tr1 tr2
  | Lf (k1,v1), Lf (k2, v2) ->
      let k = Pervasives.compare k1 k2 in
      if k <> 0 then k
      else f v1 v2
  | Empty, Empty -> 0
  | Br _, Lf _ | Br _, Empty | Lf _, Empty -> -1
  | Lf _, Br _ | Empty, Br _ | Empty, Lf _ -> 1

let rec equal f t1 t2 = match (t1, t2) with
  | Br (p1,m1,tl1,tr1), Br (p2,m2,tl2,tr2) ->
      p1 = p2 && m1 = m2 && equal f tl1 tl2 && equal f tr1 tr2
  | Lf (k1,v1), Lf (k2, v2) ->
      k1 = k2 && f v1 v2
  | Empty, Empty -> true
  | _, _ -> false

module Iter : (IterSig.S with type +'a element = int * 'a and type +'a structure = 'a t) = struct
  type 'a structure = 'a t
  type 'a element = int * 'a
  type 'a t = ('a structure) list

  let rec make_aux acc = function
    | Empty -> acc
    | Br (_,_,tl,tr) -> make_aux (tr :: acc) tl
    | Lf (k,v) -> Lf (k,v) :: acc

  let make m = make_aux [] m

  let get = function
    | Lf (k,v) :: _l -> k, v
    | [] -> raise IteratorEnd
    | _ -> assert false

  let next = function
    | Lf (_k, _v) :: Br (_,_,tl,tr) :: l -> make_aux (tr :: l) tl
    | Lf (_k, _v) :: l -> l
    | [] -> raise IteratorEnd
    | _ -> assert false

  let at_end i = i = []

  (* TODO raise notimplemented, not just failwith *)
  let remaining _i = failwith "NotImplemented: IntMap.Iter.remaining"
end

(*this is not efficient, but at this time, this function is not called and is
here only to confom to the signature*)
(*todo: look if ordertop is needed, and can be applied to both tree*)
let fold_map2 f t t' acc =
  (* the result of the map, the accumulator and the iterator*)
  let rec aux iter acc = function
    | Br (p,m,tl,tr) ->
        let acc, tl, iter = aux iter acc tl in
        let acc, tr, iter = aux iter acc tr in
        acc, Br (p, m, tl, tr), iter
    | Lf (k,v) ->
        let ()=if Iter.at_end iter
        then invalid_arg "intMap.fold_map2" in
        let k',v' = Iter.get iter in
        let () = assert (k=k')
        and iter = Iter.next iter in
        let acc, v = f k v v' acc in
        acc, Lf (k, v), iter
    | Empty ->
        acc, Empty, iter
  in
  let iter' = Iter.make t' in
  let acc, t, iter' = aux iter' acc t in
  if Iter.at_end iter' then acc, t
  else invalid_arg "intMap.fold_map2"


module RevIter : (IterSig.S with type +'a element = int * 'a and type +'a structure = 'a t) = struct
  type 'a structure = 'a t
  type 'a element = int * 'a
  type 'a t = ('a structure) list

  let rec make_aux acc = function
    | Empty -> acc
    | Br (_,_,tl,tr) -> make_aux (tl :: acc) tr
    | Lf (k,v) -> Lf (k,v) :: acc

  let make m = make_aux [] m

  let get = function
    | Lf (k,v) :: _l -> k, v
    | [] -> raise IteratorEnd
    | _ -> assert false

  let next = function
    | Lf (_k, _v) :: Br (_,_,tl,tr) :: l -> make_aux (tl :: l) tr
    | Lf (_k, _v) :: l -> l
    | [] -> raise IteratorEnd
    | _ -> assert false

  let at_end i = i = []

  let remaining _i = failwith "NotImplemented: IntMap.Iter.remaining"
end

let rec min t = match t with
  | Br (_,_,tl,_tr) -> min tl
  | Lf (k,v) -> k,v
  | Empty -> raise Not_found
let min t = ordertop min t

(*
  Choose is min, because they are no value except on leaf
  hence I must go to a leaf to find something. And min already
  find a leaf
*)
let choose = min

let choose_opt t =
  try Some (choose t) with | Not_found -> None

let rec max t = match t with
  | Br (_,_m,_tl,tr) -> max tr
  | Lf (k,v) -> k,v
  | Empty -> raise Not_found
let max t = ordertop max t

let rec find_inf k t = match t with
  | Br (p,_,tl,tr) ->
      if k <= p then find_inf k tl
      else (try find_inf k tr with Not_found -> max tl)
  | Lf (k',v) -> if k' <= k then k',v else raise Not_found
  | Empty -> raise Not_found
let find_inf k t = ordertop (find_inf k) t

let rec find_sup k t = match t with
  | Br (p,_,tl,tr) ->
      if k <= p
      then (try find_sup k tl with Not_found -> min tr)
      else find_sup k tr
  | Lf (k',v) -> if k' >= k then k',v else raise Not_found
  | Empty -> raise Not_found
let find_sup k t = ordertop (find_sup k) t

let nearest k t =
  let b = try Some (find_inf k t) with Not_found -> None in
  let a = try Some (find_sup k t) with Not_found -> None in
  match b,a with
  | None, Some x | Some x, None -> x
  | None, None -> raise Not_found
  | Some (kb,vb), Some (ka,va) ->
      if k - kb <= ka - k then kb, vb
      else ka, va

let from_list l = List.fold_left (fun t (k,v) -> add k v t) empty l


let fold_assoc k v acc = (k, v) :: acc
let to_list t = fold_rev fold_assoc t []
let ordered_list t = fold_rev fold_assoc t []
let rev_ordered_list t = fold fold_assoc t []

let keys t = fold_rev (fun k _ acc -> k::acc) t []
let elts t = fold_rev (fun _ v acc -> v::acc) t []

(* FIXME: Random according to the distribution of bits, not the actual map *)
let rec random t = match t with
  | Br (_p,_m,tl,tr) -> if Random.int 2 = 0 then random tl else random tr
  | Lf (k,v) -> k, v
  | Empty -> raise Not_found

let size t = fold (fun _ _ i -> i+1) t 0

let rec height t = match t with
  | Br (_p,_m,tl,tr) -> Pervasives.max (height tl) (height tr) + 1
  | Lf _ -> 1
  | Empty -> 0

let rec merge_i f t1 t2 = match t1, t2 with
  | Br (p1,m1,tl1,tr1), Br (p2,m2,tl2,tr2) ->
      if p1 = p2 && m1 = m2 then (* same node *)
        Br (p1, m1, merge_i f tl1 tl2, merge_i f tr1 tr2)
      else if m2 lsr 1 > m1 lsr 1 && mask p1 m2 = p2 then (* p1 is a sub-node of p2 *)
        if zerobit p1 m2
        then Br (p2, m2, merge_i f t1 tl2, tr2)
        else Br (p2, m2, tl2, merge_i f t1 tr2)
      else if m1 lsr 1 > m2 lsr 1 && mask p2 m1 = p1 then (* p2 is a sub-node of p1 *)
        if zerobit p2 m1
        then Br (p1,m1,merge_i f tl1 t2 ,tr1)
        else Br (p1,m1,tl1,merge_i f tr1 t2)
      else (* disjoint nodes *)
        join p1 t1 p2 t2 m1
  | Lf (k1,v1), Lf (k2,v2) when k1 = k2 -> Lf (k1, f k1 v1 v2)
  | Lf (k1,v1), t2 -> add_fi f k1 v1 t2
  | t1, Lf (k2,v2) -> add_fi (fun k v2 v1 -> f k v1 v2) k2 v2 t1
  | Empty, t | t, Empty -> t

let merge f t1 t2 = merge_i (fun _ -> f) t1 t2
let safe_merge t1 t2 =
  merge (fun _ _ -> raise (Invalid_argument "Base.Map.safe_merge")) t1 t2

let rec decons = function
  | Empty -> invalid_arg "IntMap.decons"
  | Lf (key, val_) -> Empty, key, val_, Empty
  | Br (_prefix, _mask, left, right) -> (
      match left, right with
      | Empty, Empty -> invalid_arg "IntMap.decons"
      | left, Lf (key, val_)
      | Lf (key, val_), left
          -> left, key, val_, Empty
      | br_left, br_right ->
          let l_left, key, val_, l_right = decons br_left in
          l_left, key, val_, (merge (fun _ a -> a) l_right br_right)
    )

let rename f t =
  fold (fun k v t -> add (f k) v t) t empty

(* cf doc *)
let pp sep ppe fmt t =
  let fiter elt val_ =
    ppe fmt elt val_ ;
    Format.fprintf fmt sep
  in
  iter fiter t

let compare_key : int -> int -> int = Pervasives.compare

let diff map1 map2 =
  fold (fun k v acc ->
          if mem k map2 then
            acc
          else
            add k v acc
       ) map1 empty
let diff2 map1 map2 map3 =
  fold (fun k v acc ->
          if mem k map2 && not (mem k map3) then
            acc
          else
            add k v acc
       ) map1 empty

  let example_diff s1 s2 =
    let diff_ = diff s1 s2 in
    match choose_opt diff_ with
    | Some elt -> Some elt
    | None ->
        let diff = diff s2 s1 in
        match choose_opt diff with
        | Some elt -> Some elt
        | None -> None

let from_sorted_array _ = assert false
