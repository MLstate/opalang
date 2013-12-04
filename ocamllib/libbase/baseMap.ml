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
(* depends *)
module Set = BaseSet

(* -- *)

exception IteratorEnd

module Make (Ord: OrderedTypeSig.S) : (BaseMapSig.S with type key = Ord.t) =
struct

  type key = Ord.t

  type 'a t =
    | Empty
    | Node of 'a t * key * 'a * 'a t * int

  let decons = function
    | Empty -> invalid_arg "Map.decons"
    | Node (left, key, val_, right, _) -> left, key, val_, right

  let height = function
    | Empty -> 0
    | Node(_,_,_,_,h) -> h

  let create l x d r =
    let hl = height l and hr = height r in
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

  (*
    Balance the tree such that the difference between the heights of
    the two branches is at most one
  *)
  let bal l x d r =
    let hl = height l
    and hr = height r in
    if hl > hr + 2 then begin
      match l with
      | Empty -> assert false
      | Node(ll, lv, ld, lr, _) ->
          if height ll >= height lr then
            create ll lv ld (create lr x d r)
          else begin
            match lr with
              Empty -> assert false
            | Node(lrl, lrv, lrd, lrr, _)->
                create (create ll lv ld lrl) lrv lrd (create lrr x d r)
          end
    end else if hr > hl + 2 then begin
      match r with
      | Empty -> assert false
      | Node(rl, rv, rd, rr, _) ->
          if height rr >= height rl then
            create (create l x d rl) rv rd rr
          else begin
            match rl with
              Empty -> assert false
            | Node(rll, rlv, rld, rlr, _) ->
                create (create l x d rll) rlv rld (create rlr rv rd rr)
          end
    end else
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let singleton x data = Node (Empty, x, data, Empty, 1)

  let rec add x data = function
    | Empty -> Node (Empty, x, data, Empty, 1)
    | Node (l, v, d, r, h) ->
        let c = Ord.compare x v in
        if c = 0 then Node(l, x, data, r, h)
        else if c < 0 then bal (add x data l) v d r
        else bal l v d (add x data r)

  let replace x replace map =
    let rec aux = function
      | Empty -> Node (Empty, x, replace None, Empty, 1)
      | Node (l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then Node (l, x, replace (Some d), r, h)
          else if c < 0 then bal (aux l) v d r
          else bal l v d (aux r)
    in
    aux map

  let update key f map =
    let f = function
      | None -> raise Not_found
      | Some a -> f a
    in
    replace key f map

  let update_default key f default map =
    let f = function
      | None -> default
      | Some a -> f a
    in
    replace key f map

  let rec safe_add x data = function
    | Empty -> Node (Empty, x, data, Empty, 1)
    | Node (l, v, d, r, _h) ->
        let c = Ord.compare x v in
        if c = 0 then raise (Invalid_argument "Base.Map.safe_add")
        else if c < 0 then bal (safe_add x data l) v d r
        else bal l v d (safe_add x data r)

  let rec findi_opt x = function
    | Empty -> None
    | Node(l, v, d, r, _) ->
        let c = Ord.compare x v in
        if c = 0 then Some (v,d)
        else findi_opt x (if c < 0 then l else r)

  let find_opt x m = Option.map snd (findi_opt x m)

  (** @raise Not_found. *)
  let find x m = Option.get_exn (Not_found) (find_opt x m)

  (** @raise Not_found. *)
  let findi x m = Option.get_exn (Not_found) (findi_opt x m)

  let rec size = function
    | Empty -> 0
    | Node (l, _, _, r, _) -> 1 + size l + size r

  (*    let random m =
        let rec aux height = function
        | Empty -> raise Not_found
        | Node (Empty, v, d, Empty, _) -> v, d
        | Node (Empty, v, d, r, _) ->

        | Node (l, v, d, r, _) ->
        match Random.int height with
        | 0 -> (v, d)
        | i when i mod 2 = 0 -> random (pred height)
        let c = Ord.compare x v in
        if c = 0 then (v, d)
        else if c < 0 then
        if l = Empty then (v, d)
        else nearest x l
        else (* c > 0 *)
        if r = Empty then (v, d)
        else nearest x r *)

  (* FIXME: slow *)
  let rec random = function
    | Empty -> raise Not_found
    | Node (l, v, d, r, _) ->
        let sl = size l
        and sr = size r in
        if sl = 0 then
          if sr = 0 || Random.int (sr + 1) = 0 then v, d
          else random r
        else if sr = 0 then
          if sl = 0 || Random.int (sl + 1) = 0 then v, d
          else random l
        else
          match Random.int (sl + sr + 1) with
          | 0 -> v, d
          | i -> random (if i mod 2 = 0 then l else r)

  let rec nearest x = function
    | Empty -> raise Not_found
    | Node (l, v, d, r, _) ->
        let c = Ord.compare x v in
        if c = 0 then (v, d)
        else if c < 0 then
          if l = Empty then (v, d)
          else nearest x l
        else (* c > 0 *)
          if r = Empty then (v, d)
          else nearest x r

  let return = function
    | Some r -> r
    | _ -> raise Not_found

  let find_inf x t =
    let rec aux res = function
      | Empty -> return res
      | Node (l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then (v, d)
          else if c < 0 then
            if l = Empty then return res
            else aux res l
          else (* c > 0 *)
            let res = match res with
              | Some (rv, _rd) when rv < v -> Some (v, d)
              | None -> Some (v, d)
              | _ -> res in
            if r = Empty then return res
            else aux res r
    in aux None t

  let find_sup x t =
    let rec aux res = function
      | Empty -> return res
      | Node (l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then (v, d)
          else if c < 0 then
            let res = match res with
              | Some (rv, _rd) when rv > v -> Some (v, d)
              | None -> Some (v, d)
              | _ -> res in
            if l = Empty then return res
            else aux res l
          else (* c > 0 *)
            if r = Empty then return res
            else aux res r
    in aux None t

  let min t =
    let rec aux = function
      | Empty -> raise Not_found
      | Node (Empty, v, d, _, _) -> v, d
      | Node (l, _, _, _, _) -> aux l
    in aux t

  let choose = function
    | Empty -> raise Not_found
    | Node (_,v,d,_,_) -> v,d

  let max t =
    let rec aux = function
      | Empty -> raise Not_found
      | Node (_, v, d, Empty, _) -> v, d
      | Node (_, _, _, r, _) -> aux r
    in aux t

  let rec mem x = function
    | Empty -> false
    | Node(l, v, _d, r, _) ->
        let c = Ord.compare x v in
        (*  FIXME : benchmark *)
        (*        if c < 0 then mem x l *)
        (*        else if c > 0 then mem x r *)
        (*        else true *)
        c = 0 || mem x (if c < 0 then l else r)

  let rec min_binding = function
      Empty -> raise Not_found
    | Node(Empty, x, d, _r, _) -> (x, d)
    | Node(l, _x, _d, _r, _) -> min_binding l

  let rec remove_min_binding = function
      Empty -> invalid_arg "Map.remove_min_elt"
    | Node(Empty, _x, _d, r, _) -> r
    | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

  let _merge t1 t2 =
    match t1, t2 with
    | (Empty, t) | (t, Empty) -> t
    | _ ->
        let (x, d) = min_binding t2 in
        bal t1 x d (remove_min_binding t2)

  let rec remove x = function
    | Empty -> Empty
    | Node (l, v, d, r, _h) ->
        let c = Ord.compare x v in
        if c = 0 then _merge l r
        else if c < 0 then bal (remove x l) v d r
        else bal l v d (remove x r)

  let rec iter f = function
      Empty -> ()
    | Node(l, v, d, r, _) ->
        iter f l; f v d; iter f r

  let rec rev_iter f = function
      Empty -> ()
    | Node(l, v, d, r, _) ->
        rev_iter f r; f v d; rev_iter f l

  let rec map f = function
      Empty               -> Empty
    | Node(l, v, d, r, h) -> Node(map f l, v, f d, map f r, h)

  let rec mapi f = function
      Empty               -> Empty
    | Node(l, v, d, r, h) -> Node(mapi f l, v, f v d, mapi f r, h)

  let rec fold_map f m acc =
    match m with
    | Empty -> (acc, Empty)
    | Node (l, v, d, r, h) ->
        let (acc, l) = fold_map f l acc in
        let (acc, d) = f v d acc in
        let (acc, r) = fold_map f r acc in
        (acc, Node (l, v, d, r, h))

  let rec fold f m acc = match m with
    | Empty -> acc
    | Node (l, v, d, r, _) -> fold f r (f v d (fold f l acc))

  let fold_range_compare compare f m kmin kmax acc =
    (* if compare kmin kmax = 1 then *)
    (*   invalid_arg "[Map.fold_range] kmin > kmax"; *)
    (* Fold until find kmax. *)
    let rec fold_max m acc = match m with
      | Empty -> acc
      | Node (l, k, v, r, _) ->
          match compare kmax k with
          | -1 -> aux_range l acc
          |  0 -> f k v (fold f l acc)
          |  1 -> fold_max r (f k v (fold f l acc))
          |  x -> invalid_arg
               (Printf.sprintf
                  "[Map.fold_range_compare] Unexpected result of compare (%d)" x)
    (* Fold until find kmin. *)
    and fold_min m acc = match m with
      | Empty -> acc
      | Node (l, k, v, r, _) ->
          match compare kmin k with
          | -1 -> fold f r (f k v (fold_min l acc))
          |  0 -> fold f r (f k v acc)
          |  1 -> aux_range r acc
          |  x -> invalid_arg
               (Printf.sprintf
                  "[Map.fold_range_compare] Unexpected result of compare (%d)" x)
    (* Fold in range. *)
    and aux_range m acc =
      match m with
      | Empty -> acc
      | Node (l, k, v, r, _) ->
          match compare kmin k, compare kmax k with
          | -1,  1 -> fold_max r (f k v (fold_min l acc))
          | -1,  0 -> f k v (fold_min l acc)
          | -1, -1 -> aux_range l acc
          |  0,  1 -> fold_max r (f k v acc)
          |  0,  0 -> f k v acc
          |  1,  1 -> aux_range r acc
          |  1,  0    (* Smaller than kmin and equals to kmax!? *)
          |  1, -1    (* Smaller than kmin and smaller than kmax!? *)
          |  0, -1 -> (* Equals to kmin and higher to kmax!? *)
               assert false
          |  x,  y -> invalid_arg
               (Printf.sprintf
                  "[Map.fold_range_compare] Unexpected result of compare (%d, %d)" x y)
    in aux_range m acc

  let fold_range x = fold_range_compare Ord.compare x

  let fold_length ~start ~length f m acc =
    (* Different tool functions used depending on the sign of [length],
       to avoid code duplication. Probably not more readable, though. *)
    let (cmp, length, pr_l, pr_r) =
      if length >= 0 then
        (Ord.compare, length, fst, snd)
      else
        ((fun x y -> 0 - Ord.compare x y), 0 - length, snd, fst)
    in
    (* [n] is the number of applications of [f] so far *)
    let rec aux n f m acc =
      if n = length then (acc, n) else
        match m with
        | Empty -> (acc, n)
        | Node (l, v, d, r, _) ->
            let l = pr_l (l, r) in
            let r = pr_r (l, r) in
            match cmp start v with
            | -1 ->
                let (acc, n) = aux n f l acc in
                if n = length then (acc, n) else
                  let (acc, n) = (f v d acc, n + 1) in
                  aux n f r acc
            | 0 ->
                let (acc, n) = (f v d acc, n + 1) in
                aux n f r acc
            | 1 ->
                aux n f r acc
            | _ -> assert false
    in
    let (acc, n) = aux 0 f m acc in
    assert (n <= length);
    (* Value of [n] is the number of elements that we managed to gather;
       it's equal to [length], unless not enough elements in the tree. *)
    acc

  (*     let rec fold2 f m1 m2 acc = match m1, m2 with *)
  (*     | Empty, Empty -> acc *)
  (*     | Node (l, v, d, r, _) -> fold f r (f v d (fold f l acc)) *)

  let rec fold_rev f m acc = match m with
    | Empty -> acc
    | Node (l, v, d, r, _) -> fold_rev f l (f v d (fold_rev f r acc))

  let filter_val f t =
    fold (fun k v acc -> if f v then add k v acc else acc) t empty

  let filter_keys f m =
    let rec aux = function
      | Empty -> Empty
      | Node (l, v, d, r, _h) ->
          if f v
          then bal (aux l) v d (aux r)
          else _merge (aux l) (aux r)
    in aux m

  module Iter : (IterSig.S with type +'a element = key * 'a and type +'a structure = 'a t and type +'a t = ((key *'a ) * 'a t) list) =
  struct
    type 'a structure = 'a t
    type 'a element = key * 'a
    type 'a t = ('a element * 'a structure) list

    let rec aux_make acc= function
      | Empty -> acc
      | Node (l, v, d, r, _) ->
          let acc=((v,d),r)::acc in
          aux_make acc l

    let make m = aux_make [] m
          (* let make_iterator m = let rec aux = function | Empty ->
             [] | Node (l, v, d, r, _) -> (v, d, r) :: aux l in List.rev (aux
             m) *)
    let get = function
      | ((v, d), _) :: _ -> v, d
      | _ -> raise IteratorEnd
    let next = function
      | (_, Empty) :: tl -> tl
      | (_, r) :: tl -> aux_make tl r
      | _ -> raise IteratorEnd
    let at_end i = i = []

    (* TODO change failwith to raise NotImplemented *)
    let remaining _i = failwith ("NotImplemented BaseMap.Make.Iter.remaining")
  end

  let rec fold_map2 f m m' acc =
    let iter' = Iter.make m' in
    (* the result of the map, the accumulator and the iterator*)
    let rec aux iter acc = function
      | Empty -> Empty, acc, iter
      | Node (l, v, d, r, h) ->
          let l, acc, iter = aux iter acc l in
          let () = if Iter.at_end iter then invalid_arg "baseMap.fold_map2: not enough element in second map" in
          let v',e = Iter.get iter in
          let () = if not (v'=v) then invalid_arg "baseMap.fold_map2: map with different elements" in
          let iter = Iter.next iter in
          let acc, e = f v d e acc in
          let r, acc, iter = aux iter acc r in
          let m = Node (l, v, e, r, h) in
          m, acc, iter
    in
    let m, result, iter'=aux iter' acc m in
    if Iter.at_end iter' then result, m
    else invalid_arg "baseMap.fold_map2 not enough element in first map"

  module RevIter : (IterSig.S with type +'a element = key * 'a and type +'a structure = 'a t) =
    struct
    type 'a structure = 'a t
    type 'a element = key * 'a
    type 'a t = ('a structure * 'a element ) list

    let rec make = function
      | Empty -> []
      | Node (l, v, d, r, _) -> make r @ [l, (v, d)]
          (* let make_iterator m = let rec aux = function | Empty ->
             [] | Node (l, v, d, r, _) -> (v, d, r) :: aux l in List.rev (aux
             m) *)
    let get = function
      | (_, (v, d)) :: _ -> v, d
      | _ -> raise IteratorEnd
    let next = function
      | (Empty, _) :: tl -> tl
      | (r, _) :: tl -> make r @ tl
      | _ -> raise IteratorEnd
    let at_end i = i = []

    (* TODO change failwith to raise NotImplemented *)
    let remaining _i = failwith ("NotImplemented BaseMap.Make.Iter.remaining")
  end

  type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

  let rec cons_enum m e =
    match m with
      Empty -> e
    | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

  let compare cmp m1 m2 =
    let rec compare_aux e1 e2 =
      match (e1, e2) with
        (End, End) -> 0
      | (End, _)  -> -1
      | (_, End) -> 1
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
          let c = Ord.compare v1 v2 in
          if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
              compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
    in compare_aux (cons_enum m1 End) (cons_enum m2 End)

  let equal cmp m1 m2 =
    let rec equal_aux e1 e2 =
      match (e1, e2) with
        (End, End) -> true
      | (End, _)  -> false
      | (_, End) -> false
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
          Ord.compare v1 v2 = 0 && cmp d1 d2 &&
      equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
    in equal_aux (cons_enum m1 End) (cons_enum m2 End)

  let from_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l

  let fold_assoc k v acc = (k, v) :: acc

  let to_list t = fold fold_assoc t []
  let keys t = fold (fun k _ acc -> k :: acc) t []
  let elts t = fold (fun _ el acc -> el :: acc) t []

  let ordered_list t = fold_rev fold_assoc t []
  let rev_ordered_list t = fold fold_assoc t []

  let merge_i f m1 m2 = (* warning: [f] may be not commutative! *)
    let add_merge x y acc =
      add x (
        match find_opt x acc with
        | Some v -> f x y v
        | _ -> y
      ) acc
    in
    let add_merge2 x y acc =
      add x (
        match find_opt x acc with
        | Some v -> f x v y
        | _ -> y
      ) acc
    in
    if height m1 <= height m2 then
      fold add_merge m1 m2
    else
      fold add_merge2 m2 m1

  let merge f m1 m2 = merge_i (fun _ x y -> f x y) m1 m2

  let safe_merge m1 m2 =
    let m1, m2 = if height m1 <= height m2 then m1, m2 else m2, m1 in
    try fold safe_add m1 m2 with
    | Invalid_argument "Base.Map.safe_add" ->
        raise (Invalid_argument "Base.Map.safe_merge")

  (* Heritage of old unused functions *)
  let of_set f s =
    let module S = Set.Make(Ord) in
    S.fold (fun x acc -> add x (f x) acc) s empty

  let incr ?(step=1) k m =
    add k ((Option.default 0 (find_opt k m)) + step) m

  let rename f m =
    fold (fun k elt acc -> add (f k) elt acc) m empty

  (* cf doc *)
  let pp sep ppe fmt t =
    let fiter elt val_ =
      ppe fmt elt val_ ;
      Format.fprintf fmt sep
    in
    iter fiter t

  let compare_key = Ord.compare

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

  let rec choose_opt = function
    | Empty -> None
    | Node(_, k, v, _r, _) -> Some (k, v)

  let example_diff s1 s2 =
    let diff_ = diff s1 s2 in
    match choose_opt diff_ with
    | Some elt -> Some elt
    | None ->
        let diff = diff s2 s1 in
        match choose_opt diff with
        | Some elt -> Some elt
        | None -> None

  let from_sorted_array keys vals =
    let len_keys = Array.length keys in
    let len_vals = Array.length vals in
    if len_keys <> len_vals then assert false ;
    let rec aux left right =
      if left > right
      then Empty
      else
        let midle = (left + right) lsr 1 in
        let left_tree = aux left (pred midle) in
        let right_tree = aux (succ midle) right in
        let key = Array.unsafe_get keys midle in
        let value = Array.unsafe_get vals midle in
        create left_tree key value right_tree
    in
    aux 0 (pred len_keys)
end
