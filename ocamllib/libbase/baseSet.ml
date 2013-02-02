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
module Make (Ord: OrderedTypeSig.S) : (BaseSetSig.S with type elt = Ord.t) =
struct
  type elt = Ord.t
  type t =
    | Empty
    | Node of t * elt * t * int (* int (* size *) *)

  (* Sets are represented by balanced binary trees (the heights of the
     children differ by at most 2 *)

  let height = function
      Empty -> 0
    | Node(_, _, _, h) -> h

  (* Creates a new node with left son l, value v and right son r.
     We must have all elements of l < v < all elements of r.
     l and r must be balanced and | height l - height r | <= 2.
     Inline expansion of height for better speed. *)

  let create l v r =
    let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
    let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
    Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

  (* Same as create, but performs one step of rebalancing if necessary.
     Assumes l and r balanced and | height l - height r | <= 3.
     Inline expansion of create for better speed in the most frequent case
     where no rebalancing is required. *)

  let bal l v r =
    let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
    let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
    if hl > hr + 2 then begin
      match l with
        Empty -> invalid_arg "Set.bal"
      | Node(ll, lv, lr, _) ->
          if height ll >= height lr then
            create ll lv (create lr v r)
          else begin
            match lr with
              Empty -> invalid_arg "Set.bal"
            | Node(lrl, lrv, lrr, _)->
                create (create ll lv lrl) lrv (create lrr v r)
          end
    end else if hr > hl + 2 then begin
      match r with
        Empty -> invalid_arg "Set.bal"
      | Node(rl, rv, rr, _) ->
          if height rr >= height rl then
            create (create l v rl) rv rr
          else begin
            match rl with
              Empty -> invalid_arg "Set.bal"
            | Node(rll, rlv, rlr, _) ->
                create (create l v rll) rlv (create rlr rv rr)
          end
    end else
      Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

  (* Insertion of one element *)

  let rec add x = function
      Empty -> Node(Empty, x, Empty, 1)
    | Node(l, v, r, _) as t ->
        let c = Ord.compare x v in
        if c = 0 then t else
          if c < 0 then bal (add x l) v r else bal l v (add x r)

  (* Same as create and bal, but no assumptions are made on the
     relative heights of l and r. *)

  let rec join l v r =
    match (l, r) with
      (Empty, _) -> add v r
    | (_, Empty) -> add v l
    | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
        if lh > rh + 2 then bal ll lv (join lr v r) else
          if rh > lh + 2 then bal (join l v rl) rv rr else
            create l v r

  (* Smallest and greatest element of a set *)

  let rec min_elt = function
      Empty -> raise Not_found
    | Node(Empty, v, _r, _) -> v
    | Node(l, _v, _r, _) -> min_elt l

  let rec max_elt = function
      Empty -> raise Not_found
    | Node(_l, v, Empty, _) -> v
    | Node(_l, _v, r, _) -> max_elt r

  (* Remove the smallest element of the given set *)

  let rec remove_min_elt = function
      Empty -> invalid_arg "Set.remove_min_elt"
    | Node(Empty, _v, r, _) -> r
    | Node(l, v, r, _) -> bal (remove_min_elt l) v r

  (* Merge two trees l and r into one.
     All elements of l must precede the elements of r.
     Assume | height l - height r | <= 2. *)

  let merge t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

  (* Merge two trees l and r into one.
     All elements of l must precede the elements of r.
     No assumption on the heights of l and r. *)

  let concat t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)

  (* Splitting.  split x s returns a triple (l, present, r) where
     - l is the set of elements of s that are < x
     - r is the set of elements of s that are > x
     - present is false if s contains no element equal to x,
     or true if s contains an element equal to x. *)

  let rec split x = function
      Empty ->
        (Empty, false, Empty)
    | Node(l, v, r, _) ->
        let c = Ord.compare x v in
        if c = 0 then (l, true, r)
        else if c < 0 then
          let (ll, pres, rl) = split x l in (ll, pres, join rl v r)
        else
          let (lr, pres, rr) = split x r in (join l v lr, pres, rr)

  (* Implementation of the set operations *)

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let rec mem x = function
      Empty -> false
    | Node(l, v, r, _) ->
        let c = Ord.compare x v in
        c = 0 || mem x (if c < 0 then l else r)

  let singleton x = Node(Empty, x, Empty, 1)

  let rec remove x = function
      Empty -> Empty
    | Node(l, v, r, _) ->
        let c = Ord.compare x v in
        if c = 0 then merge l r else
          if c < 0 then bal (remove x l) v r else bal l v (remove x r)

  let rec size = function
    | Empty -> 0
    | Node (l, _, r, _) -> 1 + size l + size r

  let draw t =
    let rec aux = function
      | Empty -> raise Not_found
      | Node (l, v, r, _) ->
          let sl = size l
          and sr = size r in
          match Random.int (1 + sl + sr) with
          | 0 -> v, remove v t
          | i when i <= sl -> aux l
          | _ -> aux r
    in
    aux t

  let rec union s1 s2 =
    match (s1, s2) with
      (Empty, t2) -> t2
    | (t1, Empty) -> t1
    | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
        if h1 >= h2 then
          if h2 = 1 then add v2 s1 else begin
            let (l2, _, r2) = split v1 s2 in
            join (union l1 l2) v1 (union r1 r2)
          end
        else
          if h1 = 1 then add v1 s2 else begin
            let (l1, _, r1) = split v2 s1 in
            join (union l1 l2) v2 (union r1 r2)
          end

  let rec inter s1 s2 =
    match (s1, s2) with
      (Empty, _t2) -> Empty
    | (_t1, Empty) -> Empty
    | (Node(l1, v1, r1, _), t2) ->
        match split v1 t2 with
          (l2, false, r2) ->
            concat (inter l1 l2) (inter r1 r2)
        | (l2, true, r2) ->
            join (inter l1 l2) v1 (inter r1 r2)

  let rec diff s1 s2 =
    match (s1, s2) with
      (Empty, _t2) -> Empty
    | (t1, Empty) -> t1
    | (Node(l1, v1, r1, _), t2) ->
        match split v1 t2 with
          (l2, false, r2) ->
            join (diff l1 l2) v1 (diff r1 r2)
        | (l2, true, r2) ->
            concat (diff l1 l2) (diff r1 r2)

  type enumeration = End | More of elt * t * enumeration

  let rec cons_enum s e =
    match s with
      Empty -> e
    | Node(l, v, r, _) -> cons_enum l (More(v, r, e))

  let rec compare_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> 0
    | (End, _)  -> -1
    | (_, End) -> 1
    | (More(v1, r1, e1), More(v2, r2, e2)) ->
        let c = Ord.compare v1 v2 in
        if c <> 0
        then c
        else compare_aux (cons_enum r1 e1) (cons_enum r2 e2)

  let compare s1 s2 =
    compare_aux (cons_enum s1 End) (cons_enum s2 End)

  let equal s1 s2 =
    compare s1 s2 = 0

  let rec subset s1 s2 =
    match (s1, s2) with
      Empty, _ ->
        true
    | _, Empty ->
        false
    | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
        let c = Ord.compare v1 v2 in
        if c = 0 then
          subset l1 l2 && subset r1 r2
        else if c < 0 then
          subset (Node (l1, v1, Empty, 0)) l2 && subset r1 t2
        else
          subset (Node (Empty, v1, r1, 0)) r2 && subset l1 t2

  let rec iter f = function
      Empty -> ()
    | Node(l, v, r, _) -> iter f l; f v; iter f r

  let rec fold f s accu =
    match s with
      Empty -> accu
    | Node(l, v, r, _) -> fold f r (f v (fold f l accu))

  let rec fold_rev f s accu =
    match s with
    | Empty -> accu
    | Node(l, v, r, _) -> fold_rev f l (f v (fold_rev f r accu))

  let map f s =
    fold (fun x acc -> add (f x) acc) s Empty

  let rec for_all p = function
      Empty -> true
    | Node(l, v, r, _) -> p v && for_all p l && for_all p r

  let rec exists p = function
      Empty -> false
    | Node(l, v, r, _) -> p v || exists p l || exists p r

  let filter p s =
    let rec filt accu = function
      | Empty -> accu
      | Node(l, v, r, _) ->
          filt (filt (if p v then add v accu else accu) l) r in
    filt Empty s

  let partition p s =
    let rec part (t, f as accu) = function
      | Empty -> accu
      | Node(l, v, r, _) ->
          part (part (if p v then (add v t, f) else (t, add v f)) l) r in
    part (Empty, Empty) s

  let rec cardinal = function
      Empty -> 0
    | Node(l, _v, r, _) -> cardinal l + 1 + cardinal r

  let rec elements_aux accu = function
      Empty -> accu
    | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

  let elements s =
    elements_aux [] s

  let add_list l t = List.fold_left (fun acc v -> add v acc) t l
  let from_list l = add_list l empty

  let choose = function
    | Empty -> raise Not_found
    | Node(_, v, _, _) -> v

  let rec choose_opt = function
    | Empty -> None
    | Node (_, v, _r, _) -> Some v

  let example_diff s1 s2 =
    let diff_ = diff s1 s2 in
    match choose_opt diff_ with
    | Some elt -> Some elt
    | None ->
        let diff = diff s2 s1 in
        match choose_opt diff with
        | Some elt -> Some elt
        | None -> None

  let complete_join big small =
    fold add small big

  let rec complete fun_prefixe k = function
    | Empty -> Empty
    | Node (l, key, r, _) ->
        if (fun_prefixe k key) then
          (** k est un prefixe de key *)
          let set_1 = complete fun_prefixe k l in
          let set_2 = complete fun_prefixe k r in
          let joined_set =
            if (height set_1) >= (height set_2)
            then complete_join set_1 set_2
            else complete_join set_2 set_1
          in add key joined_set
        else
          if k < key then complete fun_prefixe k l
          else complete fun_prefixe k r

  (* cf doc *)
  let pp sep ppe fmt t =
    let fiter elt =
      ppe fmt elt ;
      Format.fprintf fmt sep
    in
    iter fiter t

  let compare_elt = Ord.compare

  let safe_union s1 s2 =
    let u = union s1 s2 in
    (* We ensure that the 2 sets to join were disjoint. This is the case if
       the sum of their sizes equal the size of the set obtained after
       union. *)
    if not (size u = size s1 + size s2) then
      raise (Invalid_argument "Base.Set.safe_union") ;
    u

  let from_sorted_array elts =
    let rec aux left right =
      if left > right
      then Empty
      else
        let midle = (left + right) lsr 1 in
        let left_tree = aux left (pred midle) in
        let right_tree = aux (succ midle) right in
        let elt = Array.unsafe_get elts midle in
        create left_tree elt right_tree
    in
    aux 0 (pred (Array.length elts))
end
