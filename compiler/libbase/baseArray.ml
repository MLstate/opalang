(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)
(* CF mli *)

include Array
let is_empty a = Array.length a = 0
let unsafe_create l = create l (Obj.magic 0)
let copy_memory a =
  let l = length a in
  if l = 0 then [||] else begin
    let res = unsafe_create l in
    for i = 0 to pred l do
      unsafe_set res i (unsafe_get a i)
    done;
    res
  end
let unsafe_blit a1 ofs1 a2 ofs2 len =
  if ofs1 < ofs2 then
    (* Top-down copy *)
    for i = len - 1 downto 0 do
      unsafe_set a2 (ofs2 + i) (unsafe_get a1 (ofs1 + i))
    done
  else
    (* Bottom-up copy *)
    for i = 0 to len - 1 do
      unsafe_set a2 (ofs2 + i) (unsafe_get a1 (ofs1 + i))
    done
let append_memory a1 a2 =
  let l1 = Array.length a1 and l2 = Array.length a2 in
  if l1 = 0 && l2 = 0 then [||] else begin
    let r = unsafe_create (l1 + l2) (*This deactivates [float] optimizations*) in
    for i = 0 to l1 - 1 do Array.unsafe_set r i (Array.unsafe_get a1 i) done;
    for i = 0 to l2 - 1 do Array.unsafe_set r (i + l1) (Array.unsafe_get a2 i) done;
    r
  end
let swap a i j =
  let t = a.(i) in
  a.(i) <- a.(j) ;
  a.(j) <- t
let insert_sorted ?(cmp=Pervasives.compare) ?(dupl=false) x a =
  let l = Array.length a in
  let insert_and_blit x i =
    let t = unsafe_create (succ l) in
    t.(i) <- x;
    unsafe_blit a 0 t 0 i;
    unsafe_blit a i t (i+1) (l-i);
    t
  in
  let rec aux i t =
    if i < l then
      let c = cmp x a.(i) in
      if c < 0 then insert_and_blit x i
      else if c=0 then (if not dupl then t else insert_and_blit x i)
      else aux (i+1) t
    else insert_and_blit x i
  in aux 0 a

(*
  Mathieu Wed Nov  3 16:03:15 CET 2010
  I've benched VS snd (fold_left (fun (i, acc) x -> (succ i, f acc x i)) (0, init) a),
  The GC die in case of too much tuple allocations.
*)
let fold_left_i f acc a =
  let p = ref (-1) in
  let f acc e =
    incr(p);
    f acc e !p
  in
  Array.fold_left f acc a

(** max et min d'un tableau (doit être non vide) *)
let max, min =
  let oper op a =
    let l = length a in
    assert (l > 0) ;
    let r = ref a.(0) in
    for i = 1 to pred l do
      r := op !r (unsafe_get a i)
    done ;
    !r
  in
  (fun a -> oper Pervasives.max a),
  (fun a -> oper Pervasives.min a)

(** argmax et argmin d'un tableau (doit être non vide) *)
let argmax, argmin =
  let arg_oper op a =
    let l = length a in
    assert (l > 0) ;
    let m = ref 0 in
    for i = 1 to pred l do
      if op (unsafe_get a i) (unsafe_get a !m) then m := i
    done ;
    !m
  in
  (fun a -> arg_oper (>) a),
  (fun a -> arg_oper (<) a)

let map_some f a =
  let a' = map f a in
  let nb = fold_left (fun acc -> function Some _ -> succ acc | _ -> acc) 0 a' in
  let pos = ref 0 in
  init nb (
    fun _ ->
      while not(Option.is_some a'.(!pos)) do incr pos done ;
      let r = match a'.(!pos) with Some r -> r | _ -> assert false in
      incr pos ; r
  )

let mapi_some f a =
  let a' = mapi f a in
  let nb = fold_left (fun acc -> function Some _ -> succ acc | _ -> acc) 0 a' in
  let pos = ref 0 in
  init nb (
    fun _ ->
      while not(Option.is_some a'.(!pos)) do incr pos done ;
      let r = match a'.(!pos) with Some r -> r | _ -> assert false in
      incr pos ; r
  )

let map2 f a1 a2 =
  let l = length a1 in
  assert (l = length a2) ;
  init l (fun i -> f (unsafe_get a1 i) (unsafe_get a2 i))

let fill_some a v pfrom pto =
  for i = pfrom to pred pto do
    match a.(i) with
    | None -> a.(i) <- Some v
    | _ -> ()
  done

let mem a tab =
  let l = length tab in
  let rec aux i =
    if i >= l then false
    else if unsafe_get tab i = a then true else aux (succ i)
  in aux 0

let exists fct tab =
  let l = length tab in
  let rec aux i =
    if i >= l then false
    else if fct (unsafe_get tab i) then true else aux (succ i)
  in aux 0

let split a =
  let len  = Array.length a in
  if len = 0 then ([||], [||])
  else
    let (left_init, right_init) = a.(0) in
    let left = Array.make len left_init
    and right= Array.make len right_init in
    for i = 0 to len - 1 do
      let (left_item, right_item) = unsafe_get a i in
      unsafe_set left  i left_item;
      unsafe_set right i right_item
    done;
    (left, right)

let find a x =
  let l = length a in
  let rec aux i =
    if i >= l then raise Not_found
    else if unsafe_get a i = x then i else aux (succ i)
  in aux 0

let compare cmp a b =
  if a == b then 0 else
  let len_a = length a in
  let len_b = length b in
  let rec aux i =
    if i >= len_a then
      if i >= len_b then 0 else -1
    else if i >= len_b then
      1
    else
      let va = unsafe_get a i in
      let vb = unsafe_get b i in
      let cmp = cmp va vb in
      if cmp <> 0 then cmp
      else aux (succ i)
  in aux 0


let to_string f a =
  let rec aux acc i =
    if i < 0 then acc
    else aux ((f a.(i))^acc) (pred i)
  in
    let l = length a in
    if l = 0 then "[||]"
    else aux "" (l -1);;

let print f a =
  let rec aux acc i =
    if i = 0 then "[|"^(f a.(i))^acc
    else aux (";"^(f a.(i))^acc) (pred i)
  in
    let l = length a in
    if l = 0 then "[||]"
    else aux "|]" (l-1);;

let filteri fct a =
  let len = length a in
  if len = 0
  then a
  else
    let rec aux filter acc i =
      if i = len
      then
        filter, acc
      else
        let ai = unsafe_get a i in
        if fct i ai
        then
          aux (succ filter) (ai :: acc) (succ i)
        else
          aux filter acc (succ i)
    in
    let filter, acc = aux 0 [] 0 in
    let fa = create filter (unsafe_get a 0) in
    let rec fill acc i =
      match acc with
      | [] -> ()
      | hd :: tl -> (
          unsafe_set fa i hd ;
          fill tl (pred i)
        )
    in
    fill acc (pred filter) ;
    fa

let filter fct a =
  let fct _ a = fct a in
  filteri fct a
