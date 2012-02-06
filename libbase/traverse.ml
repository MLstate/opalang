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
(* depends *)
module List = BaseList

(* -- *)

module Utils =
struct
  type ('a, 'at, 'bt, 'b) sub = 'a -> ('bt list -> 'b) * 'at list
  let sub_2 sub1 sub2 = fun (x,y) ->
    let unsub1, list1 = sub1 x in
    let unsub2, list2 = sub2 y in
    let length = List.length list1 in
      (fun l ->
         let l1, l2 = List.split_at length l in
           (unsub1 l1, unsub2 l2)),
      (list1 @ list2)
  (* could be simplified using wrap *)
  let sub_3 sub1 sub2 sub3 = fun (a,b,c) ->
    let unsub,l = sub_2 (sub_2 sub1 sub2) sub3 ((a,b),c) in
      (fun l -> let ((a,b),c) = unsub l in (a,b,c)), l
  let sub_4 sub1 sub2 sub3 sub4 (a,b,c,d) =
    let unsub,l = sub_2 (sub_2 sub1 sub2) (sub_2 sub3 sub4) ((a,b),(c,d)) in
      (fun l -> let ((a,b),(c,d)) = unsub l in (a,b,c,d)), l

  let sub_list sub = fun args ->
    let unsubs, lists = List.split (List.map sub args) in
    let lengths = List.map List.length lists in
      (fun l ->
         let ll = List.split_ats lengths l in
           List.map2 (fun f x -> f x) unsubs ll),
    (List.concat lists)

  let sub_current e = (function [e] -> e | _ ->  assert false), [e]
  let sub_ignore x = (function [] -> x | _ -> assert false), []

  let sub_option sub_e opt =
    match opt with
    | None -> (fun x -> assert (List.is_empty x); None), []
    | Some e ->
        let usub, l = sub_e e in
        (fun l -> Some (usub l)), l

  let wrap cons (unsub,l) = (fun l -> cons (unsub l)), l
end

(* TODO: remove this, merge with TRAVERSE *)
module type OLD_TRAVERSE =
sig

  type 'p t constraint 'p = _ * _ * _
  val traverse_iter : (('p t -> unit) -> 'p t -> unit) -> 'p t -> unit
  val traverse_map : (('p t -> 'p t) -> 'p t -> 'p t) -> 'p t -> 'p t
  val traverse_fold : (('a -> 'p t -> 'a) -> 'a -> 'p t -> 'a) -> 'a -> 'p t -> 'a
  val traverse_foldmap : (('a -> 'p t -> 'a * 'p t) -> 'a -> 'p t -> 'a * 'p t) -> 'a -> 'p t -> 'a * 'p t
  val traverse_exists : (('p t -> bool) -> 'p t -> bool) -> 'p t -> bool
  val traverse_forall : (('p t -> bool) -> 'p t -> bool) -> 'p t -> bool
  val traverse_fold_context_down : (('env -> 'a -> 'p t -> 'a) -> 'env -> 'a -> 'p t -> 'a) -> 'env -> 'a -> 'p t -> 'a
  val iter : ('p t -> unit) -> 'p t -> unit
  val iter_up : ('p t -> unit) -> 'p t -> unit
  val iter_down : ('p t -> unit) -> 'p t -> unit
  val map : ('p t -> 'p t) -> 'p t -> 'p t
  val map_up : ('p t -> 'p t) -> 'p t -> 'p t
  val map_down : ('p t -> 'p t) -> 'p t -> 'p t
  val fold : ('a -> 'p t -> 'a) -> 'a -> 'p t -> 'a
  val fold_up : ('a -> 'p t -> 'a) -> 'a -> 'p t -> 'a
  val fold_down : ('a -> 'p t -> 'a) -> 'a -> 'p t -> 'a
  val foldmap : ('a -> 'p t -> 'a * 'p t) -> 'a -> 'p t -> 'a * 'p t
  val foldmap_up : ('a -> 'p t -> 'a * 'p t) -> 'a -> 'p t -> 'a * 'p t
  val foldmap_down : ('a -> 'p t -> 'a * 'p t) -> 'a -> 'p t -> 'a * 'p t
  val exists : ('p t -> bool) -> 'p t -> bool
  val exists_up : ('p t -> bool) -> 'p t -> bool
  val exists_down : ('p t -> bool) -> 'p t -> bool
  val find : ('p t -> bool) -> 'p t -> 'p t option
  val find_up : ('p t -> bool) -> 'p t -> 'p t option
  val find_down : ('p t -> bool) -> 'p t -> 'p t option
  val findmap : ('p t -> 'a option) -> 'p t -> 'a option
  val findmap_up : ('p t -> 'a option) -> 'p t -> 'a option
  val findmap_down : ('p t -> 'a option) -> 'p t -> 'a option

  val traverse_fold_right :  (('b t -> 'a -> 'a) -> 'b t -> 'a -> 'a) -> 'b t -> 'a -> 'a
  val fold_up_combine : ?combine:('a -> 'a -> 'a) -> ('a -> 'b t -> 'a) -> 'a -> 'b t -> 'a
  val fold_right_down : ('b t -> 'a -> 'a) -> 'b t -> 'a -> 'a
  val foldmap_up_combine : ?combine:('a -> 'a -> 'a) -> ('a -> 'b t -> 'a * 'b t) -> 'a -> 'b t -> 'a * 'b t
  val map_nonrec : ('b t -> 'b t) -> 'b t -> 'b t
  val fold_nonrec : ('a -> 'b t -> 'a) -> 'a -> 'b t -> 'a
  val foldmap_nonrec : ('a -> 'b t -> 'a * 'b t) -> 'a -> 'b t -> 'a * 'b t
  val map_down_fix : ('b t -> 'b t) -> 'b t -> 'b t
  type ('b, 'c) sub = ('b, 'c t, 'c t , 'b) Utils.sub
  val lift_iter_up : ('b,'c) sub -> ('c t -> unit) -> ('b -> unit)
  val lift_iter_down : ('b,'c) sub -> ('c t -> unit) -> ('b -> unit)
  val lift_map_up : ('b,'c) sub -> ('c t -> 'c t) -> ('b -> 'b)
  val lift_map_down : ('b,'c) sub -> ('c t -> 'c t) -> ('b -> 'b)
  val lift_fold_up_combine : ('b,'c) sub -> ?combine:('a -> 'a -> 'a) -> ('a -> 'c t -> 'a) -> ('a -> 'b -> 'a)
  val lift_fold : ('b,'c) sub -> ('a -> 'c t -> 'a) -> ('a -> 'b -> 'a)
  val lift_fold_right_down : ('b,'c) sub -> ('c t -> 'a -> 'a) -> ('b -> 'a -> 'a)
  val lift_foldmap_up : ('b,'c) sub -> ('a -> 'c t -> 'a * 'c t) -> ('a -> 'b -> 'a * 'b)
  val lift_foldmap_down : ('b,'c) sub -> ('a -> 'c t -> 'a * 'c t) -> ('a -> 'b -> 'a * 'b)
  val lift_exists : ('b,'c) sub -> ('c t -> bool) -> ('b -> bool)
end


(* This internal module generates function find*, find_map* and *exists* from an iteration function
 * When an object is found, the execution is stopped (just like if you had raised an exception) *)
module type IteratedType =
  sig
    type 'a t constraint 'a = _ * _ * _
    type 'a container constraint 'a = _ * _ * _
    val iter_up : ('a t -> unit) -> 'a container -> unit
    val iter_down : ('a t -> unit) -> 'a container -> unit
  end
module MakeFromIter (X : IteratedType) =
struct
  let find_up pred tree =
    Return.set_checkpoint_opt
      (fun label ->
         X.iter_up (fun e -> if pred e then Return.return label e) tree)
  let find_down pred tree =
    Return.set_checkpoint_opt
      (fun label ->
         X.iter_down (fun e -> if pred e then Return.return label e) tree)
  let find = find_up

  let findmap_up pred tree =
    Return.set_checkpoint_opt
      (fun label ->
         X.iter_up
           (fun e ->
              match pred e with
              | None -> ()
              | Some a -> Return.return label a) tree)
  let findmap_down pred tree =
    Return.set_checkpoint_opt
      (fun label ->
         X.iter_down
           (fun e ->
              match pred e with
              | None -> ()
              | Some a -> Return.return label a) tree)
  let findmap = findmap_up

  let exists_up f tree = find_up f tree <> None
  let exists_down f tree = find_down f tree <> None
  let exists = exists_up
end

(* FIRST VERSION *)
module Make (X : TraverseInterface.S) (* : TRAVERSE with type 'a t = 'a X.t *) = struct
  type 'a t = 'a X.t

  let subs e = snd (X.subs_cons e)
  let subs_cons e = (* An optimisation ; no idea if it's really worth it *)
    let f_cons, sub_e = X.subs_cons e in
    (fun sub_e' -> if sub_e' == sub_e then e else f_cons sub_e'),
    sub_e

  (* Higher-order traverse functions *)

  let rec traverse_iter f e =
    f (fun e -> List.iter (traverse_iter f) (subs e)) e

  let rec traverse_map f e =
    f (fun e -> let f_cons, sub_e = subs_cons e in f_cons (List.map (traverse_map f) sub_e)) e

  let rec traverse_fold f e =
    f (fun acc e -> List.fold_left (traverse_fold f) acc (subs e)) e

  let rec traverse_fold_right f e =
    f (fun e acc -> List.fold_right (traverse_fold_right f) (subs e) acc) e

  let rec traverse_foldmap f e =
    f (fun acc e ->
         let f_cons, sub_e = subs_cons e in
         let acc, el = List.fold_left_map (traverse_foldmap f) acc sub_e in
         acc, f_cons el) e

  let traverse_foldmap_context_down f =
    let rec tra env acc e =
      let f_cons, sub_e = subs_cons e in
      let acc, el = List.fold_left_map (fun acc e -> aux env acc e) acc sub_e in
      acc, f_cons el
    and aux env acc e = f tra env acc e in
    aux
  let traverse_fold_context_down f =
    let rec tra env acc e =
      let _, sub_e = subs_cons e in
      List.fold_left (fun acc e -> aux env acc e) acc sub_e
    and aux env acc e = f tra env acc e in
    aux

  let rec traverse_exists f e =
    f (fun e -> List.exists (traverse_exists f) (subs e)) e

  let rec traverse_forall f e =
    f (fun e -> List.for_all (traverse_forall f) (subs e)) e

  (* Usual, more simple traverse functions *)

  let iter_up f = traverse_iter (fun tra e -> tra e; f e)
  let iter_down f = traverse_iter (fun tra e -> f e; tra e)
  let iter = iter_down

  let map_up f = traverse_map (fun tra e -> f (tra e))
  let map_down f = traverse_map (fun tra e -> tra (f e))
  let map = map_up

  let fold_up_combine ?(combine = fun _ b -> b) f acc = traverse_fold (fun tra acc' e -> combine acc' (f (tra acc e) e)) acc
  let fold_up f = traverse_fold (fun tra acc e -> f (tra acc e) e)
  let fold_down f = traverse_fold (fun tra acc e -> tra (f acc e) e)
  let fold = fold_down
  let fold_right_down f = traverse_fold_right (fun tra e acc -> tra e (f e acc))
  let foldmap_up_combine ?(combine = fun _ b -> b) f acc = traverse_foldmap
    (fun tra acc' e ->
       let acc, e = tra acc e in
       let acc, e = f acc e in
       combine acc' acc, e)
    acc
  let foldmap_up f = traverse_foldmap (fun tra acc e -> let acc, e = tra acc e in f acc e)
  let foldmap_down f = traverse_foldmap (fun tra acc e -> let acc, e = f acc e in tra acc e)
  let foldmap = foldmap_up

  include MakeFromIter(
    struct
      type 'a t = ('b * 'c * 'd) X.t constraint 'a = 'b * 'c * 'd
      type 'a container = 'a t
      let iter_up = iter_up
      let iter_down = iter_down
    end
  )

  (* Non-recursive versions *)

  let map_nonrec f e =
    let f_cons, sub_e = subs_cons e in
    f_cons (List.map f sub_e)

  let fold_nonrec f acc e =
    let _, sub_e = subs_cons e in
    let acc = List.fold_left f acc sub_e in
    acc

  let foldmap_nonrec f acc e =
    let f_cons, sub_e = subs_cons e in
    let acc, el = List.fold_left_map f acc sub_e in
    acc, f_cons el

  (* Just because we had fun writing it :] *)
  let map_down_fix f = traverse_map
    (fun tra e ->
       let rec fixtra e =
         let e' = tra e in
         if e' = e then e else fixtra e' in
       fixtra (f e))


  type ('b, 'c) sub = ('b, 'c X.t, 'c X.t, 'b) Utils.sub

  let lift_iter_up sub f c =
    let _, l = sub c in
      List.iter (iter_up f) l
  let lift_iter_down sub f c =
    let _, l = sub c in
      List.iter (iter_down f) l
  let lift_map_up sub f c =
    let unsub, l = sub c in
    let l2 = List.map (map_up f) l in
      unsub l2
  let lift_map_down sub f c =
    let unsub, l = sub c in
    let l2 = List.map (map_down f) l in
      unsub l2
  let lift_fold_up_combine sub ?(combine=fun _ b -> b) f acc c =
    let _, l = sub c in
      List.fold_left (fold_up_combine ~combine f) acc l
  let lift_fold sub f acc c =
    let _, l = sub c in
      List.fold_left (fold_down f) acc l
  let lift_fold_right_down sub f c acc =
    let _, l = sub c in
      List.fold_right (fold_right_down f) l acc
  let lift_foldmap_up sub f acc c =
    let unsub, l = sub c in
    let acc2, l2 = List.fold_left_map (foldmap_up f) acc l in
      acc2, unsub l2
  let lift_foldmap_down sub f acc c =
    let unsub, l = sub c in
    let acc2, l2 = List.fold_left_map (foldmap_down f) acc l in
      acc2, unsub l2
  let lift_exists sub f c =
    let _, l = sub c in
      List.exists (exists f) l
end

module MakePair (Fst : TraverseInterface.S) (Snd : TraverseInterface.S) =
  Make (struct
    type 'a t = 'a Fst.t * 'a Snd.t
    let subs_cons (x1, x2) =
      let f1, l1 = Fst.subs_cons x1
      and f2, l2 = Snd.subs_cons x2 in
      let f l =
        let (l1, l2) = List.split l in
        (f1 l1, f2 l2)
      and l = List.combine l1 l2 in
      (f, l)
  end)

(* =============================================================== *)

open TraverseInterface

module Unoptimized =
struct

  type ('acc, 't, 't2) foldmap = ('acc -> 't -> 'acc * 't) -> 'acc -> 't2 -> 'acc * 't2
  let iter foldmap iter t = fst (foldmap (fun () t -> let _ = iter t in ((), t)) () t)
  let map foldmap map t = snd (foldmap (fun () t -> ((), map t)) () t)
  let fold foldmap fold acc t = fst (foldmap (fun acc t -> fold acc t, t) acc t)

  type ('acc, 'tA, 'tB) foldmapAB =
      ('acc -> 'tA -> 'acc * 'tA) ->
      ('acc -> 'tB -> 'acc * 'tB) ->
      'acc -> 'tA -> 'acc * 'tA
  let iterAB foldmap iterA iterB t =
    fst (foldmap (fun () tA -> iterA tA; ((), tA)) (fun () tB -> iterB tB; ((), tB)) () t)
  let mapAB foldmap mapA mapB t =
    snd (foldmap (fun () tA -> ((), mapA tA)) (fun () tB -> ((), mapB tB)) () t)
  let foldAB foldmap foldA foldB acc t =
    fst (foldmap (fun acc tA -> foldA acc tA, tA) (fun acc tB -> foldB acc tB, tB) acc t)
end

module Make2 (X : S2) =
struct
  type 'a t = 'a X.t constraint 'a = _ * _ * _
  type 'a container = 'a X.t constraint 'a = _ * _ * _
  let xfoldmap = X.foldmap
  let xiter = X.iter
  let xmap = X.map
  let xfold = X.fold
  exception StopExists
  let xexists exists t =
    try xiter (fun t -> if exists t then raise StopExists) t; false
    with StopExists -> true

  let xfindmap findmap t =
    let r = ref None in
    try
      xiter (fun t ->
               match findmap t with
               | Some a -> r := Some a; raise StopExists
               | None -> ()) t;
      None
    with StopExists -> !r

  (* <!> beware by factorizing arguments,
     it may lead to infinity recursive loop @runtime (should not, needs further investigation)
     Solution to this long standing mystery: you probably wrote
     [let rec traverse_foldmap f = f (xfoldmap (traverse_foldmap f))]
  *)

  (* Higher-order *)
  exception StopForall
  let xforall forall t =
    try xiter (fun t -> if not (forall t) then raise StopForall) t; true
    with StopForall -> false

  let traverse_iter f =
    let rec tra e = xiter aux e
    and aux e = f tra e in
    aux
  let traverse_map f =
    let rec tra e = xmap aux e
    and aux e = f tra e in
    aux
  let traverse_fold f =
    let rec tra acc e = xfold aux acc e
    and aux acc e = f tra acc e in
    aux
  let traverse_foldmap f =
    let rec tra acc e = xfoldmap aux acc e
    and aux acc e = f tra acc e in
    aux
  let traverse_exists f =
    let rec tra e = xexists aux e
    and aux e = f tra e in
    aux
  let traverse_forall f =
    let rec tra e = xforall aux e
    and aux e = f tra e in
    aux
  let traverse_findmap f =
    let rec tra e = xfindmap aux e
    and aux e = f tra e in
    aux
  let traverse_foldmap_context_down f =
    let rec tra env acc e = xfoldmap (fun acc e -> aux env acc e) acc e
    and aux env acc e = f tra env acc e in
    aux
  let traverse_map_context_down f =
    let rec tra env e = xmap (fun e -> aux env e) e
    and aux env e = f tra env e in
    aux
  let traverse_fold_context_down f =
    let rec tra env acc e = xfold (fun acc e -> aux env acc e) acc e
    and aux env acc e = f tra env acc e in
    aux
  let traverse_iter_context_down f =
    let rec tra env e = xiter (fun e -> aux env e) e
    and aux env e = f tra env e in
    aux
  let traverse_forall_context_down f =
    let rec tra env e = xforall (fun e -> aux env e) e
    and aux env e = f tra env e in
    aux
  let traverse_exists_context_down f =
    let rec tra env e = xexists (fun e -> aux env e) e
    and aux env e = f tra env e in
    aux

  let self_traverse_iter f =
    let rec tra e = xiter aux e
    and aux e = f aux tra e in
    aux
  let self_traverse_map f =
    let rec tra e = xmap aux e
    and aux e = f aux tra e in
    aux
  let self_traverse_fold f =
    let rec tra acc e = xfold aux acc e
    and aux acc e = f aux tra acc e in
    aux
  let self_traverse_foldmap f =
    let rec tra acc e = xfoldmap aux acc e
    and aux acc e = f aux tra acc e in
    aux
  let self_traverse_exists f =
    let rec tra e = xexists aux e
    and aux e = f aux tra e in
    aux
  let self_traverse_forall f =
    let rec tra e = xforall aux e
    and aux e = f aux tra e in
    aux
  let self_traverse_findmap f =
    let rec tra e = xfindmap aux e
    and aux e = f aux tra e in
    aux
  let self_traverse_foldmap_context_down f =
    let rec tra env acc e = xfoldmap (fun acc e -> aux env acc e) acc e
    and aux env acc e = f aux tra env acc e in
    aux
  let self_traverse_map_context_down f =
    let rec tra env e = xmap (fun e -> aux env e) e
    and aux env e = f aux tra env e in
    aux
  let self_traverse_fold_context_down f =
    let rec tra env acc e = xfold (fun acc e -> aux env acc e) acc e
    and aux env acc e = f aux tra env acc e in
    aux
  let self_traverse_iter_context_down f =
    let rec tra env e = xiter (fun e -> aux env e) e
    and aux env e = f aux tra env e in
    aux
  let self_traverse_forall_context_down f =
    let rec tra env e = xforall (fun e -> aux env e) e
    and aux env e = f aux tra env e in
    aux
  let self_traverse_exists_context_down f =
    let rec tra env e = xexists (fun e -> aux env e) e
    and aux env e = f aux tra env e in
    aux

  (* iter *)
  let iter_up f = traverse_iter (fun tra e -> tra e; f e)
  let iter_down f = traverse_iter (fun tra e -> f e; tra e)
  let iter = iter_down

  (* map *)
  let map_up f = traverse_map (fun tra e -> f (tra e))
  let map_down f = traverse_map (fun tra e -> tra (f e))
  let map = map_up (* down can loop *)

  (* fold *)
  let fold_up f = traverse_fold (fun tra acc e -> f (tra acc e) e)
  let fold_down f = traverse_fold (fun tra acc e -> tra (f acc e) e)
  let fold = fold_down

  (* foldmap *)
  let foldmap_up f = traverse_foldmap (fun tra acc e -> let acc, e = tra acc e in f acc e)
  let foldmap_down f = traverse_foldmap (fun tra acc e -> let acc, e = f acc e in tra acc e)
  let foldmap = foldmap_up (* down can loop *)

  (* exists, find, find_map *)
  include MakeFromIter(
    struct
      type 'a t = ('b * 'c * 'd) X.t constraint 'a = 'b * 'c * 'd
      type 'a container = 'a X.t
      let iter_up = iter_up
      let iter_down = iter_down
    end
  )

  (* iterators with contexts *)
  let rec foldmap_context_down f env acc v =
    let env, acc, v = f env acc v in
    xfoldmap (fun acc e -> foldmap_context_down f env acc e) acc v
  let rec fold_context_down f env acc v =
    let env, acc = f env acc v in
    xfold (fun acc e -> fold_context_down f env acc e) acc v
  let rec map_context_down f env v =
    let env, v = f env v in
    xmap (fun e -> map_context_down f env e) v
  let rec iter_context_down f env v =
    let env = f env v in
    xiter (fun v -> iter_context_down f env v) v

  let iter_nonrec = X.iter
  let map_nonrec = X.map
  let fold_nonrec = X.fold
  let foldmap_nonrec = X.foldmap
  let exists_nonrec = xexists
  let forall_nonrec = xforall
end

module MakeLift2 (Y : LIFT2) (X : TRAVERSE with type 'a container = 'a Y.t) =
struct
  type 'a t = 'a X.t
  type 'a container = 'a Y.container

  (* high-order *)
  exception StopExists
  let yexists exists cont =
    try
      Y.iter (fun t -> if exists t then raise StopExists) cont;
      false
    with StopExists -> true

  exception StopForall
  let yforall forall cont =
    try
      Y.iter (fun t -> if not (forall t) then raise StopForall) cont;
      true
    with StopForall -> false

  let yfindmap findmap t =
    let r = ref None in
    try
      Y.iter (fun t ->
                match findmap t with
                | Some a -> r := Some a; raise StopExists
                | None -> ()) t;
      None
    with StopExists -> !r

  let traverse_iter f = Y.iter (X.traverse_iter f)
  let traverse_map f = Y.map (X.traverse_map f)
  let traverse_fold f = Y.fold (X.traverse_fold f)
  let traverse_foldmap f = Y.foldmap (X.traverse_foldmap f)
  let traverse_exists f = yexists (X.traverse_exists f)
  let traverse_forall f = yforall (X.traverse_forall f)
  let traverse_findmap f = yfindmap (X.traverse_findmap f)
  let traverse_iter_context_down f env = Y.iter (let g = X.traverse_iter_context_down f in fun e -> g env e)
  let traverse_fold_context_down f env = Y.fold (let g = X.traverse_fold_context_down f in fun acc e -> g env acc e)
  let traverse_map_context_down f env = Y.map (let g = X.traverse_map_context_down f in fun e -> g env e)
  let traverse_foldmap_context_down f env = Y.foldmap (let g = X.traverse_foldmap_context_down f in fun acc e -> g env acc e)
  let traverse_forall_context_down f env = yforall (let g = X.traverse_forall_context_down f in fun e -> g env e)
  let traverse_exists_context_down f env = yexists (let g = X.traverse_exists_context_down f in fun e -> g env e)

  let self_traverse_iter f = Y.iter (X.self_traverse_iter f)
  let self_traverse_map f = Y.map (X.self_traverse_map f)
  let self_traverse_fold f = Y.fold (X.self_traverse_fold f)
  let self_traverse_foldmap f = Y.foldmap (X.self_traverse_foldmap f)
  let self_traverse_exists f = yexists (X.self_traverse_exists f)
  let self_traverse_forall f = yforall (X.self_traverse_forall f)
  let self_traverse_findmap f = yfindmap (X.self_traverse_findmap f)
  let self_traverse_iter_context_down f env = Y.iter (let g = X.self_traverse_iter_context_down f in fun e -> g env e)
  let self_traverse_fold_context_down f env = Y.fold (let g = X.self_traverse_fold_context_down f in fun acc e -> g env acc e)
  let self_traverse_map_context_down f env = Y.map (let g = X.self_traverse_map_context_down f in fun e -> g env e)
  let self_traverse_foldmap_context_down f env = Y.foldmap (let g = X.self_traverse_foldmap_context_down f in fun acc e -> g env acc e)
  let self_traverse_forall_context_down f env = yforall (let g = X.self_traverse_forall_context_down f in fun e -> g env e)
  let self_traverse_exists_context_down f env = yexists (let g = X.self_traverse_exists_context_down f in fun e -> g env e)

  (* iter *)
  let iter f = Y.iter (X.iter f)
  let iter_up f = Y.iter (X.iter_up f)
  let iter_down f = Y.iter (X.iter_down f)

  (* map *)
  let map f = Y.map (X.map f)
  let map_up f = Y.map (X.map_up f)
  let map_down f = Y.map (X.map_down f)

  (* fold *)
  let fold f = Y.fold (X.fold f)
  let fold_up f = Y.fold (X.fold_up f)
  let fold_down f = Y.fold (X.fold_down f)

  (* foldmap *)
  let foldmap f = Y.foldmap (X.foldmap f)
  let foldmap_up f = Y.foldmap (X.foldmap_up f)
  let foldmap_down f = Y.foldmap (X.foldmap_down f)

  (* traversal with context *)
  let iter_context_down f env = Y.iter (fun e -> X.iter_context_down f env e)
  let fold_context_down f env = Y.fold (fun acc e -> X.fold_context_down f env acc e)
  let map_context_down f env = Y.map (fun e -> X.map_context_down f env e)
  let foldmap_context_down f env = Y.foldmap (fun acc e -> X.foldmap_context_down f env acc e)

  include MakeFromIter(
    struct
      type 'a t = 'a X.t
      type 'a container = 'a Y.container
      let iter_up = iter_up
      let iter_down = iter_down
    end
  )

  (* non rec *)
  let iter_nonrec f = Y.iter (fun e -> X.iter_nonrec f e)
  let map_nonrec f = Y.map (fun e -> X.map_nonrec f e)
  let fold_nonrec f = Y.fold (fun acc e -> X.fold_nonrec f acc e)
  let foldmap_nonrec f = Y.foldmap (fun acc e -> X.foldmap_nonrec f acc e)
  let exists_nonrec f = yexists (fun e -> X.exists_nonrec f e)
  let forall_nonrec f = yforall (fun e -> X.forall_nonrec f e)
end

module MakeLift1 (Y : LIFT2) (X :  TRAVERSE with type 'a container = 'a Y.t and type 'a t = 'a Y.t)  =
struct
  include MakeLift2(Y)(X)
  let iter_nonrec = Y.iter
  let map_nonrec = Y.map
  let fold_nonrec = Y.fold
  let foldmap_nonrec = Y.foldmap
  let exists_nonrec = yexists
  let forall_nonrec = yforall
end

(* mutual recursive types *)

module type IteratedType12 =
  sig
    type 'p t1 constraint 'p = _ * _ * _
    type 'p t2 constraint 'p = _ * _ * _
    val iter_up : ('p t1 -> unit) -> ('p t2 -> unit) -> 'p t1 -> unit
    val iter_down : ('p t1 -> unit) -> ('p t2 -> unit) -> 'p t1 -> unit
  end
module MakeFromIter12 (X : IteratedType12) =
struct
  let find_up predA predB tree =
    Return.set_checkpoint_opt
      (fun label ->
         X.iter_up
           (fun e -> if predA e then Return.return label (Base.Left e))
           (fun e -> if predB e then Return.return label (Base.Right e))
           tree)
  let find_down predA predB tree =
    Return.set_checkpoint_opt
      (fun label ->
         X.iter_down
           (fun e -> if predA e then Return.return label (Base.Left e))
           (fun e -> if predB e then Return.return label (Base.Right e))
           tree)
  let find = find_up

  let findmap_up predA predB tree =
    Return.set_checkpoint_opt
      (fun label ->
         X.iter_up
           (fun e -> match predA e with Some e -> Return.return label e | None -> ())
           (fun e -> match predB e with Some e -> Return.return label e | None -> ())
           tree)
  let findmap_down predA predB tree =
    Return.set_checkpoint_opt
      (fun label ->
         X.iter_down
           (fun e -> match predA e with Some e -> Return.return label e | None -> ())
           (fun e -> match predB e with Some e -> Return.return label e | None -> ())
           tree)
  let findmap = findmap_up

  let exists_up fa fb tree = find_up fa fb tree <> None
  let exists_down fa fb tree = find_down fa fb tree <> None
  let exists = exists_up
end

module type X_12 =
sig
  include AB
  val existsA : ('p tA -> bool) -> ('p tB -> bool) -> 'p tA -> bool
  val existsB : ('p tB -> bool) -> ('p tA -> bool) -> 'p tB -> bool
  val findmapA : ('p tA -> 'a option) -> ('p tB -> 'a option) -> 'p tA -> 'a option
  val findmapB : ('p tB -> 'a option) -> ('p tA -> 'a option) -> 'p tB -> 'a option
  val forallA : ('p tA -> bool) -> ('p tB -> bool) -> 'p tA -> bool
  val forallB : ('p tB -> bool) -> ('p tA -> bool) -> 'p tB -> bool
end


module Make12 (AB : X_12) =
struct
    type 'p t1 = 'p AB.tA
    type 'p t2 = 'p AB.tB

    let traverse_iter fA fB =
      let rec traA e = AB.iterA auxA auxB e
      and traB e = AB.iterB auxB auxA e
      and auxA e = fA traA traB e
      and auxB e = fB traB traA e in
      auxA

    let traverse_map fA fB =
      let rec traA e = AB.mapA auxA auxB e
      and traB e = AB.mapB auxB auxA e
      and auxA e = fA traA traB e
      and auxB e = fB traB traA e in
      auxA

    let traverse_fold fA fB =
      let rec traA acc e = AB.foldA auxA auxB acc e
      and traB acc e = AB.foldB auxB auxA acc e
      and auxA acc e = fA traA traB acc e
      and auxB acc e = fB traB traA acc e in
      auxA

    let traverse_foldmap fA fB =
      let rec traA acc e = AB.foldmapA auxA auxB acc e
      and traB acc e = AB.foldmapB auxB auxA acc e
      and auxA acc e = fA traA traB acc e
      and auxB acc e = fB traB traA acc e in
      auxA

    let traverse_exists fA fB =
      let rec traA e = AB.existsA auxA auxB e
      and traB e = AB.existsB auxB auxA e
      and auxA e = fA traA traB e
      and auxB e = fB traB traA e in
      auxA

    let traverse_forall fA fB =
      let rec traA e = AB.forallA auxA auxB e
      and traB e = AB.forallB auxB auxA e
      and auxA e = fA traA traB e
      and auxB e = fB traB traA e in
      auxA

    let traverse_findmap fA fB =
      let rec traA e = AB.findmapA auxA auxB e
      and traB e = AB.findmapB auxB auxA e
      and auxA e = fA traA traB e
      and auxB e = fB traB traA e in
      auxA

    let traverse_foldmap_context_down fA fB =
      let rec traA env acc a = AB.foldmapA (fun acc a -> auxA env acc a) (fun acc b -> auxB env acc b) acc a
      and traB env acc e = AB.foldmapB (fun acc b -> auxB env acc b) (fun acc a -> auxA env acc a) acc e
      and auxA env acc e = fA traA traB env acc e
      and auxB env acc e = fB traB traA env acc e in
      auxA
    let traverse_map_context_down fA fB =
      let rec traA env e = AB.mapA (fun a -> auxA env a) (fun b -> auxB env b) e
      and traB env e = AB.mapB (fun b -> auxB env b) (fun a -> auxA env a) e
      and auxA env e = fA traA traB env e
      and auxB env e = fB traB traA env e in
      auxA
    let traverse_fold_context_down fA fB =
      let rec traA env acc e = AB.foldA (fun acc a -> auxA env acc a) (fun acc b -> auxB env acc b) acc e
      and traB env acc e = AB.foldB (fun acc b -> auxB env acc b) (fun acc a -> auxA env acc a) acc e
      and auxA env acc e = fA traA traB env acc e
      and auxB env acc e = fB traB traA env acc e in
      auxA
    let traverse_iter_context_down fA fB =
      let rec traA env e = AB.iterA (fun a -> auxA env a) (fun b -> auxB env b) e
      and traB env e = AB.iterB (fun b -> auxB env b) (fun a -> auxA env a) e
      and auxA env e = fA traA traB env e
      and auxB env e = fB traB traA env e in
      auxA
    let traverse_forall_context_down fA fB =
      let rec traA env a = AB.forallA (fun a -> auxA env a) (fun b -> auxB env b) a
      and traB env b = AB.forallB (fun b -> auxB env b) (fun a -> auxA env a) b
      and auxA env a = fA traA traB env a
      and auxB env b = fB traB traA env b in
      auxA
    let traverse_exists_context_down fA fB =
      let rec traA env a = AB.existsA (fun a -> auxA env a) (fun b -> auxB env b) a
      and traB env b = AB.existsB (fun b -> auxB env b) (fun a -> auxA env a) b
      and auxA env a = fA traA traB env a
      and auxB env b = fB traB traA env b in
      auxA

    let self_traverse_iter fA fB =
      let rec traA e = AB.iterA auxA auxB e
      and traB e = AB.iterB auxB auxA e
      and auxA e = fA auxA traA auxB traB e
      and auxB e = fB auxB traB auxA traA e in
      auxA
    let self_traverse_map fA fB =
      let rec traA e = AB.mapA auxA auxB e
      and traB e = AB.mapB auxB auxA e
      and auxA e = fA auxA traA auxB traB e
      and auxB e = fB auxB traB auxA traA e in
      auxA
    let self_traverse_fold fA fB =
      let rec traA acc e = AB.foldA auxA auxB acc e
      and traB acc e = AB.foldB auxB auxA acc e
      and auxA acc e = fA auxA traA auxB traB acc e
      and auxB acc e = fB auxB traB auxA traA acc e in
      auxA
    let self_traverse_foldmap fA fB =
      let rec traA acc e = AB.foldmapA auxA auxB acc e
      and traB acc e = AB.foldmapB auxB auxA acc e
      and auxA acc e = fA auxA traA auxB traB acc e
      and auxB acc e = fB auxB traB auxA traA acc e in
      auxA
    let self_traverse_exists fA fB =
      let rec traA e = AB.existsA auxA auxB e
      and traB e = AB.existsB auxB auxA e
      and auxA e = fA auxA traA auxB traB e
      and auxB e = fB auxB traB auxA traA e in
      auxA
    let self_traverse_forall fA fB =
      let rec traA e = AB.forallA auxA auxB e
      and traB e = AB.forallB auxB auxA e
      and auxA e = fA auxA traA auxB traB e
      and auxB e = fB auxB traB auxA traA e in
      auxA
    let self_traverse_findmap fA fB =
      let rec traA e = AB.findmapA auxA auxB e
      and traB e = AB.findmapB auxB auxA e
      and auxA e = fA auxA traA auxB traB e
      and auxB e = fB auxB traB auxA traA e in
      auxA

    let self_traverse_foldmap_context_down fA fB =
      let rec traA env acc a = AB.foldmapA (fun acc a -> auxA env acc a) (fun acc b -> auxB env acc b) acc a
      and traB env acc e = AB.foldmapB (fun acc b -> auxB env acc b) (fun acc a -> auxA env acc a) acc e
      and auxA env acc e = fA auxA traA auxB traB env acc e
      and auxB env acc e = fB auxB traB auxA traA env acc e in
      auxA
    let self_traverse_map_context_down fA fB =
      let rec traA env e = AB.mapA (fun a -> auxA env a) (fun b -> auxB env b) e
      and traB env e = AB.mapB (fun b -> auxB env b) (fun a -> auxA env a) e
      and auxA env e = fA auxA traA auxB traB env e
      and auxB env e = fB auxB traB auxA traA env e in
      auxA
    let self_traverse_fold_context_down fA fB =
      let rec traA env acc e = AB.foldA (fun acc a -> auxA env acc a) (fun acc b -> auxB env acc b) acc e
      and traB env acc e = AB.foldB (fun acc b -> auxB env acc b) (fun acc a -> auxA env acc a) acc e
      and auxA env acc e = fA auxA traA auxB traB env acc e
      and auxB env acc e = fB auxB traB auxA traA env acc e in
      auxA
    let self_traverse_iter_context_down fA fB =
      let rec traA env e = AB.iterA (fun a -> auxA env a) (fun b -> auxB env b) e
      and traB env e = AB.iterB (fun b -> auxB env b) (fun a -> auxA env a) e
      and auxA env e = fA auxA traA auxB traB env e
      and auxB env e = fB auxB traB auxA traA env e in
      auxA
    let self_traverse_forall_context_down fA fB =
      let rec traA env a = AB.forallA (fun a -> auxA env a) (fun b -> auxB env b) a
      and traB env b = AB.forallB (fun b -> auxB env b) (fun a -> auxA env a) b
      and auxA env a = fA auxA traA auxB traB env a
      and auxB env b = fB auxB traB auxA traA env b in
      auxA
    let self_traverse_exists_context_down fA fB =
      let rec traA env a = AB.existsA (fun a -> auxA env a) (fun b -> auxB env b) a
      and traB env b = AB.existsB (fun b -> auxB env b) (fun a -> auxA env a) b
      and auxA env a = fA auxA traA auxB traB env a
      and auxB env b = fB auxB traB auxA traA env b in
      auxA

    let iter_up fA fB = traverse_iter (fun traA _ e -> traA e; fA e) (fun traB _ e -> traB e; fB e)
    let iter_down fA fB = traverse_iter (fun traA _ e -> fA e; traA e) (fun traB _ e -> fB e; traB e)
    let iter = iter_down

    let map_up fA fB = traverse_map (fun traA _ e -> fA (traA e)) (fun traB _ e -> fB (traB e))
    let map_down fA fB = traverse_map (fun traA _ e -> traA (fA e)) (fun traB _ e -> traB (fB e))
    let map = map_up (* down can loop *)

    let fold_up fA fB = traverse_fold (fun traA _ acc e -> fA (traA acc e) e) (fun traB _ acc e -> fB (traB acc e) e)
    let fold_down fA fB = traverse_fold (fun traA _ acc e -> traA (fA acc e) e) (fun traB _ acc e -> traB (fB acc e) e)
    let fold = fold_down

    let foldmap_up fA fB =
      traverse_foldmap
        (fun traA _ acc e -> let acc, e = traA acc e in fA acc e)
        (fun traB _ acc e -> let acc, e = traB acc e in fB acc e)
    let foldmap_down fA fB =
      traverse_foldmap
        (fun traA _ acc e -> let acc, e = fA acc e in traA acc e)
        (fun traB _ acc e -> let acc, e = fB acc e in traB acc e)
    let foldmap = foldmap_up (* down can loop *)

    include MakeFromIter12(
      struct
        type 'p t1 = 'p AB.tA
        type 'p t2 = 'p AB.tB
        let iter_up = iter_up
        let iter_down = iter_down
      end
    )

    let foldmap_context_down fA fB =
      traverse_foldmap_context_down
        (fun traA _ env acc a -> let env, acc, a = fA env acc a in traA env acc a)
        (fun traB _ env acc b -> let env, acc, b = fB env acc b in traB env acc b)
    let fold_context_down fA fB =
      traverse_fold_context_down
        (fun traA _ env acc a -> let env, acc = fA env acc a in traA env acc a)
        (fun traB _ env acc b -> let env, acc = fB env acc b in traB env acc b)
    let map_context_down fA fB =
      traverse_map_context_down
        (fun traA _ env a -> let env, a = fA env a in traA env a)
        (fun traB _ env b -> let env, b = fB env b in traB env b)
    let iter_context_down fA fB =
      traverse_iter_context_down
        (fun traA _ env a -> let env = fA env a in traA env a)
        (fun traB _ env b -> let env = fB env b in traB env b)

    let iter_nonrec = AB.iterA
    let map_nonrec = AB.mapA
    let fold_nonrec = AB.foldA
    let foldmap_nonrec = AB.foldmapA
    let exists_nonrec = AB.existsA
    let forall_nonrec = AB.forallA
end

module MakeAB (AB : AB) =
struct
  type 'p tA = 'p AB.tA
  type 'p tB = 'p AB.tB

  exception StopExists
  let existsGEN iterA existsA existsB tA =
    try
      iterA
        (fun tA -> if existsA tA then raise StopExists)
        (fun tB -> if existsB tB then raise StopExists)
        tA;
      false
    with StopExists -> true

  let existsA a = existsGEN AB.iterA a
  let existsB a = existsGEN AB.iterB a

  let findmapGEN iterA findmapA findmapB tA =
    let r = ref None in
    try
      iterA
        (fun tA -> match findmapA tA with
         | Some a -> r := Some a; raise StopExists
         | None -> ())
        (fun tB -> match findmapB tB with
         | Some a -> r := Some a; raise StopExists
         | None -> ())
        tA
      ;
      None
    with StopExists -> !r

  let findmapA a = findmapGEN AB.iterA a
  let findmapB a = findmapGEN AB.iterB a

  exception StopForall
  let forallGEN iterA forallA forallB tA =
    try
      iterA
        (fun tA -> if not (forallA tA) then raise StopForall)
        (fun tB -> if not (forallB tB) then raise StopForall)
        tA;
      true
    with StopForall -> false

  let forallA a = forallGEN AB.iterA a
  let forallB a = forallGEN AB.iterB a

  module A = Make12 (
    struct
      include AB
      let existsA = existsA
      let existsB = existsB
      let findmapA = findmapA
      let findmapB = findmapB
      let forallA = forallA
      let forallB = forallB
    end
  )

  module B = Make12 (
    struct
      type 'p tA = 'p AB.tB
      type 'p tB = 'p AB.tA
      let foldmapA = AB.foldmapB
      let foldmapB = AB.foldmapA
      let iterA = AB.iterB
      let iterB = AB.iterA
      let mapA = AB.mapB
      let mapB = AB.mapA
      let foldA = AB.foldB
      let foldB = AB.foldA
      let existsA, existsB = existsB, existsA
      let findmapA, findmapB = findmapB, findmapA
      let forallA, forallB = forallB, forallA
    end
  )

  module AinA = Make2 (
    struct
      type 'p t = 'p tA

      let foldmap traA acc e =
        let rec traB acc e = AB.foldmapB traB traA acc e in
        AB.foldmapA traA traB acc e

      let iter traA e =
        let rec traB e = AB.iterB traB traA e in
        AB.iterA traA traB e

      let map traA e =
        let rec traB e = AB.mapB traB traA e in
        AB.mapA traA traB e

      let fold traA acc e =
        let rec traB acc e = AB.foldB traB traA acc e in
        AB.foldA traA traB acc e
    end
  )

  module BinB = Make2 (
    struct
      type 'p t = 'p tB

      let foldmap traB acc e =
        let rec traA acc e = AB.foldmapA traA traB acc e in
        AB.foldmapB traB traA acc e

      let iter traB e =
        let rec traA e = AB.iterA traA traB e in
        AB.iterB traB traA e

      let map traB e =
        let rec traA e = AB.mapA traA traB e in
        AB.mapB traB traA e

      let fold traB acc e =
        let rec traA acc e = AB.foldA traA traB acc e in
        AB.foldB traB traA acc e
    end
  )

  module OnlyA = Make2 (
    struct
      type 'p t = 'p tA

      let foldmap traA acc e =
        let traB acc e = acc, e in
        AB.foldmapA traA traB acc e

      let iter traA e =
        let traB _ = () in
        AB.iterA traA traB e

      let map traA e =
        let traB e = e in
        AB.mapA traA traB e

      let fold traA acc e =
        let traB acc _ = acc in
        AB.foldA traA traB acc e
    end
  )

  module OnlyB = Make2 (
    struct
      type 'p t = 'p tB

      let foldmap traB acc e =
        let traA acc e = acc, e in
        AB.foldmapB traB traA acc e

      let iter traB e =
        let traA _ = () in
        AB.iterB traB traA e

      let map traB e =
        let traA e = e in
        AB.mapB traB traA e

      let fold traB acc e =
        let traA acc _ = acc in
        AB.foldB traB traA acc e
    end
  )

  module AinB = MakeLift1 (
    struct
      type 'p t = 'p tA
      type 'p container = 'p tB

      (* : ('acc -> 'p tA -> 'acc * 'p tA) -> 'acc -> 'p tB -> 'acc * 'p tB *)
      let foldmap traA acc e =
        let rec traB acc e = AB.foldmapB traB traA acc e in
        AB.foldmapB traB traA acc e

      (* : ('p tA -> unit) -> 'p tB -> unit *)
      let iter traA e =
        let rec traB e = AB.iterB traB traA e in
        AB.iterB traB traA e

      (* : ('p tA -> 'p tA) -> 'p tB -> 'p tB *)
      let map traA e =
        let rec traB e = AB.mapB traB traA e in
        AB.mapB traB traA e

      (* : ('acc -> 'p tA -> 'acc) -> 'acc -> 'p tB -> 'acc *)
      let fold traA acc e =
        let rec traB acc e = AB.foldB traB traA acc e in
        AB.foldB traB traA acc e
    end
  ) (AinA)

  module BinA = MakeLift1 (
    struct
      type 'p t = 'p tB
      type 'p container = 'p tA

      (* : ('acc -> 'p tB -> 'acc * 'p tB) -> 'acc -> 'p tA -> 'acc * 'p tA *)
      let foldmap traB acc e =
        let rec traA acc e = AB.foldmapA traA traB acc e in
        AB.foldmapA traA traB acc e

      (* : ('p tB -> unit) -> 'p tA -> unit *)
      let iter traB e =
        let rec traA e = AB.iterA traA traB e in
        AB.iterA traA traB e

      (* : ('p tB -> 'p tB) -> 'p tA -> 'p tA *)
      let map traB e =
        let rec traA e = AB.mapA traA traB e in
        AB.mapA traA traB e

      (* : ('acc -> 'p tB -> 'acc) -> 'acc -> 'p tA -> 'acc *)
      let fold traB acc e =
        let rec traA acc e = AB.foldA traA traB acc e in
        AB.foldA traA traB acc e
    end
  ) (BinB)

end
