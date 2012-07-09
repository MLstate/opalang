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
(* CF mli *)

include List
exception Empty

let sprintf = Printf.sprintf
let (|>) x f = f x
let (@*) f g x = f (g x)

let get_only_element = function
  | [x] -> x
  | _ -> invalid_arg "List.get_only_element"

let map_right f l = rev_map f (rev l)

let tail_append l1 l2 = rev_append (rev l1) l2

let rev_map_append fct l1 l2 =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl ->
        let hd = fct hd in
        aux (hd :: acc) tl
  in
  aux l2 l1

let rev_filter_map_append fct l1 l2 =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> (
        match fct hd with
        | None -> aux acc tl
        | Some hd -> aux (hd :: acc) tl
      )
  in aux l2 l1

let empty = []

let is_empty = function
  | [] -> true
  | _  -> false

let rec mem_eq ~eq x = function
  | [] -> false
  | a :: l -> eq a x || mem_eq ~eq x l

let substract l1 l2 =
  List.rev (fold_left (fun l x -> if mem x l2 then l else x::l) [] l1)
let subtract = substract (* backwards-typo compatibility *)

let substract_eq ~eq l1 l2 =
  List.rev (fold_left (fun l x -> if mem_eq ~eq x l2 then l else x :: l) [] l1)

(* subset l1 l2 tests whether the list l1 is a subset of the list l2 *)
let subset l1 l2 =
  List.for_all (fun e -> List.mem e l2) l1

let subset_eq ~eq l1 l2 =
  List.for_all (fun e -> List.exists (eq e) l2) l1

let iter_right f l =
  let rec aux = function
    | [] -> ()
    | hd::tl -> f hd ; aux tl
  in aux l
let iteri f l =
  ignore (
    List.fold_left (fun acc x -> let () = f x acc in succ acc) 0 l
  )
let rev_mapi f l =
  fst (List.fold_left (fun (acc, i) x -> f i x :: acc, i + 1) ([], 0) l)
let mapi f l =
  List.rev (rev_mapi f l)

let for_alli f l =
  let rec aux i = function
    | [] -> true
    | h :: t -> f i h && aux (i+1) t
  in
  aux 0 l

let map_with_tail f = List.fold_right (fun x y -> (f x)::y)
let init n f =
  let rec aux r i =
    if i >= 0 then aux ((f i) :: r) (pred i)
    else r
  in aux [] (pred n)

let side_effect_init n f =
  let rec aux acc i =
    if i >= n then List.rev acc
    else
      aux ((f i) :: acc) (succ i)
  in
  aux [] 0

let rec last = function
  | [] -> failwith "List.last"
  | [e] -> e
  | _e::l -> last l

let rec take n l =
  assert (n >= 0);
  if n = 0 then [] else
    match l with
    | [] -> []
    | e::l -> e :: take (n-1) l

let rec take_last k l =
  assert(k>=0);
  let n=List.length l in
  let rec aux i l =  if i < n-k then aux (i+1) (List.tl l) else l in
  aux 0 l

let rec drop n l =
  assert (n >= 0);
  if n = 0 then l else
    match l with
    | [] -> l
    | _e::l -> drop (n-1) l

let rec extract_last = function
  | [] -> failwith "List.extract_last"
  | [e] -> [], e
  | x::xs ->
      let ys, y = extract_last xs in
      x::ys, y

(* example: split_at 2 ["a";"b";"c"] gives ["a";"b"],["c"] *)
let split_at n l =
  let rec aux accu n l =
    match (n, l) with
    | 0, _
    | _, [] -> List.rev accu, l
    | _, e::l -> aux (e::accu) (n-1) l
  in
  assert (n >= 0);
  aux [] n l

(* example : split_ats [1;2;3] ["a";"b";"c";"d";"e";"f"] gives [["a"]; ["b";"c"]; ["d";"e";"f"]]*)
let split_ats lengths l =
  let rec aux lengths l acc =
    match lengths with
    | [] ->
        if l = [] then
          List.rev acc
        else
          raise (Invalid_argument "List.split_ats")
    | n :: lengths_t ->
        let l1, r = split_at n l in
        aux lengths_t r (l1 :: acc)
  in
  aux lengths l []

let split_at_sep f l =
  let rec aux f l acc0 acc1 =
    match l with
    | x :: rl -> if f x then (List.rev acc0)::acc1 else aux f rl (x::acc0) acc1
    | _ -> List.rev acc1
  in aux f l [] []

(* example:
   splice 1 1 [] ["a";"b";"c"] gives ["a";"c"]
   splice 1 1 ["B"] ["a";"b";"c"] gives ["a";"B";"c"]
*)
let splice index nb_to_remove elts_to_add l =
  let beg_to_keep, l = split_at index l in
  let end_to_keep = drop nb_to_remove l in
  beg_to_keep @ elts_to_add @ end_to_keep

let fold_left_i f init l =
  snd (fold_left (fun (i, acc) x -> (succ i, f acc x i)) (0, init) l)

let fold_right_i f l init =
  let len = ref 0 in
  let l = rev_map (fun e -> incr(len) ; e) l in
  snd (fold_left (fun (i, acc) x -> (pred i, f x i acc)) (pred (!len), init) l)

let fold f = function
  | hd :: tl -> fold_left f hd tl
  | _ -> raise Empty

let collect f l =
  let rec collect_accu f accu = function
    | [] -> accu
    | e::l -> collect_accu f (List.rev_append (f e) accu) l
  in
  List.rev (collect_accu f [] l)

let rev_filter f tl =
  let rec aux accu = function
    | [] -> accu
    | hd::tl ->
        if f hd
        then aux (hd::accu) tl
        else aux accu tl
  in
  aux [] tl

let tail_filter f tl = List.rev (rev_filter f tl)

let to_string f l =
  let rec aux = function
    | hd::tl -> sprintf "%s%s" (f hd) (aux tl)
    | _ -> "" in
  aux l
let print f l =
  let rec aux = function
    | [] -> "]"
    | [hd] -> sprintf "%s]" (f hd)
    | hd::tl -> sprintf "%s;%s" (f hd) (aux tl) in
  sprintf "[%s" (aux l)
let max, min =
  let oper op = function
    | [] -> raise Empty
    | hd::tl -> fold_left op hd tl
  in
  (fun l -> oper max l),
  (fun l -> oper min l)

let minmax =
  let minmax (mi, ma) x = if x < mi then x, ma else if x > ma then mi, x else mi, ma in
  function
  | [] -> raise Empty
  | hd::tl -> fold_left minmax (hd, hd) tl

let argmax, argmin =
  let argoper op = function
    | [] -> raise Empty
    | hd::tl -> fold_left (fun acc x -> if op x acc then x else acc) hd tl
  in
  (fun l -> argoper (>) l),
  (fun l -> argoper (<) l)

let remove_all v = filter (fun x -> x<>v)
let remove_first v =
  let rec aux stack = function
    | [] -> stack
    | hd :: tl when hd = v -> List.rev stack @ tl
    | hd :: tl -> aux (hd :: stack) tl
  in aux []



(* ************************************************************************** *)
(** {b Descr}: See .mli file for documentation.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let remove_first_or_fail_eq ~eq v =
  let rec aux stack = function
    | [] -> raise Not_found
    | hd :: tl when eq hd v -> List.rev stack @ tl
    | hd :: tl -> aux (hd :: stack) tl
  in aux []



let remove_last l =
  fst (List.fold_right (
         fun e (acc, last) ->
           if last then
             acc, false
           else
             e :: acc, last
       ) l ([], true))

let replace v rl =
  let rec aux acc = function
    | [] -> rev acc
    | hd :: tl when hd = v -> aux ((List.rev rl) @ acc) tl
    | hd :: tl -> aux (hd :: acc) tl
  in aux []

let cons e l = e::l

let uniq ?(cmp = Pervasives.compare) = function
  | hd :: tl ->
      let l, _ =
        List.fold_left (
          fun ((l, e) as acc) x ->
            if 0 = cmp x e then acc
            else (x :: l, x)
        ) ([hd], hd) tl
      in
      List.rev l
  | [] -> []

let uniq_unsorted ?(cmp = Pervasives.compare) ?(conflict=(fun _ _ -> ())) l =
  let l = fold_left (fun acc e ->
                       try
                         let e' = find (fun x -> cmp e x = 0) acc in
                         conflict e e';
                         acc
                       with Not_found -> e :: acc) [] l
  in
  rev l

let rec insert p e l =  match p, l with
  | 0, _ -> e :: l
  | _n, [] -> raise Empty
  | _n, (t::q) ->  t :: (insert (pred p) e q)

let insert_sorted ?(cmp=Pervasives.compare) ?(conflict=(fun x y -> [x;y])) x l =
  let rec aux = function
    | [] -> [x]
    | (t::q) as l -> let c = cmp x t in
      if c < 0 then x::l
      else if c > 0 then t::(aux q)
      else (conflict x t)@q
  in aux l


let filter_and_fold f =
  let rec aux accu = function
    | [] -> accu, []
    | e::l ->
        let accu, b = f accu e in
        let accu, l = aux accu l in
        accu, (if b then e::l else l)
  in aux

let filteri f =
  let rec aux acc pos = function
    | [] -> List.rev(acc)
    | x::y -> aux (if f pos x then x::acc else acc) (pos+1) y
  in
  aux [] 0

let flip l =
  let rec aux accu l =
    match accu, l with
    | accu, [] -> accu
    | [], e::l -> [e]::(aux [] l)
    | a::accu, e::l -> (e::a)::(aux accu l)
  in
  List.fold_left aux [] l

let combine_opt l1 l2 =
  try Some (combine l1 l2) with Invalid_argument _ -> None

let assoc_opt key =
  let rec aux = function
    | [] -> None
    | (k, v)::_ when key = k -> Some v
    | _::q -> aux q
  in aux

let assq_opt key =
  let rec aux = function
    | [] -> None
    | (k, v)::_ when key == k -> Some v
    | _::q -> aux q
  in aux

let find_opt f l =
  try
    Some (find f l)
  with
  | Not_found -> None

let find_map f l =
  let rec aux = function
    | [] -> None
    | hd::tl -> (
        match f hd with
        | ( Some _ ) as some -> some
        | None -> aux tl
      )
  in aux l

let findi f =
  let rec aux i = function
    | [] -> None
    | x::_ when f x -> Some i
    | _::l -> aux (succ i) l
  in
  fun l -> aux 0 l

let find_i f =
  let rec aux i = function
    | [] -> None
    | x::_ when f x -> Some (i, x)
    | _::l -> aux (succ i) l
  in
  fun l -> aux 0 l

let find_map f tl =
  let rec aux = function
    | [] -> None
    | hd :: tl -> (
        match f hd with
        | None ->
            aux tl
        | ( Some _ ) as some -> some
      )
  in
  aux tl

(** memi e l returns the index (position) of the element e *)
let memi e l = findi (fun x -> x = e) l
let pos_opt = memi

let filter_map f l =
  List.fold_right (fun v acc -> match f v with None -> acc | Some v -> v::acc) l []

let filter_mapi f l =
  mapi (fun i it -> (i, it) ) l
  |> filter_map (fun (i, it) -> f i it)


let partition_map f l =
  let cons_opt o l = Option.default_map l (fun x -> x::l) o in
  List.fold_right (fun v (acc1, acc2) ->
                     let (o1, o2) = f v in
                     (cons_opt o1 acc1, cons_opt o2 acc2)) l ([], [])

let get_first_some list arg =
  let rec aux = function
    | [] -> None
    | t::q ->
        begin
          match t arg with
          | None -> aux q
          | some -> some
        end
  in aux list

let get_first_some_ar2 list arg1 arg2 =
  let rec aux = function
    | [] -> None
    | t::q ->
        begin
          match t arg1 arg2 with
          | None -> aux q
          | some -> some
        end
  in aux list

(* maping with accu : no tail rec or rev *)
let fold_right_map fct list accu =
  let rec aux = function
    | [] -> [], accu
    | t::q ->
        let tl, accu = aux q in
        let hd, accu = fct t accu in
        hd::tl, accu
  in aux list


(* see the mli for comments on foldl[1] foldr[1] *)
let rec foldl f l a = match l with
  | hd :: tl -> foldl f tl (f hd a)
  | _ -> a

let foldl1 f = function
  | [] -> invalid_arg "List.foldl1: empty list"
  | e::l -> foldl f l e


let fold_left1 f = function
  | [] -> invalid_arg "List.fold_left1: empty list"
  | e::l -> List.fold_left f e l

let foldr = List.fold_right

let foldr1 f =
  let rec aux = function
    | [] -> invalid_arg "List.foldr1: empty list"
    | [x] -> x
    | t::q -> f t (aux q)
  in aux

let rec fold_left_snd f acc = function
  | [] -> acc
  | (_,x) :: t -> fold_left_snd f (f acc x) t

let map_stable map list =
  let equal = ref true in
  let fct acc elt =
    let felt = map elt in
    if elt != felt then equal := false;
    felt::acc in
  let flist = List.fold_left fct [] list in
  if !equal then list else List.rev flist

let filter_stable filter list =
  let equal = ref true in
  let f acc elt =
    if filter elt then elt :: acc else (equal := false ; acc)
  in
  let acc = List.fold_left f [] list in
  if !equal then list else List.rev acc

(**
   {[('acc -> 'input -> ('acc * 'output))  ->  'acc  ->  'output list  ->  'input list  -> ('acc *  'output list)]}
   @param f takes the accumulator and the head of the list to give back the new element
   of the output list and the new accumulator
   @param end_ is the end of the output list
   the result of the mapping is reversed
*)
let fold_left_rev_map_end f acc end_ list =
  let rec aux acc list = function
    | [] -> acc, list
    | h :: t ->
        let acc, h = f acc h in
        aux acc (h :: list) t in
  aux acc end_ list

let fold_left_rev_map f acc list = fold_left_rev_map_end f acc [] list

(**
   {[('acc -> 'input -> ('acc * 'output))  ->  'acc  ->  'output list  ->  'input list  -> ('acc *  'output list)]}
   @param f takes the accumulator and the head of the list to give back the new element
   of the output list and the new accumulator
   @param rev_beginning is the beginning of the output list in the reverse order
*)
let fold_left_map_init f acc rev_beginning list =
  let acc, l = fold_left_rev_map_end f acc rev_beginning list in
  acc, List.rev l

let fold_left_map f acc list = fold_left_map_init f acc [] list

let fold_left_map_stable f acc orig_list =
  let rec aux acc list = function
    | [] -> acc, orig_list (* if we come here, then all the images were physically
                            * to the original element, so we give back the input list *)
    | h :: t ->
        let acc, h' = f acc h in
        if h == h' then
          aux acc (h' :: list) t
        else
          (* when one equality fails, switching to the usual fold_left_map
           * because there is no point in doing the other comparisons anymore *)
          fold_left_map_init f acc (h' :: list) t in
  aux acc [] orig_list

let fold_right_map_stable f acc orig_list =
  let rec aux acc list = function
    | [] -> acc, orig_list
    | h :: t ->
        let acc, h' = f acc h in
        if h == h' then
          aux acc (h' :: list) t
        else
          fold_left_rev_map_end f acc (h' :: list) t in
  aux acc [] (List.rev orig_list)

let fold_left_filter_map fct accu list =
  let fct (accu, list) elt =
    let accu, elt = fct accu elt in
    accu, (match elt with None -> list | Some elt -> elt::list) in
  let accu, list = List.fold_left fct (accu,[]) list in
  accu, List.rev list

(** raises Invalid_argument "fold_left2" *)
let fold_left_map2 fct accu list1 list2 =
  let fct (accu, list) elt1 elt2 =
    let accu, elt = fct accu elt1 elt2 in
    accu, elt::list in
  let accu, list = List.fold_left2 fct (accu,[]) list1 list2 in
  accu, List.rev list

let fold_left_collect fct accu list =
  let fct (accu, list) elt =
    let accu, elt_list = fct accu elt in
    accu, List.rev_append elt_list list in
  let accu, list = List.fold_left fct (accu, []) list in
  accu, List.rev list

let fold_left_map_i f init l =
  let (_, acc), l = fold_left_map
    (fun (i, acc) x ->
       let acc, x = f i acc x
       in ((succ i, acc), x)) (0, init) l
  in acc, l

let for_all2_same_length f l1 l2 =
  List.length l1 = List.length l2 && for_all2 f l1 l2

(* tail rec *)
let rev_concat_map fct =
  let rec aux accu = function
    | [] -> accu
    | e::l -> aux (List.rev_append (fct e) accu) l
  in aux []

let concat_map f = rev @* rev_concat_map f

let rev_concat_map2 fct l1 l2 =
  let rec aux acc l1 l2 =
    match l1, l2 with
    | [], [] -> acc
    | e1 :: l1, e2 :: l2 -> aux (List.rev_append (fct e1 e2) acc) l1 l2
    | _ -> invalid_arg "List.rev_concat_map2" in
  aux [] l1 l2
let concat_map2 f l1 l2 = rev (rev_concat_map2 f l1 l2)

let tail_concat l = (* dont factor l : '_a *)
  let rec aux accu = function
    | [] -> List.rev accu
    | t::q -> aux (List.rev_append t accu) q
  in aux [] l

let tail_append_keep_length a b =
  let rec aux acc i = function
    | [] -> i, acc
    | t::q -> aux (t::acc) (succ i) q in
  let la, a = aux [] 0 a in
  let lb, ba = aux a 0 b in
  la, lb, List.rev ba

let tail_split a =
  let u, v = List.fold_left (fun (u, v) (x, y) -> x::u, y::v) ([], []) a in
  List.rev u, List.rev v

let tail_map f a = List.rev (List.rev_map f a)
let tail_map2 f a b = List.rev (List.rev_map2 f a b)

(* association lists; value map, index map and renaming map *)
let vmap f l = List.map (fun (i, v) -> (i, f v)) l
let imap f l = List.map (fun (i, _v) -> (i, f i)) l
let rmap f l = List.map (fun (i, v) -> (f i, v)) l

(* association lists, tail version : value map, index map and renaming map *)
let tail_vmap f l = tail_map (fun (i, v) -> (i, f v)) l
let tail_imap f l = tail_map (fun (i, _v) -> (i, f i)) l
let tail_rmap f l = tail_map (fun (i, v) -> (f i, v)) l

(* applies f on all pairs of the cartesian product l1 * l2 *)
let rectangle_map f l1 l2 =
  if l2 = [] then [] (* speedup *) else
    let rec aux l1 l2 =
      match l1 with
      | [] -> []
      | h1::t1 -> List.map (f h1) l2 @ aux t1 l2
    in aux l1 l2

(* a combinator useful when forall x y. f x y = f y x *)
let triangle_map f l1 l2 =
  if l1 = [] || l2 = [] then [] (* speedup *) else
    let (l1, l2) =
      if List.length l1 <= List.length l2
      then (l1, l2) else (l2, l1) in
    let rec aux l1 l2 =
      match l1 with
      | [] -> []
      | [h1] -> List.map (f h1) l2
      | h1::t1 ->
          match l2 with
          | [] -> assert false (* because length l1 <= length l2 *)
          | _h2::t2 -> List.map (f h1) l2 @ aux t1 t2
    in aux l1 l2

let make_compare cmp =
  let rec compare l1 l2 = match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | h1::t1, h2::t2 -> let cmp_h = cmp h1 h2 in
      if cmp_h <> 0 then cmp_h else compare t1 t2
  in compare

let option_like_merge conflict l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | _, [] -> l1
  | [], _ -> l2
  | _, _ -> conflict l1 l2

let choose_random = function
  | [] -> invalid_arg "List.choose"
  | l ->
      let idx = Random.int (length l) in
      nth l idx

(** A generic range filtering function, used in db *)
let filterbounds (start_opt, len) index l =
  let rec cut_beg cond =
    function x::r when cond x -> cut_beg cond r | l -> l in
  let rec rev_cut_end cond acc =
    function [] -> acc | x::_ when cond x -> acc | x::r -> rev_cut_end cond (x::acc) r in
  let rec rev_cut_len n acc =
    function [] -> acc | x::r -> if n <= 0 then acc else rev_cut_len (n-1) (x::acc) r in
  if len >= 0 then
    let l = match start_opt with Some s -> cut_beg (fun x -> index x < s) l | None -> l in
    if len <> 0 then rev (rev_cut_len len [] l) else l
  else
    match start_opt with
    | Some s -> rev_cut_end (fun x -> index x > s) [] l |> rev_cut_len (0-len) [] |> rev
    | None -> rev l |> rev_cut_len (- len) [] |> rev



(* ************************************************************************** *)
(** {b Descr}: Returns the value (second component) associated with the key
    (first component) equal to [x] in a list of pairs.
    Raises [Not_found] if there is no value associated with [x] in the list.
    Equality test is performed with the provided function [eq] instead of the
    general [=] function.                                                     *)
(* ************************************************************************** *)
let rec assoc_custom_equality ~eq x = function
  | [] -> raise Not_found
  | (k, v) :: rem ->
      if eq k x then v else assoc_custom_equality ~eq x rem

let rec assoc_custom_equality_opt ~eq x = function
  | [] -> None
  | (k, v) :: rem ->
      if eq k x then Some v else assoc_custom_equality_opt ~eq x rem


(* ************************************************************************** *)
(** {b Descr}: Transforms a list of triplets into a triplet of lists.         *)
(* ************************************************************************** *)
let rec split3 = function
    [] -> ([], [], [])
  | (x,y,z) :: l ->
      let (rx, ry, rz) = split3 l in (x :: rx, y :: ry, z :: rz)

module MakeAssoc(S:Map.OrderedType) = struct
  type 'a t = (S.t * 'a) list
  let equal x y = S.compare x y = 0
  let rec find k = function
    | [] -> raise Not_found
    | (k',v) :: t ->
        if equal k k' then v
        else find k t
  let rec find_opt k = function
    | [] -> None
    | (k',v) :: t ->
        if equal k k' then Some v
        else find_opt k t
  let rec mem k = function
    | [] -> false
    | (k',_) :: t -> equal k k' || mem k t
  let remove k l =
    let rec aux acc = function
      | [] -> l
      | ((k',_) as c) :: t ->
          if equal k k' then List.rev_append acc t
          else aux (c :: acc) t in
    aux [] l

  let sorted_merge l1 l2 =
    let rec aux acc l1 l2 =
      match l1, l2 with
      | [], l
      | l, [] -> List.rev_append acc l
      | ((k1,_) as c1) :: t1, ((k2,_) as c2) :: t2 ->
          let c = S.compare k1 k2 in
          if c < 0 then
            aux (c1 :: acc) t1 l2
          else
            aux (c2 :: acc) l1 t2 in
    aux [] l1 l2

  let unique_sorted_merge ~merge l1 l2 =
    let rec aux acc l1 l2 =
      match l1, l2 with
      | [], l
      | l, [] -> List.rev_append acc l
      | ((k1,_) as c1) :: t1, ((k2,_) as c2) :: t2 ->
          let c = S.compare k1 k2 in
          if c < 0 then
            aux (c1 :: acc) t1 l2
          else if c = 0 then (
            let (k3,_) as r = merge c1 c2 in
            if not (equal k1 k3) then invalid_arg "BaseList.MakeAssoc.unique_sorted_merge";
            aux acc (r :: t1) t2
          ) else
            aux (c2 :: acc) l1 t2 in
    aux [] l1 l2

  let sort l =
    sort (fun (k1,_) (k2,_) -> S.compare k1 k2) l
end

module StringAssoc = MakeAssoc(String)

let rev_partial_map2 f l1 l2 =
  let rec aux acc l1 l2 =
    match l1, l2 with
    | [], _
    | _, [] -> acc
    | h1 :: t1, h2 :: t2 -> aux (f h1 h2 :: acc) t1 t2 in
  aux [] l1 l2
let partial_map2 f l1 l2 = List.rev (rev_partial_map2 f l1 l2)

let rev_fold_left_partial_map2 f acc l1 l2 =
  let rec aux acc1 acc2 l1 l2 =
    match l1, l2 with
    | [], _
    | _, [] -> acc1, acc2
    | h1 :: t1, h2 :: t2 ->
        let acc1, h = f acc1 h1 h2 in
        aux acc1 (h :: acc2) t1 t2 in
  aux acc [] l1 l2
let fold_left_partial_map2 f acc l1 l2 =
  let acc, l = rev_fold_left_partial_map2 f acc l1 l2 in
  acc, List.rev l

let filter2 f l1 l2 =
  let rec aux acc1 acc2 l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev acc1, List.rev acc2
    | h1 :: t1, h2 :: t2 ->
        if f h1 h2
        then aux (h1 :: acc1) (h2 :: acc2) t1 t2
        else aux acc1 acc2 t1 t2
    | _ -> invalid_arg "List.filter2" in
  aux [] [] l1 l2
