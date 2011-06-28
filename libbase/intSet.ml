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
let (|>) = InfixOperator.(|>)

  (* -- *)

exception Found
type elt = int
type t = unit IntMap.t
let empty:t = IntMap.empty
let is_empty t = IntMap.is_empty t
let mem (int:int) (set:t) = (IntMap.find_opt int set ) <> None
let add int set = IntMap.add int () set
let add_list int_list set = List.fold_left (fun acc item -> add item acc) set int_list
let singleton int = IntMap.singleton int ()
let remove int set = IntMap.remove int set
let union (set:t) (set':t) = IntMap.merge (fun a _b -> a) set set'
let compare (set:t) set' = IntMap.compare (compare) set set'
let elements (set:t) = IntMap.keys set
let from_list (li: int list) = List.map (fun x -> x, ()) li |> IntMap.from_list
let iter (f:int -> unit) (set:t) = IntMap.iter (fun k _v -> f k) set
let fold f (set:t) acc = IntMap.fold (fun int () acc -> f int acc) set acc
let fold_rev f set acc = IntMap.fold_rev (fun int () acc -> f int acc) set acc
let equal set set' = IntMap.equal (=) set set'
let min_elt (set:t) = IntMap.min set |> fst
let max_elt (set:t) = IntMap.max set |> fst
let size (set:t) = IntMap.size set
let cardinal = size
let filter = IntMap.filter_keys

let exists f set =
  try
    iter (fun x -> if f x then raise Found) set;
    false
  with Found -> true

let map (f:int -> int) (set:t) = fold (fun x acc -> add (f x) acc) set empty

let for_all f (set:t) =
  try
    iter (fun x -> if f x then () else raise Found) set;
    true
  with Found -> false

let partition f set = fold
  (fun k (ok, no) ->
     if f k
     then (add k ok), no
     else ok, (add k no)
  ) set (empty, empty)

let inter set set' = filter (fun x -> mem x set') set
let diff set set' = filter (fun x -> not (mem x set')) set
let subset (set:t) (set':t) = for_all (fun x -> mem x set') set

let choose = min_elt

let choose_opt (set:t) =
  match IntMap.choose_opt set with
  | None -> None
  | Some (a, _) -> Some a

let split int set =
  fold
    (fun i (inf, b, sup) ->
       if i < int then ((add i inf), b, sup)
       else if i > int then (inf, b, (add i sup))
       else (inf, true, sup)
    ) set (empty, false, empty)

let draw set = let min = min_elt set in min, remove min set
let complete (_:(elt -> elt -> bool)) (_:elt) (_s:t) = let _ = assert false in empty

(* cf doc *)
let pp sep ppe fmt t =
  let fiter elt =
    ppe fmt elt ;
    Format.fprintf fmt sep
  in
  iter fiter t

let compare_elt : int -> int -> int = Pervasives.compare

let safe_union = IntMap.safe_merge

let from_sorted_array _ = assert false
