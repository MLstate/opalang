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
(* some syntactical loop functions
   e.g.
   fold(datastructure[,accumulator])(fun key [val,acc] ->
     loop code
   )

   accumulator is always in last position in the argument function
   key is always in the first position (index is considered as a key)
*)

module For =
struct
  let range a b acc fct =
    let rec aux i acc =
      if i >= b then acc
      else
        let acc = fct i acc in
        aux (succ i) acc
    in
    aux a acc
end


module Fold = struct

  let list(l,acc)(f) = Base.List.foldl f l acc

  let array(a,acc)(f) =
    let len = Array.length a in
    let rec aux f len i acc =
      if i = len then acc
      else aux f len (i+1) (f i (Array.unsafe_get a i) acc)
    in
    aux f len 0 acc

  let rev_array(a,acc)(f) =
    let len = Array.length a in
    let rec aux f i acc =
      if i = (-1) then acc
      else aux f (i-1) (f i (Array.unsafe_get a i) acc)
    in
    aux f (len-1) acc

  let option((o:'a option),(acc:'b))((f:'a->'b->'b)) = Option.fold (fun acc v->f v acc) acc o


end

module InitAcc = struct
  (** init the list with [ fst( f i _) : i C {0..n-1} ] and return the folded accumulator  *)
  let list(n,acc)(f) =
    let rec aux f i acc l =
      if i<0 then l,acc
      else let fi,acc = f i acc in
           let i = i-1
           and l = fi::l in
           aux f i acc l
    in aux f (n-1) acc []
end


module FilterMap = struct
  (* warning : the filter function should be pure, it is called two times for each element *)
  let array(a)(filter)(image) =
    let nb = Fold.array(a,0)(fun i v nb-> if filter i v then nb+1 else nb) in
    Fold.array(a,[||])(fun i v narray ->
      if filter i v then (
        let image = image i v in
        if narray = [||] then Array.create nb image
        else (narray.(i) <- image; narray)
      ) else narray)
end

module Map = struct
  let list(l)(f) = Base.List.map f l
  let array(l)(f) = Base.Array.mapi f l
  let option(o)(f) = Option.map f o
end

module FoldMap = struct
  (* rewrite because would by inefficient otherwise *)
  let list(l,acc)(f) =
    let rec aux f il ol acc =
      match il with
      | x::il ->
          let x,acc = (f x acc) in
          aux f il (x::ol) acc
      | _ -> List.rev ol,acc
    in aux f l [] acc

  let array(array,accu)(f) =
    let len = Array.length array in
    if len = 0 then [||],accu else
      let v0,accu = f 0 array.(0) accu in
      let res = Array.create len v0 in
      let rec aux i ecu =
        if i = len then ecu
        else
      let v,accu = (f i array.(i) ecu) in
      res.(i) <- v;
      aux (i+1) accu
      in
      let accu = aux 1 accu in
      res,accu
end

module Iter = struct
  let list(l)(f) = Base.List.iter f l
  let array(l)(f) = Base.Array.iteri f l
end

module Deprecated = struct
let l_fold = Fold.list
let l_map  = Map.list
let l_filter(l)(f) = List.filter f l
let l_filter_map(l)(f) = Base.List.filter_map f l
let l_for_all(l)(f) = List.for_all f l
let l_map_flat(l) (f) = List.flatten (Base.List.map f l)
let l_map_sort(l) (f) = List.sort compare (Base.List.map f l)
let l_iter (l) (f) = List.iter f l
end
