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
exception MaxSize
exception UnknownCell

type 'a t =
    { mutable array : 'a array
    ; init : 'a
    ; mutable length : int }

let make ?size n init =
  let size = match size with Some s -> s | _ -> n in
  { array = Array.make size init
  ; init = init
  ; length = n }

let create = make

let clear t =
  t.array <- [||];
  t.length <- 0

let get a i =
  if i < a.length then Array.unsafe_get a.array i
  else raise UnknownCell
    (** problème: set peut renvoyer (sans qu'on le sache) un tableau frais
        cela est gênant si on dépend d'effets de bord
    *)

let set a i v =
  let l = Array.length a.array in
  if i < l then begin
    Array.unsafe_set a.array i v ;
    if i >= a.length then a.length <- succ i
  end
  else if i < Sys.max_array_length then
    let n = max i (min Sys.max_array_length (2 * l)) in
    begin
      a.array <- Array.init n (
        fun j ->
          if j < l then Array.unsafe_get a.array j
          else if j = i then v
          else a.init
      ) ;
      a.length <- succ i
    end
  else raise MaxSize

let length a = a.length

let real_length a = Array.length a.array

let append a b =
  let la = length a
  and lb = length b in
  let l = la + lb in
  { array = Array.init l (
      fun i ->
        if i < la then Array.unsafe_get a.array i
        else Array.unsafe_get b.array (i - la)
    )
  ; init = if la>0 || lb>0 then Array.unsafe_get (if la > 0 then a else b).array 0 else a.init
  ; length = la + lb }

let fold_left f init a =
  let rec aux acc i =
    if i = length a then acc
    else aux (f acc (Array.unsafe_get a.array i)) (succ i)
  in aux init 0

let fold_left_i f init a =
  let rec aux acc i =
    if i = length a then acc
    else aux (f acc (Array.unsafe_get a.array i) ~i) (succ i)
  in aux init 0

let delete a pfrom pto =
  let offset = pto - pfrom in
  if offset > 0 then begin
    for i = pto to pred (length a) do
      a.array.(i - offset) <- a.array.(i)
    done ;
    a.length <- a.length - offset
  end else failwith "delete failed: empty region"

let insert a pos b =
  let la = length a
  and lb = length b in
  let l = la + lb in
  if l <= real_length a then begin
    for i = pred l downto pos + lb do
      a.array.(i) <- a.array.(i - lb)
    done ;
    for i = pos to pred (pos + lb) do
      a.array.(i) <- b.array.(i - pos)
    done ;
    a.length <- a.length + lb
  end else
    raise (Base.NotImplemented "insert")
