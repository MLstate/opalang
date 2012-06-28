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
exception MaxSize

type 'a t =
    {
      mutable array : 'a Weak.t;
      mutable length : int
    }

let make ?size n =
  let size =
    match size with
      Some s -> s
    | _ -> n in
  {
    array = Weak.create size;
    length = n
  }

let create = make

let get a i =
  if i < a.length && i >= 0 then Weak.get a.array i
  else None

(** problème: set peut renvoyer (sans qu'on le sache) un tableau frais
    cela est gênant si on dépend d'effets de bord *)
let set a i v =
  let l = Weak.length a.array in
  if i < l then begin
    Weak.set a.array i v ;
    if i >= a.length then a.length <- succ i
  end
  else if i < Sys.max_array_length then
    let n = max i (min Sys.max_array_length (2 * l)) in
    begin
      let na = Weak.create n in
      Weak.blit a.array 0 na 0 l;
      a.array <- na;
      a.length <- succ i
    end
  else raise MaxSize

let remove a i =
  set a i None

let length a = a.length

let real_length a = Weak.length a.array

let fold_left f init a =
  let rec aux acc i =
    if i = length a then acc
    else
      match Weak.get a.array i with
      | Some x -> aux (f acc x) (succ i)
      | None -> aux acc (succ i)
  in aux init 0

let fold_left_i f init a =
  let rec aux acc j i =
    if j = length a then acc
    else
      match Weak.get a.array j with
      | Some x -> aux (f acc x ~i) (succ j) (succ i)
      | None -> aux acc (succ j) i
  in aux init 0 0
