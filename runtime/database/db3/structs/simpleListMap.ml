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

(* -- Normal log -- *)
let debug fmt =
  #<If:DEBUG_DB$minlevel 5> Printf.fprintf stdout ("[37m[ListMap][0m"^^fmt^^"\n%!")
    #<Else> Printf.ifprintf stderr ("[37m[ListMap][0m"^^fmt^^"\n%!")
    #<End>

    let print fmt = Logger.info ("[ListMap] "^^fmt)

let error fmt = Logger.error ("[ListMap] "^^fmt)

(* -- *)

type key = Revision.t
type 'a t = (Revision.t * 'a) list

let empty = []

let is_empty = BaseList.is_empty

let add k v lst =
  match lst with
  | [] -> [k,v]
  | (kk,_)::tl ->
      (match Revision.compare k kk with
       | 0 -> (k,v) :: tl
       | 1 -> (k,v) :: lst
       | -1 ->
           (debug "Try to insert an older revision : %s, head is %s"
              (Revision.to_string k) (Revision.to_string kk);
            BaseList.insert_sorted ~cmp:(fun (a,_) (b,_) -> (Revision.compare a b) * -1) ~conflict:(fun a _ -> [a]) (k,v) lst
           )
       | _ -> assert false)

let fold f lst acc =
  let f = fun a (k,v) -> f k v a in
  List.fold_left f acc lst

let iter f lst =
  let f = fun (k,v) -> f k v in
  List.iter f lst

let rev_iter f lst =
  iter f (List.rev lst)

let find = List.assoc

let find_inf key lst =
  let rec aux = function
    | [] -> raise Not_found
    | (k,v)::y ->
        if  Revision.compare k key > 0 then aux y
        else k,v
  in aux lst

let size = List.length

let max = function
  | [] -> raise Not_found
  | x::_ -> x

let keys lst = fst (List.split lst)

let remove_last = List.tl
