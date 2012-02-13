(*
  Copyright © 2011 MLstate

  This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
