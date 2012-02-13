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

(* depends *)
module Array = BaseArray

  #<Debugvar:DEBUG_DB>

  (* -- Normal log -- *)
  let debug fmt =
    #<If> Printf.fprintf stdout ("[37m[TabMap][0m "^^fmt^^"\n%!")
      #<Else> Printf.ifprintf stderr ("[37m[TabMap][0m "^^fmt^^"\n%!")
      #<End>

      let print fmt = Logger.info ("[TabMap] "^^fmt)

let error fmt = Logger.error ("[TabMap] "^^fmt)

(* -- *)

module Make (A :
  sig
    type key
    val value : key -> int
    val make : int -> key
  end) =
struct

  type key = A.key
  type 'a t =
      { mutable size : int;
        mutable arr : 'a array;
        name: string;
      }

  let zero_val : 'a = Obj.magic 404

  let empty () =
    { size = 0; arr = Array.make 10000 zero_val; name = BaseRandom.string 10 }

  let is_empty tab = tab.size = 0

  let find key tab =
    let k = A.value key in
    assert (k >= 0);
    if k < tab.size then
      let r = tab.arr.(k) in
      if (r != zero_val) then r
      else raise Not_found
    else
      (raise Not_found)

  let find_opt key tab =
    try Some (find key tab)
    with Not_found -> None

  let add key elem tab =
    let k = A.value key in
    assert (k >= 0);
    let s = Array.length tab.arr in

    if s <= k+1 then
      ( let size = if (2*s) <= (k + 1) then k+1 else s in
        let a = Array.make size zero_val in
        let n = Array.append tab.arr a in
        tab.arr <- n);

    tab.arr.(k) <- elem;

    (if k >= tab.size then
       tab.size <- succ k);

    tab

  let mem key tab =
    try let _ = find key tab in true
    with Not_found -> false

  let fold f tab acc =
    let rec aux i acc =
      if i < tab.size then
        if tab.arr.(i) != zero_val then
          aux (succ i) (f (A.make i) tab.arr.(i) acc)
        else
          aux (succ i) acc
      else acc
    in
    aux 0 acc

  let iter f tab =
    let f = fun k v _ -> f k v in
    fold f tab ()

  let keys tab =
    let f = fun k _ a -> k :: a in
    fold f tab []


  let filter_keys f tab =
    let max = ref 0 in
    let f = fun k -> f (A.make k) in
    for i = 0 to tab.size -1 do
      if f i && tab.arr.(i) != zero_val then
        max := i
      else tab.arr.(i) <- zero_val
    done;
    incr max;
    if !max <> tab.size then
      tab.size <- !max;

    tab

  let max tab =
    let i = tab.size -1 in
    assert (i >= 0);
    let r = tab.arr.(i) in
    assert (r != zero_val);
    A.make i, r

  let size tab = tab.size

  let resize tab n =
    tab.size <- n
end
