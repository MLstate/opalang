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
(* CF mli *)

type ('a, 'b) t =
    {
      hash : ('a, 'b) Hashtbl.t;
      queue : ('a * 'b ref) Queue.t;
      mutable list : ('a * 'b ref) list
    }

let create i =
  {
    hash = Hashtbl.create i;
    queue = Queue.create ();
    list = []
  }
let clear t = Hashtbl.clear t.hash; Queue.clear t.queue; t.list <- []

let add t a b = let ab = (a, ref b) in Hashtbl.add t.hash a b; Queue.add ab t.queue; t.list <- ab::t.list
let replace t a b =
  if Hashtbl.mem t.hash a then begin
    Hashtbl.replace t.hash a b;
    List.iter (fun (u, t) -> if a = u then t := b else ()) t.list;
    Queue.iter (fun (u, t) -> if a = u then t := b else ()) t.queue end
  else add t a b

let find_opt t a = try Some (Hashtbl.find t.hash a) with Not_found -> None
let mem t = Hashtbl.mem t.hash

let iter f t = let ff (a, b) = f a !b in Queue.iter ff t.queue
let rev_iter f t = let ff (a, b) = f a !b in List.iter ff t.list

let fold_left f acc t = let ff acc (a, b) = f acc a !b in Queue.fold ff acc t.queue
let fold_right f t acc = let ff acc (a, b) = f a !b acc in List.fold_left ff acc t.list

let to_list t = List.rev_map (fun (a, b) -> (a, !b)) t.list
let to_rev_list t = Queue.fold (fun acc (a, b) -> (a, !b)::acc) [] t.queue

let length t = Hashtbl.length t.hash
