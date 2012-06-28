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
