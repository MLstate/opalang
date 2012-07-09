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

(* Note for hacker :
   size is an indicator of how many elt are in the set.
   it is just a hint, because inhabitants of a set may deleted by the GC.
   it is used in union for optimization purpose.
*)
type ('a, 'b) t = { mutable link : ('a, 'b) node }

and ('a, 'b) immediate = { size  : int ;
			   key   : 'a  ;
			   value : 'b    }

and ('a, 'b) node = | Immediate of ('a, 'b) immediate
                    | Link of ('a, 'b) t

let make k v =
  { link = Immediate { size = 1 ; key = k ; value = v } }

let rec follow = function
  | { link = Immediate _ } as root ->
      root
        
  | { link = Link link } as child ->
      let root = follow link in
      begin
	(* Collapsing rule *)
	child.link <- Link root;
	root
      end

let info x = 
  match follow x with
  | { link = Immediate imm } as root ->
      root, imm.size, imm.key, imm.value
  | { link = Link _ } -> assert false

(* The fact to split find in 2 function in inefficent in case
   of we need the 2 at the same time : factorization of lookup 
   (call to function info) and simplification of API *)

let find x =
  match follow x with
  | { link = Immediate imm } -> imm.key, imm.value
  | _ -> assert false

let key x =
  match follow x with
  | { link = Immediate imm } -> imm.key
  | _ -> assert false

let value x =
  match follow x with
  | { link = Immediate imm } -> imm.value
  | _ -> assert false


let union a b =
  let ca, sa, _, _ = info a
  and cb, sb, k, v = info b in
  (* Weighted Union rule *)
  let tall, low, low_o = 
    (if sa > sb 
     then ca, cb, b
     else cb, ca, a)
  in
  begin
    (* optimisation : origin of low can be collapsed there as well as low.link *)
    low_o.link <- Link tall ;
    low.link   <- Link tall ;
    tall.link  <- Immediate { size = sa + sb ; key = k ; value = v }
  end

let replace ~replaced ~keeped = union replaced keeped

let changeval x v =
  let root = follow x in
  match root.link with
  | Immediate imm -> root.link <- Immediate { imm with value = v }
  | Link _ -> assert false

(*
let refresh_singleton x k v =
  match x.link with
  | Link _ -> assert false
  | Immediate _ ->
      x.link <- Immediate {size = 1; key = k; value = v}
*)
