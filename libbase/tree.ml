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
(** A module for general non empty tree.
    If you need trees that may be empty, you can use an option on this type.
 *)

module List = BaseList

module Tree : sig
  type 'a t = Tree of 'a * 'a t list
  val leaf : 'a -> 'a t
  val is_leaf : 'a t -> bool
  val value : 'a t -> 'a
  val children : 'a t -> 'a t list
  val to_string : ('a -> string) -> 'a t -> string
  val get_path_opt : 'a t -> 'a list -> 'a t option
end = struct
  type 'a t = Tree of 'a * 'a t list
  let leaf a = Tree(a,[])
  let is_leaf (Tree (_,l)) = l = []
  let value (Tree (a,_)) = a
  let children (Tree (_,l)) = l
  let rec to_string f (Tree (s,l)) =
    Printf.sprintf "(%s:%s)" (f s) (List.to_string (to_string f) l)
  let rec get_path_opt (Tree (_,children) as tree) = function
    | [] -> Some tree
    | h :: t ->
        match List.find_opt (fun tr -> value tr = h) children with
          | None -> None
          | Some tree -> get_path_opt tree t
end

include Tree

module S = struct
  type 'a t = 'b Tree.t constraint 'a = 'b * 'c * 'd
  let subs_cons (Tree (x,l)) = (fun l -> Tree(x,l)), l
end

(** defines map, fold, etc. *)
module Walk = Traverse.Make(S)
