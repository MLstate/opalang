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
