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

(**
    @author Henri Binsztok,
    @author Gregoire Makridis
*)

type t

val to_string : t -> string

(* In all these functions, ~f is the function that will be used to get node
   from uid. It is expected to know about all uids of the given node between
   last "Full" and current rev (ie all info to get the full node, a list of
   Full, Delta, Delta, ..., Delta) *)

(* creation and updates *)
val create : ?content:Datas.t -> Revision.t -> t
val update : f:(Uid.t -> t) -> Uid.t -> t -> Revision.t -> ?content:Datas.t -> ?child:(Keys.t * Eid.t) -> bool
  -> t * bool
  (** true si taille max de map atteinte *)
val remove_child : f:(Uid.t -> t) -> Revision.t -> t -> Keys.t -> t

(* access to the fields *)
val get_content : f:(Uid.t -> t) -> t -> Datas.t
val get_cur_rev : t -> Revision.t
val get_old_revs : f:(Uid.t -> t) -> t -> (Revision.t * Uid.t) list
val get_children : f:(Uid.t -> t) -> t -> Keys.t list
val get_children_eid : f:(Uid.t -> t) -> t -> Eid.t list

val find_opt : f:(Uid.t -> t) -> Keys.t -> t -> Eid.t option
val next_eid : f:(Uid.t -> t) -> Keys.t -> t -> Eid.t

(* folding *)
val fold : f:(Uid.t -> t) -> (Keys.t -> Eid.t -> 'a -> 'a) -> t -> 'a -> 'a
val fold_range :
  Keys.t option * int -> f:(Uid.t -> t) ->
  (Keys.t -> Eid.t -> 'a -> 'a) -> t -> 'a -> 'a

(** {6 IO} *)

(**
   Types used by the [IoManager] for reading/writing nodes in files.
   The types correspond to the implementation of the type.
   This offers a way for breaking the abstraction of nodes,
   but we assume that the following types and import/export function
   will be used only from [IoManager], only for read/write purpose.
*)

type full = {
  max : Keys.t ;
  min : Keys.t ;
  cur_rev : Revision.t ;
  pred_rev : Revision.t option ;
  old_revs : (Revision.t * Uid.t) list ;
  content : Datas.t ;
  map : Eid.t KeyMap.t ;
}

type delta = {
  new_content : Datas.t option ;
  new_childs : Eid.t KeyMap.t ;
  prof : int ;
}

type rev_delta = {
  old_content : Datas.t option ;
  extra_children : Keys.t list ;
  rev_prof : int ;
}

type io_t =
  | Full of full
  | Delta of (Uid.t * Revision.t * delta)
  | RevDelta of Uid.t * Revision.t * rev_delta

external write : t -> io_t = "%identity"
external read : io_t -> t = "%identity"
