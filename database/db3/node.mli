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
  cur_rev : Revision.t ;
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
