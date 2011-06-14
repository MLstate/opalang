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
exception UnqualifiedPath
exception Merge

type t
type tree
(*type node_map*)
type index = ((Path.t * float) list) StringMap.t

(* access to node *)
val node_uid : tree -> Uid.t
val node_key : tree -> Keys.t
val node_node : tree -> Node_light.t
val node_up : tree -> tree

(* screen printting - debug only *)
val print_db : t -> string

(* the root of the database *)
val root_eid:Eid.t

(* access to the db field *)
val get_rev : t -> Revision.t
val get_tcount : t -> Eid.t
val get_next_uid : t -> Uid.t
val is_empty : t -> bool
(*val get_uid_map : t -> Uid.t RevisionMap.t EidMap.t
val get_node_map : t -> node_map
val get_noweak_node_map : t -> Node.t UidMap.t
val get_last_nodes : t -> Node.t UidMap.t*)
val get_index : t -> index

val set_version : t -> string -> unit

(* navigation through the db *)
(*val get_uid_of_eid : t -> Revision.t -> Eid.t -> Uid.t
val get_node_of_uid : t -> Uid.t -> Node_light.t*)
val get_node_of_path : t -> (*Revision.t ->*) Path.t -> Node_light.t * Revision.t
(*val get_node_of_eid : t -> Revision.t -> Eid.t -> Node_light.t
val get_eid_of_path : t -> Revision.t -> Path.t -> Eid.t * Revision.t
val is_new_uid : t -> Uid.t -> bool*)
val get_tree_of_path : t -> Path.t -> tree

(* cleaning *)
(*val clean_tmp_maps : t -> t*)

(* creation / rebuilding of a database *)
(*val make_node_map_from_weak : (Uid.t, Node_light.t) WeakCacheMap.t -> node_map
val make_node_map_from_uidmap : Node_light.t UidMap.t -> node_map*)
val make : (*?weak:(Uid.t -> Node_light.t) ->*) unit -> t
(*val restart :
  ?index:index ->
  Revision.t -> Eid.t -> Uid.t ->
  (Uid.t RevisionMap.t EidMap.t) ->
  node_map ->
  t*)

(* basic db writing *)
(*val update_db : t -> Revision.t -> (Eid.t * Uid.t) list -> (Uid.t * Node_light.t) list -> t*)
val update : t -> Path.t -> Datas.t -> t
val remove : t -> Path.t -> t
val set_rev : t -> Revision.t -> t

(* basic db reading *)
val get : t -> (*Revision.t ->*) Path.t -> DataImpl.t
val get_data : t -> Node_light.t -> DataImpl.t
(*val get_children :
  t -> Revision.t -> (Keys.t option * int) option
  -> Path.t -> (Path.t * Revision.t) list
*)
val get_children : t -> (Keys.t option * int) option -> Path.t -> Path.t list
(*
val get_descendants : t -> Path.t -> (Path.t * DataImpl.t) list
val get_all_rev_of_path : t -> Path.t -> Revision.t list
val get_last_rev_of_path : t -> Revision.t -> Path.t -> Revision.t*)
(* Index management *)
val update_index : t -> (Path.t * DataImpl.t) list -> t
val remove_from_index : t -> (Path.t * DataImpl.t) list -> t
val full_search : t -> string list -> Path.t -> Keys.t list

(* Links *)
val set_link : t -> Path.t -> Path.t -> t

(* Copies *)
val set_copy : t -> Path.t -> Path.t -> t

(** [follow_path db rev node path_end] follows a path until copy or link
    is encountered, if any.

    @param db  the database to inspect
    @param rev  everything will be read in this revision
    @param node  the node to start traversing at
    @param path_end  the path to walk along (as a [Keys.t list])

    @return The node at which a copy or link was encountered
    and the remaining suffix of the path.
*)
val follow_path :
  (*t -> Revision.t -> Node_light.t -> Keys.t list -> Keys.t list * Node_light.t*)
  t -> tree -> Keys.t list -> Keys.t list * tree

(*
(** [follow_link db original_rev path] returns unwound path as it was
    at db revision [original_rev]. The result is independent on any
    changes to the databse after [original_rev]. There is no escape
    to the current revision via links (as would be the case if
    the old revision came from a Copy node). If there is escape via link,
    it's to [original_rev]. All copies are followed, just as links are.

    @param db  the database to inspect
    @param original_rev  everything will be read in this revision
    @param path  the path to traverse and unwind from root to the end

    @return The path unwound at [original_rev].
*)
val follow_link : t -> Revision.t -> Path.t -> Path.t * Node_light.t
*)

val follow_link : t -> Path.t -> Path.t * tree
