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
(*
    @author1 Henri Binsztok,
    @author2 Gregoire Makridis
**)

exception UnqualifiedPath
exception Merge

type t
type index = ((Path.t * float) list) StringMap.t

(* screen printting - debug only *)
val print_db : t -> string

(* the root of the database *)
val root_eid:Eid.t

(* access to the db field *)
val get_rev : t -> Revision.t
val get_tcount : t -> Eid.t
val get_next_uid : t -> Uid.t
val is_empty : t -> bool
val get_uid_map : t -> Uid.t RevisionMap.t EidMap.t
val get_node_map : t -> Node.t UidMap.t
val get_last_nodes : t -> Node.t Uid.Map.t
val get_index : t -> index

(* navigation through the db *)
val get_uid_of_eid : t -> Revision.t -> Eid.t -> Uid.t
val get_node_of_uid : t -> Uid.t -> Node.t
val get_node_of_path : t -> Revision.t -> Path.t -> Node.t * Revision.t
val get_node_of_eid : t -> Revision.t -> Eid.t -> Node.t
val get_eid_of_path : t -> Revision.t -> Path.t -> Eid.t * Revision.t
val is_new_uid : t -> Uid.t -> bool

(* cleaning *)
val clean_tmp_maps : t -> t

(* creation / rebuilding of a database *)
val make : ?weak:(Uid.t -> Node.t) -> unit -> t
val restart :
  ?index:index ->
  Revision.t -> Eid.t -> Uid.t ->
  (Uid.t RevisionMap.t EidMap.t) ->
  Node.t UidMap.t ->
  t

(* basic db writing *)
val update_db : t -> Revision.t -> (Eid.t * Uid.t) list -> (Uid.t * Node.t) list -> t
val remove : t -> Revision.t -> Path.t -> Keys.t -> t
val set_rev : t -> Revision.t -> t

(* basic db reading *)
val get : t -> Revision.t -> Path.t -> DataImpl.t
val get_data : t -> Node.t -> DataImpl.t
val get_children :
  t -> Revision.t -> (Keys.t option * int) option
  -> Path.t -> (Path.t * Revision.t) list
val get_descendants : t -> Path.t -> (Path.t * DataImpl.t) list
val get_all_rev_of_path : t -> Path.t -> Revision.t list
val get_last_rev_of_path : t -> Revision.t -> Path.t -> Revision.t

(* Index management *)
val update_index : t -> (Path.t * DataImpl.t) list -> t
val remove_from_index : t -> (Path.t * DataImpl.t) list -> t
val full_search : t -> string list -> Path.t -> Keys.t list

(* Links *)
val set_link :
  t -> Revision.t -> Path.t -> Path.t -> t

(* Copies *)
val set_copy :
  t -> Revision.t -> Path.t -> ?copy_rev:Revision.t -> Path.t -> t

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
  t -> Revision.t -> Node.t -> Keys.t list -> Keys.t list * Node.t

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
val follow_link : t -> Revision.t -> Path.t -> Path.t * Node.t

val update_aborted : t -> t -> t
