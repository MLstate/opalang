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

module type DB_LIGHT =
sig
  module DB : DbSig.DB
  module Io_light : Io_light_sig.IO_LIGHT

  exception UnqualifiedPath
  exception Merge

  (* flags *)
  val verify : bool ref
  val use_od : bool ref
  val od_early : bool ref
  val add_tree_1 : float ref
  val ondemand_add_1 : float ref
  val update_data_1 : float ref
  val update_data_2 : float ref
  val update_data_3 : float ref

  (* types *)
  type t
  type tree

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
  val get_index : t -> index
  val get_mtree : t -> Mem_tree_light.mem_tree
  val get_mst : t -> Mst.t option

  val set_version : t -> string -> unit
  val set_filemanager : t -> Io_light.t option -> unit
  val set_max_size : t -> int -> unit
  val set_mtree : t -> Mem_tree_light.mem_tree -> unit
  val set_mst : t -> Mst.t option -> unit

  (* navigation through the db *)
  val get_node_of_path : t -> Path.t -> Node_light.t * Revision.t
  val get_tree_of_path : t -> Path.t -> tree

  (* creation / rebuilding of a database *)
  val make : ?filemanager:Io_light.t -> ?mst:Mst.t -> ?max_size:int -> unit -> t

  (* basic db writing *)
  (*val add_mtree : mem_tree -> Path.t -> Datas.t -> unit*)
  val update : ?no_write:bool -> t -> Path.t -> Datas.t -> t
  val remove : t -> Path.t -> t
  val remove_direct : t -> Path.t -> t
  val set_rev : t -> Revision.t -> t

  (* basic db reading *)
  val get : t -> Path.t -> DataImpl.t
  val get_data : t -> Node_light.t -> DataImpl.t
  val get_children : t -> (Keys.t option * int) option -> Path.t -> Path.t list
  val get_all_children : t -> (Keys.t option * int) option -> Path.t -> Path.t list

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
  val follow_path : t -> tree -> Keys.t list -> Keys.t list * tree

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
  val follow_link : t -> Path.t -> Path.t * tree

  (* Direct *)
  val add_tree_direct : t -> Path.t -> Datas.t -> unit
  val remove_tree_direct : t -> Path.t -> unit
  val get_node_direct : t -> Path.t -> Node_light.t
  val get_direct : t -> Path.t -> DataImpl.t
  val get_children_direct : t -> (Keys.t option * int) option -> Path.t -> Path.t list
  val set_link_direct : t -> Path.t -> Path.t -> t
  val set_copy_direct : t -> Path.t -> Path.t -> t

  (* Caching *)
  val action_od : unit -> unit

  (* Verification *)
  val verify_database : t -> unit
end

