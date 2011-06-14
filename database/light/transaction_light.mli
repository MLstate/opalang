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
type query = Set of Datas.t | Remove of Path.t
val string_of_query : query -> string
val string_of_query_list : query list -> string
val string_of_query_map : (Path.t * query) list -> string
type t = {
  tr_num : int;
  tr_db : Db_light.t;
  tr_query_map : (Path.t * query) list;
  tr_remove_list : Path.t list;
  tr_index_set : (Path.t * DataImpl.t) list;
  tr_index_remove : (Path.t * DataImpl.t) list;
  tr_op_counter : int;
}
val get_num : t -> int
val get_db : t -> Db_light.t
val get_query_map : t -> (Path.t * query) list
val full_search : t -> string list -> Path.t -> Keys.t list
exception Data_not_found
val find_data_in_query_list : query list -> DataImpl.t
exception Removed
val get_query_at : t -> Path.t -> (Path.t * query) list
val find_set_data_in_query_list : query list -> DataImpl.t option
val stat : t -> Path.t -> Path.t * Revision.t option * [> `Data | `Link | `Unset ]
val get : t -> Path.t -> DataImpl.t
val virtual_get_children : t -> Path.t -> Path.t list
val get_children : t -> Keys.t option * int -> Path.t -> Path.t list
val trans_operation_counter_limit : int
val add_to_query_map : t -> Path.t -> query -> t
val rm_all_with_prefix : Path.t -> Path.t list -> Path.t list
val remove_from_query_map : (Path.t * 'a) list -> Path.t -> (Path.t * 'a) list
val set_link : t -> Path.t -> Path.t -> t
val set_copy : t -> Path.t -> Path.t * 'a -> t
val set : t -> Path.t -> DataImpl.t -> t
val check_remove : t -> Path.t -> bool
val remove : t -> Path.t -> t
val init : Db_light.t -> ?read_only:'a -> int -> t
val update_uid_list : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list
val update_node_list : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list
val execute_query_map : t -> Db_light.t -> t * Db_light.t
val execute_remove_list : t -> Db_light.t -> t * Db_light.t
val modified : t -> bool
val commit : t -> Db_light.t -> Db_light.t
