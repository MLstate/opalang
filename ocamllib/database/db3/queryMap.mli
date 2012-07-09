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

type query =
  | Set of Datas.t
  | Remove of Keys.t

(* This type represents a map from nonempty paths to queries. *)
type t = query list KeyRecMap.map

val print_query : query -> string
val print_query_map : t -> string

val mergeable_query_maps : t -> t -> unit
val add_to_query_map : t -> Path.t -> query -> t
val overwrite_in_query_map : t -> Path.t -> t -> t
val remove_from_query_map : t -> Path.t -> t

(* TODO: both of these only used from copy; revisit after copy code redone *)
val find_map_from_path : t -> Path.t -> query list * t
val merge_query_map : t -> t -> t

val find_set_in_query_list : query list -> Datas.t option
