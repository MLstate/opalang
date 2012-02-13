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
