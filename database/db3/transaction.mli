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
    @author Mikolaj Konarski
*)

type t

(* start-up and execution of the transaction *)
val init : Hldb.t -> ?read_only:(bool * Revision.t option)-> int -> t
val commit : Revision.t -> t -> Hldb.t -> Hldb.t

(* access to the fields of the transaction *)
val get_db : t -> Hldb.t
val get_num : t -> int
val is_read_only : t -> bool
val modified : t -> bool
val get_read_rev : t -> Revision.t option

(* access to the abstract query_map *)
val get_query_map : t -> QueryMap.t

(* write access to the db *)
val set : t -> Path.t -> DataImpl.t -> t
val remove : t -> Path.t -> t
val set_link : t -> Path.t -> Path.t -> t
val set_copy : t -> Path.t -> (Path.t * Revision.t option) -> t

(* read access to the db *)
val get : t -> Path.t -> DataImpl.t
val get_all_rev_of_path : t -> Path.t -> Revision.t list
val get_last_rev_of_path : t -> Path.t -> Revision.t
val full_search : t -> string list -> Path.t -> Keys.t list
val get_children :
  t -> Revision.t -> Keys.t option * int -> Path.t
  -> (Path.t * Revision.t) list
val stat :
  t -> Path.t
  -> Path.t * Revision.t option * [`Data|`Link|`Unset]

(* disk *)
val append_disk : t -> DbTypes.trans option
  (** eciture de la transaction sur le disque *)

val apply_disk : DbTypes.trans -> Hldb.t -> Revision.t -> Revision.t * Hldb.t
  (** lecture d'une transaction depuis le disque *)
