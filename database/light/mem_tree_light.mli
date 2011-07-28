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

(* Simple image of the DB tree in memory without data *)
type mem_tree = {
  msts : (Keys.t, mem_tree) Hashtbl.t;
  mutable mkey : Keys.t;
  mutable mdata : bool;
}
val make : ?hint:int -> Keys.t -> mem_tree
val fold : (Path.t -> Keys.t -> bool -> 'a -> 'a) -> 'a -> mem_tree -> Path.t -> 'a
val find_mtree : mem_tree -> Path.t -> mem_tree option
val find_mtree_data : mem_tree -> Path.t -> bool option
val find_mtree_sts : mem_tree -> Path.t -> mem_tree list option
val find_mtree_sks : mem_tree -> Path.t -> Keys.t list option
(*val refresh_mtree : mem_tree -> Path.t -> bool -> unit*)
val add_mtree : mem_tree -> Path.t -> Datas.ns -> unit
(*val remove_mtree : mem_tree -> Path.t -> bool*)
val copymt : mem_tree -> mem_tree
val comparemt : mem_tree -> mem_tree -> bool
val output_mt : out_channel -> mem_tree -> unit
val input_mt : in_channel -> mem_tree
val string_of_mtree : mem_tree -> string
