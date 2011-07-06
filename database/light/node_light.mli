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
type t = {
  mutable content : Datas.t;
  mutable disk_file : string option;
  mutable on_disk : bool;
}

val to_string : t -> string

(* creation and updates *)
val create : ?disk_file:string -> ?max_size:int -> ?content:Datas.t -> unit -> t
val delete : t -> unit
val is_occupied : t -> bool

(* access to the fields *)
val get_content : t -> Datas.t
val set_content : ?max_size:int -> t -> Datas.t -> unit
val equals : t -> t -> bool
val equals_data : t -> Datas.t -> bool
