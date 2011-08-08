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

module type IO_LIGHT =
sig
  module DB : DbSig.DB
  val version : string
  type mode = Create | Append | ReadOnly
  type t = {
    location : string;
    mode : mode;
    mutable dbm : DB.t option;
    mutable link_count : int;
    mutable has_lock : bool;
    mutable timestamp : Time.t;
    mutable next_file_idx : int;
  }
  val dbtbl : (string, t) Hashtbl.t
  val is_open : t -> bool
  val is_closed : t -> bool
  val really_remove_lock_file : t -> unit
  val get_content_file_name : t -> string
  val close : t -> unit
  val make_lock_file : t -> unit
  val remove_lock_file : t -> unit
  val read_lock_file : t -> (string * int) option
  val check_other_used : t -> unit
  val reopen : t -> unit
  val make : mode -> string -> t
  val get_timestamp : t -> Time.t
  val get_location : t -> string
  val get_dbm : t -> DB.t option
  val get_link_count : t -> int
  val get_has_lock : t -> bool
end
