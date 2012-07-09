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

module type SESSION_LIGHT =
sig
  module DB : DbSig.DB
  module Io_light : Io_light_sig.IO_LIGHT
  module Db_light : Db_light_sig.DB_LIGHT
  module Transaction_light : Transaction_light_sig.TRANSACTION_LIGHT

  exception UnqualifiedPath
  type tr_FIFO = (Transaction_light.t * (Transaction_light.t * bool -> unit)) Queue.t
  val is_empty_FIFO : 'a Queue.t -> bool
  val create_FIFO : unit -> 'a Queue.t
  val add_FIFO : 'a -> 'b -> ('a * 'b) Queue.t -> unit
  val take_FIFO : 'a Queue.t -> 'a
  type lock = (Transaction_light.t * Db_light.t) option
  type t = {
    mutable trans_num : int;
    mutable db_ref : Db_light.t;
    with_dot : bool;
    with_ondemand : bool;
    with_direct : bool;
    with_max_size : int;
    is_weak : bool;
    file_manager : Db_light.Io_light.t;
    mutable session_lock : lock;
    waiting_FIFO : tr_FIFO;
  }
  exception Open of (t option * string)
  exception DiskError of string
  val disk_writing : t -> Transaction_light.t -> unit
  val get_timestamp : t -> Time.t
  val position : string -> string
  val init_db : ?ondemand:bool -> ?direct:bool -> ?max_size:int -> Db_light.Io_light.mode -> string -> t
  val make : ?dot:'a -> ?weak:'b -> ?ondemand:bool -> ?direct:bool -> ?max_size:int -> string -> t
  val close_db : ?donothing:bool -> t -> unit
  val restart_db_from_last : t -> Db_light.t
  val restart_db : ?dot:'a -> ?weak:'b -> ?restore:'c -> ?openat_rev:'d -> ?ondemand:bool -> ?direct:bool -> ?max_size:int -> string -> t
  val open_db_aux : ?dot:'a -> ?weak:'b -> ?rev:'c -> ?restore:'d -> ?ondemand:bool -> ?direct:bool -> ?max_size:int -> string -> t * bool
  val open_db : ?dot:'a -> ?weak:'b -> ?rev:'c -> ?restore:'d -> ?ondemand:bool -> ?direct:bool -> ?max_size:int -> string -> t * bool
  val is_empty : t -> bool
  val get_rev : t -> Revision.t
  val is_closed_db : t -> bool
  val new_trans : ?read_only:bool * 'a -> t -> Transaction_light.t
  val abort_of_unprepared : t -> Transaction_light.t -> unit
  val _prepare_commit : Db_light.t -> Transaction_light.t -> Db_light.t
  val prepare_commit : t -> Transaction_light.t -> (Transaction_light.t * bool -> unit) -> (Transaction_light.t * (Transaction_light.t * bool -> unit)) option
  val try_prepare_commit : t -> Transaction_light.t -> (Transaction_light.t * bool -> unit) -> (Transaction_light.t * (Transaction_light.t * bool -> unit)) option
  val pop_trans_k : t -> (Transaction_light.t * (Transaction_light.t * bool -> unit)) option
  val try_trans_prepare : t -> Transaction_light.t -> (Transaction_light.t * bool -> unit) -> unit
  val pop_trans_prepare : t -> unit
  val abort_or_rollback : t -> Transaction_light.t -> unit
  val really_commit : t -> Transaction_light.t -> bool
  val get : t -> Transaction_light.t -> Path.t -> DataImpl.t
  val get_children : t -> Transaction_light.t -> Keys.t option * int -> Path.t -> Path.t list
  val stat : t -> Transaction_light.t -> Path.t -> Path.t * Revision.t option * [> `Data | `Link | `Unset ]
  val full_search : t -> Transaction_light.t -> string list -> Path.t -> Keys.t list
  val set : t -> Transaction_light.t -> Path.t -> DataImpl.t -> Transaction_light.t
  val remove : t -> Transaction_light.t -> Path.t -> Transaction_light.t
  val set_link : t -> Transaction_light.t -> Path.t -> Path.t -> Transaction_light.t
  val set_copy : t -> Transaction_light.t -> Path.t -> Path.t * 'b -> Transaction_light.t
end

