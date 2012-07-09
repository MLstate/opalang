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

module type S =
sig

  exception Corruption
  exception EOF

  type t
  type mode = Create | Append | ReadOnly
  type file = Node | Uid | Uid_rev | Timestamp | Flags | Trans | Db_state | Config | Lock
  type write = WChar of char | WInt of int | WString of string  | WFloat of float | WInt32 of int32 | WInt64 of int64
  type read = RChar | RInt | RString  | RFloat | RInt32 | RInt64


  val make : ?_extension:string -> mode -> string -> t
  val get_name : t -> file -> string
  val get_location : t -> string
  val empty_file : t -> file -> unit
  val output : t -> file -> unit
  val add_int : t -> file -> int -> unit
  val add_char : t -> file -> char -> unit
  val add_string : t -> file -> string -> unit
  val add_float : t -> file -> float -> unit
  val add_int32 : t -> file -> int32 -> unit
  val add_int64 : t -> file -> int64 -> unit
  val add : t -> file -> ?output:bool -> write list -> unit
  val seek_out : t -> file -> int -> unit
  val position_out : t -> file -> int
  val read_int : t -> file -> int
  val read_char : t -> file -> char
  val read_string : t -> file -> string
  val read_float : t -> file -> float
  val read_int32 : t -> file -> int32
  val read_int64 : t -> file -> int64
  val read : t -> file -> read list -> write list
  val position_in : t -> file -> int
  val seek_in : t -> file -> int -> unit
  val length : t -> file -> int
  val close : t -> unit
  val is_open : t -> bool
  val reset_files : t -> unit
  val set_size : t -> file -> int -> unit
  val copy_file : t -> file -> ?location:string -> string -> unit


  val single_write : string -> write list -> unit
  val single_read : string -> read list -> write list
  val get_single_length : string -> int


  type unik
  val create_unik : ?mode:mode -> string -> unik
  val add_unik : unik -> ?output:bool -> write list -> unit
  val position_out_unik : unik -> int
  val seek_out_unik : unik -> int -> unit

  val mv : t -> unik -> file -> unit

  (*
    Specialized read/write
  *)
  module Channel :
  sig
    type channel
    val get : t -> file -> channel
    val add : channel -> write -> unit

    val read_char : channel -> char
    val read_int : channel -> int
    val read_string : channel -> string

    val add_char : channel -> char -> unit
    val add_int : channel -> int -> unit
    val add_string : channel -> string -> unit
  end
end
