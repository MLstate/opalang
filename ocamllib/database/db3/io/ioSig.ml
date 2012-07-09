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
  type chan
  exception EOF

  val make_chan_create : string -> chan
  val make_chan_append : string -> chan
  val make_chan_readonly : string -> chan

  val add_int : chan -> int -> unit
  val add_char : chan -> char -> unit
  val add_string : chan -> string -> unit
  val add_float : chan -> float -> unit
  val add_int32 : chan -> int32 -> unit
  val add_int64 : chan -> int64 -> unit
  val output : chan -> unit
  val seek_out : chan -> int -> unit
  val position_out : chan -> int

  val read_int : chan -> int
  val read_char : chan -> char
  val read_string : chan -> string
  val read_float : chan -> float
  val read_int32 : chan -> int32
  val read_int64 : chan -> int64
  val length : chan -> int
  val seek_in : chan -> int -> unit
  val position_in : chan -> int

  val close : chan -> unit
  val erase_file : chan -> unit

  val reset_file : chan -> unit
  val truncate_file : chan -> int -> unit
  val reload : chan -> unit
end
