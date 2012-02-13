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
