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
include Cryptokit

external whirlpool_init: unit -> string = "caml_whirlpool_init"
external whirlpool_update: string -> string -> int -> int -> unit = "caml_whirlpool_update"
external whirlpool_final: string -> string = "caml_whirlpool_final"

class whirlpool =
  object(self)
    val context = whirlpool_init()
    method hash_size = 64
    method add_substring src ofs len =
      if ofs < 0 || ofs + len > String.length src
      then invalid_arg "whirlpool#add_substring";
      whirlpool_update context src ofs len
    method add_string src =
      whirlpool_update context src 0 (String.length src)
    method add_char c =
      self#add_string (String.make 1 c)
    method add_byte b =
      self#add_char (Char.unsafe_chr b)
    method result =
      whirlpool_final context
    method wipe =
      wipe_string context
  end

let whirlpool () = new whirlpool
