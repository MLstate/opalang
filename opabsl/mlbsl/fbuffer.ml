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
(*
 * A private type of *non-functional* buffer
 *
 * Note: It's not serializable, nor is it meant to be.
 *)


##extern-type Buffer2_private.buffer = Buffer.t
##register create \ `Buffer.create` : int -> Buffer2_private.buffer

##register add \ `Buffer.add_string` : Buffer2_private.buffer, string -> void

##register addln: Buffer2_private.buffer, string -> void
let addln buf s =
  Buffer.add_string buf s;
  Buffer.add_string buf "\n"

##register contents \ `Buffer.contents` : Buffer2_private.buffer -> string

##register is_empty: Buffer2_private.buffer -> bool
let is_empty buf = Buffer.length buf = 0

##register reset: Buffer2_private.buffer, int -> void
let reset buf _i = Buffer.clear buf
