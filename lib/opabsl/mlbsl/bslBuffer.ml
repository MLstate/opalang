(*
    Copyright © 2011, 2012 MLstate

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
(**
 * A low-level module of mutable string buffers.
 *
 * Values of type [buffer] are not and should not be serialized, so any use of buffer must be strictly controlled
 *
 *
 * @author David Rajchenbach-Teller
 *)

##extern-type Buffer.t = Buffer.t

##register create\ `Buffer.create`: int -> Buffer.t
##register append\ `Buffer.add_string`: Buffer.t, string -> void
##register contents\ `Buffer.contents`: Buffer.t -> string
##register length\ `Buffer.length`: Buffer.t -> int
##register clear\ `Buffer.clear`: Buffer.t -> void

(* Temporary *)
##register serialize_string_length: string -> string
let serialize_string_length s =
  let ser_int b i = (* DIRTY DIRTY copy pasting *)
    for j = 64 / 8 - 1 downto 0 do
      Buffer.add_char b (Char.chr ((i lsr (j*8)) mod 256));
    done
  in
  let b = Buffer.create 10 in
  ser_int b (String.length s);
  Buffer.contents b
