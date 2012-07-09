(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
