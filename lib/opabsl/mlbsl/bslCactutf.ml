(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
(**
   Opabsl binding for Cactutf library.
   @author Corentin Gallet
   @author Rudy Sicard
*)

(**
   The implementation and the documentation is in libbase/Cactutf
   In particular, the documentation of the module Cactutf explain exactly
   the difference between indexation in bytes and unicode char.
*)

(*
  FIXME: this is incorrect, the exception is never raised !
*)
##register lenbytes : int -> option(int)
let lenbytes x =
  try Some (Cactutf.lenbytes x) with Cactutf.Lenbytes _ -> None

##register length_until \ `Cactutf.length_until` : string, int -> int
##register length \ `Cactutf.length` : string -> int
##register nth \ `Cactutf.nth` : string, int -> int
##register next \ `Cactutf.next` : string, int -> int
##register sub \ `Cactutf.sub` : string, int, int -> string
##register sub_opt \ `Cactutf.sub_opt` : string, int, int -> option(string)
##register get \ `Cactutf.get` : string, int -> int
##register look \ `Cactutf.look` : string, int -> int
##register cons \ `Cactutf.cons` : int -> string
##register uppercase \ `Cactutf.uppercase` : string -> string
##register lowercase \ `Cactutf.lowercase` : string -> string
##register one_byte \ `Cactutf.one_byte` : int -> int
##register two_bytes \ `Cactutf.two_bytes` : int, int -> int
##register three_bytes \ `Cactutf.three_bytes` : int, int, int -> int
##register four_bytes \ `Cactutf.four_bytes` : int, int, int, int -> int
##register check \ `Cactutf.check` : string -> bool

(*register charutf8 : string -> int -> int*)
