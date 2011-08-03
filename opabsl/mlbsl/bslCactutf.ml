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

(*register charutf8 : string -> int -> int*)
