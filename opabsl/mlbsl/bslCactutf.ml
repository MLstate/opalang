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
