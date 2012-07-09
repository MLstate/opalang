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

(*
  GUIDELINES FOR BASE : CF MLI
*)


type ('a,'b) either = Left of 'a | Right of 'b

let copyright = "(*
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
"

let is_windows = Sys.os_type = "Win32"

let failwithf fmt = Printf.ksprintf failwith fmt
let invalid_argf fmt = Printf.ksprintf invalid_arg fmt

(* *)
let debug_getenv _ dflt = dflt
let debug_getenv_toggle _ = false

let crlf = "\r\n"
external identity : 'a -> 'a = "%identity"
let intmax (a:int) b = if a > b then a else b
let intmin (a:int) b = if a < b then a else b
let compare_int (a:int) b = Pervasives.compare a b
external format_float: string -> float -> string = "caml_format_float"
(* string_of_float should be optimized ! *)

let int_of_string_opt s =
  try Some (int_of_string s) with Failure "int_of_string" -> None

let max3 a b = max (max a b)

(** arrondi à n *)
let round n v =
  let p = 10. ** (float_of_int n) in
  let vp = v *. p in
  let ce = ceil vp
  and fl = floor vp in
  (if ce -. vp < vp -. fl then ce else fl) /. p



exception NotImplemented of string
exception ParseError


(* ======================== *)

(* alphabetic order *)
module Arg = BaseArg
module Array = BaseArray
module Char = BaseChar
module Filename = BaseFilename
module Format = BaseFormat
module Hashtbl = BaseHashtbl
module Int64 = BaseInt64
module Lazy = BaseLazy
module List = BaseList
module Map = BaseMap
module Marshal = BaseMarshal
module Obj = BaseObj
module Random = BaseRandom
module Set = BaseSet
module String = BaseString
module Utf8 = BaseUtf8