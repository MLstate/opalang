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

#<Ifstatic:OCAML_WORD_SIZE 64>

type t = Nativeint.t

let ( + ) = Nativeint.add
let ( - ) = Nativeint.sub
let ( * ) = Nativeint.mul
let ( / ) = Nativeint.div

exception Overflow
let of_int = Nativeint.of_int
let to_int v =
  let res = Nativeint.to_int v in
   (* make sure that we don't loose the representation during the conversion *)
  if v == of_int res then
    res
  else
    raise Overflow

#<Else>

type t = Int64.t

let ( + ) = Int64.add
let ( - ) = Int64.sub
let ( * ) = Int64.mul
let ( / ) = Int64.div

let of_int v =
  Int64.of_nativeint (Nativeint.of_int v)

exception Overflow
let to_int v =
  let res = Nativeint.to_int (Int64.to_nativeint v) in
   (* make sure that we don't loose the representation during the conversion *)
  if v == of_int res then
    res
  else
    raise Overflow

#<End>
