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

#<Ifstatic:MLSTATE_WINDOWS .*>
(* TODO: We need to fall back on the arithmetic version for Windows. *)
let __i_le_32 _ _ _ = assert false
let __i_be_32 _ _ _ = assert false
let __i_le_32l _ _ _ = assert false
let __i_be_32l _ _ _ = assert false
let __i_le_64 _ _ _ = assert false
let __i_be_64 _ _ _ = assert false
let __i_le_64L _ _ _ = assert false
let __i_be_64L _ _ _ = assert false
#<Else>
external __i_le_32 : string -> int -> int -> unit = "__i_le_32"
external __i_be_32 : string -> int -> int -> unit = "__i_be_32"
external __i_le_32l : string -> int -> int32 -> unit = "__i_le_32l"
external __i_be_32l : string -> int -> int32 -> unit = "__i_be_32l"
external __i_le_64 : string -> int -> int -> unit = "__i_le_64"
external __i_be_64 : string -> int -> int -> unit = "__i_be_64"
external __i_le_64L : string -> int -> int64 -> unit = "__i_le_64L"
external __i_be_64L : string -> int -> int64 -> unit = "__i_be_64L"
#<End>

(* Test code *)
(*
let tstli32 i = let s32 = "------" in Endian.__i_le_32 s32 1 i; print_string (dump 10 s32);;
let tstbi32 i = let s32 = "------" in Endian.__i_be_32 s32 1 i; print_string (dump 10 s32);;
let tstli32l i = let s32 = "------" in Endian.__i_le_32l s32 1 i; print_string (dump 10 s32);;
let tstbi32l i = let s32 = "------" in Endian.__i_be_32l s32 1 i; print_string (dump 10 s32);;
let tstli64 i = let s64 = "----------" in Endian.__i_le_64 s64 1 i; print_string (dump 10 s64);;
let tstbi64 i = let s64 = "----------" in Endian.__i_be_64 s64 1 i; print_string (dump 10 s64);;
let tstli64L i = let s64 = "----------" in Endian.__i_le_64L s64 1 i; print_string (dump 10 s64);;
let tstbi64L i = let s64 = "----------" in Endian.__i_be_64L s64 1 i; print_string (dump 10 s64);;
module S = Stuff.StuffString;;
HttpTools.timefn 1000000 (Endian.__i_le_32 "----" 0) 0x55555555;; (*time: 0.138049*)
HttpTools.timefn 1000000 (S.lei32 "----" 0) 0x55555555;; (*time: 0.800141*)
HttpTools.timefn 1000000 (Endian.__i_le_64 "--------" 0) 0x5555555555555555;; (*time: 0.096849*)
HttpTools.timefn 1000000 (S.lei64 "--------" 0) 0x5555555555555555;; (*time: 1.248208*)
*)

