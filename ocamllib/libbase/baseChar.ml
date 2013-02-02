(*
    Copyright © 2011, 2012 MLstate

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
(* CF mli *)

include Char

(* TODO: benchmark against c1 = c2 or ... *)
(** égalité sans casse *)
let equal_insensitive c1 c2 =
  lowercase c1 = lowercase c2

(** compare sans casse *)
let compare_insensitive c1 c2 =
  compare (lowercase c1) (lowercase c2)

(**
   Mathieu Tue Oct 19 13:33:05 CEST 2010
   What does it do there ??
   Who does use this ?
   My opinion is that it should not be defined there.
*)
(* largeur (approximative) d'un caractère *)
let width =
  (* open Graphics;;
     #load "graphics.cma";;
     open_graph "";;
     set_font "-microsoft-trebuchet ms-*-*-*-*-*-*-*-*-*-*-*-*";;
     let f i = fst (text_size (String.make 1 (char_of_int i)));;
     let a = Array.init 256 f;;
     Array.fold_left (+) 0 a;; *)
  let char_width =
    [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
      0; 0; 0; 0; 0; 0; 0; 7; 8; 9; 13; 13; 16; 16; 7; 8; 8; 9; 13; 8; 8; 8; 9;
      13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 8; 8; 13; 13; 13; 9; 17; 13; 13;
      13; 15; 13; 13; 16; 16; 7; 11; 15; 12; 17; 15; 16; 13; 17; 15; 12; 16; 15;
      16; 21; 15; 16; 13; 11; 11; 11; 13; 13; 13; 13; 13; 11; 13; 12; 9; 12; 13;
      8; 9; 12; 7; 19; 13; 13; 13; 13; 11; 11; 9; 12; 12; 17; 13; 13; 12; 11; 13;
      11; 13; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
      0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 7; 8; 13; 13; 13; 13; 13; 13; 13; 16; 9;
      12; 13; 8; 16; 13; 13; 13; 11; 11; 13; 12; 13; 8; 13; 11; 9; 12; 20; 20;
      20; 8; 13; 13; 13; 13; 13; 13; 21; 13; 13; 13; 13; 13; 7; 7; 7; 7; 15; 15;
      16; 16; 16; 16; 16; 13; 16; 15; 15; 15; 15; 16; 13; 13; 13; 13; 13; 13; 13;
      13; 20; 11; 12; 12; 12; 12; 8; 8; 8; 8; 13; 13; 13; 13; 13; 13; 13; 13; 13;
      12; 12; 12; 12; 13; 13; 13|] in
  fun c -> char_width.(int_of_char c)

let is_digit c = c >= '0' && c <= '9'
let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_lower c = c >= 'a' && c <= 'z'
let is_upper c = c >= 'A' && c <= 'Z'

let is_alpha c = is_digit c or is_lower c or is_upper c

let pred a = (Obj.magic (pred (Obj.magic a)) : char)
let succ a = (Obj.magic (succ (Obj.magic a)) : char)

let is_space = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

(** valeur entière d'un chiffre hexa *)
let hexa_value c =
  int_of_char c - (
    if c >= '0' && c <= '9' then 48 (*int_of_char '0'*)
    else if c >= 'A' && c <= 'F' then 55 (* int_of_char 'A' - 10 *)
    else if c >= 'a' && c <= 'f' then 87 (* int_of_char 'a' - 10 *)
    else assert false
  )

let cache fun_charset =
  let table = Array.init 256 (fun code -> fun_charset (Char.unsafe_chr code)) in
  fun c -> Array.unsafe_get table (Char.code c)
