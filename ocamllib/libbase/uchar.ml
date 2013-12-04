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
(**)

type uchar = int
type ustring = uchar array

exception Uchar of int
let ulength = Array.length

let uchar s pos =
  let uchar_aux pos =
    let c = int_of_char s.[pos] in
    if c >= 128 && c < 192 then c land 0b111111
    else raise (Uchar pos)
  in
  let c = int_of_char s.[pos] in
  if c < 128 then c, 1
  else if c >= 192 && c < 224 then 
    ((c land 0b11111) lsl 6) lor (uchar_aux (succ pos)), 2
  else if c >= 224 && c < 240 then 
    ((c land 0b1111) lsl 12) lor (uchar_aux (pos + 1) lsl 6) lor (uchar_aux (pos + 2)), 3
  else if c >= 240 && c <= 247 then 
    ((c land 0b111) lsl 18) lor (uchar_aux (pos + 1) lsl 12) lor (uchar_aux (pos + 2) lsl 6) lor (uchar_aux (pos + 3)), 4
  else raise (Uchar pos)

(** benchmark vs FUNC *)
let ulength_of_string s =
  let l = ref 0 in
  String.iter (fun x -> if x < '\128' || (x >= '\192' && x < '\248') then incr l) s ;
  !l

let length_of_ustring us =
  let l = ref 0 in
  Array.iteri (
    fun i x ->      
      if x < 128 then incr l
      else l := !l + (	
	if x < 2048 then 2
	else if x < 65536 then 3
	else if x < 2097152 then 4
	else raise (Uchar i)
      )
  ) us ;
  !l

let ustring s =
  let l = ulength_of_string s in
  let us = Array.create l 0 in
  let f = uchar s in
  let rec aux pos spos =
    if pos = l then us
    else 
      let uc, nb = f spos in 
      Array.unsafe_set us pos uc ; 
      aux (succ pos) (spos + nb)
  in aux 0 0
    
let chars_of_uchar c =
  if c < 128 then [char_of_int c]
  else if c < 2048 then 
    [ char_of_int (((c land 0b11111000000) lsr 6) lor 0b11000000)
    ; char_of_int  ((c land 0b00000111111)        lor 0b10000000) ]
  else if c < 65536 then
    [ char_of_int (((c land 0b1111000000000000) lsr 12) lor 0b11100000)
    ; char_of_int (((c land 0b0000111111000000) lsr  6) lor 0b10000000)
    ; char_of_int  ((c land 0b0000000000111111)         lor 0b10000000) ]
  else ( 
    assert (c < 2097152) ;
    [ char_of_int (((c land 0b111000000000000000000) lsr 18) lor 0b11110000)
    ; char_of_int (((c land 0b000111111000000000000) lsr 12) lor 0b10000000)
    ; char_of_int (((c land 0b000000000111111000000) lsr  6) lor 0b10000000)
    ; char_of_int  ((c land 0b000000000000000111111)         lor 0b10000000) ]
  )

(* FIXME: unsafe_set/get *)
let of_ustring us =
  let l = length_of_ustring us in
  let s = String.create l in
  let rec aux pos spos =
    if pos = Array.length us then s
    else
      let cl = chars_of_uchar us.(pos) in
      let nb = List.fold_left (fun i c -> s.[spos + i] <- c ; succ i) 0 cl in
      aux (succ pos) (spos + nb)
  in aux 0 0
    
