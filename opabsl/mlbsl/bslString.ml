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
##register concat \ `Pervasives.( ^ )` : string, string -> string

##register length \ `String.length` : string -> int

##register get : string, int -> string
  let get s i = String.make 1 (String.get s i)

##register repeat \ `BaseString.repeat` : string, int -> string

##register sub : int, int, string -> string
  let sub start len src = String.sub src start len

##register replace : string, string, string -> string
  let replace search replacement source =
    BaseString.replace source search replacement

##register strip_quotes \ `BaseString.strip_quotes` : string -> string

##register index : string, string -> option(int)
  let index pattern source =
    let lp = String.length pattern in
    let ls = String.length source in
    let rec aux id =
      if ls - lp - id >= 0 then
        if BaseString.is_substring pattern source id then Some id
        else aux (id + 1)
      else
        None
    in
      aux 0

(*Low-level: fails with UTF-8*)
##register reverse : string -> string
  let reverse s =
    let len = String.length s in
    let res = String.make len ' ' in
    let i = ref 1 in
      String.iter (fun c ->
                   String.set res (len - !i) c;
                   i := !i + 1;
                   ()) s;
      res

##register lowercase \ `Cactutf.lowercase` : string -> string

##register uppercase \ `Cactutf.uppercase` : string -> string

##register remove_accents \ `BaseString.remove_accents` : string -> string

let have_to_be_escaped_table =
  let have_to_be_escaped chr =
    let code = Char.code chr in
    code >= 128 || String.contains Base.Utf8.except_html_char chr || (code < 32 && not (String.contains Base.Utf8.allowed_special_char chr)) in
  Array.init 256 (fun code -> have_to_be_escaped (Char.unsafe_chr code))
let have_to_be_escaped (c:char) = Array.unsafe_get have_to_be_escaped_table (Char.code c)
let not_have_to_be_escaped (c:char) = not (have_to_be_escaped c)

(* too slow
let _utf8_byte_have_to_be_escaped = function
  | '"' | '<' | '>' | '&' -> true
(* | '\'' -> true *)
  | _ -> false
*)
(* Don't understand how using an array is faster in this case, but it is *)
let table_utf8_byte_have_to_be_escaped = Array.init 256 (fun code -> String.contains Base.Utf8.except_html_char (Char.unsafe_chr code))
let utf8_byte_have_to_be_escaped c = Array.unsafe_get table_utf8_byte_have_to_be_escaped (Char.code c)


let utf8_byte_not_have_to_be_escaped c = not (utf8_byte_have_to_be_escaped c)

(* fast path for non escaped part x2 speed-up *)
(* same as BaseString.len_from but avoiding second order call and hopefully benefit from inlining *)
let rec from_to_utf8_nbhtbe src i len =
  if (i<>len) && (utf8_byte_not_have_to_be_escaped (String.unsafe_get src i)) then from_to_utf8_nbhtbe src (i+1) len else i
let rec from_to_nhtbe src i len =
  if (i<>len) && (not_have_to_be_escaped (String.unsafe_get src i)) then from_to_nhtbe src (i+1) len else i

let empty_buf = Buffer.create 0

(* This thing works with utf-8 because
   - if utf8 encoding is ok, no 'one byte utf8 char' needs to be escaped if the html has utf-8 encoding,
   - if utf8 is not ok, all byte of longer than on byte character are seen as needing escaping *)
##register escapeHTML : bool, string -> string
let escapeHTML utf8 src =
  let len = String.length src in
  if len=0 then "" else
  let have_to_be_escaped     = if utf8 then utf8_byte_have_to_be_escaped     else have_to_be_escaped     in
  let not_have_to_be_escaped = if utf8 then utf8_byte_not_have_to_be_escaped else not_have_to_be_escaped in
  let rec aux pos buf =
    let need_escaping = have_to_be_escaped src.[pos] in
    let len_to_push =
      if utf8 && not(need_escaping) then (from_to_utf8_nbhtbe src pos len)-pos
      else if not(need_escaping) then (from_to_nhtbe src pos len)-pos
      else (
        let case = if need_escaping then have_to_be_escaped else not_have_to_be_escaped in
        BaseString.len_from case src pos
      )
    in
    let new_pos = pos + len_to_push in
    if new_pos >= len && buf == empty_buf then (* optimize for the escape-in-one-step case *)
      (if need_escaping then Base.Utf8.htmlentities src else src)
    else
      let buf = if buf == empty_buf then Buffer.create (len+(4*len/(new_pos+1))) else buf in
      if need_escaping then
        Base.Utf8.htmlentities_append buf src pos len_to_push
      else
        Buffer.add_substring buf src pos len_to_push;
      if new_pos < len then
        aux new_pos buf
      else
        Buffer.contents buf
  in
  aux 0 empty_buf

##register to_character \ `Base.Utf8.string_of_int` : int -> string

##register of_int \ `Pervasives.string_of_int` : int -> string

##register of_byte_val : int -> string
let of_byte_val byte =
  try
    String.make 1 (Char.chr byte)
  with
  | Invalid_argument _ -> "\000"

##register of_byte_unsafe : int -> string
let of_byte_unsafe i =
  String.make 1 (Base.Char.chr i)

##register byte_at_unsafe : int, string -> int
let byte_at_unsafe n s = Base.Char.code s.[n]
   (* special function for TRX *)
   (* TODO write it in C for better performance (on pointers)?
           we could then even use some bit-level magic cleverlness to compare word-by-word instead
           of byte-by-byte*)
##register check_match_literal : string, int, string -> bool
  let check_match_literal input pos literal =
    let n = String.length literal in
    let i = ref 0 in
    while !i < n && String.unsafe_get input (pos + !i) == String.unsafe_get literal !i do
      incr i
    done;
    !i == n


##register leq: string, string -> bool
let leq (a:string) (b:string) = a <= b

##register lt: string, string -> bool
let lt (a:string) (b:string) = a < b

##register eq: string, string -> bool
let eq (a:string) (b:string) = a = b

##register geq: string, string -> bool
let geq (a:string) (b:string) = a >= b

##register gt: string, string -> bool
let gt (a:string) (b:string) = a > b

##register neq: string, string -> bool
let neq (a:string) (b:string) = a <> b

##register ordering: string, string -> opa[Order.ordering]
let ordering (a:string) (b:string) =
  match String.compare a b with
  | -1 -> BslPervasives.ord_result_lt
  | 0 -> BslPervasives.ord_result_eq
  | 1 -> BslPervasives.ord_result_gt
  | _ -> assert false

##register encode_uri_component\ `Encodings.encode_uri_component`: string -> string
##register decode_uri_component\ `Encodings.decode_uri_component`: string -> string
