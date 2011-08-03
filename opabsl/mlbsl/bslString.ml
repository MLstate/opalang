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
let have_to_be_escaped (c:char) = have_to_be_escaped_table.(Char.code c)

(*Fails with UTF-8 -- use Cactutf?*)
(*TODO: This looks slow -- constructing lists ?*)
(* I think it works ok with utf8 because whenever the code is greater than 128
 * (ie we have a character of more than one byte, BaseString.len_from is used to
 * agglomerate the following bytes whose code is more than 128 (which is the end
 * of the unicode character if is the input is well formed) *)
##register escapeHTML : string -> string
  let escapeHTML src =
    if BaseString.exists have_to_be_escaped src then
      let len = String.length src in
      let rec aux pos acc =
        if pos < len then
          if not (have_to_be_escaped src.[pos]) then
            let to_push = String.sub src pos (BaseString.len_from (fun c -> not (have_to_be_escaped c)) src pos) in
            aux (pos + (String.length to_push)) (to_push::acc)
          else
            let to_push = String.sub src pos (BaseString.len_from have_to_be_escaped src pos) in
            aux (pos + (String.length to_push)) ((Base.Utf8.htmlentities to_push)::acc)
        else acc
      in
      BaseString.rev_sconcat "" (aux 0 [])
    else
      src


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
