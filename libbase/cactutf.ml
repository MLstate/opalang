(*
    Copyright © 2011, 2012 MLstate

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
(*
  -------------------
  | cactUTF section |
  -------------------
*)

(*
 * Note of the authors:
 *
 * Beware of the stings !
**)

type unicode = int
type unicode_index = int
type bytes_index = int

let (@>) s pos =
  int_of_char(s.[pos])

(*
  FIXME: wild error management, incorrect
*)
exception Error of string
let myerror s = raise (Error s)
let warning_or_error s =
  if true then prerr_endline s else myerror s
(* END OF FIXME *)

exception Lenbytes of int
let pre_lenbytes i =
  if (i < 128) then
    1
  else if (i < 192) then
    raise (Lenbytes i)
  else if (i < 224) then
    2
  else if (i < 240) then
    3
  else
    4
(*
  For one Unicode code, return the number of bytes needed for
  a representation in UTF-8.
*)
let lenbytes i =
  try
    pre_lenbytes i
  with
    Lenbytes i ->
      warning_or_error (Printf.sprintf "bslCactutf.lenbytes : invalid UTF8 opcode: %d" i);
      1

(* determine if a bytechar is the first of a utf-8 char *)
let is_first_char i = i < 128 ||  192 <= i
let is_middle_char i = 128 <= i && i < 192

(*
** prev()
** Return the index of the first utf8-char before or at the given char.
*)
let prev_first_char str i =
  let len = String.length str in
    if i>= len then myerror (Printf.sprintf "prev_first_char : index too big (i=%d, len=%d)"  i len);
    let rec aux i =
      if i<0 then myerror "prev_first_char : reach the begin of the string"
      else (if is_middle_char (str @>  i) then aux (i-1) else i)
    in aux i
(*
(*register prev : string -> int -> int*)
let prev_first_char s n =
  let i4 = s @> (n - 1)
  in
  if (i4 < 128) then (* one byte *)
    n - 1
  else if (i4 >= 192) then (* 0b11xxxxxx *)
    myerror "cactutf.opa : prev : error in UTF8 [1]"
  else
    let i3 = s @> (n - 2)
    in
    if (i3 < 128) then (* 0b0xxxxxxx *)
      myerror "cactutf.opa : prev : error in UTF8 [2]"
    else if (i3 >= 224) then (* 0b111xxxxx *)
      myerror "cactutf.opa : prev : error in UTF8 [3]"
    else if (i3 < 192) then (* 0b110xxxxx *)
      n - 2
    else
      let i2 = s @> (n - 3)
      in
      if (i2 < 128) then
        myerror "cactutf.opa : prev : error in UTF8 [4]"
      else if (i2 >= 224) then (* three bytes *)
        n - 3
      else if (i2 >= 192) then
        myerror "cactutf.opa : prev : error in UTF8 [5]"
      else
        let i1 = s @> (n - 4)
        in
        if (i1 >= 224) then (* four bytes *)
          n - 4
        else
          myerror "cactutf.opa : prev : error in UTF8 [6]"
*)


(* a light version of next *)
let pre_next str n = n + lenbytes (str @> n)

let check str =
  let len = String.length str in
  let rec aux i =
    if i<len then aux (pre_next str i)
    else if i=len then true
    else false
  in
    try aux 0 with Error _ -> false

let mess_invalid_char str n =
  let v = str @> n in Printf.sprintf "the char at raw position %d is invalid [code is %d] : <<%c>> , global string validity is %b" n v (str.[n]) (check str)

let length_until s pos =
  let rec aux s c i len =
    if (i >= len) then
      c
    else
      let n = s @> i
      in
      let k = lenbytes n
      in
      aux s (c + 1) (i + k) len
  in
  aux s 0 0 pos

let length s =
  length_until s (String.length s)

(*
** next()
** Return the index of the next Unicode character.
*)
(*##register next : string -> int -> int*)
let next str n =
  try
    n + pre_lenbytes (str @> n)
  with Lenbytes _ -> warning_or_error ("bslCactutf.next : "^(mess_invalid_char str n)); n+1

(*
** prev()
** Return the index of the previous Unicode character.
*)
(*##register prev : string -> int -> int*)
let prev str n =
  (* FIND THE PREVIOUS CHAR *)
  let i = prev_first_char str (n-1) in
    if i<0 then ( warning_or_error ("bslCactutf.prev"); i )
    else i


(*
** nth()
** Return the index of the n-th Unicode character.
** use a cache to speed-up similar calls
** memoize last n-th caracter position and restart from it
*)
let nth =
  let cache_s = ref "" in
  let cache_th= ref 0 in
  let cache_i = ref 0 in
    fun str th ->
      try
        let len = String.length str in
          if not(!cache_s == str) then begin
            (* if str change then start from begining
               TODO could start from begin or end to be at least 2x faster
            *)
            cache_th:= 0;
            cache_i := 0;
            cache_s := str;
          end;
          (* TODO could start from begin or end to be faster if previous cache is not adapted *)
          (* disabled this warning since it floods TT and is useless if nobody works on improving it
           * if !cache_th < th-100 || !cache_th > th+100 then (Printf.printf "bslCactutf.nth may slow you"; flush stdout);*)
          if !cache_th < th then (
            while !cache_th<th && (!cache_i) < len do
              cache_th := !cache_th +1;
              cache_i  := next str !cache_i;
            done;
            !cache_i
          ) else (

            while !cache_th > th && (!cache_i)>= 0 do
              cache_th := !cache_th -1;
              cache_i  := prev str !cache_i;
            done;
            !cache_i
          )
      with _ ->
        warning_or_error (Printf.sprintf "bslCactutf.nth : utf-8 position %d[=>%d], global string validity is %b" (!cache_th) (!cache_i) (check str));
        !cache_i

type range_validity =
  | Invalid_range of string (* an error message is given *)
  | Valid_range (* the requested substring *)

(*
 * Return a pair
 * - a flag saying if the requested substring was valid
 * - the unicode substring (clipped to the size of the string if needed)
 *)
let sub_no_failure s i n =
  if n = 0 then (Valid_range, "") else (* used to work that way, should be improved
                                        * bacause n = 0 and i < 0 is not valid
                                        * but when n = 0, and i = 0, we say
                                        * [pj = nth s (-1)] which prints an unwanted
                                        * error ! *)
  let len = String.length s in
  let pi = nth s i in
  let pj = nth s (i+n-1) in
  let pi' = max pi 0 in
  let pj' =
    if pj >= len then
      len - pi'
    else
      min (pj-pi'+lenbytes (s @> pj)) (len-pi') in
  let substring = String.sub s pi' pj' in
  let validity =
    if i < 0 then
      Invalid_range "cactutf.opa : sub(_, i<0 ,_) : index is negative"
    else if n < 0 then
      Invalid_range "cactutf.opa : sub(_, n<0 ,_) : index is negative"
    else if n = 0 then
      Valid_range
    else if pi >= len then
      Invalid_range (Printf.sprintf "cactutf.opa : sub(s, i=%d>utf_length(s)=%d ,_) : the index is too big, cryptic info=(%d,%d)" i (length s) pi len)
    else if pj >= len then
      Invalid_range (Printf.sprintf "cactutf.opa : sub(s, i=%d ,n=%d | i+n=%d>utf_length(s)=%d ) : the required length is too big, cryptic info=(%d,%d)" i n (i+n) (length s) pj len)
    else
      Valid_range in
  (validity, substring)

(*
** sub()
** Return an Unicode sub-string.
*)
(*##register sub : string -> int -> int -> string*)
let sub s i n =
  let validity, substring = sub_no_failure s i n in
  ( match validity with
      | Invalid_range s -> warning_or_error s
      | Valid_range -> ()
  );
  substring

(*##register sub_opt : string -> int -> int -> string option*)
let sub_opt s i n =
  match sub_no_failure s i n with
    | (Invalid_range _, _) -> None
    | (Valid_range, s) -> Some s


let one_byte b1 = b1
(*
** two_bytes()
** Encode two bytes into one Unicode character.
** 0b110xxxx 0b10xxxxxx
*)
let two_bytes b1 b2 =
  (((b1 - 192) * 64) + (b2 - 128))

(*
** three bytes()
*)
let three_bytes b1 b2 b3 =
  (((b1 - 224) * 4096) + ((b2 - 128) * 64) + (b3 - 128))

(*
** four bytes()
*)
let four_bytes b1 b2 b3 b4 =
  (((b1 - 240) * 262144) + ((b2 - 128) * 4096) + ((b3 - 128) * 64) + (b4 - 128))

(*
** charutf8()
** Return the Unicode code at the index in a string.
*)
(*register charutf8 : string -> int -> int*)
let charutf8 str pos =
  let i = str @> pos
  in
  let len = lenbytes i
  in
  if (len = 1) then
    i
  else if (len = 2) then
    two_bytes i (str @> (pos + 1))
  else if (len = 3) then
    three_bytes i (str @> (pos + 1)) (str @> (pos + 2))
  else
    four_bytes i (str @> (pos + 1)) (str @> (pos + 2)) (str @> (pos + 3))

(*
** get()
** Return the Unicode code of the nth Unicode character.
*)
(*##register get : string -> int -> int*)
let get str n =
  charutf8 str (nth str n)

(*
** look()
** Return the Unicode code using the index (and not the nth).
** A lot faster, but only when using index instead of position.
*)
(*##register look : string -> int -> int*)
let look str i =
  charutf8 str i

(*
*)
let csize n =
  if (n < 128) then
    1
  else if (n < 2048) then
    2
  else if (n < 65536) then
    3
  else
    4

(*
** cons()
** Build a new string from a character.
*)
(*##register cons : int -> string*)
let cons c =
  let c = if c < 0 then 0 else c in
  let s = csize c in
  let str = String.create s in
  if (s = 1) then
    (str.[0] <- char_of_int(c);
     str)
  else if (s = 2) then
    let n1 = c / 64 in
    let n2 = c - (n1 * 64) in
    str.[0] <- char_of_int(n1 + 192);
    str.[1] <- char_of_int(n2 + 128);
    str
  else if (s = 3) then
    let n1 = c / 4096 in
    let n2 = (c - (n1 * 4096)) / 64 in
    let n3 = (c - (n1 * 4096) - (n2 * 64)) in
    str.[0] <- char_of_int(n1 + 224);
    str.[1] <- char_of_int(n2 + 128);
    str.[2] <- char_of_int(n3 + 128);
    str
  else
    let n1 = c / 262144 in
    let n2 = (c - (n1 * 262144)) / 4096 in
    let n3 = (c - (n1 * 262144) - (n2 * 4096)) / 64 in
    let n4 = (c - (n1 * 262144) - (n2 * 4096)) - (n3 * 64) in
    str.[0] <- char_of_int(n1 + 240);
    str.[1] <- char_of_int(n2 + 128);
    str.[2] <- char_of_int(n3 + 128);
    str.[3] <- char_of_int(n4 + 128);
    str

(*
** uppercase()
** Return an Uppercase version of the string.
** Beware of the Braille and some greeks caracters.
*)
(*##register uppercase : string -> string*)
let uppercase str =
  let myupp i =
    if ((i >= 97) && (i <= 123))                                  (* US-ASCII *)
      || ((i >= 224) && (i <= 246))                               (* ISO-8859-1 (latin-1)  v *)
      || ((i >= 248) && (i <= 254))                               (* ISO-8859-1 (latin-1)  ^ *)
      || ((i >= 65345) && (i <= 65370))                           (* caracteres demi/pleine chasse *)
      || ((i >= 944) && (i <= 974))                               (* grec *)
      || ((i >= 1072) && (i <= 1103)) then                        (* cyrillique *)
        i - 32
    else if ((i >= 257) && (i <= 319) && ((i mod 2) = 1))         (* latin étendu A v *)
      || ((i >= 314) && (i <= 328) && ((i mod 2) = 0))
      || ((i >= 331) && (i <= 375) && ((i mod 2) = 1))
      || (i = 378) || (i = 380) || (i = 382)                      (* latin étendu A ^ *)
      || (i = 389) || (i = 392) || (i = 396) || (i = 402)         (* latin étendu B v *)
      || (i = 409) || (i = 417) || (i = 419) || (i = 421)
      || (i = 424) || (i = 429) || (i = 432) || (i = 434)
      || (i = 436) || (i = 438) || (i = 453) || (i = 456)
      || (i = 459)
      || ((i >= 462) && (i <= 476) && ((i mod 2) = 0))
      || ((i >= 479) && (i <= 495) && ((i mod 2) = 1))
      || (i = 498)
      || ((i >= 501) && (i <= 563) && ((i mod 2) = 1))
      || (i = 572) || (i = 578) || (i = 585) || (i = 587)
      || (i = 589) || (i = 591)                                   (* latin étendu B ^ *)
      || ((i >= 7680) && (i <= 7935) && ((i mod 2) = 1))          (* latin étendu additionnel *)
      || (i = 8580)                                               (* nombre latin : facteur 10 *)
      || ((i >= 977) && (i <= 1007) && ((i mod 2) = 1))           (* grec avec accents *)
      || ((i >= 1120) && (i <= 1153) && ((i mod 2) = 1))          (* cyrillique v *)
      || ((i >= 1163) && (i <= 1215) && ((i mod 2) = 1))
      || ((i >= 1217) && (i <= 1230) && ((i mod 2) = 0))
      || ((i >= 1232) && (i <= 1279) && ((i mod 2) = 1))          (* cyrillique ^ *)
      || ((i >= 1280) && (i <= 1299) && ((i mod 2) = 1)) then     (* cyrillique additionnel *)
      i - 1
    else if (i = 454) || (i = 457) || (i = 460) || (i = 499) then (* latin étendu B doubles lettres *)
      i - 2
    else if (i = 255) then                                        (* special case : ÿ. Latin 1&A.*)
      376
    else if ((i >= 9424) && (i <= 9449)) then                     (* lettres pastilles *)
      i - 26
    else if ((i >= 1104) && (i <= 1119)) then                     (* cyrillique *)
      i - 80
    else if ((i >= 7936) && (i <= 8047) && ((i mod 16) <= 7))      (* grec polytonique v *)
      || ((i >= 8064) && (i <= 8111) && ((i mod 16) <= 7)) then    (* grev polytonique ^ *)
        i + 8
    else if ((i >= 1377) && (i <= 1414)) then                     (* arménien *)
      i - 48
    else if ((i >= 8560) && (i <= 8575)) then                     (* nombres latins *)
      i - 16
    else
      i
  in
  let rec aux len pos accu =
    if (len = 0) then
      accu
    else
      aux (len - 1) (next str pos) (accu ^ cons (myupp (look str pos)))
  in
  aux (length str) 0 ""

(*
** lowercase()
** See uppercase().
*)
(*##register lowercase : string -> string*)
let lowercase str =
  let mylow i =
    if ((i >= 65) && (i <= 91))                                  (* US-ASCII *)
      || ((i >= 192) && (i <= 214))                               (* ISO-8859-1 (latin-1)  v *)
      || ((i >= 216) && (i <= 222))                               (* ISO-8859-1 (latin-1)  ^ *)
      || ((i >= 65313) && (i <= 65338))                           (* caracteres demi/pleine chasse *)
      || ((i >= 912) && (i <= 942))                               (* grec *)
      || ((i >= 1040) && (i <= 1071)) then                        (* cyrillique *)
        i + 32
    else if ((i >= 256) && (i <= 319) && ((i mod 2) = 0))         (* latin étendu A v *)
      || ((i >= 313) && (i <= 328) && ((i mod 2) = 1))
      || ((i >= 330) && (i <= 375) && ((i mod 2) = 0))
      || (i = 377) || (i = 379) || (i = 381)                      (* latin étendu A ^ *)
      || (i = 388) || (i = 391) || (i = 395) || (i = 401)         (* latin étendu B v *)
      || (i = 408) || (i = 416) || (i = 418) || (i = 420)
      || (i = 423) || (i = 428) || (i = 431) || (i = 433)
      || (i = 435) || (i = 437) || (i = 453) || (i = 456)
      || (i = 459)
      || ((i >= 461) && (i <= 476) && ((i mod 2) = 1))
      || ((i >= 478) && (i <= 495) && ((i mod 2) = 0))
      || (i = 498)
      || ((i >= 500) && (i <= 563) && ((i mod 2) = 0))
      || (i = 571) || (i = 577) || (i = 584) || (i = 586)
      || (i = 588) || (i = 590)                                   (* latin étendu B ^ *)
      || ((i >= 7680) && (i <= 7935) && ((i mod 2) = 0))          (* latin étendu additionnel *)
      || (i = 8579)                                               (* nombre latin : facteur 10 *)
      || ((i >= 976) && (i <= 1007) && ((i mod 2) = 0))           (* grec avec accents *)
      || ((i >= 1120) && (i <= 1153) && ((i mod 2) = 0))          (* cyrillique v *)
      || ((i >= 1162) && (i <= 1215) && ((i mod 2) = 0))
      || ((i >= 1217) && (i <= 1230) && ((i mod 2) = 1))
      || ((i >= 1232) && (i <= 1279) && ((i mod 2) = 0))          (* cyrillique ^ *)
      || ((i >= 1280) && (i <= 1299) && ((i mod 2) = 0)) then     (* cyrillique additionnel *)
        i + 1
    else if (i = 452) || (i = 455) || (i = 458) || (i = 497) then (* latin étendu B doubles lettres *)
      i + 2
    else if (i = 376) then                                        (* special case : ÿ. Latin 1&A.*)
      255
    else if ((i >= 9398) && (i <= 9423)) then                     (* lettres pastilles *)
      i + 26
    else if ((i >= 1024) && (i <= 1039)) then                     (* cyrillique *)
      i + 80
    else if ((i >= 7936) && (i <= 8047) && ((i mod 16) > 7))      (* grec polytonique v *)
      || ((i >= 8064) && (i <= 8111) && ((i mod 16) > 7)) then    (* grev polytonique ^ *)
        i - 8
    else if ((i >= 1329) && (i <= 1366)) then                      (* arménien *)
      i + 48
    else if ((i >= 8544) && (i <= 8559)) then                      (* nombres latins *)
      i + 16
    else
      i
  in
  let rec aux len pos accu =
    if (len = 0) then
      accu
    else
      aux (len - 1) (next str pos) (accu ^ cons (mylow (look str pos)))
  in
  aux (length str) 0 ""


exception Done
(*
let remove_accents s =
    let buffer = Buffer.create (String.length s) in
    let add    = Buffer.add_char buffer       in
    let add_array a ~start ~length=
      for i = 0 to length - 1 do
        Utf8.store buffer a.(start + i)
      done
    in
    let lex_one_char    = lexer
      | [ 192 193 194 195 196 197 65 (*ÀÁÂÃÄÅA*) ] -> add 'A'
      | [ 224 225 226 227 228 229 97 (*àáâãäåa*) ] -> add 'a'
      | [ 210 211 212 213 214 216 79 (*ÒÓÔÕÖØO*) ] -> add 'O'
      | [ 242 243 244 245 246 248 111 (*òóôõöøo*) ]-> add 'o'
      | [ 200 201 202 203 69 (*ÈÉÊËE*) ]           -> add 'E'
      | [ 232 233 234 235 101 (*èéêëe*) ]          -> add 'e'
      | [ 199 67 (*ÇC*) ]                          -> add 'C'
      | [ 231 99 (*çc*) ]                          -> add 'c'
      | [ 204 205 206 207 73 (*ÌÍÎÏI*) ]           -> add 'I'
      | [ 236 237 238 239 105 (*ìíîïi*) ]          -> add 'i'
      | [ 217 218 219 220 85 (*ÙÚÛÜU*) ]           -> add 'U'
      | [ 249 250 251 252 117 (*ùúûüu*) ]          -> add 'u'
      | [ 255 121 (*ÿy*) ]                         -> add 'y'
      | [ 209 78 (*ÑN*) ]                          -> add 'N'
      | [ 241 110 (*ñn*) ]                         -> add 'n'
      | eof       -> raise Done
      | _        -> add_array (Ulexing.get_buf lexbuf) ~start:(Ulexing.get_start lexbuf) ~length:(Ulexing.lexeme_length lexbuf)
    in
    try
      let lexbuf = Ulexing.from_utf8_string s in
      while true do
        lex_one_char lexbuf
      done;
      assert false
    with
      Done -> Buffer.contents buffer
*)
(*##endmodule*)
