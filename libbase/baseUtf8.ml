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
(* depends *)
module String = BaseString

(* -- *)

let except_html_char = "\"&<>" (* normal characters that *should* be escaped in html *)
let allowed_special_char = "\t\n\r" (* special characters that *should not* be escaped in html *)

let string_of_int i =
  let r1 i = Char.chr (((i mod 64) + 128)) in
  let r2 i = Char.chr (((i / 64) mod 64) + 128) in
  if i < 128 then String.make 1 (Char.chr i)
  else if i < 2048 then
    let t = [| Char.chr ((i / 64) + 192); r1 i |] in
    String.init 2 (fun n -> t.(n))
  else if i < 65536 then
    let t = [| Char.chr (((i / 4096) mod 16) + 224); r2 i; r1 i |] in
    String.init 3 (fun n -> t.(n))
  else if i < 2097152 then
    let t = [| Char.chr (((i / 262144) mod 8) + 240); Char.chr (((i / 4096) mod 64) + 128); r2 i; r1 i |] in
    String.init 4 (fun n -> t.(n))
  else assert false

(*  takes any utf8 string and converts it into html entities (escape *all* characters, which is not avised in general; please perform checks before calling it) *)
let htmlentities src =
  let rec pow n p = if p = 0 then 1 else (pow n (p - 1)) * n in
  let len = String.length src in
  let soi = Pervasives.string_of_int in
  let rec aux nbr pos i lst =
    if i = len then lst
    else
      let chr = Char.code src.[i] in
      if pos = (-1) then
        if chr >= 240 then aux ((chr mod 16) * 262144) 2 (i + 1) lst
        else if chr >= 224 then aux ((chr mod 32) * 4096) 1 (i + 1) lst
        else if chr >= 192 then aux ((chr mod 64) * 64) 0 (i + 1) lst
        else if (chr < 128 && (chr >= 32 || String.contains allowed_special_char src.[i])) || String.contains except_html_char src.[i]
        then aux 0 (-1) (i + 1) (chr::lst)
        else
          (* between 128 and 192: malformed UTF-8; we should absolutely not fail, but return the usual black question mark for invalid symbols *)
          (* between 0 and 31, except allowed_special_char, the entities seem to be illegal, so we project again to the question mark *)
          begin
            (* Journal.Interface.warning (Printf.sprintf "Warning: htmlentities: invalid UTF-8: in string %s at position %d on character of code %d" src i chr); *)
            aux 0 (-1) (i + 1) (65533::lst)
          end
      else
        let nbr = nbr + ((chr mod 64) * (pow 64 pos)) in
        if pos = 0 then aux 0 (-1) (i + 1) (nbr::lst)
        else aux nbr (pos - 1) (i + 1) lst
  in
  List.fold_right (fun item acc -> acc^"&#"^(soi item)^";") (aux 0 (-1) 0 []) ""
