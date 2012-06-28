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
(* depends *)
module String = BaseString

(* -- *)

(* normal characters that *should* be escaped in html *)
let except_html_char = "\"&<>"
let fexcept_html_char = BaseChar.cache (String.contains except_html_char)

(* special characters that *should not* be escaped in html *)
let allowed_special_char = "\t\n\r"
let fallowed_special_char = BaseChar.cache (String.contains allowed_special_char)

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
let htmlentities_append buf src start len =
  let unknown = 65533 in
  let last = start+len in
  let rec aux i =
    if i=last then () else (
      let char_code = Cactutf.look src i in
      let next_i = try Cactutf.next src i with _ -> (* bad utf8 *) i+1 in
      let char_ok = (char_code >= 32  ||  fallowed_special_char src.[i])  (* control char *)
                (*&& (char_code <= 128 ||  fexcept_html_char src.[i]) *) (* Useless *)
      in
      Buffer.add_string buf "&#";
      Buffer.add_string buf (Pervasives.string_of_int (if char_ok then char_code else unknown));
      Buffer.add_char buf ';';
      aux next_i
    )
  in
  aux start

let htmlentities src =
  let buf = Buffer.create ((String.length src) * ( (*String.length "&#00;"*)3 + 2 )) in
  htmlentities_append buf src 0 (String.length src);
  Buffer.contents buf
