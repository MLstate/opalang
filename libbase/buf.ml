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

(* Simple library, like Buffer but fixed size but can also look like String if required *)

type buf = { mutable str : string; mutable i : int }
type t = buf

let empty = { str=""; i=0; }

let create size = { str=String.create size; i=0; }

let make size ch = { str=String.make size ch; i=size; }

let resize buf size =
  let str = String.create size in
  let newlen = min buf.i (String.length str) in
  if buf.i > 0 then String.unsafe_blit buf.str 0 str 0 newlen;
  buf.str <- str;
  buf.i <- newlen

let clear buf = buf.i <- 0

let reset buf = buf.str <- ""; buf.i <- 0

let length buf = buf.i

let real_length buf = String.length buf.str

let get buf i =
  if i < 0 || i >= buf.i then invalid_arg (Printf.sprintf "Buf.get index out of bounds %d" i);
  String.get buf.str i
let nth = get

let unsafe_get buf i = String.unsafe_get buf.str i

let set buf i ch =
  if i < 0 || i >= buf.i then invalid_arg (Printf.sprintf "Buf.set index out of bounds %d" i);
  String.set buf.str i ch

let unsafe_set buf i ch = String.unsafe_set buf.str i ch

let sub buf base len =
  if base < 0 || base + len > buf.i then invalid_arg (Printf.sprintf "Buf.sub index out of bounds %d %d" base len);
  String.sub buf.str base len

let add_char buf ch =
  if String.length buf.str - buf.i < 1 then invalid_arg (Printf.sprintf "Buf.add_char %c" ch);
  buf.str.[buf.i] <- ch;
  buf.i <- buf.i + 1

let add_substring buf str base len =
  if String.length buf.str - buf.i < len then invalid_arg (Printf.sprintf "Buf.add_substring %s %d %d" str base len);
  String.unsafe_blit str base buf.str buf.i len;
  buf.i <- buf.i + len

let append buf str len = add_substring buf str 0 len

let extend buf len =
  if String.length buf.str - buf.i < len then invalid_arg (Printf.sprintf "Buf.extend %d" len);
  buf.i <- buf.i + len

let add_string buf str =
  append buf str (String.length str)

let add_buf buf1 buf2 =
  append buf1 buf2.str buf2.i

let of_string str = { str; i=String.length str; }

let to_string buf = String.sub buf.str 0 buf.i
let contents = to_string

let spare buf = String.length buf.str - buf.i

(* Test code *)
(*
let buf = of_string "abc";;
let () = set buf 1 'B';;
let ch = get buf 0;;
let ch = get buf 1;;
let ch = get buf 2;;
let str = try ignore (get buf 3); "NOT OK" with Invalid_argument str -> "OK: "^str;;
let str = try ignore (set buf 4 'x'); "NOT OK" with Invalid_argument str -> "OK: "^str;;
let buf = create 5;;
let () = add_char buf 'D';;
let () = add_string buf "ef";;
let len = length buf;;
let rlen = real_length buf;;
let str = to_string buf;;
let () = resize buf 8;;
let len = length buf;;
let rlen = real_length buf;;
let str = to_string buf;;
let () = resize buf 2;;
let len = length buf;;
let rlen = real_length buf;;
let str = to_string buf;;
let str = try ignore (add_char empty 'x'); "NOT OK" with Invalid_argument str -> "OK: "^str;;
let () = resize buf 10;;
let () = add_string buf "fghi";;
let str = to_string buf;;
let str = sub buf 1 3;;
let str = try ignore (sub buf 100 100); "NOT OK" with Invalid_argument str -> "OK: "^str;;
*)
