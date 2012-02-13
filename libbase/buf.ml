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

(* Simple library, like Buffer but fixed size but can also look like String if required *)

type buf = { mutable str : string; mutable i : int }
type t = buf

type resize_mode =
  | RM_stdout
  | RM_stderr
  | RM_custom of (string -> unit)
  | RM_failwith
  | RM_exit
  | RM_noresize

let auto_resize = ref RM_stderr

let empty () = { str=""; i=0; }

let create size = { str=String.create size; i=0; }

let make size ch = { str=String.make size ch; i=size; }

(* More conservative than Buffer, we grow more slowly and
 * we allow shrinkage by giving negative values to extra.
 *)
let resize buf extra =
  let strlen = String.length buf.str in
  let target = max 0 (if extra >= 0 then buf.i + extra else strlen + extra) in
  let newsize =
    if extra >= 0
    then
      let newsize = ref (max strlen 2) in
      while !newsize < target do
        newsize := max (!newsize+1) ((!newsize + !newsize + !newsize) / 2)
      done;
      if !newsize > Sys.max_string_length
      then
        if target <= Sys.max_string_length
        then Sys.max_string_length
        else failwith "Buf.resize: cannot increase size of buffer"
      else !newsize
    else target
  in
  let str = String.create newsize in
  let newlen = min buf.i newsize in
  if buf.i > 0 then String.unsafe_blit buf.str 0 str 0 newlen;
  buf.str <- str;
  buf.i <- newlen;
  if newsize > strlen
  then
    let msg = Printf.sprintf "Buf.resize called (now %d), please resize your buffers" newsize in
    match !auto_resize with
    | RM_stdout -> Printf.printf "%s\n%!" msg
    | RM_stderr -> Printf.eprintf "%s\n%!" msg
    | RM_custom f -> f msg
    | RM_failwith -> failwith msg
    | RM_exit -> exit 1
    | RM_noresize -> ()

let autoresize buf extra msg =
  if !auto_resize <> RM_noresize
  then resize buf extra
  else invalid_arg msg

let copy buf = { str=String.copy buf.str; i=buf.i }

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
  if String.length buf.str - buf.i < 1 then autoresize buf 1 (Printf.sprintf "Buf.add_char %c" ch);
  buf.str.[buf.i] <- ch;
  buf.i <- buf.i + 1

let add_substring buf str base len =
  if String.length buf.str - buf.i < len then autoresize buf len (Printf.sprintf "Buf.add_substring %s %d %d" str base len);
  String.unsafe_blit str base buf.str buf.i len;
  buf.i <- buf.i + len

let append buf str len = add_substring buf str 0 len

let extend buf len =
  if String.length buf.str - buf.i < len then autoresize buf len (Printf.sprintf "Buf.extend %d" len);
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
let () = resize buf (-2);;
let len = length buf;;
let rlen = real_length buf;;
let str = to_string buf;;
let str =
  try ignore (add_char (empty()) 'x');
    if !auto_resize <> RM_noresize then "OK: resized" else "NOT OK"
  with Invalid_argument str ->
    if !auto_resize <> RM_noresize then "NOT OK" else "OK: "^str;;
let () = resize buf 10;;
let () = add_string buf "fghi";;
let str = to_string buf;;
let str = sub buf 1 3;;
let str = try ignore (sub buf 100 100); "NOT OK" with Invalid_argument str -> "OK: "^str;;
*)
