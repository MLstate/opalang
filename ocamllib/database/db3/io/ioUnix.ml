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

(* shorthands *)
module U = Unix


(* -- *)
exception EOF
exception ReadOnly

type chan = 
  { ufd : U.file_descr;
    mutable fd_inpos : int;
    mutable fd_outpos : int;
    readonly : bool;
  }

let englobe ?(out=0) ?(readonly=false) f =
  { ufd = f; fd_inpos = 0; fd_outpos = out; readonly}

let fd_length fd = (U.fstat fd).U.st_size

let make_chan_create s = englobe (U.openfile s [U.O_RDWR; U.O_CREAT; ] File.default_rights)

let make_chan_append s =
  let fd = U.openfile s [U.O_RDWR; U.O_CREAT; ] File.default_rights in
  if Sys.file_exists s then
    englobe fd
  else
    let out = fd_length fd in
    englobe ~out fd

let make_chan_readonly s = englobe ~readonly:true (U.openfile s [U.O_RDONLY] File.default_rights)


let check_write w buff n =
  if w.readonly then raise ReadOnly; 
  assert ((U.lseek w.ufd w.fd_outpos U.SEEK_SET) = w.fd_outpos);
  let t = U.write w.ufd (Buffer.contents buff) 0 n in
  if t <> n then failwith (Printf.sprintf "WRITING : %d/%d" t n);
  w.fd_outpos <- w.fd_outpos + n

let add_int32 w i =
  let i = ref i in
  let b = Buffer.create 4 in
  for c = 0 to 3 do
    Buffer.add_char b (char_of_int (Int32.to_int (Int32.shift_right_logical !i 24))) ;
    i := Int32.shift_left !i 8
  done;
  check_write w b 4

let add_int w i =
  add_int32 w (Int32.of_int i)

let add_int64 w i =
  let i = ref i in
  let b = Buffer.create 8 in
  for c = 0 to 7 do
    Buffer.add_char b (char_of_int (Int64.to_int (Int64.shift_right_logical !i (64-8)))) ;
    i := Int64.shift_left !i 8
  done;
  check_write w b 8

let add_string w s =
  add_int w (String.length s) ;
  let b = Buffer.create (String.length s) in
  Buffer.add_string b s;
  check_write w b (String.length s)

let add_char w c =
  let b = Buffer.create 1 in
  Buffer.add_char b c;
  check_write w b 1

let add_float w f =
  let n = ref (Int64.bits_of_float f) in
  let b = Buffer.create 8 in
  for i = 0 to 7 do
    Buffer.add_char b (char_of_int (Int64.to_int (Int64.shift_right_logical !n 56))) ;
    n := Int64.shift_left !n 8
  done;
  check_write w b 8


let output _  = ()

let seek_out fd i =
  fd.fd_outpos <- i ;
  let res =  (U.lseek fd.ufd i U.SEEK_SET) in
  assert (res = fd.fd_outpos)

let position_out fd =
  fd.fd_outpos


let read_char t =
  assert ((U.lseek t.ufd t.fd_inpos U.SEEK_SET) = t.fd_inpos);
  let s = String.make 1 ' ' in
  let r =  U.read t.ufd s 0 1 in
  if r = 0 then raise EOF;
  if r <> 1 then failwith (Printf.sprintf "READING : %d/%d" r 1);
  t.fd_inpos <- succ t.fd_inpos;
  s.[0]

let read_int32 t =
  let i = ref Int32.zero in
  for c = 0 to 3 do
    let b = int_of_char (read_char t) in
    i := Int32.logor (Int32.shift_left !i 8) (Int32.of_int b)
  done ;
  !i

let read_int64 t =
  let i = ref Int64.zero in
  for c = 0 to 7 do
    let b = int_of_char (read_char t) in
    i := Int64.logor (Int64.shift_left !i 8) (Int64.of_int b)
  done ;
  !i

let read_int t =
  Int32.to_int (read_int32 t)

let read_bytes t l =
  if l >= Sys.max_string_length then
    failwith (Printf.sprintf "read_bytes: limit (%d > %d) exceeded" l Sys.max_string_length)
  else
    Base.String.init l (fun _ -> read_char t)

let read_string t =
  read_bytes t (read_int t)

let read_float t =
  Int64.float_of_bits (read_int64 t)

let length w = fd_length w.ufd

let seek_in fd i =
  fd.fd_inpos <- i ;
  let res = (U.lseek fd.ufd i U.SEEK_SET) in
  assert (res = fd.fd_inpos)


let position_in fd =
  fd.fd_inpos

let close fd = U.close fd.ufd


let erase_file fd =
  fd.fd_inpos <- 0;
  fd.fd_outpos <- 0;
  if not fd.readonly then
    U.ftruncate fd.ufd 0

let reset_file _ = ()

let truncate_file fd s =
  if not fd.readonly then
    U.ftruncate fd.ufd s;
  if fd.fd_inpos > s then fd.fd_inpos <- s;
  if fd.fd_outpos > s then fd.fd_outpos <- s

let reload _ = ()
