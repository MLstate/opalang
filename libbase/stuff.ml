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


(* Stuff data in buffers *)

(* These might be more efficient written as C externals. *)

(* Naming convention:
   - (l|b) little/big endian
   - (e|d) encode/decode
   - (i32|i64|d|i32l) int32/int64/float/Int32.t
*)

module type STUFF =
sig
  type t
  val get : t -> int -> char
  val set : t -> int -> char -> unit
end

module StuffF (S : STUFF) =
struct

  let lei32 s pos i =
    S.set s (pos+3) (Char.chr ((i lsr 24) land 0xff));
    S.set s (pos+2) (Char.chr ((i lsr 16) land 0xff));
    S.set s (pos+1) (Char.chr ((i lsr 8 ) land 0xff));
    S.set s (pos+0) (Char.chr ( i         land 0xff))

  let bei32 s pos i =
    S.set s (pos+0) (Char.chr ((i lsr 24) land 0xff));
    S.set s (pos+1) (Char.chr ((i lsr 16) land 0xff));
    S.set s (pos+2) (Char.chr ((i lsr 8 ) land 0xff));
    S.set s (pos+3) (Char.chr ( i         land 0xff))

(*
  let lei64 s pos i =
    S.set s (pos+7) (Char.chr ((i lsr 56) land 0xff));
    S.set s (pos+6) (Char.chr ((i lsr 48) land 0xff));
    S.set s (pos+5) (Char.chr ((i lsr 40) land 0xff));
    S.set s (pos+4) (Char.chr ((i lsr 32) land 0xff));
    S.set s (pos+3) (Char.chr ((i lsr 24) land 0xff));
    S.set s (pos+2) (Char.chr ((i lsr 16) land 0xff));
    S.set s (pos+1) (Char.chr ((i lsr 8 ) land 0xff));
    S.set s (pos+0) (Char.chr ( i         land 0xff))

  let bei64 s pos i =
    S.set s (pos+0) (Char.chr ((i lsr 56) land 0xff));
    S.set s (pos+1) (Char.chr ((i lsr 48) land 0xff));
    S.set s (pos+2) (Char.chr ((i lsr 40) land 0xff));
    S.set s (pos+3) (Char.chr ((i lsr 32) land 0xff));
    S.set s (pos+4) (Char.chr ((i lsr 24) land 0xff));
    S.set s (pos+5) (Char.chr ((i lsr 16) land 0xff));
    S.set s (pos+6) (Char.chr ((i lsr 8 ) land 0xff));
    S.set s (pos+7) (Char.chr ( i         land 0xff))
*)

  let led s pos f =
    let b = Int64.bits_of_float f in
    S.set s (pos+7) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 56) 0xffL)));
    S.set s (pos+6) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 48) 0xffL)));
    S.set s (pos+5) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 40) 0xffL)));
    S.set s (pos+4) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 32) 0xffL)));
    S.set s (pos+3) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 24) 0xffL)));
    S.set s (pos+2) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 16) 0xffL)));
    S.set s (pos+1) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 8 ) 0xffL)));
    S.set s (pos+0) (Char.chr (Int64.to_int (Int64.logand (                          b   ) 0xffL)))

  let bed s pos f =
    let b = Int64.bits_of_float f in
    S.set s (pos+0) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 56) 0xffL)));
    S.set s (pos+1) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 48) 0xffL)));
    S.set s (pos+2) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 40) 0xffL)));
    S.set s (pos+3) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 32) 0xffL)));
    S.set s (pos+4) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 24) 0xffL)));
    S.set s (pos+5) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 16) 0xffL)));
    S.set s (pos+6) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 8 ) 0xffL)));
    S.set s (pos+7) (Char.chr (Int64.to_int (Int64.logand (                          b   ) 0xffL)))

  let lei32l s pos i32 =
    S.set s (pos+3) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i32 24) 0xffl)));
    S.set s (pos+2) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i32 16) 0xffl)));
    S.set s (pos+1) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i32 8 ) 0xffl)));
    S.set s (pos+0) (Char.chr (Int32.to_int (Int32.logand (                          i32   ) 0xffl)))

  let bei32l s pos i32 =
    S.set s (pos+0) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i32 24) 0xffl)));
    S.set s (pos+1) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i32 16) 0xffl)));
    S.set s (pos+2) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i32 8 ) 0xffl)));
    S.set s (pos+3) (Char.chr (Int32.to_int (Int32.logand (                          i32   ) 0xffl)))

  let lei64L s pos i64 =
    S.set s (pos+7) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 56) 0xffL)));
    S.set s (pos+6) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 48) 0xffL)));
    S.set s (pos+5) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 40) 0xffL)));
    S.set s (pos+4) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 32) 0xffL)));
    S.set s (pos+3) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 24) 0xffL)));
    S.set s (pos+2) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 16) 0xffL)));
    S.set s (pos+1) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 8 ) 0xffL)));
    S.set s (pos+0) (Char.chr (Int64.to_int (Int64.logand (                          i64   ) 0xffL)))

  let bei64L s pos i64 =
    S.set s (pos+0) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 56) 0xffL)));
    S.set s (pos+1) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 48) 0xffL)));
    S.set s (pos+2) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 40) 0xffL)));
    S.set s (pos+3) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 32) 0xffL)));
    S.set s (pos+4) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 24) 0xffL)));
    S.set s (pos+5) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 16) 0xffL)));
    S.set s (pos+6) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 8 ) 0xffL)));
    S.set s (pos+7) (Char.chr (Int64.to_int (Int64.logand (                          i64   ) 0xffL)))

(*
  let ldi32 s i =
    (((Char.code (S.get (s) (i+3))) lsl 24) land 0xff000000) lor
    (((Char.code (S.get (s) (i+2))) lsl 16) land 0x00ff0000) lor
    (((Char.code (S.get (s) (i+1))) lsl  8) land 0x0000ff00) lor
    (((Char.code (S.get (s) (i+0)))       ) land 0x000000ff)

  let bdi32 s i =
    (((Char.code (S.get (s) (i+0))) lsl 24) land 0xff000000) lor
    (((Char.code (S.get (s) (i+1))) lsl 16) land 0x00ff0000) lor
    (((Char.code (S.get (s) (i+2))) lsl  8) land 0x0000ff00) lor
    (((Char.code (S.get (s) (i+3)))       ) land 0x000000ff)
*)

(*
  let ldi64 s i =
    (((Char.code (S.get (s) (i+7))) lsl 56) land 0x7f00000000000000) lor
    (((Char.code (S.get (s) (i+6))) lsl 48) land 0x00ff000000000000) lor
    (((Char.code (S.get (s) (i+5))) lsl 40) land 0x0000ff0000000000) lor
    (((Char.code (S.get (s) (i+4))) lsl 32) land 0x000000ff00000000) lor
    (((Char.code (S.get (s) (i+3))) lsl 24) land 0x00000000ff000000) lor
    (((Char.code (S.get (s) (i+2))) lsl 16) land 0x0000000000ff0000) lor
    (((Char.code (S.get (s) (i+1))) lsl  8) land 0x000000000000ff00) lor
    (((Char.code (S.get (s) (i+0)))       ) land 0x00000000000000ff)


  let bdi64 s i =
    (((Char.code (S.get (s) (i+0))) lsl 56) land 0x7f00000000000000) lor
    (((Char.code (S.get (s) (i+1))) lsl 48) land 0x00ff000000000000) lor
    (((Char.code (S.get (s) (i+2))) lsl 40) land 0x0000ff0000000000) lor
    (((Char.code (S.get (s) (i+3))) lsl 32) land 0x000000ff00000000) lor
    (((Char.code (S.get (s) (i+4))) lsl 24) land 0x00000000ff000000) lor
    (((Char.code (S.get (s) (i+5))) lsl 16) land 0x0000000000ff0000) lor
    (((Char.code (S.get (s) (i+6))) lsl  8) land 0x000000000000ff00) lor
    (((Char.code (S.get (s) (i+7)))       ) land 0x00000000000000ff)
*)

  let ldi32l s i =
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (S.get (s) (i+3)))) 24) 0x00000000ff000000l)
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (S.get (s) (i+2)))) 16) 0x0000000000ff0000l)
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (S.get (s) (i+1))))  8) 0x000000000000ff00l)
                 (Int32.logand (                 (Int32.of_int (Char.code (S.get (s) (i+0))))   ) 0x00000000000000ffl))))

  let bdi32l s i =
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (S.get (s) (i+0)))) 24) 0x00000000ff000000l)
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (S.get (s) (i+1)))) 16) 0x0000000000ff0000l)
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (S.get (s) (i+2))))  8) 0x000000000000ff00l)
                 (Int32.logand (                 (Int32.of_int (Char.code (S.get (s) (i+3))))   ) 0x00000000000000ffl))))

  let ldi32 s i = Int32.to_int(ldi32l s i)
  let bdi32 s i = Int32.to_int(bdi32l s i)

  let ldi64L s i =
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (S.get (s) (i+7)))) 56) 0xff00000000000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (S.get (s) (i+6)))) 48) 0x00ff000000000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (S.get (s) (i+5)))) 40) 0x0000ff0000000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (S.get (s) (i+4)))) 32) 0x000000ff00000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (S.get (s) (i+3)))) 24) 0x00000000ff000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (S.get (s) (i+2)))) 16) 0x0000000000ff0000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (S.get (s) (i+1))))  8) 0x000000000000ff00L)
                 (Int64.logand (                 (Int64.of_int (Char.code (S.get (s) (i+0))))   ) 0x00000000000000ffL))))))))

  let bdi64L s i =
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (S.get (s) (i+0)))) 56) 0xff00000000000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (S.get (s) (i+1)))) 48) 0x00ff000000000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (S.get (s) (i+2)))) 40) 0x0000ff0000000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (S.get (s) (i+3)))) 32) 0x000000ff00000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (S.get (s) (i+4)))) 24) 0x00000000ff000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (S.get (s) (i+5)))) 16) 0x0000000000ff0000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (S.get (s) (i+6))))  8) 0x000000000000ff00L)
                 (Int64.logand (                 (Int64.of_int (Char.code (S.get (s) (i+7))))   ) 0x00000000000000ffL))))))))

  let ldd s i = Int64.float_of_bits (ldi64L s i)
  let bdd s i = Int64.float_of_bits (bdi64L s i)

end (* module StuffF *)

module StuffString = StuffF(String)

let add_le_int32 b i =
  if Buf.spare b <= 4 then raise (Failure "add_le_int32");
  StuffString.lei32 b.Buf.str b.Buf.i i;
  b.Buf.i <- b.Buf.i + 4

let add_be_int32 b i =
  if Buf.spare b <= 4 then raise (Failure "add_be_int32");
  StuffString.bei32 b.Buf.str b.Buf.i i;
  b.Buf.i <- b.Buf.i + 4

(*
let add_le_int64 b i =
  if Buf.spare b <= 8 then raise (Failure "add_le_int64");
  StuffString.lei64 b.Buf.str b.Buf.i i;
  b.Buf.i <- b.Buf.i + 8

let add_be_int64 b i =
  if Buf.spare b <= 8 then raise (Failure "add_be_int64");
  StuffString.bei64 b.Buf.str b.Buf.i i;
  b.Buf.i <- b.Buf.i + 8
*)

let add_le_d b i =
  if Buf.spare b <= 8 then raise (Failure "add_le_d");
  StuffString.led b.Buf.str b.Buf.i i;
  b.Buf.i <- b.Buf.i + 8

let add_be_d b i =
  if Buf.spare b <= 8 then raise (Failure "add_be_d");
  StuffString.bed b.Buf.str b.Buf.i i;
  b.Buf.i <- b.Buf.i + 8

let add_le_int32l b i =
  if Buf.spare b <= 4 then raise (Failure "add_le_i32l");
  StuffString.lei32l b.Buf.str b.Buf.i i;
  b.Buf.i <- b.Buf.i + 4

let add_be_int32l b i =
  if Buf.spare b <= 4 then raise (Failure "add_be_i32l");
  StuffString.bei32l b.Buf.str b.Buf.i i;
  b.Buf.i <- b.Buf.i + 4

let add_le_int64L b i =
  if Buf.spare b <= 4 then raise (Failure "add_le_i64L");
  StuffString.lei64L b.Buf.str b.Buf.i i;
  b.Buf.i <- b.Buf.i + 8

let add_be_int64L b i =
  if Buf.spare b <= 4 then raise (Failure "add_be_i64L");
  StuffString.bei64L b.Buf.str b.Buf.i i;
  b.Buf.i <- b.Buf.i + 8

