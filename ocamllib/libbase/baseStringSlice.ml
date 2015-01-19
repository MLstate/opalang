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


type slice = { str : string; mutable base : int; mutable len : int; }

let empty = { str=""; base=0; len=0; }

let length s = s.len

let get s i = Bytes.get s.str (s.base+i)

let set s i ch = Bytes.set s.str (s.base+i) ch

let create len = { str=Bytes.create len; base=0; len=len; }

let unsafe_get s i = Bytes.unsafe_get s.str (s.base+i)

let unsafe_set s i ch = Bytes.unsafe_set s.str (s.base+i) ch

let unsafe_blit s soff d doff len = Bytes.unsafe_blit s.str (s.base+soff) d.str (d.base+doff) len

let unsafe_fill s off len ch = Bytes.unsafe_fill s.str (s.base+off) len ch

let make n c =
  let s = create n in
  unsafe_fill s 0 n c;
  s

let copy s =
  let len = length s in
  let r = create len in
  unsafe_blit s 0 r 0 len;
  r

let unsafe_sub s ofs len =
  { str=s.str; base=s.base+ofs; len; }

let sub s ofs len =
  if ofs < 0 || len < 0 || ofs > length s - len
  then invalid_arg "BaseStringSlice.sub"
  else unsafe_sub s ofs len

let fill s ofs len c =
  if ofs < 0 || len < 0 || ofs > length s - len
  then invalid_arg "BaseStringSlice.fill"
  else unsafe_fill s ofs len c

let blit s1 ofs1 s2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > length s1 - len
             || ofs2 < 0 || ofs2 > length s2 - len
  then invalid_arg "Bytes.blit"
  else unsafe_blit s1 ofs1 s2 ofs2 len

let iter f a =
  for i = 0 to length a - 1 do f(unsafe_get a i) done

let concat sep l =
  match l with
    [] -> empty
  | hd :: tl ->
      let num = ref 0 and len = ref 0 in
      List.iter (fun s -> incr num; len := !len + length s) l;
      let r = create (!len + length sep * (!num - 1)) in
      unsafe_blit hd 0 r 0 (length hd);
      let pos = ref(length hd) in
      List.iter
        (fun s ->
          unsafe_blit sep 0 r !pos (length sep);
          pos := !pos + length sep;
          unsafe_blit s 0 r !pos (length s);
          pos := !pos + length s)
        tl;
      r

external is_printable: char -> bool = "caml_is_printable"
external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

let escaped s =
  let n = ref 0 in
    for i = 0 to length s - 1 do
      n := !n +
        (match unsafe_get s i with
         | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
         | c -> if is_printable c then 1 else 4)
    done;
    if !n = length s then s else begin
      let s' = create !n in
        n := 0;
        for i = 0 to length s - 1 do
          begin
            match unsafe_get s i with
            | ('"' | '\\') as c ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
            | '\n' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
            | '\t' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 't'
            | '\r' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'r'
            | '\b' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'b'
            | c ->
                if is_printable c then
                  unsafe_set s' !n c
                else begin
                  let a = char_code c in
                  unsafe_set s' !n '\\';
                  incr n;
                  unsafe_set s' !n (char_chr (48 + a / 100));
                  incr n;
                  unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
                  incr n;
                  unsafe_set s' !n (char_chr (48 + a mod 10))
                end
          end;
          incr n
        done;
        s'
      end

let map f s =
  let l = length s in
  if l = 0 then s else begin
    let r = create l in
    for i = 0 to l - 1 do unsafe_set r i (f(unsafe_get s i)) done;
    r
  end

let uppercase s = map Char.uppercase s
let lowercase s = map Char.lowercase s

let apply1 f s =
  if length s = 0 then s else begin
    let r = copy s in
    unsafe_set r 0 (f(unsafe_get s 0));
    r
  end

let capitalize s = apply1 Char.uppercase s
let uncapitalize s = apply1 Char.lowercase s

let rec index_rec s lim i c =
  if i >= lim then raise Not_found else
  if unsafe_get s i = c then i else index_rec s lim (i + 1) c;;

let index s c = index_rec s (length s) 0 c;;

let index_from s i c =
  let l = length s in
  if i < 0 || i > l then invalid_arg "Bytes.index_from" else
  index_rec s l i c;;

let rec rindex_rec s i c =
  if i < 0 then raise Not_found else
  if unsafe_get s i = c then i else rindex_rec s (i - 1) c;;

let rindex s c = rindex_rec s (length s - 1) c;;

let rindex_from s i c =
  if i < -1 || i >= length s then invalid_arg "Bytes.rindex_from" else
  rindex_rec s i c;;

let contains_from s i c =
  let l = length s in
  if i < 0 || i > l then invalid_arg "Bytes.contains_from" else
  try ignore (index_rec s l i c); true with Not_found -> false;;

let contains s c = contains_from s 0 c;;

let rcontains_from s i c =
  if i < 0 || i >= length s then invalid_arg "Bytes.rcontains_from" else
  try ignore (rindex_rec s i c); true with Not_found -> false;;

type t = slice

let compare s1 s2 =
  match Pervasives.compare s1.len s2.len with
  | 0 ->
      let rec aux n =
        if n >= s1.len
        then 0
        else
          match Pervasives.compare s1.str.[s1.base+n] s2.str.[s1.base+n] with
          | 0 -> aux (n+1)
          | n -> n
      in
      aux 0
  | n -> n

(* ---Specials--- *)

let of_string str = { str; base=0; len=Bytes.length str; }

let to_string s = Bytes.sub s.str s.base s.len

let export s = (s.str,s.base,s.len)

let import (str,base,len) = { str; base; len; }

let widen s = s.base <- 0; s.len <- Bytes.length s.str

let normalize s = { str=to_string s; base=0; len=s.len; }

let real_size s = Bytes.length s.str

let set_size s len =
  let str = Bytes.create len in
  Bytes.unsafe_blit s.str s.base str 0 (min s.len len);
  { str; base=0; len=len; }

let rebase s =
  if s.base <> 0
  then (Bytes.unsafe_blit s.str s.base s.str 0 s.len;
        s.base <- 0)
