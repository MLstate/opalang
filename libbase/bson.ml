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

module type S_sig =
sig
  type t
  val empty : t
  val length : t -> int
  val get : t -> int -> char
  val set : t -> int -> char -> unit
  val create : int -> t
  val make : int -> char -> t
  val copy : t -> t
  val sub : t -> int -> int -> t
  val fill : t -> int -> int -> char -> unit
  val blit : t -> int -> t -> int -> int -> unit
  val concat : t -> t list -> t
  val iter : (char -> unit) -> t -> unit
  val escaped : t -> t
  val index : t -> char -> int
  val rindex : t -> char -> int
  val index_from : t -> int -> char -> int
  val rindex_from : t -> int -> char -> int
  val contains : t -> char -> bool
  val contains_from : t -> int -> char -> bool
  val rcontains_from : t -> int -> char -> bool
  val uppercase : t -> t
  val lowercase : t -> t
  val capitalize : t -> t
  val uncapitalize : t -> t
  val compare : t -> t -> int
  val unsafe_get : t -> int -> char
  val unsafe_set : t -> int -> char -> unit
  val unsafe_blit : t -> int -> t -> int -> int -> unit
  val unsafe_fill : t -> int -> int -> char -> unit
  val to_string : t -> string
  val of_string : string -> t
  val export : t -> string * int * int
  val import : string * int * int -> t
  val widen : t -> unit
  val normalize : t -> t
  val real_size : t -> int
  val set_size : t -> int -> t
  val rebase : t -> unit
  val unsafe_sub : t -> int -> int -> t
end

module S : S_sig with type t = string =
struct
  include BaseString
  let empty = ""
  let to_string s = s
  let of_string s = s
  let export s = (s,0,String.length s)
  let import (str,base,len) = String.sub str base len
  let widen _ = ()
  let normalize s = s
  let real_size s = String.length s
  let set_size s _ = s
  let rebase _ = ()
  let unsafe_sub = String.sub
end
module St = Stuff.StuffF(S)
module SS = BaseStringSlice
let sprintf = Printf.sprintf
let fprintf = Printf.fprintf

(* Element codes *)
let el_eoo = '\x00'        (*                  End of object? *)
let el_double = '\x01'     (* double           Floating point *)
let el_string = '\x02'     (* string           UTF-8 string *)
let el_object = '\x03'     (* document         Embedded document *)
let el_array = '\x04'      (* document         Array *)
let el_bindata = '\x05'    (* binary           Binary data *)
let el_undefined = '\x06'  (*                  Undefined — Deprecated *)
let el_oid = '\x07'        (* (byte*12)        ObjectId *)
let el_bool = '\x08'       (* "\x00"           Boolean "false"
                              "\x01"           Boolean "true" *)
let el_date = '\x09'       (* int64            UTC datetime *)
let el_null = '\x0A'       (*                  Null value *)
let el_regex = '\x0B'      (* cstring cstring  Regular expression *)
let el_dbref = '\x0C'      (* string (byte*12) DBPointer — Deprecated *)
let el_code = '\x0D'       (* string           JavaScript code *)
let el_symbol = '\x0E'     (* string           Symbol *)
let el_codewscope = '\x0F' (* code_w_s         JavaScript code w/ scope *)
let el_int = '\x10'        (* int32            32-bit Integer *)
let el_timestamp = '\x11'  (* int64            Timestamp *)
let el_long = '\x12'       (* int64            64-bit integer *)
let el_minkey = '\xFF'     (*                  Min key *)
let el_maxkey = '\x7F'     (*                  Max key *)

(* Binary subtype *)
let st_bin_binary = '\x00'
let st_bin_func = '\x01'
let st_bin_binary_old = '\x02'
let st_bin_uuid = '\x03'
let st_bin_md5 = '\x05'
let st_bin_user = '\x80'

(* Types *)

type buf =
    { buf: Buf.buf;
      mutable stack : int list;
      mutable finished : bool;
    }

(* Object identifiers *)

module Oid =
struct

  let from_string str =
    let oid = S.create 12 in
    for i = 0 to 11 do
      oid.[i] <- Char.chr (Charf.c2h str.[i*2] str.[i*2+1]);
    done;
    oid

  let to_string oid =
    let hex = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'a';'b';'c';'d';'e';'f'|] in
    let str = S.create 24 in
    for i = 0 to 11 do
      str.[2*i] <- hex.(((Char.code oid.[i]) land 0xf0) lsr 4);
      str.[2*i+1] <- hex.((Char.code oid.[i]) land 0x0f);
    done;
    str

  let counter = ref 0
  let gen () =
    let s = S.create 12 in
    St.bei32 s 0 (int_of_float (Time.in_seconds(Time.now())));
    St.lei32l s 4 (Random.int32 Int32.max_int);
    St.bei32 s 8 (incr counter; !counter);
    s

  let generated_time oid =
    Time.seconds (St.bdi32 oid 0)

end (* module Oid *)

module Append =
struct

  let init ?(hint=100) () : buf =
    if hint < 5 then raise (Failure "init: ridiculous hint value");
    let b = { buf=Buf.create hint; stack=[]; finished=false; } in
    b.buf.Buf.i <- 4;
    b

  let empty =
    let b = { buf = Buf.of_string "\x05\x00\x00\x00\x00";
              stack = [];
              finished = true;
            } in
    b.buf.Buf.i <- 4;
    b

  let size b =
    if not b.finished
    then 0
    else St.ldi32 b.buf.Buf.str 0

  let estart b _type name =
    Buf.add_char b.buf _type;
    Buf.add_string b.buf name;
    Buf.add_char b.buf '\x00'

  let int b name i =
    estart b el_int name;
    Stuff.add_le_int32 b.buf i

  let long b name l =
    estart b el_long name;
    Stuff.add_le_int64 b.buf l

  let double b name d =
    estart b el_double name;
    Stuff.add_le_d b.buf d

  let bool b name _b =
    estart b el_bool name;
    Buf.add_char b.buf (if _b then '\x01' else '\x00')

  let null b name =
    estart b el_null name

  let undefined b name =
    estart b el_undefined name

  let string_base b name value len _type =
    estart b _type name;
    Stuff.add_le_int32 b.buf (len+1);
    Buf.append b.buf value len;
    Buf.add_char b.buf '\x00'

  let string b name value =
    string_base b name value (S.length value) el_string

  let symbol b name value =
    string_base b name value (S.length value) el_symbol

  let code b name value =
    string_base b name value (S.length value) el_code

  let string_n b name value len =
    string_base b name value len el_string

  let symbol_n b name value len =
    string_base b name value len el_symbol

  let code_n b name value len =
    string_base b name value len el_code

  let code_w_scope_n b name code len scope =
    let slen = len + 1 in
    let ssize = size scope in
    let size = slen + ssize + 8 in
    estart b el_codewscope name;
    Stuff.add_le_int32 b.buf size;
    Stuff.add_le_int32 b.buf slen;
    Buf.append b.buf code len;
    Buf.add_char b.buf '\x00';
    Buf.append b.buf scope.buf.Buf.str ssize

  let start_codewscope b name code =
    let len = S.length code in
    estart b el_codewscope name;
    b.stack <- b.buf.Buf.i :: b.stack;
    Stuff.add_le_int32 b.buf 0;
    Stuff.add_le_int32 b.buf (len+1);
    Buf.append b.buf code len;
    Buf.add_char b.buf '\x00';
    b.stack <- b.buf.Buf.i :: b.stack;
    Stuff.add_le_int32 b.buf 0

  let finish_codewscope b code =
    Buf.add_char b.buf '\x00';
    let len = S.length code in
    let start = List.hd b.stack in
    b.stack <- List.tl b.stack;
    let ssize = b.buf.Buf.i - start in
    St.lei32 b.buf.Buf.str start ssize;
    let size = len + ssize + 9 in
    let start = List.hd b.stack in
    b.stack <- List.tl b.stack;
    St.lei32 b.buf.Buf.str start size

  let code_w_scope b name code scope =
    code_w_scope_n b name code (S.length code) scope

  let binary b name _type str len =
    if _type = st_bin_binary_old
    then (estart b el_bindata name;
          Stuff.add_le_int32 b.buf (len+4);
          Buf.add_char b.buf _type;
          Stuff.add_le_int32 b.buf len;
          Buf.append b.buf str len)
    else (estart b el_bindata name;
          Stuff.add_le_int32 b.buf len;
          Buf.add_char b.buf _type;
          Buf.append b.buf str len)

  let oid b name oid =
    estart b el_oid name;
    Buf.append b.buf oid 12

  let new_oid b name =
    oid b name (Oid.gen ())

  let regex b name pattern opts =
    let plen = S.length pattern in
    let olen = S.length opts in
    estart b el_regex name;
    Buf.append b.buf pattern plen;
    Buf.add_char b.buf '\x00';
    Buf.append b.buf opts olen;
    Buf.add_char b.buf '\x00'

  let bson b name bson =
    let bsize = size bson in
    estart b el_object name;
    Buf.append b.buf bson.buf.Buf.str bsize

  let timestamp b name (i,t) =
    estart b el_timestamp name;
    Stuff.add_le_int32 b.buf i;
    Stuff.add_le_int32 b.buf t

  let date b name millis =
    estart b el_date name;
    Stuff.add_le_int64 b.buf millis

  let time_t b name t =
    date b name (Time.in_milliseconds t)

  let start_object b name =
    estart b el_object name;
    b.stack <- b.buf.Buf.i :: b.stack;
    Stuff.add_le_int32 b.buf 0

  let start_array b name =
    estart b el_array name;
    b.stack <- b.buf.Buf.i :: b.stack;
    Stuff.add_le_int32 b.buf 0

  let finish_object b =
    Buf.add_char b.buf '\x00';
    let start = List.hd b.stack in
    b.stack <- List.tl b.stack;
    St.lei32 b.buf.Buf.str start (b.buf.Buf.i - start)

  let finish_array b =
    finish_object b

  let finish b =
    if not b.finished
    then (Buf.add_char b.buf '\x00';
          St.lei32 b.buf.Buf.str 0 b.buf.Buf.i;
          b.finished <- true)

  let get b =
    if not b.finished then raise (Failure "get: not finished");
    S.sub b.buf.Buf.str 0 b.buf.Buf.i

end (* module Append *)

module type Iterator_sig =
sig
  module S : S_sig
  type iter = { ibuf : S.t; mutable pos : int; mutable first : bool; }
  val init : buf -> iter
  val from_buffer : S.t -> iter
  val iterator_type : iter -> char
  val key : iter -> string
  val value : iter -> int
  val int_raw : iter -> int
  val long_raw : iter -> int
  val double_raw : iter -> float
  val bool_raw : iter -> bool
  val oid : iter -> string
  val string : ?offset:int -> iter -> string
  val symbol : ?offset:int -> iter -> string
  val cstring : ?offset:int -> iter -> string
  val string_len : iter -> int
  val int : iter -> int
  val long : iter -> int
  val double : iter -> float
  val timestamp : iter -> int * int
  val bool : iter -> bool
  val code : iter -> string
  val code_scope : iter -> buf
  val date : iter -> int
  val time_t : iter -> Time.t
  val bin_type : iter -> char
  val bin_len : iter -> int
  val bin_data : iter -> string
  val regex : iter -> string
  val regex_opts : iter -> string
  val subobject : iter -> buf
  val subiterator : iter -> iter
  val next : iter -> char
  val find : buf -> string -> iter * char
end

module IteratorF(S : S_sig) : Iterator_sig with module S = S =
struct

  module S = S
  module St = Stuff.StuffF(S)

  type iter =
      { ibuf : S.t;
        mutable pos : int;
        mutable first : bool;
      }

  let init b =
    { ibuf = S.of_string b.buf.Buf.str;
      pos = 4;
      first = true;
    }

  let from_buffer buffer =
    { ibuf = buffer;
      pos = 4;
      first = true;
    }

  let iterator_type i = S.get i.ibuf i.pos

  let key i =
    try
      let ks = i.pos + 1 in
      S.to_string (S.sub i.ibuf ks ((S.index_from i.ibuf ks '\x00') - ks))
    with Not_found -> ""

  let value i =
    try (S.index_from i.ibuf (i.pos+1) '\x00') + 1
    with Not_found -> S.length i.ibuf

  let int_raw i =
    St.ldi32 i.ibuf (value i)

  let long_raw i =
    St.ldi64 i.ibuf (value i)

  let double_raw i =
    St.ldd i.ibuf (value i)

  let bool_raw i =
    match S.get i.ibuf (value i) with
    | '\x00' -> false
    | '\x01' -> true
    | c -> raise (Failure (sprintf "iterator_bool_raw: Unknown code %02x" (Char.code c)))

  let oid i =
    S.to_string (S.sub i.ibuf (value i) 12)

  let string ?(offset=4) i =
    let v = value i in
    S.to_string (S.sub i.ibuf (v+offset) ((St.ldi32 i.ibuf v)-1))

  let symbol = string

  let bslen s i =
    try (S.index_from s i '\x00') + 1 - i
    with Not_found -> S.length s - i

  let cstring ?(offset=0) i =
    let v = value i in
    S.to_string (S.sub i.ibuf (v+offset) ((bslen i.ibuf (v+offset))-1))

  let string_len i =
    int_raw i - 1

  let int i =
    match S.get i.ibuf (i.pos) with
    | c when c = el_int -> int_raw i
    | c when c = el_long -> long_raw i
    | c when c = el_double -> int_of_float (double_raw i)
    | _ -> 0

  let long = int

  let double i =
    match S.get i.ibuf (i.pos) with
    | c when c = el_int -> float_of_int (int_raw i)
    | c when c = el_long -> float_of_int (long_raw i)
    | c when c = el_double -> double_raw i
    | _ -> 0.0

  let timestamp i =
    let v = value i in
    (St.ldi32 i.ibuf v, St.ldi32 i.ibuf (v+4))

  let bool i =
    match S.get i.ibuf (i.pos) with
    | c when c = el_bool -> bool_raw i
    | c when c = el_int -> int_raw i <> 0
    | c when c = el_long -> long_raw i <> 0
    | c when c = el_double -> double_raw i <> 0.0
    | c when c = el_eoo || c = el_null -> false
    | _ -> true

  let code i =
    match S.get i.ibuf i.pos with
    | c when c = el_string || c = el_code -> string ~offset:4 i
    | c when c = el_codewscope ->
        let v = value i in
        S.to_string (S.sub i.ibuf (v+8) ((St.ldi32 i.ibuf (v+4))-1))
    | _ -> ""

  let code_scope i =
    match S.get i.ibuf i.pos with
    | c when c = el_codewscope ->
        let v = value i in
        let code_len = St.ldi32 i.ibuf (v+4) in
        let scope_len = St.ldi32 i.ibuf (v+8+code_len) in
        let b = { buf = Buf.of_string (S.to_string (S.sub i.ibuf (v+8+code_len) scope_len));
                  stack = [];
                  finished = true;
                } in
        b.buf.Buf.i <- 4;
        b
    | _ -> Append.empty

  let date i =
    long_raw i

  let time_t i =
    Time.milliseconds (date i)

  let bin_type i =
    S.get i.ibuf (value i + 4)

  let bin_len i =
    if bin_type i = st_bin_binary_old
    then int_raw i - 4
    else int_raw i

  let bin_data i =
    let v = value i in
    let offset = if bin_type i = st_bin_binary_old then 9 else 5 in
    S.to_string (S.sub i.ibuf (v+offset) (bin_len i))

  let regex i =
    cstring i

  let regex_opts i =
    let v = value i in
    let offset = bslen i.ibuf v in
    S.to_string (S.sub i.ibuf (v+offset) ((bslen i.ibuf (v+offset))-1))

  let subobject i =
    let v = value i in
    let len = St.ldi32 i.ibuf v in
    let b = { buf = Buf.of_string (S.to_string (S.sub i.ibuf v len));
              stack = [];
              finished = true;
            } in
    b.buf.Buf.i <- 4;
    b

  let subiterator i =
    { ibuf = i.ibuf;
      pos = value i + 4;
      first = true;
    }

  let next i =
    if i.first || S.get i.ibuf i.pos = el_eoo
    then (i.first <- false; S.get i.ibuf i.pos)
    else
      let ds =
        match S.get i.ibuf i.pos with
        | c when c = el_undefined || c = el_null ->
            0
        | c when c = el_bool ->
            1
        | c when c = el_int ->
            4
        | c when c = el_long || c = el_double || c = el_timestamp || c = el_date ->
            8
        | c when c = el_oid ->
            12
        | c when c = el_string || c = el_symbol || c = el_code ->
            4 + int_raw i
        | c when c = el_bindata ->
            5 + int_raw i
        | c when c = el_object || c = el_array || c = el_codewscope ->
            int_raw i
        | c when c = el_dbref ->
            16 + int_raw i
        | c when c = el_regex ->
            let s = value i in
            let p = (S.index_from i.ibuf s '\x00') + 1 in
            let p = (S.index_from i.ibuf p '\x00') + 1 in
            p-s
        | c -> raise (Failure (sprintf "next: Unknown code %02x" (Char.code c)))
      in
      i.pos <- i.pos + (1 + (bslen i.ibuf (i.pos+1)) + ds);
      S.get i.ibuf i.pos

  let find obj name =
    let i = init obj in
    let rec aux () =
      let c = next i in
      if c = el_eoo
      then i, el_eoo
      else
        if name = key i
        then i, c
        else aux ()
    in
    aux ()

end (* module Iterator *)

module Iterator : Iterator_sig with module S = S = IteratorF(S)
module IteratorSS : Iterator_sig with module S = SS = IteratorF(SS)

(* Module dependencies, we need Iterator, even though it's an Append function *)
(*
module Element =
struct

  let element b name_opt elem =
    let next = { Iterator.ibuf = elem.Iterator.ibuf; pos = elem.Iterator.pos; first = elem.Iterator.first } in
    ignore (Iterator.next next);
    let size = next.Iterator.pos - elem.Iterator.pos in
    match name_opt with
    | Some name ->
        let data_size = size - S.length (Iterator.key elem) - 2 in
        Append.estart b (S.get elem.Iterator.ibuf 0) name data_size;
        Append.append b ~offset:(Iterator.value elem) name data_size
    | None ->
        Append.append b (S.to_string elem.Iterator.ibuf) size

end (* module Element *)
*)

module Print =
struct

  let rec print b =
    print_raw b.buf.Buf.str 0 0

  and print_raw str i depth =
    let i = { Iterator.ibuf = S.of_string str; pos = i+4; first = true; } in
    let rec aux () =
      ignore (Iterator.next i);
      let t = Iterator.iterator_type i in
      if t <> el_eoo
      then
        let key = Iterator.key i in
        Printf.printf "%s" (S.make depth '\t');
        Printf.printf "%s : %02x \t" key (Char.code t);
        (match t with
         | c when c = el_double -> Printf.printf "%f" (Iterator.double i)
         | c when c = el_string -> Printf.printf "%s" (Iterator.string i)
         | c when c = el_symbol -> Printf.printf "SYMBOL: %s" (Iterator.string i)
         | c when c = el_oid -> Printf.printf "%s" (Oid.to_string (Iterator.oid i))
         | c when c = el_bool -> Printf.printf "%b" (Iterator.bool i)
         | c when c = el_date -> Printf.printf "%d" (Iterator.date i)
         | c when c = el_bindata -> Printf.printf "el_bindata"
         | c when c = el_undefined -> Printf.printf "el_undefined"
         | c when c = el_null -> Printf.printf "el_null"
         | c when c = el_regex -> Printf.printf "el_regex: %s" (Iterator.regex i)
         | c when c = el_code -> Printf.printf "el_code: %s" (Iterator.code i)
         | c when c = el_codewscope ->
             Printf.printf "el_code_w_scope: %s" (Iterator.code i);
             let scope = Iterator.code_scope i in
             Printf.printf "\n\t SCOPE: ";
             print scope
         | c when c = el_int -> Printf.printf "%d" (Iterator.int i)
         | c when c = el_long -> Printf.printf "%d" (Iterator.long i)
         | c when c = el_timestamp ->
             let (i,t) = Iterator.timestamp i in
             Printf.printf "i: %d, t: %d" i t
         | c when c = el_object || c = el_array ->
             Printf.printf "\n";
             print_raw i.Iterator.ibuf (Iterator.value i) (depth + 1)
         | _ ->
             Printf.eprintf "can't print type : %d\n%!" (Char.code t));
        Printf.printf "\n";
        aux ()
    in
    aux ()

end (* module Print *)

(*
(* Test code *)

let hex s =
  let len = S.length s in
  let hs = S.create (len * 3) in
  let pos = ref 0 in
  for i = 0 to len - 1 do
    let code = Char.code s.[i] in
    let ss =
      if code >= 32 && code < 127
      then Printf.sprintf " %c " s.[i]
      else Printf.sprintf "%02x " code
    in
    S.blit ss 0 hs !pos 3;
    pos := !pos + 3
  done;
  S.sub hs 0 !pos;;

let dump ?(base=10) s =
  let bb = Buffer.create 1024 in
  let bh = Buffer.create 1024 in
  let ba = Buffer.create 1024 in
  let len = S.length s in
  let m, n = len / base, len mod base in
  for i = 0 to m do
    let row = i * base in
    for j = 0 to (if i = m then n-1 else base-1) do
      let idx = i * base + j in
      let code = Char.code s.[idx] in
      Printf.bprintf bh "%02x " code;
      Printf.bprintf ba "%c" (if code >= 32 && code < 127 then s.[idx] else '.');
      if j = base-1 || (i = m && j = n-1)
      then
        (if base = 10
         then Printf.bprintf bb "%04d %-30s %-10s\n" row (Buffer.contents bh) (Buffer.contents ba)
         else Printf.bprintf bb "%04x %-48s %-10s\n" row (Buffer.contents bh) (Buffer.contents ba);
         Buffer.clear bh; Buffer.clear ba)
    done
  done;
  Buffer.contents bb;;

let b1 = Append.init ();;
let () = Append.new_oid b1 "_id";;
let () = Append.string b1 "name" "Joe";;
let () = Append.int b1 "age" 33;;
let () = Append.finish b1;;
let s1 = hex (Append.get b1);;
let () = print_string (dump (Append.get b1));;

let fi1, fc1 = Iterator.find b1 "name";;
let fv1 = Iterator.string fi1;;
let fv1l = Iterator.string_len fi1;;
let fi2, fc2 = Iterator.find b1 "age";;
let fv2 = Iterator.int fi2;;
let fv2d = Iterator.double fi2;;
let fv2b = Iterator.bool fi2;;
let good1 = (fc1,fv1,fv1l,fc2,fv2,fv2d,fv2b)
  = ('\002', "Joe", 3, '\016', 33, 33.0, true);;

let i = Iterator.init b1;;
let c1 = Iterator.next i;;
let k1 = Iterator.key i;;
let v1 = Iterator.oid i;;
let c2 = Iterator.next i;;
let k2 = Iterator.key i;;
let v2 = Iterator.string i;;
let c3 = Iterator.next i;;
let k3 = Iterator.key i;;
let v3 = Iterator.int i;;
let good2 = (c1,k1,c2,k2,v2,c3,k3,v3)
  = ('\007', "_id",
     '\002', "name", "Joe",
     '\016', "age", 33);;

let b2 = Append.init ();;
let () = Append.string b2 "hello" "world";;
let () = Append.finish b2;;
let s2 = hex (Append.get b2);;

(* {"BSON": ["awesome", 5.05, 1986]} *)
let b3 = Append.init ();;
let () = Append.start_array b3 "BSON";;
let () = Append.string b3 "0" "awesome";;
let () = Append.double b3 "1" 5.05;;
let () = Append.int b3 "2" 1986;;
let () = Append.finish_array b3;;
let () = Append.finish b3;;
let s3 = hex (Append.get b3);;

let b4 = Append.init ~hint:1024 ();;
let () = Append.start_array b4 "Test";;
let () = Append.int b4 "int" 12345678;;
let () = Append.long b4 "long" 123456789012345;;
let () = Append.double b4 "double" 1234.5678;;
let () = Append.bool b4 "boolt" true;;
let () = Append.bool b4 "boolf" false;;
let () = Append.string b4 "string" "test";;
let () = Append.symbol b4 "symbol" "test";;
let () = Append.code b4 "code" "test";;
let scope = Append.init ();;
let () = Append.string scope "scope" "<--scope-->";;
let () = Append.finish scope;;
let () = Append.code_w_scope b4 "code" "test" scope;;
let () = Append.binary b4 "binary" st_bin_binary "test" 4;;
let () = Append.binary b4 "binary" st_bin_binary_old "test_old" 8;;
let () = Append.regex b4 "regex" "regex_pat" "regex_opts";;
let bson = Append.init ();;
let () = Append.string bson "bson" "bson_text";;
let () = Append.finish bson;;
let () = Append.bson b4 "bson" bson;;
let () = Append.timestamp b4 "timestamp" (1234,123456789);;
let () = Append.date b4 "date" 123456789;;
let () = Append.time_t b4 "time_t" (Time.seconds 1234);;
let () = Append.finish_array b4;;
let () = Append.finish b4;;
let s4 = hex (Append.get b4);;
let s4s = Append.size b4;;
let () = Print.print b4;;
let i4 = Iterator.init b4;;
let c41 = Iterator.next i4;;
let k41 = Iterator.key i4;;
let i4s = Iterator.subiterator i4;;
let c4s1 = Iterator.next i4s;;
let k4s1 = Iterator.key i4s;;
let v4s1 = Iterator.int i4s;;
let c4s2 = Iterator.next i4s;;
let k4s2 = Iterator.key i4s;;
let v4s2 = Iterator.long i4s;;
let c4s3 = Iterator.next i4s;;
let k4s3 = Iterator.key i4s;;
let v4s3 = Iterator.double i4s;;
let c4s4 = Iterator.next i4s;;
let k4s4 = Iterator.key i4s;;
let v4s4 = Iterator.bool i4s;;
let c4s5 = Iterator.next i4s;;
let k4s5 = Iterator.key i4s;;
let v4s5 = Iterator.bool i4s;;
let c4s6 = Iterator.next i4s;;
let k4s6 = Iterator.key i4s;;
let v4s6 = Iterator.string i4s;;
let c4s7 = Iterator.next i4s;;
let k4s7 = Iterator.key i4s;;
let v4s7 = Iterator.symbol i4s;;
let c4s8 = Iterator.next i4s;;
let k4s8 = Iterator.key i4s;;
let v4s8 = Iterator.code i4s;;
let c4s9 = Iterator.next i4s;;
let k4s9 = Iterator.key i4s;;
let v4s9c = Iterator.code i4s;;
let v4s9s = Iterator.code_scope i4s;;
let v4s9si = Iterator.init v4s9s;;
let c4s9s = Iterator.next v4s9si;;
let k4s9s = Iterator.key v4s9si;;
let v4s9s = Iterator.string v4s9si;;
let c4s10 = Iterator.next i4s;;
let k4s10 = Iterator.key i4s;;
let v4s10t = Iterator.bin_type i4s;;
let v4s10l = Iterator.bin_len i4s;;
let v4s10d = Iterator.bin_data i4s;;
let c4s11 = Iterator.next i4s;;
let k4s11 = Iterator.key i4s;;
let v4s11t = Iterator.bin_type i4s;;
let v4s11l = Iterator.bin_len i4s;;
let v4s11d = Iterator.bin_data i4s;;
let c4s12 = Iterator.next i4s;;
let k4s12 = Iterator.key i4s;;
let v4s12r = Iterator.regex i4s;;
let v4s12o = Iterator.regex_opts i4s;;
let c4s13 = Iterator.next i4s;;
let k4s13 = Iterator.key i4s;;
let v4s13o = Iterator.subobject i4s;;
let v4s13i = Iterator.subiterator i4s;;
let c4s13i = Iterator.next v4s13i;;
let k4s13i = Iterator.key v4s13i;;
let v4s13i = Iterator.string v4s13i;;
let c4s14 = Iterator.next i4s;;
let k4s14 = Iterator.key i4s;;
let v4s14 = Iterator.timestamp i4s;;
let c4s15 = Iterator.next i4s;;
let k4s15 = Iterator.key i4s;;
let v4s15 = Iterator.date i4s;;
let c4s16 = Iterator.next i4s;;
let k4s16 = Iterator.key i4s;;
let v4s16 = Time.in_seconds (Iterator.time_t i4s);;
let good3 =
  (c41, k41, c4s1, k4s1, v4s1, c4s2, k4s2, v4s2, c4s3, k4s3, v4s3, c4s4, k4s4, v4s4, c4s5, k4s5, v4s5,
   c4s6, k4s6, v4s6, c4s7, k4s7, v4s7, c4s8, k4s8, v4s8, c4s9, k4s9, v4s9c, v4s9s, c4s9s, k4s9s, v4s9s,
   c4s10, k4s10, v4s10t, v4s10l, v4s10d, c4s11, k4s11, v4s11t, v4s11l, v4s11d, c4s12, k4s12, v4s12r, v4s12o,
   c4s13, k4s13, c4s13i, k4s13i, v4s13i, c4s14, k4s14, v4s14, c4s15, k4s15, v4s15, c4s16, k4s16, v4s16) =
  ('\004', "Test", '\016', "int", 12345678, '\018', "long", 123456789012345,
   '\001', "double", 1234.5678, '\b', "boolt", true, '\b', "boolf", false,
   '\002', "string", "test", '\014', "symbol", "test", '\r', "code", "test",
   '\015', "code", "test", "<--scope-->", '\002', "scope", "<--scope-->",
   '\005', "binary", '\000', 4, "test", '\005', "binary", '\002', 8,
   "test_old", '\011', "regex", "regex_pat", "regex_opts", '\003', "bson",
   '\002', "bson", "bson_text", '\017', "timestamp", (1234, 123456789), '\t',
   "date", 123456789, '\t', "time_t", 1234.);;

let i4ss = IteratorSS.init b4;;
let c41ss = IteratorSS.next i4ss;;
let k41ss = IteratorSS.key i4ss;;
let i4ssi = IteratorSS.subiterator i4ss;;
let c4s1ss = IteratorSS.next i4ssi;;
let k4s1ss = IteratorSS.key i4ssi;;
let v4s1ss = IteratorSS.int i4ssi;;
let good4 =
  (k41ss, c4s1ss, k4s1ss, v4s1ss) =
  ("Test", '\016', "int", 12345678);;

let b5a = Append.init ~hint:100 ();;
let () = Append.code_w_scope b5a "code" "test" scope;;
let () = Append.finish b5a;;
let s5a = hex (Append.get b5a);;
let b5 = Append.init ~hint:100 ();;
let () = Append.start_codewscope b5 "code" "test";;
let () = Append.string b5 "scope" "<--scope-->";;
let () = Append.finish_codewscope b5 "code";;
let () = Append.finish b5;;
let s5 = hex (Append.get b5);;
let i5 = Iterator.init b5;;
let c5 = Iterator.next i5;;
let k5 = Iterator.key i5;;
let v5c = Iterator.code i5;;
let v5s = Iterator.code_scope i5;;
let v5si = Iterator.init v5s;;
let c5si = Iterator.next v5si;;
let k5si = Iterator.key v5si;;
let v5si = Iterator.string v5si;;
let () = Print.print b5;;
let good5 = s5 = s5a;;

let s6 = "\x2c\x00\x00\x00\x04\x61\x72\x72\x61\x79\x00\x20\x00\x00\x00\x01\x64\x6f\x75\x62\x6c\x65\x00\xae\x47\xe1\x7a\x14\xae\xf3\x3f\x10\x69\x6e\x74\x33\x32\x00\x7b\x00\x00\x00\x00\x00";;
let s6 = "\x3b\x00\x00\x00\x04\x61\x72\x72\x61\x79\x00\x2f\x00\x00\x00\x01\x64\x6f\x75\x62\x6c\x65\x00\xae\x47\xe1\x7a\x14\xae\xf3\x3f\x10\x69\x6e\x74\x33\x32\x00\x7b\x00\x00\x00\x12\x69\x6e\x74\x36\x34\x00\xaf\xe2\x01\x00\x00\x00\x00\x00\x00\x00";;
let i6 = Iterator.from_buffer s6;;
let c6 = Iterator.next i6;;
let k6 = Iterator.key i6;;
let i6i = Iterator.subiterator i6;;
let c6s1 = Iterator.next i6i;;
let k6s1 = Iterator.key i6i;;
let v6s1 = Iterator.double i6i;;
let c6s2 = Iterator.next i6i;;
let k6s2 = Iterator.key i6i;;
let v6s2 = Iterator.int i6i;;
let c6s3 = Iterator.next i6i;;
let k6s3 = Iterator.key i6i;;
let v6s3 = Iterator.long i6i;;

let good = List.for_all (fun x -> x) [good1;good2;good3;good4;good5];;

*)
