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
(* Encode_light:
   Slightly faster and slightly more compact than Marshal but without sharing.
*)

module String = Base.String
let sprintf = Printf.sprintf
let fprintf = Printf.fprintf

let ei8 i =
  let s = String.create 1 in
  s.[0] <- Char.chr  (i         land 0xff);
  s

let ei16 i =
  let s = String.create 2 in
  s.[0] <- Char.chr ((i lsr 8 ) land 0xff);
  s.[1] <- Char.chr  (i         land 0xff);
  s

let ei32 i =
  let s = String.create 4 in
  s.[0] <- Char.chr ((i lsr 24) land 0xff);
  s.[1] <- Char.chr ((i lsr 16) land 0xff);
  s.[2] <- Char.chr ((i lsr 8 ) land 0xff);
  s.[3] <- Char.chr  (i         land 0xff);
  s

let ei64 i =
  let s = String.create 8 in
  s.[0] <- Char.chr ((i lsr 56) land 0xff);
  s.[1] <- Char.chr ((i lsr 48) land 0xff);
  s.[2] <- Char.chr ((i lsr 40) land 0xff);
  s.[3] <- Char.chr ((i lsr 32) land 0xff);
  s.[4] <- Char.chr ((i lsr 24) land 0xff);
  s.[5] <- Char.chr ((i lsr 16) land 0xff);
  s.[6] <- Char.chr ((i lsr 8 ) land 0xff);
  s.[7] <- Char.chr  (i         land 0xff);
  s

let ef f =
  let s = String.create 8 in
  let b = Int64.bits_of_float f in
  s.[0] <- Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 56) 0xffL));
  s.[1] <- Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 48) 0xffL));
  s.[2] <- Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 40) 0xffL));
  s.[3] <- Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 32) 0xffL));
  s.[4] <- Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 24) 0xffL));
  s.[5] <- Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 16) 0xffL));
  s.[6] <- Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical b 8 ) 0xffL));
  s.[7] <- Char.chr (Int64.to_int (Int64.logand (                          b   ) 0xffL));
  s

let di8 s i =
  (((Char.code s.[i])         ) land 0xff)

let di16 s i =
  (((Char.code s.[i])   lsl  8) land 0xff00) lor
  (((Char.code s.[i+1])       ) land 0x00ff)

let di32 s i =
  (((Char.code s.[i])   lsl 24) land 0xff000000) lor
  (((Char.code s.[i+1]) lsl 16) land 0x00ff0000) lor
  (((Char.code s.[i+2]) lsl  8) land 0x0000ff00) lor
  (((Char.code s.[i+3])       ) land 0x000000ff)

let di64 s i =
  (((Char.code s.[i])   lsl 56) land 0x7f00000000000000) lor
  (((Char.code s.[i+1]) lsl 48) land 0x00ff000000000000) lor
  (((Char.code s.[i+2]) lsl 40) land 0x0000ff0000000000) lor
  (((Char.code s.[i+3]) lsl 32) land 0x000000ff00000000) lor
  (((Char.code s.[i+4]) lsl 24) land 0x00000000ff000000) lor
  (((Char.code s.[i+5]) lsl 16) land 0x0000000000ff0000) lor
  (((Char.code s.[i+6]) lsl  8) land 0x000000000000ff00) lor
  (((Char.code s.[i+7])       ) land 0x00000000000000ff)

let df s i =
  Int64.float_of_bits
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i]))   56) 0xff00000000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+1])) 48) 0x00ff000000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+2])) 40) 0x0000ff0000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+3])) 32) 0x000000ff00000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+4])) 24) 0x00000000ff000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+5])) 16) 0x0000000000ff0000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+6]))  8) 0x000000000000ff00L)
               (Int64.logand (                 (Int64.of_int (Char.code s.[i+7]))   ) 0x00000000000000ffL))))))))

(*
let tst8  i = i = di8  (ei8  i) 0
let tst16 i = i = di16 (ei16 i) 0
let tst32 i = i = di32 (ei32 i) 0
let tst64 i = i = di64 (ei64 i) 0
let tstf  f = f = dif  (eif  f) 0
*)

let put_len c1 c2 c3 c4 i =
  if i >= 0 && i <= 255 then sprintf "%c%s" c1 (ei8 i)
  else if i >= 0 && i <= 65535 then sprintf "%c%s" c2 (ei16 i)
  else if i >= 0 && i <= 4294967295 then sprintf "%c%s" c3 (ei32 i)
  else sprintf "%c%s" c4 (ei64 i)

let rec encode_key = function
  | Keys.IntKey i -> put_len 'i' 'j' 'I' 'J' i
  | Keys.StringKey s -> (put_len 's' 't' 'S' 'T' (String.length s))^s
  | Keys.ListKey r -> String.concat_map ~left:(put_len 'l' 'm' 'L' 'M' (Array.length r)) "" encode_key (Array.to_list r)
  | Keys.VariableKey i -> put_len 'v' 'w' 'V' 'W' i

let get_len c1 c2 c3 c4 s i c =
  if c = c1
  then i+2, di8 s (i+1)
  else if c = c2
  then i+3, di16 s (i+1)
  else if c = c3
  then i+5, di32 s (i+1)
  else if c = c4
  then i+9, di64 s (i+1)
  else assert false

let get_len_ic c1 c2 c3 c4 c ic =
  try
    let s = "xxxxxxxx" in
    if c = c1
    then (really_input ic s 0 1; di8 s 0)
    else if c = c2
    then (really_input ic s 0 2; di16 s 0)
    else if c = c3
    then (really_input ic s 0 4; di32 s 0)
    else if c = c4
    then (really_input ic s 0 8; di64 s 0)
    else assert false
  with End_of_file -> assert false

let rec decode_key s i =
  match s.[i] with
  | ('i' | 'j' | 'I' | 'J') as c ->
      let i, num = get_len 'i' 'j' 'I' 'J' s i c in
      i, Keys.IntKey num
  | ('s' | 't' | 'S' | 'T') as c ->
      let i, len = get_len 's' 't' 'S' 'T' s i c in
      (i+len, Keys.StringKey (String.sub s i len))
  | ('l' | 'm' | 'L' | 'M') as c ->
      let i, len = get_len 'l' 'm' 'L' 'M' s i c in
      let a = Array.make len (Keys.IntKey 0) in
      let rec aux i j =
        if j >= len
        then i, Keys.ListKey a
        else
          let i, k = decode_key s i in
          a.(j) <- k;
          aux i (j+1)
      in
      aux i 0
  | ('v' | 'w' | 'V' | 'W') as c ->
      let i, num = get_len 'v' 'w' 'V' 'W' s i c in
      i, Keys.VariableKey num
  | _ -> assert false

let rec decode_key_ic ic =
  match input_char ic with
  | ('i' | 'j' | 'I' | 'J') as c ->
      let num = get_len_ic 'i' 'j' 'I' 'J' c ic in
      Keys.IntKey num
  | ('s' | 't' | 'S' | 'T') as c ->
      let len = get_len_ic 's' 't' 'S' 'T' c ic in
      let s = String.create len in
      really_input ic s 0 len;
      Keys.StringKey s
  | ('l' | 'm' | 'L' | 'M') as c ->
      let len = get_len_ic 'l' 'm' 'L' 'M' c ic in
      let a = Array.make len (Keys.IntKey 0) in
      let rec aux j =
        if j >= len
        then Keys.ListKey a
        else
          let k = decode_key_ic ic in
          a.(j) <- k;
          aux (j+1)
      in
      aux 0
  | ('v' | 'w' | 'V' | 'W') as c ->
      let num = get_len_ic 'v' 'w' 'V' 'W' c ic in
      Keys.VariableKey num
  | _ -> assert false

(*
let km1 = Keys.IntKey (-1)
let k1 = Keys.IntKey 123
let k2 = Keys.IntKey 0x100
let k3 = Keys.IntKey 0x10000
let k4 = Keys.IntKey 0x100000000
let k5 = Keys.StringKey "abc"
let k6 = Keys.StringKey (String.make 256 'x')
let k7 = Keys.VariableKey 123
let k8 = Keys.VariableKey 0x100
let k9 = Keys.VariableKey 0x10000
let k10 = Keys.VariableKey 0x100000000
let k11 = Keys.ListKey (Array.of_list [k1;k2;k3;k4;k5;k6;k7;k8;k9;k10])
let k12 = Keys.ListKey (Array.of_list [k11;k11])
let tstk k = k = snd (decode_key (encode_key k) 0)
let allk = [km1;k1;k2;k3;k4;k5;k6;k7;k8;k9;k10;k11;k12]
let good = List.for_all tstk allk
*)

let encode_keylist (kl:Keys.t list) =
  String.concat_map ~left:(put_len 'p' 'q' 'P' 'Q' (List.length kl)) "" encode_key kl

let encode_path (path:Path.t) = encode_keylist (Path.to_list path)

let decode_keylist s i =
  let i, len = get_len 'p' 'q' 'P' 'Q' s i s.[i] in
  let rec aux i j l =
    if j >= len
    then i, List.rev l
    else
      let i, k = decode_key s i in
      aux i (j+1) (k::l)
  in
  aux i 0 []

let decode_path s i =
  let i, kl = decode_keylist s i in
  i, Path.of_list kl

(*
let p1 = Path.of_list [k1;k2]
let k256 = List.init 256 (fun _ -> k1)
let tstp kl = let p = Path.of_list kl in p = snd (decode_path (encode_path p) 0)
let good = List.for_all tstp (List.map (fun x -> [x]) (allk@k256))
*)

let encode_dataimpl = function
  | DataImpl.Int i -> put_len 'a' 'd' 'A' 'D' i
  | DataImpl.Text s -> (put_len 'x' 'y' 'X' 'Y' (String.length s))^s
  | DataImpl.Binary s -> (put_len 'b' 'c' 'B' 'C' (String.length s))^s
  | DataImpl.Float f -> sprintf "f%s" (ef f)
  | DataImpl.Unit -> "u"

let decode_dataimpl s i =
  match s.[i] with
  | ('a' | 'd' | 'A' | 'D') as c ->
      let i, num = get_len 'a' 'd' 'A' 'D' s i c in
      i, DataImpl.Int num
  | ('x' | 'y' | 'X' | 'Y') as c ->
      let i, len = get_len 'x' 'y' 'X' 'Y' s i c in
      (i+len, DataImpl.Text (String.sub s i len))
  | ('b' | 'c' | 'B' | 'C') as c ->
      let i, len = get_len 'b' 'c' 'B' 'C' s i c in
      (i+len, DataImpl.Binary (String.sub s i len))
  | 'f' ->
      let f = df s (i+1) in
      (i+9, DataImpl.Float f)
  | 'u' ->
      (i+1, DataImpl.Unit)
  | _ -> assert false

(*
let di1 = DataImpl.Int 123
let di2 = DataImpl.Int 0x100
let di3 = DataImpl.Int 0x10000
let di4 = DataImpl.Int 0x100000000
let di5 = DataImpl.Text "abc"
let di6 = DataImpl.Text (String.make 256 'x')
let di7 = DataImpl.Binary (String.make 10 '\001')
let di8 = DataImpl.Binary (String.make 256 '\002')
let di9 = DataImpl.Float 123.456
let di10 = DataImpl.Float 123.456e10
let di11 = DataImpl.Float 123.456e-10
let di11 = DataImpl.Unit
let alldi = [di1;di2;di3;di4;di5;di6;di7;di8;di9;di10;di11]
let tstdi di = di = snd (decode_dataimpl (encode_dataimpl di) 0)
let good = List.for_all tstdi alldi
*)


let encode_datas = function
  | Datas.Data di -> "e"^(encode_dataimpl di)
  | Datas.Link p -> "n"^(encode_path p)
  | Datas.Copy (_,p) -> "o"^(encode_path p)
  | Datas.UnsetData -> "U"

let decode_datas s i =
  match s.[i] with
  | 'e' -> let i, di = decode_dataimpl s (i+1) in i, Datas.Data di
  | 'n' -> let i, p = decode_path s (i+1) in i, Datas.Link p
  | 'o' -> let i, p = decode_path s (i+1) in i, Datas.Copy (None,p)
  | 'U' -> i+1, Datas.UnsetData
  | _ -> assert false

(*
let d1 = Datas.Data di1
let d2 = Datas.Data di5
let d3 = Datas.Link (Path.of_list [k1;k2])
let d4 = Datas.Copy (None,Path.of_list [k5;k5])
let d5 = Datas.UnsetData
let alld = [d1;d2;d3;d4;d5]
let tstd d = d = snd (decode_datas (encode_datas d) 0)
let good = List.for_all tstd alld
*)

let encode_node { Node_light. on_disk; disk_file; content } =
  match on_disk, disk_file with
  | true, Some file -> (put_len 'g' 'h' 'G' 'H' (String.length file))^file
  | _, _ -> "k"^(encode_datas content)

let decode_node s i =
  match s.[i] with
  | ('g' | 'h' | 'G' | 'H') as c ->
      let i, len = get_len 'g' 'h' 'G' 'H' s i c in
      (i+len, { Node_light. on_disk=true; disk_file=Some (String.sub s i len); content=Datas.UnsetData; })
  | 'k' ->
      let i, d = decode_datas s (i+1) in
      i, { Node_light. on_disk=false; disk_file=None; content=d; }
  | _ -> assert false

(*
let n1 = { Node_light. on_disk=false; disk_file=None; content=Datas.Data (DataImpl.Text "abc") }
let n2 = { Node_light. on_disk=true; disk_file=Some "/tmp/dog"; content=Datas.UnsetData }
let alln = [n1;n2]
let tstn n = n = snd (decode_node (encode_node n) 0)
let good = List.for_all tstn alln
*)

let encode_tuple_2 (e1,e2) (v1,v2) =
  "2"^(e1 v1)^(e2 v2)

let decode_tuple_2 (d1,d2) s i =
  match s.[i] with
  | '2' ->
      let (i1:int), v1 = d1 s (i+1) in
      let (i2:int), v2 = d2 s i1 in
      i2, (v1, v2)
  | _ -> assert false

let encode_kld kld = encode_tuple_2 (encode_keylist,encode_datas) kld
let decode_kld s i = decode_tuple_2 (decode_keylist,decode_datas) s i
let encode_kln kln = encode_tuple_2 (encode_keylist,encode_node) kln
let decode_kln s i = decode_tuple_2 (decode_keylist,decode_node) s i

(*
let tst2 kld = kld = snd (decode_kld (encode_kld kld) 0)
let di1 = DataImpl.Int 123
let d1 = Datas.Data di1
let k1 = Keys.IntKey 123
let k2 = Keys.IntKey 0x100
let kl1 = [k1;k2]
let good = tst2 (kl1,d1)
*)

(* 2 Aa Bb Cc Dd e f Gg Hh Ii Jj k Ll Mm Nn Oo Pp Qq Rr Ss Tt Uu Vv Ww Xx Yy Zz *)

(*
let db = Dbm.opendbm ("/home/norman/.mlstate/"^(Filename.basename Sys.argv.(0))^"/db_light") [Dbm.Dbm_rdwr] 0O664;;
let db = Dbm.opendbm "/tmp/opadb1" [Dbm.Dbm_rdwr] 0O664;;
let dbl = ref [];;
Dbm.iter (fun k d ->
            (match k with
             | "version" -> ()
             | "timestamp" -> ()
             | _ -> dbl := (Path.to_string (snd (Encode_light.decode_path k 0)),
                            snd (Encode_light.decode_datas d 0))::!dbl);
            print_endline (String.escaped (Printf.sprintf "%s -> %s" k d))) db;;
Dbm.close db;;
*)

