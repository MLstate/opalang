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
(** Charf:
    Equivalent of Char.is_xyz except implemented as lookup tables.
*)
module List = Base.List
module String = Base.String
module Char = Base.Char

(*
(* Code to generate chtab. *)
let is_lu ch = Char.is_lower ch || Char.is_upper ch
let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_sptab c = c = ' ' || c = '\t'
let is_name = function | 'a'..'z' -> true | 'A'..'Z' -> true | '0'..'9' -> true | '_' | '-' -> true | _ -> false
let is_url = function
  | 'a'..'z' -> true | 'A'..'Z' -> true | '0'..'9' -> true
  | '_' | '-' | '$' | '.' | '+' | '!' | '*' | '\'' | '(' | ')' | ',' -> true
  | _ -> false
(* is_url but excluding , / ? : @ & = + $ # *) (* and now: ! ' *)
let is_urlx = function
  | 'a'..'z' -> true | 'A'..'Z' -> true | '0'..'9' -> true
  | '_' | '-' | '.' | '*' | '(' | ')' -> true
  | _ -> false
(*A-Z, a-z, 0-9, hyphen ( - ), underscore ( _ ), period ( . ), and tilde ( ~ )*)
let is_aws = function
  | 'a'..'z' -> true | 'A'..'Z' -> true | '0'..'9' -> true
  | '-' | '_' | '.' | '~' -> true
  | _ -> false
let is_char = function | ' ' | '\t' | '\r' | '\n' | '%' | '&' | '?' | '=' | '+' -> true | _ -> false
let is_sep = function
  | ' ' | '\n' | '\t' | '\r' | ',' | '?' | ';' | '.' | ':' | '/' | '!' | '*' | '+' | '(' | ')' | '[' | ']' -> true
  | _ -> false
let is_htmlchar = function
  | 'a'..'z' -> true | 'A'..'Z' -> true | '0'..'9' -> true | '\'' | '"' | '_' | '-' | '@' -> true | _ -> false
let chtab = Array.create 256 0
let setcodes (bit,is_fn) =
  List.iter (fun ch -> if is_fn ch then let idx = Char.code ch in chtab.(idx) <- chtab.(idx) lor bit else ())
            (List.init 256 (fun i -> Char.chr i))
let codes = [(0x0001,Char.is_digit); (0x0002,is_hex);        (0x0004,Char.is_lower); (0x0008,Char.is_upper);
             (0x0010,Char.is_alpha); (0x0020,Char.is_space); (0x0040,is_lu);         (0x0080,is_sptab);
             (0x0100,is_name);       (0x0200,is_char);       (0x0400,is_url);        (0x0800,is_sep);
             (0x1000,is_htmlchar);   (0x2000,is_urlx);       (0x4000,is_aws); ]
let _ = List.iter setcodes codes
*)
let chtab = [|0; 0; 0; 0; 0; 0; 0; 0; 0; 2720; 2592; 0; 0; 2592; 0; 0; 0; 0; 0; 0; 0; 0;
              0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2720; 3072; 4096; 0; 1024; 512; 512; 5120;
              11264; 11264; 11264; 3584; 3072; 29952; 27648; 2048; 29971; 29971; 29971;
              29971; 29971; 29971; 29971; 29971; 29971; 29971; 2048; 2048; 0; 512; 0;
              2560; 4096; 30042; 30042; 30042; 30042; 30042; 30042; 30040; 30040; 30040;
              30040; 30040; 30040; 30040; 30040; 30040; 30040; 30040; 30040; 30040;
              30040; 30040; 30040; 30040; 30040; 30040; 30040; 2048; 0; 2048; 0; 29952;
              0; 30038; 30038; 30038; 30038; 30038; 30038; 30036; 30036; 30036; 30036;
              30036; 30036; 30036; 30036; 30036; 30036; 30036; 30036; 30036; 30036;
              30036; 30036; 30036; 30036; 30036; 30036; 0; 0; 0; 16384; 0; 0; 0; 0; 0; 0;
              0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
              0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
              0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
              0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
              0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]

(* These are faster than the Char ones:
 * for digit, lower, upper and alpha about 10% faster,
 * for hex, space about 2x as fast.
 *)
let is_digitf    ch = (chtab.(Char.code ch) land 0x0001) <> 0
let is_hexf      ch = (chtab.(Char.code ch) land 0x0002) <> 0
let is_lowerf    ch = (chtab.(Char.code ch) land 0x0004) <> 0
let is_upperf    ch = (chtab.(Char.code ch) land 0x0008) <> 0
let is_alphaf    ch = (chtab.(Char.code ch) land 0x0010) <> 0
let is_spacef    ch = (chtab.(Char.code ch) land 0x0020) <> 0
let is_luf       ch = (chtab.(Char.code ch) land 0x0040) <> 0
let is_sptabf    ch = (chtab.(Char.code ch) land 0x0080) <> 0
let is_namef     ch = (chtab.(Char.code ch) land 0x0100) <> 0
let is_charf     ch = (chtab.(Char.code ch) land 0x0200) <> 0
let is_urlf      ch = (chtab.(Char.code ch) land 0x0400) <> 0
let is_sepf      ch = (chtab.(Char.code ch) land 0x0800) <> 0
let is_htmlcharf ch = (chtab.(Char.code ch) land 0x1000) <> 0
let is_urlxf     ch = (chtab.(Char.code ch) land 0x2000) <> 0
let is_awsf      ch = (chtab.(Char.code ch) land 0x4000) <> 0

(* Code to generate hxtab.
let hcode ch =
  match ch with
  | '0'..'9' -> (Char.code ch)-48
  | 'a'..'f' -> (Char.code ch)-87
  | 'A'..'F' -> (Char.code ch)-55
  | _ -> 0
let hxtab = Array.create 256 0
let _ = List.iter (fun ch -> hxtab.(Char.code ch) <- hcode ch) (List.init 256 (fun i -> Char.chr i))
*)
let hxtab = [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
              0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1;
              2; 3; 4; 5; 6; 7; 8; 9; 0; 0; 0; 0; 0; 0; 0; 10; 11; 12; 13; 14; 15; 0; 0;
              0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 10;
              11; 12; 13; 14; 15; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
              0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
              0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
              0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
              0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
              0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
              0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
let hcodef ch = hxtab.(Char.code ch)

let c3i c1 c2 c3 = ((Char.code c1)-48)*100+((Char.code c2)-48)*10+((Char.code c3)-48)
let c2h c1 c2 = (hcodef (c1))*16+(hcodef c2)

(* SML names... *)
let implode cl = let s = String.create (List.length cl) in List.iteri (fun c i -> s.[i] <- c) cl; s

let c4u c1 c2 c3 c4 = implode (Uchar.chars_of_uchar (4096 * (hcodef c1) + 256 * (hcodef c2) + 16 * (hcodef c3) + (hcodef c4)))

(* End of file charf.ml *)
