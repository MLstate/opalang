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
    Versions of Char character predicates based on lookup tables.
    Some tests 10% faster, others several times faster.
    Also some highly specialised tests mostly used in HttpTools and Encodings.
*)

(** Fast character predicates *)
val is_digitf : char -> bool
val is_hexf : char -> bool
val is_lowerf : char -> bool
val is_upperf : char -> bool
val is_alphaf : char -> bool
val is_spacef : char -> bool
val is_luf : char -> bool
val is_sptabf : char -> bool
val is_namef : char -> bool
val is_charf : char -> bool
val is_urlf : char -> bool
val is_sepf : char -> bool
val is_htmlcharf : char -> bool
val is_urlxf : char -> bool
val is_awsf : char -> bool

(** Fast character conversions *)

(** Hex char to number. *)
val hcodef : char -> int

(** Three decimal digits to int *)
val c3i : char -> char -> char -> int

(** Two hexadecimal digits to int *)
val c2h : char -> char -> int

(** Four unicode chars to string *)
val c4u : char -> char -> char -> char -> string

(* End of file charf.mli *)
