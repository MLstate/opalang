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
