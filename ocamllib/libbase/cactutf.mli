(*
    Copyright © 2011, 2012 MLstate

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
(**
   Server side implementation of Cactutf.
   Encoding of unicode characters
   @author Corentin Gallet
   @author Rudy Sicard
   @author Mathieu Barbin (documentation)
*)

(**
   CactUTF is a `light version' of Camomile, the popular Ocaml library often
   used for its full support of Unicode. Sadly, there are two problems with
   it :
   - Camomile is big. Really big.
   - Camomile has to be installed.

   So, here comes Cactutf, with only the functions needed.
   It's a plain translation of EucalyptUTF into Ocaml.

   You may note that this implementation only used up to 4 bytes for one
   Unicode character, where Camomile goes as far as 6. That's because
   they go further than the RFC 3629.
*)

(**
   {6 Types alias}
*)

(**
   There is at least 3 different int manipulated, so these types
   alias try to reduce confusion
*)

(**
   The representation of unicode char
*)
type unicode = int

(**
   The indexation in unicode char, independant from the implementation
*)
type unicode_index = int

(**
   The indexation in bytes
*)
type bytes_index = int

(**
   {6 Indexation and length}
*)

(**
   For one Unicode code, return the number of bytes needed for
   a representation in UTF-8.
   @raises Lenbytes if the code is invalid
*)
val lenbytes : unicode -> bytes_index

(**
   [Cactutf.length_until string pos]
   Returns the number of unicode characters encoded in the string
   until the position [pos] given in [bytes]
*)
val length_until : string -> bytes_index -> unicode_index

(**
   Returns the number of unicode characters encoded in the string.
   This returns the same result as [Cactutf.length_until s (String.length s)]
*)
val length : string -> unicode_index

(**
   Return the index in bytes of the n-th Unicode character.
*)
val nth : string -> unicode_index -> bytes_index

(**
   [Cactutf.next string pos]
   Return the index of the next Unicode character.
   <!> Silently returns [pos+1] in case of error.
*)
val next : string -> bytes_index -> bytes_index

(**
   {6 Unicode}
*)

(**
   Unicode char can be encoded with 1 up to 4 bytes.
   Theses function encode unicode from there bytes.
*)

val one_byte : int -> unicode
val two_bytes : int -> int -> unicode
val three_bytes : int -> int -> int -> unicode
val four_bytes : int -> int -> int -> int -> unicode

(**
   {6 Access}
*)

(**
   Return the Unicode code of the nth Unicode character.
*)
val get : string -> unicode_index -> unicode

(**
   Return the Unicode code using the index (and not the nth).
   A lot faster, but only when using index instead of position.
*)
val look : string -> bytes_index -> unicode

(**
   {6 Allocation}
*)

(**
   Build a new string from a character.
*)
val cons : unicode -> string

(**
   {6 Extraction, Transformation}
*)

(**
   <!> This is weird, the length given for [sub] is the length in bytes,
   not in number of unicode characters.
*)

val sub : string -> bytes_index -> int -> string

val sub_opt : string -> bytes_index -> int -> string option

(**
   uppercase the string
*)
val uppercase : string -> string

(**
   lowercase the string
*)
val lowercase : string -> string

(**
   check utf8 validity
*)
val check : string -> bool

(**
   {6 Deprecated}
*)

(**
   FIXME: undocumented, incorrect, dirty, not following guidelines.
   This exception should not be exported, and goes not out this module.
*)
exception Lenbytes of int
