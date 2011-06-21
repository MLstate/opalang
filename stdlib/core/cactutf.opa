/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/
/*
    @authors Corentin Gallet
**/

/*-------------------
| CactUTF interface |
-------------------*/

/*
** The purpose of this file is to permit the use of the functions of
** cactutf.ml into the module Text in opa.
** For a description of the functions, see cactutf.ml
*/

Cactutf = {{

  /* length of the sequence of bytes, given the first byte */
  lenbytes = %% BslCactutf.lenbytes %% : int -> option(int)

  one_byte = %% BslCactutf.one_byte %% : int -> int
  two_bytes = %% BslCactutf.two_bytes %% : int, int -> int
  three_bytes = %% BslCactutf.three_bytes %% : int, int, int -> int
  four_bytes = %% BslCactutf.four_bytes %% : int, int, int, int -> int

  /* length */
  length = %% BslCactutf.length %% : string -> int
  length_until = %% BslCactutf.length_until %% : string, int -> int

  /* uppercase */
  uppercase = %% BslCactutf.uppercase %% : string -> string

  /* lowercase */
  lowercase = %% BslCactutf.lowercase %% : string -> string

  /* sub */
  sub = %% BslCactutf.sub %% : string, int, int -> string

  /* sub_opt */
  sub_opt = %% BslCactutf.sub_opt %% : string, int, int -> option(string)

  /* nth */
  nth = %% BslCactutf.nth %% : string, int -> int

  /* get */
  get = %% BslCactutf.get %% : string, int -> Unicode.character

  /* look */
  look = %% BslCactutf.look %% : string, int -> Unicode.character

  /* next */
  next = %% BslCactutf.next %% : string, int -> int

  /* cons */
  cons = %% BslCactutf.cons %% : int -> string

}}
