/*
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
*/
/*
    @authors Corentin Gallet
**/

/*-------------------
| CactUTF interface |
-------------------*/

/**
 * The purpose of this file is to permit the use of the functions of
 * cactutf.ml into the module Text in opa.
 *
 * For a description of the functions, see cactutf.ml
 */

/**
 * {1 About this module}
 *
 * {1 Where should I start ?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Interface}
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
