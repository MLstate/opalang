/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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

  // check = %% BslCactutf.check %% : string -> bool

}}
