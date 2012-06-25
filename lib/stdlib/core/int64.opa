/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * This modules defines operations on 64 bits integers
 *
 * @category data
 * @destination public
 * @stability stable
**/

/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

type int64 = external

/**
 * {1 Interface}
 */

Int64 = {{

  catch = %%BslNumber.BslInt64.catch%% : (-> 'a) -> outcome('a,string)

  is_NaN = %%BslNumber.BslInt64.is_NaN%% : int64 -> bool
  add = %%BslNumber.BslInt64.add%% : int64, int64 -> int64
  sub = %%BslNumber.BslInt64.sub%% : int64, int64 -> int64
  mul = %%BslNumber.BslInt64.mul%% : int64, int64 -> int64
  div = %%BslNumber.BslInt64.div%% : int64, int64 -> int64
  rem = %%BslNumber.BslInt64.rem%% : int64, int64 -> int64
  pred = %%BslNumber.BslInt64.pred%% : int64 -> int64
  succ = %%BslNumber.BslInt64.succ%% : int64 -> int64
  max_int = %%BslNumber.BslInt64.max_int%% : -> int64
  logand = %%BslNumber.BslInt64.logand%% : int64, int64 -> int64
  logor = %%BslNumber.BslInt64.logor%% : int64, int64 -> int64
  logxor = %%BslNumber.BslInt64.logxor%% : int64, int64 -> int64
  lognot = %%BslNumber.BslInt64.lognot%% : int64 -> int64
  shift_left = %%BslNumber.BslInt64.shift_left%% : int64, int -> int64
  shift_right = %%BslNumber.BslInt64.shift_right%% : int64, int -> int64
  shift_right_logical = %%BslNumber.BslInt64.shift_right_logical%% : int64, int -> int64
  of_int = %%BslNumber.BslInt64.of_int%% : int -> int64
  to_int = %%BslNumber.BslInt64.to_int%% : int64 -> int
  of_string = %%BslNumber.BslInt64.of_string%% : string -> int64
  of_string_radix = %%BslNumber.BslInt64.of_string_radix%% : string, int -> int64
  to_string = %%BslNumber.BslInt64.to_string%% : int64 -> string
  to_string_radix = %%BslNumber.BslInt64.to_string_radix%% : int64, int -> string
  op_eq = %%BslNumber.BslInt64.op_eq%% : int64, int64 -> bool
  op_ne = %%BslNumber.BslInt64.op_ne%% : int64, int64 -> bool
  op_gt = %%BslNumber.BslInt64.op_gt%% : int64, int64 -> bool
  op_ge = %%BslNumber.BslInt64.op_ge%% : int64, int64 -> bool
  op_lt = %%BslNumber.BslInt64.op_lt%% : int64, int64 -> bool
  op_le = %%BslNumber.BslInt64.op_le%% : int64, int64 -> bool
  embed_be = %%BslNumber.BslInt64.embed_be%% : int64 -> string
  embed_le = %%BslNumber.BslInt64.embed_le%% : int64 -> string
  unembed_be = %%BslNumber.BslInt64.unembed_be%% : string, int -> int64
  unembed_le = %%BslNumber.BslInt64.unembed_le%% : string, int -> int64

}}
