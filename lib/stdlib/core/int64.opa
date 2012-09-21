/*
    Copyright © 2011, 2012 MLstate

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
 * The Int64 module was previously just a placeholder for anonymous external data providing
 * a means for retaining pure 64-bit values hidden behind the Bsl.
 *
 * This is now a minimal-implementation of an int64 type which provides 64-bit integer
 * arithmetic and bitwise operations.
 *
 * Note, however, that there is a disparity between the OCaml and Node.js backends, OCaml 64-bit
 * integers are signed, whereas the Node.js implementation is unsigned.  This will be fixed at some
 * future development.
 *
 * Note also that the Node.js representation is as two 32-bit integers (each of which is actually
 * stored as 64-bit values) and arithmetic operations are likely to be very slow.  This module is
 * not intended to support significant computations on 64-bit ints.  Due to the underlying float
 * model for Node.js integers, int64 values in Node.js can actually be NaN.
 *
 * {1 Where should I start?}
 *
 * The operations in here are very self-explanatory.
 *
 * {1 What if I need more?}
 *
 * It is unlikely that this module will ever be developed any further other than to make
 * Node.js int64 values signed.
 */

/**
 * {1 Types defined in this module}
 *
 * The int64 type is external, represented as Int64.t values in OCaml
 * and [{h:<high 32 bits>,l:<low 32 bits>}] in Node.js.
 */

type int64 = external

/**
 * {1 Interface}
 */

Int64 = {{

  /** Simple wrapper to allow catching thrown exceptions under Node.js. **/
  catch = %%BslNumber.BslInt64.catch%% : (-> 'a) -> outcome('a,string)

  /** Predicate for NaN status, always false under OCaml **/
  is_NaN = %%BslNumber.BslInt64.is_NaN%% : int64 -> bool

  /** Add two int64 values **/
  add = %%BslNumber.BslInt64.add%% : int64, int64 -> int64

  /** Subtract two int64 values **/
  sub = %%BslNumber.BslInt64.sub%% : int64, int64 -> int64

  /** Multiply two int64 values **/
  mul = %%BslNumber.BslInt64.mul%% : int64, int64 -> int64

  /** Divide two int64 values **/
  div = %%BslNumber.BslInt64.div%% : int64, int64 -> int64

  /** Modulus of two int64 values **/
  rem = %%BslNumber.BslInt64.rem%% : int64, int64 -> int64

  /** Subtract 1 from an int64 value, may raise underflow exception **/
  pred = %%BslNumber.BslInt64.pred%% : int64 -> int64

  /** Add 1 to an int64 value, may raise overflow exception **/
  succ = %%BslNumber.BslInt64.succ%% : int64 -> int64

  /** Maximum int64 value **/
  max_int = %%BslNumber.BslInt64.max_int%% : int64

  /** Bitwise and of two int64 values **/
  logand = %%BslNumber.BslInt64.logand%% : int64, int64 -> int64

  /** Bitwise or of two int64 values **/
  logor = %%BslNumber.BslInt64.logor%% : int64, int64 -> int64

  /** Bitwise exclusive-or of two int64 values **/
  logxor = %%BslNumber.BslInt64.logxor%% : int64, int64 -> int64

  /** Bitwise complement of an int64 value **/
  lognot = %%BslNumber.BslInt64.lognot%% : int64 -> int64

  /** Left shift of an int64 value **/
  shift_left = %%BslNumber.BslInt64.shift_left%% : int64, int -> int64

  /** Arithmetic right shift of an int64 value **/
  shift_right = %%BslNumber.BslInt64.shift_right%% : int64, int -> int64

  /** Logical right shift of an int64 value **/
  shift_right_logical = %%BslNumber.BslInt64.shift_right_logical%% : int64, int -> int64

  /** Convert int into int64 **/
  of_int = %%BslNumber.BslInt64.of_int%% : int -> int64

  /** Convert int64 to int, may raise overflow **/
  to_int = %%BslNumber.BslInt64.to_int%% : int64 -> int

  /** Convert signed int into int64 **/
  of_int_signed = %%BslBinary.i64_of_int_signed%% : int -> int64

  /**
   * Convert int64 into signed int if the int64 is not greater than max_int.
   * @param i64 The int64 number to convert
   * @return An option which contains the converted int, or [none] if [i64] is
   * greater than max_int
   **/
  to_int_signed_opt = %%BslNumber.BslInt64.to_int_signed_opt%% : int64 -> option(int)

  /** Convert int64 to signed int, may raise overflow **/
  to_int_signed = %%BslBinary.i64_to_int_signed%% : int64 -> int

  /** Generate int64 from decimal string **/
  of_string = %%BslNumber.BslInt64.of_string%% : string -> int64

  /** Generate int64 from string of radix 2 to 16 **/
  of_string_radix = %%BslNumber.BslInt64.of_string_radix%% : string, int -> int64

  /** Turn int64 into decimal string **/
  to_string = %%BslNumber.BslInt64.to_string%% : int64 -> string

  /** Turn int64 into string of radix 2 to 16 **/
  to_string_radix = %%BslNumber.BslInt64.to_string_radix%% : int64, int -> string

  /** Equality predicate for int64 **/
  op_eq = %%BslNumber.BslInt64.op_eq%% : int64, int64 -> bool

  /** Inequality predicate for int64 **/
  op_ne = %%BslNumber.BslInt64.op_ne%% : int64, int64 -> bool

  /** Greater than predicate for int64 **/
  op_gt = %%BslNumber.BslInt64.op_gt%% : int64, int64 -> bool

  /** Greater than or equal to predicate for int64 **/
  op_ge = %%BslNumber.BslInt64.op_ge%% : int64, int64 -> bool

  /** Less than predicate for int64 **/
  op_lt = %%BslNumber.BslInt64.op_lt%% : int64, int64 -> bool

  /** Less than or equal to predicate for int64 **/
  op_le = %%BslNumber.BslInt64.op_le%% : int64, int64 -> bool

  /** Zero **/
  zero = of_int(0)

}}
