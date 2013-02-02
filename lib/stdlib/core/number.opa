/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * This modules defines operations on integers, which are guaranteed to be at
 * least 31 bits wide (including the sign bit)
 *
 * @category data
 * @destination public
 * @author Valentin Gatien-Baron, 2010 (documentation)
 * @author Nicolas Pelletier, 2010
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

/**
 * Order type label for int and float
 */
type Int.order = Order.default
type Float.order = Order.default

/**
 * {1 Interface}
 */

Int = {{

  /**
   * {2 Numeric operations}
   */

  /**
   * The standard addition
   */
  add         = %% BslPervasives.int_add %% : int, int -> int

  /**
   * The standard subtracion
   */
  sub         = %% BslPervasives.int_sub %% : int, int -> int

  /**
   * The standard multiplication
   */
  mul         = %% BslPervasives.int_mul %% : int, int -> int

  /**
   * The standard quotient
   */
  div         = %% BslPervasives.int_div %% : int, int -> int

  /**
   * {2 Order}
   */
  order       = Order.make(ordering) : order(int, Int.order)

  /**
   * Compare two int
   */
  compare_raw = %% BslPervasives.compare_int %%
  ordering    = %% BslNumber.Int.ordering %%

  compare(x,y): Order.comparison =
     @opensums(ordering(x,y))


  `!=` = %% BslPervasives.int_cmp_neq  %% : int, int -> bool
  `==` = %% BslPervasives.int_cmp_eq   %% : int, int -> bool
  `<`  = %% BslPervasives.int_cmp_lneq %% : int, int -> bool
  `<=` = %% BslPervasives.int_cmp_leq  %% : int, int -> bool
  `>`  = %% BslPervasives.int_cmp_gneq %% : int, int -> bool
  `>=` = %% BslPervasives.int_cmp_geq  %% : int, int -> bool

  `+` = %% BslPervasives.int_add %%
  `-` = %% BslPervasives.int_sub %%
  `*` = %% BslPervasives.int_mul %%
  `/` = %% BslPervasives.int_div %%

  /**
   * Add 1 to the int given in parameter
   */
  succ(i) = i+1

  /**
   * Subtract 1 to the int given in parameter
   */
  pred(i) = i-1

  /**
   * Check equality of two int
   */
  equals =  %% BslPervasives.int_cmp_eq %% : int, int -> bool

  /**
   * {2 Conversion functions}
   */

  /**
   * Returns the string representation of the integer
   */
  to_string   = String.of_int : int -> string

  /**
   * Parse a string as an int
   * The usage of this function is discouraged, as it will exit with an error
   * when the input is incorrect
   * It should be used only in parsers, where you can be sure that the string is
   * valid
   */
  of_string = %% BslNumber.Int.of_string %% : string -> int

  of_string_opt = %% BslNumber.Int.of_string_opt %% : string -> option(int)

  /**
   * Converts a float to int
   */
  of_float = %% BslNumber.Int.of_float %% : float -> int

  /**
   * Return the ascii value (later utf-8 value) of the first char of the string
   */
  of_utf8 = %% bslPervasives.int_of_first_char %% : string -> int

  /**
   * Return the float representation of an integer
   */
  to_float= Float.of_int

  /**
   * Convert the integer to its string representation in hexadecimal
   */
  to_hex =
    int_to_1hex(int) =
      //if int<10 then string_of_int(int) <-- problems with node
      //else
        match int:int with
        | 0 -> "0"
        | 1 -> "1"
        | 2 -> "2"
        | 3 -> "3"
        | 4 -> "4"
        | 5 -> "5"
        | 6 -> "6"
        | 7 -> "7"
        | 8 -> "8"
        | 9 -> "9"
        | 10 -> "A"
        | 11 -> "B"
        | 12 -> "C"
        | 13 -> "D"
        | 14 -> "E"
        | 15 -> "F"
        | _ -> @fail
    (i ->
      rec aux(i, s) = if i > 0
        then aux(i / 16, int_to_1hex(mod(i, 16))^s)
        else s
      aux(i, "")
    ) : int -> string

  /**
   *
  **/
  fold = (f, init, n ->
    rec aux(i, acc) = if n == i then acc else aux(i + 1, f(acc, i))
    aux(0, init)
  ) : ('a, int -> 'a), 'a, int -> 'a

  /**
   *
  **/
  repeat = (f,init,n ->
    fold(acc, _index -> f(acc), init, n)
  ) : ('a -> 'a), 'a, int -> 'a

  max(a:int, b:int) =
    match compare_raw(a,b) with
      | -1 -> b
      |  _ -> a

  min(a:int, b:int) =
    match compare_raw(a,b) with
      |  1 -> b
      |  _ -> a

  abs = %% BslNumber.Math.abs_i %%     : int -> int

}}

Float =
{{

  /** UNSAFE
   * try to convert a string into another type
   * if you want to do that, that should mean the string come from a parser
   * but if this is the case, you probably already get the wanted type
   * in case of the string is not a representation this type the function throw an error
   */
  of_string= %% BslNumber.Float.of_string %%
  of_string_opt = %% BslNumber.Float.of_string_opt %%
  of_int= %% BslNumber.Float.of_int %%
  to_int    = Int.of_float
  to_string = String.of_float
  to_formatted_string = %% BslNumber.Float.to_formatted_string %%
  infinity = 1. / 0.
  NaN = 0.0 / 0.0
  abs =  %% BslNumber.Math.abs_f %%     : float -> float
  equals      = `==`
  `<`  = %% BslNumber.Float.lt%%
  `>`  = %% BslNumber.Float.gt%%
  `!=` = %% BslNumber.Float.neq%%
  `==` = %% BslNumber.Float.eq%%
  `>=` = %% BslNumber.Float.geq%%
  `<=` = %% BslNumber.Float.leq%%
  compare_raw = %% Bslpervasives.compare_float %%
  compare     = %% BslNumber.Float.comparison %%
  `+` = %% Bslpervasives.float_add %%
  `-` = %% Bslpervasives.float_sub %%
  `*` = %% Bslpervasives.float_mul %%
  `/` = %% Bslpervasives.float_div %%


  eq(a, b) =
    if not(Math.is_normal(a) && Math.is_normal(b)) then
      a == b
    else
      // we divide by zero only when a=0 and b=0, in which case v is NaN and eq(a,b) is true
      v = abs(a - b) / max(abs(a), abs(b))
      Math.is_NaN(v) || v < 0.000001

  /**
   * A total order on floating-point numbers
   *
   * Note: In this order, special number [NaN] ("not a number" is larger than the largest floating point number, but smaller than positive infinity)
   */
  ordering(a:float,b:float): Order.ordering =
     match compare_raw(a,b) with
       | -1 -> {lt}
       |  0 -> {eq}
       |  1 -> {gt}
       | _ -> @fail

  order       = Order.make(ordering) : order(float, Float.order)

  ceil : float -> float = %% BslNumber.Float.ceil %%
  floor : float -> float  = %% BslNumber.Float.floor %%
  round : float -> int = %% BslNumber.Float.round %%

  max(a:float, b:float) =
    match compare_raw(a,b) with
      | -1 -> b
      |  _ -> a

  min(a:float, b:float) =
    match compare_raw(a,b) with
      |  1 -> b
      |  _ -> a

}}


Bitwise = {{

  /**
   * {2 Bitwise operations}
   *
   * {b Warning}: Low-level operations with different semantics on the server
   * and on the client. On the server, they work on 64-bits integers; on the
   * client, they work on 32-bits integers.
   */

  /**
   * Bitwise logical and.
  **/
  land        = %% BslNumber.Int.op_land %% : int, int -> int

  /**
   * Bitwise logical or.
  **/
  lor         = %% BslNumber.Int.op_lor %% : int, int -> int

  /**
   * Bitwise logical xor.
  **/
  lxor        = %% BslNumber.Int.op_lxor %% : int, int -> int

  /**
   * Bitwise logical negation.
  **/
  lnot        = %% BslNumber.Int.op_lnot %% : int -> int

  /**
   * [n lsl m] shifts n to the left by m bits.
   * The result is unspecified if [m < 0] or [m >= bitsize],
   * where bitsize is 32 on a 32-bit platform and 64 on a 64-bit platform.
   *
  **/
  lsl         = %% BslNumber.Int.op_lsl %% : int, int -> int

  /**
   * [Bitwise.lsr(n, m)] shifts n to the right by m bits.
   * This is a logical shift:
   * zeroes are inserted regardless of the sign of n .
   * The result is unspecified if [m < 0] or [m >= bitsize]
   *
  **/
  lsr         = %% BslNumber.Int.op_lsr %% : int, int -> int

  /**
   * [Bitwise.asr(n, m)] shifts n to the right by m bits.
   * This is an arithmetic shift: the sign bit of n is replicated.
   * The result is unspecified if [m < 0] or [m >= bitsize].
  **/
  asr         = %% BslNumber.Int.op_asr %% : int, int -> int

}}
