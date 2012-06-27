/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Binary data handling: for any sequence of bytes that is not to be considered
 * as text.
 *
 * @destination PRIVATE
 * @stability STABLE
 */

/**
 * {1 About this module}
 *
 * For Node.js Buffer objects are not extensible but this code implements copy-based
 * expansion according to a conservative policy.
 * For both Node.js and OCaml, therefore, growth is an expensive operation in both
 * memory and computation so it pays to get the initial hint correct.
 *
 * {1 Where should I start?}
 *
 * You can create directly from strings with [binary_of_string] or an empty binary can
 * be created using [Binary.create].
 *
 * You can fill binary data using the functions provided by the interface.
 *
 * Insufficient space for added data will cause the data to be expanded, resulting in copying of existing data.
 *
 * Out-of-range numerical values will result in an exception being raised, as will attempting to read data
 * from beyond the end of the binary.
 *
 * {1 What if I need more?}
 *
 * The most low-level routines are [add_string] and [get_string], if you can get your
 * data into a string, use these to embed/unembed in/from the binary data.
 *
 */

/**
 * {1 Types defined in this module}
 */

/**
 * The type of binary data.
 *
 * The underlying type is module Buffer for both OCaml and Node.js.
 * Remember that these are imperative.
 *
**/
@opacapi
@abstract
type binary = external

/**
 * {1 Interface}
 */

/**
 * Create binary data from a string.
 *
 * Copies the entire string.
 *
 * Note: Beware of the behaviour of multi-byte characters which may not be portable
 * between the different backends.
 *
 * @param s The string to copy.
 * @return A binary type set exactly to the size of the string.
 */
binary_of_string : string -> binary = %%BslBinary.binary_of_string%%

/**
 * Turns binary data back into a string.
 *
 * As for binary_of_string this function copies the entire string.
 *
 * Multi-byte characters are not reconstituted.
 *
 * @param b The binary data.
 * @return A string of 8-bit characters.
 */
string_of_binary : binary -> string = %%BslBinary.string_of_binary%%

@opacapi
bin_of_base64(x:string):binary = %%bslPervasivesServer.bin_of_base64%%(x)

Binary = {{

  /**
   * Create binary data of the given size.
   *
   * @param hint The initial size.
   * @return An empty binary value.
   */
  create : int -> binary = %%BslBinary.create%%

  /**
   * The number of bytes in the data.
   * There may be more empty space after the data.
   *
   * @param b The binary data.
   * @return The size of the binary data.
   */
  length : binary -> int = %%BslBinary.length%%

  /**
   * Resize the binary data.
   * Smaller means truncate existing data.
   * May copy the data.
   *
   * @param b The binary data.
   * @param amount Set the buffer to this size.
   * @return void
   */
  resize : binary, int -> void = %%BslBinary.resize%%

  /**
   * Clear out any data.
   * The data remains the same size.
   *
   * @param b The binary data.
   * @return void
   */
  clear : binary -> void = %%BslBinary.clear%%

  /**
   * Set the data size to the size of the defined data.
   * May copy the data.
   *
   * @param b The binary data.
   * @return void
   */
  trim : binary -> void = %%BslBinary.trim%%

  /**
   * Return any underlying memory and set data size to zero.
   *
   * @param b The binary data.
   * @return void
   */
  reset : binary -> void = %%BslBinary.reset%%

  /**
   * Add a string to the end of the data.
   *
   * @param b The binary data.
   * @param str The string to add.
   * @return void
   */
  add_string : binary, string -> void = %%BslBinary.add_string%%

  /**
   * Add binary data to the end of the data.
   *
   * @param b The binary data.
   * @param nb New binary data to add.
   * @return void
   */
  add_binary : binary, binary -> void = %%BslBinary.add_binary%%

  /**
   * Add a signed 8-bit int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (-128 to 127).
   * @return void
   */
  add_int8 : binary, int -> void = %%BslBinary.add_int8%%

  /**
   * Add an unsigned 8-bit int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (0 to 255).
   * @return void
   */
  add_uint8 : binary, int -> void = %%BslBinary.add_uint8%%

  /**
   * Add a signed 16-bit little-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (-32768 to 32767).
   * @return void
   */
  add_int16_le : binary, int -> void = %%BslBinary.add_int16_le%%

  /**
   * Add an unsigned 16-bit little-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (0 to 65535).
   * @return void
   */
  add_uint16_le : binary, int -> void = %%BslBinary.add_uint16_le%%

  /**
   * Add a signed 16-bit big-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (-32768 to 32767).
   * @return void
   */
  add_int16_be : binary, int -> void = %%BslBinary.add_int16_be%%

  /**
   * Add an unsigned 16-bit big-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (0 to 65535).
   * @return void
   */
  add_uint16_be : binary, int -> void = %%BslBinary.add_uint16_be%%

  /**
   * Add a signed 32-bit little-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (-2147483648 to 2147483647).
   * @return void
   */
  add_int32_le : binary, int -> void = %%BslBinary.add_int32_le%%

  /**
   * Add an unsigned 32-bit little-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (0 to 4294967295).
   * @return void
   */
  add_uint32_le : binary, int -> void = %%BslBinary.add_uint32_le%%

  /**
   * Add a signed 32-bit big-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (-2147483648 to 2147483647).
   * @return void
   */
  add_int32_be : binary, int -> void = %%BslBinary.add_int32_be%%

  /**
   * Add an unsigned 32-bit big-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (0 to 4294967295).
   * @return void
   */
  add_uint32_be : binary, int -> void = %%BslBinary.add_uint32_be%%

  /**
   * Add a signed 53-bit little-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (-9007199254740992 to 9007199254740991).
   * @return void
   */
  add_int53_le : binary, int -> void = %%BslBinary.add_int53_le%%

  /**
   * Add a signed 53-bit big-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (-9007199254740992 to 9007199254740991).
   * @return void
   */
  add_int53_be : binary, int -> void = %%BslBinary.add_int53_be%%

  /**
   * Add an unsigned 64-bit little-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Int64 to add (0 to 18446744073709551615).
   * @return void
   */
  add_uint64_le : binary, int64 -> void = %%BslBinary.add_uint64_le%%

  /**
   * Add an unsigned 64-bit big-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Int64 to add (0 to 18446744073709551615).
   * @return void
   */
  add_uint64_be : binary, int64 -> void = %%BslBinary.add_uint64_be%%

  /**
   * Add a 32-bit little-endian float to the end of the data.
   * The storage format is not a strict IEEE 32-bit float but a rounded equivalent,
   * there may be rounding errors when read back and the ranges shown here are
   * not accurate to the last bit.
   *
   * @param b The binary data.
   * @param f Float to add (approx 1.175494351e-38 to 3.402823466e38).
   * @return void
   */
  add_float_le : binary, float -> void = %%BslBinary.add_float_le%%

  /**
   * Add a 32-bit big-endian float to the end of the data.
   * The storage format is not a strict IEEE 32-bit float but a rounded equivalent,
   * there may be rounding errors when read back and the ranges shown here are
   * not accurate to the last bit.
   *
   * @param b The binary data.
   * @param f Float to add (approx 1.175494351e-38 to 3.402823466e38).
   * @return void
   */
  add_float_be : binary, float -> void = %%BslBinary.add_float_be%%

  /**
   * Add a 64-bit little-endian float to the end of the data.
   *
   * @param b The binary data.
   * @param f Float to add (+/-2.22507385850720138e-308 to +/-1.79769313486231571e+308).
   * @return void
   */
  add_double_le : binary, float -> void = %%BslBinary.add_double_le%%

  /**
   * Add a 64-bit big-endian float to the end of the data.
   *
   * @param b The binary data.
   * @param f Float to add (+/-2.22507385850720138e-308 to +/-1.79769313486231571e+308).
   * @return void
   */
  add_double_be : binary, float -> void = %%BslBinary.add_double_be%%

  /**
   * Read a section of the buffer back into a string.
   *
   * Particularly for Node.js, the string read back is not translated in any way so you
   * should attempt to keep the data as binary as far as possible, even passing the binary
   * data directly to transmission routines which should know about any required translation.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @param length Number of bytes to read.
   * @return The data section as a string.
   */
  get_string : binary, int, int -> string = %%BslBinary.get_string%%

  /**
   * Read a section of the buffer back into binary data.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @param length Number of bytes to read.
   * @return untranslated binary data
   */
  get_binary : binary, int, int -> binary = %%BslBinary.get_binary%%

  /**
   * Read a signed 8-bit int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (-128 to 127).
   */
  get_int8 : binary, int -> int = %%BslBinary.get_int8%%

  /**
   * Read an unsigned 8-bit int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (0 to 255).
   */
  get_uint8 : binary, int -> int = %%BslBinary.get_uint8%%

  /**
   * Read a signed 16-bit little-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (-32768 to 32767).
   */
  get_int16_le : binary, int -> int = %%BslBinary.get_int16_le%%

  /**
   * Read an unsigned 16-bit little-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (0 to 65535).
   */
  get_uint16_le : binary, int -> int = %%BslBinary.get_uint16_le%%

  /**
   * Read a signed 16-bit big-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (-32768 to 32767).
   */
  get_int16_be : binary, int -> int = %%BslBinary.get_int16_be%%

  /**
   * Read an unsigned 16-bit big-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (0 to 65535).
   */
  get_uint16_be : binary, int -> int = %%BslBinary.get_uint16_be%%

  /**
   * Read a signed 32-bit little-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (-2147483648 to 2147483647).
   */
  get_int32_le : binary, int -> int = %%BslBinary.get_int32_le%%

  /**
   * Read an unsigned 32-bit little-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (0 to 4294967295).
   */
  get_uint32_le : binary, int -> int = %%BslBinary.get_uint32_le%%

  /**
   * Read a signed 32-bit big-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (-2147483648 to 2147483647).
   */
  get_int32_be : binary, int -> int = %%BslBinary.get_int32_be%%

  /**
   * Read an unsigned 32-bit big-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (0 to 4294967295).
   */
  get_uint32_be : binary, int -> int = %%BslBinary.get_uint32_be%%

  /**
   * Read a signed 53-bit little-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (-9007199254740992 to 9007199254740991).
   */
  get_int53_le : binary, int -> int = %%BslBinary.get_int53_le%%

  /**
   * Read a signed 53-bit big-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (-9007199254740992 to 9007199254740991).
   */
  get_int53_be : binary, int -> int = %%BslBinary.get_int53_be%%

  /**
   * Read an unsigned 64-bit little-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Int64 (0 to 18446744073709551615).
   */
  get_uint64_le : binary, int -> int64 = %%BslBinary.get_uint64_le%%

  /**
   * Read an unsigned 64-bit big-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Int64 (0 to 18446744073709551615).
   */
  get_uint64_be : binary, int -> int64 = %%BslBinary.get_uint64_be%%

  /**
   * Read a 32-bit little-endian float from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Float (approx 1.175494351e-38 to 3.402823466e38).
   */
  get_float_le : binary, int -> float = %%BslBinary.get_float_le%%

  /**
   * Read a 32-bit big-endian float from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Float (approx 1.175494351e-38 to 3.402823466e38).
   */
  get_float_be : binary, int -> float = %%BslBinary.get_float_be%%

  /**
   * Read a 64-bit little-endian float from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Float (+/-2.22507385850720138e-308 to +/-1.79769313486231571e+308).
   */
  get_double_le : binary, int -> float = %%BslBinary.get_double_le%%

  /**
   * Read a 64-bit big-endian float from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Float (+/-2.22507385850720138e-308 to +/-1.79769313486231571e+308).
   */
  get_double_be : binary, int -> float = %%BslBinary.get_double_be%%

}}


