/*
    Copyright © 2011-2013 MLstate

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

binary_of_string : string -> binary = Binary.of_string

string_of_binary : binary -> string = Binary.to_string

@opacapi
bin_of_base64 = Binary.of_base64

Binary = {{

  /**
   * A generic to_string function.
   * @param encoding is wanted encoding
   * @param data is binary data to encode
   * @returns An encoded string of [data]
   */
  @private
  to_encoding(data, encoding) = %%bslBinary.to_encoding%%(data, encoding)

  /**
   * Returns the string content of a binary [data].
   * Note: The binary data *must* be valid utf8 sequence else the resulted string
   * can be invalid.
   * @param data The binary data
   * @return The decoded content of [data]
   */
  to_string(data) = to_encoding(data, "utf8")

  /**
   * Returns a base64 encoded string of a binary [data].
   * @param data The binary data
   * @return The base64 encoded content of [data]
   */
  to_base64(data) = to_encoding(data, "base64")

  /**
   * Returns a hex encoded string of a binary [data].
   * @param data The binary data
   * @return The hex encoded content of [data]
   */
  to_hex(data) = to_encoding(data, "hex")

  /**
   * Returns a binary string of a binary [data].
   * @warning This encoding is deprecated.
   * @param data The binary data
   * @return The binary string content of [data]
   */
  to_binary(data) = to_encoding(data, "binary")

  /**
   * A generic of_string function.
   * @param encoding is the way where the string is encoded
   * @param data is string data to decode to a binary
   * @return The decoded binary of [data]
   */
  @private
  of_encoding(data, encoding) = %%bslBinary.of_encoding%%(data, encoding)

  /**
   * Convert a (non-encoded) string to its binary representation.
   * @param data The string to convert
   * @return The binary representation of [data]
   */
  of_string(data) = of_encoding(data, "utf8")

  /**
   * Convert a base64 encoded string to its binary representation.
   * @param data The string to convert
   * @return The binary representation of base64 decoded [data]
   */
  of_base64(data) = of_encoding(data, "base64")

  /**
   * Convert a hex encoded string to its binary representation.
   * @param data The string to convert
   * @return The binary representation of hex decoded [data]
   */
  of_hex(data) = of_encoding(data, "hex")

  /**
   * Convert a binary string to its binary representation.
   * @warning This encoding is deprecated.
   * @param data The string to convert
   * @return The binary representation of binary string [data]
   */
  of_binary(data) = of_encoding(data, "binary")

  /**
   * Returns the index of the first occurrence of [value] on [binary] after
   * the [start] position.
   * @param binary The binary container where to look
   * @param separator
   * @return The index of the first occurrence or -1 if [value] does not occurs.
   */
  indexOf(binary, value, start) =
    // TODO - Naive
    lb = Binary.length(binary)
    ls = Binary.length(value)
    rec aux(i, j) =
      if j == ls then i - j
      else if (i + ls - j > lb) then
        -1
      else if(Binary.get_int8(binary, i)==Binary.get_int8(value, j)) then
        aux(i+1, j+1)
      else if (j != 0) then
        aux(i-j+1, 0)
      else aux(i+1, 0)
    aux(start,0)

  /**
   * Splits a binary by a binary separator.
   * @param binary The binary to split.
   * @param separator The binary which separates chunks.
   */
  explode(binary, separator) =
    lb = Binary.length(binary)
    ls = Binary.length(separator)
    rec aux(start) =
      match indexOf(binary, separator, start)
      | -1 -> [Binary.get_binary(binary, start, lb - start)]
      | i -> [Binary.get_binary(binary, start, i - start) | aux(i+ls)]
    aux(0)

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
   * Sets the position of the cursor of the end. If the requested postion is
   * greater than the underlying buffer, the buffer can be reallocated.
   *
   * @param b The binary data.
   * @param pos Move the cursor of the end to this postion.
   * @return void
   */
  seek : binary, int -> void = %%BslBinary.resize%%

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
   * Add bytes to end of data.
   *
   * @param b The binary data.
   * @param len The number of bytes to add
   * @param v The value of the bytes (0-255)
   * @return void
   */
  add_fill : binary, int, int -> void = %%BslBinary.add_fill%%

  /**
   * Add a unicode character to the end of the data.
   *
   * @param b The binary data.
   * @param c The unicode character to add.
   * @return void
   */
  add_unicode : binary, Unicode.character -> void = %%BslBinary.add_unicode%%

  /**
   * Add a string to the end of the data.
   *
   * @param b The binary data.
   * @param str The string to add.
   * @return void
   */
  add_string : binary, string -> void = %%BslBinary.add_string%%
  add_stringr : binary, string -> outcome(void,string) = %%BslBinary.add_stringr%%

  /**
   * Add binary data to the end of the data.
   *
   * @param b The binary data.
   * @param nb New binary data to add.
   * @return void
   */
  add_binary : binary, binary -> void = %%BslBinary.add_binary%%
  add_binaryr : binary, binary -> outcome(void,string) = %%BslBinary.add_binaryr%%

  /**
   * Add a signed 8-bit int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (-128 to 127).
   * @return void
   */
  add_int8 : binary, int -> void = %%BslBinary.add_int8%%
  add_int8r : binary, int -> outcome(void,string) = %%BslBinary.add_int8r%%

  /**
   * Add an unsigned 8-bit int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (0 to 255).
   * @return void
   */
  add_uint8 : binary, int -> void = %%BslBinary.add_uint8%%
  add_uint8r : binary, int -> outcome(void,string) = %%BslBinary.add_uint8r%%

  /**
   * Add a signed 16-bit little-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (-32768 to 32767).
   * @return void
   */
  add_int16_le : binary, int -> void = %%BslBinary.add_int16_le%%
  add_int16_ler : binary, int -> outcome(void,string) = %%BslBinary.add_int16_ler%%

  /**
   * Add an unsigned 16-bit little-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (0 to 65535).
   * @return void
   */
  add_uint16_le : binary, int -> void = %%BslBinary.add_uint16_le%%
  add_uint16_ler : binary, int -> outcome(void,string) = %%BslBinary.add_uint16_ler%%

  /**
   * Add a signed 16-bit big-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (-32768 to 32767).
   * @return void
   */
  add_int16_be : binary, int -> void = %%BslBinary.add_int16_be%%
  add_int16_ber : binary, int -> outcome(void,string) = %%BslBinary.add_int16_ber%%

  /**
   * Add an unsigned 16-bit big-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (0 to 65535).
   * @return void
   */
  add_uint16_be : binary, int -> void = %%BslBinary.add_uint16_be%%
  add_uint16_ber : binary, int -> outcome(void,string) = %%BslBinary.add_uint16_ber%%

  /**
   * Add a signed 32-bit little-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (-2147483648 to 2147483647).
   * @return void
   */
  add_int32_le : binary, int -> void = %%BslBinary.add_int32_le%%
  add_int32_ler : binary, int -> outcome(void,string) = %%BslBinary.add_int32_ler%%

  /**
   * Add an unsigned 32-bit little-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (0 to 4294967295).
   * @return void
   */
  add_uint32_le : binary, int -> void = %%BslBinary.add_uint32_le%%
  add_uint32_ler : binary, int -> outcome(void,string) = %%BslBinary.add_uint32_ler%%

  /**
   * Add a signed 32-bit big-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (-2147483648 to 2147483647).
   * @return void
   */
  add_int32_be : binary, int -> void = %%BslBinary.add_int32_be%%
  add_int32_ber : binary, int -> outcome(void,string) = %%BslBinary.add_int32_ber%%

  /**
   * Add an unsigned 32-bit big-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (0 to 4294967295).
   * @return void
   */
  add_uint32_be : binary, int -> void = %%BslBinary.add_uint32_be%%
  add_uint32_ber : binary, int -> outcome(void,string) = %%BslBinary.add_uint32_ber%%

  /**
   * Add a signed 53-bit little-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (-9007199254740992 to 9007199254740991).
   * @return void
   */
  add_int53_le : binary, int -> void = %%BslBinary.add_int53_le%%
  add_int53_ler : binary, int -> outcome(void,string) = %%BslBinary.add_int53_ler%%

  /**
   * Add a signed 53-bit big-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Integer to add (-9007199254740992 to 9007199254740991).
   * @return void
   */
  add_int53_be : binary, int -> void = %%BslBinary.add_int53_be%%
  add_int53_ber : binary, int -> outcome(void,string) = %%BslBinary.add_int53_ber%%

  /**
   * Add an unsigned 64-bit little-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Int64 to add (0 to 18446744073709551615).
   * @return void
   */
  add_uint64_le : binary, int64 -> void = %%BslBinary.add_uint64_le%%
  add_uint64_ler : binary, int64 -> outcome(void,string) = %%BslBinary.add_uint64_ler%%

  /**
   * Add an unsigned 64-bit big-endian int to the end of the data.
   *
   * @param b The binary data.
   * @param i Int64 to add (0 to 18446744073709551615).
   * @return void
   */
  add_uint64_be : binary, int64 -> void = %%BslBinary.add_uint64_be%%
  add_uint64_ber : binary, int64 -> outcome(void,string) = %%BslBinary.add_uint64_ber%%

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
  add_float_ler : binary, float -> outcome(void,string) = %%BslBinary.add_float_ler%%

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
  add_float_ber : binary, float -> outcome(void,string) = %%BslBinary.add_float_ber%%

  /**
   * Add a 64-bit little-endian float to the end of the data.
   *
   * @param b The binary data.
   * @param f Float to add (+/-2.22507385850720138e-308 to +/-1.79769313486231571e+308).
   * @return void
   */
  add_double_le : binary, float -> void = %%BslBinary.add_double_le%%
  add_double_ler : binary, float -> outcome(void,string) = %%BslBinary.add_double_ler%%

  /**
   * Add a 64-bit big-endian float to the end of the data.
   *
   * @param b The binary data.
   * @param f Float to add (+/-2.22507385850720138e-308 to +/-1.79769313486231571e+308).
   * @return void
   */
  add_double_be : binary, float -> void = %%BslBinary.add_double_be%%
  add_double_ber : binary, float -> outcome(void,string) = %%BslBinary.add_double_ber%%

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
  get_stringr : binary, int, int -> outcome(string,string) = %%BslBinary.get_stringr%%

  /**
   * Read a section of the buffer back into binary data.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @param length Number of bytes to read.
   * @return untranslated binary data
   */
  get_binary : binary, int, int -> binary = %%BslBinary.get_binary%%
  get_binaryr : binary, int, int -> outcome(binary,string) = %%BslBinary.get_binaryr%%

  /**
   * Read a signed 8-bit int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (-128 to 127).
   */
  get_int8 : binary, int -> int = %%BslBinary.get_int8%%
  get_int8r : binary, int -> outcome(int,string) = %%BslBinary.get_int8r%%

  /**
   * Read an unsigned 8-bit int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (0 to 255).
   */
  get_uint8 : binary, int -> int = %%BslBinary.get_uint8%%
  get_uint8r : binary, int -> outcome(int,string) = %%BslBinary.get_uint8r%%

  /**
   * Read a signed 16-bit little-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (-32768 to 32767).
   */
  get_int16_le : binary, int -> int = %%BslBinary.get_int16_le%%
  get_int16_ler : binary, int -> outcome(int,string) = %%BslBinary.get_int16_ler%%

  /**
   * Read an unsigned 16-bit little-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (0 to 65535).
   */
  get_uint16_le : binary, int -> int = %%BslBinary.get_uint16_le%%
  get_uint16_ler : binary, int -> outcome(int,string) = %%BslBinary.get_uint16_ler%%

  /**
   * Read a signed 16-bit big-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (-32768 to 32767).
   */
  get_int16_be : binary, int -> int = %%BslBinary.get_int16_be%%
  get_int16_ber : binary, int -> outcome(int,string) = %%BslBinary.get_int16_ber%%

  /**
   * Read an unsigned 16-bit big-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (0 to 65535).
   */
  get_uint16_be : binary, int -> int = %%BslBinary.get_uint16_be%%
  get_uint16_ber : binary, int -> outcome(int,string) = %%BslBinary.get_uint16_ber%%

  /**
   * Read a signed 32-bit little-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (-2147483648 to 2147483647).
   */
  get_int32_le : binary, int -> int = %%BslBinary.get_int32_le%%
  get_int32_ler : binary, int -> outcome(int,string) = %%BslBinary.get_int32_ler%%

  /**
   * Read an unsigned 32-bit little-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (0 to 4294967295).
   */
  get_uint32_le : binary, int -> int = %%BslBinary.get_uint32_le%%
  get_uint32_ler : binary, int -> outcome(int,string) = %%BslBinary.get_uint32_ler%%

  /**
   * Read a signed 32-bit big-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (-2147483648 to 2147483647).
   */
  get_int32_be : binary, int -> int = %%BslBinary.get_int32_be%%
  get_int32_ber : binary, int -> outcome(int,string) = %%BslBinary.get_int32_ber%%

  /**
   * Read an unsigned 32-bit big-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (0 to 4294967295).
   */
  get_uint32_be : binary, int -> int = %%BslBinary.get_uint32_be%%
  get_uint32_ber : binary, int -> outcome(int,string) = %%BslBinary.get_uint32_ber%%

  /**
   * Read a signed 53-bit little-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (-9007199254740992 to 9007199254740991).
   */
  get_int53_le : binary, int -> int = %%BslBinary.get_int53_le%%
  get_int53_ler : binary, int -> outcome(int,string) = %%BslBinary.get_int53_ler%%

  /**
   * Read a signed 53-bit big-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Integer (-9007199254740992 to 9007199254740991).
   */
  get_int53_be : binary, int -> int = %%BslBinary.get_int53_be%%
  get_int53_ber : binary, int -> outcome(int,string) = %%BslBinary.get_int53_ber%%

  /**
   * Read an unsigned 64-bit little-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Int64 (0 to 18446744073709551615).
   */
  get_uint64_le : binary, int -> int64 = %%BslBinary.get_uint64_le%%
  get_uint64_ler : binary, int -> outcome(int64,string) = %%BslBinary.get_uint64_ler%%

  /**
   * Read an unsigned 64-bit big-endian int from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Int64 (0 to 18446744073709551615).
   */
  get_uint64_be : binary, int -> int64 = %%BslBinary.get_uint64_be%%
  get_uint64_ber : binary, int -> outcome(int64,string) = %%BslBinary.get_uint64_ber%%

  /**
   * Read a 32-bit little-endian float from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Float (approx 1.175494351e-38 to 3.402823466e38).
   */
  get_float_le : binary, int -> float = %%BslBinary.get_float_le%%
  get_float_ler : binary, int -> outcome(float,string) = %%BslBinary.get_float_ler%%

  /**
   * Read a 32-bit big-endian float from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Float (approx 1.175494351e-38 to 3.402823466e38).
   */
  get_float_be : binary, int -> float = %%BslBinary.get_float_be%%
  get_float_ber : binary, int -> outcome(float,string) = %%BslBinary.get_float_ber%%

  /**
   * Read a 64-bit little-endian float from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Float (+/-2.22507385850720138e-308 to +/-1.79769313486231571e+308).
   */
  get_double_le : binary, int -> float = %%BslBinary.get_double_le%%
  get_double_ler : binary, int -> outcome(float,string) = %%BslBinary.get_double_ler%%

  /**
   * Read a 64-bit big-endian float from the buffer.
   *
   * @param b The binary data.
   * @param start The starting position in the buffer.
   * @return Float (+/-2.22507385850720138e-308 to +/-1.79769313486231571e+308).
   */
  get_double_be : binary, int -> float = %%BslBinary.get_double_be%%
  get_double_ber : binary, int -> outcome(float,string) = %%BslBinary.get_double_ber%%

  /**
   * The default order of binary data.
   */
  order : order(binary, Order.default) = Order.make(ordering)

  /**
   * Return an ordering of two binary data.
   */
  ordering(bin1, bin2) =
    len1 = length(bin1)
    len2 = length(bin2)
    match Int.ordering(len1, len2)
    | {eq} ->
      rec aux(i) =
        if i == len1 then {eq}
        else
          match Int.ordering(get_int8(bin1, i), get_int8(bin2, i))
          | {eq} -> aux(i+1)
          | x -> x
      aux(0)
    | x -> x

  /**
   * Comparison of two binary data.
   */
  compare(bin1, bin2) = ordering(bin1, bin2) <: Order.comparison

  /**
   * Checks the equality of two binary
   */
  equals(bin1, bin2) = ordering(bin1, bin2) == {eq}

  /**
   * Turns a binary iterator to binary
   * @param iter A binary iterator
   */
  of_iter(iter:iter(binary)) =
    (l, bins) =
      Iter.fold((b, (l, acc) ->
        (length(b) + l, [b|acc])
      ), iter, (0, []))
    bin = Binary.create(l)
    do List.rev_iter(Binary.add_binary(bin, _), bins)
    bin

}}


