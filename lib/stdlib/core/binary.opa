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
 * @stability UNSTABLE
 */

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

//TODO:This should be [external]. However, for the moment, we rely on [static_file_include] and we just can't.
@opacapi
@abstract
type binary = external

/**
 * {1 Interface}
 */

//string_of_binary(x:binary) = @unsafe_cast(x): string
//binary_of_string(x:string) = @unsafe_cast(x): binary
binary_of_string : string -> binary = %%BslBinary.binary_of_string%%
string_of_binary : binary -> string = %%BslBinary.string_of_binary%%

@opacapi
bin_of_base64(x:string):binary = %%bslPervasivesServer.bin_of_base64%%(x)

Binary = {{
  create : int -> binary = %%BslBinary.create%%
  length : binary -> int = %%BslBinary.length%%
  resize : binary, int -> void = %%BslBinary.resize%%
  clear : binary -> void = %%BslBinary.clear%%
  trim : binary -> void = %%BslBinary.trim%%
  reset : binary -> void = %%BslBinary.reset%%
  add_string : binary, string -> void = %%BslBinary.add_string%%
  add_binary : binary, binary -> void = %%BslBinary.add_binary%%
  add_int8 : binary, int -> void = %%BslBinary.add_int8%%
  add_uint8 : binary, int -> void = %%BslBinary.add_uint8%%
  add_int16_le : binary, int -> void = %%BslBinary.add_int16_le%%
  add_uint16_le : binary, int -> void = %%BslBinary.add_uint16_le%%
  add_int16_be : binary, int -> void = %%BslBinary.add_int16_be%%
  add_uint16_be : binary, int -> void = %%BslBinary.add_uint16_be%%
  add_int32_le : binary, int -> void = %%BslBinary.add_int32_le%%
  add_uint32_le : binary, int -> void = %%BslBinary.add_uint32_le%%
  add_int32_be : binary, int -> void = %%BslBinary.add_int32_be%%
  add_uint32_be : binary, int -> void = %%BslBinary.add_uint32_be%%
  add_float_le : binary, float -> void = %%BslBinary.add_float_le%%
  add_float_be : binary, float -> void = %%BslBinary.add_float_be%%
  add_double_le : binary, float -> void = %%BslBinary.add_double_le%%
  add_double_be : binary, float -> void = %%BslBinary.add_double_be%%
  get_string : binary, int, int -> string = %%BslBinary.get_string%%
  get_binary : binary, int, int -> binary = %%BslBinary.get_binary%%
  get_int8 : binary, int -> int = %%BslBinary.get_int8%%
  get_uint8 : binary, int -> int = %%BslBinary.get_uint8%%
  get_int16_le : binary, int -> int = %%BslBinary.get_int16_le%%
  get_uint16_le : binary, int -> int = %%BslBinary.get_uint16_le%%
  get_int16_be : binary, int -> int = %%BslBinary.get_int16_be%%
  get_uint16_be : binary, int -> int = %%BslBinary.get_uint16_be%%
  get_int32_le : binary, int -> int = %%BslBinary.get_int32_le%%
  get_uint32_le : binary, int -> int = %%BslBinary.get_uint32_le%%
  get_int32_be : binary, int -> int = %%BslBinary.get_int32_be%%
  get_uint32_be : binary, int -> int = %%BslBinary.get_uint32_be%%
  get_float_le : binary, int -> float = %%BslBinary.get_float_le%%
  get_float_be : binary, int -> float = %%BslBinary.get_float_be%%
  get_double_le : binary, int -> float = %%BslBinary.get_double_le%%
  get_double_be : binary, int -> float = %%BslBinary.get_double_be%%
}}

