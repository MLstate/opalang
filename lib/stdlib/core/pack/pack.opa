/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import stdlib.core.args

/**
 * Pack binary data into buffers.
 *
 * Important note: the buffers used by this module are currently founded upon the
 * Buffer module and are therefore imperative, non-serializable and are not
 * valid across thread boundaries.
 *
 * @author Norman Scaife
 * @review Quentin Bourgerie
 * @destination public
 */

/**
 * {1 About this module}
 *
 * This module provides a convenient method for defining structured binary data.
 * You define the structure of the data with a [Pack.data] type, for example:
 *
 * [data = [{Cstring="abc"}, {Int=123}, {Bool=true}, {Pad}]]
 *
 * This defines a binary data of a null-terminated string, followed by an integer
 * which uses the default endianness, signedness and width, followed by a single
 * byte representing a bool, followed by a single null byte of padding.
 *
 * To pack this data into a binary type, we would just do:
 *
 * [bin = Pack.Encode.pack(data)]
 *
 * which would return the binary data (the defaults for Int are big-endian, signed 32-bits):
 *
 * [0000 61 62 63 00 00 00 00 7b 01 00                    abc....{..]
 *
 * Given this binary data, you can decode the buffer using:
 *
 * [decoded_data = Pack.Decode.unpack(data, bin, 0)]
 *
 * (the values in tha data specification can be anything but the types have to match)
 * which will return a [Pack.data] type exactly as above.
 *
 * The specification includes directives which can be embedded in the data, for example if you had
 * little-endian, unsigned 16-bit integers, you could specify: [[{Le}, {Unsigned}, {S}, ...]]
 *
 * {S} stands for "short", ie. 16-bits.  For [{Int}] and [{String}], you can add the
 * directives locally, for example: [{Int=1; size={Ll}; signed=false}].
 *
 * The directives are retained by [Pack.Decode.unpack] so you can reuse the returned data type.
 * To remove these directives, just leaving items with data, you can call [Pack.Decode.clean].
 *
 * {1 Where should I start?}
 *
 * Start by looking at the [Pack.u] type which contains all the directives and
 * basic types.  In general, the functions in the Encode and Decode modules shadow
 * the rows in this type, with aggregated equivalents for convenience, for example,
 * a size-prefixed string whose size is indicated by a big-endian, 16-bit integer,
 * will be "string_s_be".
 *
 * The main modules are Encode and Decode which are the mirror-image of each other but there
 * is also a composable, functional interface for decoding called Unser.  This has some
 * additional functions for Opa-specific types such as options and references.  The mirror
 * image of this, the Ser module, is mostly covered by Encode but Ser provides the additional
 * functions to mirror Unser.
 *
 * {1 What if I need more?}
 *
 * There are probably many other types which could be supported but you can write your own
 * packing, unpacking routines using the routines from the underlying Binary module.
 *
 */

/**
 * {1 Types defined in this module}
 */

/**
 * The type of binary data, [Pack.t] is just binary.
**/
type Pack.t = binary

/**
 * Type representing size directives: [{B}]=8-bits, [{S}]=16-bits, [{L}]=32-bits, [{Ll}]=64-bits
**/
type Pack.s = {B} / {S} / {L} / {Ll}

/**
 * [Pack.codes] is for data-dependent encode/decode.  Data is packed according to
 * the codes: [{Coded=[(code1,data1)]}] will generate [<code1><data1>] in the binary output.
 *
 * On decode, there can be more than one code, for example [{Coded=[(code1,data1),(code2,data2),...]}] but
 * note that only the first code in the list is used.
 * The code is read in and the following data is decoded according to the matching code-data pair.
 * For full generality, the codes are pack items, so you can index your data with, for example ints, strings,
 * bools or even more complex data such as lists.
 *
 * Note: don't attempt to use directives or padding/boundary items as codes.
 **/
type Pack.codes = list((Pack.u, Pack.data))

/**
 * [Pack.u] is the main type defining primitive items to be encoded/decoded.
 *
 * Directives are: [{Be}] and [{Le}] for endianness, [{Signed}] and [{Unsigned}] for
 * signedness of integers and [{B}], [{S}], [{L}] and [{Ll}] for integer width (which are
 * reproduced in the [Pack.s] type).
 *
 * Basic data types are: [{Byte}] for 8-bit integers, [{Short}] for 16-bit integers,
 * [{Long}] for 32-bit integers, [{Longlong}] for 64-bit integers (in the binary output, the source
 * integers will be the underlying native ints which are not, in general, 64-bits), [{Int}] for generic
 * integers (controlled by the directives), [{Int64}] for 64-bit integers on the Opa side (there
 * is a basic Int64 module implementation),
 * [{Cstring}] for null-terminated strings, [{String}] for generic, size-prefixed strings,
 * where the size data is controlled by the directives, [{Bool}] for a single-byte value
 * representing a bool, [{Float32}] for 32-bit floats and [{Float}] for 64-bit floats.
 *
 * Aggregate data types are: [{List}] for lists of data, [{Array}] for arrays and [{Coded}]
 * for data-dependent encoding.
 *
 * Other utility types include: [{Pad}] for a single null-padding byte, [{Padn}] for a series
 * of null-padding bytes, [{Bound}] to fill the current binary data up to a given boundary,
 * [{Char}] which is just a single-character string and [{Void}] which is present for cosmetic purposes.
 **/
type Pack.u =
   {Be}
 / {Le}
 / {Signed}
 / {Unsigned}
 / {B}
 / {S}
 / {L}
 / {Ll}
 / {Char:string}
 / {Byte:int}
 / {Short:int}
 / {Long:int}
 / {Longlong:int}
 / {Int64:int64}
 / {Int:int}
 / {Int:int; size:Pack.s}
 / {Int:int; signed:bool}
 / {Int:int; size:Pack.s; signed:bool}
 / {Int:int; le:bool}
 / {Int:int; le:bool; size:Pack.s}
 / {Int:int; le:bool; signed:bool}
 / {Int:int; le:bool; size:Pack.s; signed:bool}
 / {Pad}
 / {Padn:int}
 / {Bound:int}
 / {Void}
 / {Bool:bool}
 / {Cstring:string}
 / {String:string}
 / {String:string; size:Pack.s}
 / {String:string; le:bool}
 / {String:string; le:bool; size:Pack.s}
 / {Float32:float}
 / {Float:float}
 / {Coded:Pack.codes}
 / {List:(Pack.data,list(Pack.data))}
 / {Array:(Pack.data,llarray(Pack.data))}
// Other possibilities...
// / {Record:list((field_name,field_type,data))} // <-- problem, type vars

/**
 * [Pack.data] is the main specification type but it is just a list of the [Pack.u]
 * item types.
 **/
type Pack.data = list(Pack.u)

/**
 * [Pack.input] is a specialised type which wraps the input binary plus
 * a position.  It is used by the Unser module.
 **/
type Pack.input = { binary:Pack.t; pos:int }

/**
 * [Pack.options] is the type of options which is used by the main [pack] and
 * [unpack] functions.
 */
type Pack.options = {signed : bool; endian : bool; size : Pack.s}

/**
 * [Pack.result] is the result type for the Unser module.
 **/
type Pack.result('a) = outcome((Pack.input,'a),string)

/**
 * {1 Interface}
 */
Pack = {{

  // For debugging buffer contents
  @private memdump = (%% BslPervasives.memdump %%: string -> string)

  /** Convenience value for little Endian values **/
  littleEndian = true

  /** Convenience value for big Endian values **/
  bigEndian = false

  /** Convenience value for meaningless Endian values, eg. bytes **/
  noEndian = false

  /** Convenience value for signed integers **/
  signedInts = true

  /** Convenience value for unsigned integers **/
  unsignedInts = false

  /** Convenience value for 8-bit integers **/
  byteSize = {B} : Pack.s

  /** Convenience value for 16-bit integers **/
  shortSize = {S} : Pack.s

  /** Convenience value for 32-bit integers **/
  longSize = {L} : Pack.s

  /** Convenience value for 64-bit integers **/
  longlongSize = {Ll} : Pack.s

  /**
   * A generator of options which can be overridden by the command line.
   * Default options are [init] or overridden by the command line :
   * - --default-endian:[name] : To set the default endian
   * - --default-ints  : To set the default ints
   * - --default-size : To set the default size
   *
   * @param init The initial options
   * @param name The name of the pack options
   */
  command_line_options(init:Pack.options, name):Pack.options =
    args = {
      title = "Pack options for {name}"
      ~init
      anonymous = []
      parsers = [
        CommandLine.case(["--pack-endian:{name}}"],
                [("little",Pack.littleEndian),
                 ("big",   Pack.bigEndian)],
                "Default endian", "little, big"
               )(endian,p -> {p with ~endian}),

        CommandLine.case(["--pack-signed:{name}"],
                [("signed",  Pack.signedInts),
                 ("unsigned",Pack.unsignedInts)],
                "Default ints", "signed, unsigned"
               )(signed,p -> {p with ~signed}),

        CommandLine.case(["--pack-size:{name}"],
                [("byte",    Pack.byteSize),
                 ("short",   Pack.shortSize),
                 ("long",    Pack.longSize),
                 ("longlong",Pack.longlongSize)],
                "Default size","byte, short, long, longlong"
               )(size,p -> {p with ~size}),
      ]
    }
    CommandLine.filter(args)

  /**
   * Default options are [{signed = signedInts; endian = bigEndian; size =
   * longSize}]
   */
  default_options:Pack.options = {signed = signedInts; endian = bigEndian; size = longSize}

  /** size of sized items in bytes **/
  sizesize(s:Pack.s): int = match s with | {B} -> 1 | {S} -> 2 | {L} -> 4 | {Ll} -> 8

#<Ifstatic:OPA_BACKEND_QMLJS>
  @private llsize = 0x001fffffffffffff // 53 bits
#<Else>
  @private llsize = 0x3fffffffffffffff // 62 bits
#<End>

  /** maximum (unsigned) value for int **/
  sizemax(s:Pack.s) : int = match s with | {B} -> 0xff | {S} -> 0xffff | {L} -> 0xffffffff | {Ll} -> llsize

  /** names of the sizes, eg. [{B}] is "byte" **/
  sizename(s:Pack.s) : string = match s with | {B} -> "byte" | {S} -> "short" | {L} -> "long" | {Ll} -> "longlong"

  /** make size item (ie. for preceding strings or arrays), returns failure on out-of-range **/
  mksize(s:Pack.s, i:int) : outcome(Pack.u,string) =
    match s with
    | {B} -> if i < 0 || i > 0xff then {failure="Pack.mksize: int {i} too big for byte size"} else {success={Byte=i}}
    | {S} -> if i < 0 || i > 0xffff then {failure="Pack.mksize: int {i} too big for short size"} else {success={Short=i}}
    | {L} -> if i < 0 || i > 0xffffffff then {failure="Pack.mksize: int {i} too big for long size"} else {success={Long=i}}
    | {Ll} -> {success={Longlong=i}}

  /** recover size from item, failure if not an integer type **/
  getsize(u:Pack.u) : outcome(int,string) =
    match u with
    | {~Byte} -> {success=Byte}
    | {~Short} -> {success=Short}
    | {~Long} -> {success=Long}
    | {~Longlong} -> {success=Longlong}
    | {~Int; ...} -> {success=Int}
    | {Int64=i} -> {success=Int64.to_int(i)}
    | _ -> {failure="Pack.getsize: size {u} is not int"}

  /** boundary computation **/
  bound(len, bnd) = if bnd <= 0 then 0 else rem = mod(len, bnd) if rem == 0 then 0 else (bnd - rem)

  /** test if item is valid for Coded codes **/
  valid_code(u:Pack.u) : bool =
    match u with
    | {Be} -> false
    | {Le} -> false
    | {Signed} -> false
    | {Unsigned} -> false
    | {B} -> false
    | {S} -> false
    | {L} -> false
    | {Ll} -> false
    | {Char=_} -> true
    | {Byte=_} -> true
    | {Short=_} -> true
    | {Long=_} -> true
    | {Longlong=_} -> true
    | {Int=_; ...} -> true
    | {Int64=_} -> true
    | {Pad} -> false
    | {Padn=_} -> false
    | {Bound=_} -> false
    | {Void} -> true
    | {Bool=_} -> true
    | {Cstring=_} -> true
    | {String=_; ...} -> true
    | {Float32=_} -> true
    | {Float=_} -> true
    | {Coded=_} -> true
    | {List=_} -> true
    | {Array=_} -> true

  /** Debug, memdump first 32-bytes of current input
   * @param name string to prefix the output
   * @param input the current Pack.input value
   **/
  @expand pinput(_name:string, _input:Pack.input) =
    #<Ifstatic:OPA_PACK_DEBUG>
      data = Binary.get_string(input.binary,input.pos,Int.min(32,Binary.length(input.binary)-input.pos))
      Log.debug("{name}: input=\n{memdump(data)}")
    #<Else>
      void
    #<End>

  /**
   * {2 Encode data}
   *
   * This module provides functions to encode data in binary types.
   */
  Encode = {{

    /** Encode char, actually just a single 8-bit character string, failure if multi-byte **/
    char(buf:Pack.t, c:string) : outcome(void,string) =
      b = String.byte_at_unsafe(0,c)
      not8 = b < 0 || b > 0xff
      if ((String.length(c) != 1) || not8)
      then {failure="Pack.Encode.char: multi-byte char \"{c}\""}
      else {success=Binary.add_uint8(buf, b)}

    /** Encode signed 8-bit int, failure if out-of-range **/
    octet(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x80 || i > 0x7f)
      then {failure="Pack.Encode.byte: out of range {i}"}
      else {success=Binary.add_int8(buf, i)}

    /** Encode unsigned 8-bit int, failure if out-of-range **/
    uoctet(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xff)
      then {failure="Pack.Encode.byte: out of range {i}"}
      else {success=Binary.add_uint8(buf, i)}

    /**
     * Encode 8-bit int, failure if out-of-range
     *
     * @param buf binary data to encode into.
     * @param signed signed if true
     * @param i integer
     **/
    byte(buf:Pack.t, signed:bool, b:int): outcome(void,string) =
      if signed then octet(buf, b) else uoctet(buf, b)

    /** Encode signed 16-bit big-endian int, failure if out-of-range **/
    short_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x8000 || i > 0xffff)
      then {failure="Pack.Encode.short_be: out of range {i}"}
      else {success=Binary.add_int16_be(buf, i)}

    /** Encode signed 16-bit little-endian int, failure if out-of-range **/
    short_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x8000 || i > 0xffff)
      then {failure="Pack.Encode.short_le: out of range {i}"}
      else {success=Binary.add_int16_le(buf, i)}

    /** Encode unsigned 16-bit big-endian int, failure if out-of-range **/
    ushort_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffff)
      then {failure="Pack.Encode.ushort_be: out of range {i}"}
      else {success=Binary.add_uint16_be(buf, i)}

    /** Encode unsigned 16-bit little-endian int, failure if out-of-range **/
    ushort_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffff)
      then {failure="Pack.Encode.ushort_le: out of range {i}"}
      else {success=Binary.add_uint16_le(buf, i)}

    /**
     * Encode 16-bit int, failure if out-of-range
     *
     * @param buf binary data to encode into.
     * @param le little-endian if true
     * @param signed signed if true
     * @param i integer
     **/
    short(buf:Pack.t, le:bool, signed:bool, s:int): outcome(void,string) =
      match (le, signed) with
      | ({false},{false}) -> ushort_be(buf, s)
      | ({true},{false}) -> ushort_le(buf, s)
      | ({false},{true}) -> short_be(buf, s)
      | ({true},{true}) -> short_le(buf, s)

    /** Encode signed 32-bit big-endian int, failure if out-of-range **/
    long_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x80000000 || i > 0x7fffffff)
      then {failure="Pack.Encode.long_be: out of range {i}"}
      else {success=Binary.add_int32_be(buf, i)}

    /** Encode signed 32-bit little-endian int, failure if out-of-range **/
    long_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x80000000 || i > 0x7fffffff)
      then {failure="Pack.Encode.long_le: out of range {i}"}
      else {success=Binary.add_int32_le(buf, i)}

    /** Encode unsigned 32-bit big-endian int, failure if out-of-range **/
    ulong_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffffffff)
      then {failure="Pack.Encode.ulong_be: out of range {i}"}
      else {success=Binary.add_uint32_be(buf, i)}

    /** Encode unsigned 32-bit little-endian int, failure if out-of-range **/
    ulong_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffffffff)
      then {failure="Pack.Encode.ulong_le: out of range {i}"}
      else {success=Binary.add_uint32_le(buf, i)}

    /**
     * Encode 32-bit int, failure if out-of-range
     *
     * @param buf binary data to encode into.
     * @param le little-endian if true
     * @param signed signed if true
     * @param i integer
     **/
    long(buf:Pack.t, le:bool, signed:bool, l:int): outcome(void,string) =
      match (le, signed) with
      | ({false},{false}) -> ulong_be(buf, l)
      | ({true},{false}) -> ulong_le(buf, l)
      | ({false},{true}) -> long_be(buf, l)
      | ({true},{true}) -> long_le(buf, l)

    /** Encode signed 53-bit big-endian int, failure if out-of-range **/
    longlong_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x20000000000000 || i > 0x1fffffffffffff)
      then {failure="Pack.Encode.ulonglong_be: out of range {i}"}
      else {success=Binary.add_int53_be(buf, i)}

    /** Encode signed 53-bit little-endian int, failure if out-of-range **/
    longlong_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x20000000000000 || i > 0x1fffffffffffff)
      then {failure="Pack.Encode.ulonglong_le: out of range {i}"}
      else {success=Binary.add_int53_le(buf, i)}

    /**
     * Encode 53-bit int, failure if out-of-range
     *
     * @param buf binary data to encode into.
     * @param le little-endian if true
     * @param i integer
     **/
    longlong(buf:Pack.t, le:bool, b:int): outcome(void,string) =
      if le then longlong_le(buf, b) else longlong_be(buf, b)

    /** Encode unsigned 64-bit big-endian int **/
    int64_be(buf:Pack.t, i:int64) : outcome(void,string) =
      {success=Binary.add_uint64_be(buf, i)}

    /** Encode unsigned 64-bit little-endian int **/
    int64_le(buf:Pack.t, i:int64) : outcome(void,string) =
      {success=Binary.add_uint64_le(buf, i)}

    /**
     * Encode 64-bit int
     *
     * @param buf binary data to encode into.
     * @param le little-endian if true
     * @param i int64
     **/
    int64(buf:Pack.t, le:bool, b:int64): outcome(void,string) =
      if le then int64_le(buf, b) else int64_be(buf, b)

    /** Encode single null-byte padding **/
    pad(buf:Pack.t) : outcome(void,string) =
      octet(buf, 0)

    /**
     * Encode null-byte padding
     * @param n number of bytes of padding
     **/
    padn(buf:Pack.t, n:int) : outcome(void,string) =
      if n <= 0
      then {success=void}
      else {success=Binary.add_string(buf, String.repeat(n, String.of_byte_unsafe(0)))}

    /**
     * Add null-byte padding to make up to given boundary.
     * @param n boundary value
     **/
    boundary(buf:Pack.t, n:int) : outcome(void,string) =
      if n <= 0
      then {success=void}
      else
        padding = bound(Binary.length(buf),n)
        do Binary.add_string(buf, String.repeat(padding, String.of_byte_unsafe(0)))
        {success=void}

    /**
     * Encode boolean as either 8-bit 0x00 or 0x01
     * @param b bool
     **/
    bool(buf:Pack.t, b:bool) : outcome(void,string) =
      octet(buf, if b then 1 else 0)

    /**
     * Encode null-terminated string
     * @param str string
     **/
    cstring(buf:Pack.t, str:string) : outcome(void,string) =
      do Binary.add_string(buf, str)
      octet(buf, 0)

    /**
     * Encode size-prefixed string.
     * The format of the size prefix is determined by the le and size parameters (always unsigned).
     * This function will fail if the string length is greater than the maximum size.
     *
     * @param buf the binary data to append the string to
     * @param le true=little endian, false=big endian
     * @param size the width of the size prefix
     * @param str string
     **/
    string(buf:Pack.t, le:bool, size:Pack.s, str:string): outcome(void,string) =
      len = String.length(str)
      if len > sizemax(size)
      then {failure="Pack.Encode.string: string too long for {sizename(size)} length \"{String.sub(0,30,str)^"..."}\""}
      else
        match
          match size with
          | {B} -> uoctet(buf, len)
          | {S} -> if le then ushort_le(buf, len) else ushort_be(buf, len)
          | {L} -> if le then ulong_le(buf, len) else ulong_be(buf, len)
          | {Ll} -> if le then longlong_le(buf, len) else longlong_be(buf, len)
        with
        | {~failure} -> {~failure}
        | {success=_} -> {success=Binary.add_string(buf, str)}

    /** Encode byte-prefixed string **/
    string_b(buf:Pack.t, str:string) : outcome(void,string) = string(buf, noEndian, {B}, str)

    /** Encode short-prefixed string.
     * @param le endianness of prefix
     **/
    string_s(buf:Pack.t, le:bool, str:string) : outcome(void,string) = string(buf, le, {S}, str)

    /** Encode little-endian short-prefixed string **/
    string_s_le(buf:Pack.t, str:string) : outcome(void,string) = string(buf, littleEndian, {S}, str)

    /** Encode big-endian short-prefixed string **/
    string_s_be(buf:Pack.t, str:string) : outcome(void,string) = string(buf, bigEndian, {S}, str)

    /** Encode long-prefixed string.
     * @param le endianness of prefix
     **/
    string_l(buf:Pack.t, le:bool, str:string) : outcome(void,string) = string(buf, le, {L}, str)

    /** Encode little-endian long-prefixed string **/
    string_l_le(buf:Pack.t, str:string) : outcome(void,string) = string(buf, littleEndian, {L}, str)

    /** Encode big-endian long-prefixed string **/
    string_l_be(buf:Pack.t, str:string) : outcome(void,string) = string(buf, bigEndian, {L}, str)

    /* Encode longlong-prefixed string.
     * @param le endianness of prefix
     **/
    string_ll(buf:Pack.t, le:bool, str:string) : outcome(void,string) = string(buf, le, {Ll}, str)

    /** Encode little-endian longlong-prefixed string **/
    string_ll_le(buf:Pack.t, str:string) : outcome(void,string) = string(buf, littleEndian, {Ll}, str)

    /** Encode big-endian longlong-prefixed string **/
    string_ll_be(buf:Pack.t, str:string) : outcome(void,string) = string(buf, bigEndian, {Ll}, str)

    /** Encode big-endian 32-bit float **/
    float_be(buf:Pack.t, f:float) : outcome(void,string) =
      {success=Binary.add_float_be(buf, f)}

    /** Encode little-endian 32-bit float **/
    float_le(buf:Pack.t, f:float) : outcome(void,string) =
      {success=Binary.add_float_le(buf, f)}

    /** Encode 32-bit float.
     * @param le endianness
     **/
    float(buf:Pack.t, le:bool, f:float): outcome(void,string) =
      if le then float_le(buf, f) else float_be(buf, f)

    /** Encode big-endian 64-bit float **/
    double_be(buf:Pack.t, f:float) : outcome(void,string) =
      {success=Binary.add_double_be(buf, f)}

    /** Encode little-endian 64-bit float **/
    double_le(buf:Pack.t, f:float) : outcome(void,string) =
      {success=Binary.add_double_le(buf, f)}

    /** Encode 64-bit float.
     * @param le endianness
     **/
    double(buf:Pack.t, le:bool, f:float): outcome(void,string) =
      if le then double_le(buf, f) else double_be(buf, f)

    /** Encode Coded data.
     *
     * Since this function is out of context and packing data can change the
     * endianness, signedness or integer width, you have to pass these in.
     * The return value includes the final context.
     *
     * @param buf the binary data to pack into
     * @param le initial endianness (true=little endian)
     * @param signed initial signedness (true=signed)
     * @param size the initial width for integers
     * @param code the code item
     * @param data the data to pack
     * @return the final endianness, signedness and integer width plus the usual outcome
     **/
    coded(buf:Pack.t, le:bool, signed:bool, size:Pack.s, code:Pack.u, data:Pack.data)
          : (bool, bool, Pack.s, outcome(void,string)) =
      match pack_u(buf, le, signed, size, code) with
      | (le, signed, size, {success=_}) -> pack_data(buf, le, signed, size, data)
      | failure -> failure

    // same item type
    @private same_u(u1:Pack.u, u2:Pack.u): bool =
      match (u1,u2) with
      | ({Be},{Be}) -> true
      | ({Le},{Le}) -> true
      | ({Signed},{Signed}) -> true
      | ({Unsigned},{Unsigned}) -> true
      | ({B},{B}) -> true
      | ({S},{S}) -> true
      | ({L},{L}) -> true
      | ({Ll},{Ll}) -> true
      | ({Char=_},{Char=_}) -> true
      | ({Byte=_},{Byte=_}) -> true
      | ({Short=_},{Short=_}) -> true
      | ({Long=_},{Long=_}) -> true
      | ({Longlong=_},{Longlong=_}) -> true
      | ({Int=_},{Int=_}) -> true
      | ({Int=_; size=s1},{Int=_; size=s2}) -> s1 == s2
      | ({Int=_; signed=sg1},{Int=_; signed=sg2}) -> sg1 == sg2
      | ({Int=_; size=s1; signed=sg1},{Int=_; size=s2; signed=sg2}) -> s1 == s2 && sg1 == sg2
      | ({Int=_; le=le1},{Int=_; le=le2}) -> le1 == le2
      | ({Int=_; le=le1; size=s1},{Int=_; le=le2; size=s2}) -> le1 == le2 && s1 == s2
      | ({Int=_; le=le1; signed=sg1},{Int=_; le=le2; signed=sg2}) -> le1 == le2 && sg1 == sg2
      | ({Int=_; le=le1; size=s1; signed=sg1},{Int=_; le=le2; size=s2; signed=sg2}) ->
         leseq = le1 == le2 && s1 == s2 // non-lazy semantic???
         leseq && sg1 == sg2
      | ({Int64=_},{Int64=_}) -> true
      | ({Pad},{Pad}) -> true
      | ({Padn=_},{Padn=_}) -> true
      | ({Bound=_},{Bound=_}) -> true
      | ({Void},{Void}) -> true
      | ({Bool=_},{Bool=_}) -> true
      | ({Cstring=_},{Cstring=_}) -> true
      | ({String=_},{String=_}) -> true
      | ({String=_; size=s1},{String=_; size=s2}) -> s1 == s2
      | ({String=_; le=le1},{String=_; le=le2}) -> le1 == le2
      | ({String=_; le=le1; size=s1},{String=_; le=le2; size=s2}) -> le1 == le2 && s1 == s2
      | ({Float32=_},{Float32=_}) -> true
      | ({Float=_},{Float=_}) -> true
      | ({Coded=_},{Coded=_}) -> true
      | ({List=_},{List=_}) -> true
      | ({Array=_},{Array=_}) -> true
      | (_,_) -> false

    // same data type
    @private same_data(d1:Pack.data, d2:Pack.data): bool =
      match List.for_all2((u1, u2 -> same_u(u1,u2)),d1,d2) with
      | {result=tf} -> tf
      | _ -> false

    /** Encode size-prefixed list of data.
     *
     * Since this function is out of context and packing data can change the
     * endianness, signedness or integer width, you have to pass these in.
     * The return value includes the final context.
     *
     * The characteristics of the prefix (endianness, width) are determined by
     * the initial values.  If your data has different characteristics from the prefix,
     * you will have to include directives in the data.
     *
     * @param buf the binary data to pack into
     * @param le initial endianness (true=little endian)
     * @param signed initial signedness (true=signed)
     * @param size the initial width for integers
     * @param typ a proforma data value which indicates the types of all the elements,
                  all the elements in the list must be of this type.
     * @param data a list of data values to encode
     * @return the final endianness, signedness and integer width plus the usual outcome
     **/
    list(buf:Pack.t, le:bool, signed:bool, s:Pack.s, typ:Pack.data, data:list(Pack.data))
         : (bool, bool, Pack.s, outcome(void,string)) =
      match mksize(s, List.length(data)) with
      | {success=size} ->
        match pack_u(buf, le, signed, s, size) with
        | (le, signed, size, {success=_}) ->
           rec aux(le, signed, size, l) =
             match l with
             | [] -> (le, signed, size, {success=void})
             | [data|t] ->
                if same_data(typ, data)
                then
                  match pack_data(buf, le, signed, size, data) with
                  | (le, signed, size, {success=_}) -> aux(le, signed, size, t)
                  | failure -> failure
                  end
                else (le, signed, size, {failure="Pack.Encode.list: non-matching list elements"})
             end
           aux(le, signed, size, data)
        | failure -> failure
        end
      | {~failure} -> (le, signed, s, {~failure})

    // pack item length
    @private packitemsize(s:Pack.s, u:Pack.u) : (Pack.s, int, int) =
      match u with
      | {Be} -> (s,0,0)
      | {Le} -> (s,0,0)
      | {Signed} -> (s,0,0)
      | {Unsigned} -> (s,0,0)
      | {B} -> ({B},0,0)
      | {S} -> ({S},0,0)
      | {L} -> ({L},0,0)
      | {Ll} -> ({Ll},0,0)
      | {Char=_} -> (s,1,0)
      | {Byte=_} -> (s,1,0)
      | {Short=_} -> (s,2,0)
      | {Long=_} -> (s,4,0)
      | {Longlong=_} -> (s,8,0)
      | {Int=_; ~size; ...} -> (s,sizesize(size),0)
      | {Int=_; ...} -> (s,sizesize(s),0)
      | {Int64=_} -> (s,8,0)
      | {Pad} -> (s,1,0)
      | {~Padn} -> (s,Padn,0)
      | {~Bound} -> (s,0,Bound)
      | {Void} -> (s,1,0)
      | {Bool=_} -> (s,1,0)
      | {Cstring=str} -> (s,String.length(str)+1,0)
      | {String=str; ~size; ...} -> (s,sizesize(size)+String.length(str),0)
      | {String=str; ...} -> (s,sizesize(s)+String.length(str),0)
      | {Float32=_} -> (s,4,0)
      | {Float=_} -> (s,8,0)
      | {Coded=[]} -> (s,0,0) // Can't pack nothing
      | {Coded=[(code,data)]} ->
         if valid_code(code)
         then (s,icnt,_) = packitemsize(s,code) (s,dcnt) = packdatasize(s,data) (s, icnt + dcnt, 0)
         else (s,0,0) // Bad code
      | {Coded=_} -> (s,0,0) // Will generate error
      | {List=(_,l)} -> List.fold((data, (s,cnt,bnd) -> (s,dcnt) = packdatasize(s, data) (s, cnt+dcnt, bnd)),l,(s,sizesize(s),0))
      | {Array=(_,a)} ->
         LowLevelArray.fold((data, (s,cnt,bnd) -> (s,dcnt) = packdatasize(s, data) (s, cnt+dcnt, bnd)),a,(s,sizesize(s),0))

    // predict pack length
    @private packdatasize(s:Pack.s, data:Pack.data) : (Pack.s, int) =
      List.fold((u, (s, len) -> match packitemsize(s, u) with | (s,size,bnd) -> (s,len+size+bound(len,bnd))), data, (s,0))

    /**
     * Predict pack length.  Given a data element, this function will return
     * the exact size of the binary data needed to pack it.
     *
     * Note that this has to simulate a packing  without actually inserting the
     * data and so is computationally quite expensive.
     *
     * @param data the data to be sized
     * @return the size in bytes of the resulting packing
     **/
    packlen(data:Pack.data) : int =
      (List.fold((u, (s, len) ->
                  match packitemsize(s, u) with
                  | (s,size,bnd) -> (s,len+size+bound(len,bnd))), data, (default_options.size,0))).f2

    // missing from LowLevelArray?
    @private a2l(a:llarray('a)) : list('a) =
      size = LowLevelArray.size(a)
      rec aux(i) = if i == size then [] else [LowLevelArray.get(a, i)|aux(i+1)]
      aux(0)

    @private pack_string(buf:Pack.t,
                         actual_le:bool, return_le:bool,
                         signed:bool,
                         actual_size:Pack.s, return_size:Pack.s,
                         str:string)
                         : (bool, bool, Pack.s, outcome(void,string)) =
      match actual_size with
      | {B} -> (return_le, signed, return_size, string_b(buf, str))
      | {S} -> (return_le, signed, return_size,  string_s(buf, actual_le, str))
      | {L} -> (return_le, signed, return_size,  string_l(buf, actual_le, str))
      | {Ll} -> (return_le, signed, return_size, string_ll(buf, actual_le, str))

    @private pack_int(buf:Pack.t,
                      actual_le:bool, return_le:bool,
                      actual_signed:bool, return_signed:bool,
                      actual_size:Pack.s, return_size:Pack.s,
                      i:int)
                      : (bool, bool, Pack.s, outcome(void,string)) =
      match actual_size with
      | {B} -> (return_le, return_signed, return_size, byte(buf, actual_signed, i))
      | {S} -> (return_le, return_signed, return_size,  short(buf, actual_le, actual_signed, i))
      | {L} -> (return_le, return_signed, return_size,  long(buf, actual_le, actual_signed, i))
      | {Ll} -> (return_le, return_signed, return_size, longlong(buf, actual_le, i))

    /** Pack item into binary data.
     *
     * Since this function is out of context and packing data can change the
     * endianness, signedness or integer width, you have to pass these in.
     * The return value includes the final context.
     *
     * @param buf the binary data to pack into
     * @param le initial endianness (true=little endian)
     * @param signed initial signedness (true=signed)
     * @param size the initial width for integers
     * @param u the item to pack into the binary.
     * @return the final endianness, signedness and integer width plus the usual outcome
     **/
    pack_u(buf:Pack.t, le:bool, signed:bool, size:Pack.s, u:Pack.u) : (bool, bool, Pack.s, outcome(void,string)) =
      match u with
      | {Be} -> (false, signed, size, {success=void})
      | {Le} -> (true, signed, size, {success=void})
      | {Signed} -> (le, true, size, {success=void})
      | {Unsigned} -> (le, false, size, {success=void})
      | {B} -> (le, false, {B}, {success=void})
      | {S} -> (le, false, {S}, {success=void})
      | {L} -> (le, false, {L}, {success=void})
      | {Ll} -> (le, false, {Ll}, {success=void})
      | {~Char} -> (le, signed, size, char(buf, Char))
      | {~Byte} -> (le, signed, size, byte(buf, signed, Byte))
      | {~Short} -> (le, signed, size, short(buf, le, signed, Short))
      | {~Long} -> (le, signed, size, long(buf, le, signed, Long))
      | {~Longlong} -> (le, signed, size, longlong(buf, le, Longlong))
      | {Int=i} -> pack_int(buf, le, le, signed, signed, size, size, i)
      | {Int=i; size=actual_size} -> pack_int(buf, le, le, signed, signed, actual_size, size, i)
      | {Int=i; signed=actual_signed} -> pack_int(buf, le, le, actual_signed, signed, size, size, i)
      | {Int=i; size=actual_size; signed=actual_signed} -> pack_int(buf, le, le, actual_signed, signed, actual_size, size, i)
      | {Int=i; le=actual_le} -> pack_int(buf, actual_le, le, signed, signed, size, size, i)
      | {Int=i; le=actual_le; size=actual_size} -> pack_int(buf, actual_le, le, signed, signed, actual_size, size, i)
      | {Int=i; le=actual_le; signed=actual_signed} -> pack_int(buf, actual_le, le, actual_signed, signed, size, size, i)
      | {Int=i; le=actual_le; size=actual_size; signed=actual_signed} ->
         pack_int(buf, actual_le, le, actual_signed, signed, actual_size, size, i)
      | {Int64=i64} -> (le, signed, size, int64(buf, le, i64))
      | {Pad} -> (le, signed, size, pad(buf))
      | {~Padn} -> (le, signed, size, padn(buf, Padn))
      | {~Bound} -> (le, signed, size, boundary(buf, Bound))
      | {Void} -> (le, signed, size, pad(buf))
      | {~Bool} -> (le, signed, size, bool(buf, Bool))
      | {~Cstring} -> (le, signed, size, cstring(buf, Cstring))
      | {String=str} -> pack_string(buf, le, le, signed, size, size, str)
      | {String=str; size=actual_size} -> pack_string(buf, le, le, signed, actual_size, size, str)
      | {String=str; le=actual_le} -> pack_string(buf, actual_le, le, signed, size, size, str)
      | {String=str; le=actual_le; size=actual_size} -> pack_string(buf, actual_le, le, signed, actual_size, size, str)
      | {~Float32} -> (le, signed, size, float(buf, le, Float32))
      | {~Float} -> (le, signed, size, double(buf, le, Float))
      | {Coded=[]} -> (le, signed, size, {failure="Pack.Encode.pack: Coded has no codes"})
      | {Coded=[(code,data)]} ->
         if valid_code(code)
         then coded(buf, le, signed, size, code, data)
         else (le, signed, size, {failure="Pack.Encode.pack: Coded has invalid code {code}"})
      | {Coded=_} -> (le, signed, size, {failure="Pack.Encode.pack: Coded has multiple codes"})
      | {List=(t,l)} -> list(buf, le, signed, size, t, l)
      | {Array=(t,a)} -> list(buf, le, signed, size, t, a2l(a))

    /** Pack data into binary data.
     *
     * Since this function is out of context and packing data can change the
     * endianness, signedness or integer width, you have to pass these in.
     * The return value includes the final context.
     *
     * @param buf the binary data to pack into
     * @param le initial endianness (true=little endian)
     * @param signed initial signedness (true=signed)
     * @param size the initial width for integers
     * @param data the data value to back into the binary
     * @return the final endianness, signedness and integer width plus the usual outcome
     **/
    pack_data(buf:Pack.t, le:bool, signed:bool, size:Pack.s, data:Pack.data) : (bool, bool, Pack.s, outcome(void,string)) =
      List.fold((u, (le, signed, size, res) ->
                 match res with
                 | {~failure} -> (le, signed, size, {~failure})
                 | {success=_} -> pack_u(buf, le, signed, size, u)
                ), data, (le, signed, size, {success:void}))

    /** Create binary and pack with given data.
     *
     * Uses [packlen] to predict the size of the binary so the binary size is exact.
     *
     * @param data the data value to pack into the binary
     * @return the binary data
     **/
    pack(data:Pack.data) : outcome(Pack.t,string) =
      pack_with(data, default_options)
    /**
     * As [Pack.Decode.pack] but with custom [options].
     */
    pack_with(data:Pack.data, options:Pack.options) =
      buf = Binary.create(packlen(data))
      (_, _, _, res) = pack_data(buf, options.endian,
                                      options.signed,
                                      options.size, data)
      match res with
      | {success=_} -> {success=buf}
      | {~failure} -> {~failure}

  }}

  /**
   * {2 Serialize data}
   *
   * The Ser functions give some Opa-specific encoding functions to supplement the Encode module.
   *
   * Note that these are not necessarily unique, for instance option(a) is basically the same
   * as: [{Coded=[({Byte=0},[]),({Byte=1},[<a>])]}].
   **/
  Ser = {{

    /** Pack a Server.reference value.
     *
     * eg. [ref(buf, Pack.Encode.bool, bool_ref)]
     **/
    ref(buf:Pack.t, ea:Pack.t, 'a -> outcome(void,string), r:Server.reference('a)) : outcome(void,string) =
      ea(buf, ServerReference.get(r))

    /** Pack an optional value.
     *
     * eg. [option(buf, Pack.Encode.double, {some=1.23})]
     **/
    option(buf:Pack.t, ea:Pack.t, 'a -> outcome(void,string), oa:option('a)) : outcome(void,string) =
      match oa with
      | {some=a} ->
         match Encode.uoctet(buf, 1) with
         | {success=_} -> ea(buf, a)
         | {~failure} -> {~failure}
         end
      | {none} -> Encode.uoctet(buf, 0)

    /** Pack a 2-tuple value.
     *
     * eg. [tuple2(buf, Pack.Encode.long_be, Pack.Encode.short_be, (123,456))]
     **/
    tuple2(buf:Pack.t,
           ea:Pack.t, 'a -> outcome(void,string),
           eb:Pack.t, 'b -> outcome(void,string),
           (a,b):('a,'b))
          : outcome(void,string) =
      match ea(buf, a) with
      | {success=_} ->
         eb(buf, b)
      | {~failure} -> {~failure}

    /** Pack a 3-tuple value. **/
    tuple3(buf:Pack.t,
           ea:Pack.t, 'a -> outcome(void,string),
           eb:Pack.t, 'b -> outcome(void,string),
           ec:Pack.t, 'c -> outcome(void,string),
           (a,b,c):('a,'b,'c))
          : outcome(void,string) =
      match ea(buf, a) with
      | {success=_} ->
        match eb(buf, b) with
        | {success=_} -> ec(buf, c)
        | {~failure} -> {~failure}
        end
      | {~failure} -> {~failure}

    /** Pack a 4-tuple value. **/
    tuple4(buf:Pack.t,
           ea:Pack.t, 'a -> outcome(void,string),
           eb:Pack.t, 'b -> outcome(void,string),
           ec:Pack.t, 'c -> outcome(void,string),
           ed:Pack.t, 'd -> outcome(void,string),
           (a,b,c,d):('a,'b,'c,'d))
          : outcome(void,string) =
      match ea(buf, a) with
      | {success=_} ->
        match eb(buf, b) with
        | {success=_} ->
          match ec(buf, c) with
          | {success=_} -> ed(buf, d)
          | {~failure} -> {~failure}
          end
        | {~failure} -> {~failure}
        end
      | {~failure} -> {~failure}

    /** Pack a 5-tuple value. **/
    tuple5(buf:Pack.t,
           ea:Pack.t, 'a -> outcome(void,string),
           eb:Pack.t, 'b -> outcome(void,string),
           ec:Pack.t, 'c -> outcome(void,string),
           ed:Pack.t, 'd -> outcome(void,string),
           ee:Pack.t, 'e -> outcome(void,string),
           (a,b,c,d,e):('a,'b,'c,'d,'e))
          : outcome(void,string) =
      match ea(buf, a) with
      | {success=_} ->
        match eb(buf, b) with
        | {success=_} ->
          match ec(buf, c) with
          | {success=_} ->
            match ed(buf, d) with
            | {success=_} -> ee(buf, e)
            | {~failure} -> {~failure}
            end
          | {~failure} -> {~failure}
          end
        | {~failure} -> {~failure}
        end
      | {~failure} -> {~failure}

    /** Pack a 6-tuple value. **/
    tuple6(buf:Pack.t,
           ea:Pack.t, 'a -> outcome(void,string),
           eb:Pack.t, 'b -> outcome(void,string),
           ec:Pack.t, 'c -> outcome(void,string),
           ed:Pack.t, 'd -> outcome(void,string),
           ee:Pack.t, 'e -> outcome(void,string),
           ef:Pack.t, 'f -> outcome(void,string),
           (a,b,c,d,e,f):('a,'b,'c,'d,'e,'f))
          : outcome(void,string) =
      match ea(buf, a) with
      | {success=_} ->
        match eb(buf, b) with
        | {success=_} ->
          match ec(buf, c) with
          | {success=_} ->
            match ed(buf, d) with
            | {success=_} ->
              match ee(buf, e) with
              | {success=_} -> ef(buf, f)
              | {~failure} -> {~failure}
              end
            | {~failure} -> {~failure}
            end
          | {~failure} -> {~failure}
          end
        | {~failure} -> {~failure}
        end
      | {~failure} -> {~failure}

    /** Pack a 7-tuple value. **/
    tuple7(buf:Pack.t,
           ea:Pack.t, 'a -> outcome(void,string),
           eb:Pack.t, 'b -> outcome(void,string),
           ec:Pack.t, 'c -> outcome(void,string),
           ed:Pack.t, 'd -> outcome(void,string),
           ee:Pack.t, 'e -> outcome(void,string),
           ef:Pack.t, 'f -> outcome(void,string),
           eg:Pack.t, 'g -> outcome(void,string),
           (a,b,c,d,e,f,g):('a,'b,'c,'d,'e,'f,'g))
          : outcome(void,string) =
      match ea(buf, a) with
      | {success=_} ->
        match eb(buf, b) with
        | {success=_} ->
          match ec(buf, c) with
          | {success=_} ->
            match ed(buf, d) with
            | {success=_} ->
              match ee(buf, e) with
              | {success=_} ->
                match ef(buf, f) with
                | {success=_} -> eg(buf, g)
                | {~failure} -> {~failure}
                end
              | {~failure} -> {~failure}
              end
            | {~failure} -> {~failure}
            end
          | {~failure} -> {~failure}
          end
        | {~failure} -> {~failure}
        end
      | {~failure} -> {~failure}

  }}

  /**
   * {2 Decode data}
   *
   * This module provides functions to extract values from binary data.
   *
   **/
  Decode = {{

    /** Decode single 8-bit byte as a string of length 1 **/
    char(data:Pack.t, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.char",{binary=data; ~pos})
      if Binary.length(data) >= pos + 1
      then {success=Binary.get_string(data, pos, 1)}
      else {failure="Pack.Decode.char: not enough data for char"}

    /** Decode 8-bit signed integer **/
    octet(data:Pack.t, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.byte",{binary=data; ~pos})
      if Binary.length(data) >= pos + 1
      then {success=Binary.get_int8(data, pos)}
      else {failure="Pack.Decode.byte: not enough data for byte"}

    /** Decode 8-bit unsigned integer **/
    uoctet(data:Pack.t, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.byte",{binary=data; ~pos})
      if Binary.length(data) >= pos + 1
      then {success=Binary.get_uint8(data, pos)}
      else {failure="Pack.Decode.byte: not enough data for byte"}

    /** Decode 8-bit integer, signedness as per argument **/
    byte(signed:bool, data:Pack.t, pos:int) : outcome(int,string) =
      if signed then octet(data, pos) else uoctet(data, pos)

    /** Decode 16-bit unsigned big-endian integer **/
    ushort_be(data:Pack.t, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.ushort_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 2
      then {success=Binary.get_uint16_be(data, pos)}
      else {failure="Pack.Decode.ushort_be: not enough data for short"}

    /** Decode 16-bit unsigned little-endian integer **/
    ushort_le(data:Pack.t, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.ushort_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 2
      then {success=Binary.get_uint16_le(data, pos)}
      else {failure="Pack.Decode.ushort_le: not enough data for short"}

    /** Decode 16-bit signed big-endian integer **/
    short_be(data:Pack.t, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.short_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 2
      then {success=Binary.get_int16_be(data, pos)}
      else {failure="Pack.Decode.short_be: not enough data for short"}

    /** Decode 16-bit signed little-endian integer **/
    short_le(data:Pack.t, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.short_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 2
      then {success=Binary.get_int16_le(data, pos)}
      else {failure="Pack.Decode.short_le: not enough data for short"}

    /** Decode 16-bit integer
     * @param le endianness
     * @param signed signedness
     **/
    short(le:bool, signed:bool, data:Pack.t, pos:int): outcome(int,string) =
      match (le, signed) with
      | ({false},{false}) -> ushort_be(data, pos)
      | ({true},{false}) -> ushort_le(data, pos)
      | ({false},{true}) -> short_be(data, pos)
      | ({true},{true}) -> short_le(data, pos)

    /** Decode 32-bit signed big-endian integer **/
    long_be(data:Pack.t, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.long_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_int32_be(data, pos)}
      else {failure="Pack.Decode.long_le: not enough data for long"}

    /** Decode 32-bit signed little-endian integer **/
    long_le(data:Pack.t, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.long_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_int32_le(data, pos)}
      else {failure="Pack.Decode.long_le: not enough data for long"}

    /** Decode 32-bit unsigned big-endian integer **/
    ulong_be(data:Pack.t, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.ulong_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_uint32_be(data, pos)}
      else {failure="Pack.Decode.ulong_le: not enough data for long"}

    /** Decode 32-bit unsigned little-endian integer **/
    ulong_le(data:Pack.t, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.ulong_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_uint32_le(data, pos)}
      else {failure="Pack.Decode.ulong_le: not enough data for long"}

    /** Decode 32-bit integer
     * @param le endianness
     * @param signed signedness
     **/
    long(le:bool, signed:bool, data:Pack.t, pos:int): outcome(int,string) =
      match (le, signed) with
      | ({false},{false}) -> ulong_be(data, pos)
      | ({true},{false}) -> ulong_le(data, pos)
      | ({false},{true}) -> long_be(data, pos)
      | ({true},{true}) -> long_le(data, pos)

    /** Decode 64-bit signed big-endian integer, resulting integer is in native format **/
    longlong_be(data:Pack.t, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.longlong_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_int53_be(data, pos)}
      else {failure="Pack.Decode.longlong_le: not enough data for longlong"}

    /** Decode 64-bit signed little-endian integer, resulting integer is in native format **/
    longlong_le(data:Pack.t, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.longlong_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_int53_le(data, pos)}
      else {failure="Pack.Decode.longlong_le: not enough data for longlong"}

    /** Decode 64-bit integer
     * @param le endianness
     **/
    longlong(le:bool, data:Pack.t, pos:int) : outcome(int,string) =
      if le then longlong_le(data, pos) else longlong_be(data, pos)

    /** Decode generalised integer
     * @param le endianness
     * @param signed signedness
     * @param size integer width
     **/
    int(le:bool, signed:bool, s:Pack.s, data:Pack.t, pos:int) : outcome(int,string) =
      match s with
      | {B} -> byte(signed, data, pos) // no endian
      | {S} -> short(le, signed, data, pos)
      | {L} -> long(le, signed, data, pos)
      | {Ll} -> longlong(le, data, pos) // always signed

    /** Decode 64-bit unsigned little-endian integer, resulting integer is int64 **/
    int64_le(data:Pack.t, pos:int) : outcome(int64,string) =
      do pinput("Pack.Decode.int64_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 8
      then {success=Binary.get_uint64_le(data, pos)}
      else {failure="Pack.Decode.int64_le: not enough data for int64"}

    /** Decode 64-bit unsigned big-endian integer, resulting integer is int64 **/
    int64_be(data:Pack.t, pos:int) : outcome(int64,string) =
      do pinput("Pack.Decode.int64_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 8
      then {success=Binary.get_uint64_be(data, pos)}
      else {failure="Pack.Decode.int64_be: not enough data for int64"}

    /** Decode 64-bit unsigned integer
     * @param le endianness
     **/
    int64(le:bool, data:Pack.t, pos:int) : outcome(int64,string) =
      if le then int64_le(data, pos) else int64_be(data, pos)

    /** Skip over a single padding byte **/
    pad(data:Pack.t, pos:int) : outcome(void,string) =
      do pinput("Pack.Decode.pad",{binary=data; ~pos})
      if Binary.length(data) >= pos + 1
      then {success=void}
      else {failure="Pack.Decode.pad: not enough data for padding"}

    /** Skip over n padding bytes
     * @param data binary data
     * @param pos position in binary data
     * @param n number of bytes to skip
     **/
    padn(data:Pack.t, pos:int, n:int) : outcome(void,string) =
      do pinput("Pack.Decode.padn({n})",{binary=data; ~pos})
      if Binary.length(data) >= pos + n
      then {success=void}
      else {failure="Pack.Decode.padn: not enough data for padding"}

    /** Skip up to given boundary.
     * @param data binary data
     * @param pos position in binary data
     * @param n the boundary value
     * @return the actual number of bytes skipped
     **/
    boundary(data:Pack.t, pos:int, n:int) : outcome(int,string) =
      if n <= 0
      then {success=0}
      else
        do pinput("Pack.Decode.boundary({n})",{binary=data; ~pos})
        padding = bound(pos, n)
        if Binary.length(data) >= pos + padding
        then {success=padding}
        else {failure="Pack.Decode.boundary: not enough data for padding"}

    /** Decode single byte as bool, 0 means false, true otherwise **/
    bool(data:Pack.t, pos:int) : outcome(bool,string) =
      do pinput("Pack.Decode.bool",{binary=data; ~pos})
      if Binary.length(data) >= pos + 1
      then {success=Binary.get_uint8(data, pos) != 0}
      else {failure="Pack.Decode.bool: not enough data for bool"}

    @private clen(data:Pack.t, pos:int) : option(int) =
      dlen = Binary.length(data) - pos
      rec aux(i) =
        if i > dlen
        then none
        else
          // Implement Binary.index?
          if Binary.get_uint8(data, pos + i) == 0
          then {some=i}
          else aux(i+1)
      aux(0)

    /** Decode null-terminated string from data, searches forward for null **/
    cstring(data:Pack.t, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.cstring",{binary=data; ~pos})
      match clen(data, pos) with
      | {some=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos, len)}
        else {failure="Pack.Decode.cstring: not enough data for string"}
      | {none} ->
        {failure="Pack.Decode.cstring: can't find null"}

    /** Decode 8-bit integer size-prefixed string */
    string_b(data:Pack.t, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.string_b",{binary=data; ~pos})
      match uoctet(data, pos) with
      | {success=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos + 1, len)}
        else {failure="Pack.Decode.string_b: not enough data for string"}
      | {~failure} -> {~failure}

    /** Decode 16-bit big-endian integer size-prefixed string */
    string_s_be(data:Pack.t, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.string_s_be",{binary=data; ~pos})
      match short_be(data, pos) with
      | {success=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos + 2, len)}
        else {failure="Pack.Decode.string_s_be: not enough data for string"}
      | {~failure} -> {~failure}

    /** Decode 16-bit big-endian integer size-prefixed string */
    string_s_le(data:Pack.t, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.string_s_le",{binary=data; ~pos})
      match short_le(data, pos) with
      | {success=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos + 2, len)}
        else {failure="Pack.Decode.string_s_le: not enough data for string"}
      | {~failure} -> {~failure}

    /** Decode 16-bit integer size-prefixed string
     * @param le endianness
     **/
    string_s(le:bool, data:Pack.t, pos:int) : outcome(string,string) =
      if le then string_s_le(data, pos) else string_s_be(data, pos)

    /** Decode 32-bit big-endian integer size-prefixed string */
    string_l_be(data:Pack.t, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.string_l_be",{binary=data; ~pos})
      match long_be(data, pos) with
      | {success=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos + 4, len)}
        else {failure="Pack.Decode.string_l_be: not enough data for string ({Binary.length(data)}:{data} > ({pos} + {len})"}
      | {~failure} -> {~failure}

    /** Decode 32-bit little-endian integer size-prefixed string */
    string_l_le(data:Pack.t, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.string_l_le",{binary=data; ~pos})
      match long_le(data, pos) with
      | {success=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos + 4, len)}
        else {failure="Pack.Decode.string_l_le: not enough data for string ({Binary.length(data)}:{data} > ({pos} + {len})"}
      | {~failure} -> {~failure}

    /** Decode 32-bit integer size-prefixed string
     * @param le endianness
     **/
    string_l(le:bool, data:Pack.t, pos:int) : outcome(string,string) =
      if le then string_l_le(data, pos) else string_l_be(data, pos)

    /** Decode 64-bit big-endian integer size-prefixed string */
    string_ll_be(data:Pack.t, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.string_ll_be",{binary=data; ~pos})
      match longlong_be(data, pos) with
      | {success=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos + 8, len)}
        else {failure="Pack.Decode.string_ll_be: not enough data for string ({Binary.length(data)}:{data} > ({pos} + {len})"}
      | {~failure} -> {~failure}

    /** Decode 64-bit little-endian integer size-prefixed string */
    string_ll_le(data:Pack.t, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.string_ll_le",{binary=data; ~pos})
      match longlong_le(data, pos) with
      | {success=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos + 8, len)}
        else {failure="Pack.Decode.string_ll_le: not enough data for string ({Binary.length(data)}:{data} > ({pos} + {len})"}
      | {~failure} -> {~failure}

    /** Decode 64-bit integer size-prefixed string
     * @param le endianness
     **/
    string_ll(le:bool, data:Pack.t, pos:int) : outcome(string,string) =
      if le then string_ll_le(data, pos) else string_ll_be(data, pos)

    /** Decode generalised string
     * @param le endianness
     * @param size integer width
     **/
    // generalised string
    string(le:bool, size:Pack.s, data:Pack.t, pos:int) : outcome(string,string) =
      match size with
      | {B} -> string_b(data, pos)
      | {S} -> string_s(le, data, pos)
      | {L} -> string_l(le, data, pos)
      | {Ll} -> string_ll(le, data, pos)

    /** Decode 32-bit little-endian float */
    float_le(data:Pack.t, pos:int) : outcome(float,string) =
      do pinput("Pack.Decode.float_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_float_le(data, pos)}
      else {failure="Pack.Decode.float_le: not enough data for float"}

    /** Decode 32-bit big-endian float */
    float_be(data:Pack.t, pos:int) : outcome(float,string) =
      do pinput("Pack.Decode.float_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_float_be(data, pos)}
      else {failure="Pack.Decode.float_be: not enough data for float"}

    /** Decode 32-bit float
     * @param le endianness
     **/
    float(le:bool, data:Pack.t, pos:int) : outcome(float,string) =
      if le then float_le(data, pos) else float_be(data, pos)

    /** Decode 64-bit little-endian float */
    double_le(data:Pack.t, pos:int) : outcome(float,string) =
      do pinput("Pack.Decode.double_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 8
      then {success=Binary.get_double_le(data, pos)}
      else {failure="Pack.Decode.double_le: not enough data for double"}

    /** Decode 64-bit big-endian float */
    double_be(data:Pack.t, pos:int) : outcome(float,string) =
      do pinput("Pack.Decode.double_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 8
      then {success=Binary.get_double_be(data, pos)}
      else {failure="Pack.Decode.double_be: not enough data for double"}

    /** Decode 64-bit float
     * @param le endianness
     **/
    double(le:bool, data:Pack.t, pos:int) : outcome(float,string) =
      if le then double_le(data, pos) else double_be(data, pos)

    @private mksla(lora:bool, typ:Pack.data, l:list(Pack.data)) : Pack.u =
      if lora then {List=(typ,l)} else {Array=(typ,LowLevelArray.of_list(l))}

    /** Decode list or array of data.
     *
     * Since this function is out of context and packing data can change the
     * endianness, signedness or integer width, you have to pass these in.
     * The return value includes the final context.
     *
     * @param lora true means List, false means Array
     * @param le initial endianness (true=little endian)
     * @param signed initial signedness (true=signed)
     * @param s the initial width for integers
     * @param typ proforma value for list/array elements
     * @param pos position in the binary data
     * @param data the resultant data is added to the start of this data element (used to accumulate)
     * @return the final endianness, signedness, integer width and buffer position
     *         plus an outcome of either a [{List}] or [{Array}] item containing the data items read in.
     **/
    list(lora:bool, le:bool, signed:bool, s:Pack.s, typ:Pack.data, bin:Pack.t, pos:int)
         : outcome((bool, bool, Pack.s, int, Pack.u),string) =
      do pinput("Pack.Decode.list",{binary=bin; ~pos})
      match mksize(s, 0) with
      | {success=size} ->
        match unpack([size], bin, pos) with
        | {success=(pos,[size])} ->
          match getsize(size) with
          | {success=len} ->
            rec aux(i, l, pos) =
              if i == len
              then {success=(le, signed, s, pos, mksla(lora, typ, List.rev(l)))}
              else
                match unpack(typ, bin, pos) with
                | {success=(npos,ndata)} -> aux(i+1, [ndata|l], npos)
                | {~failure} -> {~failure}
                end
            aux(0, [], pos)
          | {~failure} -> {~failure}
          end
        | {success=_} -> {failure="Pack.Decode.list: list has missing or multiple lengths"}
        | {~failure} -> {~failure}
        end
      | {~failure} -> {~failure}

    @private unpack_list(lora:bool, le:bool, signed:bool, size:Pack.s, typ:Pack.data, bin:Pack.t, pos:int, data:Pack.data)
                        : outcome((bool, bool, Pack.s, int, Pack.data),string) =
      match list(lora, le, signed, size, typ, bin, pos) with
      | {success=(le, signed, size, npos, item)} -> {success=(le, signed, size, npos, [item|data])}
      | {~failure} -> {~failure}

    /** Decode a coded value.
     *
     * Since this function is out of context and packing data can change the
     * endianness, signedness or integer width, you have to pass these in.
     * The return value includes the final context.
     *
     * @param codes The list of recognised codes and matching data
     * @return A triple of the final buffer position, the actual matched code and the resulting data.
     **/
    coded(codes:Pack.codes, bin:Pack.t, pos:int) : outcome((int, Pack.u, Pack.data),string) =
      match codes with
      | [] -> {failure="Pack.Decode.coded: no codes"}
      | [(code1,data1)|codes] ->
         if valid_code(code1)
         then
           (match unpack([code1], bin, pos) with
            | {success=(npos,[code])} ->
              (match List.assoc(code, [(code1,data1)|codes]) with
               | {some=cdata} ->
                  (match unpack(cdata, bin, npos) with
                   | {success=(npos,ndata)} -> {success=(npos, code, ndata)}
                   | {~failure} -> {failure="Pack.Decode.coded: bad data {failure}"})
               | {none} -> {failure="Pack.Decode.coded: missing code {code}"})
            | {success=_} -> {failure="Pack.Decode.coded: multiple decodings"}
            | {~failure} -> {failure="Pack.Decode.coded: bad code {failure}"})
          else {failure="Pack.Decode.coded: bad code {code1}"}

    @private unpack_coded(codes, data, le, signed, size, bin, pos) =
      match coded(codes, bin, pos) with
      | {success=(npos, code, ndata)} -> {success=(le, signed, size, npos, [{Coded=[(code,ndata)]}|data])}
      | {~failure} -> {~failure}

    @private unpack_int(data:Pack.data,
                        actual_le:option(bool), return_le:bool,
                        actual_signed:option(bool), return_signed:bool,
                        actual_size:option(Pack.s), return_size:Pack.s,
                        bin:Pack.t, pos:int)
                        : outcome((bool, bool, Pack.s, int, Pack.data),string) =
      real_le = Option.default(return_le, actual_le)
      real_signed = Option.default(return_signed, actual_signed)
      real_size = Option.default(return_size, actual_size)
      match int(real_le, real_signed, real_size, bin, pos) with
      | {success=i} ->
         {success=(return_le, return_signed, return_size, pos+sizesize(real_size),
                   [(match (actual_le,actual_size,actual_signed) with
                     | ({none},{none},{none}) -> {Int=i}
                     | ({none},{some=actual_size},{none}) -> {Int=i; size=actual_size}
                     | ({none},{none},{some=actual_signed}) -> {Int=i; signed=actual_signed}
                     | ({none},{some=actual_size},{some=actual_signed}) -> {Int=i; size=actual_size; signed=actual_signed}
                     | ({some=actual_le},{none},{none}) -> {Int=i; le=actual_le}
                     | ({some=actual_le},{some=actual_size},{none}) -> {Int=i; le=actual_le; size=actual_size}
                     | ({some=actual_le},{none},{some=actual_signed}) -> {Int=i; le=actual_le; signed=actual_signed}
                     | ({some=actual_le},{some=actual_size},{some=actual_signed}) ->
                        {Int=i; le=actual_le; size=actual_size; signed=actual_signed}
                    )|data])}
      | {~failure} -> {~failure}

    @private unpack_string(data:Pack.data,
                           actual_le:option(bool), return_le:bool,
                           signed:bool,
                           actual_size:option(Pack.s), return_size:Pack.s,
                           bin:binary, pos:int)
                          : outcome((bool, bool, Pack.s, int, Pack.data),string) =
      real_le = Option.default(return_le, actual_le)
      real_size = Option.default(return_size, actual_size)
      match
        match real_size with
        | {B} -> string_b(bin, pos)
        | {L} -> string_l(real_le, bin, pos)
        | {S} -> string_s(real_le, bin, pos)
        | {Ll} -> string_ll(real_le, bin, pos)
      with
      | {success=s} ->
         {success=(return_le, signed, return_size, pos+String.length(s)+sizesize(real_size),
                   [(match (actual_le, actual_size) with
                     | ({some=actual_le},{some=actual_size}) -> {String=s; le=actual_le; size=actual_size}
                     | ({some=actual_le},{none}) -> {String=s; le=actual_le}
                     | ({none},{some=actual_size}) -> {String=s; size=actual_size}
                     | ({none},{none}) -> {String=s}
                    )|data])}
      | {~failure} -> {~failure}

    // Unpack item
    @private unpack_item(bin:Pack.t, u:Pack.u, acc) =
      match acc with
      | {~failure} -> {~failure}
      | {success=(le, signed, size, pos, data)} ->
        (match u:Pack.u with
         | {Be} -> {success=(false, signed, size, pos, [{Be}|data])}
         | {Le} -> {success=(true, signed, size, pos, [{Le}|data])}
         | {Signed} -> {success=(le, true, size, pos, [{Signed}|data])}
         | {Unsigned} -> {success=(le, false, size, pos, [{Unsigned}|data])}
         | {B} -> {success=(le, false, {B}, pos, [{B}|data])}
         | {S} -> {success=(le, false, {S}, pos, [{S}|data])}
         | {L} -> {success=(le, false, {L}, pos, [{L}|data])}
         | {Ll} -> {success=(le, false, {Ll}, pos, [{Ll}|data])}
         | {Char=_} ->
            (match char(bin, pos) with
             | {success=Char} -> {success=(le, signed, size, pos+1, [{~Char}|data])}
             | {~failure} -> {~failure})
         | {Byte=_} ->
            (match byte(signed, bin, pos) with
             | {success=Byte} -> {success=(le, signed, size, pos+1, [{~Byte}|data])}
             | {~failure} -> {~failure})
         | {Short=_} ->
            (match short(le, signed, bin, pos) with
             | {success=Short} -> {success=(le, signed, size, pos+2, [{~Short}|data])}
             | {~failure} -> {~failure})
         | {Long=_} ->
            (match long(le, signed, bin, pos) with
             | {success=Long} -> {success=(le, signed, size, pos+4, [{~Long}|data])}
             | {~failure} -> {~failure})
         | {Longlong=_} ->
            (match longlong(le, bin, pos) with
             | {success=Longlong} -> {success=(le, signed, size, pos+8, [{~Longlong}|data])}
             | {~failure} -> {~failure})
         | {Int=_} ->
            unpack_int(data, {none}, le, {none}, signed, {none}, size, bin, pos)
         | {Int=_; size=actual_size} ->
            unpack_int(data, {none}, le, {none}, signed, {some=actual_size}, size, bin, pos)
         | {Int=_; signed=actual_signed} ->
            unpack_int(data, {none}, le, {some=actual_signed}, signed, {none}, size, bin, pos)
         | {Int=_; size=actual_size; signed=actual_signed} ->
            unpack_int(data, {none}, le, {some=actual_signed}, signed, {some=actual_size}, size, bin, pos)
         | {Int=_; le=actual_le} ->
            unpack_int(data, {some=actual_le}, le, {none}, signed, {none}, size, bin, pos)
         | {Int=_; le=actual_le; size=actual_size} ->
            unpack_int(data, {some=actual_le}, le, {none}, signed, {some=actual_size}, size, bin, pos)
         | {Int=_; le=actual_le; signed=actual_signed} ->
            unpack_int(data, {some=actual_le}, le, {some=actual_signed}, signed, {none}, size, bin, pos)
         | {Int=_; le=actual_le; size=actual_size; signed=actual_signed} ->
            unpack_int(data, {some=actual_le}, le, {some=actual_signed}, signed, {some=actual_size}, size, bin, pos)
         | {Int64=_} ->
            (match int64(le, bin, pos) with
             | {success=i64} -> {success=(le, signed, size, pos+8, [{Int64=i64}|data])}
             | {~failure} -> {~failure})
         | {Pad} ->
            (match pad(bin, pos) with
             | {success=_} -> {success=(le, signed, size, pos+1, [{Pad}|data])}
             | {~failure} -> {~failure})
         | {~Padn} ->
            (match padn(bin, pos, Padn) with
             | {success=_} -> {success=(le, signed, size, pos+Padn, [{~Padn}|data])}
             | {~failure} -> {~failure})
         | {~Bound} ->
            (match boundary(bin, pos, Bound) with
             | {success=padding} -> {success=(le, signed, size, pos+padding, [{~Bound}|data])}
             | {~failure} -> {~failure})
         | {Void} ->
            (match pad(bin, pos) with
             | {success=_} -> {success=(le, signed, size, pos+1, [{Void}|data])}
             | {~failure} -> {~failure})
         | {Bool=_} ->
            (match bool(bin, pos) with
             | {success=Bool} -> {success=(le, signed, size, pos+1, [{~Bool}|data])}
             | {~failure} -> {~failure})
         | {Cstring=_} ->
            (match cstring(bin, pos) with
             | {success=s} -> {success=(le, signed, size, pos+String.length(s)+1, [{Cstring=s}|data])}
             | {~failure} -> {~failure})
         | {String=_} -> unpack_string(data, {none}, le, signed, {none}, size, bin, pos)
         | {String=_; size=actual_size} -> unpack_string(data, {none}, le, signed, {some=actual_size}, size, bin, pos)
         | {String=_; le=actual_le} -> unpack_string(data, {some=actual_le}, le, signed, {none}, size, bin, pos)
         | {String=_; le=actual_le; size=actual_size} ->
            unpack_string(data, {some=actual_le}, le, signed, {some=actual_size}, size, bin, pos)
         | {Float32=_} ->
            (match float(le, bin, pos) with
             | {success=Float32} -> {success=(le, signed, size, pos+4, [{~Float32}|data])}
             | {~failure} -> {~failure})
         | {Float=_} ->
            (match double(le, bin, pos) with
             | {success=Float} -> {success=(le, signed, size, pos+8, [{~Float}|data])}
             | {~failure} -> {~failure})
         | {~Coded} -> unpack_coded(Coded, data, le, signed, size, bin, pos)
         | {List=(typ,_)} -> unpack_list(true, le, signed, size, typ, bin, pos, data)
         | {Array=(typ,_)} -> unpack_list(false, le, signed, size, typ, bin, pos, data))

    /** Decode single item.
     *
     * Since this function is out of context and packing data can change the
     * endianness, signedness or integer width, you have to pass these in.
     * The return value includes the final context.
     *
     * @param le initial endianness (true=little endian)
     * @param signed initial signedness (true=signed)
     * @param size the initial width for integers
     * @param bin the binary data
     * @param pos position in the binary data
     * @param u the item specification
     * @return the final endianness, signedness, integer width and buffer position
     *         plus an outcome of the item read in
     **/
    unpack_u(le:bool, signed:bool, size:Pack.s, bin:Pack.t, pos:int, u:Pack.u)
            : outcome((bool, bool, Pack.s, int, Pack.u),string) =
      match unpack_item(bin, u, {success=(le, signed, size, pos, [])}) with
      | {success=(le, signed, size, pos, [item])} -> {success=(le, signed, size, pos, item)}
      | {success=(_, _, _, _, [])} -> {failure="Pack.Decode.unpack_u: no items"}
      | {success=(_, _, _, _, _)} -> {failure="Pack.Decode.unpack_u: multiple items"}
      | {~failure} -> {~failure}

    /** Decode data.
     *
     * Read in data starting with the default endianness, signedness and size from
     * the given binary data and position.
     * Note that empty data is not considered an error and will return empty data plus the current position.
     *
     * @param data the data specification
     * @param bin the binary data
     * @param pos position in the binary data
     * @return an outcome of the final buffer position and the data read in
     **/
    unpack(data:Pack.data, bin:Pack.t, pos:int) : outcome((int,Pack.data),string) =
      unpack_with(data:Pack.data, bin:Pack.t, pos:int, default_options)

    /**
     * As [Pack.Decode.unpack] but with custom [options].
     */
    unpack_with(data:Pack.data, bin:Pack.t, pos:int, options:Pack.options) : outcome((int,Pack.data),string) =
      do pinput("Pack.Decode.unpack",{binary=bin; ~pos})
      if data == []
      then {success=(pos,[])}
      else
        match
          List.fold(unpack_item(bin, _, _),
                    data, {success=(options.endian,
                                    options.signed,
                                    options.size, pos, [])})
        with
        | {success=(_,_,_,pos,data)} -> {success=(pos,List.rev(data))}
        | {~failure} -> {~failure}

    // meaningful entities
    @private has_data(u:Pack.u) : option(Pack.u) =
      match u with
      | {Be} -> none
      | {Le} -> none
      | {Signed} -> none
      | {Unsigned} -> none
      | {B} -> none
      | {S} -> none
      | {L} -> none
      | {Ll} -> none
      | {Char=_} -> {some=u}
      | {Byte=_} -> {some=u}
      | {Short=_} -> {some=u}
      | {Long=_} -> {some=u}
      | {Longlong=_} -> {some=u}
      | {~Int; ...} -> {some={~Int}}
      | {Int64=_} -> {some=u}
      | {Pad} -> none
      | {Padn=_} -> none
      | {Bound=_} -> none
      | {Void} -> {some=u}
      | {Bool=_} -> {some=u}
      | {Cstring=_} -> {some=u}
      | {~String; ...} -> {some={~String}}
      | {Float32=_} -> {some=u}
      | {Float=_} -> {some=u}
      | {Coded=_} -> {some=u}
      | {List=_} -> {some=u}
      | {Array=_} -> {some=u}

    /** Cleanup elements with no data
     *
     * All directives, padding and non-data items are filtered out.
     * Note that in situ directives (eg. {Int=1; size={S}}) are also removed.
     **/
    clean(data:Pack.data) = List.filter_map(has_data, data)

  }}

  /**
   * {2 Unserialize data}
   *
   * The Unser functions provide a compositional functional decode over the Pack.input type.
   *
   * You can create a Pack.input value from binary data by: [{binary=bin_data; pos=0}] or you
   * can use the top-level start function provided here ([Pack.Unser.unser]).
   *
   * The idea here is that you can compose these functions:
   *
   * [unser_bs = Pack.Unser.tuple2(_, Pack.Unser.bool, Pack.Unser.short_be)]
   *
   * In general, these functions return a [Pack.result] type, which is an outcome of
   * an updated [Pack.input] type plus the value read in.
   **/
  Unser = {{

    /** Unpack char **/
    char(input:Pack.input) : Pack.result(string) =
      do pinput("Pack.Unser.char", input)
      match Decode.char(input.binary, input.pos) with
      | {success=c} -> {success=({input with pos=input.pos+1},c)}
      | {~failure} -> {~failure}

    /** Unpack generalized int.
     *
     * @param le endianness (true=little endian)
     * @param signed signedness (true=signed)
     * @param size the width for integers
     * @param input the input value
     * @param pos position in the binary data
     * @return an int Pack.result
     **/
    int(le:bool, signed:bool, size:Pack.s, input:Pack.input) : Pack.result(int) =
      do pinput("Pack.Unser.int", input)
      match Decode.int(le, signed, size, input.binary, input.pos) with
      | {success=i} -> {success=({input with pos=input.pos+sizesize(size)},i)}
      | {~failure} -> {~failure}

    byte(signed:bool, input:Pack.input) = int(noEndian, signed, {B}, input)

    /** Unpack signed 8-bit int **/
    octet(input:Pack.input) = byte(signedInts, input)

    /** Unpack unsigned 8-bit int **/
    uoctet(input:Pack.input) = byte(unsignedInts, input)

    /** Unpack 16-bit int.
     *
     * @param le endianness (true=little endian)
     * @param signed signedness (true=signed)
     * @param input the input value
     * @return an int Pack.result
     **/
    short(le:bool, signed:bool, input:Pack.input) = int(le, signed, {S}, input)

    /** Unpack signed 16-bit little-endian int **/
    short_le(input:Pack.input) = int(littleEndian, signedInts, {S}, input)

    /** Unpack signed 16-bit big-endian int **/
    short_be(input:Pack.input) = int(bigEndian, signedInts, {S}, input)

    /** Unpack unsigned 16-bit little-endian int **/
    ushort_le(input:Pack.input) = int(littleEndian, unsignedInts, {S}, input)

    /** Unpack unsigned 16-bit big-endian int **/
    ushort_be(input:Pack.input) = int(bigEndian, unsignedInts, {S}, input)

    /** Unpack 32-bit int.
     *
     * @param le endianness (true=little endian)
     * @param signed signedness (true=signed)
     * @param input the input value
     * @return an int Pack.result
     **/
    long(le:bool, signed:bool, input:Pack.input) = int(le, signed, {L}, input)

    /** Unpack signed 32-bit little-endian int **/
    long_le(input:Pack.input) = int(littleEndian, signedInts, {L}, input)

    /** Unpack signed 32-bit big-endian int **/
    long_be(input:Pack.input) = int(bigEndian, signedInts, {L}, input)

    /** Unpack unsigned 32-bit little-endian int **/
    ulong_le(input:Pack.input) = int(littleEndian, unsignedInts, {L}, input)

    /** Unpack unsigned 32-bit big-endian int **/
    ulong_be(input:Pack.input) = int(bigEndian, unsignedInts, {L}, input)

    /** Unpack 64-bit int into native int.
     *
     * @param le endianness (true=little endian)
     * @param input the input value
     * @return an int Pack.result
     **/
    longlong(le:bool, input:Pack.input) = int(le, signedInts, {Ll}, input)

    /** Unpack signed 64-bit little-endian int as native int **/
    longlong_le(input:Pack.input) = int(littleEndian, signedInts, {Ll}, input)

    /** Unpack signed 64-bit big-endian int as native int **/
    longlong_be(input:Pack.input) = int(bigEndian, signedInts, {Ll}, input)

    /** Unpack unsigned 64-bit int into int64.
     *
     * @param le endianness (true=little endian)
     * @param input the input value
     * @return an int64 Pack.result
     **/
    int64(le:bool, input:Pack.input) : Pack.result(int64) =
      do pinput("Pack.Unser.int64", input)
      match Decode.int64(le, input.binary, input.pos) with
      | {success=i64} -> {success=({input with pos=input.pos+8},i64)}
      | {~failure} -> {~failure}

    /** Unpack unsigned 64-bit little-endian int as int64 **/
    int64_le(input:Pack.input) = int64(true, input)

    /** Unpack unsigned 64-bit big-endian int as int64 **/
    int64_be(input:Pack.input) = int64(false, input)

    /** Unpack 32-bit float.
     *
     * @param le endianness (true=little endian)
     * @param input the input value
     * @return a float Pack.result
     **/
    float32(le:bool, input:Pack.input) : Pack.result(float) =
      do pinput("Pack.Unser.float", input)
      match Decode.float(le, input.binary, input.pos) with
      | {success=f} -> {success=({input with pos=input.pos+8},f)}
      | {~failure} -> {~failure}

    /** Unpack 64-bit float.
     *
     * @param le endianness (true=little endian)
     * @param input the input value
     * @return a float Pack.result
     **/
    float(le:bool, input:Pack.input) : Pack.result(float) =
      do pinput("Pack.Unser.float", input)
      match Decode.double(le, input.binary, input.pos) with
      | {success=f} -> {success=({input with pos=input.pos+8},f)}
      | {~failure} -> {~failure}

    /** Unpack null-terminated string **/
    cstring(input:Pack.input) : Pack.result(string) =
      do pinput("Pack.Unser.cstring", input)
      match Decode.cstring(input.binary, input.pos) with
      | {success=s} -> {success=({input with pos=input.pos+1+String.length(s)},s)}
      | {~failure} -> {~failure}

    /** Unpack size-prefixed string.
     *
     * @param le endianness (true=little endian)
     * @param size the width for integers
     * @param input the input value
     * @return a string Pack.result
     **/
    string(le:bool, size:Pack.s, input:Pack.input) : Pack.result(string) =
      do pinput("Pack.Unser.string", input)
      match Decode.string(le, size, input.binary, input.pos) with
      | {success=s} -> {success=({input with pos=input.pos+sizesize(size)+String.length(s)},s)}
      | {~failure} -> {~failure}

    /** Unpack 8-bit int-prefixed string **/
    string_b(input:Pack.input) = string(false, {B}, input)

    /** Unpack short-prefixed string.
     *
     * @param le endianness (true=little endian)
     * @param input the input value
     * @return a string Pack.result
     **/
    string_s(le:bool, input:Pack.input) = string(le, {S}, input)

    /** Unpack 16-bit little-endian int-prefixed string **/
    string_s_le(input:Pack.input) = string(true, {S}, input)

    /** Unpack 16-bit big-endian int-prefixed string **/
    string_s_be(input:Pack.input) = string(false, {S}, input)

    /** Unpack long-prefixed string.
     *
     * @param le endianness (true=little endian)
     * @param input the input value
     * @return a string Pack.result
     **/
    string_l(le:bool, input:Pack.input) = string(le, {L}, input)

    /** Unpack 32-bit little-endian int-prefixed string **/
    string_l_le(input:Pack.input) = string(true, {L}, input)

    /** Unpack 32-bit big-endian int-prefixed string **/
    string_l_be(input:Pack.input) = string(false, {L}, input)

    /** Unpack longlong-prefixed string.
     *
     * @param le endianness (true=little endian)
     * @param input the input value
     * @return a string Pack.result
     **/
    string_ll(le:bool, input:Pack.input) = string(le, {Ll}, input)

    /** Unpack 64-bit little-endian int-prefixed string **/
    string_ll_le(input:Pack.input) = string(true, {Ll}, input)

    /** Unpack 64-bit big-endian int-prefixed string **/
    string_ll_be(input:Pack.input) = string(false, {Ll}, input)

    /** Skip one byte of input **/
    pad(input:Pack.input) : Pack.result(void) =
      do pinput("Pack.Unser.pad", input)
      match Decode.pad(input.binary, input.pos) with
      | {success=_} -> {success=({input with pos=input.pos+1},void)}
      | {~failure} -> {~failure}

    /** Skip n bytes of input **/
    padn(n:int, input:Pack.input) : Pack.result(void) =
      do pinput("Pack.Unser.padn({n})", input)
      match Decode.padn(input.binary, input.pos, n) with
      | {success=_} -> {success=({input with pos=input.pos+1},void)}
      | {~failure} -> {~failure}

    /** Unpack single byte into bool **/
    bool(input:Pack.input) : Pack.result(bool) =
      do pinput("Pack.Unser.bool", input)
      match Decode.bool(input.binary, input.pos) with
      | {success=b} -> {success=({input with pos=input.pos+1},b)}
      | {~failure} -> {~failure}

    /** Unpack coded value **/
    coded(codes:Pack.codes, input:Pack.input) : Pack.result(Pack.data) =
      do pinput("Pack.Unser.coded", input)
      match Decode.coded(codes, input.binary, input.pos) with
      | {success=(pos, _code, data)} -> {success=({input with ~pos},data)}
      | {~failure} -> {~failure}

    /** Unpack reference value.
     * @param a function to unpack the value
     * @param input the input value
     * @result a Pack.result of a reference to the type returned by the unpack function, the reference is fresh
     **/
    ref(a:Pack.input -> Pack.result('a), input:Pack.input) : Pack.result(Server.reference('a)) =
      do pinput("Pack.Unser.ref", input)
      match a(input) with
      | {success=(input,a)} -> {success=(input,ServerReference.create(a))}
      | {~failure} -> {~failure}

    /** Unpack into reference value.
     * @param a function to unpack the value
     * @param r reference value
     * @param input the input value
     * @result a void Pack.result, the value read in is used to update the reference
     **/
    setref(a:Pack.input -> Pack.result('a), r:Server.reference('a), input:Pack.input) : Pack.result(void) =
      do pinput("Pack.Unser.setref", input)
      match a(input) with
      | {success=(input,a)} -> do ServerReference.set(r,a) {success=(input,void)}
      | {~failure} -> {~failure}

    /** Unpack optional value.
     * @param a function to unpack the value
     * @param input the input value
     * @result a Pack.result of an option of the type returned by the unpack function
     **/
    option(a:Pack.input -> Pack.result('a), input:Pack.input): Pack.result(option('a)) =
      do pinput("Pack.Unser.option", input)
      match Decode.byte(false, input.binary, input.pos) with
      | {success=0} -> {success=({input with pos=input.pos+1},{none})}
      | {success=1} ->
         match a({input with pos=input.pos+1}) with
         | {success=(input, a)} -> {success=(input,{some=a})}
         | {~failure} -> {~failure}
         end
      | {success=n} -> {failure="Pack.Unser.option: bad option code {n}"}
      | {~failure} -> {~failure}

    /** Unpack list of values.
     * @param a function to unpack the values
     * @param le endianness (true=little endian) of the list prefix
     * @param size the width for the list prefix
     * @param input the input value
     * @result a Pack.result of a list of values of the type returned by the unpack function
     **/
    list(a:Pack.input -> Pack.result('a), le:bool, size:Pack.s, input:Pack.input) : Pack.result(list('a)) =
      do pinput("Pack.Unser.list", input)
      match int(le, false, size, input) with
      | {success=(input,len)} ->
         do pinput("Pack.Unser.list: len={len}", input)
         rec aux(input, i, l) =
           if i == len
           then {success=(input,List.rev(l))}
           else
             match a(input) with
             | {success=(input, v:'a)} -> aux(input, i+1, [v|l])
             | {~failure} -> {~failure}
             end
         aux(input, 0, [])
      | {~failure} -> {~failure}

    /** Unpack array of values.
     * @param a function to unpack the values
     * @param le endianness (true=little endian) of the list prefix
     * @param size the width for the list prefix
     * @param input the input value
     * @result a Pack.result of an array of values of the type returned by the unpack function
     **/
    array(a:Pack.input -> Pack.result('a), le:bool, size:Pack.s, def:'a, input:Pack.input) : Pack.result(llarray('a)) =
      do pinput("Pack.Unser.array", input)
      match int(le, false, size, input) with
      | {success=(input,len)} ->
         do pinput("Pack.Unser.array: len={len}", input)
         arr = LowLevelArray.create(len, def)
         rec aux(input, i) =
           if i == len
           then {success=(input,arr)}
           else
             match a(input) with
             | {success=(input, v:'a)} ->
                _ = LowLevelArray.set(arr, i, v)
                aux(input, i+1)
             | {~failure} -> {~failure}
             end
         aux(input, 0)
      | {~failure} -> {~failure}

    /** Unpack 2-tuple of values.
     * @param input the input value
     * @param a function to unpack the first value
     * @param b function to unpack the second value
     * @result a Pack.result of a tuple of the two values
     **/
    tuple2(input:Pack.input,
           a:Pack.input -> Pack.result('a),
           b:Pack.input -> Pack.result('b)) : Pack.result(('a,'b)) =
      do pinput("Pack.Unser.tuple2", input)
      match a(input) with
      | {success=(input, a)} ->
         match b(input) with
         | {success=(input, b)} ->
            {success=(input, (a,b))}
         | {~failure} -> {~failure}
         end
      | {~failure} -> {~failure}

    /** Unpack a 3-tuple of values **/
    tuple3(input:Pack.input,
           a:Pack.input -> Pack.result('a),
           b:Pack.input -> Pack.result('b),
           c:Pack.input -> Pack.result('c)) : Pack.result(('a,'b,'c)) =
      do pinput("Pack.Unser.tuple3", input)
      match a(input) with
      | {success=(input, a)} ->
         match b(input) with
         | {success=(input, b)} ->
           match c(input) with
           | {success=(input, c)} ->
              {success=(input, (a,b,c))}
           | {~failure} -> {~failure}
           end
         | {~failure} -> {~failure}
         end
      | {~failure} -> {~failure}

    /** Unpack a 4-tuple of values **/
    tuple4(input:Pack.input,
           a:Pack.input -> Pack.result('a),
           b:Pack.input -> Pack.result('b),
           c:Pack.input -> Pack.result('c),
           d:Pack.input -> Pack.result('d)) : Pack.result(('a,'b,'c,'d)) =
      do pinput("Pack.Unser.tuple4", input)
      match a(input) with
      | {success=(input, a)} ->
         match b(input) with
         | {success=(input, b)} ->
           match c(input) with
           | {success=(input, c)} ->
             match d(input) with
             | {success=(input, d)} ->
                {success=(input, (a,b,c,d))}
             | {~failure} -> {~failure}
             end
           | {~failure} -> {~failure}
           end
         | {~failure} -> {~failure}
         end
      | {~failure} -> {~failure}

    /** Unpack a 5-tuple of values **/
    tuple5(input:Pack.input,
           a:Pack.input -> Pack.result('a),
           b:Pack.input -> Pack.result('b),
           c:Pack.input -> Pack.result('c),
           d:Pack.input -> Pack.result('d),
           e:Pack.input -> Pack.result('e)) : Pack.result(('a,'b,'c,'d,'e)) =
      do pinput("Pack.Unser.tuple5", input)
      match a(input) with
      | {success=(input, a)} ->
         match b(input) with
         | {success=(input, b)} ->
           match c(input) with
           | {success=(input, c)} ->
             match d(input) with
             | {success=(input, d)} ->
               match e(input) with
               | {success=(input, e)} ->
                  {success=(input, (a,b,c,d,e))}
               | {~failure} -> {~failure}
               end
             | {~failure} -> {~failure}
             end
           | {~failure} -> {~failure}
           end
         | {~failure} -> {~failure}
         end
      | {~failure} -> {~failure}

    /** Unpack a 6-tuple of values **/
    tuple6(input:Pack.input,
           a:Pack.input -> Pack.result('a),
           b:Pack.input -> Pack.result('b),
           c:Pack.input -> Pack.result('c),
           d:Pack.input -> Pack.result('d),
           e:Pack.input -> Pack.result('e),
           f:Pack.input -> Pack.result('f)) : Pack.result(('a,'b,'c,'d,'e,'f)) =
      do pinput("Pack.Unser.tuple6", input)
      match a(input) with
      | {success=(input, a)} ->
         match b(input) with
         | {success=(input, b)} ->
           match c(input) with
           | {success=(input, c)} ->
             match d(input) with
             | {success=(input, d)} ->
               match e(input) with
               | {success=(input, e)} ->
                 match f(input) with
                 | {success=(input, f)} ->
                    {success=(input, (a,b,c,d,e,f))}
                 | {~failure} -> {~failure}
                 end
               | {~failure} -> {~failure}
               end
             | {~failure} -> {~failure}
             end
           | {~failure} -> {~failure}
           end
         | {~failure} -> {~failure}
         end
      | {~failure} -> {~failure}

    /** Unpack a 7-tuple of values **/
    tuple7(input:Pack.input,
           a:Pack.input -> Pack.result('a),
           b:Pack.input -> Pack.result('b),
           c:Pack.input -> Pack.result('c),
           d:Pack.input -> Pack.result('d),
           e:Pack.input -> Pack.result('e),
           f:Pack.input -> Pack.result('f),
           g:Pack.input -> Pack.result('g)) : Pack.result(('a,'b,'c,'d,'e,'f,'g)) =
      do pinput("Pack.Unser.tuple7", input)
      match a(input) with
      | {success=(input, a)} ->
         match b(input) with
         | {success=(input, b)} ->
           match c(input) with
           | {success=(input, c)} ->
             match d(input) with
             | {success=(input, d)} ->
               match e(input) with
               | {success=(input, e)} ->
                 match f(input) with
                 | {success=(input, f)} ->
                   match g(input) with
                   | {success=(input, g)} ->
                      {success=(input, (a,b,c,d,e,f,g))}
                   | {~failure} -> {~failure}
                   end
                 | {~failure} -> {~failure}
                 end
               | {~failure} -> {~failure}
               end
             | {~failure} -> {~failure}
             end
           | {~failure} -> {~failure}
           end
         | {~failure} -> {~failure}
         end
      | {~failure} -> {~failure}

    /** Unpack value from binary data.
     * @param a function to extract the data from the binary
     * @param binary the binary data
     * @param use_all_data when true then all the data in the binary must be used or en error is returned
     * @result an outcome of the value extracted or a string error message
     **/
    unser(a:Pack.input -> Pack.result('a), binary:Pack.t, use_all_data:bool) : outcome('a,string) =
      input = {~binary; pos=0}
      do pinput("Pack.Unser.unser", input)
      match a(input) with
      | {success=(input, a)} ->
         len = Binary.length(binary)
         if use_all_data && input.pos != len
         then
           unused = len - input.pos
           {failure="Pack.Unser.unser: {unused} unused byte{if unused == 1 then "" else "s"} at end of data"}
         else {success=a}
      | {~failure} -> {~failure}

    /** Same as Pack.Unser.unser, except that the binary data is copied from a string **/
    from_string(a:Pack.input -> Pack.result('a), str:string, use_all_data:bool) : outcome('a,string) =
      unser(a, binary_of_string(str), use_all_data)

  }}

}}

// End of file pack.opa


