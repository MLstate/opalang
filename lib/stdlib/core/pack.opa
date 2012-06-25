/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Pack binary data into buffers.
 *
 * Important note: the buffers used by this module are currently founded upon the
 * Buffer module and are therefore imperative, non-serializable and are not
 * valid across thread boundaries.
 *
 * @author Norman Scaife
 * @destination EXTERNAL
 */

type Pack.t = binary

// TODO: multibyte characters

type Pack.s = {B} / {S} / {L} / {Ll}

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
 / {Pad}
 / {Padn:int}
 / {Void}
 / {Bool:bool}
 / {Cstring:string}
 / {String:string}
 / {String:string; size:Pack.s}
 / {Float32:float}
 / {Float:float}
 / {Coded:list((Pack.u, Pack.data))}
 / {List:(Pack.data,list(Pack.data))}
 / {Array:(Pack.data,llarray(Pack.data))}
 /// TODO: {Record:list((field_name,field_type,data))} // <-- problem, type vars

type Pack.data = list(Pack.u)

type Pack.input = { binary:binary; pos:int }

type Pack.result('a) = outcome((Pack.input,'a),string)

Pack = {{

  // For debugging buffer contents
  @private memdump = (%% BslPervasives.memdump %%: string -> string)

  debug = ServerReference.create(false)

  // convenience values
  littleEndian = true
  bigEndian = false
  defaultEndian = ServerReference.create(bigEndian)
  signedInts = true
  unsignedInts = false
  defaultInts = ServerReference.create(signedInts)
  sizeByte = {B}
  sizeShort = {S}
  sizeLong = {L}
  sizeLonglong = {Ll}
  defaultSize = ServerReference.create(sizeLong)

  // size of sized items
  sizesize(s:Pack.s): int = match s with | {B} -> 1 | {S} -> 2 | {L} -> 4 | {Ll} -> 8

#<Ifstatic:OPA_BACKEND_QMLJS>
  llsize = 0x001fffffffffffff // 53 bits
#<Else>
  llsize = 0x3fffffffffffffff // 62 bits
#<End>

  // maximum (unsigned) value for int
  sizemax(s:Pack.s) : int = match s with | {B} -> 0xff | {S} -> 0xffff | {L} -> 0xffffffff | {Ll} -> llsize

  // maximum (unsigned) value for int
  sizename(s:Pack.s) : string = match s with | {B} -> "byte" | {S} -> "short" | {L} -> "long" | {Ll} -> "longlong"

  // make size item
  mksize(s:Pack.s, i:int) : outcome(Pack.u,string) =
    match s with
    | {B} -> if i < 0 || i > 0xff then {failure="Pack.mksize: int {i} too big for byte size"} else {success={Byte=i}}
    | {S} -> if i < 0 || i > 0xffff then {failure="Pack.mksize: int {i} too big for short size"} else {success={Short=i}}
    | {L} -> if i < 0 || i > 0xffffffff then {failure="Pack.mksize: int {i} too big for long size"} else {success={Long=i}}
    | {Ll} -> {success={Longlong=i}}

  // recover size from item
  getsize(u:Pack.u) : outcome(int,string) =
    match u with
    | {~Byte} -> {success=Byte}
    | {~Short} -> {success=Short}
    | {~Long} -> {success=Long}
    | {~Longlong} -> {success=Longlong}
    | {~Int} -> {success=Int}
    | {Int64=i} -> {success=Int64.to_int(i)}
    | _ -> {failure="Pack.getsize: size {u} is not int"}

  // Encode
  Encode = {{

    // char
    char(buf:Pack.t, c:string) : outcome(void,string) =
      b = String.byte_at_unsafe(0,c)
      not8 = b < 0 || b > 0xff
      if ((String.length(c) != 1) || not8)
      then {failure="Pack.Encode.char: multi-byte char \"{c}\""}
      else {success=Binary.add_uint8(buf, b)}

    // octet
    octet(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x80 || i > 0x7f)
      then {failure="Pack.Encode.byte: out of range {i}"}
      else {success=Binary.add_int8(buf, i)}
    uoctet(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xff)
      then {failure="Pack.Encode.byte: out of range {i}"}
      else {success=Binary.add_uint8(buf, i)}

    // byte
    byte(buf:Pack.t, signed:bool, b:int): outcome(void,string) =
      if signed then octet(buf, b) else uoctet(buf, b)

    // short
    short_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x8000 || i > 0xffff)
      then {failure="Pack.Encode.short_be: out of range {i}"}
      else {success=Binary.add_int16_be(buf, i)}
    short_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x8000 || i > 0xffff)
      then {failure="Pack.Encode.short_le: out of range {i}"}
      else {success=Binary.add_int16_le(buf, i)}
    ushort_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffff)
      then {failure="Pack.Encode.ushort_be: out of range {i}"}
      else {success=Binary.add_uint16_be(buf, i)}
    ushort_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffff)
      then {failure="Pack.Encode.ushort_le: out of range {i}"}
      else {success=Binary.add_uint16_le(buf, i)}

    // short
    short(buf:Pack.t, le:bool, signed:bool, s:int): outcome(void,string) =
      match (le, signed) with
      | ({false},{false}) -> ushort_be(buf, s)
      | ({true},{false}) -> ushort_le(buf, s)
      | ({false},{true}) -> short_be(buf, s)
      | ({true},{true}) -> short_le(buf, s)

    // long
    long_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x80000000 || i > 0x7fffffff)
      then {failure="Pack.Encode.long_be: out of range {i}"}
      else {success=Binary.add_int32_be(buf, i)}
    long_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x80000000 || i > 0x7fffffff)
      then {failure="Pack.Encode.long_le: out of range {i}"}
      else {success=Binary.add_int32_le(buf, i)}
    ulong_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffffffff)
      then {failure="Pack.Encode.ulong_be: out of range {i}"}
      else {success=Binary.add_uint32_be(buf, i)}
    ulong_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffffffff)
      then {failure="Pack.Encode.ulong_le: out of range {i}"}
      else {success=Binary.add_uint32_le(buf, i)}

    // long
    long(buf:Pack.t, le:bool, signed:bool, l:int): outcome(void,string) =
      match (le, signed) with
      | ({false},{false}) -> ulong_be(buf, l)
      | ({true},{false}) -> ulong_le(buf, l)
      | ({false},{true}) -> long_be(buf, l)
      | ({true},{true}) -> long_le(buf, l)

    // These are restricted to 53 bits
    longlong_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x20000000000000 || i > 0x1fffffffffffff)
      then {failure="Pack.Encode.ulonglong_be: out of range {i}"}
      else {success=Binary.add_int53_be(buf, i)}
    longlong_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x20000000000000 || i > 0x1fffffffffffff)
      then {failure="Pack.Encode.ulonglong_le: out of range {i}"}
      else {success=Binary.add_int53_le(buf, i)}

    // longlong
    longlong(buf:Pack.t, le:bool, b:int): outcome(void,string) =
      if le then longlong_le(buf, b) else longlong_be(buf, b)

    int64_be(buf:Pack.t, i:int64) : outcome(void,string) =
      {success=Binary.add_uint64_be(buf, i)}
    int64_le(buf:Pack.t, i:int64) : outcome(void,string) =
      {success=Binary.add_uint64_le(buf, i)}

    // int64
    int64(buf:Pack.t, le:bool, b:int64): outcome(void,string) =
      if le then int64_le(buf, b) else int64_be(buf, b)

    // pad
    pad(buf:Pack.t) : outcome(void,string) =
      octet(buf, 0)

    // padn
    padn(buf:Pack.t, n:int) : outcome(void,string) =
      {success=Binary.add_string(buf, String.repeat(n, String.of_byte_unsafe(0)))}

    // bool
    bool(buf:Pack.t, b:bool) : outcome(void,string) =
      octet(buf, if b then 1 else 0)

    // cstring
    cstring(buf:Pack.t, str:string) : outcome(void,string) =
      do Binary.add_string(buf, str)
      octet(buf, 0)

    // string
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

    // string derivatives
    string_b(buf:Pack.t, str:string) : outcome(void,string) = string(buf, false, {B}, str)
    string_s(buf:Pack.t, le:bool, str:string) : outcome(void,string) = string(buf, le, {S}, str)
    string_s_le(buf:Pack.t, str:string) : outcome(void,string) = string(buf, true, {S}, str)
    string_s_be(buf:Pack.t, str:string) : outcome(void,string) = string(buf, false, {S}, str)
    string_l(buf:Pack.t, le:bool, str:string) : outcome(void,string) = string(buf, le, {L}, str)
    string_l_le(buf:Pack.t, str:string) : outcome(void,string) = string(buf, true, {L}, str)
    string_l_be(buf:Pack.t, str:string) : outcome(void,string) = string(buf, false, {L}, str)
    string_ll(buf:Pack.t, le:bool, str:string) : outcome(void,string) = string(buf, le, {Ll}, str)
    string_ll_le(buf:Pack.t, str:string) : outcome(void,string) = string(buf, true, {Ll}, str)
    string_ll_be(buf:Pack.t, str:string) : outcome(void,string) = string(buf, false, {Ll}, str)

    // float32
    float_be(buf:Pack.t, f:float) : outcome(void,string) =
      {success=Binary.add_float_be(buf, f)}
    float_le(buf:Pack.t, f:float) : outcome(void,string) =
      {success=Binary.add_float_le(buf, f)}

    float(buf:Pack.t, le:bool, f:float): outcome(void,string) =
      if le then float_le(buf, f) else float_be(buf, f)

    // float
    double_be(buf:Pack.t, f:float) : outcome(void,string) =
      {success=Binary.add_double_be(buf, f)}
    double_le(buf:Pack.t, f:float) : outcome(void,string) =
      {success=Binary.add_double_le(buf, f)}

    // float
    double(buf:Pack.t, le:bool, f:float): outcome(void,string) =
      if le then double_le(buf, f) else double_be(buf, f)

    // coded
    coded(buf:Pack.t, le:bool, signed:bool, size:Pack.s, code:Pack.u, data:Pack.data)
          : (bool, bool, Pack.s, outcome(void,string)) =
      match pack_u(buf, le, signed, size, code) with
      | (le, signed, size, {success=_}) -> pack_data(buf, le, signed, size, data)
      | failure -> failure

    // same item type
    same_u(u1:Pack.u, u2:Pack.u): bool =
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
      | ({Int64=_},{Int64=_}) -> true
      | ({Pad},{Pad}) -> true
      | ({Padn=_},{Padn=_}) -> true
      | ({Void},{Void}) -> true
      | ({Bool=_},{Bool=_}) -> true
      | ({Cstring=_},{Cstring=_}) -> true
      | ({String=_},{String=_}) -> true
      | ({String=_; size=s1},{String=_; size=s2}) -> s1 == s2
      | ({Float32=_},{Float32=_}) -> true
      | ({Float=_},{Float=_}) -> true
      | ({Coded=_},{Coded=_}) -> true
      | ({List=_},{List=_}) -> true
      | ({Array=_},{Array=_}) -> true
      | (_,_) -> false

    // same data type
    same_data(d1:Pack.data, d2:Pack.data): bool =
      match List.for_all2((u1, u2 -> same_u(u1,u2)),d1,d2) with
      | {result=tf} -> tf
      | _ -> false

    // list
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
    packitemsize(s:Pack.s, u:Pack.u) : (Pack.s, int) =
      match u with
      | {Be} -> (s,0)
      | {Le} -> (s,0)
      | {Signed} -> (s,0)
      | {Unsigned} -> (s,0)
      | {B} -> ({B},0)
      | {S} -> ({S},0)
      | {L} -> ({L},0)
      | {Ll} -> ({Ll},0)
      | {Char=_} -> (s,1)
      | {Byte=_} -> (s,1)
      | {Short=_} -> (s,2)
      | {Long=_} -> (s,4)
      | {Longlong=_} -> (s,8)
      | {Int=_} -> (s,sizesize(s))
      | {Int=_; ~size} -> (s,sizesize(size))
      | {Int=_; signed=_} -> (s,sizesize(s))
      | {Int=_; ~size; signed=_} -> (s,sizesize(size))
      | {Int64=_} -> (s,8)
      | {Pad} -> (s,1)
      | {~Padn} -> (s,Padn)
      | {Void} -> (s,1)
      | {Bool=_} -> (s,1)
      | {Cstring=str} -> (s,String.length(str)+1)
      | {String=str} -> (s,sizesize(s)+String.length(str))
      | {String=str; ~size} -> (s,sizesize(size)+String.length(str))
      | {Float32=_} -> (s,4)
      | {Float=_} -> (s,8)
      | {Coded=[]} -> (s,0) // Can't pack nothing
      | {Coded=[(code,data)]} -> (s,icnt) = packitemsize(s,code) (s,dcnt) = packdatasize(s,data) (s, icnt + dcnt)
      | {Coded=_} -> (s,0) // Will generate error
      | {List=(_,l)} -> List.fold((data, (s,cnt) -> (s,dcnt) = packdatasize(s, data) (s, cnt+dcnt)),l,(s,sizesize(s)))
      | {Array=(_,a)} -> LowLevelArray.fold((data, (s,cnt) -> (s,dcnt) = packdatasize(s, data) (s, cnt+dcnt)),a,(s,sizesize(s)))

    // predict pack length
    packdatasize(s:Pack.s, data:Pack.data) : (Pack.s, int) =
      List.fold((u, (s, len) -> match packitemsize(s, u) with | (s,size) -> (s,len+size)), data, (s,0))

    // predict pack length
    packlen(data:Pack.data) : int =
      (List.fold((u, (s, len) ->
                  match packitemsize(s, u) with
                  | (s,size) -> (s,len+size)), data, (ServerReference.get(defaultSize),0))).f2

    // missing from LowLevelArray?
    @private a2l(a:llarray('a)) : list('a) =
      size = LowLevelArray.size(a)
      rec aux(i) = if i == size then [] else [LowLevelArray.get(a, i)|aux(i+1)]
      aux(0)

    @private pack_string(buf:Pack.t, le:bool, signed:bool, actual_size:Pack.s, return_size:Pack.s, str:string)
                         : (bool, bool, Pack.s, outcome(void,string)) =
      match actual_size with
      | {B} -> (le, signed, return_size, string_b(buf, str))
      | {S} -> (le, signed, return_size,  string_s(buf, le, str))
      | {L} -> (le, signed, return_size,  string_l(buf, le, str))
      | {Ll} -> (le, signed, return_size, string_ll(buf, le, str))

    @private pack_int(buf:Pack.t, le:bool, actual_signed:bool, return_signed:bool, actual_size:Pack.s, return_size:Pack.s, i:int)
                      : (bool, bool, Pack.s, outcome(void,string)) =
      match actual_size with
      | {B} -> (le, return_signed, return_size, byte(buf, actual_signed, i))
      | {S} -> (le, return_signed, return_size,  short(buf, le, actual_signed, i))
      | {L} -> (le, return_signed, return_size,  long(buf, le, actual_signed, i))
      | {Ll} -> (le, return_signed, return_size, longlong(buf, le, i))

    // pack item into buffer
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
      | {Int=i} -> pack_int(buf, le, signed, signed, size, size, i)
      | {Int=i; size=actual_size} -> pack_int(buf, le, signed, signed, actual_size, size, i)
      | {Int=i; signed=actual_signed} -> pack_int(buf, le, actual_signed, signed, size, size, i)
      | {Int=i; size=actual_size; signed=actual_signed} -> pack_int(buf, le, actual_signed, signed, actual_size, size, i)
      | {Int64=i64} -> (le, signed, size, int64(buf, le, i64))
      | {Pad} -> (le, signed, size, pad(buf))
      | {~Padn} -> (le, signed, size, padn(buf, Padn))
      | {Void} -> (le, signed, size, pad(buf))
      | {~Bool} -> (le, signed, size, bool(buf, Bool))
      | {~Cstring} -> (le, signed, size, cstring(buf, Cstring))
      | {String=str} -> pack_string(buf, le, signed, size, size, str)
      | {String=str; size=actual_size} -> pack_string(buf, le, signed, actual_size, size, str)
      | {~Float32} -> (le, signed, size, float(buf, le, Float32))
      | {~Float} -> (le, signed, size, double(buf, le, Float))
      | {Coded=[]} -> (le, signed, size, {failure="Pack.Encode.pack: Coded has no codes"})
      | {Coded=[(code,data)]} -> coded(buf, le, signed, size, code, data)
      | {Coded=_} -> (le, signed, size, {failure="Pack.Encode.pack: Coded has multiple codes"})
      | {List=(t,l)} -> list(buf, le, signed, size, t, l)
      | {Array=(t,a)} -> list(buf, le, signed, size, t, a2l(a))

    // pack data into buffer
    pack_data(buf:Pack.t, le:bool, signed:bool, size:Pack.s, data:Pack.data) : (bool, bool, Pack.s, outcome(void,string)) =
      List.fold((u, (le, signed, size, res) ->
                 match res with
                 | {~failure} -> (le, signed, size, {~failure})
                 | {success=_} -> pack_u(buf, le, signed, size, u)
                ), data, (le, signed, size, {success:void}))

    // pack data into string
    pack(data:Pack.data) : outcome(binary,string) =
      buf = Binary.create(packlen(data))
      (_, _, _, res) = pack_data(buf, ServerReference.get(defaultEndian), ServerReference.get(defaultInts),
                                         ServerReference.get(defaultSize), data);
      match res with
      | {success=_} -> {success=buf}
      | {~failure} -> {~failure}

    // TODO: ser_xxx functions to parallel unser_xxx

  }}

  // Decode
  Decode = {{

    pinput(name, input) =
      if ServerReference.get(debug)
      then
        data = Binary.get_string(input.binary,input.pos,Int.min(32,Binary.length(input.binary)-input.pos))
        jlog("{name}: input=\n{memdump(data)}")
      else void

    // char
    char(data:binary, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.char",{binary=data; ~pos})
      if Binary.length(data) >= pos + 1
      then {success=Binary.get_string(data, pos, 1)}
      else {failure="Pack.Decode.char: not enough data for char"}

    // octet
    octet(data:binary, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.byte",{binary=data; ~pos})
      if Binary.length(data) >= pos + 1
      then {success=Binary.get_int8(data, pos)}
      else {failure="Pack.Decode.byte: not enough data for byte"}
    uoctet(data:binary, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.byte",{binary=data; ~pos})
      if Binary.length(data) >= pos + 1
      then {success=Binary.get_uint8(data, pos)}
      else {failure="Pack.Decode.byte: not enough data for byte"}

    byte(signed:bool, data:binary, pos:int) : outcome(int,string) =
      if signed then octet(data, pos) else uoctet(data, pos)

    // short
    ushort_be(data:binary, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.ushort_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 2
      then {success=Binary.get_uint16_be(data, pos)}
      else {failure="Pack.Decode.ushort_be: not enough data for short"}
    ushort_le(data:binary, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.ushort_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 2
      then {success=Binary.get_uint16_le(data, pos)}
      else {failure="Pack.Decode.ushort_le: not enough data for short"}
    short_be(data:binary, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.short_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 2
      then {success=Binary.get_int16_be(data, pos)}
      else {failure="Pack.Decode.short_be: not enough data for short"}
    short_le(data:binary, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.short_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 2
      then {success=Binary.get_int16_le(data, pos)}
      else {failure="Pack.Decode.short_le: not enough data for short"}

    short(le:bool, signed:bool, data:binary, pos:int): outcome(int,string) =
      match (le, signed) with
      | ({false},{false}) -> ushort_be(data, pos)
      | ({true},{false}) -> ushort_le(data, pos)
      | ({false},{true}) -> short_be(data, pos)
      | ({true},{true}) -> short_le(data, pos)

    // long
    long_be(data:binary, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.long_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_int32_be(data, pos)}
      else {failure="Pack.Decode.long_le: not enough data for long"}
    long_le(data:binary, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.long_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_int32_le(data, pos)}
      else {failure="Pack.Decode.long_le: not enough data for long"}
    ulong_be(data:binary, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.ulong_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_uint32_be(data, pos)}
      else {failure="Pack.Decode.ulong_le: not enough data for long"}
    ulong_le(data:binary, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.ulong_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_uint32_le(data, pos)}
      else {failure="Pack.Decode.ulong_le: not enough data for long"}

    long(le:bool, signed:bool, data:binary, pos:int): outcome(int,string) =
      match (le, signed) with
      | ({false},{false}) -> ulong_be(data, pos)
      | ({true},{false}) -> ulong_le(data, pos)
      | ({false},{true}) -> long_be(data, pos)
      | ({true},{true}) -> long_le(data, pos)

    // longlong
    longlong_be(data:binary, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.longlong_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_int53_be(data, pos)}
      else {failure="Pack.Decode.longlong_le: not enough data for longlong"}
    longlong_le(data:binary, pos:int) : outcome(int,string) =
      do pinput("Pack.Decode.longlong_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_int53_le(data, pos)}
      else {failure="Pack.Decode.longlong_le: not enough data for longlong"}

    longlong(le:bool, data:binary, pos:int) : outcome(int,string) =
      if le then longlong_le(data, pos) else longlong_be(data, pos)

    // generalised int
    int(le:bool, signed:bool, s:Pack.s, data:binary, pos:int) : outcome(int,string) =
      match s with
      | {B} -> byte(signed, data, pos) // no endian
      | {S} -> short(le, signed, data, pos)
      | {L} -> long(le, signed, data, pos)
      | {Ll} -> longlong(le, data, pos) // always signed

    // int64
    int64_le(data:binary, pos:int) : outcome(int64,string) =
      do pinput("Pack.Decode.int64_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 8
      then {success=Binary.get_uint64_le(data, pos)}
      else {failure="Pack.Decode.int64_le: not enough data for int64"}
    int64_be(data:binary, pos:int) : outcome(int64,string) =
      do pinput("Pack.Decode.int64_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 8
      then {success=Binary.get_uint64_be(data, pos)}
      else {failure="Pack.Decode.int64_be: not enough data for int64"}

    int64(le:bool, data:binary, pos:int) : outcome(int64,string) =
      if le then int64_le(data, pos) else int64_be(data, pos)

    // pad
    pad(data:binary, pos:int) : outcome(void,string) =
      do pinput("Pack.Decode.pad",{binary=data; ~pos})
      if Binary.length(data) >= pos + 1
      then {success=void}
      else {failure="Pack.Decode.pad: not enough data for padding"}

    // padn
    padn(data:binary, pos:int, n:int) : outcome(void,string) =
      do pinput("Pack.Decode.padn({n})",{binary=data; ~pos})
      if Binary.length(data) >= pos + n
      then {success=void}
      else {failure="Pack.Decode.padn: not enough data for padding"}

    // bool
    bool(data:binary, pos:int) : outcome(bool,string) =
      do pinput("Pack.Decode.bool",{binary=data; ~pos})
      if Binary.length(data) >= pos + 1
      then {success=Binary.get_uint8(data, pos) != 0}
      else {failure="Pack.Decode.bool: not enough data for bool"}

    @private clen(data:binary, pos:int) : option(int) =
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

    // cstring
    cstring(data:binary, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.cstring",{binary=data; ~pos})
      match clen(data, pos) with
      | {some=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos, len)}
        else {failure="Pack.Decode.cstring: not enough data for string"}
      | {none} ->
        {failure="Pack.Decode.cstring: can't find null"}

    //bytestr
    string_b(data:binary, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.string_b",{binary=data; ~pos})
      match uoctet(data, pos) with
      | {success=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos + 1, len)}
        else {failure="Pack.Decode.string_b: not enough data for string"}
      | {~failure} -> {~failure}

    //shortstr
    string_s_be(data:binary, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.string_s_be",{binary=data; ~pos})
      match short_be(data, pos) with
      | {success=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos + 2, len)}
        else {failure="Pack.Decode.string_s_be: not enough data for string"}
      | {~failure} -> {~failure}
    string_s_le(data:binary, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.string_s_le",{binary=data; ~pos})
      match short_le(data, pos) with
      | {success=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos + 2, len)}
        else {failure="Pack.Decode.string_s_le: not enough data for string"}
      | {~failure} -> {~failure}

    string_s(le:bool, data:binary, pos:int) : outcome(string,string) =
      if le then string_s_le(data, pos) else string_s_be(data, pos)

    //longstr
    string_l_be(data:binary, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.string_l_be",{binary=data; ~pos})
      match long_be(data, pos) with
      | {success=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos + 4, len)}
        else {failure="Pack.Decode.string_l_be: not enough data for string ({Binary.length(data)}:{data} > ({pos} + {len})"}
      | {~failure} -> {~failure}
    string_l_le(data:binary, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.string_l_le",{binary=data; ~pos})
      match long_le(data, pos) with
      | {success=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos + 4, len)}
        else {failure="Pack.Decode.string_l_le: not enough data for string ({Binary.length(data)}:{data} > ({pos} + {len})"}
      | {~failure} -> {~failure}

    string_l(le:bool, data:binary, pos:int) : outcome(string,string) =
      if le then string_l_le(data, pos) else string_l_be(data, pos)

    //longlongstr
    string_ll_be(data:binary, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.string_ll_be",{binary=data; ~pos})
      match longlong_be(data, pos) with
      | {success=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos + 8, len)}
        else {failure="Pack.Decode.string_ll_be: not enough data for string ({Binary.length(data)}:{data} > ({pos} + {len})"}
      | {~failure} -> {~failure}
    string_ll_le(data:binary, pos:int) : outcome(string,string) =
      do pinput("Pack.Decode.string_ll_le",{binary=data; ~pos})
      match longlong_le(data, pos) with
      | {success=len} ->
        if Binary.length(data) > pos + len
        then {success=Binary.get_string(data, pos + 8, len)}
        else {failure="Pack.Decode.string_ll_le: not enough data for string ({Binary.length(data)}:{data} > ({pos} + {len})"}
      | {~failure} -> {~failure}

    string_ll(le:bool, data:binary, pos:int) : outcome(string,string) =
      if le then string_ll_le(data, pos) else string_ll_be(data, pos)

    // generalised string
    string(le:bool, size:Pack.s, data:binary, pos:int) : outcome(string,string) =
      match size with
      | {B} -> string_b(data, pos)
      | {S} -> string_s(le, data, pos)
      | {L} -> string_l(le, data, pos)
      | {Ll} -> string_ll(le, data, pos)

    // float
    float_le(data:binary, pos:int) : outcome(float,string) =
      do pinput("Pack.Decode.float_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_float_le(data, pos)}
      else {failure="Pack.Decode.float_le: not enough data for float"}
    float_be(data:binary, pos:int) : outcome(float,string) =
      do pinput("Pack.Decode.float_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 4
      then {success=Binary.get_float_be(data, pos)}
      else {failure="Pack.Decode.float_be: not enough data for float"}

    float(le:bool, data:binary, pos:int) : outcome(float,string) =
      if le then float_le(data, pos) else float_be(data, pos)

    double_le(data:binary, pos:int) : outcome(float,string) =
      do pinput("Pack.Decode.double_le",{binary=data; ~pos})
      if Binary.length(data) >= pos + 8
      then {success=Binary.get_double_le(data, pos)}
      else {failure="Pack.Decode.double_le: not enough data for double"}
    double_be(data:binary, pos:int) : outcome(float,string) =
      do pinput("Pack.Decode.double_be",{binary=data; ~pos})
      if Binary.length(data) >= pos + 8
      then {success=Binary.get_double_be(data, pos)}
      else {failure="Pack.Decode.double_be: not enough data for double"}

    double(le:bool, data:binary, pos:int) : outcome(float,string) =
      if le then double_le(data, pos) else double_be(data, pos)

    @private mksla(lora:bool, typ:Pack.data, l:list(Pack.data)) : Pack.u =
      if lora then {List=(typ,l)} else {Array=(typ,LowLevelArray.of_list(l))}

    // list
    list(lora:bool, le:bool, signed:bool, s:Pack.s, typ:Pack.data, bin:binary, pos:int, data:Pack.data)
         : outcome((bool, bool, Pack.s, int, Pack.data),string) =
      do pinput("Pack.Decode.list",{binary=bin; ~pos})
      match mksize(s, 0) with
      | {success=size} ->
        match unpack([size], bin, pos) with
        | {success=(pos,[size])} ->
          match getsize(size) with
          | {success=len} ->
            rec aux(i, l, pos) =
              if i == len
              then {success=(le, signed, s, pos, [mksla(lora, typ, List.rev(l))|data])}
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

    @private unpack_int(data:Pack.data, le:bool,
                        actual_signed:option(bool), return_signed:bool, actual_size:option(Pack.s), return_size:Pack.s,
                        bin:binary, pos:int)
                        : outcome((bool, bool, Pack.s, int, Pack.data),string) =
      real_size = Option.default(return_size, actual_size)
      real_signed = Option.default(return_signed, actual_signed)
      match int(le, real_signed, real_size, bin, pos) with
      | {success=i} ->
         {success=(le, return_signed, return_size, pos+sizesize(real_size),
                   [(match (actual_size,actual_signed) with
                     | ({none},{none}) -> {Int=i}
                     | ({some=actual_size},{none}) -> {Int=i; size=actual_size}
                     | ({none},{some=actual_signed}) -> {Int=i; signed=actual_signed}
                     | ({some=actual_size},{some=actual_signed}) -> {Int=i; size=actual_size; signed=actual_signed}
                    )|data])}
      | {~failure} -> {~failure}

    @private unpack_string(data:Pack.data, le:bool, signed:bool, actual_size:option(Pack.s), return_size:Pack.s,
                              bin:binary, pos:int)
                              : outcome((bool, bool, Pack.s, int, Pack.data),string) =
      real_size = Option.default(return_size, actual_size)
      match
        match real_size with
        | {B} -> string_b(bin, pos)
        | {L} -> string_l(le, bin, pos)
        | {S} -> string_s(le, bin, pos)
        | {Ll} -> string_ll(le, bin, pos)
      with
      | {success=s} ->
         {success=(le, signed, return_size, pos+String.length(s)+sizesize(real_size),
                   [(if Option.is_some(actual_size)
                     then {String=s; size=Option.get(actual_size)}
                     else {String=s})|data])}
      | {~failure} -> {~failure}

    // unpack data
    unpack(data:Pack.data, bin:binary, pos:int) : outcome((int,Pack.data),string) =
      do pinput("Pack.Decode.unpack",{binary=bin; ~pos})
      if data == [] then {success=(pos,[])} else
      match
        List.fold((u:Pack.u, acc ->
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
                          unpack_int(data, le, {none}, signed, {none}, size, bin, pos)
                       | {Int=_; size=actual_size} ->
                          unpack_int(data, le, {none}, signed, {some=actual_size}, size, bin, pos)
                       | {Int=_; signed=actual_signed} ->
                          unpack_int(data, le, {some=actual_signed}, signed, {none}, size, bin, pos)
                       | {Int=_; size=actual_size; signed=actual_signed} ->
                          unpack_int(data, le, {some=actual_signed}, signed, {some=actual_size}, size, bin, pos)
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
                       | {String=_} -> unpack_string(data, le, signed, {none}, size, bin, pos)
                       | {String=_; size=actual_size} -> unpack_string(data, le, signed, {some=actual_size}, size, bin, pos)
                       | {Float32=_} ->
                          (match float(le, bin, pos) with
                           | {success=Float32} -> {success=(le, signed, size, pos+4, [{~Float32}|data])}
                           | {~failure} -> {~failure})
                       | {Float=_} ->
                          (match double(le, bin, pos) with
                           | {success=Float} -> {success=(le, signed, size, pos+8, [{~Float}|data])}
                           | {~failure} -> {~failure})
                       | {Coded=[]} -> {failure="Pack.Decode.unpack: Coded has no codes"}
                       | {Coded=[(code1,data1)|codes]} ->
                          (match unpack([code1], bin, pos) with
                           | {success=(npos,[code])} ->
                             (match List.assoc(code, [(code1,data1)|codes]) with
                              | {some=cdata} ->
                                 (match unpack(cdata, bin, npos) with
                                  | {success=(npos,ndata)} -> {success=(le, signed, size, npos, [{Coded=[(code,ndata)]}|data])}
                                  | {~failure} -> {failure="Pack.Decode.unpack: Coded has bad data {failure}"})
                              | {none} -> {failure="Pack.Decode.unpack: Coded has missing code {code}"})
                           | {success=_} -> {failure="Pack.Decode.unpack: Coded has multiple decodings"}
                           | {~failure} -> {failure="Pack.Decode.unpack: Coded has bad code {failure}"})
                       | {List=(typ,_)} -> list(true, le, signed, size, typ, bin, pos, data)
                       | {Array=(typ,_)} -> list(false, le, signed, size, typ, bin, pos, data)
                       )), data, {success=(ServerReference.get(defaultEndian),
                                           ServerReference.get(defaultInts),
                                           ServerReference.get(defaultSize), pos, [])})
      with
      | {success=(_,_,_,pos,data)} -> {success=(pos,List.rev(data))}
      | {~failure} -> {~failure}

    // meaningful entities
    has_data(u:Pack.u) : option(Pack.u) =
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
      | {Int=_} -> {some=u}
      | {~Int; ...} -> {some={~Int}}
      | {Int64=_} -> {some=u}
      | {Pad} -> none
      | {Padn=_} -> none
      | {Void} -> {some=u}
      | {Bool=_} -> {some=u}
      | {Cstring=_} -> {some=u}
      | {~String; ...} -> {some={~String}}
      | {Float32=_} -> {some=u}
      | {Float=_} -> {some=u}
      | {Coded=_} -> {some=u}
      | {List=_} -> {some=u}
      | {Array=_} -> {some=u}

    // cleanup elements with no data
    clean(data:Pack.data) = List.filter_map(has_data, data)

    // The unser functions provide a compositional functional decode over the Pack.input type

    // TODO: complete this list of functions

    unser_char(input:Pack.input) : Pack.result(string) =
      match char(input.binary, input.pos) with
      | {success=c} -> {success=({input with pos=input.pos+1},c)}
      | {~failure} -> {~failure}

    unser_int(le:bool, signed:bool, size:Pack.s, input:Pack.input) : Pack.result(int) =
      do pinput("unser_int", input)
      match int(le, signed, size, input.binary, input.pos) with
      | {success=i} -> {success=({input with pos=input.pos+sizesize(size)},i)}
      | {~failure} -> {~failure}

    unser_float(le:bool, input:Pack.input) : Pack.result(float) =
      do pinput("unser_float", input)
      match double(le, input.binary, input.pos) with
      | {success=f} -> {success=({input with pos=input.pos+8},f)}
      | {~failure} -> {~failure}

    unser_string(le:bool, size:Pack.s, input:Pack.input) : Pack.result(string) =
      do pinput("unser_string", input)
      match string(le, size, input.binary, input.pos) with
      | {success=s} -> {success=({input with pos=input.pos+sizesize(size)+String.length(s)},s)}
      | {~failure} -> {~failure}

    unser_bool(input:Pack.input) : Pack.result(bool) =
      do pinput("unser_bool", input)
      match bool(input.binary, input.pos) with
      | {success=b} -> {success=({input with pos=input.pos+1},b)}
      | {~failure} -> {~failure}

    unser_ref(unser_a:Pack.input -> Pack.result('a), input:Pack.input)
              : Pack.result(Server.reference('a)) =
      match unser_a(input) with
      | {success=(input,a)} -> {success=(input,ServerReference.create(a))}
      | {~failure} -> {~failure}

    unser_option(unser_a:Pack.input -> Pack.result('a), input:Pack.input): Pack.result(option('a)) =
      do pinput("unser_option", input)
      match byte(false, input.binary, input.pos) with
      | {success=0} -> {success=({input with pos=input.pos+1},{none})}
      | {success=1} ->
         match unser_a({input with pos=input.pos+1}) with
         | {success=(input, a)} -> {success=(input,{some=a})}
         | {~failure} -> {~failure}
         end
      | {success=n} -> {failure="Pack.unser_option: bad option code {n}"}
      | {~failure} -> {~failure}

    unser_array(unser_a:Pack.input -> Pack.result('a), le:bool, size:Pack.s, def:'a, input:Pack.input)
                : Pack.result(llarray('a)) =
      do pinput("unser_array", input)
      match unser_int(le, false, size, input) with
      | {success=(input,len)} ->
         do if ServerReference.get(debug) then jlog("unser_array: len={len}") else void
         a = LowLevelArray.create(len, def)
         rec aux(input, i) =
           if i == len
           then {success=(input,a)}
           else
             match unser_a(input) with
             | {success=(input, v:'a)} ->
                _ = LowLevelArray.set(a, i, v)
                aux(input, i+1)
             | {~failure} -> {~failure}
             end
         aux(input, 0)
      | {~failure} -> {~failure}

    unser2(input:Pack.input,
           unser_a:Pack.input -> Pack.result('a),
           unser_b:Pack.input -> Pack.result('b)) : Pack.result(('a,'b)) =
      match unser_a(input) with
      | {success=(input, a)} ->
         match unser_b(input) with
         | {success=(input, b)} ->
            {success=(input, (a,b))}
         | {~failure} -> {~failure}
         end
      | {~failure} -> {~failure}

    unser3(input:Pack.input,
           unser_a:Pack.input -> Pack.result('a),
           unser_b:Pack.input -> Pack.result('b),
           unser_c:Pack.input -> Pack.result('c)) : Pack.result(('a,'b,'c)) =
      match unser_a(input) with
      | {success=(input, a)} ->
         match unser_b(input) with
         | {success=(input, b)} ->
           match unser_c(input) with
           | {success=(input, c)} ->
              {success=(input, (a,b,c))}
           | {~failure} -> {~failure}
           end
         | {~failure} -> {~failure}
         end
      | {~failure} -> {~failure}

    unser4(input:Pack.input,
           unser_a:Pack.input -> Pack.result('a),
           unser_b:Pack.input -> Pack.result('b),
           unser_c:Pack.input -> Pack.result('c),
           unser_d:Pack.input -> Pack.result('d)) : Pack.result(('a,'b,'c,'d)) =
      do pinput("unser4", input)
      match unser_a(input) with
      | {success=(input, a)} ->
         match unser_b(input) with
         | {success=(input, b)} ->
           match unser_c(input) with
           | {success=(input, c)} ->
             match unser_d(input) with
             | {success=(input, d)} ->
                {success=(input, (a,b,c,d))}
             | {~failure} -> {~failure}
             end
           | {~failure} -> {~failure}
           end
         | {~failure} -> {~failure}
         end
      | {~failure} -> {~failure}

    unser5(input:Pack.input,
           unser_a:Pack.input -> Pack.result('a),
           unser_b:Pack.input -> Pack.result('b),
           unser_c:Pack.input -> Pack.result('c),
           unser_d:Pack.input -> Pack.result('d),
           unser_e:Pack.input -> Pack.result('e)) : Pack.result(('a,'b,'c,'d,'e)) =
      match unser_a(input) with
      | {success=(input, a)} ->
         match unser_b(input) with
         | {success=(input, b)} ->
           match unser_c(input) with
           | {success=(input, c)} ->
             match unser_d(input) with
             | {success=(input, d)} ->
               match unser_e(input) with
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

    unser6(input:Pack.input,
           unser_a:Pack.input -> Pack.result('a),
           unser_b:Pack.input -> Pack.result('b),
           unser_c:Pack.input -> Pack.result('c),
           unser_d:Pack.input -> Pack.result('d),
           unser_e:Pack.input -> Pack.result('e),
           unser_f:Pack.input -> Pack.result('f)) : Pack.result(('a,'b,'c,'d,'e,'f)) =
      match unser_a(input) with
      | {success=(input, a)} ->
         match unser_b(input) with
         | {success=(input, b)} ->
           match unser_c(input) with
           | {success=(input, c)} ->
             match unser_d(input) with
             | {success=(input, d)} ->
               match unser_e(input) with
               | {success=(input, e)} ->
                 match unser_f(input) with
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

    unser7(input:Pack.input,
           unser_a:Pack.input -> Pack.result('a),
           unser_b:Pack.input -> Pack.result('b),
           unser_c:Pack.input -> Pack.result('c),
           unser_d:Pack.input -> Pack.result('d),
           unser_e:Pack.input -> Pack.result('e),
           unser_f:Pack.input -> Pack.result('f),
           unser_g:Pack.input -> Pack.result('g)) : Pack.result(('a,'b,'c,'d,'e,'f,'g)) =
      do pinput("unser7", input)
      match unser_a(input) with
      | {success=(input, a)} ->
         match unser_b(input) with
         | {success=(input, b)} ->
           match unser_c(input) with
           | {success=(input, c)} ->
             match unser_d(input) with
             | {success=(input, d)} ->
               match unser_e(input) with
               | {success=(input, e)} ->
                 match unser_f(input) with
                 | {success=(input, f)} ->
                   match unser_g(input) with
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

    unser(unser_a:Pack.input -> Pack.result('a), binary:binary, use_all_data:bool) : outcome('a,string) =
      input = {~binary; pos=0}
      do pinput("unser", input)
      match unser_a(input) with
      | {success=(input, a)} ->
         len = Binary.length(binary)
         if use_all_data && input.pos != len
         then
           unused = len - input.pos
           {failure="Pack.decode.unser: {unused} unused byte{if unused == 1 then "" else "s"} at end of data"}
         else {success=a}
      | {~failure} -> {~failure}

    unser_from_string(unser_a:Pack.input -> Pack.result('a), str:string, use_all_data:bool) : outcome('a,string) =
      unser(unser_a, binary_of_string(str), use_all_data)

  }}

  // TODO: pack_to_opa, opa_to_pack

}}

// End of file pack.opa


