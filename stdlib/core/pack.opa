/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
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

// TODO: find a better buffer module
@private PackBuffer = {{
  create   = %% BslBuffer.create %%
  append   = %% BslBuffer.append %%
  contents = %% BslBuffer.contents %%
}}

type Pack.t = Buffer_private.buffer

// TODO: multibyte characters

type Pack.u =
   {Be}
 / {Le}
 / {Signed}
 / {Unsigned}
 / {Char:string}
 / {Byte:int}
 / {Short:int}
 / {Long:int}
 / {Longlong:int}
 / {Pad}
 / {Padn:int}
 / {Void}
 / {Bool:bool}
 / {Cstring:string}
 / {Bstring:string}
 / {Sstring:string}
 / {Lstring:string}
 / {Llstring:string}
 / {Float:float}

type Pack.data = list(Pack.u)

Pack = {{

  // Re-export underlying module

  create = PackBuffer.create
  append = PackBuffer.append
  contents = PackBuffer.contents

  @server_private null = String.of_byte_unsafe(0)

  // Encode
  Encode = {{

    // char
    char(buf:Pack.t, c:string) : outcome(void,string) =
      if (String.length(c) != 1)
      then {failure="Pack.char: multi-byte char \"{c}\""}
      else {success=append(buf, c)}

    // octet
    byte(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x80 || i > 0x7f)
      then {failure="Pack.byte: out of range {i}"}
      else {success=append(buf, String.of_byte_unsafe(i))}
    ubyte(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x80 || i > 0x7f)
      then {failure="Pack.byte: out of range {i}"}
      else {success=append(buf, String.of_byte_unsafe(i))}

    // short
    short_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x8000 || i > 0xffff)
      then {failure="Pack.short_be: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        {success=void}
    short_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x8000 || i > 0xffff)
      then {failure="Pack.short_le: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        {success=void}
    ushort_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffff)
      then {failure="Pack.ushort_be: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        {success=void}
    ushort_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffff)
      then {failure="Pack.ushort_le: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        {success=void}

    // long
    long_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x80000000 || i > 0x7fffffff)
      then {failure="Pack.long_be: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,24),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,16),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        {success=void}
    long_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x80000000 || i > 0x7fffffff)
      then {failure="Pack.long_le: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,16),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,24),0xff)))
        {success=void}
    ulong_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffffffff)
      then {failure="Pack.ulong_be: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,24),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,16),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        {success=void}
    ulong_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffffffff)
      then {failure="Pack.ulong_le: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,16),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,24),0xff)))
        {success=void}

    // longlong - note, 63 bits!!!
    longlong_be(buf:Pack.t, i:int) : outcome(void,string) =
      do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,56),0xff)))
      do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,48),0xff)))
      do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,40),0xff)))
      do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,32),0xff)))
      do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,24),0xff)))
      do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,16),0xff)))
      do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
      do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
      {success=void}
    longlong_le(buf:Pack.t, i:int) : outcome(void,string) =
      do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
      do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
      do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,16),0xff)))
      do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,24),0xff)))
      do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,32),0xff)))
      do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,40),0xff)))
      do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,48),0xff)))
      do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,56),0xff)))
      {success=void}

    // int64 - snookered by javascript, we need a Long module
    //int64_be(buf:Pack.t, i:int64) : outcome(void,string) =
    //  append(buf, Int64.embed_be(i))
    //int64_le(buf:Pack.t, i:int64) : outcome(void,string) =
    //  append(buf, Int64.embed_le(i))

    // pad
    pad(buf:Pack.t) : outcome(void,string) =
      byte(buf, 0)

    // padn
    padn(buf:Pack.t, n:int) : outcome(void,string) =
      do append(buf, String.repeat(n, null))
      {success=void}

    // bool
    bool(buf:Pack.t, b:bool) : outcome(void,string) =
      byte(buf, if b then 1 else 0)

    // cstring
    cstring(buf:Pack.t, str:string) : outcome(void,string) =
      do append(buf, str)
      byte(buf, 0)

    // bytestr
    string_b(buf:Pack.t, str:string) : outcome(void,string) =
      len = String.length(str)
      if (len > 0xff)
      then {failure="Pack.string_b: string too long for byte length \"{String.sub(0,30,str)^"..."}\""}
      else
        match ubyte(buf, len) with
        | {~failure} -> {~failure}
        | {success=_} ->
           do append(buf, str)
           {success=void}

    // shortstr
    string_s_be(buf:Pack.t, str:string) : outcome(void,string) =
      len = String.length(str)
      if (len > 0xffff)
      then {failure="Pack.string_s_be: string too long for short length \"{String.sub(0,30,str)^"..."}\""}
      else
        match ushort_be(buf, len) with
        | {~failure} -> {~failure}
        | {success=_} ->
           do append(buf, str)
           {success=void}
    string_s_le(buf:Pack.t, str:string) : outcome(void,string) =
      len = String.length(str)
      if (len > 0xffff)
      then {failure="Pack.string_s_le: string too long for short length \"{String.sub(0,30,str)^"..."}\""}
      else
        match ushort_le(buf, len) with
        | {~failure} -> {~failure}
        | {success=_} ->
           do append(buf, str)
           {success=void}

    // longstr
    string_l_be(buf:Pack.t, str:string) : outcome(void,string) =
      len = String.length(str)
      if (len > 0xffffffff)
      then {failure="Pack.string_l_be: string too long for long length \"{String.sub(0,30,str)^"..."}\""}
      else
        match ulong_be(buf, len) with
        | {~failure} -> {~failure}
        | {success=_} ->
           do append(buf, str)
           {success=void}
    string_l_le(buf:Pack.t, str:string) : outcome(void,string) =
      len = String.length(str)
      if (len > 0xffffffff)
      then {failure="Pack.string_l_le: string too long for long length \"{String.sub(0,30,str)^"..."}\""}
      else
        match ulong_le(buf, len) with
        | {~failure} -> {~failure}
        | {success=_} ->
           do append(buf, str)
           {success=void}

    // longlongstr
    string_ll_be(buf:Pack.t, str:string) : outcome(void,string) =
      len = String.length(str)
      match longlong_be(buf, len) with
      | {~failure} -> {~failure}
      | {success=_} ->
         do append(buf, str)
         {success=void}
    string_ll_le(buf:Pack.t, str:string) : outcome(void,string) =
      len = String.length(str)
      match longlong_le(buf, len) with
      | {~failure} -> {~failure}
      | {success=_} ->
         do append(buf, str)
         {success=void}

    // float
    embed_le = %% BslNumber.Float.embed_le %% : float -> string
    embed_be = %% BslNumber.Float.embed_be %% : float -> string

    float_be(buf:Pack.t, f:float) : outcome(void,string) =
      do append(buf, embed_be(f))
      {success=void}
    float_le(buf:Pack.t, f:float) : outcome(void,string) =
      do append(buf, embed_le(f))
      {success=void}

    // pack item length
    packitemsize(u:Pack.u) : int =
      match u with
      | {Be} -> 0
      | {Le} -> 0
      | {Signed} -> 0
      | {Unsigned} -> 0
      | {Char=_} -> 1
      | {Byte=_} -> 1
      | {Short=_} -> 2
      | {Long=_} -> 4
      | {Longlong=_} -> 8
      | {Pad} -> 1
      | {~Padn} -> Padn
      | {Void} -> 1
      | {Bool=_} -> 1
      | {~Cstring} -> String.length(Cstring)+1
      | {~Bstring} -> 1+String.length(Bstring)
      | {~Sstring} -> 2+String.length(Sstring)
      | {~Lstring} -> 4+String.length(Lstring)
      | {~Llstring} -> 8+String.length(Llstring)
      | {Float=_} -> 8

    // predict pack length
    packlen(data:Pack.data) : int = List.fold((u, len -> len + packitemsize(u)), data, 0)

    // pack data into string
    pack(data:Pack.data) : outcome(string,string) =
      buf = create(packlen(data))
      (_, _, res) =
        List.fold((u, (le, signed, res) ->
                    match res with
                    | {~failure} -> (le, signed, {~failure})
                    | {success=_} ->
                       (match u with
                        | {Be} -> (false, signed, res)
                        | {Le} -> (true, signed, res)
                        | {Signed} -> (le, true, res)
                        | {Unsigned} -> (le, false, res)
                        | {~Char} -> (le, signed, char(buf, Char))
                        | {~Byte} -> (le, signed, if signed then byte(buf, Byte) else ubyte(buf, Byte))
                        | {~Short} ->
                           (le, signed, match (le, signed) with
                                        | ({false},{false}) -> ushort_be(buf, Short)
                                        | ({true},{false}) -> ushort_le(buf, Short)
                                        | ({false},{true}) -> short_be(buf, Short)
                                        | ({true},{true}) -> short_le(buf, Short))
                        | {~Long} ->
                           (le, signed, match (le, signed) with
                                        | ({false},{false}) -> ulong_be(buf, Long)
                                        | ({true},{false}) -> ulong_le(buf, Long)
                                        | ({false},{true}) -> long_be(buf, Long)
                                        | ({true},{true}) -> long_le(buf, Long))
                        | {~Longlong} ->
                           (le, signed, if le then longlong_le(buf, Longlong) else longlong_be(buf, Longlong))
                        | {Pad} -> (le, signed, pad(buf))
                        | {~Padn} -> (le, signed, padn(buf, Padn))
                        | {Void} -> (le, signed, pad(buf))
                        | {~Bool} -> (le, signed, bool(buf, Bool))
                        | {~Cstring} -> (le, signed, cstring(buf, Cstring))
                        | {~Bstring} -> (le, signed, string_b(buf, Bstring))
                        | {~Sstring} ->
                           (le, signed, if le then string_s_le(buf, Sstring) else string_s_be(buf, Sstring))
                        | {~Lstring} ->
                           (le, signed, if le then string_l_le(buf, Lstring) else string_l_be(buf, Lstring))
                        | {~Llstring} ->
                           (le, signed, if le then string_ll_le(buf, Llstring) else string_ll_be(buf, Llstring))
                        | {~Float} ->
                           (le, signed, if le then float_le(buf, Float) else float_be(buf, Float))
                       )), data, (true, true, {success:void}))
      match res with
      | {success=_} -> {success=contents(buf)}
      | {~failure} -> {~failure}

  }}

  // Decode
  Decode = {{

    // char
    char(data:string, pos:int) : outcome(string,string) =
      if String.length(data) > pos + 1
      then {success=String.substring(pos, 1, data)}
      else {failure="Pack.char: not enough data for char"}

    // octet
    byte(data:string, pos:int) : outcome(int,string) =
      if String.length(data) > pos + 1
      then {success=String.byte_at_unsafe(pos, data)}
      else {failure="Pack.byte: not enough data for byte"}

    // short
    short_be(data:string, pos:int) : outcome(int,string) =
      if String.length(data) > pos + 2
      then {success=
        Bitwise.lsl(String.byte_at_unsafe(pos,   data),8)
      +             String.byte_at_unsafe(pos+1, data)}
      else {failure="Pack.short_be: not enough data for short"}
    short_le(data:string, pos:int) : outcome(int,string) =
      if String.length(data) > pos + 2
      then {success=
                    String.byte_at_unsafe(pos,   data)
      + Bitwise.lsl(String.byte_at_unsafe(pos+1, data),8)}
      else {failure="Pack.short_le: not enough data for short"}

    // long
    long_be(data:string, pos:int) : outcome(int,string) =
      if String.length(data) > pos + 4
      then {success=
        Bitwise.lsl(String.byte_at_unsafe(pos,   data),24)
      + Bitwise.lsl(String.byte_at_unsafe(pos+1, data),16)
      + Bitwise.lsl(String.byte_at_unsafe(pos+2, data),8)
      +             String.byte_at_unsafe(pos+3, data)}
      else {failure="Pack.long_le: not enough data for long"}
    long_le(data:string, pos:int) : outcome(int,string) =
      if String.length(data) > pos + 4
      then {success=
                    String.byte_at_unsafe(pos,   data)
      + Bitwise.lsl(String.byte_at_unsafe(pos+1, data),8)
      + Bitwise.lsl(String.byte_at_unsafe(pos+2, data),16)
      + Bitwise.lsl(String.byte_at_unsafe(pos+3, data),24)}
      else {failure="Pack.long_le: not enough data for long"}

    // longlong
    longlong_be(data:string, pos:int) : outcome(int,string) =
      if String.length(data) > pos + 8
      then {success=
        Bitwise.lsl(String.byte_at_unsafe(pos,   data),56)
      + Bitwise.lsl(String.byte_at_unsafe(pos+1, data),48)
      + Bitwise.lsl(String.byte_at_unsafe(pos+2, data),40)
      + Bitwise.lsl(String.byte_at_unsafe(pos+3, data),32)
      + Bitwise.lsl(String.byte_at_unsafe(pos+4, data),24)
      + Bitwise.lsl(String.byte_at_unsafe(pos+5, data),16)
      + Bitwise.lsl(String.byte_at_unsafe(pos+6, data),8)
      +             String.byte_at_unsafe(pos+7, data)}
      else {failure="Pack.longlong_be: not enough data for longlong"}
    longlong_le(data:string, pos:int) : outcome(int,string) =
      if String.length(data) > pos + 8
      then {success=
                    String.byte_at_unsafe(pos,   data)
      + Bitwise.lsl(String.byte_at_unsafe(pos+1, data),8)
      + Bitwise.lsl(String.byte_at_unsafe(pos+2, data),16)
      + Bitwise.lsl(String.byte_at_unsafe(pos+3, data),24)
      + Bitwise.lsl(String.byte_at_unsafe(pos+4, data),32)
      + Bitwise.lsl(String.byte_at_unsafe(pos+5, data),40)
      + Bitwise.lsl(String.byte_at_unsafe(pos+6, data),48)
      + Bitwise.lsl(String.byte_at_unsafe(pos+7, data),56)}
      else {failure="Pack.longlong_be: not enough data for longlong"}

    // int64 - Not yet, need Javascript Long module
    //int64_be(data:string, pos:int) : int64 =
    //  Int64.unembed_be(String.substring(pos, 8, data))
    //int64_le(data:string, pos:int) : int64 =
    //  Int64.unembed_le(String.substring(pos, 8, data))

    // pad
    pad(data:string, pos:int) : outcome(void,string) =
      if String.length(data) > pos + 1
      then {success=void}
      else {failure="Pack.pad: not enough data for padding"}

    // padn
    padn(data:string, pos:int, n:int) : outcome(void,string) =
      if String.length(data) > pos + n
      then {success=void}
      else {failure="Pack.padn: not enough data for padding"}

    // bool
    bool(data:string, pos:int) : outcome(bool,string) =
      if String.length(data) > pos + 1
      then {success=String.byte_at_unsafe(pos, data) != 0}
      else {failure="Pack.bool: not enough data for bool"}

    @private clen(data:string, pos:int) : option(int) =
      dlen = String.length(data) - pos
      rec aux(i) =
        if i > dlen
        then none
        else
          // Faster with String.index?
          if String.substring(pos + i, 1, data) == null
          then {some=i}
          else aux(i+1)
      aux(0)

    // cstring
    cstring(data:string, pos:int) : outcome(string,string) =
      match clen(data, pos) with
      | {some=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos, len, data)}
        else {failure="Pack.cstring: not enough data for string"}
      | {none} ->
        {failure="Pack.cstring: can't find null"}

    //bytestr
    string_b(data:string, pos:int) : outcome(string,string) =
      match byte(data, pos) with
      | {success=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos + 1, len, data)}
        else {failure="Pack.string_b: not enough data for string"}
      | {~failure} -> {~failure}

    //shortstr
    string_s_be(data:string, pos:int) : outcome(string,string) =
      match short_be(data, pos) with
      | {success=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos + 2, len, data)}
        else {failure="Pack.string_s_be: not enough data for string"}
      | {~failure} -> {~failure}
    string_s_le(data:string, pos:int) : outcome(string,string) =
      match short_le(data, pos) with
      | {success=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos + 2, len, data)}
        else {failure="Pack.string_s_le: not enough data for string"}
      | {~failure} -> {~failure}

    //longstr
    string_l_be(data:string, pos:int) : outcome(string,string) =
      match long_be(data, pos) with
      | {success=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos + 4, len, data)}
        else {failure="Pack.string_l_be: not enough data for string"}
      | {~failure} -> {~failure}
    string_l_le(data:string, pos:int) : outcome(string,string) =
      match long_le(data, pos) with
      | {success=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos + 4, len, data)}
        else {failure="Pack.string_l_le: not enough data for string"}
      | {~failure} -> {~failure}

    //longlongstr
    string_ll_be(data:string, pos:int) : outcome(string,string) =
      match longlong_be(data, pos) with
      | {success=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos + 8, len, data)}
        else {failure="Pack.string_ll_be: not enough data for string"}
      | {~failure} -> {~failure}
    string_ll_le(data:string, pos:int) : outcome(string,string) =
      match longlong_le(data, pos) with
      | {success=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos + 8, len, data)}
        else {failure="Pack.string_ll_le: not enough data for string"}
      | {~failure} -> {~failure}

    // float
    unembed_le = %% BslNumber.Float.unembed_le %% : string, int -> float
    unembed_be = %% BslNumber.Float.unembed_be %% : string, int -> float

    float_le(data:string, pos:int) : outcome(float,string) =
      if String.length(data) > pos + 8
      then {success=unembed_le(data, pos)}
      else {failure="Pack.float_le: not enough data for float"}
    float_be(data:string, pos:int) : outcome(float,string) =
      if String.length(data) > pos + 8
      then {success=unembed_be(data, pos)}
      else {failure="Pack.float_be: not enough data for float"}

    // unpack data
    unpack(data:Pack.data, str:string, pos:int) : outcome((int,Pack.data),string) =
      match
        List.fold((u:Pack.u, acc ->
                    match acc with
                    | {~failure} -> {~failure}
                    | {success=(le, signed, pos, data)} ->
                      (match u:Pack.u with
                       | {Be} -> {success=(false, signed, pos, [{Be}|data])}
                       | {Le} -> {success=(true, signed, pos, [{Le}|data])}
                       | {Signed} -> {success=(le, true, pos, [{Signed}|data])}
                       | {Unsigned} -> {success=(le, false, pos, [{Unsigned}|data])}
                       | {Char=_} ->
                          (match char(str, pos) with
                           | {success=Char} -> {success=(le, signed, pos+1, [{~Char}|data])}
                           | {~failure} -> {~failure})
                       | {Byte=_} ->
                          (match byte(str, pos) with
                           | {success=Byte} -> {success=(le, signed, pos+1, [{~Byte}|data])}
                           | {~failure} -> {~failure})
                       | {Short=_} ->
                          (match if le then short_le(str, pos) else short_be(str, pos) with
                           | {success=Short} -> {success=(le, signed, pos+2, [{~Short}|data])}
                           | {~failure} -> {~failure})
                       | {Long=_} ->
                          (match if le then long_le(str, pos) else long_be(str, pos) with
                           | {success=Long} -> {success=(le, signed, pos+4, [{~Long}|data])}
                           | {~failure} -> {~failure})
                       | {Longlong=_} ->
                          (match if le then longlong_le(str, pos) else longlong_be(str, pos) with
                           | {success=Longlong} -> {success=(le, signed, pos+8, [{~Longlong}|data])}
                           | {~failure} -> {~failure})
                       | {Pad} ->
                          (match pad(str, pos) with
                           | {success=_} -> {success=(le, signed, pos+1, [{Pad}|data])}
                           | {~failure} -> {~failure})
                       | {~Padn} ->
                          (match padn(str, pos, Padn) with
                           | {success=_} -> {success=(le, signed, pos+Padn, [{~Padn}|data])}
                           | {~failure} -> {~failure})
                       | {Void} ->
                          (match pad(str, pos) with
                           | {success=_} -> {success=(le, signed, pos+1, [{Void}|data])}
                           | {~failure} -> {~failure})
                       | {Bool=_} ->
                          (match bool(str, pos) with
                           | {success=Bool} -> {success=(le, signed, pos+1, [{~Bool}|data])}
                           | {~failure} -> {~failure})
                       | {Cstring=_} ->
                          (match cstring(str, pos) with
                           | {success=Cstring} -> {success=(le, signed, pos+String.length(Cstring)+1, [{~Cstring}|data])}
                           | {~failure} -> {~failure})
                       | {Bstring=_} ->
                          (match string_b(str, pos) with
                           | {success=Bstring} -> {success=(le, signed, pos+String.length(Bstring)+1, [{~Bstring}|data])}
                           | {~failure} -> {~failure})
                       | {Sstring=_} ->
                          (match if le then string_s_le(str, pos) else string_s_be(str, pos) with
                           | {success=Sstring} -> {success=(le, signed, pos+String.length(Sstring)+2, [{~Sstring}|data])}
                           | {~failure} -> {~failure})
                       | {Lstring=_} ->
                          (match if le then string_l_le(str, pos) else string_l_be(str, pos) with
                           | {success=Lstring} -> {success=(le, signed, pos+String.length(Lstring)+4, [{~Lstring}|data])}
                           | {~failure} -> {~failure})
                       | {Llstring=_} ->
                          (match if le then string_ll_le(str, pos) else string_ll_be(str, pos) with
                           | {success=Llstring} -> {success=(le, signed, pos+String.length(Llstring)+8, [{~Llstring}|data])}
                           | {~failure} -> {~failure})
                       | {Float=_} ->
                          (match if le then float_le(str, pos) else float_be(str, pos) with
                           | {success=Float} -> {success=(le, signed, pos+8, [{~Float}|data])}
                           | {~failure} -> {~failure})
                       )), data, {success=(true, true, pos, [])})
      with
      | {success=(_,_,pos,data)} -> {success=(pos,List.rev(data))}
      | {~failure} -> {~failure}

    // meaningful entities
    has_data(u:Pack.u) : bool =
      match u with
      | {Be} -> false
      | {Le} -> false
      | {Signed} -> false
      | {Unsigned} -> false
      | {Char=_} -> true
      | {Byte=_} -> true
      | {Short=_} -> true
      | {Long=_} -> true
      | {Longlong=_} -> true
      | {Pad} -> false
      | {Padn=_} -> false
      | {Void} -> true
      | {Bool=_} -> true
      | {Cstring=_} -> true
      | {Bstring=_} -> true
      | {Sstring=_} -> true
      | {Lstring=_} -> true
      | {Llstring=_} -> true
      | {Float=_} -> true

    // cleanup elements with no data
    clean(data:Pack.data) = List.filter(has_data, data)

  }}

}}

// End of file pack.opa

