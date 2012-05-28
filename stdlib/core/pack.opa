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
 / {Coded:list((Pack.u, Pack.data))}
 / {Lllist:(Pack.data,list(Pack.data))} // TODO: Blist, Slist, Llist

type Pack.data = list(Pack.u)

type Pack.input = { string:string; pos:int }

type Pack.result('a) = outcome((Pack.input,'a),string)

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
      then {failure="Pack.Encode.char: multi-byte char \"{c}\""}
      else {success=append(buf, c)}

    // octet
    byte(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x80 || i > 0x7f)
      then {failure="Pack.Encode.byte: out of range {i}"}
      else {success=append(buf, String.of_byte_unsafe(i))}
    ubyte(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xff)
      then {failure="Pack.Encode.byte: out of range {i}"}
      else {success=append(buf, String.of_byte_unsafe(i))}

    // short
    short_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x8000 || i > 0xffff)
      then {failure="Pack.Encode.short_be: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        {success=void}
    short_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x8000 || i > 0xffff)
      then {failure="Pack.Encode.short_le: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        {success=void}
    ushort_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffff)
      then {failure="Pack.Encode.ushort_be: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        {success=void}
    ushort_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffff)
      then {failure="Pack.Encode.ushort_le: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        {success=void}

    // long
    long_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x80000000 || i > 0x7fffffff)
      then {failure="Pack.Encode.long_be: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,24),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,16),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        {success=void}
    long_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < -0x80000000 || i > 0x7fffffff)
      then {failure="Pack.Encode.long_le: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,16),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,24),0xff)))
        {success=void}
    ulong_be(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffffffff)
      then {failure="Pack.Encode.ulong_be: out of range {i}"}
      else
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,24),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i,16),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(Bitwise.lsr(i, 8),0xff)))
        do append(buf, String.of_byte_unsafe(Bitwise.land(            i    ,0xff)))
        {success=void}
    ulong_le(buf:Pack.t, i:int) : outcome(void,string) =
      if (i < 0 || i > 0xffffffff)
      then {failure="Pack.Encode.ulong_le: out of range {i}"}
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
      then {failure="Pack.Encode.string_b: string too long for byte length \"{String.sub(0,30,str)^"..."}\""}
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
      then {failure="Pack.Encode.string_s_be: string too long for short length \"{String.sub(0,30,str)^"..."}\""}
      else
        match ushort_be(buf, len) with
        | {~failure} -> {~failure}
        | {success=_} ->
           do append(buf, str)
           {success=void}
    string_s_le(buf:Pack.t, str:string) : outcome(void,string) =
      len = String.length(str)
      if (len > 0xffff)
      then {failure="Pack.Encode.string_s_le: string too long for short length \"{String.sub(0,30,str)^"..."}\""}
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
      then {failure="Pack.Encode.string_l_be: string too long for long length \"{String.sub(0,30,str)^"..."}\""}
      else
        match ulong_be(buf, len) with
        | {~failure} -> {~failure}
        | {success=_} ->
           do append(buf, str)
           {success=void}
    string_l_le(buf:Pack.t, str:string) : outcome(void,string) =
      len = String.length(str)
      if (len > 0xffffffff)
      then {failure="Pack.Encode.string_l_le: string too long for long length \"{String.sub(0,30,str)^"..."}\""}
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

    // coded
    coded(buf:Pack.t, le:bool, signed:bool, code:Pack.u, data:Pack.data) : (bool, bool, outcome(void,string)) =
      match pack_u(buf, le, signed, code) with
      | (le, signed, {success=_}) -> pack_data(buf, le, signed, data)
      | failure -> failure

    same_u(u1:Pack.u, u2:Pack.u): bool =
      match (u1,u2) with
      | ({Be},{Be}) -> true
      | ({Le},{Le}) -> true
      | ({Signed},{Signed}) -> true
      | ({Unsigned},{Unsigned}) -> true
      | ({Char=_},{Char=_}) -> true
      | ({Byte=_},{Byte=_}) -> true
      | ({Short=_},{Short=_}) -> true
      | ({Long=_},{Long=_}) -> true
      | ({Longlong=_},{Longlong=_}) -> true
      | ({Pad},{Pad}) -> true
      | ({Padn=_},{Padn=_}) -> true
      | ({Void},{Void}) -> true
      | ({Bool=_},{Bool=_}) -> true
      | ({Cstring=_},{Cstring=_}) -> true
      | ({Bstring=_},{Bstring=_}) -> true
      | ({Sstring=_},{Sstring=_}) -> true
      | ({Lstring=_},{Lstring=_}) -> true
      | ({Llstring=_},{Llstring=_}) -> true
      | ({Float=_},{Float=_}) -> true
      | ({Coded=_},{Coded=_}) -> true
      | ({Lllist=_},{Lllist=_}) -> true
      | (_,_) -> false

    same_data(d1:Pack.data, d2:Pack.data): bool =
      match List.for_all2((u1, u2 -> same_u(u1,u2)),d1,d2) with
      | {result=tf} -> tf
      | _ -> false

    // list
    list(buf:Pack.t, le:bool, signed:bool, mklen:int -> Pack.u, typ:Pack.data, data:list(Pack.data))
         : (bool, bool, outcome(void,string)) =
      match pack_u(buf, le, signed, mklen(List.length(data))) with
      | (le, signed, {success=_}) ->
         rec aux(le, signed, l) =
           match l with
           | [] -> (le, signed, {success=void})
           | [data|t] ->
              if same_data(typ, data)
              then
                match pack_data(buf, le, signed, data) with
                | (le, signed, {success=_}) -> aux(le, signed, t)
                | failure -> failure
                end
              else (le, signed, {failure="Pack.Encode.list: non-matching list elements"})
           end
         aux(le, signed, data)
      | failure -> failure

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
      | {Coded=[]} -> 0 // Can't pack nothing
      | {Coded=[(code,data)]} -> packitemsize(code) + packlen(data)
      | {Coded=_} -> 0 // Will generate error
      | {Lllist=(_,l)} -> 8+List.fold((data, size -> size+packlen(data)),l,0)

    // predict pack length
    packlen(data:Pack.data) : int = List.fold((u, len -> len + packitemsize(u)), data, 0)

    // pack item into buffer
    pack_u(buf:Pack.t, le:bool, signed:bool, u:Pack.u) : (bool, bool, outcome(void,string)) =
      match u with
      | {Be} -> (false, signed, {success=void})
      | {Le} -> (true, signed, {success=void})
      | {Signed} -> (le, true, {success=void})
      | {Unsigned} -> (le, false, {success=void})
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
      | {~Sstring} -> (le, signed, if le then string_s_le(buf, Sstring) else string_s_be(buf, Sstring))
      | {~Lstring} -> (le, signed, if le then string_l_le(buf, Lstring) else string_l_be(buf, Lstring))
      | {~Llstring} -> (le, signed, if le then string_ll_le(buf, Llstring) else string_ll_be(buf, Llstring))
      | {~Float} -> (le, signed, if le then float_le(buf, Float) else float_be(buf, Float))
      | {Coded=[]} -> (le, signed, {failure="Pack.Encode.pack: Coded has no codes"})
      | {Coded=[(code,data)]} -> coded(buf, le, signed, code, data)
      | {Coded=_} -> (le, signed, {failure="Pack.Encode.pack: Coded has multiple codes"})
      | {Lllist=(t,l)} -> list(buf, le, signed, (len -> {Longlong=len}), t, l)

    // pack data into buffer
    pack_data(buf:Pack.t, le:bool, signed:bool, data:Pack.data) : (bool, bool, outcome(void,string)) =
      List.fold((u, (le, signed, res) ->
                 match res with
                 | {~failure} -> (le, signed, {~failure})
                 | {success=_} -> pack_u(buf, le, signed, u)
                ), data, (le, signed, {success:void}))

    // pack data into string
    pack(data:Pack.data) : outcome(string,string) =
      buf = create(packlen(data))
      (_, _, res) = pack_data(buf, true, true, data);
      match res with
      | {success=_} -> {success=contents(buf)}
      | {~failure} -> {~failure}

  }}

  // Decode
  Decode = {{

    // char
    char(data:string, pos:int) : outcome(string,string) =
      if String.length(data) >= pos + 1
      then {success=String.substring(pos, 1, data)}
      else {failure="Pack.Decode.char: not enough data for char"}

    // octet
    byte(data:string, pos:int) : outcome(int,string) =
      if String.length(data) >= pos + 1
      then {success=String.byte_at_unsafe(pos, data)}
      else {failure="Pack.Decode.byte: not enough data for byte"}

    // short
    short_be(data:string, pos:int) : outcome(int,string) =
      if String.length(data) >= pos + 2
      then {success=
        Bitwise.lsl(String.byte_at_unsafe(pos,   data),8)
      +             String.byte_at_unsafe(pos+1, data)}
      else {failure="Pack.Decode.short_be: not enough data for short"}
    short_le(data:string, pos:int) : outcome(int,string) =
      if String.length(data) >= pos + 2
      then {success=
                    String.byte_at_unsafe(pos,   data)
      + Bitwise.lsl(String.byte_at_unsafe(pos+1, data),8)}
      else {failure="Pack.Decode.short_le: not enough data for short"}

    // long
    long_be(data:string, pos:int) : outcome(int,string) =
      if String.length(data) >= pos + 4
      then {success=
        Bitwise.lsl(String.byte_at_unsafe(pos,   data),24)
      + Bitwise.lsl(String.byte_at_unsafe(pos+1, data),16)
      + Bitwise.lsl(String.byte_at_unsafe(pos+2, data),8)
      +             String.byte_at_unsafe(pos+3, data)}
      else {failure="Pack.Decode.long_le: not enough data for long"}
    long_le(data:string, pos:int) : outcome(int,string) =
      if String.length(data) >= pos + 4
      then {success=
                    String.byte_at_unsafe(pos,   data)
      + Bitwise.lsl(String.byte_at_unsafe(pos+1, data),8)
      + Bitwise.lsl(String.byte_at_unsafe(pos+2, data),16)
      + Bitwise.lsl(String.byte_at_unsafe(pos+3, data),24)}
      else {failure="Pack.Decode.long_le: not enough data for long"}

    // longlong
    longlong_be(data:string, pos:int) : outcome(int,string) =
      if String.length(data) >= pos + 8
      then {success=
        Bitwise.lsl(String.byte_at_unsafe(pos,   data),56)
      + Bitwise.lsl(String.byte_at_unsafe(pos+1, data),48)
      + Bitwise.lsl(String.byte_at_unsafe(pos+2, data),40)
      + Bitwise.lsl(String.byte_at_unsafe(pos+3, data),32)
      + Bitwise.lsl(String.byte_at_unsafe(pos+4, data),24)
      + Bitwise.lsl(String.byte_at_unsafe(pos+5, data),16)
      + Bitwise.lsl(String.byte_at_unsafe(pos+6, data),8)
      +             String.byte_at_unsafe(pos+7, data)}
      else {failure="Pack.Decode.longlong_be: not enough data for longlong"}
    longlong_le(data:string, pos:int) : outcome(int,string) =
      if String.length(data) >= pos + 8
      then {success=
                    String.byte_at_unsafe(pos,   data)
      + Bitwise.lsl(String.byte_at_unsafe(pos+1, data),8)
      + Bitwise.lsl(String.byte_at_unsafe(pos+2, data),16)
      + Bitwise.lsl(String.byte_at_unsafe(pos+3, data),24)
      + Bitwise.lsl(String.byte_at_unsafe(pos+4, data),32)
      + Bitwise.lsl(String.byte_at_unsafe(pos+5, data),40)
      + Bitwise.lsl(String.byte_at_unsafe(pos+6, data),48)
      + Bitwise.lsl(String.byte_at_unsafe(pos+7, data),56)}
      else {failure="Pack.Decode.longlong_be: not enough data for longlong"}

    // int64 - Not yet, need Javascript Long module
    //int64_be(data:string, pos:int) : int64 =
    //  Int64.unembed_be(String.substring(pos, 8, data))
    //int64_le(data:string, pos:int) : int64 =
    //  Int64.unembed_le(String.substring(pos, 8, data))

    // pad
    pad(data:string, pos:int) : outcome(void,string) =
      if String.length(data) >= pos + 1
      then {success=void}
      else {failure="Pack.Decode.pad: not enough data for padding"}

    // padn
    padn(data:string, pos:int, n:int) : outcome(void,string) =
      if String.length(data) >= pos + n
      then {success=void}
      else {failure="Pack.Decode.padn: not enough data for padding"}

    // bool
    bool(data:string, pos:int) : outcome(bool,string) =
      if String.length(data) >= pos + 1
      then {success=String.byte_at_unsafe(pos, data) != 0}
      else {failure="Pack.Decode.bool: not enough data for bool"}

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
        else {failure="Pack.Decode.cstring: not enough data for string"}
      | {none} ->
        {failure="Pack.Decode.cstring: can't find null"}

    //bytestr
    string_b(data:string, pos:int) : outcome(string,string) =
      match byte(data, pos) with
      | {success=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos + 1, len, data)}
        else {failure="Pack.Decode.string_b: not enough data for string"}
      | {~failure} -> {~failure}

    //shortstr
    string_s_be(data:string, pos:int) : outcome(string,string) =
      match short_be(data, pos) with
      | {success=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos + 2, len, data)}
        else {failure="Pack.Decode.string_s_be: not enough data for string"}
      | {~failure} -> {~failure}
    string_s_le(data:string, pos:int) : outcome(string,string) =
      match short_le(data, pos) with
      | {success=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos + 2, len, data)}
        else {failure="Pack.Decode.string_s_le: not enough data for string"}
      | {~failure} -> {~failure}

    //longstr
    string_l_be(data:string, pos:int) : outcome(string,string) =
      match long_be(data, pos) with
      | {success=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos + 4, len, data)}
        else {failure="Pack.Decode.string_l_be: not enough data for string"}
      | {~failure} -> {~failure}
    string_l_le(data:string, pos:int) : outcome(string,string) =
      match long_le(data, pos) with
      | {success=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos + 4, len, data)}
        else {failure="Pack.Decode.string_l_le: not enough data for string"}
      | {~failure} -> {~failure}

    //longlongstr
    string_ll_be(data:string, pos:int) : outcome(string,string) =
      match longlong_be(data, pos) with
      | {success=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos + 8, len, data)}
        else {failure="Pack.Decode.string_ll_be: not enough data for string"}
      | {~failure} -> {~failure}
    string_ll_le(data:string, pos:int) : outcome(string,string) =
      match longlong_le(data, pos) with
      | {success=len} ->
        if String.length(data) > pos + len
        then {success=String.substring(pos + 8, len, data)}
        else {failure="Pack.Decode.string_ll_le: not enough data for string"}
      | {~failure} -> {~failure}

    // float
    unembed_le = %% BslNumber.Float.unembed_le %% : string, int -> float
    unembed_be = %% BslNumber.Float.unembed_be %% : string, int -> float

    float_le(data:string, pos:int) : outcome(float,string) =
      if String.length(data) >= pos + 8
      then {success=unembed_le(data, pos)}
      else {failure="Pack.Decode.float_le: not enough data for float"}
    float_be(data:string, pos:int) : outcome(float,string) =
      if String.length(data) >= pos + 8
      then {success=unembed_be(data, pos)}
      else {failure="Pack.Decode.float_be: not enough data for float"}

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
                       | {Coded=[]} -> {failure="Pack.Decode.unpack: Coded has no codes"}
                       | {Coded=[(code1,data1)|codes]} ->
                          (match unpack([code1], str, pos) with
                           | {success=(npos,[code])} ->
                             (match List.assoc(code, [(code1,data1)|codes]) with
                              | {some=cdata} ->
                                 (match unpack(cdata, str, npos) with
                                  | {success=(npos,ndata)} -> {success=(le, signed, npos, [{Coded=[(code,ndata)]}|data])}
                                  | {~failure} -> {failure="Pack.Decode.unpack: Coded has bad data {failure}"})
                              | {none} -> {failure="Pack.Decode.unpack: Coded has missing code {code}"})
                           | {success=_} -> {failure="Pack.Decode.unpack: Coded has multiple decodings"}
                           | {~failure} -> {failure="Pack.Decode.unpack: Coded has bad code {failure}"})
                       | {Lllist=(d,_)} ->
                          (match unpack([{Longlong=0}], str, pos) with
                           | {success=(pos,[{Longlong=len}])} ->
                              rec aux(i, l, pos) =
                                if i == len
                                then {success=(le, signed, pos, [{Lllist=(d,List.rev(l))}|data])}
                                else
                                  match unpack(d, str, pos) with
                                  | {success=(npos,ndata)} -> aux(i+1, [ndata|l], npos)
                                  | {~failure} -> {~failure}
                                  end
                              aux(0, [], pos)
                           | {success=_} -> {failure="Pack.Decode.unpack: Lllist has missing/multiple lengths"}
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
      | {Coded=_} -> true
      | {Lllist=_} -> true

    // cleanup elements with no data
    clean(data:Pack.data) = List.filter(has_data, data)

    // The unser functions provide a compositional functional decode over the Pack.input type
    // TODO: complete this list of functions

    unser_string(input:Pack.input) : Pack.result(string) =
      match string_ll_le(input.string, input.pos) with
      | {success=s} -> {success=({input with pos=input.pos+8+String.length(s)},s)}
      | {~failure} -> {~failure}

    unser_bool(input:Pack.input) : Pack.result(bool) =
      match bool(input.string, input.pos) with
      | {success=b} -> {success=({input with pos=input.pos+1},b)}
      | {~failure} -> {~failure}

    unser_ref(unser_a:Pack.input -> Pack.result('a), input:Pack.input)
              : Pack.result(Server.reference('a)) =
      match unser_a(input) with
      | {success=(input,a)} -> {success=(input,ServerReference.create(a))}
      | {~failure} -> {~failure}

    unser_option(unser_a:Pack.input -> Pack.result('a), input:Pack.input): Pack.result(option('a)) =
      match byte(input.string, input.pos) with
      | {success=0} ->
         match unser_a({input with pos=input.pos+1}) with
         | {success=(input, a)} -> {success=(input,{some=a})}
         | {~failure} -> {~failure}
         end
      | {success=1} -> {success=({input with pos=input.pos+1},{none})}
      | {success=n} -> {failure="Pack.unser_option: bad option code {n}"}
      | {~failure} -> {~failure}

    unser_array(f:Pack.input -> Pack.result('a), def:'a, input:Pack.input) : Pack.result(llarray('a)) =
      match longlong_le(input.string, input.pos) with
      | {success=len} ->
         a = LowLevelArray.create(len, def)
         rec aux(input, i) =
           if i == len
           then {success=(input,a)}
           else
             match f(input) with
             | {success=(input, v:'a)} ->
                _ = LowLevelArray.set(a, i, v)
                aux(input, i+1)
             | {~failure} -> {~failure}
             end
         aux(input, 0)
      | {~failure} -> {~failure}

    unser4(input:Pack.input,
           unser_a:Pack.input -> Pack.result('a),
           unser_b:Pack.input -> Pack.result('b),
           unser_c:Pack.input -> Pack.result('c),
           unser_d:Pack.input -> Pack.result('d)) : Pack.result(('a,'b,'c,'d)) =
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

    unser7(input:Pack.input,
           unser_a:Pack.input -> Pack.result('a),
           unser_b:Pack.input -> Pack.result('b),
           unser_c:Pack.input -> Pack.result('c),
           unser_d:Pack.input -> Pack.result('d),
           unser_e:Pack.input -> Pack.result('e),
           unser_f:Pack.input -> Pack.result('f),
           unser_g:Pack.input -> Pack.result('g)) : Pack.result(('a,'b,'c,'d,'e,'f,'g)) =
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

  }}

}}

// End of file pack.opa

