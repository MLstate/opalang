##property[mli]
##extern-type binary = Buf.t
##property[endmli]

##register create\ `Buf.create`: int -> binary

##register length\ `Buf.length`: binary -> int

##register binary_of_string : string -> binary
let binary_of_string s =
  let b = Buf.create (String.length s) in
  Buf.add_string b s;
  b

##register binary_of_string8 : string -> binary
let binary_of_string8 s =
  let b = Buf.create (String.length s) in
  Buf.add_string b s;
  b

##register string_of_binary\ `Buf.contents`: binary -> string

##register string_of_binary8\ `Buf.contents`: binary -> string

##register resize : binary, int -> void
let resize b size =
  (* There's no way of changing the hint size for OCaml buffers *)
  let len = Buf.length b in
  if size <> len
  then
    let s = if size < len then Buf.sub b 0 size else Buf.contents b in
    Buf.reset b;
    Buf.add_string b s

##register clear\ `Buf.clear`: binary -> void

##register trim: binary -> void
let trim b =
  let s = Buf.contents b in
  let b = Buf.create (String.length s) in
  Buf.add_string b s

##register reset\ `Buf.reset`: binary -> void

##register add_fill : binary, int, int -> void
let add_fill b len v =
  for i = 0 to len-1 do
    Buf.add_char b (Char.chr v)
  done

##register add_string\ `Buf.add_string`: binary, string -> void

##register add_binary : binary, binary -> void
let add_binary b nb =
  Buf.add_string b (Buf.contents nb)

exception BslBinaryError of string
let error str = raise (BslBinaryError str)

##register add_int8 : binary, int -> void
let add_int8 b i =
  (* if (i < -0x80 || i > 0x7f) then error (Printf.sprintf "BslBinary.add_int8: out of range int %d" i); *)
  Buf.add_char b (Char.chr (if i < 0 then i+0x100 else i))

##register add_uint8 : binary, int -> void
let add_uint8 b i =
  (* if (i < 0 || i > 0xff) then error (Printf.sprintf "BslBinary.add_uint8: out of range int %d" i); *)
  Buf.add_char b (Char.chr i)

##register add_int16_le : binary, int -> void
let add_int16_le b i =
  (* if (i < -0x8000 || i > 0x7fff) then error (Printf.sprintf "BslBinary.add_int16_le: out of range int %d" i); *)
  let i = if i < 0 then 0x10000 + i else i in
  Buf.add_char b (Char.chr ( i         land 0xff));
  Buf.add_char b (Char.chr ((i lsr 8 ) land 0xff))

##register add_int16_be : binary, int -> void
let add_int16_be b i =
  (* if (i < -0x8000 || i > 0x7fff) then error (Printf.sprintf "BslBinary.add_int16_be: out of range int %d" i); *)
  let i = if i < 0 then 0x10000 + i else i in
  Buf.add_char b (Char.chr ((i lsr 8 ) land 0xff));
  Buf.add_char b (Char.chr ( i         land 0xff))

##register add_uint16_le : binary, int -> void
let add_uint16_le b i =
  (* if (i < 0 || i > 0xffff) then error (Printf.sprintf "BslBinary.add_uint16_le: out of range int %d" i); *)
  Buf.add_char b (Char.chr ( i         land 0xff));
  Buf.add_char b (Char.chr ((i lsr 8 ) land 0xff))

##register add_uint16_be : binary, int -> void
let add_uint16_be b i =
  (* if (i < 0 || i > 0xffff) then error (Printf.sprintf "BslBinary.add_uint16_be: out of range int %d" i); *)
  Buf.add_char b (Char.chr ((i lsr 8 ) land 0xff));
  Buf.add_char b (Char.chr ( i         land 0xff))

let _0x100000000 =
  #<Ifstatic:OCAML_WORD_SIZE 64>
    0x100000000
  #<Else>
    0
  #<End>

##register add_int32_le : binary, int -> void
let add_int32_le b i =
  (* if (i < -0x80000000 || i > 0x7fffffff) then error (Printf.sprintf "BslBinary.add_int32_le: out of range int %d" i); *)
  let i = if i < 0 then _0x100000000 + i else i in
  Buf.add_char b (Char.chr ( i         land 0xff));
  Buf.add_char b (Char.chr ((i lsr 8 ) land 0xff));
  Buf.add_char b (Char.chr ((i lsr 16) land 0xff));
  Buf.add_char b (Char.chr ((i lsr 24) land 0xff))

##register add_int32_be : binary, int -> void
let add_int32_be b i =
  (* if (i < -0x80000000 || i > 0x7fffffff) then error (Printf.sprintf "BslBinary.add_int32_be: out of range int %d" i); *)
  let i = if i < 0 then _0x100000000 + i else i in
  Buf.add_char b (Char.chr ((i lsr 24) land 0xff));
  Buf.add_char b (Char.chr ((i lsr 16) land 0xff));
  Buf.add_char b (Char.chr ((i lsr 8 ) land 0xff));
  Buf.add_char b (Char.chr ( i         land 0xff))

##register add_uint32_le : binary, int -> void
let add_uint32_le b i =
  (* if (i < 0 || i > 0xffffffff) then error (Printf.sprintf "BslBinary.add_uint32_le: out of range int %d" i); *)
  Buf.add_char b (Char.chr ( i         land 0xff));
  Buf.add_char b (Char.chr ((i lsr 8 ) land 0xff));
  Buf.add_char b (Char.chr ((i lsr 16) land 0xff));
  Buf.add_char b (Char.chr ((i lsr 24) land 0xff))

##register add_uint32_be : binary, int -> void
let add_uint32_be b i =
  (* if (i < 0 || i > 0xffffffff) then error (Printf.sprintf "BslBinary.add_uint32_be: out of range int %d" i); *)
  Buf.add_char b (Char.chr ((i lsr 24) land 0xff));
  Buf.add_char b (Char.chr ((i lsr 16) land 0xff));
  Buf.add_char b (Char.chr ((i lsr 8 ) land 0xff));
  Buf.add_char b (Char.chr ( i         land 0xff))

##register add_uint64_le : binary, int64 -> void
let add_uint64_le b i =
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (                          i   ) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 8 ) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 16) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 24) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 32) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 40) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 48) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 56) 0xffL)))

##register add_uint64_be : binary, int64 -> void
let add_uint64_be b i =
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 56) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 48) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 40) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 32) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 24) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 16) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 8 ) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (                          i   ) 0xffL)))

##register add_int53_le : binary, int -> void
let add_int53_le b i =
  (* if (i < -0x20000000000000 || i > 0x1fffffffffffff) then error (Printf.sprintf "BslBinary.add_int53_le: out of range int %d" i); *)
  add_uint64_le b (Int64.of_int i)

##register add_int53_be : binary, int -> void
let add_int53_be b i =
  (* if (i < -0x20000000000000 || i > 0x1fffffffffffff) then error (Printf.sprintf "BslBinary.add_int53_be: out of range int %d" i); *)
  add_uint64_be b (Int64.of_int i)

(* Might not be accurate to the last bit... *)
let _FLOATMIN32 = 1.175494351e-38
let _FLOATMAX32 = 3.402823466e38

##register add_float_le : binary, float -> void
let add_float_le b f =
  (* if f < _FLOATMIN32 || f > _FLOATMAX32 then error (Printf.sprintf "BslBinary.add_float_le: out of range float %g" f); *)
  let i = Int32.bits_of_float f in
  Buf.add_char b (Char.chr (Int32.to_int (Int32.logand (                          i   ) 0xffl)));
  Buf.add_char b (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i 8 ) 0xffl)));
  Buf.add_char b (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i 16) 0xffl)));
  Buf.add_char b (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i 24) 0xffl)))

##register add_float_be : binary, float -> void
let add_float_be b f =
  (* if f < _FLOATMIN32 || f > _FLOATMAX32 then error (Printf.sprintf "BslBinary.add_float_le: out of range float %g" f); *)
  let i = Int32.bits_of_float f in
  Buf.add_char b (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i 24) 0xffl)));
  Buf.add_char b (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i 16) 0xffl)));
  Buf.add_char b (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i 8 ) 0xffl)));
  Buf.add_char b (Char.chr (Int32.to_int (Int32.logand (                          i   ) 0xffl)))

##register add_double_le : binary, float -> void
let add_double_le b f =
  let i = Int64.bits_of_float f in
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (                          i   ) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 8 ) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 16) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 24) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 32) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 40) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 48) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 56) 0xffL)))

##register add_double_be : binary, float -> void
let add_double_be b f =
  let i = Int64.bits_of_float f in
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 56) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 48) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 40) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 32) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 24) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 16) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 8 ) 0xffL)));
  Buf.add_char b (Char.chr (Int64.to_int (Int64.logand (                          i   ) 0xffL)))

##register get_string : binary, int, int -> string
let get_string b start length =
  Buf.sub b start length

##register get_binary : binary, int, int -> binary
let get_binary b start length =
  let blen = Buf.length b in
  if start < 0 || start >= blen || length < 0
  then Buf.create 0
  else
    try binary_of_string (Buf.sub b start (if blen < start + length then blen - start else length))
    with Invalid_argument _ -> Buf.create 0

##register get_int8 : binary, int -> int
let get_int8 b start =
  if start >= Buf.length b then error "BslBinary.get_int8: insufficient buffer data";
  let i = Char.code (Buf.nth b start) in
  if i > 0x7f then i - 0x100 else i

##register get_uint8 : binary, int -> int
let get_uint8 b start =
  if start >= Buf.length b then error "BslBinary.get_uint8: insufficient buffer data";
  Char.code (Buf.nth b start)

##register get_int16_le : binary, int -> int
let get_int16_le b start =
  if start > Buf.length b - 2 then error "BslBinary.get_int16_le: insufficient buffer data";
  let i =
           (Char.code (Buf.nth b  start             ) land 0x00ff)
     lor (((Char.code (Buf.nth b (start + 1))) lsl 8) land 0xff00)
  in
  if i > 0x7fff then i - 0x10000 else i

##register get_int16_be : binary, int -> int
let get_int16_be b start =
  if start > Buf.length b - 2 then error "BslBinary.get_int16_be: insufficient buffer data";
  let i =
           (Char.code (Buf.nth b (start + 1)        ) land 0x00ff)
     lor (((Char.code (Buf.nth b  start     )) lsl 8) land 0xff00)
  in
  if i > 0x7fff then i - 0x10000 else i

##register get_uint16_le : binary, int -> int
let get_uint16_le b start =
  if start > Buf.length b - 2 then error "BslBinary.get_uint16_le: insufficient buffer data";
        (Char.code (Buf.nth b  start             ) land 0x00ff)
  lor (((Char.code (Buf.nth b (start + 1))) lsl 8) land 0xff00)

##register get_uint16_be : binary, int -> int
let get_uint16_be b start =
  if start > Buf.length b - 2 then error "BslBinary.get_uint16_be: insufficient buffer data";
        (Char.code (Buf.nth b (start + 1)        ) land 0x00ff)
  lor (((Char.code (Buf.nth b  start     )) lsl 8) land 0xff00)

let _0xff000000 =
  #<Ifstatic:OCAML_WORD_SIZE 64>
    0xff000000
  #<Else>
    0x7f000000
  #<End>


##register get_int32_le : binary, int -> int
let get_int32_le b start =
  if start > Buf.length b - 4 then error "BslBinary.get_int32_le: insufficient buffer data";
  let i =
           (Char.code (Buf.nth b  start              ) land 0x000000ff)
     lor (((Char.code (Buf.nth b (start + 1))) lsl 8 ) land 0x0000ff00)
     lor (((Char.code (Buf.nth b (start + 2))) lsl 16) land 0x00ff0000)
     lor (((Char.code (Buf.nth b (start + 3))) lsl 24) land _0xff000000)
  in
  #<Ifstatic:OCAML_WORD_SIZE 64>
  if i > 0x7fffffff then i - 0x100000000 else i
  #<Else>
  i
  #<End>

##register get_int32_be : binary, int -> int
let get_int32_be b start =
  if start > Buf.length b - 4 then error "BslBinary.get_int32_be: insufficient buffer data";
  let i =
           (Char.code (Buf.nth b (start + 3)         ) land 0x000000ff)
     lor (((Char.code (Buf.nth b (start + 2))) lsl 8 ) land 0x0000ff00)
     lor (((Char.code (Buf.nth b (start + 1))) lsl 16) land 0x00ff0000)
     lor (((Char.code (Buf.nth b  start     )) lsl 24) land _0xff000000)
  in
  #<Ifstatic:OCAML_WORD_SIZE 64>
  if i > 0x7fffffff then i - 0x100000000 else i
  #<Else>
  i
  #<End>

##register get_uint32_le : binary, int -> int
let get_uint32_le b start =
  if start > Buf.length b - 4 then error "BslBinary.get_uint32_le: insufficient buffer data";
        (Char.code (Buf.nth b  start              ) land 0x000000ff)
  lor (((Char.code (Buf.nth b (start + 1))) lsl 8 ) land 0x0000ff00)
  lor (((Char.code (Buf.nth b (start + 2))) lsl 16) land 0x00ff0000)
  lor (((Char.code (Buf.nth b (start + 3))) lsl 24) land _0xff000000)

##register get_uint32_be : binary, int -> int
let get_uint32_be b start =
  if start > Buf.length b - 4 then error "BslBinary.get_uint32_be: insufficient buffer data";
        (Char.code (Buf.nth b (start + 3)         ) land 0x000000ff)
  lor (((Char.code (Buf.nth b (start + 2))) lsl 8 ) land 0x0000ff00)
  lor (((Char.code (Buf.nth b (start + 1))) lsl 16) land 0x00ff0000)
  lor (((Char.code (Buf.nth b  start     )) lsl 24) land _0xff000000)

let get_int64_le b p =
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buf.nth b (p+7)))) 56) 0xff00000000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buf.nth b (p+6)))) 48) 0x00ff000000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buf.nth b (p+5)))) 40) 0x0000ff0000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buf.nth b (p+4)))) 32) 0x000000ff00000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buf.nth b (p+3)))) 24) 0x00000000ff000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buf.nth b (p+2)))) 16) 0x0000000000ff0000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buf.nth b (p+1))))  8) 0x000000000000ff00L)
               (Int64.logand (                 (Int64.of_int (Char.code (Buf.nth b  p   )))   ) 0x00000000000000ffL))))))))

let get_int64_be b p =
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buf.nth b  p   ))) 56) 0xff00000000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buf.nth b (p+1)))) 48) 0x00ff000000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buf.nth b (p+2)))) 40) 0x0000ff0000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buf.nth b (p+3)))) 32) 0x000000ff00000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buf.nth b (p+4)))) 24) 0x00000000ff000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buf.nth b (p+5)))) 16) 0x0000000000ff0000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buf.nth b (p+6))))  8) 0x000000000000ff00L)
               (Int64.logand (                 (Int64.of_int (Char.code (Buf.nth b (p+7))))   ) 0x00000000000000ffL))))))))

##register get_int53_le : binary, int -> int
let get_int53_le b p = Int64.to_int (get_int64_le b p) (* will overflow *)

##register get_int53_be : binary, int -> int
let get_int53_be b p = Int64.to_int (get_int64_be b p) (* will overflow *)

##register get_uint64_le : binary, int -> int64
let get_uint64_le b p = get_int64_le b p (* it's actually signed *)

##register get_uint64_be : binary, int -> int64
let get_uint64_be b p = get_int64_be b p (* it's actually signed *)

##register get_float_le : binary, int -> float
let get_float_le b p =
  Int32.float_of_bits
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (Buf.nth b (p+3)))) 24) 0xff000000l)
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (Buf.nth b (p+2)))) 16) 0x00ff0000l)
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (Buf.nth b (p+1))))  8) 0x0000ff00l)
                 (Int32.logand (                 (Int32.of_int (Char.code (Buf.nth b  p   )))   ) 0x000000ffl))))

##register get_float_be : binary, int -> float
let get_float_be b p =
  Int32.float_of_bits
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (Buf.nth b  p   ))) 24) 0xff000000l)
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (Buf.nth b (p+1)))) 16) 0x00ff0000l)
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (Buf.nth b (p+2))))  8) 0x0000ff00l)
                 (Int32.logand (                 (Int32.of_int (Char.code (Buf.nth b (p+3))))   ) 0x000000ffl))))

##register get_double_le : binary, int -> float
let get_double_le b p = Int64.float_of_bits (get_int64_le b p)

##register get_double_be : binary, int -> float
let get_double_be b p = Int64.float_of_bits (get_int64_be b p)

