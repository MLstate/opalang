
##extern-type binary = Buffer.t

##register create\ `Buffer.create`: int -> binary

##register length\ `Buffer.length`: binary -> int

##register binary_of_string : string -> binary
let binary_of_string s = 
  let b = Buffer.create (String.length s) in
  Buffer.add_string b s;
  b

##register string_of_binary\ `Buffer.contents`: binary -> string

##register clear\ `Buffer.clear`: binary -> void

##register trim: binary -> void
let trim b =
  let s = Buffer.contents b in
  let b = Buffer.create (String.length s) in
  Buffer.add_string b s

##register reset\ `Buffer.reset`: binary -> void

##register add_string\ `Buffer.add_string`: binary, string -> void

exception BslBinaryError of string
let error str = raise (BslBinaryError str)

##register add_int8 : binary, int -> void
let add_int8 b i =
  if (i < -0x80 || i > 0x7f) then error (Printf.sprintf "BslBinary.add_int8: out of range int %d" i);
  Buffer.add_char b (Char.chr (if i < 0 then i+0x100 else i))

##register add_uint8 : binary, int -> void
let add_uint8 b i =
  if (i < 0 || i > 0xff) then error (Printf.sprintf "BslBinary.add_uint8: out of range int %d" i);
  Buffer.add_char b (Char.chr i)

##register add_int16_le : binary, int -> void
let add_int16_le b i =
  if (i < -0x8000 || i > 0x7fff) then error (Printf.sprintf "BslBinary.add_int16_le: out of range int %d" i);
  let i = if i < 0 then 0x10000 + i else i in
  Buffer.add_char b (Char.chr ( i         land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 8 ) land 0xff))

##register add_int16_be : binary, int -> void
let add_int16_be b i =
  if (i < -0x8000 || i > 0x7fff) then error (Printf.sprintf "BslBinary.add_int16_be: out of range int %d" i);
  let i = if i < 0 then 0x10000 + i else i in
  Buffer.add_char b (Char.chr ((i lsr 8 ) land 0xff));
  Buffer.add_char b (Char.chr ( i         land 0xff))

##register add_uint16_le : binary, int -> void
let add_uint16_le b i =
  if (i < 0 || i > 0xffff) then error (Printf.sprintf "BslBinary.add_uint16_le: out of range int %d" i);
  Buffer.add_char b (Char.chr ( i         land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 8 ) land 0xff))

##register add_uint16_be : binary, int -> void
let add_uint16_be b i =
  if (i < 0 || i > 0xffff) then error (Printf.sprintf "BslBinary.add_uint16_be: out of range int %d" i);
  Buffer.add_char b (Char.chr ((i lsr 8 ) land 0xff));
  Buffer.add_char b (Char.chr ( i         land 0xff))

##register add_int32_le : binary, int -> void
let add_int32_le b i =
  if (i < -0x80000000 || i > 0x7fffffff) then error (Printf.sprintf "BslBinary.add_int32_le: out of range int %d" i);
  let i = if i < 0 then 0x100000000 + i else i in
  Buffer.add_char b (Char.chr ( i         land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 8 ) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 16) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 24) land 0xff))

##register add_int32_be : binary, int -> void
let add_int32_be b i =
  if (i < -0x80000000 || i > 0x7fffffff) then error (Printf.sprintf "BslBinary.add_int32_be: out of range int %d" i);
  let i = if i < 0 then 0x100000000 + i else i in
  Buffer.add_char b (Char.chr ((i lsr 24) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 16) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 8 ) land 0xff));
  Buffer.add_char b (Char.chr ( i         land 0xff))

##register add_uint32_le : binary, int -> void
let add_uint32_le b i =
  if (i < 0 || i > 0xffffffff) then error (Printf.sprintf "BslBinary.add_uint32_le: out of range int %d" i);
  Buffer.add_char b (Char.chr ( i         land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 8 ) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 16) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 24) land 0xff))

##register add_uint32_be : binary, int -> void
let add_uint32_be b i =
  if (i < 0 || i > 0xffffffff) then error (Printf.sprintf "BslBinary.add_uint32_be: out of range int %d" i);
  Buffer.add_char b (Char.chr ((i lsr 24) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 16) land 0xff));
  Buffer.add_char b (Char.chr ((i lsr 8 ) land 0xff));
  Buffer.add_char b (Char.chr ( i         land 0xff))

(* Might not be accurate to the last bit... *)
let _FLOATMIN32 = 1.175494351e-38
let _FLOATMAX32 = 3.402823466e38

##register add_float_le : binary, float -> void
let add_float_le b f =
  if f < _FLOATMIN32 || f > _FLOATMAX32 then error (Printf.sprintf "BslBinary.add_float_le: out of range float %g" f);
  let i = Int32.bits_of_float f in
  Buffer.add_char b (Char.chr (Int32.to_int (Int32.logand (                          i   ) 0xffl)));
  Buffer.add_char b (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i 8 ) 0xffl)));
  Buffer.add_char b (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i 16) 0xffl)));
  Buffer.add_char b (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i 24) 0xffl)))

##register add_float_be : binary, float -> void
let add_float_be b f =
  if f < _FLOATMIN32 || f > _FLOATMAX32 then error (Printf.sprintf "BslBinary.add_float_le: out of range float %g" f);
  let i = Int32.bits_of_float f in
  Buffer.add_char b (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i 24) 0xffl)));
  Buffer.add_char b (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i 16) 0xffl)));
  Buffer.add_char b (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical i 8 ) 0xffl)));
  Buffer.add_char b (Char.chr (Int32.to_int (Int32.logand (                          i   ) 0xffl)))

##register add_double_le : binary, float -> void
let add_double_le b f =
  let i = Int64.bits_of_float f in
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (                          i   ) 0xffL)));
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 8 ) 0xffL)));
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 16) 0xffL)));
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 24) 0xffL)));
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 32) 0xffL)));
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 40) 0xffL)));
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 48) 0xffL)));
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 56) 0xffL)))

##register add_double_be : binary, float -> void
let add_double_be b f =
  let i = Int64.bits_of_float f in
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 56) 0xffL)));
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 48) 0xffL)));
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 40) 0xffL)));
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 32) 0xffL)));
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 24) 0xffL)));
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 16) 0xffL)));
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i 8 ) 0xffL)));
  Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (                          i   ) 0xffL)))

##register get_string : binary, int, int -> string
let get_string b start length =
  Buffer.sub b start length

##register get_int8 : binary, int -> int
let get_int8 b start =
  if start >= Buffer.length b then error "BslBinary.get_int8: insufficient buffer data";
  let i = Char.code (Buffer.nth b start) in
  if i > 0x7f then i - 0x100 else i

##register get_uint8 : binary, int -> int
let get_uint8 b start =
  if start >= Buffer.length b then error "BslBinary.get_uint8: insufficient buffer data";
  Char.code (Buffer.nth b start)

##register get_int16_le : binary, int -> int
let get_int16_le b start =
  if start > Buffer.length b - 2 then error "BslBinary.get_int16_le: insufficient buffer data";
  let i =
           (Char.code (Buffer.nth b  start             ) land 0x00ff)
     lor (((Char.code (Buffer.nth b (start + 1))) lsl 8) land 0xff00)
  in
  if i > 0x7fff then i - 0x10000 else i

##register get_int16_be : binary, int -> int
let get_int16_be b start =
  if start > Buffer.length b - 2 then error "BslBinary.get_int16_be: insufficient buffer data";
  let i =
           (Char.code (Buffer.nth b (start + 1)        ) land 0x00ff)
     lor (((Char.code (Buffer.nth b  start     )) lsl 8) land 0xff00)
  in
  if i > 0x7fff then i - 0x10000 else i

##register get_uint16_le : binary, int -> int
let get_uint16_le b start =
  if start > Buffer.length b - 2 then error "BslBinary.get_uint16_le: insufficient buffer data";
        (Char.code (Buffer.nth b  start             ) land 0x00ff)
  lor (((Char.code (Buffer.nth b (start + 1))) lsl 8) land 0xff00)

##register get_uint16_be : binary, int -> int
let get_uint16_be b start =
  if start > Buffer.length b - 2 then error "BslBinary.get_uint16_be: insufficient buffer data";
        (Char.code (Buffer.nth b (start + 1)        ) land 0x00ff)
  lor (((Char.code (Buffer.nth b  start     )) lsl 8) land 0xff00)

##register get_int32_le : binary, int -> int
let get_int32_le b start =
  if start > Buffer.length b - 4 then error "BslBinary.get_int32_le: insufficient buffer data";
  let i =
           (Char.code (Buffer.nth b  start              ) land 0x000000ff)
     lor (((Char.code (Buffer.nth b (start + 1))) lsl 8 ) land 0x0000ff00)
     lor (((Char.code (Buffer.nth b (start + 2))) lsl 16) land 0x00ff0000)
     lor (((Char.code (Buffer.nth b (start + 3))) lsl 24) land 0xff000000)
  in
  if i > 0x7fffffff then i - 0x100000000 else i

##register get_int32_be : binary, int -> int
let get_int32_be b start =
  if start > Buffer.length b - 4 then error "BslBinary.get_int32_be: insufficient buffer data";
  let i =
           (Char.code (Buffer.nth b (start + 3)         ) land 0x000000ff)
     lor (((Char.code (Buffer.nth b (start + 2))) lsl 8 ) land 0x0000ff00)
     lor (((Char.code (Buffer.nth b (start + 1))) lsl 16) land 0x00ff0000)
     lor (((Char.code (Buffer.nth b  start     )) lsl 24) land 0xff000000)
  in
  if i > 0x7fffffff then i - 0x100000000 else i

##register get_uint32_le : binary, int -> int
let get_uint32_le b start =
  if start > Buffer.length b - 4 then error "BslBinary.get_uint32_le: insufficient buffer data";
        (Char.code (Buffer.nth b  start              ) land 0x000000ff)
  lor (((Char.code (Buffer.nth b (start + 1))) lsl 8 ) land 0x0000ff00)
  lor (((Char.code (Buffer.nth b (start + 2))) lsl 16) land 0x00ff0000)
  lor (((Char.code (Buffer.nth b (start + 3))) lsl 24) land 0xff000000)

##register get_uint32_be : binary, int -> int
let get_uint32_be b start =
  if start > Buffer.length b - 4 then error "BslBinary.get_uint32_be: insufficient buffer data";
        (Char.code (Buffer.nth b (start + 3)         ) land 0x000000ff)
  lor (((Char.code (Buffer.nth b (start + 2))) lsl 8 ) land 0x0000ff00)
  lor (((Char.code (Buffer.nth b (start + 1))) lsl 16) land 0x00ff0000)
  lor (((Char.code (Buffer.nth b  start     )) lsl 24) land 0xff000000)

##register get_float_le : binary, int -> float
let get_float_le b p =
  Int32.float_of_bits
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (Buffer.nth b (p+3)))) 24) 0xff000000l)
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (Buffer.nth b (p+2)))) 16) 0x00ff0000l)
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (Buffer.nth b (p+1))))  8) 0x0000ff00l)
                 (Int32.logand (                 (Int32.of_int (Char.code (Buffer.nth b  p   )))   ) 0x000000ffl))))

##register get_float_be : binary, int -> float
let get_float_be b p =
  Int32.float_of_bits
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (Buffer.nth b  p   ))) 24) 0xff000000l)
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (Buffer.nth b (p+1)))) 16) 0x00ff0000l)
    (Int32.logor (Int32.logand (Int32.shift_left (Int32.of_int (Char.code (Buffer.nth b (p+2))))  8) 0x0000ff00l)
                 (Int32.logand (                 (Int32.of_int (Char.code (Buffer.nth b (p+3))))   ) 0x000000ffl))))

##register get_double_le : binary, int -> float
let get_double_le b p =
  Int64.float_of_bits
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buffer.nth b (p+7)))) 56) 0xff00000000000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buffer.nth b (p+6)))) 48) 0x00ff000000000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buffer.nth b (p+5)))) 40) 0x0000ff0000000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buffer.nth b (p+4)))) 32) 0x000000ff00000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buffer.nth b (p+3)))) 24) 0x00000000ff000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buffer.nth b (p+2)))) 16) 0x0000000000ff0000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buffer.nth b (p+1))))  8) 0x000000000000ff00L)
                 (Int64.logand (                 (Int64.of_int (Char.code (Buffer.nth b  p   )))   ) 0x00000000000000ffL))))))))

##register get_double_be : binary, int -> float
let get_double_be b p =
  Int64.float_of_bits
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buffer.nth b  p   ))) 56) 0xff00000000000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buffer.nth b (p+1)))) 48) 0x00ff000000000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buffer.nth b (p+2)))) 40) 0x0000ff0000000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buffer.nth b (p+3)))) 32) 0x000000ff00000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buffer.nth b (p+4)))) 24) 0x00000000ff000000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buffer.nth b (p+5)))) 16) 0x0000000000ff0000L)
    (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code (Buffer.nth b (p+6))))  8) 0x000000000000ff00L)
                 (Int64.logand (                 (Int64.of_int (Char.code (Buffer.nth b (p+7))))   ) 0x00000000000000ffL))))))))

