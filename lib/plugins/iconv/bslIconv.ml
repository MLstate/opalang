##extern-type Iconv.t = Iconv.iconv_t

##property[mli]
##extern-type binary = Buf.t
##property[endmli]

##register iconv_open : string, string -> Iconv.t
let iconv_open tocode fromcode = Iconv.iconv_open ~tocode ~fromcode

##register iconv : Iconv.t, binary -> opa[option(binary)]
let iconv t s =
  try
    let s = Iconv.iconv_prim t (Buf.contents s) in
    let x = Buf.create (String.length s) in
    Buf.add_string x s;
    ServerLib.some (x)
  with
    Failure s -> ServerLib.none
