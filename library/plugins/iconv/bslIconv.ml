##extern-type Iconv.t = Iconv.iconv_t

##property[mli]
##extern-type binary = string
##property[endmli]

##register iconv_open : string, string -> Iconv.t
let iconv_open tocode fromcode = Iconv.iconv_open ~tocode ~fromcode

##register iconv : Iconv.t, binary -> opa[option(binary)]
let iconv t s =
  try
    ServerLib.some (Iconv.iconv_prim t s)
  with
    Failure s -> ServerLib.none
