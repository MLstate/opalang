##extern-type Iconv.t = Iconv.iconv_t
##extern-type binary = string

##register iconv_open : binary, binary -> Iconv.t
let iconv_open tocode fromcode = Iconv.iconv_open ~tocode ~fromcode

##register iconv : Iconv.t, binary -> binary
let iconv t s = Iconv.iconv t s
