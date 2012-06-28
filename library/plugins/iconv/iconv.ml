type iconv_t

external iconv_open: tocode:string -> fromcode:string -> iconv_t = "mliconv_open";;
external iconv_prim: iconv_t -> string -> string = "mliconv_convert";;

