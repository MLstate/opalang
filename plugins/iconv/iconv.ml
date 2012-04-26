type iconv_t

external iconv_open: tocode:string -> fromcode:string -> iconv_t = "mliconv_open";;
external iconv: iconv_t -> string -> string = "mliconv_convert";;

