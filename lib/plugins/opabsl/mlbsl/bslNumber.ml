(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
##extern-type int64 = Int64.t

##extern-type int32 = Int32.t

##module Int \ bsl_int

##register max_int : int
  let max_int =
    let is_32 = (1 lsl 30) = max_int
    in if is_32 then max_int else  1 lsl 53;;

##register of_string : string -> int
  let of_string s =
    try
      Pervasives.int_of_string s
    with
    | Failure "int_of_string" ->
        failwith (Printf.sprintf "Error in Int.of_string: %S is not an integer." s)

##register of_string_opt : string -> option(int)
let of_string_opt s =
  try
    Some (Pervasives.int_of_string s)
  with
  | Failure "int_of_string" -> None

##register of_float : float -> int
  let of_float = Pervasives.int_of_float

(* Bitwise operations *)
##register op_land \ `Pervasives.(land)` : int, int -> int
##register op_lor \ `Pervasives.(lor)` : int, int -> int
##register op_lxor \ `Pervasives.(lxor)` : int, int -> int
##register op_lnot \ `Pervasives.lnot` : int -> int
##register op_lsl \ `Pervasives.(lsl)` : int, int -> int
##register op_lsr \ `Pervasives.(lsr)` : int, int -> int
##register op_asr \ `Pervasives.(asr)` : int, int -> int

##register leq: int, int -> bool
let leq (a:int) (b:int) = a <= b

##register geq: int, int -> bool
let geq (a:int) (b:int) = a >= b


##register ordering: int, int -> opa[Order.ordering]
let ordering (a:int) (b:int) =
  if a < b then BslPervasives.ord_result_lt
  else if a==b then BslPervasives.ord_result_eq
  else BslPervasives.ord_result_gt

##endmodule

##module Int64

##register neg \ `Int64.neg` : int64 -> int64
##register add \ `Int64.add` : int64, int64 -> int64
##register sub \ `Int64.sub` : int64, int64 -> int64
##register mul \ `Int64.mul` : int64, int64 -> int64
##register div \ `Int64.div` : int64, int64 -> int64
##register rem \ `Int64.rem` : int64, int64 -> int64
##register pred \ `Int64.pred` : int64, int64 -> int64
##register succ \ `Int64.succ` : int64, int64 -> int64
##register max_int : -> int64
let max_int _ = Int64.max_int
##register logand \ `Int64.logand` : int64, int64 -> int64
##register logor \ `Int64.logor` : int64, int64 -> int64
##register logxor \ `Int64.logxor` : int64, int64 -> int64
##register lognot \ `Int64.lognot` : int64 -> int64
##register shift_left \ `Int64.shift_left` : int64, int -> int64
##register shift_right \ `Int64.shift_right` : int64, int -> int64
##register shift_right_logical \ `Int64.shift_right_logical` : int64, int -> int64
##register of_int \ `Int64.of_int` : int -> int64
##register to_int \ `Int64.to_int` : int64 -> int
##register of_string \ `Int64.of_string` : string -> int64
##register to_string \ `Int64.to_string` : int64 -> string
##register op_eq : int64, int64 -> bool
let op_eq i1 i2 = i1 = i2
##register op_ne : int64, int64 -> bool
let op_ne i1 i2 = i1 <> i2
##register op_gt : int64, int64 -> bool
let op_gt i1 i2 = i1 > i2
##register op_ge : int64, int64 -> bool
let op_ge i1 i2 = i1 >= i2
##register op_lt : int64, int64 -> bool
let op_lt i1 i2 = i1 < i2
##register op_le : int64, int64 -> bool
let op_le i1 i2 = i1 <= i2

##endmodule

##module Float

  (* transforms the string in a format compatible with the same function
   * in jsbsl:
   * - same NaN, Infinity
   * - decimal point when not in scientific notation: 1.0, 1e-88
   * - no -0.0 *)
##register to_string : float -> string
  let to_string f =
    match classify_float f with
      | FP_nan -> "NaN"
      | FP_infinite -> if f > 0. then "Infinity" else "-Infinity"
      | FP_zero -> "0.0"
      | FP_subnormal (* what to do here for compatibility with js ? *)
      | FP_normal ->
          let s = string_of_float f in
          let last = String.length s - 1 in
          if s.[last] = '.' then s ^ "0"
          else s

##register to_formatted_string : bool, option(int), float -> string
  (** A function to format float printing
      @param always_dot [true] when the numbers should always have a decimal point: '1.00', for instance
                        [false] means that 1. will be printed as '1'
      @param decimals_option [None] means the default precision will be displayed
                             [Some _] the number of decimals that should appear after the dot
                             please note that if you say 2 decimals but always_dot is false
                             2. will be printed 2, not 2.00
                             (the number of decimals is used only if you have a decimal point to begin with)
      @param f The float you want to print
  *)
  let to_formatted_string always_dot decimals_option f =
    match classify_float f with
      | FP_nan -> "NaN"
      | FP_infinite -> if f > 0. then "Infinity" else "-Infinity"
      | FP_zero ->
          (match always_dot, decimals_option with
           | false, _ -> "0"
           | true, None -> "0.0"
           | true, Some decimals ->
               assert (decimals >= 0);
               Printf.sprintf "%.*f" decimals 0.)
      | FP_subnormal (* same remark as above *)
      | FP_normal ->
          let is_an_int_before_truncating =
            match modf f with
            | (0.,_) -> true
            | _ -> false in
          match decimals_option with
          | None ->
              if is_an_int_before_truncating then
                let int = string_of_int (int_of_float f) in
                if always_dot then int ^ ".0" else int
              else
                string_of_float f
          | Some decimals ->
              (* here we have a choice: either 4.02 with 1 decimal is displayed as 4.0
                 or it is displayed as 4 (when we don't want the decimal point)

                 here, we choose the first solution, but you would just have to replace
                 is_an_int_before_truncating by is_an_int_after_troncating to have
                 the opposite (and you would have to change the js code to so too)

                 let int = int_of_float in
                 let is_an_int_after_troncating =
                 let p =  (10. ** (float)decimals) in (* clean me *)
                 (int)(p *. f) mod (int)p = 0 in *)
              match always_dot, is_an_int_before_truncating with
              | false,true  -> string_of_int (int_of_float f)
              | _ -> Printf.sprintf "%.*f" decimals f

##register of_string : string -> float
let of_string s = Pervasives.float_of_string s

##register of_string_opt : string -> option(float)
let of_string_opt s =
  try
    Some (Pervasives.float_of_string s)
  with
  | Failure "float_of_string" -> None

##register of_int : int -> float
  let of_int = float_of_int

##register ceil : float -> float
  let ceil = Pervasives.ceil

##register floor : float -> float
  let floor = Pervasives.floor

##register leq: float, float -> bool
let leq (a:float) (b:float) = a <= b

##register lt: float, float -> bool
let lt (a:float) (b:float) = a < b

##register eq: float, float -> bool
let eq (a:float) (b:float) = a = b

##register geq: float, float -> bool
let geq (a:float) (b:float) = a >= b

##register gt: float, float -> bool
let gt (a:float) (b:float) = a > b

##register neq: float, float -> bool
let neq (a:float) (b:float) = a <> b

##register comparison: float, float -> opa[Order.comparison]
let comparison (a:float) (b:float) =
  if a = a && b = b then (*Handle [nan]*)
    if a < b then BslPervasives.comp_result_lt
    else if a = b then BslPervasives.comp_result_eq
    else BslPervasives.comp_result_gt
  else
    BslPervasives.comp_result_neq

##register round : float -> int
  let round v = int_of_float (Base.round 0 v)

##register embed_int_le : int -> string
let embed_int_le i =
  let s = "        " in
  s.[7] <- (Char.chr ((i lsr 56) land 0xff));
  s.[6] <- (Char.chr ((i lsr 48) land 0xff));
  s.[5] <- (Char.chr ((i lsr 40) land 0xff));
  s.[4] <- (Char.chr ((i lsr 32) land 0xff));
  s.[3] <- (Char.chr ((i lsr 24) land 0xff));
  s.[2] <- (Char.chr ((i lsr 16) land 0xff));
  s.[1] <- (Char.chr ((i lsr 8 ) land 0xff));
  s.[0] <- (Char.chr ( i         land 0xff));
  s

##register embed_int_be : int -> string
let embed_int_be i =
  let s = "        " in
  s.[0] <- (Char.chr ((i lsr 56) land 0xff));
  s.[1] <- (Char.chr ((i lsr 48) land 0xff));
  s.[2] <- (Char.chr ((i lsr 40) land 0xff));
  s.[3] <- (Char.chr ((i lsr 32) land 0xff));
  s.[4] <- (Char.chr ((i lsr 24) land 0xff));
  s.[5] <- (Char.chr ((i lsr 16) land 0xff));
  s.[6] <- (Char.chr ((i lsr 8 ) land 0xff));
  s.[7] <- (Char.chr ( i         land 0xff));
  s

##register embed_float_le : float -> string
let embed_float_le f =
  let i64 = Int64.bits_of_float f in
  let s = "        " in
  s.[7] <- (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 56) 0xffL)));
  s.[6] <- (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 48) 0xffL)));
  s.[5] <- (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 40) 0xffL)));
  s.[4] <- (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 32) 0xffL)));
  s.[3] <- (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 24) 0xffL)));
  s.[2] <- (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 16) 0xffL)));
  s.[1] <- (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 8 ) 0xffL)));
  s.[0] <- (Char.chr (Int64.to_int (Int64.logand (                          i64   ) 0xffL)));
  s

##register embed_float_be : float -> string
let embed_float_be f =
  let i64 = Int64.bits_of_float f in
  let s = "        " in
  s.[0] <- (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 56) 0xffL)));
  s.[1] <- (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 48) 0xffL)));
  s.[2] <- (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 40) 0xffL)));
  s.[3] <- (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 32) 0xffL)));
  s.[4] <- (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 24) 0xffL)));
  s.[5] <- (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 16) 0xffL)));
  s.[6] <- (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical i64 8 ) 0xffL)));
  s.[7] <- (Char.chr (Int64.to_int (Int64.logand (                          i64   ) 0xffL)));
  s

##register unembed_int_le: string, int -> int
let unembed_int_le s i =
      (((Char.code s.[i+7]) lsl 56) land 0x7f00000000000000)
  lor (((Char.code s.[i+6]) lsl 48) land 0x00ff000000000000)
  lor (((Char.code s.[i+5]) lsl 40) land 0x0000ff0000000000)
  lor (((Char.code s.[i+4]) lsl 32) land 0x000000ff00000000)
  lor (((Char.code s.[i+3]) lsl 24) land 0x00000000ff000000)
  lor (((Char.code s.[i+2]) lsl 16) land 0x0000000000ff0000)
  lor (((Char.code s.[i+1]) lsl  8) land 0x000000000000ff00)
  lor (((Char.code s.[i+0])       ) land 0x00000000000000ff)

##register unembed_int_be: string, int -> int
let unembed_int_be s i =
      (((Char.code s.[i+0]) lsl 56) land 0x7f00000000000000)
  lor (((Char.code s.[i+1]) lsl 48) land 0x00ff000000000000)
  lor (((Char.code s.[i+2]) lsl 40) land 0x0000ff0000000000)
  lor (((Char.code s.[i+3]) lsl 32) land 0x000000ff00000000)
  lor (((Char.code s.[i+4]) lsl 24) land 0x00000000ff000000)
  lor (((Char.code s.[i+5]) lsl 16) land 0x0000000000ff0000)
  lor (((Char.code s.[i+6]) lsl  8) land 0x000000000000ff00)
  lor (((Char.code s.[i+7])       ) land 0x00000000000000ff)

##register unembed_float_le: string, int -> float
let unembed_float_le s i =
  Int64.float_of_bits(
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+7])) 56) 0xff00000000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+6])) 48) 0x00ff000000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+5])) 40) 0x0000ff0000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+4])) 32) 0x000000ff00000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+3])) 24) 0x00000000ff000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+2])) 16) 0x0000000000ff0000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+1]))  8) 0x000000000000ff00L)
               (Int64.logand (                 (Int64.of_int (Char.code s.[i  ]))   ) 0x00000000000000ffL)))))))))

##register unembed_float_be: string, int -> float
let unembed_float_be s i =
  Int64.float_of_bits(
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i  ])) 56) 0xff00000000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+1])) 48) 0x00ff000000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+2])) 40) 0x0000ff0000000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+3])) 32) 0x000000ff00000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+4])) 24) 0x00000000ff000000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+5])) 16) 0x0000000000ff0000L)
  (Int64.logor (Int64.logand (Int64.shift_left (Int64.of_int (Char.code s.[i+6]))  8) 0x000000000000ff00L)
               (Int64.logand (                 (Int64.of_int (Char.code s.[i+7]))   ) 0x00000000000000ffL)))))))))

##endmodule

##module Math

##register sqrt_f : float -> float
  let sqrt_f = Pervasives.sqrt

##register sqrt_i : int -> int
  let sqrt_i n = Pervasives.int_of_float (Pervasives.sqrt (Pervasives.float_of_int n))

##register log : float -> float
  let log = Pervasives.log

##register exp : float -> float
  let exp = Pervasives.exp

##register abs_i : int -> int
  let abs_i = Pervasives.abs

##register abs_f : float -> float
  let abs_f = Pervasives.abs_float

##register ceil : float -> float
  let ceil = Pervasives.ceil

##register floor : float -> float
  let floor = Pervasives.floor

##register sin : float -> float
  let sin = Pervasives.sin

##register cos : float -> float
  let cos = Pervasives.cos

##register tan : float -> float
  let tan = Pervasives.tan

##register asin : float -> float
  let asin = Pervasives.asin

##register acos : float -> float
  let acos = Pervasives.acos

##register atan : float -> float
  let atan = Pervasives.atan

  (* keep the coerse `x : float', otherwise isNaN(0.0 /. 0.0) is false *)
##register isNaN : float -> bool
  let isNaN = (fun (x : float) -> not ( x = x ))

##register is_infinite : float -> bool
  let is_infinite f = classify_float f = FP_infinite

##register is_normal : float -> bool
  let is_normal f =
    match classify_float f with
      | FP_normal | FP_subnormal | FP_zero -> true
      | _ -> false

##endmodule

##module Random

  let max_int_for_random_int = 1 lsl 30

##register int : int -> int
  let int v =
    if v<max_int_for_random_int then Random.int v else Int64.to_int (Random.int64 (Int64.of_int v))

##register float : float -> float
  let float v = Random.float v

##register random_init : -> void
  let random_init() =
    Random.self_init()

##register generic_string : string, int -> string
  let generic_string chars len =
    let s = String.create len in
    for i =  0 to len - 1 do
      s.[i] <- chars.[Random.int (String.length chars)]
    done;
    s

##register string : int -> string
  let string len =
    let chars = "abcdefghijklmnopqrstuvwxyz" in
    generic_string chars len

##register base64 : int -> string
let base64 len =
  let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  generic_string chars len

##register base64_url : int -> string
let base64_url len =
  let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" in
  generic_string chars len

##endmodule
