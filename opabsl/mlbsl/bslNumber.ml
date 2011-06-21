(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)
##module Int \ bsl_int

##register to_char : int -> option(char)
  let to_char i =
    if 0 <= i && i <= 127 then
      try Some (Base.Char.chr i) with Invalid_argument _ -> None
    else None

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

##register lt: int, int -> bool
let lt (a:int) (b:int) = a < b

##register eq: int, int -> bool
let eq (a:int) (b:int) = a = b

##register geq: int, int -> bool
let geq (a:int) (b:int) = a >= b

##register gt: int, int -> bool
let gt (a:int) (b:int) = a > b

##register neq: int, int -> bool
let neq (a:int) (b:int) = a <> b


##register ordering: int, int -> opa[Order.ordering]
let ordering (a:int) (b:int) =
  if a < b then BslPervasives.ord_result_lt
  else if a==b then BslPervasives.ord_result_eq
  else BslPervasives.ord_result_gt

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

##endmodule

##module Math

##register pi : float
  let pi = 4. *. (atan 1.);;

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

##register sinh : float -> float
  let sinh = Pervasives.sinh

##register cosh : float -> float
  let cosh = Pervasives.cosh

##register tanh : float -> float
  let tanh = Pervasives.tanh

##register round : float -> int
  let round x = int_of_float (floor (x +. 0.5))

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

##register string : int -> string
  let string len =
    let s = String.create len in
      for i =  0 to len - 1 do
        s.[i] <- char_of_int (97 + Random.int 26)
      done;
      s

##endmodule
