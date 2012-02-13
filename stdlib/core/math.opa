/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Interface}
 */

Math = {{

  /**
   * {2 Constants}
  **/

  E         = 2.718281828459045235360
  PI        = 3.141592653589793238462
  SQRT2     = 1.414213562373095048802

  /**
   * {2 Methods}
  **/

  acos      = %% BslNumber.Math.acos %%      : float -> float
  asin      = %% BslNumber.Math.asin %%      : float -> float
  atan      = %% BslNumber.Math.atan %%      : float -> float
  ceil      = %% BslNumber.Math.ceil %%      : float -> float
  cos       = %% BslNumber.Math.cos %%       : float -> float
  exp       = %% BslNumber.Math.exp %%       : float -> float
  floor     = %% BslNumber.Math.floor %%     : float -> float
  is_NaN    = %% BslNumber.Math.isNaN %%     : float -> bool
  is_infinite = %% BslNumber.Math.is_infinite %% : float -> bool
  is_normal = %% BslNumber.Math.is_normal %%: float -> bool
  ln        = %% BslNumber.Math.log %%       : float -> float
  sin       = %% BslNumber.Math.sin %%       : float -> float
  sqrt_f    = %% BslNumber.Math.sqrt_f %%    : float -> float
  sqrt_i    = %% BslNumber.Math.sqrt_i %%    : int -> int
  tan       = %% BslNumber.Math.tan %%       : float -> float

  /**
   * An interesting variant of Float.round, which returns a float (instead of an int),
   * and which rounds 0.5 to 1. (instead of 0 for Float.round)
   * (well, on server-side; of course it's different on client-side)
  **/
  round(n:float) = floor(n + 0.5)

  square_f(n:float) = n * n
  square_i(n:int) = n * n

  /**
   * You can use pow_f ONLY when a is greater than 0
  **/
  pow_f(a, b:float) = Math.exp(b * Math.ln(a))

  pow_i(a, b) = match b
    0 -> 1
    1 -> a
    _ ->
    if b < 0 then error("Math.pow_i with invalid number")
    else
      square_i( pow_i(a, b / 2)) * (if mod(b, 2) == 0 then 1 else a)

  div_sup(n, d) = (n + d - 1) / d
}}
