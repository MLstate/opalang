/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * {1 About this module}
 *
 * {1 Where should I start ?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Interface}
 */

Random = {{
  /**
   * [Random.random_init()]
   * Initialize the random generator
  **/
  random_init = %% BslNumber.Random.random_init %% : -> void

  /**
   * [Random.int(N)]
   * returns an integer in [  [0 , N[  ]
  **/
  int   = %% BslNumber.Random.int %% : int -> int

  /**
   * [Random.float(R)]
   * returns a float in [  [0 , R[  ]
  **/
  float = %% BslNumber.Random.float %% : float -> float

  /**
   * [Random.bool(p)]
   * returns a random bool, true with probability p
   * use p=0.5 to obtain true or false with equiprobability
  **/
  bool(f) = Random.float(1.0) < f

  /**
   * [Random.generic_string(s, n)]
   * returns a fresh string of length [n], composed of random char,
   * picked in [s]
  **/
  generic_string = %% BslNumber.Random.generic_string %% : string, int -> string

  /**
   * [Random.string(n)]
   * returns a fresh string of length [n], composed of random char,
   * picked in [['a'-'z']]
  **/
  string = %% BslNumber.Random.string %% : int -> string

  /**
   * [Random.base64(n)]
   * returns a fresh string of length [n], composed of random char,
   * picked in [['a'-'z','A'-'Z',0-9,+,/]]
  **/
  base64 = %% BslNumber.Random.base64 %% : int -> string

  /**
   * [Random.base64_url(n)]
   * returns a fresh string of length [n], composed of random char,
   * picked in [['a'-'z','A'-'Z',0-9,-,_]]
  **/
  base64_url = %% BslNumber.Random.base64_url %% : int -> string
}}
