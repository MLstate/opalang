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
 * {1 Where should I start ?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Interface}
 */

Random = {{
  int   = %% BslNumber.Random.int %% : int -> int
  float = %% BslNumber.Random.float %% : float -> float
  random_init = %% BslNumber.Random.random_init %% : -> void

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
