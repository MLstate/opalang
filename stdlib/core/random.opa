/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
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
