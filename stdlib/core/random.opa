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
Random = {{
  int   = %% BslNumber.Random.int %% : int -> int
  float = %% BslNumber.Random.float %% : float -> float
  random_init = %% BslNumber.Random.random_init %% : -> void

  /**
   * [Random.string(n)]
   * returns a fresh string of length [n], composed of random char,
   * picked in [['a'-'z']]
  **/
  string = %% BslNumber.Random.string %% : int -> string
}}
