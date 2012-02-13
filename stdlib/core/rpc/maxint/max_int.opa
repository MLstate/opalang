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

import stdlib.core.rpc.core

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

/**
 * The largest integer usable in OPA
 *
 * Note that the size of integers is dictated both by server limitations (64-bit servers have larger integers than 32-bit servers)
 * and by client limitations (JavaScript implementations are typically limited to 53-bit integers).
 */
max_int = IntServer.max_int // 2**53 (size of significand in js)

/**
 * The smallest integer usable in OPA
 *
 * Note that the size of integers is dictated both by server limitations (64-bit servers have larger integers than 32-bit servers)
 * and by client limitations (JavaScript implementations are typically limited to 53-bit integers).
 */
min_int=-max_int


@publish IntServer =
{{
  /**
   * Returns the biggest integer representable by the server
   * @server
   */
  max_int     = %% BslNumber.Int.max_int %% : int
}}
