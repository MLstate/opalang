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
