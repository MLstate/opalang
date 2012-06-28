/*
    Copyright © 2011, 2012 MLstate

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
 * Library for fresh identifiers
 *
 * @author Mathieu Barbin
 * @author Quentin Bourgerie
 * @destination public
 */

import stdlib.core.{rpc.core}

/**
 * {1 About this module}
 *
 * This module implements primitives for creating fresh identifier.
 * Depending on what you want to do, there are different implementations.
 * On the server, it is based on a global cell, on the client, it is a reference.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
**/

/**
 * {1 Types defined in this module}
 */

/**
 * The interface of a fresh maker
 */
type Fresh.next('fresh) = -> 'fresh

/**
 * {1 Interface}
 */

Fresh = {{

  /**
   * The function [from_int] is called, starting from [0] included,
   * and increasing.
  **/

  @client client(from_int : int -> 'fresh) : Fresh.next('fresh) =
    x = Reference.create(0)
    ->
      from_int(@atomic(
        i = ClientReference.get(x)
        do ClientReference.set(x, i+1)
        i))

  /**
   * This fresh uses a shared cell
  **/
  @server server(from_int : int -> 'fresh) : Fresh.next('fresh)=
    x = Reference.create(0)
    ->
      from_int(@atomic(
        i = ServerReference.get(x)
        do ServerReference.set(x, i+1)
        i))


  /**
   * This fresh use a {!Mutable.t}
  **/
  mutable(from_int : int -> 'fresh) =
    r = Mutable.make(0)
    next() =
      v = r.get()
      do r.set(succ(v))
      from_int(v)
    next : Fresh.next('fresh)
}}
