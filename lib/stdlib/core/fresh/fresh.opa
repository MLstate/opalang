/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
