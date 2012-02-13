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
 * Library for fresh identifiers
 *
 * @author Mathieu Barbin
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

  @server @private private_server_Fresh() =
    // the only message is incr
    on_message(state : int, _ : void) = {
      return = state ;
      instruction = { set = succ(state) } ;
    }
    // FIXME: probably use Cell.cloud
    Cell.make(0, on_message)

  @client @private private_client_Fresh() =
    ClientReference.create(0)

  /**
   * The function [from_int] is called, starting from [0] included,
   * and increasing.
  **/

  @client client(from_int : int -> 'fresh) =
    r = private_client_Fresh()
    next() =
      v = ClientReference.get(r)
      do ClientReference.update(r, succ)
      from_int(v)
    next : Fresh.next('fresh)

  /**
   * This fresh uses a shared cell
  **/
  @server server(from_int : int -> 'fresh) =
    r = private_server_Fresh()
    next() =
      i = Cell.call(r, void)
      from_int(i)
    next : Fresh.next('fresh)

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
