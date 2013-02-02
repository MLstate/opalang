/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * {1 Types defined in this module}
 * CAUTION : Being a memory location, this type cannot exchanged between client and server.
 *           Consider using a Mutable.t of the Mutable module instead.
 */
type reference('a) = external

/**
 * {1 About this module}
 *
 * It provides a way to have a mutable state.
 *
 * But please consider using module Mutable instead or at least read the description of the type reference.
 */
Reference =
{{
  create = %% BslReference.create %% : 'a -> reference('a)
  set = %% BslReference.set %%    : reference('a), 'a -> void
  get = %% BslReference.get %%    : reference('a) -> 'a

  /**
   * Atomic compare-and-swap operation
   *
   * This operation is used to develop concurrent non-blocking algorithms. It is used to replace the content of a reference,
   * but only if the content still holds the latest value known to have been put in this reference. If no concurrent unit has
   * modified the value in the meantime, or if the previous value has been restored, the reference is updated with the new
   * value. Otherwise, nothing happens.
   *
   * Important note: This operation checks for {e physical equality}.
   *
   * @param r A reference
   * @param candidate A value that the current execution expects to see in [r]
   * @param replacement A value that the current execution wishes to put in [r]
   *  @return [true] If the value of [r] was physically equal to [candidate], in which case the new value of [r] is now set to [replacement] / [false] otherwise
   */
   compare_and_swap = %% BslReference.compare_and_swap %%: reference('a),'a,'a -> bool
   update(ref,f) = set(ref,f(get(ref)))
}}

/**
 * {1 Server-side reference}
 */

/**
  * Deprecated, same as reference
  */
type Server.reference('a) = reference('a)

/**
  * A server only version of Reference, see above
  */
// Duplication of code to ensure creation side
ServerReference = {{
  @server_private  create = %% BslReference.create %% : 'a -> reference('a)
  set = %% BslReference.set %%    : reference('a), 'a -> void
  get = %% BslReference.get %%    : reference('a) -> 'a
  compare_and_swap = %% BslReference.compare_and_swap %%: reference('a),'a,'a -> bool
  update(ref,f) = set(ref,f(get(ref)))
}}

@deprecated({use="ServerReference"})
Server_reference = ServerReference

@opacapi
ServerReference_create = %% BslReference.create %%
