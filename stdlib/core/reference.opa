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
