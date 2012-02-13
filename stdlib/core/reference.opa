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
 * {1 Types defined in this module}
 */

type reference('a) = external

/**
 * {1 Interface}
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
