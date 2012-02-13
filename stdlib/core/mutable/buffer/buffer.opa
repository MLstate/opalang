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
 * An implementation of non-functional buffers.
 *
 * Important note: the buffers are *not* serializable, nor are they meant to be.
 * This module is meant to be used internally by the standard library, not by
 * end-users.
 *
 * @author David Rajchenbach-Teller
 * @destination INTERNAL
 */

type Buffer2_private.buffer = external//Defined in fbuffer.js / fbuffer.ml

Buffer2_private = {{
  create: int -> Buffer2_private.buffer = %% Fbuffer.create %%
  add  :   Buffer2_private.buffer, string -> void = %% Fbuffer.add %%
  addln:   Buffer2_private.buffer, string -> void = %% Fbuffer.addln %%
  contents: Buffer2_private.buffer -> string                        = %% Fbuffer.contents %%
  to_string: Buffer2_private.buffer -> string                       = %% Fbuffer.contents %%
  is_empty: Buffer2_private.buffer -> bool                          = %% Fbuffer.is_empty %%
  add_list(l, buf:Buffer2_private.buffer): void =
    List.iter((x -> add(buf, x)), l)
  reset : Buffer2_private.buffer, int -> void = %% Fbuffer.reset %%
}}
