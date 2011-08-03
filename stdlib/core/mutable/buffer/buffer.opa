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
