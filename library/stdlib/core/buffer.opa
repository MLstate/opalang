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
 * An implementation of non-functional buffers.
 *
 * Important note: the buffers are *not* serializable, nor are they meant to be.
 * This module is meant to be used internally by the standard library, not by
 * end-users.
 *
 * @author David Rajchenbach-Teller
 * @destination INTERNAL
 */

type Buffer.t = external

Buffer = {{

    create: int -> Buffer.t = %% BslBuffer.create %%
    append: Buffer.t, string -> void = %% BslBuffer.append %%
    length: Buffer.t -> int = %% BslBuffer.length %%
    contents: Buffer.t -> string = %% BslBuffer.contents %%
    clear: Buffer.t -> void = %% BslBuffer.clear %%

    to_string = contents

    append_list(buf:Buffer.t, l): void =
        List.iter((x -> append(buf, x)), l)
    is_empty(b) = length(b) == 0
}}
