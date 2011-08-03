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

/*
 * A private type of *non-functional* buffer
 *
 * Note: It's not serializable, nor is it meant to be.
 */


##extern-type Buffer2_private.buffer

##register create: int -> Buffer2_private.buffer
##args(size)
{
    return %% BslBuffer.create %%(size)
}

##register add: Buffer2_private.buffer, string -> void
##args(buf, value)
{
    %% BslBuffer.append %%(buf, value);
}

##register addln: Buffer2_private.buffer, string -> void
##args(buf, value)
{
    %% BslBuffer.append %%(buf, value)
    %% BslBuffer.append %%(buf, "\n")
}


##register contents: Buffer2_private.buffer -> string
##args(buf)
{
    return %% BslBuffer.contents %%(buf)
}

##register is_empty: Buffer2_private.buffer -> bool
##args(buf)
{
    return %% BslBuffer.is_empty %%(buf)
}


##register reset: Buffer2_private.buffer, int -> void
##args(b,_i)
{
    b.contents = [];
    b.length = 0;
}
