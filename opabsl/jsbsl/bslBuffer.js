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
 * A low-level module of mutable string buffers.
 *
 * Values of type [buffer] are not and should not be serialized, so any use of buffer must be strictly controlled
 *
 *
 * @author David Rajchenbach-Teller
 */

##extern-type Buffer_private.buffer
   //a JS array

/**
 * @constructor
 */
function LLBuffer() {
    this.contents = [];
    this.length   = 0;
}

LLBuffer.prototype = {
    opa_do_not_inspect: true
}

##register create: int -> Buffer_private.buffer
##args(_)
{
    return new LLBuffer();
}

##register append: Buffer_private.buffer, string -> void
##args(buf, value)
{
    buf.contents.push(value);
    buf.length = buf.length + value.length
}

##register contents: Buffer_private.buffer -> string
##args(buf)
{
    var contents = buf.contents;
    var result   = contents.join("");
    buf.contents = [result];//Cache result
    return result;
}
