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

##register is_empty: Buffer_private.buffer -> bool
##args(buf)
{
    return buf.length == 0;
}
