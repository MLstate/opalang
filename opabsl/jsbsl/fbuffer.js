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
