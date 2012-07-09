/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Client-side implementation of UTF-8 strings.
 */

##register length : string -> int
##args(s)
{
    return s.length;
}

/**
 * In Javascript the indexation is done with char position, not in byte,
 * so this function is the identity on the client side
**/
##register nth : string, int -> int
##args(_, pos)
{
    return pos
}

/**
 * Given the index used in Cactutf strings, returns the number of Unicode characters
 * In javascript, the indexation uses directly the number of Unicode characters, this function is the identity
**/
##register length_until : string, int -> int
##args(_, pos)
{
    return pos
}

##register next : string, int -> int
##args(s, i)
{
    return i + 1;
}

##register sub : string, int, int -> string
##args(s, i, j)
{
        return s.substring(i, i + j);
}


##register sub_opt : string, int, int -> option(string)
##args(s, i, j)
{
    if( ( i>= 0) || i+j < s.length) return js_some(s.substring(i, i + j))
    else return js_none
}

##register get : string, int -> int
##args(s, i)
{
    return s.charCodeAt(i);
}

##register look : string, int -> int
##args(s, i)
{
    return s.charCodeAt(i);
}

##register cons : int -> string
##args(i)
{
    return String.fromCharCode(i);
}

##register uppercase : string -> string
##args(s)
{
    return s.toUpperCase();
}

##register lowercase : string -> string
##args(s)
{
    return s.toLowerCase();
}

// All the following are copied from libqml/libqml/cactutf.ml
##register lenbytes : int -> option(int)
##args(i)
{
    if (i < 128) {
        return js_some(1);
    } else if (i < 192) {
        return js_none;
    } else if (i < 224) {
        return js_some(2);
    } else if (i < 240) {
        return js_some(3);
    } else
        return js_some(4)
}

##register one_byte : int -> int
##args(b1)
{
    return b1;
}

##register two_bytes : int, int -> int
    ##args(b1,b2)
{
    return (((b1 - 192) * 64) + (b2 - 128));
}

##register three_bytes : int, int, int -> int
    ##args(b1,b2,b3)
{
    return (((b1 - 224) * 4096) + ((b2 - 128) * 64) + (b3 - 128));
}

##register four_bytes : int, int, int, int -> int
##args(b1,b2,b3,b4)
{
    return (((b1 - 240) * 262144) + ((b2 - 128) * 4096) + ((b3 - 128) * 64) + (b4 - 128));
}
