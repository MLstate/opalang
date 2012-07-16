/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

//////////////////////////////////////////////////////////////////////
// BEWARE THIS FILE IS SHARING BEETWEEN THE JAVASCRIPT AND NODE BSL //
//////////////////////////////////////////////////////////////////////

/**
 * Management of low-level references.
 *
 * @deprecated References cannot be serialized and are generally Evil (tm). You should
 * rather use [Cell]
 */
##extern-type reference('a)

/**
 * Create a reference with a given initial value
 */
##register [pure] create : 'a -> reference('a)
##args(a)
{
  return [a];
}

/**
* Return the latest value of a reference
*/
##register get : reference('a) -> 'a
##args(a)
{
  return a[0];
}

/**
* Change the value of the reference
*/
##register set : reference('a), 'a -> void
##args(r, v)
{
   r[0] = v;
   return js_void;
}

/**
   Atomic compare-and-swap, based on physical equality

   @param latest The value expected to be held by [r]
   @param replacement The value to put in the content of [r], iff it still contains [latest].
   @return {boolean} [true] if the value of [r] was physically equal to [latest], in which case the new value of [r] is now set to [replacement] / [false] otherwise
*/
##register compare_and_swap: reference('a),'a,'a -> bool
##args(r, latest, replacement)
{
    if(r[0] === latest)
    {
        r[0] = replacement;
        return true;
    } else {
        return false;
    }
}
