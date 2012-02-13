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
 * Management of low-level references.
 *
 * @deprecated References cannot be serialized and are generally Evil (tm). You should
 * rather use [Cell]
 */
##extern-type reference('a)

/**
 * Create a reference with a given initial value
 */
##register create : 'a -> reference('a)
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
