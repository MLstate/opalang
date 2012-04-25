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
