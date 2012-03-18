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
 * {1 Types defined in this module}
 */

/**
 * The type of mutable element.
 * @param 'a The type contained in the element.
 * get returns the content of the mutable element.
 * set changes the content of the mutable element.
 */
type Mutable.t('a) = {get: -> 'a; set: 'a -> void}

/**
 * {1 About this module}
 *
 * It provides a way to have a mutable state.
 *
 * e.g.
 *
 * state = Mutable.create("initial state content")
 * println(state.get())
 * state.set("new state content")
 *
 */
Mutable =
{{
   make(init:'a): Mutable.t('a) =
        r = Reference.create(init)
        {{
           get() = Reference.get(r)
           set(x)= Reference.set(r, x)
        }}

}}

@opacapi
Mutable_make = Mutable.make

@opacapi Mutable_set(mutable,v) : Mutable.t('a) = mutable.set(v)
