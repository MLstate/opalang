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

type Mutable.t('a) = {get: -> 'a; set: 'a -> void}
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
