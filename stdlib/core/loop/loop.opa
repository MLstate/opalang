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

import stdlib.core.{map,set}

Map_ = {{
    list(l)(f) = List.map(f,l)
    map(m)(f) = Map.map(f,m)
    set(s)(f) = Set.map(f,s)
}}

Mapi = {{
    list(l)(f) = List.mapi(f,l)
}}

Fold = {{
    list(l,acc)(f) = List.fold(f,l,acc)
    map(m,acc)(f) = Map.fold(f,m,acc)
    set(s,acc)(f) = Set.fold(f,s,acc)
}}
