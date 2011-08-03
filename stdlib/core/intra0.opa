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


// to use only with small list
sc_list( la ) =
 match (la : list(string)) with
 | [s | tl] -> sc(s, sc_list(tl) )
 | [] -> ""

@deprecated({hint="Use Dom.fresh_id instead of uniq"})
uniq() = Random.string(32)
