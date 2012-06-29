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
type void = {}
type bool =  {false : {}} / {true : {}}
type option('v0) = {none : {}} / {some : 'v0}
type list('v0) = {hd : 'v0; tl : list('v0)} / {nil : {}}
type pair('v0, 'v1) = {fst : 'v0; snd : 'v1}
type tuple_2('a, 'b) = { f1 : 'a ; f2 : 'b }
