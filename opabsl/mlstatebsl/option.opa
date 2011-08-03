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
none = {none} : option('v1)

some(t) = { some = t } : option

None = {none} : option('v2)

Some(t) = { some = t } : option

// All fields are used
option =
  make =
    none = {none} : option
    some(o) = { some = o } : option
    { none = none; some = some }
  to =
    sc = %%BslString.concat%%
    string(f, x) =
      match x : option with
      | {none = none} -> "\{ none = () \} : option"
      | {some = some} -> sc("\{ some = ", (sc((f(some)), " \} : option")))
      end
    { string = string }
  default(d, o) =
    match o : option with
    | {none = none} -> d
    | {some = some} -> some
    end
  get(o) =
    match o : option with
    | {some = some} -> some
    | {none = none} ->
      @fail("this option don't have any some field !.")
    end
  map(f, o) =
    match o : option with
    | {none = none} -> {none}
    | {some = some} -> { some = f(some) }
    end
  { to = to; default = default; make = make; get = get; map = map }
