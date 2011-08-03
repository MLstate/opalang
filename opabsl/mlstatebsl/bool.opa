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
true = {true} : bool

false = {false} : bool

True = {true} : bool

False = {false} : bool

bool =
  ##include functions bool
  make =
    _true = {true} : bool
    _false = {false} : bool
    { true = _true; false = _false }
  to =
    string =
      b ->
        match b : bool with
        | {true = true} -> "true"
        | {false = false} -> "false"
        end
    { string = string }
  {
    ##include bind bool
    to = to; 
    make = make 
  }
