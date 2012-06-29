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
unary_minus = %%Bslpervasives.int_neg%%

true = {true} : bool

false = {false} : bool

True = {true} : bool

False = {false} : bool

bool =
  cmp = %%BslPervasives.compare_raw%%
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
    _and(x, y):bool =
      match x with
      | {true} ->
        match y with
        | {true} -> {true}
        | _ -> {false}
        end
      | _ -> {false}
    _or(x, y):bool =
      match x with
      | {true} -> {true}
      | _ ->
        match y with
         | {true} -> {true}
         | _ -> {false}
    not =
      | {true} -> {false} :bool
      | {false} -> {true} :bool

    eq(x,y):bool = match cmp(x,y) with | 0 -> {true}
              | _ -> {false}

    lt(x,y):bool = match cmp(x,y) with | -1 -> {true}
              | _ -> {false}

    gt(x,y):bool = match cmp(x,y) with | 1 -> {true}
              | _ -> {false}

    le(x,y):bool = match cmp(x,y) | -1 | 0 -> {true}
              | _ -> {false}

    ge(x,y):bool = match cmp(x,y) | 1 | 0 -> {true}
              | _ -> {false}

    to = to;
    make = make
  }
