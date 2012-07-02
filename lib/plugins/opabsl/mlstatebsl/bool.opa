/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
