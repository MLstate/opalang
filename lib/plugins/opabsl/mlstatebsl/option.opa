/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
