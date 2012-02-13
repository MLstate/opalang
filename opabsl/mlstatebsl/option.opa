/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
