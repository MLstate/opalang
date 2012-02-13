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

/**
 * TestSuite, Tools for unit testing (with external scripts that catch "TEST KO", etc.)
 *
**/

/**
 * {1 About this module}
 *
 * API used in unit tests
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
**/

/**
 * {1 Types defined in this module}
 */

@abstract type TestSuite.t = {
  list : list(-> void) ;
  title : string ;
}

/**
 * {1 Interface}
 */

TestSuite = {{

  barsize = 70

  bar = String.repeat(barsize, "#")

  empty : TestSuite.t =
    {list = []; title = "TestSuite"}

  create(title) : TestSuite.t =
    title = "### LAUNCH : " ^ title
    title = i = (barsize-1) - String.length(title)
      if i > 0 then
        title ^ " " ^ String.repeat(i, "#")
      else title
    {list = []; title = title}

  add(suite:TestSuite.t, test) : TestSuite.t =
    {suite with list=test +> suite.list}

  run(suite:TestSuite.t) =
    do prerr(bar ^ "\n")
    do prerr(suite.title ^ "\n")
    List.fold_backwards((f, _ -> f()), suite.list, {})

}}
