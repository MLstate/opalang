/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
