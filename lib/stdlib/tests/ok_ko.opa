/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * TESTS: ok_ko
 *
 * @author ?
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
 * {1 Interface}
 */

@server OK = {{

  ok_ko(mess:string, b )=
    prerrln("ok_ko : {
      if b
      then "OK"
      else "KO - Fail"
     }: {mess}\t ##ok_ko")

  ok(mess) = ok_ko(mess, true)
  fail(mess) = ok_ko(mess, false)

  check_equal_var(mess, x_string, x, y) =
    if x == y then ok(mess)
    else fail("{mess}: {x_string} should be {y}, it is {x}")

  check_equal(mess, x, y) =
    if x == y then ok(mess)
    else fail("{mess}: expected {y}, got {x}")

}}
