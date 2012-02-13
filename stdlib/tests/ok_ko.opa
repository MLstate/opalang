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

OK = {{

  ok_ko(mess:string, b )=
    prerr("ok_ko : {
      if b
      then "OK"
      else "KO - Fail"
     }: {mess}\t ##ok_ko\n")

  ok(mess) = ok_ko(mess, true)
  fail(mess) = ok_ko(mess, false)

  check_equal_var(mess, x_string, x, y) =
    if x == y then ok(mess)
    else fail("{mess}: {x_string} should be {y}, it is {x}")

  check_equal(mess, x, y) =
    if x == y then ok(mess)
    else fail("{mess}: expected {y}, got {x}")

}}
