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
/**
 * TESTS: ok_ko
 *
 * @author ?
**/


/**
 * {1 About this module}
 *
 * API used in unit tests
**/

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
