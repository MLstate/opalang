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

CoreProfiler = {{

  /** [measure(n)(f)] returns a pair: f() and the float representing the number of
      seconds needed to execute f, n times. The timing information should have
      better than millisecond precision, hence making it suitable for execution
      profiling. */
  measure(n)(f)=
    get_time = %%BslTime.get_accurate_time%% :  -> float
    t_beg = get_time()
    rec aux(n, v) =
      if n == 0 then
        t_end = get_time()
        (t_end - t_beg, v)
      else
        aux(n-1, f())
     aux(n-1, f())

  /** [instrument(n,report)(f)] returns the value of f() and call report with the time needed to execute f, n times */
  instrument(n, report)(f)=
    (t, r) = measure(n)(f)
    do report(t) : void
    r

}}
