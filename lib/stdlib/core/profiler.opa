/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
