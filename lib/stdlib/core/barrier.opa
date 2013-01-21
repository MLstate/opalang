/*
    Copyright © 2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

type Barrier.t('a) = external

/**
 * @category concurrency
 * @author Quentin Bourgerie
 * @destination public
 */
Barrier = {{

  /**
   * Create an empty barrier.
   */
  @expand
  make() = %%BslCps.u_make_barrier%%(__POSITION__)

  /**
   * Wait that the [barrier] is released then returns its value.
   * @param barrier The barrier to wait.
   * @return The value of the barrier
   */
  wait(barrier) = %%BslCps.u_wait%%(barrier)

  /**
   * Release the [barrier] and set its [value].
   * @param barrier The barrier to release.
   * @param value The value to fill the barrier.
   */
  release(barrier, value) = %%BslCps.u_release_barrier%%(barrier, value)

}}
