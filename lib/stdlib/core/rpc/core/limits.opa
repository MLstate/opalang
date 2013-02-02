/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * {1 About this module}
 *
 *  It contains several known implementation specific limits.
 *
 */

Limits = {{
  /**
   * The largest integer usable in OPA
   *
   * Note that the size of integers is dictated both by server limitations (64-bit servers have larger integers than 32-bit servers)
   * and by client limitations (JavaScript implementations are typically limited to 53-bit integers).
   */
  max_int = server_max_int

  /**
   * The smallest integer usable in OPA
   *
   * Note that the size of integers is dictated both by server limitations (64-bit servers have larger integers than 32-bit servers)
   * and by client limitations (JavaScript implementations are typically limited to 53-bit integers).
   */
  min_int = -max_int

  @private
  @publish
  server_max_int = %% BslNumber.Int.max_int %% : int
}}
