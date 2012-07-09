/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Arguments for tagging part of the code as deprecated.
**/

/**
 * {1 About this module}
 *
 * This module defines the type of the argument of the directive @deprecated
 *
 * These arguments are read at compile time, and should use literal only.
 * @see pass_CodingDirectives
 * @see opa parser
 *
 * {1 Where should I start?}
 *
 * Here are some example of use:
 *
 * A deprecation directive that specifies the version of the code when
 * the function became deprecated and suggests another one to be used
 * instead:
 *
 * {[
 *   @deprecated({use="bar", version=some("1.2.3")}) foo = bar
 * }
 *
 * A deprecation directive that does not specify the version and
 * provides a textual hint about the deprecated code:
 *
 * {[
 *   @deprecated({hint="Please refactor your code, this function does no longer exists", version=none}) foo = ...
 * }
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

/**
 * Argument of the @deprecated directive
 * You cannot modify this code without patching the pass_CodingDirectives.
**/
@opacapi
type Deprecated.argument =
   /**
    * {[
    * @deprecated({use="this_function"}) deprec_fun() = void
    * do deprec_fun()
    * }
    * will produce a warning around the use of the deprec_fun, saying something like:
    * {This code uses a deprecated construction. Please use "this_function" instead}
   **/
   { use : string }
 / { use : string; version : string }

   /**
    * This argument is used if there is no new function to use instead.
    * The string is just a hint to be given to the user, concerning the deprecated
    * code.
   **/
 / { hint : string }
 / { hint : string; version : string }
