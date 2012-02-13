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
 * {[
 *   @deprecated({use="bar"}) foo = bar
 * }
 *
 * {[
 *   @deprecated({hint="Please refactor your code, this function does no longer exists"}) foo = ...
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
    * will produce a warning around the utilization of the deprec_fun, telling :
    * {This code uses a deprecated construction. Please use "this_function" instead}
   **/
   { use : string }

   /**
    * This argument is used if there is no new function to use instead.
    * The string is them a comment, giving an hint to explain
   **/
 / { hint : string }
