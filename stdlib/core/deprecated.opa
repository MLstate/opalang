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
