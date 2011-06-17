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
 * Common Opa Exceptions
 *
 * @destination public
 * @stabilization work in progress
**/

/**
 * {1 About this module}
 *
 * Opa exception are used internally by generated code to report error,
 * but it is discouraged to use them in a standard code.
 *
 * A next version of Opa will have a support for typing and extending exceptions.
 * Currently, exceptions are available only on server side, and are all unified
 * as there were inhabitant of the same sum type, as long as the compiler find
 * occurrences of @catch and @throw in the code.
 *
 * This module contains the definition of the most common exceptions.
 *
 * Raised on the server side, this will either executed the handler code if there
 * is any, or print an uncaught Opa exception message error.
 *
 * Raised on the client side, this will end up with an uncaught Javascript OpaException error.
 * For rpc the client Opa exception should contain a JSon representation of the corresponding
 * Opa exception.
 *
 * The syntax is the same as a [match with], with the [try catch] keywords instead.
 *
 * {[
 * try
 *   expr
 * catch
 *   | { e } ->
 * }
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
**/

type Exception.common =
   { fail : string ; position : string }
   /**
    * Raised on the server side when it executes a [@fail] directive
   **/

 / { Transaction_failure }
   /**
    * Internal error of the database. Not meant to be caught by an user.
   **/

 / { ocaml_exc : string ; bslkey : string }
   /**
    * An ocaml exception raised by a server bypass.
    * If the bypass is tagged as [raise], the exception is serialized
    * using [Printexc.to_string], and the [bslkey] of the bypass is
    * stored in the Opa exception.
   **/
