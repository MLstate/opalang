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

/**
 * <!> Built in [BslNativeLib.ml]
**/
@opacapi
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

// This is a hack for preserving the type of exceptions
@private @server_private _dead_code() =
  match 0 with
  | 1 -> @throw( @opensums( { fail = "" ; position = "" } : Exception.common ) )
  | 2 -> @throw( @opensums( { Transaction_failure } : Exception.common ) )
  | 3 -> @throw( @opensums( { ocaml_exc = "" ; bslkey = "" } : Exception.common ) )
  | _ -> void
