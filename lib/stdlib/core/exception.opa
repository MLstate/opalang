/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
