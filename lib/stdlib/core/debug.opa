/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Debugging utilities
 *
 * @author David Rajchenbach-Teller, 2010 (documentation)
 * @destination public
 * @stability stabilizing
 */

/**
 * {1 About this module}

 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Interface}
 */

Debug = {{

@deprecated({use="the directive @fail (or Log.error if you do not want to fail)"})
error(s) = @fail(s) :'a

/**
 * Print a message and continue.
 *
 * If this function is executed on the client, the message will be displayed in a side window.
 * On the server, the message will be displayed on the console.
 */
jlog = %% Bslpervasives.jlog %%

/**
 * Get a string representation of any value
 */
dump = %% Bslpervasives.dump %% : 'a -> string

/**
 * Print a message and continue, in some conditions.
 *
 * A variant of {!Debug.jlog} that prints a message only of some condition is verified.
 */
jlog_if(b: bool, msg: -> string) = if b then jlog(msg()) else void

/**
 * As {!error}, but also displays the current local stack
 */
error_with_stack(mess: string)=
        @fail(  "ERROR = {mess:string}"
                ^ "Stack =\n {Continuation.get_trace({current})}" )

/**
 * As {!Debug.jlog}, but also displays the current local stack
 */
jlog_with_stack(mess: string)=
        jlog(  "ERROR = {mess:string}"
               ^ "Stack =\n {Continuation.get_trace({current})}" )

/**
 * Return a developer-readable printout of the local stack.
 *
 * This only shows the local stack, not the full client-to-server or server-to-client stack.
 */
@deprecated({use="Continuation.get_trace"}) get_stack = %% Bslpervasives.get_stack %%

/**
 * Deprecated
**/
@deprecated({use="do @assert(cond) instead of assert(cond, fun)"})
assert(condition:bool, message:-> string) =
   if condition then void
   else error("Assertion failed: {message()}")

}}

@deprecated({use="do @assert(cond) instead of assert(cond, fun)"})
assert = Debug.assert
