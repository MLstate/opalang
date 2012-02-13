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
 * Print a warning and continue.
 */
warning = %% Bslpervasives.warning %%

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
              ^ "Stack =\n {Continuation.print_trace()}" )

/**
 * As {!Debug.jlog}, but also displays the current local stack
 */
jlog_with_stack(mess: string)=
        jlog(  "ERROR = {mess:string}"
             ^ "Stack =\n {Continuation.print_trace()}" )

/**
 * Return a developer-readable printout of the local stack.
 *
 * This only shows the local stack, not the full client-to-server or server-to-client stack.
 */
@deprecated({use="Continuation.print_trace"}) get_stack = %% Bslpervasives.get_stack %%

/**
 * As [warning].
 */
alert(s)=warning(s)

/**
 * Flush all outputs.
 */
flush_all = %% Bslpervasives.flush_all %%

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
