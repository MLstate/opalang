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
 * Debugging utilities
 *
 * @author David Rajchenbach-Teller, 2010 (documentation)
 * @destination public
 * @stability stabilizing
 */

Debug = {{

@deprecated({use="the directive @fail"})
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
 * Quit if some condition is false.
**/
@deprecated({hint="This function will be replaced by a compiler directive"})
assert(condition:bool, message:-> string) =
   if condition then void
   else error("Assertion failed: {message()}")

}}

@deprecated({hint="This function will be replaced by a compiler directive"})
assert = Debug.assert
