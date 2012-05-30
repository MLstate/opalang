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
 * Defining local_thread_context.
 *
 * This file provides the basic definitions of thread_context, and define
 * 2 primitives to access/set the current context.
 *
 * @category web
 * @author Mathieu Barbin, 2010
 * @author David Rajchenbach-Teller, 2010
 * @author Raja Boujbel, 2010
 * @author Quentin Bourgerie, 2010
 * @destination public
 * @stability stabilizing
 */

import stdlib.core.{web.core}

/**
 * {1 About this module}
 *
 * At any position in the code, the execution must know on behalf of which user it
 * is taking place. On the client, we consider that, trivially, this is on behalf of the
 * current client. On the server, we consider that this can be either on behalf of the
 * user who initiated the request which, up-to session calls and thread management,
 * lead to this position in the code, or on behalf of the server itself (e.g. if the server
 * performs regular clean-up or is still initializing).
 *
 * From the point of view of the developer, thread-local context can be accessed
 * by a primitive thread_context, which returns a record of type [ThreadContext.t]
 *
 * On the client, all fields are defined at initialization time and are imple-
 * mented as global variables. On the server, all fields are defined when a request
 * starts and propagated through the code as part of the CPS context and as part of
 * session-specific calls.
 *
 * The casual user should not use directives @thread_context/@with_thread_context himself
 * @with_thread_context is inserted in the url dispatcher, while thread_context is used by the user.
 * The directive @thread_context returns an option, and the primitive provide a default value.
 */

/**
 * {1 Types defined in this module}
 */

type ThreadContext.client = {
  client : string/**The (cookie) identity of the client */;
  page:   int   /**An identifier serving to differentiate several pages being
                   accessed simultaneously by the same client*/;
}

type ThreadContext.server = external

type ThreadContext.key =
  {server : ThreadContext.server} /
  {client : ThreadContext.client} /
  {nothing}

type ThreadContext.constraint = {free} / {no_client_calls}

/**
 * The type of the thread context
 */
@opacapi
type ThreadContext.t =
  {
    key: ThreadContext.key/**Information required to communicate with the client*/
    request: option(HttpRequest.request) /** The full HTTP request. */
    constraint : ThreadContext.constraint
    details:
    option({
        locale:  list(string) /** A list of locales, e.g. "en-US". Be careful: on the client, it's a singleton, while on the server it can contain several items, by order of priority. */
        browser: user_compat  /** Information on the browser used to display the page */
    })
  }

/**
 * An ordering on thread contexts
 */
@abstract type ThreadContext.private.order = void

/**
 * {1 Access to thread context}
 */

ThreadContext = {{
  // /**
  //  * Create a thread context with [client] and [page] identifier.
  //  */
  // create(key, request, details) =
  //   {
  //     key=key request=some(request) details=some(details)
  //   }

  // /**
  //  * Create a thread context with optional [client] and [page]
  //  * identifier.
  //  */
  // create_opt(key, request, details) =
  //   {
  //     ~key ~request ~details
  //   }

  /**
   * The default thread context. All fields are [none].
   */
  default =
    {
      key     = {nothing} ;
      request = none ;
      constraint = {free};
      details = none ;
    } : ThreadContext.t

  /**
   * Get the thread context. Return [none] if context is unbound.
   */
  get_opt =
    | {current} -> @thread_context
    | {from = k} ->
      aux = %%BslCps.thread_context%%
      aux(k)
  : option(ThreadContext.t)

  /**
   * Get the thread context. Returns a default context if context is
   * unbound.
   */
  get(select) =
    match get_opt(select) with
    | { none } -> default
    | { ~some } -> some
  : ThreadContext.t

  no_client_calls() : ThreadContext.t =
    {ThreadContext.get({current}) with constraint={no_client_calls}}


  Client = {{

    /**
     * Returns the current client context or [none] if the client context is not
     * found.
     */
    get_opt(select) =
      match ThreadContext.get_opt(select) with
      | {some = {key = ~{client} ...}} -> some(client)
      | _ -> none

    /**
     * If you use it, it means you don't need the documentation.
     */
    fake = {client="_internal_" page=-1}
    // use when you need a ThreadContext.client that you can't get
    // only here for avoiding duplication of code
    // TODO: remove all use

    /**
     * Current thread context with the given ThreadContext.client key
     */
    using(client_key) = {get({current}) with key={client=client_key}}

  }}
}}

/**
 * {1 Functions exported to global namespace}
 */

/**
 * Return the current thread context.
 */
thread_context() = ThreadContext.get({current})
@opacapi ThreadContext_no_client_calls = ThreadContext.no_client_calls
