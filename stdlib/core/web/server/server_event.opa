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

/*
    @Author: David Rajchenbach-Teller, 2010
**/


/**
 * {1 About this module}
 *
 * Server-side system event management.
 *
 * Use this module to post information on non-fatal server-side error, disconnection error, resource temporarily not available, etc.
 * or to catch these events.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

type ServerEvent.event =
  {server_event_uri_request:string}
/ {server_event_uri_dispatch: string; category: {user_defined}/{about_box}/{generated}/{non_existent}/{non_existent_internal} }
/ {server_event_db_error}

type ServerEvent.info =
  {date:   Date.date
   context:ThreadContext.t
   event:  ServerEvent.event}

/**
 * {1 Interface}
 */

@server ServerEvent = {{

  /**
   * Register a new handler for an event.
   *
   * Note: If the handler needs to be called while the server is disconnected, make sure that the full implementation
   * is server-side.
   *
   * Note: Type [ServerEvent.event] will change. Always include a do-nothing catch-all case in your sessions. If
   * you unregister the default_handler, you may with to add a fallback case to trigger it for new kinds of events
   * that you don't know how to handle.
   */
  register_handler(handler:  channel(ServerEvent.info)): void = Network.add(handler, network)
  unregister_handler(handler:channel(ServerEvent.info)): void = Network.remove(handler, network)

  /*
   * Note: It's provided so that you can unregister it.
   */
  default_handler: channel(ServerEvent.info) =
    Session.make_callback({date=_ context=_ ~event} ->
      match event with
      | ~{server_event_uri_request} ->
           Log.info("Dispatch", "Client has requested URI: {server_event_uri_request}")
      | ~{server_event_uri_dispatch category} ->
           (level, human_readable) =
             match category with
               | {non_existent_internal} -> (Log.warning, "a non-existent internal URI")
               | {user_defined}          -> (Log.info,    "a user-defined resource")
               | {about_box}             -> (Log.info,    "the about box")
               | {generated}             -> (Log.info,    "generated code")
               | {non_existent}          -> (Log.info,    "an incorrect URI")
           level("Dispatch", "URI {server_event_uri_dispatch} is {human_readable}")
       | {server_event_db_error} ->
           Log.warning("Database", "URI error detected")
    )

  /**
   * Send an event.
   *
   * Events are treated asynchronously, by whichever event handlers have been registered
   */
  trigger(event: ServerEvent.event): void =
    do Network.broadcast({date    = Date.now()
                          context = thread_context()
                          ~event}, network)
    void


/**
 * {1 Private section}
 */

  @private network: Network.network(ServerEvent.info) = Network.empty()// Coercion seems necessary to prevent explicit instanciation from adding several times to the network

   @private static_init =
     do Network.add(default_handler, network)
     init = %% BslServer_event.init %%
     do init(trigger) //Register the network so that BSL functions can trigger errors
     void
}}
