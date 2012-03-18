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
    @author Francois Pessaux
*/

import stdlib.core.date

/**
  * This file provides the primitives to bind client events to callbacks.
  * This is especially important in order to keep trace of clients
  * disconnections in order to free data-structures related to dead clients
  * and avoid memory leaks.
  * The usage principle is, when resources are allocated, a callbac must be
  * registered frying these resources. Once the client dies, these disallocation
  * callbacks get called automatically (after around 1 minute).
  */



@abstract type ClientEvent.t = external
@abstract type ClientEventKey.t = external

ClientEvent = {{
  /**
   * Event value representing a client connection.
   */
  connect = %% BslClientEvent.connect %% : ClientEvent.t



  /**
   * Event value representing a client disconnection.
   */
  disconnect = %% BslClientEvent.disconnect %% : ClientEvent.t

  /**
   * Event value representing a client inactivity.
   */
  inactive = %% BslClientEvent.inactive %% : ClientEvent.t

  /**
   * Register (binds) a function (callback) to call when an event occurs for a
   * particular client or any client.
   * Returns the id of the event registration in order to be able to remove the
   * binding of this event to this function if needed.
   *
   * @param opt_client The id of the client interested to have the function
            called when the event arises or {!none} if all clients are
            interested in.
   * @param event The event that must trigger the callback execution.
   * @param callback The function called when the event arises.
   * @return The id of the binding event/callback registration.
   */
  register_event : option(ThreadContext.client),
                   ClientEvent.t,
                   (ThreadContext.client -> void) -> ClientEventKey.t =
   %% BslClientEvent.register_event %%



  /**
   * Register (binds) a function (callback) to call when an event occurs for the
   * client bound to the current thread context. If the current thread context
   * has no client, an error is raised.
   * Returns the id of the event registration in order to be able to remove the
   * binding of this event to this function if needed.
   *
   * @param event The event that must trigger the callback execution.
   * @param callback The function called when the event arises.
   * @return The id of the binding event/callback registration.
   */
  register_client_event(event : ClientEvent.t,
                        callback : (ThreadContext.client -> void))
                       : ClientEventKey.t =
    match ThreadContext.get({current}).key with
    | { client = thread_ctxt_client } ->
        register_event({ some = thread_ctxt_client }, event, callback)
    | _ ->
       @fail("register_client_event: no client in the current context.")



  /**
   * Unregister the binding event/callback represented by the provided binding
   * id. This means that the function registered for the event represented by
   * the binding id won't be called anymore when the event arises.
   *
   * @param binding_id The id of the binding event/callback to remove.
   */
  remove_event : ClientEventKey.t -> void = %% BslClientEvent.remove_event %%




  /**
   * Register (binds) a function (callback) to call when the client of the
   * current thread context is disconnected. If the current thread context has
   * no client, an error is raised.
   * Returns the id of the event registration in order to be able to remove the
   * binding of the disconnection event to this function if needed.
   *
   * @param callback The function called when the disconnection event arises.
   * @return The id of the binding event/callback registration.
   */
  set_on_disconnect_client(callback : (ThreadContext.client -> void))
                          : ClientEventKey.t =
    register_client_event(disconnect, callback)

  /**
   * Register (binds) a function (callback) to call when the client of the
   * current thread context is inactive. If the current thread context has
   * no client, an error is raised.
   * Returns the id of the event registration in order to be able to remove the
   * binding of the inativity event to this function if needed.
   * An inactive event is raised if there is no communication between client and server for a while.
   * In this case the ping loop isn't considered as a communication.
   *
   * @param callback The function called when the inactive event arises.
   * @return The id of the binding event/callback registration.
   */
  set_on_inactive_client(callback : (ThreadContext.client -> void))
                          : ClientEventKey.t =
    register_client_event(inactive, callback)

  /**
   * Set the duration before raising an "inactive" event.
   *
   * @param opt_client The id of the client affected by this setting or {!none} if global setting.
   * @param delay The duration before raising an "inactive" event.
   * @return void.
   */
  set_inactivity_delay(ctx : option(ThreadContext.client), delay : Duration.duration) : void =
    time = Duration.in_milliseconds(delay)
    (%% BslClientEvent.set_inactive_delay %%)(ctx,some(time))

  /**
   * Remove the duration before raising an "inactive" event.
   *
   * @param opt_client The id of the client affected by this setting or {!none} if global setting.
   * @return void.
   */
  remove_inactivity_delay(ctx : option(ThreadContext.client)) : void =
    (%% BslClientEvent.set_inactive_delay %%)(ctx,none)

  // Commented because we miss some more feature to handle disconnection client side

  // /**
  //  * Disconnect the client corresponding to the ThreadContext.client
  //  *
  //  * @param ctx The id of the client to disconnect.
  //  * @return void.
  //  */
  // disconnect_client(ctx : ThreadContext.client) : void =
  //   (%%BslPingRegister.client_stop%%)(ctx)

  // /**
  //  * Disconnect the current client. If the current thread context
  //  * has no client, an error is raised.
  //  *
  //  * @return void.
  //  */
  // disconnect_current() : void =
  //   match ThreadContext.get({current}).key with
  //   | { client = thread_ctxt_client } ->
  //       disconnect_client(thread_ctxt_client)
  //   | _ ->
  //      @fail("disconnect_current: no client in the current context.")

}}
