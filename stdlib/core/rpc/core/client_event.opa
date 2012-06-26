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
import stdlib.core.date
#<Ifstatic:OPA_BACKEND_QMLJS>

type ClientEvent.t =
  {connect}
/ {disconnect}
/ {inactive}

type ClientEvent.callback = ThreadContext.client -> void

@abstract
type ClientEvent.key = {client:ThreadContext.client ckey:int}

@private
type PingRegister.state = {
  ina_delay:reference(int)
  ina_key:reference(option(Scheduler.key))
  dis_delay:reference(int)
  dis_key:reference(option(Scheduler.key))
  fresh:reference(int)
  callbacks:Hashtbl.t(int, {kind:ClientEvent.t cb:ClientEvent.callback})
}

@server_private
ClientEvent = {{

  /*
   * {3 Private utils}
   */

  @private
  debug = Log.debug("CLIENTEVENT", _)

  @private
  error = Log.error("CLIENTEVENT", _)

  /**
   * The table which store the state of client connection
   */
  @private
  state_tbl : Hashtbl.t(ThreadContext.client, PingRegister.state) =
    PingRegister.Utils.make_tctbl()

  /**
   * Default delay before raise the disconnect event
   */
  @private
  disconnect_delay : reference(int) =
    Reference.create(120 * 1000)

  /**
   * Default delay before raise the disconnect event
   */
  @private
  inactive_delay : reference(option(int)) =
    Reference.create(none)

  @private
  raise_event(client, event, binds) =
    aux(binds) =
      LowLevelArray.iter(
        {key=_ value=~{kind cb}} ->
          if kind == event then Scheduler.push(-> cb(client))
        , binds)
    do aux(binds)
    match Hashtbl.try_find(state_tbl, ThreadContext.Client.fake)
    | {none} -> void
    | {some = state} -> aux(Hashtbl.bindings(state.callbacks))

  /* The fake client is used for to store default handlers */
  @private
  _add_fake = Hashtbl.add(state_tbl, ThreadContext.Client.fake, {
      ina_delay = Reference.create(-1)
      ina_key   = Reference.create({none})
      dis_delay = Reference.create(-1)
      dis_key   = Reference.create({none})
      callbacks = Hashtbl.create(5)
      fresh     = Reference.create(0)
    })

  /**
   * Event value representing a client connection.
   */
  connect = {connect} : ClientEvent.t



  /**
   * Event value representing a client disconnection.
   */
  disconnect = {disconnect} : ClientEvent.t

  /**
   * Event value representing a client inactivity.
   */
  inactive = {inactive} : ClientEvent.t

  /**
   * {3 Callbacks registering}
   */

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
  register_event(
    client:option(ThreadContext.client),
    event:ClientEvent.t,
    callback:(ClientEvent.callback)
  ):ClientEvent.key =
    client = match client with
      | {none} -> ThreadContext.Client.fake
      | {some = client} -> client
    match Hashtbl.try_find(state_tbl, client)
    | {none} ->
      do error("Event({client}) No register callback event, client is not present")
      @fail
    | {some = state} ->
      @atomic(
        ckey = Reference.get(state.fresh)
        do Reference.set(state.fresh, ckey + 1)
        do Hashtbl.add(state.callbacks, ckey, {kind=event cb=callback})
        ~{ckey client}
      )

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
  register_client_event(
    event : ClientEvent.t,
    callback : (ClientEvent.callback)
  ):ClientEvent.key =
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
  remove_event(key:ClientEvent.key) =
    @atomic(match Hashtbl.try_find(state_tbl, key.client) with
      | {none} -> void
      | {some = state} ->
        Hashtbl.remove(state.callbacks, key.ckey))


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
                          : ClientEvent.key =
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
  set_on_inactive_client(callback : (ThreadContext.client -> void)): ClientEvent.key =
    register_client_event(inactive, callback)

  /**
   * {3 Configure timeouts}
   */

  @private
  aux_set_inactivity_delay(ctx, time) =
    match ctx with
    | {none} -> Reference.set(inactive_delay, time)
    | {some=client} ->
      match Hashtbl.try_find(state_tbl, client) with
      | {none} -> void
      | {some = state} -> Reference.set(state.ina_delay, time ? 0)

  /**
   * Set the duration before raising an "inactive" event.
   *
   * @param opt_client The id of the client affected by this setting or {!none} if global setting.
   * @param delay The duration before raising an "inactive" event.
   * @return void.
   */
  set_inactivity_delay(ctx : option(ThreadContext.client), delay : Duration.duration) : void =
    time = Duration.in_milliseconds(delay)
    aux_set_inactivity_delay(ctx, some(time))

  /**
   * Remove the duration before raising an "inactive" event.
   *
   * @param opt_client The id of the client affected by this setting or {!none} if global setting.
   * @return void.
   */
  remove_inactivity_delay(ctx : option(ThreadContext.client)) : void =
    aux_set_inactivity_delay(ctx, none)

  /**
   * {3 Manage the connection state}
   */

  /**
   * Remove the connection state of a client.
   *
   * @param client The id of client to remove.
   * @return void
   */
  remove(client:ThreadContext.client) =
    #<Ifstatic:MLSTATE_PING_DEBUG>
    do debug("Event({client}) Remove client state")
    #<End>
    match @atomic(match Hashtbl.try_find(state_tbl, client) with
      | {none} -> {}
      | {some = _} as e ->
        do Hashtbl.remove(state_tbl, client)
        e)
    with
    | {} -> void
    | {some = state} ->
      do raise_event(client, {disconnect}, Hashtbl.bindings(state.callbacks))
      void

  /**

   * Create or update the state of the given client. Delay the pending
   * disconnect timeout. And depending of the [active] parameter delay the
   * inactivity timeout.
   *
   * @param client The id of client to touch.
   * @param active Indicates if the inactive timeout should be delayed.
   * @return void
   */
  touch(client:ThreadContext.client, active) =
    #<Ifstatic:MLSTATE_PING_DEBUG>
    do debug("Event({client}) Touch client state")
    #<End>
    ~{delay ref iref idelay} =
      @atomic(match Hashtbl.try_find(state_tbl, client) with
        | {some = state} ->
          aux(ref) =
            match Reference.get(ref) with
              | {none} -> void
              | {some = k} ->
               do Scheduler.abort(k)
               do Reference.set(ref, none)
               void
          do aux(state.dis_key)
          do if active then aux(state.ina_key)
          {delay = Reference.get(state.dis_delay)  ref = state.dis_key
           idelay = Reference.get(state.ina_delay) iref = state.ina_key}
        | {none} ->
          state = {
            dis_delay = Reference.create(Reference.get(disconnect_delay))
            ina_delay = Reference.create(Reference.get(disconnect_delay))
            callbacks = Hashtbl.create(5)
            fresh     = Reference.create(0)
            dis_key   = Reference.create({none})
            ina_key   = Reference.create({none})
          }
          do Hashtbl.add(state_tbl, client, state)
          {delay = Reference.get(state.dis_delay) ref=state.dis_key
           idelay = Reference.get(state.ina_delay) iref = state.ina_key}
      )
    aux(ref, k) =
      @atomic(match Reference.get(ref)
        | {none} -> Reference.set(ref, {some = k})
        | {some = k2} ->
          do Scheduler.abort(k2)
          Reference.set(ref, {some = k})
      )
    do if active && idelay > 0 then
      k = Scheduler.asleep(idelay,
            -> Option.iter(s -> raise_event(client, {inactive}, Hashtbl.bindings(s.callbacks))
                           , Hashtbl.try_find(state_tbl, client))
          )
      aux(iref, k)
    aux(ref, Scheduler.asleep(delay, -> remove(client)))


}}


#<Else>
/*
    @author Francois Pessaux
*/

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
@abstract type ClientEvent.key = external

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
                   (ThreadContext.client -> void) -> ClientEvent.key =
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
                       : ClientEvent.key =
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
  remove_event : ClientEvent.key -> void = %% BslClientEvent.remove_event %%




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
                          : ClientEvent.key =
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
                          : ClientEvent.key =
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
#<End>
