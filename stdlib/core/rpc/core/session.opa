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

import stdlib.core.{map, args}
import stdlib.core.rpc.hlnet

/**
 * Manipulating sessions.
 *
 * This file provides the primitives for defining sessions, sending messages to sessions,
 * combining sessions, etc.
 *
 * @category concurrency
 * @author David Rajchenbach-Teller, 2009-2011
 * @author Raja Boujbel, 2010-2011
 * @author Quentin Bourgerie, 2010-2011
 * @destination public
 * @stability stable
 */

/**
 * {1 About this module}
 *
 * This module provides the necessary functions to define and interact with sessions.
 * Sessions are the high-level mechanism used pervasively in Opa for concurrency,
 * mutability and distribution.
 *
 *
 * {1 What are sessions?}
 *
 * A session is a unit of state and concurrency. A session can be created on a server
 * or on a client, an can be shared between several servers.
 *
 * To construct a session, Opa requires an initial {e state} and a message {e handler}.
 * Each time a message is sent to a session, the handler is triggered and can optionally
 * modify the state.
 *
 * Sessions can be constructed but never manipulated directly. In other words, session
 * constructors return a {e channel}, i.e. an abstraction that can be used only to send
 * messages to the session. Note that {e sessions} are located on one specific machine
 * (a client or a server) and never move, but {e channels} can be moved at will from
 * one machine to another. For this reason, this module offers no user-level type [session]
 * but rather a type [channel].
 *
 *
 * {1 Where do I start?}
 *
 * To create a session, use either [Session.make] or [Session.NonBlocking.make].
 * Function [Session.make] creates a session which handles all messages in the
 * background, but only one message at a time. This ensures absolute consistency
 * on the state of the session but may not be appropriate for all applications.
 * By opposition, function [Session.NonBlocking.make] creates a session which
 * can handle any number of messages simultaneously. This ensures maximal
 * responsiveness, but the message handler cannot be certain that it is holding the
 * latest value of the state.
 *
 * To send a message to a session through a channel, use function [send].
 *
 *
 * {1 Sessions and cloud}
 *
 * As mentioned, a session is always located on one machine. However, it is quite easy
 * to build {e cloud sessions}, also known as {e shared sessions}. Such session are toplevel
 * sessions that are automatically shared between several servers running the same application
 * on the same network. Whenever several such servers attempt to build the same cloud session,
 * the Opa distributed runtime chooses exactly one server to host the session and provides all
 * participating servers with a channel that can be used to send messages to the channel.
 *
 * To construct a cloud session, use function [Session.cloud] or [Session.NonBlocking.cloud],
 * depending on your consistency requirement.
 *
 * Once you have constructed a cloud session, to build another session on the same machine,
 * use function [Session.make_at].
 *
 *
 * {1 Sessions and context}
 *
 * By default, a session uses the concurrency context of its creator. In other words, a
 * session created to handle a given HTTP request will retain the information on the user
 * performing that request, or the content of the request, even after the response to the
 * request. For traceability and security reasons, this is generally the desired behavior.
 *
 * Some applications, however, need to be able to customize this behavior. For this purpose,
 * use function [Session.make_dynamic] instead of [Session.make].
 */

/**
 * {1 Types defined in this module}
 */

/**
 * An instruction for the session manager, related to state changes.
 *
 * @param 'state The type of states used in this session.
 */
type Session.instruction('state) =
        {set: 'state} /**Carry on with a new value of the state value.*/
      / {unchanged}   /**Carry on with the same state.*/
      / {stop}        /**Stop this session. Any further message will be ignored.*/

/**
 * An object that may be used to send messages to a session.
 *
 * Sessions can be created but never manipulated directly.
 */
type Session.channel('msg)   = channel('msg)
@abstract type channel('msg) = Session.private.channel('msg)


/**
 * The implementation of a non-blocking session.
 *
 * Use functions of module [Session.NonBlocking] to access the features of non-blocking sessions.
 *
 * @param 'state The type of states used in this session.
 */
type Session.NonBlocking.handler('state) =
{
   get:        -> 'state
   update: ('state -> 'state) -> void
   stop:       -> void
}



/**
 * An instruction for the session manager, related to session ends.
 *
 * This kind of instruction is used for session without state, allowing to specify when to kill this session.
 */
type Session.basic_instruction =
        {continue} /* Continue, don't stop this session */
       / {stop}    /* Stop this session. Any further message will be ignored */

@private type Session.private.channel('msg) = Session.private.native('msg, ThreadContext.t)

type Session.entity = external

/* Order for channels */
@abstract type Channel.order = void

/**
 * A partial order on channels
 */
compare_channel(a:channel('msg), b:channel('msg)) : Order.ordering =
  Order.of_int(%%BslSession.compare_channels%%(a, b))

Channel = {{
  order = @nonexpansive(Order.make(compare_channel)) : order(channel('message), Channel.order)
}}

ChannelSet = @nonexpansive(Set_make(Channel.order))

/** The Hlnet definitions for the protocol for "make_at" queries */
type make_at_query = (OpaType.ty, OpaType.ty, RPC.Json.json, RPC.Json.json)
type make_at_response = option(RPC.Json.json)

/**
 * {1 Interface}
 */

Session = {{

    /**
     * {2 Creating sessions}
     */

    /**
     * Create a session.
     *
     * @param state The initial state of the session.
     * @param on_message The message handler for this session. When messages are sent to this
     * session, the message handler is invoked. Note that messages are sent and handled
     * asynchronously, but that the function handler will only handle one message at a time,
     * hence ensuring that the state can be updated without conflicts. If you need to handle
     * several messages in parallel, you should rather use a {e loose session}.
     *
     * @return A channel which may be used to send messages to the session.
     */
    make(state : 'state, on_message : ('state, 'message -> Session.instruction('state))) =
      make_generic(state, {normal = on_message}) : channel('message)

    /**
     * Like [make] but the message handler of session will be executed
     * with the thread context of message sender instead of session
     * creator's context.
     */
    make_dynamic(state : 'state, on_message : ('state, 'message -> Session.instruction('state))) =
      unserialize = OpaSerialize.finish_unserialize(_, @typeval('message))
      Session_private.llmake_more(state, unserialize, {normal = on_message}, {none}, {sender})
    : channel('message)

    /**
     * As [make] but without state.
     */
    make_callback(on_message : ('message -> void)) : channel('message) =
      on_message_basic(_ : void, msg : 'message) = do on_message(msg); {continue} : Session.basic_instruction
      make_generic(void, { basic = on_message_basic })

    /**
     * As [make_callback] but permits to precise when session ends.
     */
    make_stateless(on_message : ('message -> Session.basic_instruction)) =
      on_message_basic(_ : void, msg : 'message) = on_message(msg) : Session.basic_instruction
      make_generic(void, { basic = on_message_basic })

    /**
     * As [make] but allows to choose the session handler.
     */
    @private
    make_generic(state : 'state, message_handler : Session.handler('state, 'message)) =
      unserialize = OpaSerialize.finish_unserialize(_, @typeval('message))
      Session_private.llmake(state, unserialize, message_handler) : channel('message)


    /**
     * {2 Creating distributed sessions}
     */

    make_at_protocol : Hlnet.protocol(make_at_query, make_at_response) =
      Hlnet.define_protocol(
        "chan/make_at", 1,
        OpaSerialize.serialize, (_chan, s -> OpaSerialize.unserialize(s, @typeval(make_at_query))),
        OpaSerialize.serialize, (_chan, s -> OpaSerialize.unserialize(s, @typeval(make_at_response)))
      )

    /**
     * Used for start a listening which allows other servers to
     * request a make_at.
     */
    @private accept_make_at =
      started_ref = Server_reference.create(false)
      ->
      /* Make sure that accept is called only one time */
      started = @atomic(Server_reference.compare_and_swap(started_ref, false, true))
      if started then void
      else
        Hlnet.accept(Hlnet.default_endpoint, make_at_protocol.server_spec,
          (chan ->
            Hlnet.setup_respond(chan,
              ((tystate, tymessage, state, on_message) ->
                /* Unserialize state and handler */
                tyonmessage : OpaType.ty =
                  tyret = {TyName_ident = "Session.instruction";
                           TyName_args = [tystate]}
                  {TyArrow_params = [tystate, tymessage];
                   TyArrow_res = tyret}
                match (OpaSerialize.finish_unserialize(state, tystate),
                       OpaSerialize.finish_unserialize(on_message, tyonmessage))
                | ({some = state}, {some = on_message}) ->
                  /* Create session */
                  session = Session_private.llmake(@unsafe_cast(state),
                              OpaSerialize.finish_unserialize(_, tymessage),
                              {normal = @unsafe_cast(on_message)})
                  /* Send created session */
                  tychannel = {TyName_ident = "channel";
                               TyName_args = [tymessage]}
                  entity = get_server_entity(Hlnet.remote_endpoint(chan))
                  options = { OpaSerialize.default_options with to_session={some=entity} to={server} closure={at_best} }
                  session = OpaSerialize.partial_serialize_options(@unsafe_cast(session),
                              tychannel, options)
                  some(session)
                | _ ->
                  do Log.warning("[Session.accept_make_at]",
                                 "An error occurs when try to unserialize state or handler")
                  none
              )
            )
          )
        )

    /**
     * Like [make] but created session is shared between all of your
     * servers which have the same directory. The shared session was
     * identified by a given [key].
     *
     * At server starting you can specify on command line :
     * --chan-directory (own|<ip>)[:<port>] : For indicates which
     * server is the directory.
     * Example if you want server1 and server2 can share session :
     *   server1 ... --chan-directory own
     *   server2 ... --chan-directory ipserver1
     */
    @publish @server make_shared(key : string, state : 'state, on_message : ('state, 'message -> Session.instruction('state))) =
      do accept_make_at()
      Session_private.make_shared(
        key,
        state,
        OpaSerialize.finish_unserialize(_, @typeval('message)),
        {normal = on_message}
      ) : channel('message)


    @private @publish @server shared_mode =
      commandline : CommandLine.family(bool) = {
        title = "Distribution of sessions"
        init = false
        parsers = [{ CommandLine.default_parser with
          names = ["--cloud"]
          description = "Sessions and network declared as [cloud] will be automatically shared between servers"
          on_encounter(_) = { no_params = true }
        }]
      anonymous = [] ;
      }
      CommandLine.filter(commandline)


   /**
    * Construct a session that is automatically shared between servers.
    *
    * Note: Automatic sharing between servers is activated only if your
    * application is executed with the "cloud" option (--cloud or the
    * cloud launch script).
    *
    * When one or several servers invoke [Session.cloud(k)] with the same value [k], only one
    * session is actually created, on one of the participating servers (chosen arbitrarily) and
    * shared between the servers.
    *
    * @param key A key. The value of the key serves to decide sharing, e.g. if a shared [session] has already
    * been created with [key], subsequent calls to [Session.cloud(key)], on any server, will return the same [session].
    * @param state As in [Session.make]
    * @param on_message As in [Session.make]
   **/
    @server cloud(key, state, handler) =
      if shared_mode
      then make_shared(key, state, handler)
      else get_local_cloud(key, state, handler)

    /**
     * Local index used for cloud sessions when shared mode is not aviable.
     */
    @server_private @private local_cloud_index =
      Cell.make(Map.empty, (map, msg ->
        match msg
        | ~{key ty session} ->
          match Map.get(key, map)
          | {none} ->
            { instruction = {set = Map.add(key, ~{session ty}, map)}
              return = some(session) }
          | {some = ~{session ty=sty}} ->
            { instruction = {unchanged}
              return =
                // TODO - Use Unification.unifiable when it's done
                if OpaType.implementation(sty) == OpaType.implementation(ty) then some(session)
                else none
            }
          end
        end
        )
      ) : Cell.cell({key:string ty:OpaType.ty session:black}, option(black))

    /**
     * Access to local cloud index.
     */
    @server_private @private get_local_cloud(key, state,
          handler : ('state, 'message -> Session.instruction('state))) =
        ty = @typeval('message)
        session = Magic.black(Session.make(state, handler))
        match Cell.call(local_cloud_index, ~{key ty session})
        | ~{some} -> Magic.id(some) : Session.channel('message)
        | {none} -> @fail("Session.cloud : session \"{key}\" already exists but {OpaType.to_pretty(@typeval('message))} is not compatible to type of message")


    /**
     * [make_at state handler channel] Like [make state handler] but
     * the session is created on the server that own the given channel.
     */
    @server make_at(state: 'state, on_message : ('state, 'message -> Session.instruction('state)), session :channel('a)) =
      match get_endpoint(session) with
      | {none} -> /* Make here */
        make(state, on_message)

      | {some=endpoint} -> /* Make at endpoint */
        channel = Hlnet.open_channel(endpoint, make_at_protocol.client_spec)
        entity = get_server_entity(endpoint)
        tystate = @typeof(state)
        tymessage = @typeval('message)
        options = { OpaSerialize.default_options with
          to_session={some=entity}
          to={server}
          closure={distant=(-> Log.warning("Make_at",
                              "Can't serialize handler on optimized way"))}
        }
        state = OpaSerialize.partial_serialize_options(state, tystate, options)
        on_message = OpaSerialize.partial_serialize_options(on_message, @typeof(on_message), options)
        chan = Hlnet.sendreceive(channel, (tystate, tymessage, state, on_message))
        chan = Option.get_msg(-> "[Session][make_at] Make at on remote endpoint fail", chan)
        Option.get_msg(->"[Session][make_at] Can't unserialize remote session",
                              OpaSerialize.finish_unserialize(chan, @typeval(channel('state))))


    /**
     * {2 Sending messages}
     */

    /**
     * Send a message to a session, asynchronously.
     */
    send(channel : channel('message), message : 'message) =
      options = serialization_options(channel)
      serialize = OpaSerialize.partial_serialize_options(_, @typeval('message), options)
      Session_private.llsend(channel, ~{serialize message})

    /**
     * Like [send] but if the [message] to the [channel] was not
     * delivery [herror] will be executed else id the message is
     * delivery [hsuccess] is executed.
     * Note : If [channel] is located on a client this behavior is not
     * guaranteed (client can be mischievous) but we guaranteed that
     * one of handler will be executed.
     */
    try_send(channel:channel('message), message : 'message, herror, hsuccess) =
      options = serialization_options(channel)
      serialize = OpaSerialize.partial_serialize_options(_, @typeval('message), options)
      Session_private.llsend(channel, ~{serialize; message; herror; hsuccess})

    /**
     * Like [send_then] but with [hsuccess = -> void].
     */
    send_or_error(channel:channel('message), message: 'message, herror) =
      try_send(channel, message, herror, (-> void))

    /**
     * Send the same message to a list of channel.
     */
    send_all(channels : list(channel('message)), mess : 'message)=
        List.iter( send(_,mess) ,channels)


    /**
      * {2 Combinators}
      */

    /**
     * Apply a treatment to all messages received by a channel.
     *
     * This function has two main use cases: converting some data
     * before feeding it to a channel and restricting the use of a channel
     * when distributing to other parts of the code.
     *
     * For instance, assume the existence of a channel [c] with type
     * [channel({private : t}/{public : u})], where [private] messages should
     * only ever be sent by privileged sections of the code, while [public]
     * messages could be sent by anyone. It would be a bad idea to distribute
     * [c] to all sections of the code and hope that all users read the documentation
     * and behave accordingly.
     *
     * In this case, invoke [map(x -> {public = x}, c)] to produce a new channel [c'],
     * with the same behavior as [c], but upon which only [public] messages can be
     * sent.
     *
     * @param f A transformation function.
     * @param channel The original channel.
     */
    map(f:'b -> 'a, channel:channel('a)) =
       make_callback(x -> send(channel, f(x))) : channel('b)

    /**
     * Apply a filter to all messages received by a channel, discarding messages that
     * don't fit some criterium.
     *
     * @param f A decision function. For each message [m] received by this channel,
     * if [f(m)] produces [{true}], then the message is passed to the original channel,
     * otherwise, it is discarded.
     * @param channel The original channel.
     */
    filter(f:'a -> bool, channel: channel('a)) : channel('a) =
       make_callback(message -> if f(message) then send(channel, message))

    /**
     * Discard some messages, apply a treatment to others.
     *
     * @param f A decision/treatment function. For each message [m] received by this
     * channel, if [f(m)] produces [{none}], then the message is discarded. Otherwise,
     * if [f(m)] produces [{some = m'}], [m'] is passed to the original channel.
     */
    filter_map(f:'a -> option('b), channel:channel('b)) : channel('a) =
       make_callback(message -> Option.iter(send(channel, _), f(message)))


    /**
     * {2 Advanced forms of sessions}
     */

    /**
     * Non-blocking sessions.
     *
     * By opposition to regular sessions, non-blocking sessions can handle several messages concurrently. This
     * provides better concurrency, at the expense of lowered guarantees: the message handler of a non-blocking
     * session can never be certain that it effectively knows the latest state of the session. It could be
     * an older state.
     */
    NonBlocking =
    {{

       /**
        * Construct a non-blocking session.
        *
        * @param init The initial state of the session.
        * @param on_message The message handler. It is called whenever a message is sent to the session.
        * @return A channel, which may be used with [send] to send a message to the session.
        */
       make(init: 'state, on_message:  ('msg, Session.NonBlocking.handler('state) -> void) ): channel('msg) =
       (
          make_stateless(prepare_make(init, on_message))
       )

       /**
        * Get a recent state of a non-blocking session.
        *
        * Note that the state can changed at any time, so by the time this function returns, the result could
        * be obsolete.
        */
       get(implem: Session.NonBlocking.handler('state)): 'state =
       (
          implem.get()
       )

       /**
        * Transform the state held by a session, atomically.
        *
        * @param transform A function used to transform the state of a session. Note that this function is executed atomically.
        */
       update(transformation: 'state -> 'state, implem: Session.NonBlocking.handler('state)): void =
       (
          implem.update(transformation)
       )

       /**
        * Stop a non-blocking session.
        *
        * Further messages will be ignored.
        */
       stop(implem: Session.NonBlocking.handler('state)): void =
       (
          implem.stop()
       )

       @private prepare_make(init:'state, on_message: ('msg, Session.NonBlocking.handler('state) -> void) ): ('msg -> Session.basic_instruction) =
       (
            /**
             * A reference holding the state. This reference is protected by [state_channel].
             * Note: We rely on atomicity of [Mutable.get]/[Mutable.set].
             */
            state_ref = Mutable.make(init)
            stop_ref  = Mutable.make({false})
            state_handler(msg) = match msg with
              | {stop}       ->
                  do stop_ref.set({true})
                  {stop}
              | ~{update} ->
                  do state_ref.set(update(state_ref.get()))
                  {continue}
            state_channel = Session.make_stateless(state_handler) /* not concurrent */

            implementation =
            {
               get          = state_ref.get
               update(f) = send(state_channel, {update = f})
               stop()       = send(state_channel, {stop})
            }

            external_on_message(msg) =
               do on_message(msg, implementation)
               if stop_ref.get() then {stop}
               else {continue}
            external_on_message
       )

      /**
       * As [make_callback], but concurrent.
       */
      make_callback(on_message : ('message -> void)) : channel('message) =
        on_message_basic(_ : void, msg : 'message) = do on_message(msg); {continue} : Session.basic_instruction
        make_generic(void, { concurrent = on_message_basic })

      /**
       * As [make_stateless], but concurrent.
       */
      make_stateless(on_message : ('message -> Session.basic_instruction)) =
        on_message_basic(_ : void, msg : 'message) = on_message(msg) : Session.basic_instruction
        make_generic(void, { concurrent = on_message_basic })

    }}


    /**
     * {2 Utilities}
     */
    /**
     * Get the server [entity] corresponding to the given [endpoint].
     */
    @private get_server_entity = %%BslSession.get_server_entity%%

    /**
     * Returns entity which own the given channel
     */
    @private owner(chan : channel) =
      bsl = %%BslSession.owner%%
      bsl(chan)

    /**
     * Return true if the given entity is a client
     */
    @private is_client(entity) =
      bsl = %%BslSession.is_client%%
      bsl(entity)

    /**
     * Get the endpoint where the session is located, if [session] is
     * local return [none].
     */
    @private get_endpoint(chan : channel) =
      bsl = %%BslSession.get_endpoint%%
      bsl(chan)

    /**
     * Returns true if the channel is not owned by this server.
     * Beware, this returns false in case of non RemoteClient.
     */
    is_remote(chan : channel) =
      bsl = %%BslSession.is_remote%%
      bsl(chan)

    /**
     * Returns true only if the channel is owned by this server.
     * Returns false in case of Client.
    **/
    is_local = %%BslSession.is_local%% : channel -> bool

    /**
     * [on_remove channel f] Execute f when the [channel] is removed
     * of this server (no effect on the client).
     */
    on_remove(chan:channel('msg), f:-> void): void =
      bp = @may_cps(%%BslSession.on_remove%%)
      : Session.private.native('a, _), (-> void) -> void
      bp(chan, f)

    /**
     * Get options for message serialization for the given [channel].
     */
    @package serialization_options(channel) =
      entity = owner(channel)
      {OpaSerialize.default_options with
        to = if is_client(entity) then {client} else {server}
        to_session = entity
      }


    /**
     * {1 Deprecated API}
     *
     * The following functions are deprecated and replaced by more readable counterparts.
    **/


    /**
     * Create a channel with function taking its own channel in argument
     *
     * Beware, sending a message to its own channel in a treatment function
     * can easily induce livelocks. Adopt preferably another strategy to code
     * (think about using two sessions instead of one for instance)
     *
     * @param init the initial state of the function.
     * @param handler A function triggered upon reception of a message. It takes
     *   in argument its own channel. The return value is as for [make].
    **/
    @deprecated({use="usual Session.make, with rec val construct"})
    make_own(state : 'state, on_message : 'state, 'message, channel('message) -> Session.instruction('state)) =
      rec val s = make(state, on_message(_, _, s))
      s

    @deprecated({use="Session.NonBlocking"})
    Concurrent = NonBlocking

}}

session = Session.make
send = Session.send
