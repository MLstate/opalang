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
 * Manipulating sessions with low level interface.
 *
 * @category concurrency
 * @author David Rajchenbach-Teller, 2009-2010
 * @author Raja Boujbel, 2010
 * @author Quentin Bourgerie, 2010
 * @destination private
 * @stability stable
 */

/**
 * {1 Types defined in this module}
 */
/**
 * Type of low level channel, abstract implementation
 */
@private type Session.private.native('msg, 'ctx) = external

/**
 * Indicates to low level sending function how to send the message.
 */
type Session.how_send('message) =
  { serialize : 'message -> RPC.Json.json; message : 'message } /
  /** Basic send */

  { serialize : 'message -> RPC.Json.json; message : 'message;
    herror : -> void; hsuccess : -> void}
  /** Send with an error handler and success handler */

/**
 * {1 Interface}
 */

/* Register serialization and unserialization of -> unit on bsl */
@both_implem register_uu =
  serialize_uu(x: -> void) =
    Json.to_ll_json(OpaSerialize.partial_serialize(x, @typeval(-> void)))
  unserialize_uu(x) =
    fail() = error("Unserialize of herror failed")
    json = Option.lazy_default(fail, Json.from_ll_json(x))
    value = OpaSerialize.finish_unserialize(json, @typeval(-> void))
    r : -> void = Option.lazy_default(fail, value)
    r
  bp = @may_cps(%%BslSession.set_uu%%)
  bp(serialize_uu, unserialize_uu)

@private Session_private = {{

    /**
     * {2 Utilities}
     */

    /**
     * High level handler to low level handler.
     * Convert a session handler to a function which take the state
     * and a message on parameter and returns an optional state.
     */
    to_llhandler(handler : Session.handler('state, 'message)) =
      (st, msg ->
        basic_trans =
          | {continue} -> {some = st}
          | {stop} -> {none}
        instr_trans =
          | ~{set} -> {some = set}
          | {unchanged} -> {some = st}
          | {stop} -> {none}
        match handler with
          | ~{normal} -> instr_trans(normal(st, msg))
          | {basic = handler}
          | {concurrent = handler} -> basic_trans(handler(st, msg))
      ) : 'state, 'message -> option('state)


    /**
     * {2 Creating sessions}
     */
    @private make_make(state : 'st,
                unserialize : RPC.Json.json -> option('msg),
                handler : Session.handler('state, 'message),
                more : 'more,
                selector : Session.context_selector,
                make) =
      concurrent =
        match handler with
        | {concurrent = _} -> true
        | _ -> false
      handler(st, msg, ctx) =
      @sliced_expr({
        client =
            to_llhandler(handler)(st, msg)
        server =
          @with_thread_context(
            Option.default(ThreadContext.default, ctx),
            to_llhandler(handler)(st, msg)
          )
       })
      unser(ctx, x : RPC.Json.private.native) : option('msg) =
        @with_thread_context(
          Option.default(ThreadContext.default, ctx),
          tmp =  Json.from_ll_json(x)
            unserialize(match tmp with ~{some} -> some | {none} -> do Log.error("Session", "{x}") error("Malformed JSON object"))
        )
      ctx =
        match selector with
        | { maker } -> { some = ThreadContext.get({current}) }
        | { sender } -> { none }
        | { mixed } ->
          context = ThreadContext.get({current})
          match context.key
          | { nothing } -> { none }
          | _ -> { some = context }
          end
        end
      make(state, unser, handler, {none}, ctx, more, concurrent)
    : channel('msg)

    /**
     * Low level make channel
     *
     * Considering an original [state] a function for [unserialize]
     * remote message. An [handler] who treat received message with
     * the current state. You also can attach any [more] information
     * on the created session. [selector] is used for determine in
     * which context the [handler] is execute :
     * - { maker } -> [handler] is executed with the context of the
     *   session's creator.
     * - { sender } -> [handler] is executed with the context of the
     *   session's caller.
     *
     * Note : This features (selector) work only if the session is
     * created on the server. Use selector [{maker}] by default unless
     * if you are aware what you do.
     */
    llmake_more(state : 'st,
                unserialize : RPC.Json.json -> option('msg),
                handler : Session.handler('state, 'message),
                more : 'more,
                selector : Session.context_selector) =
      make = @may_cps(%%BslSession.llmake%%)
      make_make(state, unserialize, handler, more, selector, make)
    : channel('msg)

    /**
     * Shorcut to [llmake_more], with [more = {none}] and
     * [selector = {maker}]
     */
    llmake(state, unserialize, handler) =
      llmake_more(state, unserialize, handler, {none}, {maker})

    /**
     *
     */
    @server make_shared(key, state, unserialize, handler) =
      make = %%BslSession.make_shared%%
      make_make(state, unserialize, handler, {none}, {maker},
                make(key, _, _ ,_ ,_ ,_ ,_ ,_))

    /**
     * {2 Sending messages}
     */

    /**
     * Send a message to the given channel [chan], with a function for
     * [serialize] sent [message]. If the session was created with
     * selector [sender], the session's handler is executed with the
     * given [context].
     */
    llsend_more(chan : Session.private.native('msg, 'ctx),
                context : option('ctx),
                how_send : Session.how_send('msg)
                ) =
      match how_send with
      | ~{serialize; message} ->
        LL_Session_llsend = %%BslSession.llsend%%
        ser(x : 'msg) : RPC.Json.private.native = Json.to_ll_json(serialize(x))
        LL_Session_llsend(chan, ser, message, context)
      | ~{serialize; message; herror; hsuccess} ->
        LL_Session_llsend = %%BslSession.llsend_then%%
        ser(x : 'msg) : RPC.Json.private.native = Json.to_ll_json(serialize(x))
        LL_Session_llsend(chan, ser, message, context, herror, hsuccess)


    /**
     * Shorcut to [llsend_more], with the current [context]
     * (ThreadContext.get_opt({current})).
     */
    llsend(chan, how_send) =
      llsend_more(chan, ThreadContext.get_opt({current}),how_send)


    /**
     * {2 Utilities}
     */

    /**
     * Serialize / Unserialize channel functions
     */

    serialize(x:channel('msg), options:OpaSerialize.options) : RPC.Json.json =
      x = match options.to_session with
      | {none} ->
        aux = @may_cps(%%BslSession.export%%)
        ctx = match ThreadContext.get({current}).key with
          | {server = _} | {nothing} ->
            // This case is on toplelvel javascript serialization.
            // Use a client identity which can't be collected.
            ThreadContext.Client.fake
          | ~{client} -> client
        aux(x, ctx)
      | {some = entity} ->
        aux = @may_cps(%%BslSession.serialize_for_entity%%)
        aux(x, entity)
      Option.get_msg(-> "[Session.serialize] Json object malformed",
                     Json.from_ll_json(x))



    unserialize(x : RPC.Json.json) : option(channel('msg)) =
      aux = @may_cps(%%BslSession.unserialize%%)
              : option(ThreadContext.t), RPC.Json.private.native -> option(Session.private.native('msg, _))
      match aux(ThreadContext.get_opt({current}), Json.to_ll_json(x)) with
      | {none} -> do Log.error("[Session][unserialize]","fail") {none}
      | {some = r}-> {some = r}

}}
