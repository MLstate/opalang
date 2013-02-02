/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
type Session.how_send('message) = GenChannel.how_send('message, RPC.Json.json)

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
  bp = @may_cps(%%Session.set_uu%%)
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
    make_make(state : 'st,
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
      make(state, unser, handler, ctx, more, concurrent)

    /**
     *
     */
    @server make_shared(key, state, unserialize, handler) =
      make = %%Session.make_shared%%
      make_make(state, unserialize, handler, {none}, {maker},
                make(key, _, _ ,_ ,{none} ,_ ,_ ,_))

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
        LL_Session_llsend = %%Session.llsend%%
        ser(x : 'msg) : RPC.Json.private.native = Json.to_ll_json(serialize(x))
        LL_Session_llsend(chan, ser, message, context)
      | ~{serialize; message; herror; hsuccess} ->
        LL_Session_llsend = %%Session.llsend_then%%
        ser(x : 'msg) : RPC.Json.private.native = Json.to_ll_json(serialize(x))
        LL_Session_llsend(chan, ser, message, context, herror, hsuccess)


    /**
     * Shorcut to [llsend_more], with the current [context]
     * (ThreadContext.get_opt({current})).
     */
    llsend(chan, how_send) =
      llsend_more(chan, ThreadContext.get_opt({current}),how_send)

}}
