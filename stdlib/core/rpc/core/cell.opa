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

import stdlib.core.{map, web.core, parser}

/**
 * Manipulating cells.
 *
 * This file provides the primitives for using cells.
 *
 * @category concurrency
 * @author David Rajchenbach-Teller, 2009-2010
 * @author Raja Boujbel, 2010
 * @author Quentin Bourgerie, 2010
 * @destination public
 * @stability wip
 */

/**
 * {1 About this module}
 *
 * This module provides the necessary functions to define and interact with cell.
 * A cell is a session in which sending produces return values.
 *
 * {1 Where do I start?}
 *
 * You can create a cell with {!Cell.make} and call it with {!Cell.call}.
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

/**
 * An instruction for the cell manager, related to state and return value changes.
 *
 * @param 'state The type of states used in this cell.
 * @param 'return The type of return value in this cell.
 */
type Cell.instruction('state, 'result) = {
  return : 'result; /** Return value */
  instruction : Session.instruction('state) /** Instruction for the session manager. */
}

/**
 * A basic instruction for the cell manager.
 *
 * @param 'return The type of return value in this cell.
 */
type Cell.basic_instruction('result) = {
  return : 'result; /** Return value */
  instruction : Session.basic_instruction /** Instruction for the session manager. */
}

/**
 * The type of a cell.
 *
 * @param 'message The type of messages accepted by the cell
 * @param 'result The type of returned value
 */
@abstract type Cell.cell('message, 'result) =
  channel((continuation('result), 'message))

/**
 * Type of cells handlers
 */
type Cell.handler('state, 'message, 'result) =
  { normal : 'state, 'message -> Cell.instruction('state, 'result) } /
  /** A normal handler. */

  { basic : 'state, 'message -> Cell.basic_instruction('result) } /
  /** A handler that can't be set the cell state. */

  { concurrent : 'state, 'message -> Cell.basic_instruction('result) }
  /** Like basic handler but can be executed on concurrent way. */



/**
 * {1 Low-level module}
 */

/**
 * The low level module for cells.
 */
Cell_private = {{

  /**
   * {2 Creating cells}
   */

  /**
   * Create a cell (See also {!Cell.make})
   *
   * @param state The initial state of the cell.
   * @param on_message The message handler for this cell. This handler takes a
   *        state and a message and return instruction for the cell manager
   * @param unserialize An optional function for unserialize messages, by
   *        default it's {!OpaSerialize.finish_unserialize}
   * @param serialize_result An optional function for serialize result, by
   *        default it's {!OpaSerialize.partial_serialize}
   * @return A cell
   */
  llmake(state : 'state,
         handler : Cell.handler('state, 'message, 'result),
         unserialize_msg : option(RPC.Json.json -> option('message)),
         result_ty : option(OpaType.ty)) =
    result_ty = result_ty ? @typeval('result)
    /* Get the default unserialize function if user doesn't give one
     * */
    unserialize_msg =
      match unserialize_msg with
      | {none} ->
        Magic.id(OpaSerialize.finish_unserialize(_,
                   @typeval('message)))
        : RPC.Json.json -> option('message)
      | {some = unserialize_msg : RPC.Json.json -> option('message)} -> unserialize_msg
    /* Redefine unserialize continuation if no cps on client side */
    uns_f1 =
      (| {String = "PleaseCallForMe"} ->
         {some = Continuation.make(_ -> error("That case should never happen"))}
       | json ->
         t = {TyName_args = [result_ty]; TyName_ident="continuation"}
         Magic.id(OpaSerialize.finish_unserialize(json, t)))
    /* Unserialize function */
    unserialize =
        /* Here it's a little hackish ...
         * Session that we create take a couple of ('message -> void, 'message)
         * The user give us a function for unserialize 'message, bellow we make
         * a function that unserialize ('message -> void, 'message) */
        (| ({List = [ f1 | [ f2 | []]]} : RPC.Json.json) ->
            k = uns_f1(f1)
            msg = unserialize_msg(f2)
            (match (k, msg) with
             | ({some=k}, {some=msg}) ->
               {some = (k, msg)}
             | _ ->
               {none})
          | _ -> {none})
     : RPC.Json.json -> option((continuation('result), 'message))
    /* Handler for encapsulated session */
    handler_bis =
      execute(handler, st, (k, msg)) =
        res = handler(st, msg)
        do if WebUtils.is_client() && not(@compiletime("cps_client_mode")) then
          Continuation.execute(k, res.return)
        else Continuation.return(k, res.return)
        res.instruction
      match handler with
      | {normal = handler} -> {normal = execute(handler, _, _)}
      | {basic = handler} -> {basic = execute(handler, _, _)}
      | {concurrent = handler} -> {concurrent = execute(handler, _, _)}

    /* More information contains serialization of result */
    more =
      /* TODO : What about options and sharing? */
      s_result(result) =
        OpaSerialize.partial_serialize(result, result_ty)
      on_message = Session_private.to_llhandler(handler_bis)
      some({cell = {~s_result; ~on_message}})

    /* Encapsulated session */
    sess = Session_private.llmake_more(state, unserialize, handler_bis,
                                       more, {sender})
    sess
  : Cell.cell('message, 'result)


  /**
   * {2 Sending messages}
   */

  /**
   * Generating fresh identifiers for call remote cells
  **/
  @private @server fresh_call_id = Mutable.make(0)
  @private @server call_tbl = Mutable.make(IntSet.empty)
  @private @server gen_call_id() =
    id = fresh_call_id.get()
    do fresh_call_id.set(succ(id))
    set = IntSet.add(id, call_tbl.get())
    do call_tbl.set(set)
    id
  @private @server unset_gen_call(id) =
    set = call_tbl.get()
    if IntSet.mem(id, set)
    then
      do call_tbl.set(IntSet.remove(id, set))
      true
    else
      false

  @private @publish rpc_response_delay = %%BslRPC.rpc_response_delay%%

  /**
   * Send a message to a cell
   *
   * @param cell Cell to which message is sent
   * @param message Message to send
   * @param serialize An optional function for serialize the message, by default
   *                  it's {!OpaSerialize.partial_serialize}
   * @return return value after applying on_message
   */
  @private gm = %% BslSession.get_more%%
  @private bsl_llcall = %%BslSession.SynchronousCell.llcall%%
  llcall(cell : Cell.cell('message, 'result),
         message : 'message,
         serialize : option('message -> RPC.Json.json),
         result_ty : option(OpaType.ty)) =
    result_ty = result_ty ? @typeval('result)
    /* Get the default serialize function if user doesn't give one */
    // What about value restriction ?
    serialize =
      match serialize with
      | {none} ->
        (msg ->
          options = Session.serialization_options(cell)
          OpaSerialize.partial_serialize_options(
            Magic.id(msg), @typeof(message), options):RPC.Json.json)
      | {some = serialize} -> serialize
    /* Redefine serialize if no cps on client and call a llcall */
    #<Ifstatic:OPA_CPS_CLIENT>
    #<Else>
    @sliced_expr({
    client = (
      serialize(x) =
        x = serialize(x)
        x = {List = {hd={String = "PleaseCallForMe"};tl={hd=x;tl=[]}}}
        Json.to_ll_json(x)
      unserialize_result = Magic.id(OpaSerialize.finish_unserialize(_, result_ty))
      : RPC.Json.json -> option('result)
      unserialize_result(json:RPC.Json.json):'result =
           unserialize_result(json) ?  error("CELL : Unserialize result fail")
      unserialize_result(lljson:RPC.Json.private.native):'result =
           json = Json.from_ll_json(lljson) ? error("CELL : Convert RPC.Json.private.native to json failed")
           unserialize_result(json)
      on_message =
        match gm(cell) with
        |{some = {cell = ~{on_message ...}}} -> on_message
        |{none} ->
          _, _ -> error("No handler on this cells (That case should never happens)")

      bsl_llcall(cell, message, serialize, unserialize_result, on_message)
    )

    ;

    server = (
    #<End>
      /* Encapsulated session */
      sess = cell
      /* Redefined serialize */
      serialize =
        /* Here it's a little hackish ...
         * Session encapsulated by the cell it's typed by
         * ('message -> void, 'message)
         * The user give us a function for serialize 'message, bellow we make
         * a function that serialize ('message -> void, 'message) */
        (k, msg) ->
          t = {TyName_args = [result_ty]; TyName_ident="continuation"}
          options = Session.serialization_options(sess)
          f1 = OpaSerialize.partial_serialize_options(Magic.id(k), t, options)
          f2 = serialize(msg)
          {List = {hd=f1;tl={hd=f2;tl=[]}}}
      callbis(k : continuation('result)) : void =
        @with_thread_context(
          ThreadContext.get({from = k}),
          Session_private.llsend(sess, {~serialize message=(k, message)}))

      if not(Session.is_local(sess))
      then (
        id = gen_call_id()
        @server timeout() =
          if unset_gen_call(id)
          then
            // if needed, we can retrieve the context corresponding to the client
            // and store it in the exception
            @throw( { Cell = { timeout } } )
        do sleep(rpc_response_delay, timeout)
        r = @callcc(callbis)
        if unset_gen_call(id) then r
        else error("This cell call was aborted, result ignored")
      )
      else
        @callcc(callbis)

    #<Ifstatic:OPA_CPS_CLIENT>
    #<Else>
      )
    })
    #<End>

}}

/**
 * {1 Special Cell exception}
 *
**/
@opacapi
type Cell.timeout = {
  Cell : {
    timeout : { }
  }
}

// hack
@server_private @private _please_type_me_this_cell_exception() =
  exc = { Cell = { timeout } }
  @throw( @opensums(exc) )

/**
 * {1 High-level module}
**/

@both Cell = {{


  /**
   * {2 Creating sessions}
  **/

  /**
   * Create a cell.
   *
   * @param state the initial state of the cell.
   * @param on_message The message handler for this cell.
   *                   This handler takes a state and a message and
   *                   return instruction for the cell manager
   * @return A cell
  **/
  make(state:'state,
       on_message:'state, 'message -> Cell.instruction('state, 'result)) =
    Cell_private.llmake(state, {normal = on_message}, {none}, {none})
  : Cell.cell('message, 'result)


  /**
   * {2 Sending messages}
  **/

  /**
   * Send a message to a cell
   *
   * @param cell to which message is sent
   * @param message
   * @return return value after applying on_message & on_return functions
  **/
  call(cell : Cell.cell('message, 'result), message : 'message) =
    Cell_private.llcall(cell, message, {none}, {none})
  : 'result

}}

/**
 * {1 Server module}
 * This module is used as a support for non cps client to call a non
 * local cell (a cell doesn't owned by caller client).
 */

type middle('msg, 'ctx) = external

@server Cell_Server = {{

  Dispatcher = {{
    reply(winfo, msg, status) =
      winfo.cont(
        WebCoreExport.default_make_response(
          {volatile}, winfo.http_request.request, status,
          "text/plain", msg)
      )

    parser_(winfo) =
      forbidden(msg) =
        #<Ifstatic:MLSTATE_PING_DEBUG>
        #<Else>
        _ = msg
        msg = "Unauthorized request"
        #<End>
        do reply(winfo, msg, {unauthorized})
        do Log.error("Cell_Server", msg)
        error("Cell_server")
      parser
        | "cell/CallThatPlease" ->
          do Log.info("Cell_Server", "Delegate cell call")
          middle =
            rtm = %% BslSession.Convert.request_to_middle %%
            rtm(WebInfo.to_native_web_info(winfo)) ? forbidden("Bad formatted request")
          cell =
            cfm = %% BslSession.Convert.chan_from_middle %%
            cfm(middle)
          s_result =
            get_more = %% BslSession.Convert.get_more %%
            match (get_more(middle)) : option
            /*
               Initially the line commented below was there in place of the
               following one. However, this brings both row and column variables
               which is now forbidden.
               | {some = {cell = { ~s_result ...}}} -> s_result
            */
            | {some = {cell = toto }} -> toto.s_result
            | _ -> forbidden("Try to call a non cell session")
          : 'result -> RPC.Json.json
          /* Message is the session message i.e. its a couple (k,
           * cellmsg) its why we apply a .f2 on message. */
          message =
            mfm = %% BslSession.Convert.msg_from_middle %%
            (mfm(middle) ? forbidden("Bad formatted cell message")).f2
          result = Cell.call(Magic.id(cell), message)
          reply(winfo, OpaSerialize.finish_serialize(s_result(result)), {success})

  }}

}}
