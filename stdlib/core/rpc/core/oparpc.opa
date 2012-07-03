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
 * Client and server RPC
 *
 * @category internal RPC
 * @destination private
 * @author Quentin Bourgerie
 * @stability stable?
 */

/**
 * {1 About this module}
 *
 * This module defines some types and provides functions to make RPC between
 * client and server.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/* disabled
type OpaRPC.interface = {{
  /* Unserialize */
  unserialize : string -> option(OpaRPC.request)
  extract_types : OpaRPC.request -> list(OpaType.ty)
  extract_values : OpaRPC.request, list(OpaType.ty) -> option(list('a))

  /* Serialize */
  empty_request : OpaRPC.request
  add_args : 'b, OpaRPC.request -> OpaRPC.request
  add_var_types : OpaType.ty, OpaRPC.request -> OpaRPC.request
  serialize : OpaRPC.request -> string
}}
*/

/**
 * {1 Types defined in this module}
 *
 * An RPC request in opa is composed by :
 * - A list of arguments.
 * - A list of types (for arguments whose types can't be know statically).
 */

/**
 * A RPC request.
 */
@opacapi
@abstract type OpaRPC.request = {
  types : list(OpaType.ty);
  rows : list(OpaType.row);
  cols : list(OpaType.col);
  values : list(OpaSerialize.unser)
}

/**
 * {1 Common module for client and server}
 */

@both OpaRPC = {{

  /**
   * {2 Unserialization}
   */

  /**
   * Unserialize from [string] to an {!OpaRPC.request}.
   */
  unserialize_aux(ty:OpaType.ty,json_list:list(RPC.Json.json)) =
    List.fold_backwards(
      (json, (acc, err) ->
        if err then (acc, err)
        else
          match OpaSerialize.finish_unserialize(json, ty) with
          | {none} -> ([], true)
          | {some = ty} -> (ty +> acc, err)
      ), json_list, ([], false)
    )

  unserialize(str:string): option(OpaRPC.request) =
    match Json.deserialize_opt(str) with
    | {some={List=[{List=(types : list(RPC.Json.json))} : RPC.Json.json,
                   {List=(rows : list(RPC.Json.json))},
                   {List=(cols : list(RPC.Json.json))},
                   {List=(values : list(RPC.Json.json))}]}} ->
        (types,error_var) = unserialize_aux(@typeval(OpaType.ty), types)
        (rows,error_row) = unserialize_aux(@typeval(OpaType.row), rows)
        (cols,error_col) = unserialize_aux(@typeval(OpaType.col), cols)
        if error_var || error_row || error_col then
          do Log.error("oparpc unserialize","Type|Row|Col value isn't a OpaType.ty|row|col")
          none
        else
          some(~{types; rows; cols; values}) : option(OpaRPC.request)
    | {none} ->
      do Log.error("OpaRPC.unserialize", "Incorrect request. Cannot decode string : {str}")
      error("RPC error")

    | ~{some} ->
       do Log.error("OpaRPC.unserialize", "Bad formatted json request : {some}")
      error("RPC error")

  /**
   * Extract list of types contained in given [request].
   */
  extract_types(request : OpaRPC.request) : OpaTsc.instantiation =
    r = request
    {types=r.types rows=r.rows cols=r.cols}

  /**
   * Extract values safely from [request]. This functions ensures that
   * all returned values are match with given [types]. The length of
   * [types] list must be equals to the number of values in the
   * request.
   */
  extract_values(request:OpaRPC.request, types:list(OpaType.ty)) =
    request = request
    values =
      List.fold2(
        (value, ty, (acc, err) ->
          if err then (acc, err)
          else
            match OpaSerialize.finish_unserialize(value, ty) with
            | {none} ->
              do Log.error("OPARPC","extract_values -> Value doesn't match given ty")
              ([], true)
            | {some = value} -> ((value +> acc), err)
        ),request.values, types, ([], false)
      )
    if values.f2 then none
    else some(List.rev(values.f1))
  : option(list('a))

  /**
   * {2 Serializion}
   */

  /**
   * Value of the empty request. Used for construct a request.
   */
  empty_request : OpaRPC.request = {types = []; rows = []; cols = []; values = []}

  add_args_with_type(type_:OpaType.ty, value:'a, request:OpaRPC.request) =
    {request with
      values =
        [OpaSerialize.partial_serialize(value, type_) | request.values]}
  : OpaRPC.request

  /**
   * Add a type [ty] on the request. This type can be used for check
   * arguments of the request.
   */
  add_var_types(ty:OpaType.ty, request:OpaRPC.request) : OpaRPC.request =
    {request with types = ty +> request.types}
  add_row_types(ty:OpaType.row, request:OpaRPC.request) : OpaRPC.request =
    {request with rows = ty +> request.rows}
  add_col_types(ty:OpaType.col, request:OpaRPC.request) : OpaRPC.request =
    {request with cols = ty +> request.cols}

  /**
   * Serialize given [request].
   */
  @private serialize_aux(list) =
    List.fold(
      (ty, lres -> OpaSerialize.partial_serialize(ty, @typeof(ty)) +> lres),
       list, [])
  serialize(request:OpaRPC.request) : string =
    types = serialize_aux(request.types)
    rows = serialize_aux(request.rows)
    cols = serialize_aux(request.cols)
    Json.serialize_opt(
      {List=[{List=types},{List=rows},{List=cols},{List=List.rev(request.values)}]} : RPC.Json.json
    )

}} /* disabled : OpaRPC.interface */

/**
 * {1 Specific client module for RPC}
 */
@client OpaRPC_Client = {{

  /**
   * A cache for rpc request, its used for non-functionnal rpc
   * TODO (K1) : Use a hastbl instead
   *
   * Note: This function is type-unsafe and should be used only by the compiler.
   */
  try_cache =
  @nonexpansive(
    refmap =
      // It's an hack because that code is not cleaned on no-server
      if WebUtils.is_client() then
        ClientReference.create(StringMap.empty)
      else Magic.id("That code is not cleaned on no-server")
    (id, f ->
      map = ClientReference.get(refmap)
      match StringMap.get(id, map) with
      | {none} ->
        result = f()
        do ClientReference.set(refmap, StringMap.add(id, result, map))
        result
      | {some = cached} -> cached
    ))

  /**
   * Sending a request to server.
   * TODO for CPS client use callcc?
   */
  send_to_server(fun_name, request, ty) =
    #<Ifstatic:OPA_FULL_DISPATCHER>
    mr = PingClient.sync_request : string, string -> string
    #<Else>
    mr = %%Session.PingRegister.pang_request%% : string, string -> string
    #<End>
    url = "/rpc_call/" ^ fun_name
    ty_success = [{label="success" ~ty}]
    ty_failure = [{label="failure" ty={TyRecord_row = []}}]
    ty = {TySum_col=[ty_failure,ty_success]}
    match OpaSerialize.unserialize(mr(url, OpaRPC.serialize(request)), ty) with
      | {some={~success}} -> success
      | {some={failure}} -> error("OPARPC : Request on {url} has failed")
      | {none} ->
        /* TODOK1 - One day we can manage request error??*/
        error("OPARPC : Request on {url} has failed")

  async_send_to_server(fun_name, request, _) =
    url= "/rpc_call/" ^ fun_name
    body = OpaRPC.serialize(request)
    #<Ifstatic:OPA_FULL_DISPATCHER>
    PingClient.async_request(url, body)
    #<Else>
    mr = %%Session.PingRegister.pang_request%% : string, string -> string
    ignore(mr(url, body))
    #<End>



  /**
   * This module is a dispatcher of RPC on client
   */
  Dispatcher = {{
    #<Ifstatic:OPA_FULL_DISPATCHER>
    @private
    error(msg) = Log.error("OpaRPC", msg)

    @private rpctbl = Hashtbl.create(1024) : Hashtbl.t(string, (string -> option(string)))

    register(key, rpc) = Hashtbl.add(rpctbl, key, rpc)

    call(id:option(string), name:string, argument:string) =
      match Hashtbl.try_find(rpctbl, name) with
      | {none} -> error("Client rpc({name}) was not found")
      | {some = rpc} ->
        match rpc(argument) with
        | {none} ->
          error("An error occurs when call client rpc({name}) with \"{argument}\"")
        | {some=result} ->
          match id with
          | {none} -> void
          | {some=id} -> PingClient.async_request("/rpc_return/{id}", result) : void
    #<Else>
    register = %%Session.comet_table_add%%
    #<End>
  }}

}}


/**
 * {1 Special RPC exception}
 *
 * <!> Built in [BslNativeLib.ml]
**/
@opacapi
type OpaRPC.timeout = {
  OpaRPC_Server : {
    timeout : {
      client : ThreadContext.client ;
      fun_id : string ;
    }
  }
}

// hack
@server_private @private _please_type_me_this_rpc_exception(client : ThreadContext.client) =
  timeout = { ~client ; fun_id = ""}
  exc = { OpaRPC_Server = { ~timeout } }
  @throw( @opensums(exc) )


/**
 * {1 Specific server module for RPC}
 */

@server_private
OpaRPC_Server =

  TCMap =
    tc_order = Order.make(
      x, y ->
        match x.key with
          | {client = xk} -> match y.key with
                | {client = yk} ->
                  match Int.ordering(xk.page, yk.page) with
                   | {eq} -> String.ordering(xk.client, yk.client)
                   | r    -> r
                  end
                | _ -> error("[OpaRpc] Tc_order : should never happens")
             end
          | _ -> error("[OpaRpc] Tc_order : should never happens")
        end): order(ThreadContext.t, ThreadContext.private.order)

    Map_make(tc_order)

  {{

  /**
   * A cache for rpc request, its used for non-functional rpc
   * TODO (K1) : GC of cached rpc
   *
   * Note: This function is type-unsafe and should be used only by the compiler.
   */
  try_cache =
   @nonexpansive(
    cache = Cell.make(TCMap.empty,
      (cache, msg ->
        ctx = ThreadContext.get({current})
        match msg with
        | {get = id} ->
          return =
            (match TCMap.get(ctx, cache) with
             | {none} -> {none}
             | {some = idmap} -> StringMap.get(id, idmap))
          { ~return; instruction = {unchanged} }

        | {add = (id, value)} ->
          idmap = TCMap.get(ctx, cache) ? StringMap.empty
          idmap = StringMap.add(id, value, idmap)
          {
            instruction = {set = TCMap.add(ctx, idmap, cache)};
            return = {none};
          }
      )
    ) : Cell.cell('a, option(black)) // Black coercion needed else ei
                                     // add a type argument
    (id, f ->
      match Cell.call(cache, {get = id}) with
      | {none} ->
        result = f()
        _ = Cell.call(cache, {add = (id, result)})
        result
      | {some = cached} ->
        cached
    ))

  /**
   * Sending a request to the client
   */
  #<Ifstatic:OPA_FULL_DISPATCHER>
  @private
  rpc_infos = Hashtbl.create(512)
    : Hashtbl.t(string, {k:continuation(string) client:ThreadContext.client})

  @private
  gen_id = String.fresh(0)

  @private
  send_response(sync, name, args, k, client) =
    msg =
      match sync with
      | {true} ->
        id = gen_id()
        do @atomic(Hashtbl.add(rpc_infos, id, ~{k client}))
        {`type`="rpc" ~id ~name ~args}
      | {false} ->
        {`type`="asyncrpc" ~name ~args}
    PingRegister.send(client, OpaSerialize.partial_serialize(msg, @typeof(msg)))

  @private
  rpc_return(cid, id, return) =
    @atomic(
      match Hashtbl.try_find(rpc_infos, id) with
      | {none} ->
        do Log.error("OpaRpc", "No rpc id:"^id)
        false
      | {some = ~{k client}} ->
        if client.client == cid.client && client.page == cid.page then
          do Hashtbl.remove(rpc_infos, id)
          do Continuation.return(k, return)
          true
        else
          do Log.error("OpaRpc", "Wrong client try to RPC reply")
          false
    )
  #<Else>
  @private
  send_response = %%BslRPC.call%%
  #<End>

  send_to_client(fun_name : string, request : OpaRPC.request, ty : OpaType.ty) : 'a =
    arg = OpaRPC.serialize(request)
    serialized_return =
      @callcc(
        k ->
          t = ThreadContext.get({from = k})
          match t with
          | {key = {client = x}; details = _; request = _; constraint = {free}} ->
              if not(send_response(true, fun_name, arg, k, x)) then
                error("Server request client rpc but client wasn't ping ({fun_name})")

          | {key = {client = _}; details = _; request = _; constraint = _ } ->
            error("Server request client rpc but is in a \"no_client_calls\" section ({fun_name})")
          | _ ->
            error("Invalid distant call to function ({fun_name}) at {__POSITION__}: there seems to be no client connected")
          end
      )
    do Log.debug("RPC", "{fun_name} received")
    OpaSerialize.unserialize(serialized_return, ty)
    ? error("OPARPC : Request on client url {fun_name} has failed.")

  @private dummy_cont = Continuation.make((_:string) -> @fail("Dummy cont should't be called"))
  async_send_to_client(fun_name : string, request : OpaRPC.request, _) : 'a =
    id = fun_name  //plus some things
    arg = OpaRPC.serialize(request)
    match thread_context() with
    | {key = {client = x}; details = _; request = _; constraint = _} ->
      if not(send_response(false, id, arg, dummy_cont, x)) then
        error("Server request client rpc but client wasn't ping ({fun_name})")
    | _ ->
      error("Invalid distant call to function ({fun_name}) at {__POSITION__}: there seems to be no client connected")
    end

  /**
   * This module is a dispatcher of RPC on server
   */
  Dispatcher = {{
    reply(winfo, msg, status) =
      WebInfo.simple_reply(winfo, msg, status)

    reply_error(winfo, msg) =
      #<Ifstatic:MLSTATE_PING_DEBUG>
      #<Else>
      _ = msg
      msg = "Unauthorized request"
      #<End>
      WebInfo.simple_reply(winfo, msg, {unauthorized})

    @private rpctbl = Hashtbl.create(1024) : Hashtbl.t(string, (string -> option(string)))

    register(key, rpc) = Hashtbl.add(rpctbl, key, rpc)

    get(key) = Option.map(Magic.id, Hashtbl.try_find(rpctbl, key))

    /* Duplication
     * can not use [HttpRequest.Generic.get_body] because HttpRequest.request depends on the package [stdlib.rpc.core] */
    get_requested_post_content = %% BslNet.Requestdef.get_request_message_body %% : WebInfo.private.native_request -> string

    parser_(winfo) =
      parser
        #<Ifstatic:OPA_FULL_DISPATCHER>
        | "rpc_return/" id=(.*) ->
          client =
            ThreadContext.Client.get_opt({current})
            ? error("No client context : {ThreadContext.get_opt({current})}")
          id = Text.to_string(id)
          body = (%%BslNet.Requestdef.get_request_message_body %%(winfo.http_request.request))
          if rpc_return(client, id, body) then reply(winfo, "true", {success})
          else reply(winfo, "false", {unauthorized})
        #<End>
        | "rpc_call/" name=(.*) ->
          name = "{name}"
          #<Ifstatic:MLSTATE_PING_DEBUG>
          do Log.info("OpaRPC", "RPC({name}) try call")
          #<End>
          match get(name) with
            | {none} ->
              _ = reply_error(winfo, "RPC not found")
              Log.error("OpaRPC", "RPC({name}) does not exist")
            | {some = skeleton} ->
              @catch(_ ->
                #<Ifstatic:MLSTATE_PING_DEBUG>
                do Log.info("OpaRPC", "RPC({name}) exception was raise")
                #<End>
                ty = {TyRecord_row=[{label="failure" ty={TyRecord_row = []}}]}
                serial = OpaSerialize.serialize_with_type(ty,{failure})
                reply(winfo, serial, {success}),
              #<Ifstatic:MLSTATE_PING_DEBUG>
              do Log.info("OpaRPC", "RPC({name}) skeleton was found")
              #<End>
              match skeleton(get_requested_post_content(winfo.http_request.request)) with
                | {none} ->
                  do Log.error("OpaRPC", "RPC({name}) Bad formatted request")
                  reply_error(winfo, "Bad formatted rpc request")
                | {some = (ty,result)} ->
                  #<Ifstatic:MLSTATE_PING_DEBUG>
                  do Log.info("OpaRPC", "RPC({name}) success")
                  #<End>
                  ty = {TyRecord_row=[{label="success" ~ty}]}
                  serial = OpaSerialize.serialize_with_type(ty,{success=result})
                  reply(winfo, serial, {success})
              end)
          end
  }}
}}
