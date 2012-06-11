















WebSession = {{

  @expand
  `?|>`(a,f) =
    match a with
    | {none} -> @fail
    | {some = x} -> f(x)



  /**
   * Unserialize a channel from Json
   */
  unserialize(context : ThreadContext.t, channel : RPC.Json.json) =
    match channel with
    | {Record = [("srv_id", {Int = local_id})]} -> some(~{local_id})
    | {Record = [("cl_id", {String = client})]} -> some({entity_id = ~{client}})
    | _ -> none

  /**
   * Handles the send request
   */
  send =
    | {Record = [("to", to),("message", message)|then_]} ->
      ThreadContext.get_opt({current}) ?|> context ->
      unserialize(context, to) ?|> identity ->
      OpaChannel.find(identity) ?|> channel ->
      do OpaChannel.forward(context, channel, message)
      {none}
    | _ -> @fail

  remove(id) = OpaChannel.remove({entity_id = id})


  @server_private
  parser_(winfo:web_info) =
    bad_formatted() =
      do WebInfo.simple_reply(winfo, "Bad formatted request", {unauthorized})
      {none}
    request = winfo.http_request.request
    jbody() = Json.of_string(%%BslNet.Requestdef.get_request_message_body %%(request))
    parser
    | "register"   -> jbody() ?|> (
        | {Record = [("to_register", _to_register),
                     ("uri", {String = uri}),
                     ("body", {String = body}),
                    ]} ->
          {some = {winfo with
             http_request.request = %%BslNet.Requestdef.request_with%%(request, uri, body)
          }}
        | _ -> bad_formatted()
      )
    | "send"       -> jbody() ?|> send
    | "remove"     -> jbody() ?|> remove
    | "sharedaddr" -> @fail

}}
