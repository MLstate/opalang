















@server_private
WebSession = {{


  parser_(winfo:web_info) =
    bad_formatted() =
      do WebInfo.simple_reply(winfo, "Bad formatted request", {unauthorized})
      {none}
    ok() =
      do WebInfo.simple_reply(winfo, "", {success})
      {none}
    failure() =
      do WebInfo.simple_reply(winfo, "Failure", {unauthorized})
      {none}
    `?|>`(a,f) =
      match a with
      | {none} -> failure()
      | {some = x} -> f(x)
    request = winfo.http_request.request
    jbody() = Json.of_string(%%BslNet.Requestdef.get_request_message_body %%(request))
    parser
    | "register"   ->
        ThreadContext.Client.get_opt({current}) ?|> client ->
        jbody() ?|> (
        | {Record = [("to_register", {List = to_register}),
                     ("url", {String = url}),
                     ("body", {String = body}),
                    ]} ->
          do List.iter(
            | {String = id} -> Channel.register({other = id}, ~{client})
            | j -> Log.error("WebSession", "Bad register : {j}")
            , to_register)
          {some = {winfo with
             http_request.request = %%BslNet.Requestdef.request_with%%(request, url, body)
          }}
        | _ -> bad_formatted()
      )
    | "send"       -> jbody() ?|> (
        | {Record = [("to", to),("message", message)|then_]} ->
          ThreadContext.get_opt({current}) ?|> context ->
          Channel.unserialize(to) ?|> channel ->
          serialize(_) = error("Should not happends")
          do Channel.forward(some(context), channel, message, ~{serialize message})
          ok()
        | _ -> bad_formatted()
      )
    | "remove"     -> jbody() ?|> (
        | {String = cid} ->
          do Channel.remove({entity_id = {other = cid}})
          ok()
        | _ -> bad_formatted()
      )
    | "sharedaddr" -> @fail

}}
