/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#<Ifstatic:OPA_FULL_DISPATCHER>

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
    jbody() = Json.of_string(Binary.to_string(%%BslNet.Requestdef.get_bin_body%%(request)))
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
        | {Record = [("to", to),("message", message)|_then_]} ->
          ThreadContext.get_opt({current}) ?|> context ->
          Channel.unserialize(to) ?|> channel ->
          serialize(_) = error("Should not happen")
          do Channel.forward(some(context), channel, message, ~{serialize message})
          ok()
        | _ -> bad_formatted()
      )
    | "remove"     ->
      ThreadContext.Client.get_opt({current}) ?|> client ->
      jbody() ?|> (
        | {String = cid} ->
          do Channel.client_remove(client, cid)
          ok()
        | _ -> bad_formatted()
      )
    | "sharedaddr" -> @fail

}}
#<End>
