/*
    Copyright © 2011-2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
#<Ifstatic:OPA_FULL_DISPATCHER>
@client
PingClient = {{

  @private
  error(msg) =
    Log.error("PingClient", msg)

  @private registers = Reference.create([]): reference(list(RPC.Json.json))

  @package
  register_actor(id) =
    @atomic(
      x = Reference.get(registers)
      Reference.set(registers, [{String = id} | x])
    )

  @private
  wrap_request(url, body, f) =
   match @atomic(
     x = Reference.get(registers)
     do Reference.set(registers, [])
     x)
   with
   | [] ->
     f(url, body)
   | registers ->
     msg = {Record = [
       ("to_register", {List = registers}),
       ("url", {String = "{%%Session.PingRegister.internal_prefix%%}{url}"}),
       ("body", {String = body}),
     ]}
     f("/chan/register", Json.to_string(msg))

  @package
  set_process_msg(f:RPC.Json.json -> bool):void =
    (%%Session.PingRegister.process_msg%%:(RPC.Json.json -> bool) -> void)(f)

  @package
  process_msg(json:RPC.Json.json):bool =
    error(msg) = do error(msg) false
    match json with
    | {Record = [("type", {String = "chan"}), ("id", id), ("msg", message)]} ->
      match Channel.unserialize(id) with
      | {none} -> error("Can't unserialize {id}")
      | {some = channel} ->
        serialize(_) = @toplevel.error("Should not happen")
        do Channel.forward(none, channel, message, ~{serialize message})
        true
      end
    | {Record = [("type", {String = "rpc"}),
                 ("name", {String = name}),
                 ("id",   {String = id}),
                 ("args", {String = args})]} ->
      do OpaRPC_Client.Dispatcher.call(some(id), name, args)
      true
    | {Record = [("type", {String = "asyncrpc"}),
                 ("name", {String = name}),
                 ("args", {String = args})]} ->
      do OpaRPC_Client.Dispatcher.call(none, name, args)
      true
    | _ -> error("I don't understand message : {json}")

  sync_request(url, body) =
    wrap_request(url, body, %%Session.PingRegister.pang_request%%)

  async_request(url, body) =
    wrap_request(url, body, %%Session.PingRegister.ping_async_call%%)

  /**
   * Explicitely set the domain's URL used by AJAX request, no domain by default
   */
  set_domain_url =
    %%Session.PingRegister.set_domain_url%%

  /**
   * Add a custom handler function for the case when the
   * connection between client and server appears to be
   * lost. If they are no custom handler, the default is to print
   * "the connection with the server seems to be lost" to
   * the client screen.
   *
   * Be sure to pass a `client` function here (e.g.
   * `client function my_handler() { ... }`), as it will
   * probably be called when the server is not available!
   */
  @client add_connection_lost_handler =
    %%Session.PingRegister.add_connection_lost_handler%%

}}

@private
@both_implem
_ = @sliced_expr({
  client=PingClient.set_process_msg(PingClient.process_msg)
  server=void
})

#<End>
