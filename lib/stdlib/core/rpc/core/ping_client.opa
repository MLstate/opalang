/*
    Copyright © 2011, 2012 MLstate

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

  register_actor(id) =
    do @atomic(
      x = Reference.get(registers)
      Reference.set(registers, [{String = id} | x])
    )
    Log.debug("RA", Reference.get(registers))

  @private
  wrap_request(url, body, f) =
   match @atomic(
     x = Reference.get(registers)
     do Log.debug("WR0", x)
     do Reference.set(registers, [])
     do Log.debug("WR1", x)
     x)
   with
   | [] ->
     do Log.debug("WR2", "No register")
     f(url, body)
   | registers ->
     do Log.debug("WR2", registers)
     msg = {Record = [
       ("to_register", {List = registers}),
       ("url", {String = "{%%Session.PingRegister.internal_prefix%%}{url}"}),
       ("body", {String = body}),
     ]}
     f("/chan/register", Json.to_string(msg))

  @private
  set_process_msg(f:RPC.Json.json -> bool):void =
    (%%Session.PingRegister.process_msg%%:(RPC.Json.json -> bool) -> void)(f)

  @private
  _x : void = set_process_msg(process_msg)

  @private
  process_msg(json:RPC.Json.json):bool =
    error(msg) = do error(msg) false
    match json with
    | {Record = [("type", {String = "chan"}), ("id", id), ("msg", message)]} ->
      match Channel.unserialize(id) with
      | {none} -> error("Can't unserialize {id}")
      | {some = channel} ->
        serialize(_) = @toplevel.error("Should not happends")
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

}}
#<End>
