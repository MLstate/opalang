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
