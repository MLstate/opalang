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
#<Ifstatic:OPA_BACKEND_QMLJS>
#<Else>
/*
    @authors Raja Boujbel, 2010
**/
import-plugin hlnet
import stdlib.core.{web.core, security.ssl}


/**
  An OPA interface of the high-level Network layer

  @author Raja Boujbel, 2010
  @author Louis Gesbert (interface change + documentation), 2011
*/

/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

type endpoint = external
@abstract type hlnet_channel('out, 'in) = external
@abstract type hlnet_channel_spec('out,'in) = external

/** The abstract type of channels where we send values of type ['out]
    and receive values of type ['in].
    This type actually only describes the local half of a channel:
    on the other side of the connection, the same channel will be
    seen as an [Hlnet.channel('in,'out)] */
type Hlnet.channel('out,'in) = hlnet_channel('out,'in)

/** Type of what should be provided to define a safe ('out,'in) channel
    (includes a service name, serialization and deserialization functions */
type Hlnet.channel_spec('out,'in) = hlnet_channel_spec('out,'in)

/** An [endpoint] is one end of a channel. It may be local or remote. */
type Hlnet.endpoint = endpoint

type Hlnet.peerpoint.protocol = {tcp} / {udp} / {ssl}
type Hlnet.peerpoint = {
     protocol : Hlnet.peerpoint.protocol
     addr : string
     port : int
}

/** A type useful for encapsulating the definitions of server and client
    channel specifications, when they are defined in the same source. */
type Hlnet.protocol('query,'response) = {
  client_spec: Hlnet.channel_spec('query,'response);
  server_spec: Hlnet.channel_spec('response,'query);
}

/** Type that describes hlnet error. */
type Hlnet.error =
  {disconnected : Hlnet.endpoint}

/**
 * {1 Interface}
 */

Hlnet =
{{

  /** Creates an endpoint from an address and port */
  new_endpoint(addr: ip, port: int) =
    new = %%Hlnet.new_endpoint%%
    ip : string = IPv4.string_of_ip(addr)
    new(ip, port)

  /** Creates an endpoint from an address and port */
  new_ssl_endpoint(addr: ip, port: int, secure: SSL.secure_type) =
    new = %%Hlnet.new_ssl_endpoint%%
    ip : string = IPv4.string_of_ip(addr)
    new(ip, port, secure)

  /** Builds a new {!Hlnet.channel_spec}, needed for creating channels. Two halves of a channel
      (resp. on a client and on a server) can only be connected together if they use the
      same service name and version.

      {b Warning}: the name of the service can not exceed 12 chars,
      and the version number can not exceed 9999 */
  make_channel_spec(
    name: string,
    version: int,
    serialise: 'out -> string,
    unserialise: Hlnet.channel('out,'in), string -> option('in)
  ) : Hlnet.channel_spec('out,'in)
    =
    bp = %%Hlnet.make_channel_spec%%
    bp(name,version,serialise,unserialise)

  /** Consistently defines the specs for both ends of a channel.

    {b Guideline:} always explicitly coerce the return value of define_protocol,
    to make the types used in the protocol visible. This will really help understanding
    what your code does. */
  define_protocol(
    name: string, version: int,
    serialise_query: 'query -> string,
    unserialise_query: Hlnet.channel('response,'query), string -> option('query),
    serialise_response: 'response -> string,
    unserialise_response: Hlnet.channel('query,'response), string -> option('response)
  ) : Hlnet.protocol('query,'response)
    = {
    client_spec = make_channel_spec(name,version,serialise_query,unserialise_response);
    server_spec = make_channel_spec(name,version,serialise_response,unserialise_query);
  }

  /** {6 Opening channels (client side)} */

  /** Creates a channel connected to a remote endpoint. The other hand needs to
      [accept] channels with the same service using {!Hlnet.accept} */
  open_channel(endpoint: endpoint, spec: Hlnet.channel_spec) =
    opchan = %%Hlnet.open_channel%%
    opchan(endpoint,spec)

  /** {6 Sending on channels} */

  /** Sends a packet on a channel, to be handled by a remote {!Hlnet.receive} or
      {!Hlnet.setup_receive} */
  send(chan: Hlnet.channel('a, 'b), msg: 'a) =
    snd = %%Hlnet.send%%
    snd(chan, msg)

  /** Sends a message on a channel, to be handled by a remote
      {!Hlnet.setup_respond}, and returns the response of the server. This is {b not} equivalent
      to [send] then [receive], with which you wouldn't be guaranteed to get the answer
      corresponding to your question.
      Blocking function. */
  sendreceive(chan: Hlnet.channel('a, 'b), msg: 'a) : 'b =
    sndrcv = %%Hlnet.sendreceive%%
    sndrcv(chan, msg)

  sendreceiverr = %%Hlnet.sendreceiverr%% : Hlnet.channel('a, 'b), 'a -> outcome('b, Hlnet.error)


  /** {6 Receiving on channels and setting up handlers} */

  /** The default local endpoint, for listening on all addresses */
  default_endpoint = %%Hlnet.default_endpoint%%

  /** Prepare the socket for receiving new channels on the given local endpoint
      (they can already be received, but will only be queued until you call [accept]) */
  listen(endpoint: endpoint) =
    list = %%Hlnet.listen%%
    list(endpoint)

  /** Setup a function to call every time a channel looking for the service [spec] is
      opened to the current host.
      You would usually use it to do a [setup_respond] on the given channel. */
  accept(endpoint: endpoint, spec: Hlnet.channel_spec('out,'in), handler: Hlnet.channel('out,'in) -> void) : void =
    acc = %%Hlnet.accept%%
    acc(endpoint, spec, handler)

  /** Cancel any previous call to [listen] or [accept] on the given local endpoint,
      refusing the opening of new channels from remote parties. */
  refuse(endpoint: endpoint) =
    ref = %%Hlnet.refuse%%
    ref(endpoint)

  /** Receives a single incoming message on a channel. Blocking function. */
  receive(chan: Hlnet.channel('a, 'b)) : 'b =
    rcv = %%Hlnet.receive%%
    rcv(chan)

  /** Sets up a function to handle all messages incoming on the given channel
      (except the ones that would be bound to a specific treatment, eg. responses
      to a local [sentreceive]).
      For use when the remote uses [send]. */
  setup_receive(chan: Hlnet.channel('a, 'b), handler: 'b -> void) : void =
    bp = %%Hlnet.setup_receive%%
    bp(chan,handler)

  /** Sets up a function to handle all messages incoming on the given channel, and
      respond back to them.
      For use when the remote uses [sendreceive]. */
  setup_respond(chan: Hlnet.channel('out, 'in), handler: 'in -> 'out) : void =
    setrsp = %%Hlnet.setup_respond%%
    setrsp(chan, handler)


  /** {6 Manipulating channels} */

  /** Close an open channel. Not that this can be done automatically
      by the GC if you don't hold any references to the channel anymore
      (also, the server will be informed) */
  close_channel(chan: Hlnet.channel('a, 'b)) =
    close = %%Hlnet.close_channel%%
    close(chan)

  /** Create a new channel from an existing one, with the same ends
      but possibly a different service.

      This is convenient to change state during the course of a
      communication protocol:
      - create a new channel using [dup]
      - setup a handler on it ([setup_respond])
      - send the new channel back to the other hand, so that it can
      use it to contact you on the new handler ([serialise_channel]
      and [unserialise_remote_channel]). */
  dup(chan: Hlnet.channel('out0, 'in0), spec: Hlnet.channel_spec('out,'in)) : Hlnet.channel('out, 'in) =
    dp = %%Hlnet.dup%%
    dp(chan, spec)

  /** Sets up a handler on a new channel, duplicated from the given one. For
      sending back to the other hand */
  dup_and_respond : Hlnet.channel('o0,'i0), Hlnet.channel_spec('o,'i), ('i -> 'o) -> Hlnet.channel('i,'o) =
    %%Hlnet.respond_on_new_channel%%

  /** Returns the local endpoint of an open channel */
  local_endpoint(chan) =
    bp = %%Hlnet.local_endpoint%%
    bp(chan)

  /** Returns the remote endpoint of an open channel */
  remote_endpoint(chan) =
    bp = %%Hlnet.remote_endpoint%%
    bp(chan)

  /** Utility function to make sending channels through a communication protocol possible */
  serialise_channel(chan: Hlnet.channel('out,'in)): string =
    bp = %%Hlnet.serialise_channel%%
    bp(chan)

  /** For use on a received serialized channel: unserializes it and registers
      it locally with the given [spec] (which service name should match that of the
      serialised channel). The carrier channel is asked as an argument as well, but only
      used for some auxiliary connection information. */
  unserialise_remote_channel(spec: Hlnet.channel_spec('out,'in), aux_chan: Hlnet.channel('out0,'in0), s: string)
    : option(Hlnet.channel('out,'in))
    =
    bp = %%Hlnet.unserialise_remote_channel%%
    bp(spec, aux_chan, s)

  /** Returns [true] if the channel is live and connected */
  channel_is_open(chan: Hlnet.channel('a,'b)) : bool =
    bp = %%Hlnet.channel_is_open%%
    bp(chan)

  /** Checks if there is any handler set up on the given channel */
  channel_is_handled(chan: Hlnet.channel('a,'b)) : bool =
    ex = %%Hlnet.channel_exists%%
    ex(chan)

  /** Debug function */
  channel_to_string(chan: Hlnet.channel('a,'b)) : string =
    tostr = %%Hlnet.channel_to_string%%
    tostr(chan)


  /** {6 Asynchronous versions} of the blocking functions above */

  async_receive(chan: Hlnet.channel('a, 'b), handler: 'b -> void) :void =
    rcv = %%Hlnet.async_receive%%
    rcv(chan, handler)

  async_sendreceive(chan: Hlnet.channel('a, 'b), msg: 'a, handler : 'b -> void) : void =
    sndrcv = %%Hlnet.async_sendreceive%%
    sndrcv(chan, msg, handler)

  @private endpoint_protocol = %%Hlnet.EndpointGet.protocol%% : endpoint -> string
  @private endpoint_addr     = %%Hlnet.EndpointGet.addr%%     : endpoint -> string
  @private endpoint_port     = %%Hlnet.EndpointGet.port%%     : endpoint -> int

  /** {6 Peerpoints} */

  /** Converting a endpoint to a peerpoint */
  @private
  endpoint_to_peerpoint(ep:Hlnet.endpoint):Hlnet.peerpoint=
    protocol =
      match endpoint_protocol(ep)
      "tcp" -> {tcp}
      "udp" -> {udp}
      "ssl" -> {ssl}
      _     -> error("Hlnet.endpoint_to_peerpoint : unknown protocol")
      end
    {~protocol
     addr     = endpoint_addr(ep)
     port     = endpoint_port(ep)}

  // Value restriction fake positive here !!!
  /** Returns the remote peerpoint of a channel */
  remote_peerpoint = @nonexpansive(remote_endpoint @> endpoint_to_peerpoint)
  /** Returns the local peerpoint of a channel */
  local_peerpoint  = @nonexpansive(local_endpoint  @> endpoint_to_peerpoint)

}}
#<End>
