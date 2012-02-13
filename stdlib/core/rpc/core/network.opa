/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

/**
 * {1 About this module}
 *
 * An infrastructure for broadcasting information to observers. Observers may
 * be sessions or functions,
 * located on the same machine, or on any client or server.
 *
 * This infrastructure performs automated garbage-collection when required.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

import stdlib.core.{set}

/**
 * {1 Types defined in this module}
 */

/**
 * The type of networks.
 *
 * To create a network, use {!Network.empty}
 */
type Network.network('a) = channel(Network.instruction('a))

type Network.instruction('a) = {add: channel('a)} / {remove: channel('a)} / {broadcast: 'a}

/**
 * Representation of an observer.
 *
 * It can be used to unregister the observer.
 */
@abstract type Network.observer = -> void


/**
 * Layered Networks
 *
 * Like networks, but networks can be observer and act as proxys
 *
 * Kept private because very experimental (used for distribution)
 */
@private type Network.Layered.instruction('a) =
    Network.instruction('a)
  / {add_networks: list(Network.Layered.network('a))}
  / {shallow_broadcast: 'a}
@private type Network.Layered.network('a) = channel(Network.Layered.instruction('a))

/**
 * {1 Interface}
 */

Network = {{
   /**
    * Create an empty network.
    */
   empty(): Network.network('a) =
      Session.NonBlocking.make(ChannelSet.empty,
         (msg, o ->
            match msg with
              | ~{add} ->
                  do Session.on_remove(add, (-> o.update(ChannelSet.remove(add, _))))
                  o.update(ChannelSet.add(add, _))
              | ~{remove} ->
                  o.update(ChannelSet.remove(remove, _))
              | ~{broadcast} ->
                  chans = o.get()
                  ChannelSet.iter(chan -> Session.send(chan, broadcast), chans)
         )
      )


   /**
    * Construct a network that is automatically shared between servers.
    *
    * Note: Automatic sharing between servers is activated only if your
    * application is executed with the "cloud" option (--cloud or the
    * cloud launch script).
    *
    * When one or several servers invoke [Network.cloud(k)] with the same value [k], only one
    * network is actually created, on one of the participating servers (chosen arbitrarily) and
    * shared between the servers.
    *
    * @param key A key. The value of the key serves to decide sharing, e.g. if a shared [network] has already
    * been created with [key], subsequent calls to [Network.cloud(key)], on any server, will return the same [network].
   **/
   cloud(key : string) : Network.network('a) =
     if Session.cloud_mode
     then make_distributed(key)
     else make_generic(Session.cloud(key, _, _))


   /**
    * Share a network between servers.
    *
    * When one or several servers invoke [Network.make_shared(k)] with the same value [k], only one
    * network is actually created, on one of the participating servers (chosen arbitrarily) and
    * shared between the servers.
    *
    * @param key A key. The value of the key serves to decide sharing, e.g. if a shared [network] has already
    * been created with [key], subsequent calls to [make_shared] will return the same [network].
    */
   @deprecated({use="Network.cloud"}) make_shared(key: string): Network.network('a) =
   (
      make_generic((init, handler -> Session.make_shared(key, init, handler)))
   )


   make_at(location: channel(_)): Network.network('a) =
   (
      make_generic((init, handler -> Session.make_at(init, handler, location)))
   )

   /**
    *
    */
  @private make_generic(maker:('state, ('state, Network.instruction('message) -> Session.instruction('state)) -> channel(Network.instruction('message)))):Network.network('message) =
  (
      rec val me  =
        maker(ChannelSet.empty,
          chans, msg ->
            match msg with
            | {~add}       ->
              do Session.on_remove(add, (-> remove(add, me)))
              {set = ChannelSet.add(add, chans)}
            | {~remove}    ->
              {set = ChannelSet.remove(remove, chans)}
            | {~broadcast} ->
              do sleep(0, -> ChannelSet.iter(chan -> send(chan, broadcast), chans))//Note: sending asynchronously
              {unchanged}                                                   //to minimize critical section
        )
      me
  )

  make_distributed(key : string) : Network.network('a) =
    hn = Layered.make_distributed(key)
    Session.NonBlocking.map((msg : Network.instruction('a)) -> (msg <: Network.Layered.instruction('a)), hn)


   /**
    * Add a channel to a network.
    *
    * @param channel A new channel to add to a network. If the channel has been added already,
    * the second addition is ignored.
    * @param network A network, possibly empty.
    */
   add(channel: channel('a), network: Network.network('a)): void =
     Session.send(network, {add = channel})

   /**
    * Add an observer function to a network.
    *
    * This is a convenience function. Generally, you should rather use the more powerful [observe].
    *
    * @param f A function to add to a network. This function will be called during each and any broadcast.
    * @param network A network, possibly empty.
    */
   add_callback(f: 'a -> void, network: Network.network('a)): void =
     Session.send(network, {add = Session.NonBlocking.make_callback(f)})

   /**
    * Add an observer function to a network.
    *
    * @param f A function to add to a network. This function will be called during each and any broadcast.
    * @param network A network, possibly empty.
    * @return An object which may be used at a later stage to remove the function from the network.
    */
   observe(f: 'a -> void, network: Network.network('a)): Network.observer =
   (
     c = Session.NonBlocking.make_callback(f)
     observer() = remove(c, network)
     do send(network, {add = c})
     observer
   )

   /**
    * Add an observer function to a network.
    *
    * @param f A function to add to a network. This function will be called during each and any broadcast.
    * @param on_disconnect A function invoked when the observer is removed for some reason.
    * @param network A network, possibly empty.
    * @return An object which may be used at a later stage to remove the function from the network.
    */
   observe_and_inform(f: 'a -> void, on_disconnect: -> void, network: Network.network('a)): Network.observer =
   (
     c = Session.NonBlocking.make_callback(f)
     do Session.on_remove(c, on_disconnect)
     observer() = remove(c, network)
     do send(network, {add = c})
     observer
   )

   /**
    * Remove an observer from the network.
    */
   unobserve(observer: Network.observer) =
   (
     observer()
   )

   /**
    * Remove a channel from a network.
    *
    * @param channel A channel to remove from a network. If the channel does not appear in the network,
    * this is ignored.
    * @param network A network, possibly empty.
    */
   remove(channel: channel('a), network: Network.network('a)): void =
     Session.send(network, {remove = channel})

   /**
    * Send a message to all channels of a network.
    *
    * Sending is asynchronous. The order in which channels receive the message is unspecified.
    *
    * @param message A message to send to all channels.
    * @param network A network, possibly empty.
    */
   broadcast(message: 'a, network: Network.network('a)): void =
     Session.send(network, {broadcast = message})

   /**
    * Warning: EXPERIMENTAL
    *
    * Create a network out of a first one, with modified broadcast messages.
    */
   map(f : 'a -> 'b, nw : Network.network('a)) : Network.network('b) =
     net = Network.empty()
     _ = observe((a -> broadcast(f(a), nw)), net)
     net

  @private Layered = {{

    empty() : Network.Layered.network('a) =
      Session.NonBlocking.make(
        { channels = ChannelSet.empty : channelset('a);
          networks = ChannelSet.empty : channelset(Network.Layered.instruction('a)) },
        (msg, o ->
          update_channels(f) = o.update(s -> {s with channels = f(s.channels)})
          update_networks(f) = o.update(s -> {s with networks = f(s.networks)})
          match msg with
            | {add=c} ->
              do Session.on_remove(c, (-> update_channels(ChannelSet.remove(c, _))))
              update_channels(ChannelSet.add(c, _))
            | {add_networks=nws} ->
              do List.iter(nw -> Session.on_remove(nw, (-> update_networks(ChannelSet.remove(nw, _)))), nws)
              update_networks(List.fold(ChannelSet.add, nws, _))
            | {remove=c} ->
              update_channels(ChannelSet.remove(c, _))
            | {broadcast=msg} ->
              do ChannelSet.iter(Session.send(_, msg), o.get().channels)
              do ChannelSet.iter(Session.send(_, {shallow_broadcast=msg}), o.get().networks)
              void
            | {shallow_broadcast=msg} ->
              do ChannelSet.iter(Session.send(_, msg), o.get().channels)
              void
        )
      )

    make_distributed(key : string) : Network.Layered.network('a) =
      shared = Session.cloud(key, [] : list(Network.Layered.network('a)),
        (set, (c : Network.Layered.network('a)) ->
          do Scheduler.push(-> List.iter(Session.send(_, {add_networks=[c]}), set))
          do Scheduler.push(-> Session.send(c, {add_networks=set}))
          {set = [c | set]}
        )
      ) : channel(Network.Layered.network('a))
      local = empty() : Network.Layered.network('a)
      do Session.send(shared, local)
      local

  }}

}}
