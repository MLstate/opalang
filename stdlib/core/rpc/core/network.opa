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
 * {1 Interface}
 */

//Note: For the moment, [Network] is meaningful only on the server, as the client-side implementation of channels cannot provide a stable ordering of channels
Network = {{
   /**
    * Create an empty network.
    */
   @publish empty(): Network.network('a) =
      Set = Set_make(channel_order)
      me: Network.network('a) = Session.NonBlocking.make(Set.empty,
         (msg, o ->
            match msg with
              | ~{add} ->
                  do Session.on_remove(add, (-> o.update(Set.remove(add, _))))
                  o.update(Set.add(add, _))
              | ~{remove} ->
                  o.update(Set.remove(remove, _))
              | ~{broadcast} ->
                  chans = o.get()
                  Set.iter(chan -> send(chan, broadcast), chans)
         )
      )
      me


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
     make_generic((init, handler -> Session.cloud(key, init, handler)))



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
      Set = Set_make(channel_order)
      rec val me  =
        maker(Set.empty,
          chans, msg ->
            match msg with
            | {~add}       ->
              do Session.on_remove(add, (-> remove(add, me)))
              {set = Set.add(add, chans)}
            | {~remove}    ->
              {set = Set.remove(remove, chans)}
            | {~broadcast} ->
              do sleep(0, -> Set.iter(chan -> send(chan, broadcast), chans))//Note: sending asynchronously
              {unchanged}                                                   //to minimize critical section
        )
      me
  )

   /**
    * Add a channel to a network.
    *
    * @param channel A new channel to add to a network. If the channel has been added already,
    * the second addition is ignored.
    * @param network A network, possibly empty.
    */
   add(channel: channel('a), network: Network.network('a)):  void    = send(network, {add = channel})

   /**
    * Add an observer function to a network.
    *
    * This is a convenience function. Generally, you should rather use the more powerful [observe].
    *
    * @param f A function to add to a network. This function will be called during each and any broadcast.
    * @param network A network, possibly empty.
    */
   add_callback(f: 'a -> void, network: Network.network('a)): void = send(network, {add = Session.NonBlocking.make_callback(f)})

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
   remove(channel: channel('a), network: Network.network('a)): void  = send(network, {remove = channel})

   /**
    * Send a message to all channels of a network.
    *
    * Sending is asynchronous. The order in which channels receive the message is unspecified.
    *
    * @param message A message to send to all channels.
    * @param network A network, possibly empty.
    */
   broadcast(message: 'a, network: Network.network('a)): void        = send(network, {broadcast = message})

   /**
    * Warning: EXPERIMENTAL
    *
    * Create a network out of a first one, with modified broadcast messages.
    */
   map(f : 'a -> 'b, nw : Network.network('a)) : Network.network('b) =
     net = Network.empty()
     _ = observe((a -> broadcast(f(a), nw)), net)
     net


}}
