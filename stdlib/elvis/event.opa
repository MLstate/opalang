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
 * Elvis Events
 *
 * @category UI
 * @author David Rajchenbach-Teller, 2011
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */

/**
 * {1 General definitions}
 */

@abstract type Event.source('a) = Network.network('a)
@abstract type Event.observer   = (-> void)

Event =
{{
  Source =
  {{
    /**
     * Create a source that won't ever send anything.
     */
    empty(): Event.source('a) = (Network.empty())
  }}

  /**
   * Register to be informed whenever a source produces a new event
   */
  observe(source: Event.source('a), sink: channel('a)): Event.observer =
  (
     do Network.add(sink, (source))
     (-> Network.remove(sink, (source)))
  )
  /**
   * As [observe], but with a function
   */
  callback(source: Event.source('a), sink: 'a -> void): Event.observer =
  (
     observe(source, Session.make({}, (_, msg -> do sink(msg) {unchanged})))
  )
  /**
   * As [callback], but with the function is automatically unregistered once it has been called
   */
  callback_once(source: Event.source('a), sink:   'a -> void): Event.observer =
  (
     c = Session.make({}, (_, msg -> do sink(msg) {stop}))
     observe(source, c)
  )

  /**
   * Broadcast an event to all observers
   */
  trigger(source: Event.source('a), event:'a): void =
  (
      Network.broadcast(event, (source))
  )

  unobserve(observer: Event.observer): void =
  (
     (observer)()
  )
}}
