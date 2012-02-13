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
