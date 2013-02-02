/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
