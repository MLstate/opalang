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
 * A widget for displaying a duration between now and a given date
 * (printed such as "in 5 days", "2 hours ago" etc.).
 * The text is automatically updated as the time passes by.
 *
 * @author Adam Koprowski, 2010
 * @author Frederic Ye, 2011
 * @category widget
 * @destination public
 * @stability experimental
 */

/**
 * {1 Types defined in this module}
 */

type WDatePrinter.config = {
     duration_printer : Duration.printer
     title_printer : Date.date -> string
     css_class : string

    /**
     * Whether or not the initial date comes from the server, and if it is to
     * be cached on the client (useful in order to avoid countless
     * client-server exchanges).
     */
     server_date : { disable } / { enable } / { cache }
}

WDatePrinter = {{

  /**
   * {1 Configuration}
   */

  default_config : WDatePrinter.config = {
    duration_printer = Duration.default_printer
    title_printer = date -> Date.to_formatted_string(Date.default_printer, date)
    css_class = ""
    server_date = {cache}
  }

  /**
   * {1 High-level interface}
   */

  html(config : WDatePrinter.config, id : string, date : Date.date) : xhtml =
    title = config.title_printer(date)
    <span id=#{get_widget_id(id)} class="{config.css_class}" title="{title}"
        onready={_ ->
            shift =
              match config.server_date with
                | {disable} -> Duration.empty
                | {enable}  -> Duration.between(Date.now(), now_server())
                | {cache}   -> Duration.between(Date.now(), now_server_cache())
            update(config, id, Date.shift_backward(date, shift), ( -> void))} />

  /**
   * {1 Private functions}
   */

  /*
   * Server time
   */
  @private @server now_server(): Date.date = Date.now()

  /*
   * Client cache containing the server time
   */
  @private @client date_cache: Cache.sync(void, Date.date, void) =
    Cache.make(Cache.Negotiator.always_necessary(_ -> now_server()),
          {Cache.default_options with age_limit = some(Duration.s(10))})

  /*
   * The server time cached on the client
   */
  @private @client now_server_cache(): Date.date =
    date_cache.get(void)

  @private
  @both_implem
  get_widget_id(id : string) : string =
    "{id}_dp" // dp, for date-printer

  @private
  get_text(config, now, date) =
    duration = Duration.between(now, date)
    Duration.to_formatted_string(config.duration_printer, duration)

  /*
   * This function tries to figure out the next interval
   * after which we should refresh the output (because
   * the text will change at that point). Since it's not
   * very straightforward to compute such interval directly
   * (for arbitrary Duration.printer format), we do that
   * by performing binary search. Neat, huh? :)
   */
  @private
  get_update_interval(config, text, printed_date, now) =
    // minimal interval we consider is 1sec.
    min = Date.advance(now, Duration.s(1))
    // and the maximal one is 1 hour
    max = Date.advance(now, Duration.h(1))
    check(at, range) =
       // we are happy with a resolution of 10 milliseconds
      if DateRange.length(range) < Duration.ms(10) then
        {eq}
      else
        at_text = get_text(config, at, printed_date)
        if at_text == text then
          {gt} // the text at [at] does not change, we need larger interval
        else
          {lt}
    date_to_update = Date.binary_search(check, DateRange.between(min, max)) ? min
    Duration.between(now, date_to_update)

  @private
  update(config, id, date, stop) =
//    do jlog("[{Date.to_string(Date.now())}] Performing update of date: {Date.to_string(date)}")
    dom = Dom.select_id(get_widget_id(id))
    do stop()
    if Dom.is_empty(dom) then
      void
    else
      now = Date.now()
      text = get_text(config, now, date)
      _ = Dom.put_inside(dom, Dom.of_xhtml(<>{text}</>))
      interval = get_update_interval(config, text, date, now)
      rec val timer = Scheduler.make_timer(
        Duration.in_milliseconds(interval),
        -> update(config, id, date, timer.stop)
      )
      do timer.start()
      void

}}
