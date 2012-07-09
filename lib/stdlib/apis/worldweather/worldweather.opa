/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/*
 * Author    : Adam Koprowski <adam.koprowski@mlstate.com>
 **/

import stdlib.web.client

/**
 * An API to World Weather Online provider of weather forecasts.
 *
 * @author Adam Koprowski, 2010
 * @category api
 * @destination public
 */

/**
 * {1 About this module}
 *
 * This module provides an API to obtain weather forecasts from the World
 * Weather Online provider.
 *
 * {1 Where should I start?}
 *
 * Take a look at the {!WorldWeather.get_weather} function which requests
 * a weater forecasts and calls a provided callback once the weather is
 * available.
 */

/**
 * A configuration of the forecast request.
 * - [api_key] is the key API obtained from World Weather Online
 * - [num_of_days] is the number of days of the forecast (which will
 *   always begin as of today).
**/
type WorldWeather.config = {
  api_key : string
  num_of_days : int
}

/**
 * A single day weather result.
 * - [date] a date of the forecast
 * - [tempMin] minimal temperature (in C)
 * - [tempMax] maximal temperature (in C)
 * - [iconUrl] an URL to the icon representing the weather forecast for the day
 * - [desc] a textual description (in English) of the weather forecast for the day
**/
type WorldWeather.daily_weather = {
  date : Date.date
  tempMin : int
  tempMax : int
  iconUrl : string
  desc : string
}

/**
 * A weather report.
 * - [location] a location for which the weather forecast was prepared. Note that
 *   this does *not* need to be the same as in the request. For instance if making
 *   a request for city only (which may be ambiguous) this field will usually contain
 *   more information (i.e. city + country).
 * - [weather_data] a list of daily weather reports.
**/
type WorldWeather.weather_report = {
  location : string
  weather_data : list(WorldWeather.daily_weather)
}

/**
 * @private
**/
type WorldWeather.request = {
  location : string
  config : WorldWeather.config
  callback : option(WorldWeather.weather_report) -> void
}

WorldWeather = {{

  @private cache_expiry = Duration.min(15)
  @private cache_gc_interval = Duration.min(30)

  @private @server rec val cache_channel =
    Session.make({last_gc = Date.epoch; cache = StringMap.empty},
                     s_onmessage)

  @private date_scanner = Date.generate_scanner("%Y-%m-%d")

  // we periodically perform garbage collection on the cache
  @private @server garbage_collection(cache_channel)=
    interval_ms = Duration.in_milliseconds(cache_gc_interval)
    garbage_collection() = Session.send(cache_channel, {gc})
    Scheduler.timer(interval_ms, garbage_collection)

  @private cache_expired(cached_at) =
    cache_delay = Duration.between(Date.now(), cached_at)
    Duration.is_negative(Duration.add(cache_delay, cache_expiry))

  @private s_onmessage(~{last_gc cache}, msg) =
    do if last_gc == Date.epoch then garbage_collection(cache_channel)
    match msg with
    | {gc} ->
        check_entry((cached_at, val)) =
          if cache_expired(cached_at) then
            none
          else
            some((cached_at, val))
        new_cache = StringMap.filter_map(check_entry, cache) // FIXME, we just need StringMap.filter here
        do jlog("GC WorldWeatherOnline cache from {StringMap.size(cache)} to {StringMap.size(new_cache)} values")
        {set = {last_gc=Date.now(); cache=new_cache}}
     // a request to update cache
    | {update = ~{location response}} ->
        cache_entry = (Date.now(), response)
        {set = {~last_gc cache=StringMap.add(location, cache_entry, cache)}}
     // a request to check cache
    | {check = ~{location callback : _ -> void}} ->
        do
          match StringMap.get(location, cache) with
          | {none} ->
              callback(none)
          | {some=(cached_at, response : option(WorldWeather.weather_report))} ->
              if cache_expired(cached_at) then
                 // cache expired -- say value is not cached
                callback(none)
              else
                 // use cached response
                callback(some(response))
        {unchanged}

  @private ask_for_weather(location, config, callback) =
    format = "xml"
    api_key = config.api_key
    ndays = config.num_of_days
    uri =
      { Uri.default_absolute with
        domain = "free.worldweatheronline.com"
        path = ["feed", "weather.ashx"] ;
        query = [("q", location), ("format", format), ("key", api_key), ("num_of_days", "{ndays}")]
      } <: Uri.uri
    res = match WebClient.Get.try_get(uri) with
      | {success = { ~content ... }} -> some(content)
      | _ -> none
    callback(res)

  @private decode_weather(s) =
    doc = Xmlns.try_parse_document(s)
    Option.switch((doc -> Xml_parser.try_parse(world_weather_parser, doc.element)), none, doc)

  @private world_weather_parser =
    request = xml_parser
      <request>
        <type>_*</>
        <query>query={Xml.Rule.string}</>
      </> -> query
    current = xml_parser <current_condition>_*</current_condition> -> void
    entry = xml_parser
      <weather>
        <date>dateStr={Xml.Rule.string}</>
        <tempMaxC>tempMax={Xml.Rule.integer}</>
        <tempMaxF>_*</>
        <tempMinC>tempMin={Xml.Rule.integer}</>
        <tempMinF>_*</>
        <windspeedMiles>_*</>
        <windspeedKmph>_*</>
        <winddirection>_*</>
        <winddir16Point>_*</>
        <winddirDegree>_*</>
        <weatherCode>_*</>
        <weatherIconUrl>iconUrl={Xml.Rule.string}</>
        <weatherDesc>desc={Xml.Rule.string}</>
        <precipMM>_*</>
      </weather>
      -> date = Date.of_formatted_string(date_scanner, dateStr) ? Date.epoch
         ~{date tempMin tempMax iconUrl desc}
    xml_parser <data>location={request} _current={current} weather={entry}+</data> -> {~location weather_data=List.rev(weather)}

  @private weather_callback(location, config, callback) =
   // value is cached -- simply use the cached value
  | {some=response} -> callback(response)
   // value not cached -- we need to fetch it
  | {none} ->
      cb(s) =
        doc = Option.bind(Xmlns.try_parse_document, s)
        response = Option.switch((doc -> Xml_parser.try_parse(world_weather_parser, doc.element)), none, doc)
         // update cache
        do Session.send(cache_channel, {update = ~{location response}})
         // call client callback
        callback(response)
      ask_for_weather(location, config, cb)

  /**
   * This function given an API key for World Weather Online, creates a default
   * configuration.
  **/
  default_config(api_key : string) : WorldWeather.config =
    { ~api_key; num_of_days = 4}

  /**
   * Request for a weather forecast
   *
   * @param location location of the forecast request ("city" or "city, country")
   * @param config a configuration of the request, see {!WorldWeather.config}.
   * @param callback a callback function which will be called (with the weather
   *        report as its only parameter) once the weather forecast was obtained.
  **/
  get_weather(location : string, config : WorldWeather.config, callback : option(WorldWeather.weather_report) -> void) : void =
     // first, check the cache
    Session.send(cache_channel, {check = {~location callback=weather_callback(location, config, callback)}})

}}
