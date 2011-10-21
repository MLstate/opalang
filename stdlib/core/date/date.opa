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

import stdlib.core.{parser, map, set}

/**
 * @author Adam Koprowski
 * @category data
 *
 *
 * {1 About this module}
 *
 * This module contains functions to manipulate dates and times.
 * If instead you are interested in time durations (intervals) or date
 * ranges (durations fixed in time), see, respectively, the {!Duration}
 * and {!DateRange} modules.
 *
 * {1 When to use this module}
 *
 * This module will let you manipulate dates, compute the duration between times,
 * etc.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
**/

/* TODO Here we combine daily date & time into one. Sometimes one is interested
        only in one of those (i.e. a date, but not caring about time or a time of
        the day, for on day in particular). How to nicely provide this separation? */
/* TODO time zones */

/**
 * {1 Types defined in this module}
**/

/**
 * The twelve months of the Western calendar.
 */
type Date.month = {january} / {february} / {march} / {april} / {may} / {june} / {july} / {august} / {september} / {october} / {november} / {december}

/**
 * The seven weekdays.
 */
type Date.weekday = {monday} / {tuesday} / {wednesday} / {thursday} / {friday} / {saturday} / {sunday}

/**
 * A representation of a date (i.e. date and time, with millisecond precision).
 *
 * This type is useful for date manipulation. For presentation purposes see {!Date.human_readable}
**/
@abstract type Date.date = int

/**
 * A data-type representing a calendar year
**/
type Date.year = int

/**
 * A data-type representing a day of the month
**/
type Date.day = int

/**
 * A date, split into components (decompressed in a more understandable format.
 */
type Date.human_readable = {
  /** Year since the origin of the calendar (that's 0 AD on most computers) **/
  year : Date.year

  /** Month of the year **/
  month : Date.month

  /** Day of the month (1..31) **/
  day : Date.day

  /** Day of the week **/
  wday : Date.weekday

  /** Hour (0..23) **/
  h : int

  /** Minute (0..59) */
  min : int

  /** Second (0..59) */
  s : int

  /** Millisecond (0..999) */
  ms : int
}

/**
 * A type of date printers, i.e. objects capable of rendering dates as strings
 * (according to some format). See {!Date.try_generate_printer}.
**/
@abstract type Date.printer = (Date.date -> string)

/**
 * A type of date parsers, i.e. objects capable of constructing dates from their
 * string descriptions (according to some format). See {!Date.try_generate_parser}.
**/
type Date.scanner = Parser.general_parser(Date.human_readable -> Date.human_readable)

/**
 * The ordering of dates
**/
type Date.order = Order.default

/**
 * A map from dates to values, using the default ordering on dates.
**/
type Date.map('a) = ordered_map(Date.date, 'a, Date.order)

/**
 * A set of dates.
**/
type Date.set = ordered_set(Date.date, Date.order)

/**
 * A low-level type representing dates. Not for casual users.
**/
// FIXME Should we merge Date.date & time_t ?
type time_t = external

@both Date =
{{

  /**
   * {1 Date constructors}
  **/

  /**
   * The arbitrary date of January 1st, 1970, at 0000
  **/
  epoch : Date.date =
    0

  /**
   * The current time.
   *
   * Note: depending on whether the function is executed on the client or on the
   * server this function will return, respectively, the client/server time.
  **/
  now : -> Date.date = Date_private.time_now

  /**
   * Constructs a specific date.
   *
   * Different level of detail allowed. If milliseconds not provided, set to 0;
   * if all time data not provided set to midnight on a given day.
  **/
  build(date : {year : Date.year; month : Date.month; day : Date.day; h : int; min : int; s : int; ms : int}
             / {year : Date.year; month : Date.month; day : Date.day; h : int; min : int; s : int}
             / {year : Date.year; month : Date.month; day : Date.day}) : Date.date =
    dummy_date = {year=1970 month={january} day=1 h=0 min=0 s=0 ms=0 wday={monday}}
    dhr =
      match date with
      | ~{year month day h min s ms} -> ~{dummy_date with year month day h min s ms}
      | ~{year month day h min s} -> ~{dummy_date with year month day h min s}
      | ~{year month day} -> ~{dummy_date with year month day}
    of_human_readable(dhr)

  /**
   * Converts a low-level representation into a Date.
   * Not for casual users.
  **/
  ll_import : time_t -> Date.date = Date_private.ll_import

  /**
   * Converts an integer to a low-level representation of a Date.
   * Not for casual users.
  **/
  time_t_of_int : int -> time_t =
    v -> Date_private.ll_export(v : Date.date)

  /**
   * {1 Date modifications}
  **/

  /**
   * [Date.round_to_day(date)] returns [date] rounded down to the full day (i.e.
   * it returns a date on midnight of the given day).
  **/
  round_to_day(date : Date.date) : Date.date =
    {~year ~month ~day h=_ min=_ s=_ ms=_ wday=_} = to_human_readable(date)
    of_human_readable({~year ~month ~day h=0 min=0 s=0; ms=0; wday={monday}})

  /**
   * [Date.round_to_second(date)] returns [date] rounded down to a full second
   * (i.e. milliseconds are dropped).
  **/
  round_to_second(date : Date.date) : Date.date =
    {~year ~month ~day ~h ~min ~s ms=_ wday=_} = to_human_readable(date)
    of_human_readable({~year ~month ~day ~h ~min ~s ms=0 wday={monday}})

  /**
   * Advance a time by a fixed-time duration.
   *
   * E.g.: February 2, 2010 + 1 month = March 5, 2010
   * E.g.: February 2, 2012 + 1 month = March 4, 2012
   *
   * @param date Any reference date.
   * @param duration Any duration.
   *
   * @return The date obtain by advancing [date] by [duration]. If [duration] is positive,
   * the result will be after [date], if it is negative, the result will be before [date].
   * If [duration] is infinite, the result will be infinite.
  **/
  advance(date: Date.date, duration: Duration.duration) : Date.date =
    date + duration

  /**
   * Advances a date (as [advance]) by the duration of [days] days.
   *
   * @param date A reference date
   * @param days A number of days (possibly negative)
   *
   * @return [date] shifted by [days] days.
  **/
  advance_by_days(date : Date.date, days : int) : Date.date =
    Date.advance(date, Duration.days(days))

  /**
   * Shifts a given date (forward or backward) until it represents a date on a given
   * weekday. The time of the date is not changed (see {!Date.round_to_day} to "reset"
   * time of the day).
   *
   * @param date A reference date
   * @param dir Direction of the shift ([{forward}] or [{backward}])
   * @param wday A weekday; [date] will be shifted until reaching that day of the week.
   *
   * @return [date] shifted in the direction [dir] until reaching [wday].
  **/
  move_to_weekday(date: Date.date, dir : {forward} / {backward}, wday: Date.weekday) : Date.date =
    rec shift(date) =
      d = to_human_readable(date)
      if Weekday.equals(d.wday, wday) then
        date
      else
        shift(advance_by_days(date, match dir with {forward} -> 1 | {backward} -> -1))
    shift(date)

  /**
   * As {!Date.advance}.
  **/
  shift_forward = advance

  /**
   * Go back in time by a duration.
  **/
  shift_backward(date : Date.date, duration : Duration.duration) : Date.date =
    date - duration

  /**
   * Advance a time by a calendar duration (i.e. not a fixed time-span) in [human_readable] format.
   *
   * E.g.: February 2 + 1 month = March 2 (in any year)
   *
   * @param date Any reference date.
   * @param shift Any human-readable duration.
   *
   * @return The date obtained by advancing [date] by [shift] (or shifting backwards if [shift.forward] is [false])
   * Any field of [shift] may be negative, and the shifting will be done accordingly.
  **/
  // Note: this function relies on the fact that [of_human_readable] accepts and normalizes incorrect dates in a consistent way.
// TODO: Clean, check & test this function.
  calendar_advance(date : Date.date, shift : Duration.human_readable) : Date.date =
    date = to_human_readable(date)
    {~year ~month ~day ~h ~min ~s ~ms wday=_} = date
    {~forward year=d_year month=d_month day=d_day h=d_h min=d_min s=d_s ms=d_ms} = shift
    `++` = if forward then Int.`+` else Int.`-`
    /* because months have their own type, we need to normalize them into [0..11] range */
    rec normalize(month, dy) =
      if month >= 12 then
        normalize(month - 12, dy + 1)
      else if month < 0 then
        normalize(month + 12, dy - 1)
      else
        (month, dy)
    (month, extra_year) = normalize(Month.to_int(month) ++ d_month, 0)
    date = {date with year=year++d_year++extra_year month=Month.of_int(month) day=day++d_day h=h++d_h min=min++d_min s=s++d_s ms=ms++d_ms}
    of_human_readable(date)

  /**
   * Compute the duration between two dates
   *
   * @param date1 first date
   * @param date2 second date
   *
   * @return The duration between [date1] and [date2]
  **/
  between(date1 : Date.date, date2 : Date.date) : Duration.duration =
    date2 - date1

  /**
   * Returns a date, exactly in between two other dates.
   * Can be useful for binary search with dates; see also {!Date.binary_search).
   *
   * @param date1 a date
   * @param date2 another date ([date1] does not need to come before
   *              [date2]).
   * @return a date exactly in between [date1] and [date2].
  **/
  in_between(date1 : Date.date, date2 : Date.date) : Date.date =
    (date1 + date2) / 2

  /**
   * Performs binary search within a range of dates.
   *
   * Please note that binary search can be used only if the
   * function under consideration is monotonic.
   *
   * @param f a function; [f(date, range)] should check whether
   *        the value for [date] is the value that is being
   *        searched and if so return [{eq}], if it is too
   *        late return [{lt}] and if too early [{gt}].
   *        The additional parameter [range] indicates the
   *        current range being considered in the binary search
   *        and will usually be ignored. (One possible use if
   *        we are looking for some approximation, hence can
   *        never really return [{eq}]; but if we never do
   *        then the function will return [{none}], which is
   *        not very useful. The solution in this case is to
   *        return [{eq}] when the [range] is small enough to
   *        consider our approximation acceptable as a solution)
   * @param range the range in which the binary search should
   *        be conducted.
   * @return The date for which [f] returns [{eq}], or [{none}]
   *         if such value was not found.
  **/
  binary_search(f : Date.date, DateRange.range -> Order.ordering,
                range : DateRange.range) : option(Date.date) =
    (low, high) = range
    if high < low then
      none
    else
      mid = Date.in_between(low, high)
      match f(mid, range) with
      | {eq} -> some(mid)
      | {lt} -> binary_search(f, (low, mid-1))
      | {gt} -> binary_search(f, (mid+1, high))

  /**
   * {1 Date comparison & ordering}
  **/

  /**
   * Date comparison.
  **/
  compare(date1 : Date.date, date2 : Date.date) : Order.comparison =
    compare_int(date1, date2)

  /**
   * Date ordering.
  **/
  ordering(date1 : Date.date, date2 : Date.date) : Order.ordering =
    Int.ordering(date1, date2)

  order : order(Date.date, Date.order) =
    Order.make(ordering)

  /**
   * {1 Date accessors}
  **/

  /**
   * Returns the number of milliseconds (within a second) represented by this date
   * interpreted in the local time zone.
   * If you need to completely decompose a date consider using {!Date.to_human_readable}.
   *
   * @param date A date
   * @return The number of milliseconds (within a second) represented by [date].
   *         Returned value is in the range [0..999].
  **/
  get_msec : Date.date -> int = Date_private.time_local_msec

  /**
   * Returns the number of seconds (within a minute) represented by this date
   * interpreted in the local time zone.
   * If you need to completely decompose a date consider using {!Date.to_human_readable}.
   *
   * @param date A date
   * @return The number of seconds (within a minute) represented by [date].
   *         Returned value is in the range [0..59].
  **/
  get_sec : Date.date -> int = Date_private.time_local_sec

  /**
   * Returns the number of minutes (within an hour) represented by this date
   * interpreted in the local time zone.
   * If you need to completely decompose a date consider using {!Date.to_human_readable}.
   *
   * @param date A date
   * @return The number of minutes (within an hour) represented by [date].
   *         Returned value is in the range [0..59].
  **/
  get_min : Date.date -> int = Date_private.time_local_min

  /**
   * Returns the number of hours (within a day) represented by this date
   * interpreted in the local time zone.
   * If you need to completely decompose a date consider using {!Date.to_human_readable}.
   *
   * @param date A date
   * @return The number of hours (within a day) represented by [date].
   *         Returned value is in the range [0..23].
  **/
  get_hour : Date.date -> int = Date_private.time_local_hour

  /**
   * Returns the weekday represented by this date interpreted in the local
   * time zone.
   * If you need to completely decompose a date consider using {!Date.to_human_readable}.
   *
   * @param date A date
   * @return The weekday represented by [date].
  **/
  get_weekday(date : Date.date) : Date.weekday =
    Weekday.of_int(Date_private.time_local_wday(date))

  /**
   * Returns the day number (within a month) represented by this date
   * interpreted in the local time zone.
   * If you need to completely decompose a date consider using {!Date.to_human_readable}.
   *
   * @param date A date
   * @return The day number (within a month) represented by [date].
   *         Returned value is in the range [1..31].
  **/
  get_day : Date.date -> Date.day = Date_private.time_local_mday

  /**
   * Returns the month represented by this date interpreted in the local time zone.
   * If you need to completely decompose a date consider using {!Date.to_human_readable}.
   *
   * @param date A date
   * @return The month represented by [date].
  **/
  get_month(date : Date.date) : Date.month =
    Month.of_int(Date_private.time_local_mon(date))

  /**
   * Returns the year represented by this date interpreted in the local time zone.
   * If you need to completely decompose a date consider using {!Date.to_human_readable}.
   *
   * @param date A date
   * @return The year represented by [date].
  **/
  get_year : Date.date -> Date.year = Date_private.time_local_year

  /**
   * Returns the week number corresponding to the given date.
   *
   * This routine follows ISO 8601 and hence assumes that "the first week of a year
   * is the week that contains the first Thursday of the year" and that "weeks start
   * with Monday". See {{:http://en.wikipedia.org/wiki/ISO_week_date} for more details}.
   *
   * @param date A date
   * @return The week number of the [date].
  **/
  get_week_number(d : Date.date) : int =
    dhr = to_human_readable(d)
    first_week =
      build({year=dhr.year month={january} day=1}) |>
      move_to_weekday(_, {forward}, {thursday}) |>
      move_to_weekday(_, {backward}, {monday})
    Duration.between(first_week, d) |>
      Duration.in_days(_) |>
      _ / 7. |>
      Float.floor |>
      Float.to_int |>
      _ + 1

  /**
   * {1 Date conversions}
  **/

  /**
   * Convert a date to a human-understandable format.
   *
   * Behavior is unspecified when the date is infinite.
  **/
  to_human_readable(date : Date.date) : Date.human_readable =
   // FIXME many BSL calls, causing many date conversions. Should be done with one call/conversion
    { ms = get_msec(date)
      s = get_sec(date)
      min = get_min(date)
      h = get_hour(date)
      wday = get_weekday(date)
      day = get_day(date)
      month = get_month(date)
      year = get_year(date)
    }

  /**
   * Convert a human-understandable format to date.
  **/
  of_human_readable({~year ~month ~day ~h ~min ~s ~ms wday=_} : Date.human_readable) : Date.date =
    mktime = %%BslTime.mktime%% : int, int, int, int, int, int, int -> time_t
    Date_private.ll_import(mktime(year, Month.to_int(month), day, h, min, s, ms))

  /**
   * Converts a date to a duration in reference to 1st of January 1970
   * (see {!Date.epoch}). So for instance {!Date.to_duration} applied on 2nd of
   * January 1970 will return a positive duration of 1 day.
   * See also {!Date.of_duration}.
  **/
  to_duration(date : Date.date) : Duration.duration =
    between(epoch, date)

  /**
   * Converts a duration to a date in reference to 1st of January 1970
   * (see {!Date.epoch}). So for instance {!Date.of_duration} applied on positive
   * duration of 1 day will return 2nd of January 1970.
   * See also {!Date.to_duration}.
  **/
  of_duration(duration : Duration.duration) : Date.date =
    advance(epoch, duration)

  /**
   * @deprecated Use {!Date.of_human_readable} instead
  **/
  @deprecated({use="of_human_readable"})
  human_readable_to = of_human_readable

  /**
   * @deprecated Use {!Date.to_human_readable} instead
  **/
  @deprecated({use="to_human_readable"})
  human_readable_of = to_human_readable

  /**
   * Returns the number of milliseconds since 1st January 1970 at 0000 till [date].
   *
   * This is a perfect hash function for dates. It can also be useful to convert
   * dates to "identifiers".
   *
   * But in general use with caution. Converting to milliseconds, doing integer
   * arithmetic and then converting back to a date is *strongly* discouraged.
   * You have been warned.
  **/
  in_milliseconds(date : Date.date) : int =
    date

  /**
   * Converts a number of milliseconds since 1st January 1970 at 0000 into a date.
   *
   * Use with care. Check the comment of {!Date.in_milliseconds}.
  **/
  milliseconds(m : int) : Date.date =
    m

  /**
   * Converts date into its low-level representation.
   * Not for casual users.
  **/
  ll_export : Date.date -> time_t = Date_private.ll_export

  /**
   * A module for dealing with hours.
  **/
  Hour = {{

    /**
     * Converts a 24h-based value to a 12h-based one.
     *
     * @param hour an integer in the range [0..23]
     * @return [hour] in 12h format
    **/
    convert_24h_to_12h(hour : int) : int =
      if hour == 0 then
        12
      else if hour <= 12 then
        hour
      else if hour <= 23 then
        hour - 12
      else
        error("Date.Hour.convert_24h_to_12h({Int.to_string(hour)})")

    /**
     * Is the given hour AM; as opposed to PM? (ante meridiem = before mid day)
     *
     * @param hour an integer in the range [0..23]
    **/
    is_am(hour : int) : bool = hour < 12

  }}

  /**
   * Handling of weekdays.
  **/
  Weekday = {{

    equals(a:Date.weekday, b:Date.weekday): bool =
       to_int(a) == to_int(b)

    /**
     * Converts an integer to a week-day (0 = sunday, 1 = monday, ... 6 = saturday)
     *
     * @param i an integer in the range [0..6]
    **/
    of_int : int -> Date.weekday =
      | 0 -> {sunday}
      | 1 -> {monday}
      | 2 -> {tuesday}
      | 3 -> {wednesday}
      | 4 -> {thursday}
      | 5 -> {friday}
      | 6 -> {saturday}
      | _ -> error("Date.Weekday.of_int")
      : Date.weekday

    /**
     * Converts a week-day to an integer (sunday = 0, monday = 1, ... saturday = 6)
     *
     * @param i an integer in the range [0..6]
     */
    to_int : Date.weekday -> int =
      | {monday} -> 1
      | {tuesday} -> 2
      | {wednesday} -> 3
      | {thursday} -> 4
      | {friday} -> 5
      | {saturday} -> 6
      | {sunday} -> 0

    /**
     * Converts a week-day to a string representation (in English)
     *
     * @param wday weekday to convert
     */
    to_string : Date.weekday -> string =
      | {monday} -> "Monday"
      | {tuesday} -> "Tuesday"
      | {wednesday} -> "Wednesday"
      | {thursday} -> "Thursday"
      | {friday} -> "Friday"
      | {saturday} -> "Saturday"
      | {sunday} -> "Sunday"

    /**
     * Converts a week-day to an abbreviated, 3-letter string representation (in English, i.e. Mon, Tue, ...)
     *
     * @param wday weekday to convert
     */
    to_short_string : Date.weekday -> string =
      | {monday} -> "Mon"
      | {tuesday} -> "Tue"
      | {wednesday} -> "Wed"
      | {thursday} -> "Thu"
      | {friday} -> "Fri"
      | {saturday} -> "Sat"
      | {sunday} -> "Sun"

  }}

  /**
   * Handling of months
  **/
  Month =
  {{


    equals(a:Date.month, b:Date.month): bool =
       to_int(a) == to_int(b)

    /**
     * Converts an integer to a month (0 = january, ... 11 = december)
     *
     * Results in a run-time error when the integer is not in the range [0..11].
     *
     * @param i an integer in the range [0..11]
     * @return month corresponding to [i]
    **/
    of_int : int -> Date.month=
      | 0 -> {january}
      | 1 -> {february}
      | 2 -> {march}
      | 3 -> {april}
      | 4 -> {may}
      | 5 -> {june}
      | 6 -> {july}
      | 7 -> {august}
      | 8 -> {september}
      | 9 -> {october}
      | 10 -> {november}
      | 11 -> {december}
      | _ -> error("Date.Month.of_int")

    /**
     * Converts a month to an integer (january = 0, ... december = 11)
     *
     * @param i an integer in the range [0..11]
    **/
    to_int : Date.month -> int =
      | {january} -> 0
      | {february} -> 1
      | {march} -> 2
      | {april} -> 3
      | {may} -> 4
      | {june} -> 5
      | {july} -> 6
      | {august} -> 7
      | {september} -> 8
      | {october} -> 9
      | {november} -> 10
      | {december} -> 11

    /**
     * Converts a month to a string representation (in English)
     *
     * @param month a month to convert
    **/
    to_string : Date.month -> string =
      | {january} -> "January"
      | {february} -> "February"
      | {march} -> "March"
      | {april} -> "April"
      | {may} -> "May"
      | {june} -> "June"
      | {july} -> "July"
      | {august} -> "August"
      | {september} -> "September"
      | {october} -> "October"
      | {november} -> "November"
      | {december} -> "December"

    /**
     * Converts a month to an abbreviated, 3 letter string representation (in English, i.e. Jan, Feb, ...)
     *
     * @param month a month to convert
     */
    to_short_string : Date.month -> string =
      | {january} -> "Jan"
      | {february} -> "Feb"
      | {march} -> "Mar"
      | {april} -> "Apr"
      | {may} -> "May"
      | {june} -> "Jun"
      | {july} -> "Jul"
      | {august} -> "Aug"
      | {september} -> "Sep"
      | {october} -> "Oct"
      | {november} -> "Nov"
      | {december} -> "Dec"

    /**
     * Gives the succeeding month.
     *
     * @param d a month
     * @return [some(nd)], where [nd] is the month succeeding [d], or [none] if [d] is December.
     */
    next(d : Date.month) : option(Date.month) =
      di = to_int(d)
      if di < 11 then
        some(of_int(di + 1))
      else
        none

    /**
     * Gives the preceding month.
     *
     * @param d a month
     * @return [some(nd)], where [nd] is the month preceding [d], or [none] if [d] is January.
     */
    prev(d : Date.month) : option(Date.month) =
      di = to_int(d)
      if di > 0 then
        some(of_int(di - 1))
      else
        none
  }}

  /**
   * {1 Date printing & parsing}
  **/

  /**
   * This function builds a date printer, that can than be used with
   * {!Date.to_formatted_string} function.
   *
   * It takes a format for printing dates as the only argument. The
   * format is largely based on the format accepted by the Unix date
   * command and the following documentation largely borrows from
   * date's man page (author: David MacKenzie).
   *
   * The interpreted sequences in the string format are the following:
   *
   * - [%%] a literal [%],
   * - [%a] abbreviated weekday name (ex. Mon, Tue, Wed, ...)
   * - [%A] full weekday name (ex. Monday, Tuesday, Wednesday, ...)
   * - [%b] abbreviated month name (ex. Jan, Feb, Mar, ...)
   * - [%B] full month name (ex. January, February, ...)
   * - [%c] date and time; same as [%a %b %_d %Y %T] (ex, Thu Mar  3 2005 23:05:25)
   * - [%C] century, like [%Y] but with last two digits omitted (ex. 20)
   * - [%d] day of month (01-31)
   * - [%e] day of month, space padded; same as [%_d] (ex.  3; with space in front of 3)
   * - [%E] day of month with suffixes 'st', 'nd' or 'th'
   * - [%D] date; same as [%m/%d/%y] (ex. 03/07/10)
   * - [%F] full date; same as [%Y-%m-%d] (ex. 2010-03-07)
   * - [%h] same as [%b] (ex. Jan, Feb, Mar, ...)
   * - [%H] hour (00..23)
   * - [%I] hour (01..12)
   * - [%k] hour ( 0..23)
   * - [%l] hour ( 1..12)
   * - [%m] month (01..12)
   * - [%M] minute (00..59)
   * - [%n] a newline
   * - [%p] 12h period name (AM/PM)
   * - [%P] like [%p], but lower case (am/pm)
   * - [%R] 24-hour hour and minute; same as [%H:%M] (ex. 17:35)
   * - [%S] second (00..59)
   * - [%x] millisecond (000..999)
   * - [%t] a tab
   * - [%T] time; same as [%H:%M:%S] (ex. 17:35:12)
   * - [%u] day of week (1..7); 1 is Monday
   * - [%w] day of week (0..6); 0 is Sunday
   * - [%y] last two digits of year (00..99)
   * - [%Y] year (ex. 2010)
   *
   * By default, numeric fields are padded with zeroes. The following optional flags
   * may follow `%':
   * - [-] (hyphen) do not pad the field
   * - [_] (underscore) pad with spaces
   * - [0] (zero) pad with zeros
   *
   * For now the following sequences used in Unix's date are unsupported:
   * - [%g] the  last  two  digits  of the year corresponding to the %V week number
   * - [%G] the year corresponding to the %V week number
   * - [%j] day of year (001..366)
   * - [%N] nanoseconds (000000000..999999999)
   * - [%s] seconds since 1970-01-01 00:00:00 UTC
   * - [%U] week number of year with Sunday as first day of week (00..53)
   * - [%V] week number of year with Monday as first day of week (01..53)
   * - [%W] week number of year with Monday as first day of week (00..53)
   * - [%z] +hhmm numeric timezone (e.g., -0400)
   * - [%:z] +hh:mm numeric timezone (e.g., -04:00)
   *
   * Also the padding directives are not supported:
   * - [%^] use upper case if possible
   * - [%#] use opposite case if possible
   *
   * Moreover, the following locale-sensitive sequences for now work with the fixed
   * English locale:
   *  [%a], [%A], [%b], [%B], [%c], [%p], [%P], [%r], [%x], [%X]
   *
   * @param format a format for printing dates (as described above)
   * @return either [success(printer)] where [printer] is a printer for dates
   *         using [format], or [failure(error)] where [error] is a string
   *         description of the problem with [format].
  **/
  try_generate_printer(format : string) : outcome(Date.printer, string) =
    Date_private.ToString.generate_printer(format)

  /**
   * Generates a date printer for a given format. This function behaves almost
   * as {!Date.try_generate_printer} but it assumes that the format is correct
   * and returns a printer (not an optional one). It will result in an runtime error
   * if the [format] is not correct.
  **/
  generate_printer(format : string) : Date.printer =
    match try_generate_printer(format) with
    | ~{success} -> success
    | ~{failure} -> error("Date.generate_printer({format}) -> problem with the format: {failure}")

  /**
   * The default format for printing dates.
  **/
  default_printer_fmt = "%c"

  /**
   * A format for printing time part of a date.
  **/
  time_only_printer_fmt = "%H:%M:%S"

  /**
   * A format for printing date (without the time part).
  **/
  date_only_printer_fmt = "%F"

  /**
   * A format for printing full dates in debug/log friendly format
  **/
  debug_printer_fmt = "%Y-%m-%d | %H:%M:%S.%x"

  /**
   * A default printer for dates.
   * e.x.: "Thu Mar  7 2010 23:05:25"
  **/
  @both_implem default_printer = generate_printer(default_printer_fmt)

  /**
   * A printer for dates, displaying only the time part,
   * e.x.: "23:05:25"
  **/
  @both_implem time_only_printer = generate_printer(time_only_printer_fmt)

  /**
   * A printer for dates, displaying only the date part (without time),
   * ex. "2010-03-07"
  **/
  @both_implem date_only_printer = generate_printer(date_only_printer_fmt)

  /**
   * A printer for full dates in debug/log friendly format.
   * ex. "2010-03-07 | 23:05:25.113"
  **/
  @both_implem debug_printer = generate_printer(debug_printer_fmt)

  /**
   * This function prints a date using a given printer.
   * For ways of generating date printers see {!Date.generate_printer} and
   * {!Date.try_generate_printer}.
   *
   * @param printer a date printer
   * @param date a date to be printed
   * @return string representation of [date] with [printer]
  **/
  to_formatted_string(printer : Date.printer, date : Date.date) : string =
    printer(date)

  /**
   * Converts a given date to a string, ignoring the date and presenting only the
   * time part, in the format "12:34:47".
   *
   * Use {!Date.to_formatted_string} to customize the format e.g. for a specific language.
  **/
  to_string_time_only(date : Date.date) : string =
    to_formatted_string(time_only_printer, date)

  /**
   * Converts a given date to a string, ignoring the time and presenting only the
   * date part, in the format "2010-03-07"
   *
   * Use {!Date.to_formatted_string} to customize the format e.g. for a specific language.
  **/
  to_string_date_only(date : Date.date) : string =
    to_formatted_string(date_only_printer, date)

  /**
   * Converts a given date to a string, in a format useful for logging/debugging,
   * such as: "2010-03-07 | 23:05:25.113". See {!Date.debug_printer} for more details.
  **/
  to_debug_string(date : Date.date) : string =
    to_formatted_string(debug_printer, date)

  /**
   * Prints a duration using default printer (see {!Date.default_printer}).
   * For more customization over how durations are printed see {!Date.to_formatted_string}.
   *
   * @param duration a duration to be printed
   * @return string representation of [duration] using {!Date.default_printer} format.
  **/
  @stringifier(Date.date) to_string(date : Date.date) : string =
    to_formatted_string(default_printer, date)

  /**
   * Generates a scanner (i.e. a parser) for dates in a given format.
   *
   * For the purpose of parsing there are few differences in the interpretations
   * of the format, compared to the format used for printing, see {!Date.try_generate_printer}:
   * - the directives '%C', '%u' and '%w' are not recognized,
   * - a non-empty sequence of spaces in the [format] can be matched by any (non-empty)
   *   sequence of spaces in the input.
   * - the padding flags ('_', '-' and '0') are ignored and all numerical values
   *   accept leading spaces and zeros.
   * - the format should not be redundant, i.e. parsing with format "%H %H" will
   *   effectively ignore the first occurrence of "%H" (as the first hour will be
   *   overwritten by parsing the second one). *Warning:* to parse date in a 12h
   *   format, the period directive ("%p"/"%P") must be placed *after* the hour
   *   directive.
   * - when using only last two digits for the year ("%y"), values xx<70 are assumed
   *   to mean "20xx" and for xx>=70 "19xx".
   *
   * @param format a string format used in the conversion; see {!Date.try_generate_printer}
   * for more information on the format
   * @return an optional scanner for dates written in [format], or an indication why
   * the [format] is incorrect.
  **/
  try_generate_scanner(format : string) : outcome(Date.scanner, string) =
    Date_private.OfString.generate_scanner(format)

  /**
   * Generates a date scanner for a given format. This function behaves almost
   * as {!Date.try_generate_scanner} but it assumes that the format is correct
   * and returns a scanner (not an optional one). It will result in an runtime error
   * if the [format] is not correct.
  **/
  generate_scanner(format : string) : Date.scanner =
    match try_generate_scanner(format) with
    | ~{success} -> success
    | ~{failure} -> error("Date.generate_scanner({format}) -> problem with the format: {failure}")

  /**
   * Converts a string in a given format to a date.
   *
   * @param scanner a scanner for dates. See {!Date.try_generate_scanner} for ways of
   * generating one.
   * @param init_date initial date, to be updated with date information from
   * the string. If the format does not cover all fields of a date, then the
   * remaining ones will remain as in [init_date].
   * @return optional date representing [init_date] updated with information
   * from [date_string].
  **/
  of_formatted_string_aux(scanner : Date.scanner, init_date : Date.date, date_string : string) : option(Date.date) =
    Date_private.OfString.of_string(scanner, date_string, init_date)

  /**
   * Converts a string in a given format to a date.
   *
   * As {!Date.of_formatted_string_aux} but the [init_date] is taken to be empty, so
   * either the [scanner] used should include all the components of the date type
   * or some the fields of the returned date will remain uninitialized (or
   * more precisely, as in the {!Date.epoch}).
  **/
  of_formatted_string(scanner : Date.scanner, date_string : string) : option(Date.date) =
    of_formatted_string_aux(scanner, epoch, date_string)

  /**
   * {1 Data structures based on dates}
  **/

  /**
   * A [Map] on dates, using chronological comparison.
   */
  Map = Map_make(order) : Map(Date.date, Date.order)

  /**
   * A [Set] of dates, using chronological comparison.
   */
  Set = Set_make(order) : Set(Date.date, Date.order)

}}
