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
 * This module contains functions to manipulate durations, i.e. intervals
 * of time. Duration can be either positive (duration "in the future") or
 * negative (duration "in the past").
 * You may also be interested in the module {!Date} which contains functions
 * to manipulate dates & times.
 *
 *
 * {1 When to use this module}
 *
 * This module will let you manipulate durations. You will probably need
 * it when you need to compute date intervals (i.e. the time span between
 * two dates). It will also allow you to print them in a format of your
 * choice (such as: "yesterday", "tomorrow", "in 2 days", "3 hours ago"
 * etc.)
**/

/**
 * A duration, i.e. the time difference between two dates.
 *
 * Durations may be positive, negative or infinite.
 *
 * This type is useful for duration manipulation. For presentation purposes see {!Duration.human_readable}
**/
@abstract type Duration.duration = int

/**
 * A duration, decompressed in a more understandable format.
**/
type Duration.human_readable = {
  /** [{true}] if the duration represents a forward (or null) shift in time, [{false}] otherwise. **/
  forward : bool

  /** Number of years. Here, a year is assumed to consist of 365 days. **/
  year : int

  /** Number of months, 0 to 11. Here, a month is assumed to consist of 31 days. **/
  month : int

  /** Number of days, 0 to 30. Here, a day is assumed to consist of 24 hours **/
  day : int

  /** Number of hours, 0 to 23. **/
  h : int

  /** Number of minutes, 0 to 59. **/
  min : int

  /** Number of seconds, 0 to 59. **/
  s : int

  /** Number of milliseconds, 0 to 999. **/
  ms : int
}

/**
 * A type of duration printers, i.e. objects capable of rendering durations as strings
 * (according to some format). See {!Duration.try_generate_printer}.
**/
@abstract type Duration.printer = (Duration.duration, int -> string)

/**
 * The ordering of durations
**/
type Duration.order = Order.default

/**
 * A map from duration to values, using the default ordering on durations.
**/
type Duration.map('a) = ordered_map(Duration.duration, 'a, Duration.order)

/**
 * A set of durations.
**/
type Duration.set = ordered_set(Duration.duration, Duration.order)

Duration = {{

  /**
   * {1 Constants}
  **/

  /**
   * Some time related constants
  **/
  ms_in_s = 1000

  s_in_min = 60
  ms_in_min = s_in_min * ms_in_s

  min_in_h = 60
  ms_in_h = min_in_h * ms_in_min

  h_in_day = 24
  ms_in_day = h_in_day * ms_in_h

  days_in_week = 7
  ms_in_week = days_in_week * ms_in_day

  days_in_month = Float.of_int(days_in_year) / Float.of_int(months_in_year)
  ms_in_month = Float.to_int(days_in_month * Float.of_int(ms_in_day))

  days_in_year = 365
  ms_in_year = days_in_year * ms_in_day
  months_in_year = 12

  /**
   * {1 Constructors}
  **/

  /**
   * The instantaneous duration.
  **/
  empty : Duration.duration =
    0

  /**
   * The instantaneous duration (in human-readable format).
  **/
  zero : Duration.human_readable =
    { forward=true year=0 month=0 day=0 h=0 min=0 s=0 ms=0 }

  /**
   * [between d1 d2] returns a duration between dates [d1] and [d2].
  **/
  between(d1 : Date.date, d2 : Date.date) : Duration.duration =
    Date.between(d1, d2)

  /**
   * [seconds_between d1 d2] returns a number of seconds between dates [d1] and [d2].
  **/
  seconds_between(d1 : Date.date, d2 : Date.date) : float =
    in_seconds(between(d1, d2))

  /**
   * Return a number of milliseconds as a duration.
  **/
  ms(number : int) : Duration.duration =
    number

  @private
  ms_f(number : float) : Duration.duration =
    Float.to_int(number)

  /**
   * Return a number of seconds as a duration.
  **/
  s(number : int) : Duration.duration =
    ms(number * ms_in_s)

  @private
  s_f(number : float) : Duration.duration =
    ms_f(number * Float.of_int(ms_in_s))

  /**
   * Return a number of minutes as a duration.
  **/
  min(number : int) : Duration.duration =
    s(number * s_in_min)

  @private
  min_f(number : float) : Duration.duration =
    s_f(number * Float.of_int(s_in_min))

  /**
   * Return a number of hours as a duration.
  **/
  h(number : int) : Duration.duration =
    min(number * min_in_h)

  @private
  h_f(number : float) : Duration.duration =
    min_f(number * Float.of_int(min_in_h))

  /**
   * Return a number of days as a duration.
   *
   * Note: This function assumes that a day lasts exactly 24h, which is legally true but scientifically
   * not completely accurate.
  **/
  days(number : int) : Duration.duration =
    h(number * h_in_day)

  @private
  days_f(number : float) : Duration.duration =
    h_f(number * Float.of_int(h_in_day))

  /**
   * Return a number of days as a duration.
   *
   * Note: This function assumes that a day lasts exactly 24h, which is legally true but scientifically
   * not completely accurate.
  **/
  weeks(number : int) : Duration.duration =
    days(number * days_in_week)

  /**
   * Return a number of days as a duration.
   *
   * Note: This function assumes that a day lasts exactly 24h, which is legally true but scientifically
   * not completely accurate.
   * Note: Not all months have the same duration and this function assumes that a month lasts 365/12 days.
  **/
  months(number : int) : Duration.duration =
    days_f(Float.of_int(number) * days_in_month)

  /**
   * Return a number of years as a duration.
   *
   * Note: This function assumes that a year lasts exactly 365 days of 24h, which is an approximation.
  **/
  years(number : int) : Duration.duration =
    days(number * days_in_year)

  /**
   * [execution_time f] returns the result of executing the function [f] along with
   * the time duration that it took to execute this function.
   *
   * @param f a function
   * @return a pair [(d,r)] where [d] is a duration of time that execution of [f]
   *         took and [r] is the result of evaluating [f].
   */
  execution_time(f : -> 'a) : (Duration.duration, 'a) =
    t_beg = Date.now()
    res = f()
    t_end = Date.now()
    (Duration.between(t_beg, t_end), res)

  /**
   * Conversion from low-level representation.
   * Not for casual user.
  **/
  ll_import(t : time_t) : Duration.duration =
    import_t = %%BslTime.import_t%% : time_t -> int
    import_t(t)

  /**
   * {1 Accessors, querying}
  **/

  @private convert_duration(d : Duration.duration, factor : int) : float =
    Float.of_int(d) / Float.of_int(factor)

  /**
   * @param d A duration
   * @return Returns a number of milliseconds in duration [d]
   */
  in_milliseconds(d : Duration.duration) : int =
    d

  /**
   * @param d A duration
   * @return Returns a number of seconds in duration [d]
   */
  in_seconds(d : Duration.duration) : float =
    convert_duration(d, ms_in_s)

  /**
   * @param d A duration
   * @return Returns a number of minutes in duration [d]
   */
  in_minutes(d : Duration.duration) : float =
    convert_duration(d, ms_in_min)

  /**
   * @param d A duration
   * @return Returns a number of hours in duration [d]
   */
  in_hours(d : Duration.duration) : float =
    convert_duration(d, ms_in_h)

  /**
   * @param d A duration
   * @return Returns a number of days in duration [d] (assuming a day is exactly 24h)
   */
  in_days(d : Duration.duration) : float =
    convert_duration(d, ms_in_day)

  /**
   * @param d A duration
   * @return Returns a number of days in duration [d] (assuming a day is exactly 24h),
   *         truncated down to the nearest integer value.
   */
  in_full_days(d : Duration.duration) : int =
    in_days(d) |> Float.to_int(_)

  /**
   * @param d A duration
   * @return Returns a number of months in duration [d] (assuming a month has (365/12), 24h long days)
   */
  in_months(d : Duration.duration) : float =
    convert_duration(d, ms_in_month)

  /**
   * @param d A duration
   * @return Returns a number of years in duration [d] (assuming a year has 365, 24h long days)
   */
  in_years(d : Duration.duration) : float =
    convert_duration(d, ms_in_year)

  /**
   * {1 Operations on durations}
  **/

  /**
   * Checks whether a given duration is positive (in the future)
   *
   * @param d a duration
   * @param true iff duration [d] is positive (in the future)
  **/
  is_positive(d : Duration.duration) : bool =
    d > 0

  /**
   * Checks whether a given duration is negative (in the past)
   *
   * @param d a duration
   * @param true iff duration [d] is negative (in the past)
  **/
  is_negative(d : Duration.duration) : bool =
    d < 0

  /**
   * Checks whether a given duration is instantenous (empty duration)
   *
   * @param d a duration
   * @param true iff duration [d] is instantenous
  **/
  is_instantenous(d : Duration.duration) : bool =
    d == 0

  /**
   * {1 Durations manipulation}
  **/

  /**
   * A sum of two durations
  **/
  add(duration1 : Duration.duration, duration2 : Duration.duration) : Duration.duration =
    duration1 + duration2

  /**
   * A diference between two durations
  **/
  subtract(duration1 : Duration.duration, duration2 : Duration.duration) : Duration.duration =
    duration1 - duration2

  /**
   * Returns a sum of a list of durations.
   *
   * @param durations A list of durations. If empty, the total is the empty duration.
  **/
  sum(durations : list(Duration.duration)) : Duration.duration =
    List.fold((x, acc -> x + acc), durations, 0)

  /**
   * Compare two durations
  **/
  compare(duration1 : Duration.duration, duration2 : Duration.duration) =
    Int.compare(duration1, duration2)

  /**
   * {1 Order}
  **/

  /**
   * Ordering two durations
  **/
  ordering(duration1 : Duration.duration, duration2 : Duration.duration) =
    Int.ordering(duration1, duration2)

  order : order(Duration.duration, Duration.order) =
    Order.make(ordering)

  /**
   * {1 Conversions}
  **/

  /**
   * Convert a duration to a human-understandable format.
   *
   * Behavior is unspecified when the duration is infinite.
  **/
  to_human_readable(duration: Duration.duration) : Duration.human_readable =
    fl = Float.of_int
    conv = [ (fl(1),              (d, v -> {d with ms = v} : Duration.human_readable))
           , (fl(ms_in_s),        (d, v -> {d with s = v} : Duration.human_readable))
           , (fl(s_in_min),       (d, v -> {d with min = v} : Duration.human_readable))
           , (fl(min_in_h),       (d, v -> {d with h = v} : Duration.human_readable))
           , (fl(h_in_day),       (d, v -> {d with day = v} : Duration.human_readable))
           , (days_in_month,      (d, v -> {d with month = v} : Duration.human_readable))
           , (fl(months_in_year), (d, v -> {d with year = v} : Duration.human_readable))
           ]
    conv_aggregate((mul:float, f), (acc, factor:float)) =
      fac = mul * factor
      ((fac, f) +> acc, fac)
    (conv, _) = List.fold(conv_aggregate, conv, ([], 1.))
    compute((fac:float, f), (duration, acc)) =
      units = Float.to_int(fl(duration) / fac)
      (duration - Float.to_int(fl(units) * fac), f(acc, units))
    d_init = Int.abs(duration)
    dr_init = {zero with forward = duration >= 0}
    (_, res) = List.fold(compute, conv, (d_init, dr_init))
    res

  /**
   * Convert a duration in human-understandable format back to a closed
   * format.
  **/
  of_human_readable(d : Duration.human_readable) : Duration.duration =
    conv(f, v) = f(v)
    res = sum([ conv(ms, d.ms), conv(s, d.s), conv(min, d.min), conv(h, d.h), conv(days, d.day), conv(months, d.month), conv(years, d.year)])
    if d.forward then
      res
    else // if non-forward duration switch sign
      v = res
      (-v)

  /**
   * Converts a date to a duration, see {!Date.to_duration}.
  **/
  of_date : Date.date -> Duration.duration = Date.to_duration

  /**
   * Converts a duration to a date, see {!Date.of_duration}.
  **/
  to_date : Duration.duration -> Date.date = Date.of_duration

  /**
   * Conversion to low-level representation.
   * Not for casual user.
  **/
  ll_export(d : Duration.duration) : time_t =
    export_t = %%BslTime.export_t%% : int -> time_t
    export_t(d)

  /**
   * {1 Printing}
  **/

  /**
   *   This function takes a format for printing durations and generates a
   * "duration printer", which can later be used with the {!print}
   * function. Below we describe how the format string should look like.
   *
   *   The format describes how a time duration should be* converted to a
   * string representation. Before the conversion a duration is rounded to
   * the nearest second (i.e. milliseconds are ignored).
   *
   *   Conversion takes place in the context of two variables:
   * - "VAL" (represented by #) and
   * - "DUR"
   *   At the beginning of the conversion "VAL" is undefined and "DUR" corresponds
   * to the duration being converted. Those values will be alerted by the directives
   * contained in the format string described hereafter.
   *
   *   The following directives operate on the "DUR" variable and convert it to
   * the numerical value corresponding to the maximal number of given units contained
   * in the duration. Read the following entries as: "number of complete ... in the
   * duration".
   * - [%x] milliseconds
   * - [%s] seconds (1 second = 1000 milliseconds)
   * - [%m] minutes (1 minute = 60 seconds)
   * - [%h] hours (1 hour = 60 minutes)
   * - [%D] days (1 day = 24 hours)
   * - [%W] weeks (1 week = 7 days)
   * - [%M] months (1 month = 365/12 days)
   * - [%Y] years (1 year = 365 days)
   *
   *   Besides the following modes are available:
   * - "%X" just prints the numerical value corresponding to the duration, as
   *     described above.
   * - "#" prints the value "VAL"
   * - "[%X:...]" prints the nested format "..." with the "VAL" variable
   *     set to the maximal number of "X" units in the actual duration "DUR"
   *     and the "DUR" duration decreased by that amount.
   *     Let's for instance consider the duration of 1000 days and the format:
   *       "[%y:# years and %d days]"
   *     The printer will compute the number of complete years, 2 in this case,
   *     and will evaluate the nested format with duration decreased by 2 full
   *     years, i.e. with 1000 - 2*365 = 270 days, so the resulting conversion
   *     will be:
   *      "2 years and 270 days"
   *     This mode can be nested.
   * - "[#YN:...]" where:
   *      Y = '>', '=' or '<' and
   *      N is a natural number
   *    This mode prints the nested format "..." only if the current value "VAL" is:
   *    greater than, equal or less than (resp. for Y='>', Y='=' and Y='<') the
   *    number 'N'.
   *    Typical use of this mode will be with N=0 or N=1 to check, respectively,
   *    whether given units should be printed / units should be printed in singular
   *    or plural. But higher values can also be useful, for instance to suppress
   *    printing the concrete value and replace it with 'many' or similar.
   * - "[#YN:...:...]"
   *    A variant of the above mode where the format after the first colon if
   *    printer if the condition is true and the format after the second if it
   *    is not.
   * - "[%Y:...] where:
   *      Y = '>', '0' or '<' and
   *    This mode prints the nested format "..." only if the current duration "DUR"
   *    is: positive, instantenous, negative (resp. for Y='>', Y='0' and Y='<').
   *    Typical use of this mode will be to print some prefix/suffix (such as:
   *    ('*in* ... days' or '... days *ago*'
   *
   *   Besides the above sequences and modes, the rest of the format is interpreted
   * verbatim as a string. Special characters: [%], [#], [[] and []] need escaping
   * with "\".
   *
   * Example 1)
   * ==========
   *   Let's say we want to print a duration in the format:
   * "[in] X  years Y months Z days [ago]"
   *   but:
   * - ommiting given entry if the corresponding value is zero,
   * - printing the prefix "in"/suffix "ago" depending on whether the duration is
   *   positive/negative.
   * - replacing those 3 fields with "less than one day" if absolute value duration
   *   of the duration is less than a day.
   *
   *   For instance for a positive duration of 1000 days we want to get:
   *   "in 2 years 8 months 22 days"
   *   and for the negative duration of 12 hours we want to get:
   *   "less than a day ago"
   *
   *   The format that would accomplish that is:
   *   "[%>:in ][%D:[#0:less than a day ]][%Y:[#>:# years ][%M:[#>:# months ][%D:[#>:# days ]]]][%<:ago]"
   *
   * @param format a format for printing durations.
   * @param an optional duration printer, or else an indication of an error in the
   *        duration format.
  **/
  try_generate_printer(format : string) : outcome(Duration.printer, string) =
    Duration_private.ToString.generate_printer(format)

  /**
   * Generates a duration printer for a given format. This function behaves almost
   * as {!try_generate_printer} but it assumes that the format is correct
   * and returns a printer (not an optional one). It will result in an runtime error
   * if the [format] is not correct.
  **/
  generate_printer(format : string) : Duration.printer =
    match try_generate_printer(format) with
    | ~{failure} -> error("Could not generate duration printer from format: '{format}': {failure}")
    | ~{success} -> success

  /**
   * The format for printing durations used by {!default_printer}
  **/
  default_printer_fmt =
    "[%>:[%D:[#=1:tomorrow :in ]]]" ^
    "[%Y:[#>0:# year[#>1:s] ][#=0:" ^
    "[%M:[#>0:# month[#>1:s] ][#=0:" ^
    "[%D:[#>1:# day[#>1:s] ][#=0:" ^ // we don't print days for #=1, because that was taken care with tomorrow/yesterday
    "[%h:[#>0:# hour[#>1:s] ][#=0:" ^
    "[%m:[#>0:# minute[#>1:s] ][#=0:" ^
    "[%s:[#>0:# second[#>1:s] :now ]" ^
    "]]]]]]]]]]]" ^
    "[%<:[%D:[#=1:yesterday :ago ]]]"

  /**
   * The format for printing durations used by {!Durantion.two_components_printer}
  **/
  two_components_printer_fmt =
    "[%>:in ]" ^
    "[%Y:[#>0:#Y [%M:[#>0:#M ]]][#=0:" ^
    "[%M:[#>0:#M [%D:[#>0:#D ]]][#=0:" ^
    "[%D:[#>0:#D [%h:[#>0:#h ]]][#=0:" ^
    "[%h:[#>0:#h [%m:[#>0:#m ]]][#=0:" ^
    "[%m:[#>0:#m [%s:[#>0:#s ]]][#=0:" ^
    "[%s:#s ]]]]]]]]]]]" ^
    "[%<:ago ]"

  /**
   * The format for printing durations used by {!long_time_with_ms_printer}.
  **/
  time_duration_printer_h_mm_ss_ms_fmt =
    "[%h:#\\:[%m:##\\:[%s:##.[%x:##]]]]"

  /**
   * The format for printing durations used by {!long_time_printer}.
  **/
  time_duration_printer_h_mm_ss_fmt =
    "[%h:#\\:[%m:##\\:[%s:##]]]"

  /**
   * The format for printing durations used by {!short_time_printer}.
  **/
  time_duration_printer_h_mm_fmt =
    "[%h:#\\:[%m:##]]"

  /**
   * A default printer for durations.
   * For past durations it will print:
   *   "... ago"
   * and for future ones:
   *   "in ..."
   * where "..." will be depending on the largest time unit the duration represents:
   *   "X years"
   *   "X months"
   *   "X days"
   *   "X hours"
   *   "X minutes"
   *   "X seconds"
   * and additionaly the last letter "s" will not be printed for [X=1]
  **/
  @both_implem default_printer = generate_printer(default_printer_fmt)

  /**
   * A format for printing durations.
   * For past durations it will print:
   *   "... ago"
   * and for future ones:
   *   "in ..."
   * where "..." includes two largest units of time with short suffixes, i.e.:
   *   "#Y #M"   for # years and # months
   *   "#M #D"   for # months and # days
   *   "#D #h"   for # days and # hours
   *   "#h #m"   for # hours and # minutes
   *   "#m #s"   for # minutes and # seconds
   *   "#s"      for # seconds
   * Additionally the second component is not printed if the associated
   * value is zero (so "#h" instead of "#h 0m").
  **/
  @both_implem two_components_printer = generate_printer(two_components_printer_fmt)

  /**
   * Printing of time duration as: 3:02:17 (h:mm:ss).
  **/
  @both_implem long_time_printer = generate_printer(time_duration_printer_h_mm_ss_fmt)

  /**
   * Printing of time duration as: 3:02:17.22 (h:mm:ss.ms).
  **/
  @both_implem long_time_with_ms_printer = generate_printer(time_duration_printer_h_mm_ss_ms_fmt)

  /**
   * Printing of time duration as: 3:02 (h:mm).
  **/
  @both_implem short_time_printer = generate_printer(time_duration_printer_h_mm_fmt)


  /**
   * This function prints a duration using a given printer.
   * For ways of generating duration printers see {!generate_printer} and
   * {!try_generate_printer}.
   *
   * @param printer a duration printer
   * @param duration a duration to be printed
   * @return string representation of [duration] with [printer]
  **/
  to_formatted_string(printer : Duration.printer, duration : Duration.duration) : string =
    printer(duration, 0)

  /**
   * Prints a duration using default printer (see {!default_printer}).
   * For more customization over how durations are printed see {!to_formatted_string}.
   *
   * @param duration a duration to be printed
   * @return string representation of [duration] using {!default_printer} format.
  **/
  to_string(duration : Duration.duration) : string =
    to_formatted_string(default_printer, duration)

  /**
   * Prints a duration using two components printer (see {!two_components_printer}).
   * For more customization over how durations are printed see {!to_formatted_string}.
   *
   * @param duration a duration to be printed
   * @return string representation of [duration] using {!two_components_printer} format.
  **/
  to_two_components_string(duration : Duration.duration) : string =
    to_formatted_string(two_components_printer, duration)


  /**
   * Prints a duration, including milliseconds using: {!long_time_with_ms_printer})..
   * For more customization over how durations are printed see {!to_formatted_string}.
   *
   * @param duration a duration to be printed
   * @return string representation of [duration] using {!long_time_with_ms_printer} format.
  **/
  to_detailed_string(duration : Duration.duration) : string =
    to_formatted_string(long_time_with_ms_printer, duration)

  /**
   * Converts a string representation to a duration.
   *
   * The string is composed of blocks of the following shape:
   *   [+/-][n][s/m/h/D/M/Y]
   * which represents a positive/negative (resp. with [+/-] as prefix) duration
   * of [n] [seconds/minuts/hours/days/months/years] (resp. for [s/m/h/D/M/Y].
   * The whole duration is a summation over duration represented by blocks.
   * The [+] prefix can be ommited.
   *
   * Few examples:
   * [1Y-1D] - a duration of one year, without a day (i.e. 364 days)
   * [1D+10h+10m] - a duration of one day, 10 hours & 10 minutes
   * [-5D-3h] - a negative duration of 5 days and 3 hours
  **/
  of_string(duration : string) : option(Duration.duration) =
    Parser.try_parse(Duration_private.OfString.duration_parser, duration)

  /**
   * {1 Data structures based on durations}
  **/

  /**
   * A [Map] on durations.
   */
  Map = Map_make(order) : Map(Duration.duration, Duration.order)

  /**
   * A [Set] of durations.
   */
  Set = Set_make(order) : Set(Duration.duration, Duration.order)

}}
