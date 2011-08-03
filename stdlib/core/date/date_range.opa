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
 * @author Adam Koprowski
 * @category data
 *
 *
 * {1 About this module}
 *
 *   This module contains functions to manipulate date ranges. A date range
 * is a time interval fixed in time; in other words a date range consist
 * of a starting and ending date.
 *
 *   If you are interested in time durations (intervals), without a fixed
 * location in time then see the {!Duration} module.
 *
 *
 * {1 When to use this module}
 *
 *   This module will let you manipulate date ranges, which occur naturally
 * when considering time events (think: events in your agenda).
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
**/

/**
 * {1 Types defined in this module}
 */

/**
 * A date range, i.e. an interval between two dates.
**/
@abstract type DateRange.range = (Date.date, Date.date)

DateRange = {{

  /**
   * {1 Date range construction}
  **/

  /**
   * Takes as an input two dates and constructs a range between those dates.
   *
   * @param r1 One end of the range
   * @param r2 The other end of the range
   * @return Date range [r1]-[r2] (if [r1 < r2]) or [r2]-[r1] (otherwise)
  **/
  between(r1 : Date.date, r2 : Date.date) : DateRange.range =
    if r1 < r2 then
      (r1, r2)
    else
      (r2, r1)

  /**
   * Takes as an input a start date and a duration and constructs a date
   * range starting at a given date and having the given duration.
   *
   * @param [at] a date
   * @param [duration] a duration
   * @param range [r]; if [duration] is positive then [r] starts at [at] and
   *        ends [duration] later; if [duration] is negative then [r] ends at
   *        [at] and lasts [-duration].
  **/
  construct(at : Date.date, length : Duration.duration) : DateRange.range =
    between(at, Date.advance(at, length))

  /**
   * {1 Date range accessors}
  **/

  /**
   * Returns true iff. the date range is empty (i.e. it's start and
   * end point are the same)
   *
   * @param r A range
   * @return true iff [r] is empty
  **/
  is_empty(r : DateRange.range) : bool =
    get_beg(r) == get_end(r)

  /**
   * Returns the beginning date of a range.
   *
   * @param r A range
   * @return the beginning date of [r]
  **/
  get_beg(r : DateRange.range) : Date.date =
    r.f1

  /**
   * Returns the end date of a range.
   *
   * @param r A range
   * @return the end date of [r]
  **/
  get_end(r : DateRange.range) : Date.date =
    r.f2

  /**
   * Takes as an input a date range and return a duration corresponding
   * to this range.
   *
   * @param d A date range
   * @return Time duration corresponding to [d].
  **/
  length(r : DateRange.range) : Duration.duration =
    Duration.between(r.f1, r.f2)

  /**
   * {1 Date range conversion}
  **/

  /**
   * Takes as an input a date range and return a duration corresponding
   * to this range.
   *
   * See {!DateRange.length}
  **/
  to_duration(r : DateRange.range) : Duration.duration =
    length(r)

  /**
   * {1 Date range operations}
  **/

  /**
   * Returns an intersection of two date ranges, or indicates that such
   * an intersection is empty.
   *
   * @param r1 a date range
   * @param r2 another date range
   * @return [some(r)], where [r] is the intersection of [r1] and [r2],
   *         or [none] if such intersection is empty.
  **/
  intersection(r1 : DateRange.range, r2 : DateRange.range) : option(DateRange.range) =
    s1 = r1
    s2 = r2
    if s1.f1 > s2.f1 then
      intersection(r2, r1)
    else if s1.f2 < s2.f1 then     // |--r1--|  |==r2===|
      none
    else if s2.f2 < s1.f2 then     // |---r1--|==r2==|--|
      some(r2)
    else                           // |----r1----|
      some(between(s2.f1, s1.f2)) //      |====r2===|

  /**
   * {1 Pretty-printing}
  **/

  /**
   * Returns a string representation of a date-range in the form:
   * "[Thu Mar  3 2005 23:05:25 - Fri Mar  4 2005 14:27:12]"
   *
   * @param r a date range
   * @return string representation of [r] as described above.
  **/
  // TODO add customization
  to_string(r : DateRange.range) =
    "[{Date.to_string(get_beg(r))} - {Date.to_string(get_end(r))}]"

}}
