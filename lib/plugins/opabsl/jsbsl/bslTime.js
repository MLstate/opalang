/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Time primitives
 * @side{both}
 *
 * BEWARE : Major of bypasses which manipulates time_t are tagged as pure
 * because we define only getter.
 */

var time_month_duration = new Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

function leap_year(y){
    return( ((y % 4) == 0) && ((y % 100) != 0) || ((y % 400) == 0) );
}

/**
 * Convert an integer to a string representation with at least two digits.
 *
 * @param {number} n A non-negative number.
 * @return {string} Either "0" followed by the string representation of [n] (if n < 10) or simply the string representation of [n] (otherwise)
 */
function two_digits(n)
{
    if (n < 10)
        return "0" + n;
    else
        return n.toString();
}

/**
 * Return the current time, as an [int]
 *
 * @return {number} The current time, as a delta from the epoch.
 * @register { -> time_t}
 */
function now() {
    var t = new Date();
    return t.getTime();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function gmt_msec(f) {
    var t = new Date(); t.setTime(f);
    return t.getUTCMilliseconds();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function gmt_sec(f) {
   var t = new Date(); t.setTime(f);
    return t.getUTCSeconds();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function gmt_min(f) {
   var t = new Date(); t.setTime(f);
    return t.getUTCMinutes();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function gmt_hour(f) {
    var t = new Date(); t.setTime(f);
    return t.getUTCHours();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function gmt_mday(f) {
    var t = new Date(); t.setTime(f);
    return t.getUTCDate();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function gmt_mon(f) {
    var t = new Date(); t.setTime(f);
    return t.getUTCMonth();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function gmt_year(f) {
    var t = new Date(); t.setTime(f);
    return t.getUTCFullYear();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function gmt_wday(f) {
    var t = new Date(); t.setTime(f);
    return t.getUTCDay();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function gmt_yday(f) {
    var t = new Date(); t.setTime(f);
    var yday = t.getUTCDate() - 1;
    var mon = t.getUTCMonth();
    for (var m = 0 ; m < mon ; m++)
        yday += time_month_duration[m];
    if (mon > 1 && leap_year(t.getUTCFullYear()))
        yday++;
    return yday;
}

/**
 * @register {time_t -> bool}
 * @pure
 */
function gmt_isdst(f) {
    var t = new Date(); t.setTime(f);
    return false;
}

/**
 * @register {time_t -> int}
 * @pure
 */
function local_msec(f) {
    var t = new Date(); t.setTime(f);
    return t.getMilliseconds();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function local_sec(f) {
    var t = new Date(); t.setTime(f);
    return t.getSeconds();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function local_min(f) {
    var t = new Date(); t.setTime(f);
    return t.getMinutes();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function local_hour(f) {
    var t = new Date(); t.setTime(f);
    return t.getHours();
}

/**
 * @register {-> int}
 * @pure
 */
function local_timezone_offset() {
    var t = new Date();
    return t.getTimezoneOffset();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function local_mday(f) {
    var t = new Date(); t.setTime(f);
    return t.getDate();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function local_mon(f) {
    var t = new Date(); t.setTime(f);
    return t.getMonth();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function local_year(f) {
    var t = new Date(); t.setTime(f);
    return t.getFullYear();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function local_wday(f) {
    var t = new Date(); t.setTime(f);
    return t.getDay();
}

/**
 * @register {time_t -> int}
 * @pure
 */
function local_yday(f) {
    var t = new Date(); t.setTime(f);
    var yday = t.getDate() - 1;
    var mon = t.getMonth();
    for (var m = 0 ; m < mon ; m++)
        yday += time_month_duration[m];
    if (mon > 1 && leap_year(t.getFullYear()))
        yday++;
    return yday;
}

/**
 * @register {time_t -> bool}
 * @pure
 */
function local_isdst(f) {
    var t = new Date(); t.setTime(f);
    return false;
}

/**
 * @register {int,int,int,int,int,int,int -> time_t}
 * @pure
 */
function mktime(year, mon, day, hour, min, sec, ms) {
    var t = new Date();
    t.setFullYear(year, mon, day);
    t.setHours(hour, min, sec);
    t.setMilliseconds(ms);
    return (t.getTime());
}

/**
 * @register {time_t -> int}
 * @pure
 */
function import_t(t) {
    return t;
}

/**
 * @register {int -> time_t}
 * @pure
 */
function export_t(t) {
    return t;
}

/** @register { -> float} get_accurate_time get_accurate_time */
function get_accurate_time()
{
    var t = new Date();
    return t.getTime()/1000.0;
}
