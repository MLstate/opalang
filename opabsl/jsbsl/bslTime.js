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

##extern-type time_t

var time_month_duration = new Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
var time_text_D = new Array( "Sun" , "Mon" , "Tue" , "Wed" , "Thu" , "Fri" , "Sat" );
var time_text_l = new Array( "Sunday" , "Monday" , "Tuesday" , "Wednesday" , "Thursday" , "Friday" , "Saturday" , "Sunday" );
var time_text_F = new Array( "January" , "February" , "March" , "April" , "May" , "June" , "July" , "August" , "September" , "October" , "November" , "December" );
var time_text_M = new Array( "Jan" , "Feb" , "Mar" , "Apr" , "May" , "Jun" , "Jul" , "Aug" , "Sep" , "Oct" , "Nov" , "Dec" );


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
 */
##register now \ now : -> time_t
function now()
{
    var t = new Date();
    return t.getTime();
}

##register gmt_msec : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return t.getUTCMilliseconds();
}

##register gmt_sec : time_t -> int
    ##args(f)
{
   var t = new Date(); t.setTime(f);
    return t.getUTCSeconds();
}

##register gmt_min : time_t -> int
    ##args(f)
{
   var t = new Date(); t.setTime(f);
    return t.getUTCMinutes();
}

##register gmt_hour : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return t.getUTCHours();
}

##register gmt_mday : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return t.getUTCDate();
}

##register gmt_mon : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return t.getUTCMonth();
}

##register gmt_year : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return t.getUTCFullYear();
}

##register gmt_wday : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return t.getUTCDay();
}

##register gmt_yday : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    var yday = t.getUTCDate() - 1;
    var mon = t.getUTCMonth();
    for (var m = 0 ; m < mon ; m++)
        yday += time_month_duration[m];
    if (mon > 1 && leap_year(t.getUTCFullYear()))
        yday++;
    return yday;
}

##register gmt_isdst : time_t -> bool
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return false;
}

##register local_msec : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return t.getMilliseconds();
}

##register local_sec : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return t.getSeconds();
}

##register local_min : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return t.getMinutes();
}

##register local_hour : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return t.getHours();
}

##register local_timezone_offset : -> int
    ##args()
{
    var t = new Date();
    return t.getTimezoneOffset();
}

##register local_mday : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return t.getDate();
}

##register local_mon : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return t.getMonth();
}

##register local_year : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return t.getFullYear();
}

##register local_wday : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return t.getDay();
}

##register local_yday : time_t -> int
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    var yday = t.getDate() - 1;
    var mon = t.getMonth();
    for (var m = 0 ; m < mon ; m++)
        yday += time_month_duration[m];
    if (mon > 1 && leap_year(t.getFullYear()))
        yday++;
    return yday;
}

##register local_isdst : time_t -> bool
    ##args(f)
{
    var t = new Date(); t.setTime(f);
    return false;
}

##register mktime : int,int,int,int,int,int,int -> time_t
    ##args(year, mon, day, hour, min, sec, ms)
{
    var t = new Date();
    t.setFullYear(year, mon, day);
    t.setHours(hour, min, sec);
    t.setMilliseconds(ms);
    return (t.getTime());
}

##register import_t : time_t -> int
##args(t)
{
    return t;
}

##register export_t : int -> time_t
##args(t)
{
    return t;
}

##register get_accurate_time \ get_accurate_time : -> float
function get_accurate_time()
{
    var t = new Date();
    return t.getTime()/1000.0;
}
